********************************
*     Crossword solver GS      *
*      MERLIN  ASSEMBLER       *
********************************
*
* Description :                *
* This program is a crossword solver. It reads a pattern from the keyboard, 
* and displays all words that match the pattern.
* The pattern can contain letters (A-Z, a-z) and '?' as a wildcard.
* The program uses a list of words (WORDS file) to find matching words.
* The program is written in Merlin Assembler, and is intended to run on an Apple IIGS.
* The program uses GS/OS calls to manage files I/O.
* The program mimic a console application, on a 320x200 screen.

        MX %00          ; FULL 16 BIT MODE
        REL             ; RELOCATABLE OUTPUT
        DSK cwGS.L

        use E16.GSOS    ; GS/OS calls, in this archive
                        ; folowing files are in Merlin32 Library folder      
        use 4/GsOs.Macs
        use 4/Dos.16.Macs
        use 4/Util.Macs
        use 4/Locator.Macs
        use 4/Mem.Macs
        use 4/Misc.Macs
        use 4/Event.Macs
        use 4/Qd.Macs
        use 4/Window.Macs
        use 4/Ctl.Macs
        use 4/Menu.Macs
        use 4/Dialog.Macs
        use 4/Line.Macs
        use 4/Font.Macs

        use my.Macs

* load  02/0f55
prodos  equ $e100a8     ; prodos 16 entry point
kybd    equ $00c000
strobe  equ $00c010
*
*
dpmem           equ $4
Screenmode      equ $00         ; 0=320; $80=640 pixels wide
MaxWidth        equ 0           ; $0=full screen width 
MaxPatLength    equ 15
bitmapSize      equ 10000
screenwidth     equ 320
screenheight    equ 200

ENTRY
        PHK
        PLB
        clc 
        xce
        rep #$30

* get préfix 
        iGSOS _GetPrefix;PREFIX_PARM;1          ; get cuurent prefix (application path)
        jsr InitTools
*
* * * * * * * * * * * * * * * MAIN LOOP * * * * * * * * * * * * * * *
*
        jsr welcomeScreen       ; fill screen with white display prompt
        jsr miscInits
main       
        lda quitflag            ; test exit flag
        beq taskLoop            ; = 0 : loop 
        jsr ConfimExit
        bcc taskLoop
        jmp Exit                ; <> 0 : quit
taskLoop
        PushWord #$0000         ; space for result
        PushWord #$FFFF         ; all events
        ;PushWord %000000000001110      ; accept (from right to left): mouse down, mouse up, key down
        PushLong #evtrec
        _TaskMaster
        jsr PrepareToDie          
        pla 
        beq main
        lda event
        cmp #$3                 ; keydown only
        bne taskLoop
        jsr DoKey
        bra main
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* manage key down
DoKey
        ; test key (A-Z, a-z, ?, esc)
        lda message     ; get key code
        cmp #27         ; escape key ?
        bne noesc 
        lda #1              
        sta quitflag    ; yes : exit 
        rts 

noesc
        cmp #13         ; return key ?
        bne noreturn
        ldy patternLen 
        cpy #2          ; a valid pattern must be 2 chars long
        bcc :1
        jmp ProcessPat
:1
        rts

noreturn
        cmp #$7F        ; del key ?
        bne nodel       ; no : go on
        ldy patternLen  ; yes : test length
        bne dodel       ; > 0 : del last char
        rts

dodel
        dec patternLen  ; make string 1 byte shorter
        jmp okdel       ; display

nodel
        ldy patternLen  ; get length of pattern string
        cpy #MaxPatLength
        bne ok0         ; if length(pattern) = max : no action
        rts

ok0
        cmp #'?'
        beq okchar      ; ? is wildcard
        cmp #'A'
        bcs ok1         ; < A : exit
        rts             
ok1
        cmp #'Z'+1
        bcc okchar      ; between A and Z : ok
        cmp #'a'
        bcs ok2         ; < Z : exit
        rts 
ok2
        cmp #'z'+1
        bcc okchar
        rts 
                    ; > z
okchar
        sep #$30        ; display string
        MX %11
        
        cmp #'?'        ; if not '?' char...
        beq :1
        and #$DF        ; ...convert to uppercase       
:1
        ldx patternLen
        sta pattern,x   ; poke char in string
        inc patternLen  ; inc index in pattern string for next char
                        ; now patternLen = string length
okdel
        rep #$30
        MX %00
        PushLong #erase_p_rect  ; blank previous string
        _PaintRect
        MakeSStr patternLen     ; make string a short string

        PushWord #$0000         ; space for result
        PushLong #patternLen    ; center string
        _StringWidth
        pla 
        lsr 
        sta posX
        lda #screenwidth/2
        sec 
        sbc posX
        sta posX
        goto posX;#40
        PushLong #patternLen
        _DrawString             ; print string
        MakeLStr patternLen     ; make long string
        rts
*
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* manage a valid pattern string
ProcessPat
* STEPS : 

* 2/ process index ==> final bitmap
* 3/ count words, prepare display
* 4/ Calculate # of page, words per page, etc. 
* 5/ Dispaly page 1, manage UI

* By default, all routines start and end in 16 bits mode
* if 8 bits mode is needed, it is set in the routine
* and reset at the end of the routine.
*
        MX %00          ; 16 bits
        rep #$30        ; A, X, Y in 16 bits

        jsr step0       ; prepare mem for index, init vars
        jsr step1       ; Analyse string : length (oK), only ? or not, set prefix etc.
        jsr step2       ; process index ==> final bitmap
        jsr step3       ; count words, prepare display
                        ; if no word, return address is pulled
        jsr step4       ; Calculate # of page, words per page, etc.
        jsr step5       ; Dispaly results, manage UI
        rts 
*
* >>> STEP 0
* bitmap1 = destination bitmap (result)
* bitmap2 = index are loaded here, and ANDed with bitmap1, one by one
* Fill bitmap1 
step0    
        MX %00
        rep #$30   
        lda #$FFFF
        ldx #bitmapSize-2
:1
        sta bitmap1,x 
        dex
        dex
        bpl :1

initvars
        lda #0                  ; init word counter (result)
        sta wordscnt
        lda #0
        sta wordscnt+2
        rts

* >>> STEP 1
* Analyse string : just ? or not, set directory prefix etc. 
step1
        sep #$30        ; set noletter var = 1 if '?' only in patter string
        MX %11
        lda #0
        sta noletter    ; init noletter var to 0
        ldx patternLen  ; get pattern lengh
        dex             ; adjust index  
:2
        lda pattern,x   ; get a char
        cmp #'?'        ; is it a ?
        bne :1          ; no, so it's a lettre, exit loop
        dex             ; next char
        bpl :2          ; loop 
        lda #1          ; all chars are '?', set noletter var to 1
        sta noletter
:1      
        ldx patternLen
        lda hextable,x 
        sta mydir+3                             ; set prefix L? according to pattern length
        ConcatStrL dir_path;prefix;mydir        ; dir_path = current prefix + directory

        rep #$30
        MX %00
        rts

* >>> STEP 2
* process index and generate final bitmap
step2
        MX %11
        sep #$30

        lda noletter
        beq dostring            ; if no letter in pattern string, process pattern 
        lda #1
        sta indexName           ; set index filename length (1 byte)
        lda #'L'
        sta indexName+2         ; set index filename is 'L', just 1 byte
        jsr loadIndex           ; load index file in memory

        rep #$30
        MX %00

        lda #2
        sta indexName           ; reset index filename length to 2 bytes
        jsr andData             ; AND bitmap1 with bitmap2
        rts                     ; else : exit and go on 

dostring
        MX %11
        sep #$30
        ldx #0
        stx pat_pos
patloop
        ldx pat_pos
        lda pattern,x
        cmp #'?'        ; is it a '?' ?
        beq testfin     ; yes : go on   
doletter
        sta indexName+2         ; filemane 1st lettre (= curent letter of pattern string)
        lda hextable+1,x        ; +1 : to start with 1, not 0
        sta indexName+3         ; filename 2nd letter = position in pattern string

        jsr loadIndex           ; load index file in memory
        rep #$30
        MX %00
        bcc loadok              ; if C = 0 : file loaded, go on

                                ; should never get here, as all files should exist
        jsr cleanLowerScreen    ; erase lower part of screen
        jsr noWordMsg
        pla                     ; pull return address => next rts will exit Dokey
                                ; and go to main loop
        bra s2exit

loadok
        jsr andData             ; AND bitmap1 with bitmap2
testfin
        sep #$30
        MX %11
        inc pat_pos
        ldx pat_pos             ; next char
        cpx patternLen          ; end of pattern string ?
        bne patloop             ; no : loop
s2exit
        MX %00                  ; 16 bits
        rep #$30
        rts
*
* >>> STEP 3
* count words
step3
        MX %10
        sep #$20                ; A : 8 bits ; X,Y : 16 bits
        ldx #0
loadnew
        lda bitmap1,x           ; get a byte
        beq nextbyte            ; if 0 : no word, loop
        ldy #8                  ; init bit counter
shift
        lsr                    ; shift right
        bcc :1                 ; if C = 0 : next bit
        inc wordscnt           ; otherwise : inc word counter
        bne :1
        inc wordscnt+1
        bne :1
        inc wordscnt+2
        bne :1
        inc wordscnt+3         ; should never get here, as # of words < 16 millions 
        bne :1

:1      dey                     ; next bit
        bne shift               ; if not 8 bits done : get another bit
nextbyte
        inx                     ; next byte
        cpx index_size          ; all bytes done ?
        bne loadnew             ; no : loop

        rep #$30
        MX %00                  ; 16 bits
        lda wordscnt 
        ora wordscnt+1
        ora wordscnt+2
        ora wordscnt+3
        bne s3exit

        ; here if no word found
        jsr cleanLowerScreen    ; erase lower part of screen
        jsr noWordMsg
        pla                     ; pull return address => next rts will exit Dokey function
                                ; and go to main loop
        rts

s3exit
        lda wordscnt            ; get # of words found 
                                ; !! should take account of wordcount on 4 bytes
        jsr intToStr            ; convert to string
        ConcatStr strwf;labelwf;int_str         ; prepare string to display
        rts
*
* >>> STEP 4
step4
* Calculate # of page, words per page, etc. dispaly page 1, manage UI 
        ; gap = space between words
        ; deltaX = width of a word + gap
        ; maxX = max # of words (= columns) per line
        ; wordwidth = width of a word
        ; linewidth = width of a line
        ; lmarge = left margin
        ; wordppage = # of words per page
        * wordppage = # of words per page, according to maxX
        * Set word length in a_word_l
*
        MX %00
        rep #$30                ; 16 bits
        lda #gapinit            ; init horizontal gap between words 
        sta gap
        lda patternLen          ; get pattern length (word, including 1st letter of pattern)
        and #$00FF              ; save short pattern length (1 byte)
        asl                     ; * 2 to get offset in widthTable
        tax                     ; x = widthTable offset
        lda widthTable,x        ; get width of a word (made of 'W') of pattern length
* wordwidth = width of a word (made of 'W') of pattern length
        sta wordwidth           ; save it
        clc
        adc gap                ; add gap
* deltax = width of a word + gap
        sta deltaX             ; set deltaX

        lda #0
        sta maxX               ; init max # of columns
        lda #screenwidth-leftMargin-leftMargin   ; available space for a line of words (l / r margins kept)
* maxX = max # of words per line (= # of columns)
getmaxX 
        sec
        sbc deltaX              ; substract width+gap
        bcc maxdone             ; if neg. : done
        inc maxX                ; inc # of columns
        bra getmaxX             ; loop
maxdone
* linewidth, lmarge
        ldx maxX
        lda #0                  ; init word counter (result)
        clc
l1
        adc wordwidth           ; add width of a word
        dex
        bne l1
        ldx maxX
        dex
l2
        clc
        adc gap                 ; add gap
        dex
        bne l2
        sta linewidth           ; save it
        lda #screenwidth 
        sec
        sbc linewidth           ; get space left
        lsr                     ; divide by 2
        sta lmarge

* wordppage = # of words per page
        ldx #maxY
        lda #0
:1
        clc
        adc maxX
        dex 
        bne :1
        sta wordppage

        lda patternLen           ; get pattern length
        sep #$30
        MX %11
        sta a_word_l            ; set length of word before word itself that will be overwritten
        rep #$30
        MX %00
        rts
*
* >>> STEP 5
* Dispaly matching words, manage UI
step5
        MX %00
        rep #30                 ; 16 bits
        ;init 
        ; open WORDS file
        ConcatStrL full_path;dir_path;W_filename     ; full_path = current directory + filename
        ; Open file
        iGSOS _Open;OPEN_PARM;1         ; OK, class 1 call
        lda OPEN_PARM+2                 ; get refNum
        sta SETMARK_PARM+2              ; save it for SetMark call
        sta READ_PARM+2                 ; save it for Read call
        sta CLOSE_PARM+2                ; save it for Close call

        jsr cleanLowerScreen    ; erase lower part of screen
        lda #0                  ; init all vars
        sta curline
        sta curcol
        sta wordpos            ; current position in bitmap1
        sta wordpos+2

        ldx #0
loopbyte
        lda bitmap1,x           ; get a byte from bitamp1
        and #$00FF              ; just 1 byte
        beq zeroByte            ; if 0 : no word
        ldy #8
getbit
        lsr                     ; get bit in carry
        bcs bitfound            ; if bit = 1

nextbit 
        inc wordpos             ; inc word counter
        bne :1
        inc wordpos+2
:1
        dey
        bne getbit
        bra nextbmbyte

bitfound
        pha                     ; save A
        phx                     ; save x                             
        phy                     ; save 
        jsr getWord             ; get word from WORDS file
        jsr displayWord         ; display word. Caary is a flag to stop display or not
        ply                     ; restore y
        plx                     ; restore X
        pla                     ; restore A
        bcc nextbit             ; next bit      ; Carry = 0 : go on, do NOT stop display
        bra doCloseFile         ; close file and exit (stop display)

zeroByte
        lda wordpos            ; inc word counter by 8, since byte = 0
        clc
        adc #8
        sta wordpos
        lda wordpos+2
        adc #0
        sta wordpos+2

nextbmbyte
        inx                     ; next byte
        cpx index_size          ; cmp with index size
        bne loopbyte            ; loop if not end of index

doCloseFile
        ; Close file
        iGSOS _Close;CLOSE_PARM;1         ; Close file class 0 call
        jsr displayWCount       ; display # of words found
        rts
*
*
getWord
        lda wordpos             ; get word counter
        sta filepos             ; save it
        lda wordpos+2
        sta filepos+2
        ldx #4
mul16wc
        asl filepos             ; file offset = word counter * 16 (16 char per word in words file)
        rol filepos+2
        dex
        bne mul16wc             ; multiply by 2, 4 times => multiply by 16. 
                                ; because each word takes 16 bytes in words file.
        ; prepare SetMark call
        lda filepos 
        sta SETMARK_PARM+6      ; set base for SetMark call
        lda filepos+2           ; set base for SetMark call           
        sta SETMARK_PARM+8
        ; SetMark call
        iGSOS _SetMark;SETMARK_PARM;1     ; SetMark call
        ; prepare Read call
        lda #16                 ; read 16 bytes
        sta READ_PARM+8         ; set request count
        lda #a_word             ; set data buffer for reading file
        sta READ_PARM+4
        lda #^a_word
        sta READ_PARM+6
        ; Read call
        iGSOS _Read;READ_PARM;1         ; Read word from WORDS file
        rts
*
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*
displayWord
        ; display word on screen
        ; input : a_word = word to display
        ;
        ; set cursorPosV and cursorPosH according to curline and curcol
        lda #topMargin
        sta cursorPosV
        ldx curline
        beq curlineok
addV
        clc
        adc #deltaY
        sta cursorPosV
        dex
        bne addV

curlineok  
        lda lmarge
        sta cursorPosH
        ldx curcol
        beq curcolok
addX
        clc
        adc deltaX
        sta cursorPosH
        dex
        bne addX 

curcolok
        ; display word
        goto cursorPosH;cursorPosV
        PushLong #a_word_l
        _DrawString

        ; inc curcol and curline
        inc curcol
        lda curcol
        cmp maxX
        bcc linecolok           ; if curcol < maxX, no new line needed
        lda #0                  ; else : init curcol
        sta curcol
        inc curline             ; inc curline   
        lda curline             ;  
        cmp #maxY               ; if curline < maxY, no new page needed
        bne linecolok

        ; new page needed ?           
        lda wordscnt            ; if wordscnt = #  of words par page, no new page needed
        cmp wordppage
* TODO:  * should do the same for wordppage multiples
        beq linecolok            ; no new page needed  

        ; new page needed
        lda #0                  ; init all vars
        sta curline
        sta curcol
        lda #0

        jsr displayWCount       ; display # of words found

        jsr get_key
        and #$007F              ; clear hi byte
        cmp #$1B                ; escape key ? (bit 7 set)
        bne doclean
        
        jsr cleanLowerScreen    ; erase lower part of screen
        sec                     ; Carry = 1 : stop display
                                ; cannot use pla here as A,X, and Y have been put on stack
                                ; So carry is a flag to stop display
        rts

doclean
        jsr cleanLowerScreen    ; erase lower part of screen before next page
linecolok
        clc                     ; Carry = 0 : don't stop display
        rts     
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*
*
loadIndex                       ; load index file in memory
        MX %00                  ; 16 bits 
        rep #$30
        ConcatStrL full_path;dir_path;indexName  ; full_path = current directory + filename

        iGSOS _Open;OPEN_INDEX_PARM;1   ; OPEN call
        beq okopen              ; if OK, go on
        sec                     ; else : set Carry
        rts                     ; and return
okopen
        lda OPEN_INDEX_PARM+2           ; get refNum
        sta GETEOF_PARM+2               ; save it for get_eof call
 
        iGSOS _GetEOF;GETEOF_PARM;1     ; GET_EOF call : get file size

        lda OPEN_INDEX_PARM+2           ; get refNum
        sta READ_PARM+2                 ; save it for read call 

        lda #bitmap2                    ; load address of bitmap2
        sta READ_PARM+4                 ; set data buffer
        lda #^bitmap2
        sta READ_PARM+6

        lda GETEOF_PARM+4               ; get file size
        sta READ_PARM+8                 ; set request count
        lda GETEOF_PARM+6
        sta READ_PARM+10
 
        iGSOS _Read;READ_PARM;1         ; READ call : load file in memory

        lda READ_PARM+12                ; get actual count
        sta index_size                  ; save it  
        lda READ_PARM+14                ; get actual count : always 0, all files < 64K
        sta index_size+2                ; save it          

        lda OPEN_INDEX_PARM+2           ; get refNum
        sta CLOSE_PARM+2                ; save it for Close call

        iGSOS _Close;CLOSE_PARM;1       ; close index file
        clc                             ; clear carry = file loaded
        rts
*
*
*
andData                          ; AND bitmap1 with bitmap2
        MX %10
        sep #$20                ; A : 8 bits ; X,Y : 16 bits
        ldx #0                  ; init index
:1
        lda bitmap2,x 
        and bitmap1,x
        sta bitmap1,x
        inx
        cpx index_size          ; uses low int of index_size, as all files < 64K
        bne :1
        rep #$30
        MX %00
        rts
*
*
*
welcomeScreen
        ; fill scrren with white
        PushLong #myRect
        PushWord #0
        PushWord #0
        PushWord #screenwidth
        PushWord #screenheight
        _SetRect
        lda #15
        sta color
        PushWord color    ; color = white 
        _SetSolidPenPat
        PushLong #myRect
        _PaintRect

        ; draw title string center screen
        PushWord #$0000         ; space for result
        PushLong #title
        _StringWidth
        pla 
        lsr 
        sta posX
        lda #screenwidth/2
        sec 
        sbc posX
        sta posX
        goto posX;#10
        PushLong #title
        _DrawString

        ; draw prompt string center screen
        PushWord #$0000         ; space for result
        PushLong #prompt
        _StringWidth
        pla 
        lsr 
        sta posX
        lda #screenwidth/2
        sec 
        sbc posX
        sta posX
        goto posX;#25
        PushLong #prompt
        _DrawString

        rts


cleanLowerScreen
        PushLong #myRect        ; erase lower part of screen
        PushWord #0
        PushWord #topMargin-10
        PushWord #320
        PushWord #200
        _SetRect
        PushLong #myRect
        _PaintRect
        rts
*
*
miscInits
        stz quitflag    ; quit flag = 0
        ; init widths table, used to calcultae # of words per line, and gap between words
        PushWord #0000          ; get width of W (largest letter)
        lda #'W'
        pha 
        _CharWidth
        pla 
        sta WcharWidth          ; save it
        ldx #2                  ; start at word 2          
        lda #0
:1
        clc 
        adc WcharWidth
        sta widthTable,x        ; populate table
        inx 
        inx 
        cpx #widthTableL        ; end of table ? (15 integers)
        bne :1
        rts
*
*
displayWCount
        ; display # of words found, center screen, bottom line
        MX %00
        rep #$30

        PushWord #$0000         ; space for result
        PushLong #strwf
        _StringWidth
        pla 
        lsr 
        sta posX
        lda #screenwidth/2
        sec 
        sbc posX
        sta posX
        goto posX;#195          ; center screen, bottom
        PushLong #strwf
        _DrawString
        rts
*
noWordMsg
        ; XXXX A message should be displayed here
        ; draw title string center screen
        PushWord #$0000         ; space for result
        PushLong #noword
        _StringWidth
        pla 
        lsr 
        sta posX
        lda #screenwidth/2
        sec 
        sbc posX
        sta posX
        goto posX;#100
        PushLong #noword
        _DrawString

        rts

ConfimExit
        jsr cleanLowerScreen

        ; draw title string center screen
        PushWord #$0000         ; space for result
        PushLong #exitquestion
        _StringWidth
        pla 
        lsr 
        sta posX
        lda #screenwidth/2
        sec 
        sbc posX
        sta posX
        goto posX;#100
        PushLong #exitquestion
        _DrawString

        jsr get_key
        and #$007F              ; clear hi byte
        clc
        cmp #$1B                ; escape key ? (bit 7 set)

        beq realexit            ; yes : exit
        lda #0 
        sta quitflag            ; set quitflag = 0 to indicate no exit in next loop
        jsr cleanLowerScreen
        clc                     ; clear carry (also flag)
        rts                     ; no : go on with carry = 0
realexit
        sec                     ; yes : set carry = 1 (exit flag)
        rts

*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
        put initTools
        put int2str
*
*
* * * * * * * * * * * * * * * EXIT * * * * * * * * * * * * * * *
*
* ShutDownTools
* Order : cf. Apple IIGS Assembly Language Programming (p.188/189)
*
        MX %00
Exit
        rep #$30
        _FMShutDown
        _MenuShutDown
        _WindShutDown
        _CtlShutDown
        _DialogShutDown
        _EMShutDown
        _LEShutDown 
        _QDShutDown
        _MTShutDown
        PushWord id
        _DisposeAll
        PushWord id  
        _MMShutDown
        _TLShutDown

quit    jsl prodos      ; do quit call
        da $29          ; quit code
        adrl PARMBL     ; address of parm table
        bcs error       ; never taken
        brk $00         ; should never get here...

get_key
        rep #$30        ; A, X, Y in 16 bits
        MX  %00
        ldal kybd       ; check keyboard
        and #$00ff      ; clear hi byte
        cmp #$0080      ; keypress?
        bcc get_key     ; nope
        stal strobe     ; clear keypress
        rts
*
error   brk $00         ; we'll never get here?

PrepareToDie
        bcs realdeath
        rts
realdeath
        pha                     ; push error #
        _SysBeep
        PushLong #DeathMsg      ; push message
        _SysFailMgr             ; fail
*
*
* * * * * * * * * * * * * * * DATA  * * * * * * * * * * * * * * *

wordscnt        ds 4
noletter        ds 2

evtrec
event   ds 2
message ds 4   
time    ds 4
ypos    ds 2
xpos    ds 2
mod     ds 2
tdata   ds 4
tmask   dw $0000
        dw $0000

color   ds 2

toolTable
        dw numtools
        da 14   ; window manager
        da 0
        da 15   ; menu manager
        da 0
        da 16   ; control manager
        da 0
        da 20   ; lineedit
        da 0
        da 21   ; dialog manager
        da 0 
        da 5    ; desk manager
        da 0  
        da 27   ; font Manager  
        da 0   

tablesize       equ *-toolTable
numtools        equ tablesize/4
*
*
DeathMsg        str "I'm dying now." 
*
id              dS 2
*
* string to display
prompt          str 'Type pattern (a-z, A-Z, ?)'
title           str 'CROSSWORD SOLVER (French - ODS9++)'
exitquestion    str 'Esc to quit, any key to continue'
noword          str 'No word found'
labelwf         str 'Words found : '
strwf           ds 20
*
myRect          ds 8 
*
patternLen      ds 2
pattern         ds 16
*
quitflag        ds 2

hextable        asc '0123456789ABCDEF'

cursorPosH      ds 2
cursorPosV      ds 2
posX            ds 2
posY            ds 2

maxX            ds 2

;maxY            equ 10                  ; max # of lines, change this value to change # of lines
maxY            equ 12

deltaX          ds 2
deltaY          equ 10

WcharWidth      ds 2

leftMargin      equ 10
;leftMargin      equ 35                 ; left margin : change this value to change margins
topMargin       equ 60

gap             ds 2
gapinit         equ 3

tempo           ds 2
shortlength     ds 2

index_size      ds 4

pat_pos         ds 2

erase_p_rect
        dw 30,0,42,320  ; top left bottom right

wordppage       ds 2
wordwidth       ds 2
linewidth       ds 2
lmarge          ds 2

curline         ds 2
curcol          ds 2
wordpos         ds 4    ; current bit position in bitmap1 = position of word in WORDS file
a_word_l        ds 1    ; length byte of found word
a_word          ds 16   ; 16 bytes of word found in WORDS file
filepos         ds 4    ; file position = word counter * 16 (16 bytes per word in WORDS file)

widthTable
                dw 0    ; length 0      : no pattern length should be 0
                dw 0    ; length 1      : no pattern length should be 1
                ds 2    ; length 2      : 2 chars
                ds 2    ; length 3      : 3 chars
                ds 2    ; length 4      : 4 chars
                ds 2    ; length 5      : 5 chars  
                ds 2    ; length 6      : 6 chars
                ds 2    ; length 7      : 7 chars
                ds 2    ; length 8      : 8 chars
                ds 2    ; length 9      : 9 chars
                ds 2    ; length 10     : 10 chars
                ds 2    ; length 11     : 11 chars
                ds 2    ; length 12     : 12 chars
                ds 2    ; length 13     : 13 chars
                ds 2    ; length 14     : 14 chars
                ds 2    ; length 15     : 15 chars
widthTableL     equ *-widthTable

* * * * * * * * * * * * * * * * * * GS/OS data * * * * * * * * * * * * * * * * * * 

PARMBL  ADRL $0000      ; PTR TO PATHNAME
FLAG    DA $C000        ; ABSOLUTE QUIT

OPEN_INDEX_PARM
        dw        2             ; pCount
        ds        2             ; refNum
        adrl      full_path     ; pathname

GETEOF_PARM
        dw      2             ; pCount
        ds      2             ; refNum
        ds      4             ; file size

READ_PARM
        dw      5             ; pCount
        ds      2             ; refNum (+2)
        ds      4             ; data buffer (+4)
        ds      4             ; request count (+8)
        ds      4             ; actual count (+12)
        dw      0             ; cache priority (+16)

OPEN_PARM          
        dw        2             ; pCount
        ds        2             ; refNum
        adrl      full_path     ; pathname

PREFIX_PARM
        dw      2               ; pCount
        dw      1               ; prefix # (1/)
        adrl    prefix_length   ; prefix

CLOSE_PARM
        dw        1             ; pCount
        ds        2             ; refNum
        ds 2

SETMARK_PARM
        dw      3               ; pCount
        ds      2               ; refNum
        ds      2               ; base
        ds      4               ; mark

prefix_length
        dw 256                  ; size of buffer
prefix  ds 256                  ; prefix string 

full_path
        ds 128
        hex 00

mydir
        strl 'L?:'      ; must be set according to pattern length
        hex 00          ; in case of overflow

dir_path
        ds 128          ; must be set to path to directory containing index files

W_filename
        strl 'WORDS'
        hex 00

indexName
        strl '??'
        
bitmap1 ds 10000
bitmap2 ds 10000