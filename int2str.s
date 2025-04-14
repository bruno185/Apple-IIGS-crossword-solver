* Convert an integer to a string
* in : A = integer to convert
* out : int_str = string representation of the integer
* out : A/X = address of the string (lo/hi)

        mx %00          ; A, X, Y in 16 bits
        rep #$30        ; A, X, Y in 16 bits

intToStr
        ; number to convert should be in A
        sta val         ; save to val var
        ldx #0          ; init string index
        stx  tempX      ; init tempX var

convertLoop 
        ldy #0          ; init. remainder
        sty remainder   ; clear remainder var
        jsr divideBy10  ; divide A by 10 (result in a, remainder var set)
        sta val         ; save quotien for next division
        lda remainder   ; get remainder
        clc
        adc #'0'        ; convert to ASCII
        ldx tempX       ; reload string index
        sta string1+1,x ; save char in string1
        inc tempX       ; inc string index 
        lda val         ; reload val to update flags
        bne convertLoop ; loop until val = 0

        inx
        MX %11          ; A, X, Y in 8 bits 
        sep #$30
        stx string1     ; save length of string 
        jsr reverseString ; Inverser le buffer
        rep #$30
        MX %00          ; A, X, Y in 16 bits 
        lda #int_str    ; load low bytes of int_str address
        ldx #^int_str   ; load high bytes of int_str address
        rts

divideBy10
        ; Division 16 bits by 10 (quotient in a, remainder in remainder) 
        ldx #0
loopDiv
        cmp #10
        bcc endDiv      ; if A < 10, exit loop
        sbc #10         ; A = A - 10
        inx             ; increment quotient    
        bra loopDiv     ; repeat until A < 10
endDiv
        sta remainder   ; save remainder
        txa             ; quotient in A
        rts


reverseString
        ; string1 = string to reverse
        ; int_str = string in correct order
        MX %11          ; A, X, Y in 8 bits
        sep #$30
        ldx #0
        ldy string1
revloop
        dey
        lda string1+1,x 
        sta int_str+1,y 
        inx 
        cpx string1     ; check if we reached the end of the string
        bne revloop     ; if not, continue
        lda string1     ; load string length
        sta int_str     ; 
        mx %00          ; A, X, Y in 16 bits
        rep #$30
        rts 

; Variables et Buffer
remainder       ds 2    ; Stocke le reste de la division
string1         ds 8    ; string in inverse order
int_str         ds 8    ; string in correct order
tempX           ds 2     ; Variable temporaire pour stocker x
val             ds 2    ; Variable temporaire 