* Startup main tool set
*
InitTools
* Tool Locator
        _TLStartUp      ; tool locator

* Memory Manager
        pha             ; make room on stack for id
        _MMStartUp      ; memory manager
        pla             ; get back the id
        sta id 
        
* Miscellaneous Tools
        _MTStartUp      ; misc tools
        jsr PrepareToDie        ; test success

* NewHandle
        ; TOOL SET              : BYTES REQUIRED
        ; QuickDraw II          : $300
        ; Event Manager         : $100 (The Event Manager shares its page with the Window Manager)
        ; Control Manager       : $100 (The Control Manager shares its page with the Dialog Manager)
        ; Menu Manager          : $100
        ; LineEdit              : $100
        ; Font Manager          : $100
        ; Print Manager         : $100
        ; Sound Manager         : $100
        ; Standard File Operations : $100
        ; SANE                  : $100
        ; Total                 : 12 pages = $0C00 bytes

        PushLong #0
        PushLong #$0C00         ; 12 pages
        PushWord id
        PushWord #$C005         ; Locked, fixed location, page aligned, fixed bank
        PushLong #0             ; Location = bank 0
        _NewHandle

        pla             ; get handle 
        sta 0
        pla
        sta 2
                        ; handle to pointer        
        lda [0]         ; Direct Page Indirect Long (https://nesdoug.com/2020/04/06/further-in-65816/)
                        ; read content of memory pointed by 3 bytes (0, 1, 2 in this case)
        sta dpmem       ; store it in dpmem 
        lda #0          ; always 0, since we are in bank 0    
        sta 6

* Event Manager
        lda dpmem       ; Event Manager need $100 bytes in DP
        pha             ; push dp address on stack 
        PushWord #$0000 ; queue size = default = 20 
        PushWord #$0000 ; min x clamp for mouse = 0 
        PushWord #screenwidth           ; max x clamp = 320 
        PushWord #$0000 ; min y clamp = 0 
        PushWord #screenheight          ; max y clamp = 200 
        PushWord id     ; id 
        _EMStartUp      ; call the event manager startup routine
        lda dpmem
        clc
        adc #$100       ; jump to the end of the Event Manager memory block
        sta dpmem       ; save new address in dpmem

* QuickDraw II
        lda dpmem
        pha             
        PushWord #Screenmode
        PushWord #MaxWidth  
        PushWord id
        _QDStartUp
        lda dpmem
        clc
        adc #$300       ; jump to the end of the QuickDraw II memory block
        sta dpmem       ; save new address in dpmem

* LoadTools
        PushLong #toolTable
        _LoadTools     ; ERROR !!!

* Window Manager
        PushWord id
        _WindStartUp

* Control Manager
        PushWord id
        pha
        _CtlStartUp      ; Control Manager
        lda dpmem
        clc
        adc #$100        ; jump to the end of the Control Manager memory block
        sta dpmem        ; save new address in dpmem

* Menu Manager
        PushWord id
        lda dpmem
        pha 
        _MenuStartUp    ; Menu Manager
        lda dpmem
        clc
        adc #$100        ; jump to the end of the Menu Manager memory block

* Dialog Manager
        PushWord id
        _DialogStartUp       

* LineEdit
        PushWord id
        lda dpmem
        pha
        _LEStartUp      ; LineEdit
        lda dpmem
        clc
        adc #$100        ; jump to the end of the LineEdit memory block

* Font Manager 
        lda dpmem
        pha
        PushWord id
        _FMStartUp
        lda dpmem
        clc
        adc #$100       ; jump to the end of the Font Manager memory block
        ;cf. page 202
*
        rts 