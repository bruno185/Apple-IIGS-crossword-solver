*
* This macro concatenates two short strings, startig with a length byte
* Format of the macro is: concatStr destination_string;string1;string2 
ConcatStr       MAC
                sep #$30
				MX %11

                ldx ]2          ; get length of string 1        
]loop
                lda ]2,x        ; get chars of string 1, starting from the end
                sta ]1,x        ; store them in destination_string
                dex
                bpl ]loop     ; length included

                lda ]3          ; get length of string 2
                tay             ; y = length of string 2
                clc
                adc ]2          ; add length of string 1
                sta ]1          ; store it in destination_string (length of string 1 + string 2)
                tax             ; x = length of string 0 + string 2             
]loop
                lda ]3,y        ; copy string 2 at the end of string 1
                sta ]1,x
                dex
                dey
                bne ]loop
                <<<


* This macro concatenates two long strings, startig with a 2 length bytes
* Format : concatStrL destination_string;string1;string2 
ConcatStrL      MAC
                rep #$30
		MX %00
                ldx #0           ; init counter        
]loop
                lda ]2+2,x      ; get chars from string 1, starting from the beginning
                sta ]1+2,x      ; store them in destination_string
                inx
                inx 
                cpx ]2          ; cmp with string length 
                bcc ]loop       ; x < length of string 1 
                ldx ]2
                ldy #0            
]loop
                lda ]3+2,y      ; copy string 2 at the end of string 1
                sta ]1+2,x
                inx
                inx 
                iny
                iny
                cpy ]3
                bcc ]loop
                
                lda ]3          ; get length of string 2
                clc
                adc ]2          ; add length of string 1
                sta ]1          ; store it in destination_string (length of string 1 + string 2)
                <<<


* This macro creates a long string from a short string (override short string)
* Format : MakeLStr string
* Caution : string get 1 byte longer
MakeLStr        MAC
                sep #$30
                ldx ]1          ; get length of short string 
]loop 
                lda ]1,x        ; get the last char of short string
                sta ]1+1,x      ; store it in the long string at postion + 1
                dex
                bne ]loop
                lda #0
                sta ]1+1        ; store 0 for hi byte of length
                rep #$30
                <<<

* This macro creates a short string from a long string 
* Format : MakeSStr Long_string
MakeSStr        MAC
                sep #$30
                ldx #0
]loop 
                lda ]1+2,x 
                sta ]1+1,x 
                inx 
                cpx ]1
                bne ]loop 
                rep #$30
                <<<

* This macro is used for degugging with crossrunner Apple IIGS emulator
stop            MAC
                pha
                phx
                lda #$AAAA
                ldx #$BBBB
                plx
                pla
                <<<

* * This macro calls MoveTo tool 
goto            MAC
                PushWord ]1
                PushWord ]2
                _MoveTo
                <<<
        
gotoXY          MAC
                phx
                phy
                _MoveTo
                <<<

