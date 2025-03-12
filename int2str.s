* Convert an integer to a string
* in : A = integer to convert
* out : int_str = string representation of the integer
* out : A/X = address of the string (lo/hi)

        mx %00         ; A, X, Y in 8 bits 
        rep #$30

intToStr
        ; number to convert should be in A
        sta val         ; save to val var
        ldx #0          ; init string index
        stx  tempX

convertLoop 
        ldy #0          ; init. remainder
        sty remainder
        jsr divideBy10  ; divide by 10 (result in a, remainder var set)
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
        lda #int_str    ; load int_str address
        ldx #^int_str
        rts

divideBy10
        ; Division 16 bits by 10 (quotient in a, remainder in remainder) 
        ldx #0
loopDiv
        cmp #10
        bcc endDiv
        sbc #10
        inx
        bra loopDiv
endDiv
        sta remainder  ; Stocker le reste
        txa            ; Quotient dans a
        rts


reverseString 
        MX %11
        ldx #0
        ldy string1
revloop
        dey
        lda string1+1,x 
        sta int_str+1,y 
        inx 
        cpx string1
        bne revloop
        lda string1
        sta int_str
        rts 

; Variables et Buffer
remainder       ds 2    ; Stocke le reste de la division
string1         ds 8    ; string in inverse order
int_str         ds 8    ; string in correct order
tempX           ds 2     ; Variable temporaire pour stocker x
val             ds 2    ; Variable temporaire 