;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; To be compiled within main.asm
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; Functions list:
;  rehard_vs
;  resoft_vs
;  write_vol
;  send_2048
;  sdi_out
;  beep
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

; VS1001k crystal speed in kHz:
XTALI   equ   24576

        if(XTALI/16000 > 0)
FREQ equ XTALI/2
        else
FREQ equ XTALI/2 + 0x8000
        endif

#define  sound_step  0x04            ; volume step

;-----------------------------------------------------------
; rehard_vs - execute VLSI HW reset 
;-----------------------------------------------------------
rehard_vs
        bcf     bit_vs_reset         ; turn off VS
        call    delay_01s
        call    delay_01s
        bsf     bit_vs_reset         ; turn on VS
        call    delay_01s
        ; do not return: execute sw reset after hw

;----------------------------------------------------------;
; resoft_vs                                                ;
; Software reset VS1001k. Configure SDI interface          ;
;----------------------------------------------------------;
resoft_vs
        bcf     bit_vs_cs
        movlw   0x02                ; "write"
        call    spi2_out
        movlw   0x00                ; addr 0å0
        call    spi2_out
        movlw   0x03                ; command 0å304 (SW reset)
        call    spi2_out
        movlw   0x04
        call    spi2_out
        bsf     bit_vs_cs
    
        call    delay_5us
        call    delay_5us
                                    ; wake, setup SDI
        bcf     bit_vs_cs
        movlw   0x02
        call    spi2_out
        movlw   0x00
        call    spi2_out
        movlw   0x03                ; SDI: LSB first, write on falling edge
        call    spi2_out            ; slave mode
        movlw   0x80
        andwf   mystat,W
        call    spi2_out
        bsf     bit_vs_cs
        
        call    delay_5us
        call    delay_5us

        if (XTALI != 24576)            ; for non default VLSI crystal frequency
        bcf     bit_vs_cs           
        movlw   0x02
        call    spi2_out
        movlw   0x03
        call    spi2_out
        movlw   HIGH (FREQ)            ; defined in main.asm
        call    spi2_out
        movlw   LOW  (FREQ)
        call    spi2_out
        bsf     bit_vs_cs
        endif
        
        call    delay_5us
        ; do not return either - set volume first =)
;----------------------------------------------------------;
; write_vol                                                ;
; Write contents of vol register into VLSI                 ;
; Set bass/treble enhance flag, if needed                  ;
;----------------------------------------------------------;
write_vol
        bcf     bit_vs_cs               ; write vol
        movlw   0x02
        call    spi2_out
        movlw   0x0b
        call    spi2_out
        movf    vol,w                   ; you can set left and
        call    spi2_out                ; right channels volume 
        movf    vol,w                   ; separately
        call    spi2_out
        bsf     bit_vs_cs
        call    delay_5us

        bcf     bit_vs_cs               ; write bass/treble enhancer bit
        movlw   0x02
        call    spi2_out
        movlw   0x00
        call    spi2_out
        movlw   0x03
        call    spi2_out
        movlw   0x80                    ; read flag from mystat
        andwf   mystat,W                
        call    spi2_out
        bsf     bit_vs_cs
        ;goto    delay_5us

;----------------------------------------------------------------;
; delay_5us
; Mandatory delay between SCI commands
;----------------------------------------------------------------;
delay_5us
        movlw d'18'                     
        movwf tmpz                      ; 8 MHz PIC Crystal

        decfsz tmpz,F
        goto   $-1

        return
        
;----------------------------------------------------------------;
; send_2048                                                      ;
; Send 2048 zero bytes to SDI. Should be called at the end of    ;
; every track to make sure we did not lost last seconds of song  ;                                             ;
;----------------------------------------------------------------;
send_2048
        movlw   0x64
        movwf   tmpz+1
_send32_0
        btfss   bit_vs_dreq     ; is data request set?
        goto    $-1

        movlw   0x32
        movwf   tmpz
_send_zero
        clrw
        call    sdi_out
        decfsz  tmpz,F
        goto    _send_zero

        decfsz  tmpz+1,F
        goto    _send32_0
        return

;-------------------------------------------------------;
; sdi_out(W)                                            ;
; send byte stored in W through SDI                     ;
;-------------------------------------------------------;
sdi_out
        bsf    bit_vs_bsync       ; SYNC bit
        movwf    TXREG            ; I do not use TXIF as it does not mean
        nop                       ; "byte was sent" but "TXREG is empty"
        nop
        nop
        bcf    bit_vs_bsync
        nop
        nop
        nop
        nop
        return

;---------------------------------------------------;
; beep(W)                                           ;
; "Beep" during 100 ms; Use W to control frequency  ;
;---------------------------------------------------;
beep
        movwf   answer
        call    delay_500us
        movlw   0x53
        call    sdi_out
        movlw   0xef
        call    sdi_out
        movlw   0x6e
        call    sdi_out
        movf    answer,W        ; see VS1001k documentation
        call    sdi_out         
        clrw
        call    sdi_out
        call    sdi_out
        call    sdi_out
        call    sdi_out
        call    delay_01s
        movlw   0x45
        call    sdi_out
        movlw   0x78
        call    sdi_out
        movlw   0x69
        call    sdi_out
        movlw   0x74
        call    sdi_out
        clrw
        call    sdi_out
        call    sdi_out
        call    sdi_out
        call    sdi_out
        return
        
;--------------end of vs.asm-------------------;