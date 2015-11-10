;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; To be compiled within main.asm
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#define  key0         PORTB,2    ; buttons
#define  key3         PORTB,3
#define  key2         PORTB,4
#define  key1         PORTB,5

#define  bit_mmc_cs   PORTC,2    ; select SD/MMC
#define  bit_vs_cs    PORTC,1    ; select SPI VS
#define  bit_vs_dreq  PORTC,0    ; DREQ
#define  bit_vs_reset PORTB,1    ; reset VS
#define  bit_vs_bsync PORTB,0    ; BSYNC

; SD/MMC and VLSI share the same SPI interface
#define  bit_spi_in   PORTC,4    ; SD/MMC DO
#define  bit_spi2_in  PORTC,4    ; VLSI data data

;--------------------------------------------------------
; get_spi
; @return: W - received byte
; Actually equals to spi_out(0xFF)
;--------------------------------------------------------
get_spi                           
        movlw   0xff              ; place holder byte
        ; goto spi_out

;--------------------------------------------------------
; spi_out(W)
; send byte stored in W to SPI
; @return: W - received byte
;--------------------------------------------------------
spi2_out
spi_out
        movwf   SSPBUF            ; put to buffer 
        bsf     STATUS,RP0
        btfss   SSPSTAT,BF        ; completed?
        goto    $-1               ; no
        bcf     STATUS,RP0
        movf    SSPBUF,w          ; received into W
        return

; EOF