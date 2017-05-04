#define  bit_mmc_cs         PORTC,2    ; выборка MMC
#define  bit_spi_in         PORTC,4    ; выборка MMC

#define  bit_vs_cs          PORTA,2    ; выборка SPI VS
#define  bit_vs_dreq        PORTC,1    ; сигнал DREQ
#define  bit_vs_reset       PORTA,5    ; сигнал сброса VS
#define  bit_vs_bsync       PORTC,0    ; BSYNC

#define  bit_spi2_clk       PORTA,1    ; второй програмный SPI
#define  bit_spi2_out       PORTA,0    ; для связи с экраном и с
#define  bit_spi2_in        PORTA,4    ; декодером

#define  key0               PORTB,2    ; порты кнопок
#define  key3               PORTB,3
#define  key2               PORTB,4
#define  key1               PORTB,5

;--------------------------------------------------------
; spi_out/get_spi, отправка/прием байта в/из SPI
;--------------------------------------------------------

get_spi                           ; - если требуется отправить пустоту
        movlw   0xff
spi_out
        movwf   SSPBUF            ; отправить содержимое W в spi
        bsf     STATUS,RP0
        btfss   SSPSTAT,BF        ; передача/прием завершены?
        goto    $-1               ; нет - ждать
        bcf     STATUS,RP0
        movf    SSPBUF,w          ; записать принятое в W
        return

spi2_out
        movwf   spi2
        movlw   0x08
        movwf   spi2_tmp

_spi2lp
        bcf     bit_spi2_clk
        rlf     spi2,f
        bcf     bit_spi2_out
        btfsc   STATUS,C
        bsf     bit_spi2_out
        nop
        bsf     bit_spi2_clk
        decfsz  spi2_tmp,f
        goto    _spi2lp

        bcf     bit_spi2_clk
        nop
        bcf     bit_spi2_out
        return
