#define  bit_mmc_cs         PORTC,2    ; �롮ઠ MMC
#define  bit_spi_in         PORTC,4    ; �롮ઠ MMC

#define  bit_vs_cs          PORTA,2    ; �롮ઠ SPI VS
#define  bit_vs_dreq        PORTC,1    ; ᨣ��� DREQ
#define  bit_vs_reset       PORTA,5    ; ᨣ��� ��� VS
#define  bit_vs_bsync       PORTC,0    ; BSYNC

#define  bit_spi2_clk       PORTA,1    ; ��ன �ணࠬ�� SPI
#define  bit_spi2_out       PORTA,0    ; ��� �裡 � �࠭�� � �
#define  bit_spi2_in        PORTA,4    ; ������஬

#define  key0               PORTB,2    ; ����� ������
#define  key3               PORTB,3
#define  key2               PORTB,4
#define  key1               PORTB,5

;--------------------------------------------------------
; spi_out/get_spi, ��ࠢ��/�ਥ� ���� �/�� SPI
;--------------------------------------------------------

get_spi                           ; - �᫨ �ॡ���� ��ࠢ��� ������
        movlw   0xff
spi_out
        movwf   SSPBUF            ; ��ࠢ��� ᮤ�ন��� W � spi
        bsf     STATUS,RP0
        btfss   SSPSTAT,BF        ; ��।��/�ਥ� �����襭�?
        goto    $-1               ; ��� - �����
        bcf     STATUS,RP0
        movf    SSPBUF,w          ; ������� �ਭ�⮥ � W
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
