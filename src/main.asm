#define  orig_io   1
; 1 - Use with V1 scheme
; 0 - Use with V2 scheme

 	LIST p=PIC16F73, R=DEC
	INCLUDE "p16f73.inc"
	title "MP3 player with MultiMediaCard"
        __CONFIG _WDT_OFF & _BODEN_OFF & _HS_OSC & _CP_OFF & _PWRTE_ON


; add two 24-bit words: dest=dest+src
add_24      MACRO src,dest
            LOCAL _addnxt
                                     
        movf    src+2,W              ; add LSB byte of src
        addwf   dest+2,F             ; to LSB of dst
        btfss   STATUS,C             ; test if carry
        goto    _addnxt              ; no carry
        incf    dest+1,F             ; carry occurred: increment high bytes
        btfsc   STATUS,Z             ; of dest
        incf    dest+0,F
_addnxt
        movf    src+1,W              ; add 2nd byte of src
        addwf   dest+1,F             ; to second byte of dest
        btfsc   STATUS,C             ; test if carry
        incf    dest+0,F             ; carry: increment MSB of dest
        movf    src+0,W              ; finally add MSB of src to MSB of dest
        addwf   dest+0,F
            ENDM


; MMC commands
CMD0     equ  0x40                   ; reset
CMD1     equ  0x41                   ; init
CMD16    equ  0x50                   ; define block size
CMD17    equ  0x51                   ; read block

;***************************************************************************
;  DATA SECTION
;***************************************************************************

; RAM starting at 0x20
         cblock 0x20
          cmd_num                    ; MMC command frame: command
          arg0:4                     ; ------"----"------ parameter
          crc                        ; ------"----"------ CRC

          answer                     ; stores "mmc_send_cmd" result
          tmpz:2                     ; delay counters

          vol                        ; volume

          ; FAT
          clus_base  :3              ; "zero" cluster address
          fat_base   :3              ; FAT address
          root_base  :3              ; root catalog address
          root_entry :2              ; current root catalog entry

          file_size  :4              ; current file size
          
          loop_cluster               ; main loop counter - sectors in cluster
          loop_512                   ; main loop counter - blocks in sector
          loop_32                    ; main loop counter - bytes in block
          
          sect_clus  :2              ; sectors per cluster; +0 - doubled
                                                          ; +1 - normal
          next_clus  :2              ; next cluster in chain
          
          mystat                     ; various flags, see below                                     
                                     
          counter    :2              ; еще счетчики

          track_num  :2              ; указатель на запись в корневом каталоге
          keyb_count                 ; buttons debounce/repeat delay
                                     
          key_cmd                    ; флаги нажатых кнопок

          w_temp                     ; временное хранение W,STATUS,FSR
          st_temp                    ; при выполнении прерывания
          fsr_temp
                                     ; аргументы mult:
          msrc        :3             ; множимое
          mmul                       ; множитель
          mrslt       :4             ; результат

          ffirst      :3
          tmpy:2
          tmpx:2
          id3_len:2
          spi2
          spi2_tmp
         endc

; mystat
_bass_ext    equ   7                  ; bass/teble enhanser
_block_err   equ   6                  ; block read error
_seek_fwd    equ   5                  ; move forward
_seek_back   equ   4                  ; backward
_find_start  equ   3
_playstop    equ   2                  ; Playing status: 1-playing 0-stopped
_skip_id3    equ   1

; key_cmd
_keyb_ask    equ   7                  ; keyboard request
_volm        equ   6                  ; chahge volume flag
_seek        equ   5                  ; seek flag

;***************************************************************************
;  CODE SECTION
;***************************************************************************
        org 0
        goto _start               ; main entry point

        org 4
;
; Interrupt by TIMER0: check buttons
;
        movwf  w_temp             ; store
        swapf  STATUS,w           ; registers W,STATUS & FSR
        movwf  st_temp
        swapf  FSR,W
        movwf  fsr_temp

        btfss  INTCON,T0IF
        goto   _chk2isr

        bcf    INTCON,T0IF        ; reset interrupt
        call   test_keys          ; check buttons
        goto   _stopisr
_chk2isr

_stopisr
        swapf  fsr_temp,w         ; restore registers
        movwf  FSR
        swapf  st_temp,w
        movwf  STATUS
        swapf  w_temp,f
        swapf  w_temp,w
        retfie                    ; finished
;_____________________________________________________________________________
           if orig_io
#include   "io_v1.asm"
           else
#include   "io_v2.asm"
           endif
#include   "vs.asm"
;_____________________________________________________________________________

_start
        clrf    STATUS
        clrf    INTCON           ; disable interrupts
        clrf    PORTA
        clrf    PORTB
        clrf    PORTC

        BSF	    STATUS, RP0      ; Goto Bank 1
        movlw   0x07
	    movwf	OPTION_REG
        movlw	0x07		     ; port А - digital
	    movwf	ADCON1
        clrf    TRISC
        clrf    TRISB
        clrf    TRISA
        bsf     key0             ; inputs
        bsf     key1
        bsf     key2
        bsf     key3
        bsf     bit_spi_in
        bsf     bit_vs_dreq
        bsf     bit_spi2_in
	    BCF	    STATUS, RP0      ;  Goto Bank 0

        movf    PORTB,W
        bcf     INTCON,RBIF

        bsf      bit_mmc_cs      ; turn aff MMC CS
        bsf      bit_vs_cs       ; and VS CS

	    clrf	ADCON0

	    ;   setup  SPI
        BSF     STATUS,RP0       ; BANK 1
        movlw   1<<CKE           ; write on rising edge, shift out on falling edge
	    movwf   SSPSTAT          ; initial '0'
	    BCF     STATUS,RP0       ; BANK 0
	    BSF     SSPCON,SSPEN     ; enable SPI


        ;   setup  USART
        BSF     STATUS,RP0       ; BANK 1
        movlw   (1<<CSRC)|(1<<SYNC)
        movwf   TXSTA            ; sync master, enable transmitter
        bsf     TXSTA,TXEN
        BCF     STATUS,RP0       ; BANK 0
        BSF     RCSTA,SPEN       ; enable USART


        movlw   0x20             ; RAM clean
        movwf   FSR              ; --
_clr_ram                         ; --
        clrf    INDF             ; --
        incf    FSR,F            ; --
        btfss   FSR,7            ; --
        goto    _clr_ram         ; --

        movlw   0x08
        movwf   keyb_count

        movlw   0x30             ; set normal volume
        movwf   vol

        call    rehard_vs        ; reset VS

        call    init_mmc         ; init MMC

        call    init_fat         ; read FAT constants

        bsf     INTCON,T0IE      ; enable timer0 interrupt
        bsf     INTCON,PEIE      ; --

        bsf     mystat,_playstop ; and go to play

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;  Main Loop - send data from mmc to vs
;

_main_loop
        call    resoft_vs        ; reset VS before every track

        btfss   mystat,_playstop ; is stop pressed?
        call    in_idle          ; yes: idle

        bcf     key_cmd,_keyb_ask

        call    seek_dir         ; seek next track

_loop_cluster
        call    reload_cluster   ; get address of new cluster into arq[3:0]
                                 ; read next cluster number

        ;>>>>>>>>> send contents of next cluster
        ; starting adderss is expected to be in arg[3..0]
        ; in 2006 largest card I had was 1Gb, so I don't know how to read
        ; big cards
        
        movf    sect_clus+1,W
        movwf   loop_cluster       ; clusters counter
_next512
        movlw   0x10
        movwf   loop_512           ; blocks counter
_next32
        bsf     INTCON,GIE
        btfss   bit_vs_dreq          ; wait VS ready for data
        goto    $-1
        bcf     INTCON,GIE

        ;
        ; send 32-byte block
        ;
        movlw    CMD17
        movwf    cmd_num

        bcf     bit_mmc_cs
         call    mmc_send_cmd
         call    mmc_wait_data

         movlw   d'32'
         movwf   loop_32
                                   ; going to send 32-byte block to VLSI
         movlw   0xff              ; We'll send data to VLSI and receive
         movwf   SSPBUF            ; from MMC simultaneously
         bsf     STATUS,RP0
         btfss   SSPSTAT,BF 
         goto    $-1               
         bcf     STATUS,RP0
_send32
         movf    SSPBUF,w          ; Read from MMC
         btfsc   mystat,_skip_id3  ; ID3v2 skip mode?
         call    skip_id3_tag      ; yes
         bsf	 bit_vs_bsync      
         movwf	 TXREG             ; start SDI transmission
         movlw   0xff
         movwf   SSPBUF            ; and MMC SPI receive in parallel
         bcf	 bit_vs_bsync
         nop
         nop
         nop
         nop

         decfsz   loop_32,f
         goto     _send32

         call     get_spi          ; crc
         call     get_spi          ; crc
         call     get_spi          ; empty
        bsf     bit_mmc_cs
        ; 32 bytes were transmitted

        bsf     INTCON,GIE
        movlw   d'32'              ; increment starting address
        addwf   arg0+3,f
        movlw   0x1
        btfsc   STATUS,C
        addwf   arg0+2,F
        btfsc   STATUS,C
        addwf   arg0+1,F
        btfsc   STATUS,C
        addwf   arg0+0,F
        bcf     INTCON,GIE

        movlw   d'32'              ; decrease num bytes left
        subwf   file_size+3,f
        movlw   0x1
        btfss   STATUS,C
        subwf   file_size+2,F
        btfss   STATUS,C
        subwf   file_size+1,F
        btfss   STATUS,C
        subwf   file_size+0,F
        btfss   STATUS,C
        goto    _finished

        decfsz  loop_512,F           ; is 512 bytes sent?
        goto    _next32                ; no - play next 32 from the same sector

        decfsz  loop_cluster,F           ; Have we send Sect_per_Clus sectors?
        goto    _next512               ; not yet, jump to the next sector
        ;<<<<<<<<<<<<< transmit finished

        btfss   key_cmd,_keyb_ask      ; Buttons were pressed?
        goto    _nokeybs
        bcf     key_cmd,_keyb_ask      ; yes - reset request flag
        btfss   key_cmd,_seek          ; seek?
        goto    _setvol                ; no
        goto    _main_loop             ; yes
_setvol
        btfsc   key_cmd,_volm          ; change volume?
        call    write_vol              ; yes

_nokeybs
        movlw   0xff                   ; is there next cluster?
        xorwf   next_clus+0,W
        btfss   STATUS,Z
        goto    _loop_cluster          
        movlw   0xF8
        andwf   next_clus+1,W
        xorlw   0xF8
        btfss   STATUS,Z
        goto    _loop_cluster          ; yes - continue with next one
                                       ; no, as all bits 0xFFF8 are set
_finished
        call    send_2048              ; to make sure VLSI has finished
        bcf     mystat,_seek_back        ; seek root folder down
        bsf     mystat,_seek_fwd

        goto    _main_loop
;
;  end of main loop
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;----------------------------------------------------------
;  replace id3_len[1:0] bytes from mmc with 0x0
;----------------------------------------------------------
skip_id3_tag
        movwf   tmpx                    ; save current byte
        movlw   0x01
        subwf   id3_len+1,f
        btfsc   STATUS,C
        retlw   0x00
        movlw   0x01
        subwf   id3_len,f
        btfsc   STATUS,C
        retlw   0x00
        bcf     mystat,_skip_id3
        movf    tmpx,w
        return

;----------------------------------------------------------
; in_idle - low power consumption mode
;----------------------------------------------------------

in_idle
        bsf     STATUS,RP0
        if orig_io
        movlw   0x87             ; remove pull-ups
        movwf   OPTION_REG       
        endif
        movlw   b'00100000'      ; only power button is input
        movwf   TRISB
        bcf     STATUS,RP0
        clrf    PORTB
        clrf    INTCON

_wrong_start
        call    delay_05s
        btfss   key1
        goto    $-1
        bsf     INTCON,RBIE     ; wake on RBIF
        movf    PORTB,W
        bcf     INTCON,RBIF
        sleep                   ; SLEEP
        nop
        bcf     INTCON,RBIE
        movf    PORTB,W
        bcf     INTCON,RBIF

        call    delay_05s
        call    delay_05s       

        movf    PORTB,W
        btfsc   key1            ; do not wake if button was pressed for less than a second
        goto    _wrong_start

        clrf    INTCON
        bsf     INTCON,T0IE
        bsf     INTCON,PEIE

        bsf     STATUS,RP0
        movlw   0x07
        movwf   OPTION_REG
        movlw   b'00111100'
        movwf   TRISB
        bcf     STATUS,RP0

        bsf     mystat,_playstop   ; play
        call    init_mmc           
        goto    rehard_vs          ; will 'return' us to caller

;----------------------------------
; say 'beep' 4 times
;----------------------------------
nocard
        movlw   d'82'
        call    beep
        movlw   d'73'
        call    beep
; say 'beep' 2 times
nocard2
        movlw   d'82'
        call    beep
        movlw   d'73'
        call    beep
        goto    $
        
;-----------------------------------------;
; delay_500us                             ;
;-----------------------------------------;
delay_500us
        movlw d'249'
        movwf tmpz

        nop
        nop
        decfsz tmpz,f
        goto   $-3

        return

;----------------------------------------;
; delay_01s                              ;
;----------------------------------------;
delay_01s
        movlw d'100'
        movwf tmpz+1

_00001
        call  delay_500us
        call  delay_500us
        decfsz tmpz+1,f
        goto   _00001

        return

delay_05s
        movlw d'250'
        movwf tmpz+1

_00002
        call  delay_500us
        call  delay_500us

        call  delay_500us
        call  delay_500us
        decfsz tmpz+1,f
        goto   _00002

        return

;--------------------------------------------------------;
;   seek_dir                                             ;
;   Find first cluster of the next track.                ;
;   returns: next_clus                                   ;
;            file_size                                   ;
;            id3v2 flag, if tag is found                 ;
;--------------------------------------------------------;
seek_dir
        movlw  0x4
        call   mmc_set_blk_size
_check_direction
        btfss  mystat,_seek_fwd         ; go forward?
        goto   _prev_track
                                       
        incf    track_num+1,f           ; yes
        btfsc   STATUS,Z
        incf    track_num,f

        movf    root_entry,w
        xorwf   track_num,w
        btfss   STATUS,Z
        goto    _check_entry
        movf    root_entry+1,w
        xorwf   track_num+1,w
        btfss   STATUS,Z
        goto    _check_entry


        clrf   track_num+0             ; end of root dir, move to the beginning 
        clrf   track_num+1             
        goto   _check_entry
        return
_prev_track
        btfss  mystat,_seek_back
        goto   _def_track
                                       ; move backward
        decfsz track_num+1,F
        goto   _check_entry
        movf   track_num+0,F
        btfsc  STATUS,Z
        goto   _lowlimit
        decf   track_num+0,F
        goto   _check_entry
_lowlimit
        movf   root_entry+1,w          ; 1st record of root dir, move to the last one
        movwf  track_num+1
        movf   root_entry,w
        movwf  track_num+0
        movlw  0x01
        subwf  track_num+1,f
        btfss  STATUS,Z
        decf   track_num,f
        goto   _check_entry

_def_track
                                       ; default search direction
        bsf    mystat,_seek_fwd        ; seek forward until next track

_check_entry
        movf   track_num+1,W           ; convert track number to offset
        movwf  arg0+2
        movf   track_num+0,W
        movwf  arg0+1
        clrf   arg0+0
        clrf   arg0+3
        bcf    STATUS,C
        movlw  0x3
        movwf  counter
_shift3
        rrf    arg0+1,F
        rrf    arg0+2,F
        rrf    arg0+3,F
        decfsz counter,F
        goto   _shift3

        add_24 root_base,arg0          ; calc record address
        movlw  CMD17
        movwf  cmd_num
        bcf     bit_mmc_cs
         call    mmc_send_cmd
         call    mmc_wait_data
         call    get_spi
         movwf   tmpz              ; zero byte
         call    get_spi
         call    get_spi
         movwf   tmpz+1            ; second byte (check for long name)
         call    get_spi
         call    get_spi
         call    get_spi
        bsf     bit_mmc_cs
        movlw   0xe5                   ; FAT magic. 0xE5 - deleted file
        xorwf   tmpz,W
        btfsc   STATUS,Z
        goto    _check_direction
        movf    tmpz,W                 ; 0x00 - empty
        btfsc   STATUS,Z
        goto    _check_direction

                                    ; check second byte of record
                                    ; If this record is VFAT long file name
                                    ; the character is going to be non alphabetic
        movf    tmpz+1,W            ; for english names (unicode 0x00??)
        btfsc   STATUS,Z
        goto    _check_direction

        movlw   0x04
        xorwf   tmpz+1,w            ; for russian (unicode 0x04??)
        btfsc   STATUS,Z
        goto    _check_direction
                                    ; ^^ This code should be rewritten as it works
                                    ; incorrectly for other possible unicode characters.
                                    
        movlw   0x18                ; read first cluster
        iorwf   arg0+3,F
        bcf     bit_mmc_cs
         call    mmc_send_cmd
         call    mmc_wait_data
         call    get_spi
         call    get_spi

         call    get_spi
         movwf   next_clus+1
         call    get_spi
         movwf   next_clus+0
         call    get_spi
         call    get_spi
        bsf     bit_mmc_cs

        movlw   0xE0                ; and file size
        andwf   arg0+3,F
        movlw   0x1c
        iorwf   arg0+3,F
        bcf     bit_mmc_cs
         call    mmc_send_cmd
         call    mmc_wait_data
         call    get_spi
         movwf   file_size+3
         call    get_spi
         movwf   file_size+2
         call    get_spi
         movwf   file_size+1
         call    get_spi
         movwf   file_size+0
         call    get_spi
         call    get_spi
        bsf     bit_mmc_cs

                                    ; check ID3v2 tag
        movlw   0x0b
        call    mmc_set_blk_size

        movf    next_clus,w         ; store 1st cluster
        movwf   tmpx
        movf    next_clus+1,w
        movwf   tmpx+1

        call    reload_cluster

        bcf     mystat,_skip_id3
        movlw   CMD17
        movwf   cmd_num
        bcf     bit_mmc_cs
         call     mmc_send_cmd
         call     mmc_wait_data
         call     get_spi
         movwf    tmpy
         call     get_spi
         movwf    tmpy+1
         call     get_spi
         movwf    tmpz
         call     get_spi  ; 00
         call     get_spi  ; 03
         call     get_spi  ; flags
         call     get_spi  ; size
         call     get_spi  ; size+1
         call     get_spi  ; size+2
         movwf    id3_len
         call     get_spi  ; size+3
         movwf    id3_len+1

         bcf      STATUS,C
         rrf      id3_len
         btfsc    STATUS,C
         bsf      id3_len+1,7

         movlw    d'29'
         movwf    tmpz+1

         call     get_spi
         decfsz   tmpz+1,f
         goto     $-2

        bsf     bit_mmc_cs
        movlw   'I'
        xorwf   tmpy,w
        btfss   STATUS,Z
        goto    _noID3
        movlw   'D'
        xorwf   tmpy+1,w
        btfss   STATUS,Z
        goto    _noID3
        movlw   '3'
        xorwf   tmpz,w
        btfss   STATUS,Z
        goto    _noID3
                                    ; ID3v2 found.
        bsf     mystat,_skip_id3
_noID3
        movf    tmpx,w            ; restore 1st cluster
        movwf   next_clus
        movf    tmpx+1,w
        movwf   next_clus+1

        return
        
;--------------------------------------------------------;
;   Function: reload_cluster                             ;
;   Calculate address of next cluster using next_clus    ;
;   and  put into arg0. Read next cluster number         ;
;   in chain.                                            ;
;--------------------------------------------------------;
reload_cluster
        movlw   0x2
        call    mmc_set_blk_size
        movf    next_clus+1,W
        movwf   msrc+2
        movf    next_clus+0,W
        movwf   msrc+1
        movf    sect_clus,W
        movwf   mmul

        movlw   CMD17
        movwf   cmd_num

        movf    next_clus+1,W
        movwf   arg0+3
        movf    next_clus+0,W
        movwf   arg0+2
        clrf    arg0+1
        clrf    arg0+0
        bcf     STATUS,C
        rlf     arg0+3,F
        rlf     arg0+2,F
        rlf     arg0+1,F

        add_24  fat_base,arg0

        bcf     bit_mmc_cs
         call    mmc_send_cmd
         call    mmc_wait_data
         ;btfsc   mystat,_block_err
         ;goto    nocard
         call    get_spi
         movwf   next_clus+1
         call    get_spi
         movwf   next_clus+0
         call    get_spi
         call    get_spi
         call    get_spi
        bsf     bit_mmc_cs

        movlw   d'32'
        call    mmc_set_blk_size

        ; put next cluster address into arg0[0..3]
        call    mult
        clrf    arg0+3
        movf    clus_base+2,W
        movwf   arg0+2
        movf    clus_base+1,W
        movwf   arg0+1
        movf    clus_base+0,W
        movwf   arg0+0

        add_24  mrslt,arg0

        return
;--------------------------------------------------------;
;   Function: mult                                       ;
;   mrslt[0..2] = msrc[1..2] * mmul                      ;
;--------------------------------------------------------;
mult
        movf    mmul,w
        btfsc   STATUS,Z
        goto    _mul2_256

        clrf    msrc+0
        clrf    mrslt+0
        clrf    mrslt+1
        clrf    mrslt+2

        movlw   0x8
        movwf   counter
_addnextpow
        bcf     STATUS,C
        rrf     mmul,F
        btfss   STATUS,C
        goto    _shift2

        add_24  msrc,mrslt
_shift2
        bcf     STATUS,C
        rlf     msrc+2,F
        rlf     msrc+1,F
        rlf     msrc+0,F
        decfsz  counter,F
        goto    _addnextpow

        return

_mul2_256
        clrf    mrslt+2
        movf    msrc+2,w
        movwf   mrslt+1
        movf    msrc+1,w
        movwf   mrslt+0
        return
;-------------------------------------------------
; Function: init_mmc
; Start mmc reset, wait until mmc is ready
;-------------------------------------------------
init_mmc
        movlw    8
        movwf    tmpz
_res_lp
        movlw    CMD0
        movwf    cmd_num
        clrw
        movwf    arg0
        movwf    arg0+1
        movwf    arg0+2
        movwf    arg0+3

        bcf     bit_mmc_cs
        call    mmc_send_cmd        ; perform reset
        bsf     bit_mmc_cs

        movlw   CMD1
        movwf   cmd_num
        bcf     bit_mmc_cs
        call    mmc_send_cmd        ; send "init"
        bsf     bit_mmc_cs

        btfsc   answer,0        ; card should respond with idle_state flag
        goto    _idle_mmc       
                                ; not in idle? try to reset again
        decfsz  tmpz,F
        goto    _res_lp


        goto    nocard          ; unable to get idle_state flag after 8-th try
                                ; probably the card is not present


_idle_mmc                       ; ok, let's wait until the card is ready
        clrf    tmpz+1          
_svv                            ; 
                                ; Shall wait until idle_state becomes zero
        movlw   CMD1            ; loop 65536 times
        movwf   cmd_num
        clrf    tmpz
_idle_
        bcf     bit_mmc_cs
        call    mmc_send_cmd
        bsf     bit_mmc_cs
        movf    answer,w
        btfsc   STATUS,Z
        return                  ; ready!
        decfsz  tmpz,f
        goto    _idle_

        decfsz  tmpz+1,f
        goto    _svv

        goto    nocard          ; oops, not ready =(

;---------------------------------------------------------------------;
;   считывает адреса FAT,Root DIR,       ;
;   адрес второго кластера минус размер двух кластеров, число         ;
;   (секторов на кластер * 2)                                         ;
;   возвращает: fat_base,root_base,clus_base,sect_clus                ;
;---------------------------------------------------------------------;

init_fat
        movlw   0x3
        call    mmc_set_blk_size              ; установить размер считываемого блока
                                      ; 3 байта

        movlw   CMD17                 ; есть MBR?
        movwf   cmd_num
        clrf    arg0
        clrf    arg0+1
        clrf    arg0+2
        clrf    arg0+3

        bcf     bit_mmc_cs
         call    mmc_send_cmd                ; посылаем команду чтения
         call    mmc_wait_data
         btfsc   mystat,_block_err
         goto    nocard2
                                         ; адрес задан в виде номера сектора
         call    get_spi                 ; запись в формате LSB
         movwf   arg0+2
         call    get_spi
         movwf   arg0+1
         call    get_spi
         movwf   arg0+0
         call    get_spi                 ; 2 байта crc игнорируем
         call    get_spi
        bsf     bit_mmc_cs

        movf    arg0+2,w
        btfss   STATUS,Z
        goto    _no_MBR

        movlw   CMD17                    ; считаем адрес первого блока раздела
        movwf   cmd_num
        clrf    arg0
        clrf    arg0+1                   ;линейный адрес первого блока раздела
        movlw   0x01                     ;сидит в partition table 0x1BE(+0x8)
        movwf   arg0+2
        movlw   0xC6
        movwf   arg0+3

        bcf     bit_mmc_cs
         call    mmc_send_cmd                ; посылаем команду чтения
         call    mmc_wait_data
         btfsc   mystat,_block_err
         goto    nocard2
                                         ; адрес задан в виде номера сектора
         call    get_spi                 ; запись в формате LSB
         movwf   arg0+2
         call    get_spi
         movwf   arg0+1
         call    get_spi
         movwf   arg0+0
         call    get_spi                 ; 2 байта crc игнорируем
         call    get_spi
        bsf     bit_mmc_cs

        clrf    arg0+3
        bcf     STATUS,C
        rlf     arg0+2,F           ; сдвигаем на 2 для получения лин адр
        movf    arg0+2,W
        movwf   fat_base+2         ; главной записи раздела
        rlf     arg0+1,F           ;
        movf    arg0+1,W
        movwf   fat_base+1
        rlf     arg0+0,F
        movf    arg0+0,W
        movwf   fat_base+0
        goto    _is_MBR
        ;>>>>>>>>>>>>>>>>>>>>>>>>
_no_MBR
        clrf    arg0+0
        clrf    arg0+1
        clrf    arg0+2
        clrf    arg0+3

        clrf    fat_base+0
        clrf    fat_base+1
        clrf    fat_base+2
        clrf    fat_base+3
        ;<<<<<<<<<<<<<<<<<<<<<<<<
_is_MBR
                                 ; теперь размер фат и число секоров на
                                 ; кластер
        movlw   CMD17
        movwf   cmd_num
        movlw   0x0D
        movwf   arg0+3
        bcf     bit_mmc_cs
         call    mmc_send_cmd                ; посылаем команду чтения
         call    mmc_wait_data
         btfsc   mystat,_block_err
         goto    nocard2
         call    get_spi
         movwf   sect_clus
         movwf   sect_clus+1
         call    get_spi
         movwf   tmpy+1
         call    get_spi
         movwf   tmpy+0
         call    get_spi
         call    get_spi
        bsf     bit_mmc_cs

                                   ; fat_sect.adr=boot_sect.adr+0x200
        bcf     STATUS,C
        rlf     tmpy+1,f
        rlf     tmpy,f

        movf    tmpy+1,w
        addwf   fat_base+2,F
        btfss   STATUS,C
        goto    _fatisset
        incf    fat_base+1,F
        btfss   STATUS,Z
        goto    _fatisset
        incf    fat_base+0,f
        movf    tmpy,w
        addwf   fat_base+1,F
        btfss   STATUS,C
        goto    _fatisset
        incf    fat_base+0,f
_fatisset

        btfss   sect_clus,7
        goto    _mul_sect
        movlw   0x80
        xorwf   sect_clus,W
        btfss   STATUS,Z
        goto    nocard2

_mul_sect
        bcf     STATUS,C
        rlf     sect_clus,F

        movlw   CMD17
        movwf   cmd_num
        movlw   0x16
        movwf   arg0+3
        bcf     bit_mmc_cs
         call    mmc_send_cmd                ; посылаем команду чтения
         call    mmc_wait_data
         btfsc   mystat,_block_err
         goto    nocard2
         call    get_spi                 ; сохраним число секторов
         movwf   root_base+2             ; в каждой фат
         call    get_spi
         movwf   root_base+1
         call    get_spi
         call    get_spi
         call    get_spi
        bsf     bit_mmc_cs


         clrf    root_base              ; преобразуем число секторов в
         bcf     STATUS,C               ; число байт
         rlf     root_base+2,F
         rlf     root_base+1,F
         rlf     root_base+0,F
         bcf     STATUS,C
         rlf     root_base+2,F
         rlf     root_base+1,F
         rlf     root_base+0,F

         add_24  fat_base,root_base
; смещение корневого каталога есть
; теперь найдем второй кластер

         movlw   CMD17
        movwf   cmd_num
        movlw   0x11
        movwf   arg0+3
        bcf     bit_mmc_cs
         call    mmc_send_cmd                ; посылаем команду чтения
         call    mmc_wait_data
         btfsc   mystat,_block_err
         goto    nocard2
         call    get_spi
         movwf   root_entry+1
         movwf   tmpy+1
         call    get_spi
         movwf   root_entry+0
         movwf   tmpy
         call    get_spi
         call    get_spi
         call    get_spi
        bsf     bit_mmc_cs

         movf    root_base+1,W
         movwf   clus_base+1
         movf    root_base+0,W
         movwf   clus_base+0

         bcf     STATUS,Z
         rrf     tmpy,f
         rrf     tmpy+1,f
         rrf     tmpy,f
         rrf     tmpy+1,f
         rrf     tmpy,f
         rrf     tmpy+1,w


         addwf   root_base+2,W
         movwf   clus_base+2
         btfss   STATUS,C
         goto    _sub_2clus
         incf    clus_base+1,F
         btfsc   STATUS,Z
         incf    clus_base+0,F
                                        ; теперь в clus_base адрес второго
                                        ; кластера. Для упрощения лучше
_sub_2clus                              ; сделать указание на нулевой.

         movlw   0x2
         movwf   msrc+2
         clrf    msrc+1
         movf    sect_clus,W
         movwf   mmul
         call    mult

         movf    mrslt+2,W
         subwf   clus_base+2,F
         btfsc   STATUS,C
         goto    _nosub
         decf    clus_base+1,F
         btfsc   STATUS,Z
         decf    clus_base+0,F
_nosub
         movf    mrslt+1,W
         subwf   clus_base+1,F
         btfss   STATUS,C
         decf    clus_base+0,F
         movf    mrslt+0,W
         subwf   clus_base+0,F
        return

;--------------------------------------------------------------;
;    Функция: mmc_wait_data                                        ;
;    Ожидание начала передачи блока данных из ММС              ;
;    В случае ошибки устанавливается mystat,_block_err         ;
;--------------------------------------------------------------;
mmc_wait_data
         bcf     mystat,_block_err
         movlw   0x80
         movwf   counter+1
_wait_rcv
_wait_rcv2
         call    get_spi
         movwf   answer
         xorlw   0xfe
         btfsc   STATUS,Z                ; wait data
         return
         movlw   0xE0
         andwf   answer,W

         btfsc   STATUS,Z
         goto    _err_rcv
         decfsz  counter,F
         goto    _wait_rcv2

         decfsz  counter+1,f
         goto    _wait_rcv

_err_rcv
         bsf     mystat,_block_err
         return

;---------------------------------------------------------;
;   Function: mmc_set_blk_size                            ;
;   set ММС data block size                               ;
;   param W - size to set                                 ;
;---------------------------------------------------------;
mmc_set_blk_size
        movwf    arg0+3
        movlw    CMD16
        movwf    cmd_num

        clrw
        movwf    arg0+2
        movwf    arg0+1
        movwf    arg0

        bcf      bit_mmc_cs
        call     mmc_send_cmd
        bsf      bit_mmc_cs

        return
;--------------------------------------------------------;
;   Функция: mmc_send_cmd                                    ;
;   Отправляет фрейм команды в ММС                       ;
;   Вход: cmd_num - номер команды (константы CMDxx),     ;
;         arg0[0..3] - аргумент команды                  ;
;   Возвращает: answer - ответ на команду                ;
;--------------------------------------------------------;
mmc_send_cmd
        movlw   0x2
        movwf   counter+1
_try_cmd
        call    get_spi
        call    get_spi
        movlw   0x95
        movwf   crc

        movlw   0xff
        movwf   SSPBUF

        movlw   cmd_num
        movwf   FSR
        movlw   0x06
        movwf   counter
        call    get_spi
_nextincmd
        movf    INDF,W
        call    spi_out
        incf    FSR,F
        decfsz  counter,F
        goto    _nextincmd

        movlw   0x80
        movwf   counter
_wait_notff
        call    get_spi
        movwf   answer
        btfss   answer,7
        return

        decfsz  counter,F
        goto    _wait_notff

        decfsz  counter+1,F
        goto    _try_cmd
        goto    nocard2


;------------------------------------------------------------;
;   Функция: test_keys                                       ;
;   Проверка состояния клавиш. Если что-то нажато, то        ;
;   возможна установка бита _keyb_ask и изменение регистров  ;
;   mystat, key_cmd или vol                                  ;
;------------------------------------------------------------;
test_keys
        movf   keyb_count,F           ; клавиши проверяются если соблюдены
        btfsc  STATUS,Z               ; 2 условия:
        goto   _can_watch             ; 1)прошло >8 прерываний с прошлого
        decf   keyb_count,F           ;   нажатия на клавишу(см. конец процедуры)
        return
_can_watch

        btfsc  key_cmd,_keyb_ask      ; 2)предыдущее нажатие обработано
        return

        clrf   key_cmd

        btfsc  key3                    ; is shift pressed?
        goto   _0not_pressed           ; no
                                       ; yes
        btfsc  key1
        goto   _t00
        movlw  0x80                    ; bass/treble
        xorwf  mystat,F
        bsf    key_cmd,_volm
        goto   _send_isr

_t00
        btfss  mystat,_playstop
        return

        btfsc  key0                    ; key0 vs key3
        goto   _t02
        bsf    key_cmd,_seek           ; forward
        bsf    mystat,_seek_fwd
        bcf    mystat,_seek_back
        goto   _send_isr
_t02
        btfsc  key2                    ; key2 vs key3
        goto   _end_isr
        bsf    key_cmd,_seek           ; back
        bsf    mystat,_seek_back
        bcf    mystat,_seek_fwd
        goto   _send_isr

_0not_pressed                          ; shift is not pressed
        btfsc  key0
        goto   _t11
        movlw  sound_step              ; volume up
        subwf  vol,F
        btfss  STATUS,C
        clrf   vol
        bsf    key_cmd,_volm
        goto   _send_isr
_t11

        btfsc  key1                    
        goto   _t12
        bcf    mystat,_seek_fwd         ; stop
        bcf    mystat,_seek_back
        bsf    key_cmd,_seek
        bcf    mystat,_playstop
        movlw  0x40
        movwf  keyb_count
        bsf    key_cmd,_keyb_ask
        return
_t12
        btfsc  key2
        goto   _end_isr
        bsf    key_cmd,_volm           ; volume down
        movlw  sound_step
        addwf  vol,F
        btfss  STATUS,C
        goto   _send_isr
        clrf   vol
        decf   vol,F
        goto   _send_isr
_send_isr                              ; some button was pressed
        movlw  0x8                     
        movwf  keyb_count              ; debounce
        bsf    key_cmd,_keyb_ask
_end_isr
         return
;@@@@@@@; THE
             END ;@@@@@@@;





;***********************************
;	Author: Alexey Katichev
;***********************************
