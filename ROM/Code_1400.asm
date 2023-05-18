;-----------------------------------------------------------
;      RIOT RAM (zero-page) labels
;-----------------------------------------------------------

ram_C0          = $c0
ram_C1          = $c1
                   

;***********************************************************
;      Code
;***********************************************************

    RORG    $1400                   ;               code for slice 1/3

L1400 SUBROUTINE
    jmp     .jump                   ;3   =   3 *

L1403    
    .word   LoadData                ; $1403 (D)
    
.jump
    ldx     #$00                    ;2         
    stx     ram_CF                  ;3         
    stx     COLUBK                  ;3         
    lda     #RED|$3                 ;2         
    sta     COLUPF                  ;3   
;L100f?
    inx                             ;2         
    stx     CTRLPF                  ;3              = 1       
    lda     animState               ;3              animation already started?
    bne     .setupPF                ;2/3             yes, continue animation
    inc     animState               ;5               no, start animation
    inx                             ;2         
    stx     animDelay               ;3              = 2   
    lda     #$1b                    ;2         
    sta     xCurtain                ;3         
    jsr     L148c                   ;6         
    rts                             ;6   =  50 
    
.setupPF
; coarse setup of PF registers:
    dex                             ;2              -> X = 0  
    stx     PF0                     ;3         
    stx     PF1                     ;3         
    stx     PF2                     ;3         
    dex                             ;2         
    ldy     xCurtain                ;3         
    cpy     #$18                    ;2         
    bpl     L1439                   ;2/3       
    cpy     #$10                    ;2         
    bpl     L1437                   ;2/3       
    stx     PF1                     ;3   =  27 
L1437
    stx     PF0                     ;3   =   3 
L1439
; no clue...:
    lda     ram_DB                  ;3         
    bmi     L1443                   ;2/3       
    jsr     L169f                   ;6         
    jsr     L14e0                   ;6   =  17 
L1443
    jsr     L149e                   ;6         
    rts                             ;6   =  12 
    
AnimLoad SUBROUTINE
    dec     animDelay               ;5         
    bne     .exit                   ;2/3       
; slowly close the PF curtain:
    lda     #$04                    ;2         
    sta     animDelay               ;3         
    ldx     xCurtain                ;3         
    ldy     #$00                    ;2              update PF0    
    cpx     #$18                    ;2         
    bpl     .pfOK                   ;2/3       
    dec     animDelay               ;5         
    iny                             ;2              update PF1
    cpx     #$10                    ;2         
    bpl     .pfOK                   ;2/3       
    iny                             ;2   =  34      update PF2
.pfOK
    stx     AUDF0                   ;3         
    cpx     #$08                    ;2         
    bpl     .indexOK                ;2/3       
    ldx     #$08                    ;2   =   9      X >= 8
.indexOK
    lda     CurtainGfx-8,x          ;4         
    sta.wy  PF0,y                   ;5              PF0|1|2   
    lda     #$01                    ;2
    sta     AUDC0                   ;2
    lda     #$0a                    ;2
    sta     AUDV0                   ;3         
    dec     xCurtain                ;5   =  23 
.exit
    rts                             ;6   =   6 

CurtainGfx    
    .byte   %11111111 ; |********|            $1478 (P)
    .byte   %01111111 ; | *******|            $1479 (P)
    .byte   %00111111 ; |  ******|            $147a (P)
    .byte   %00011111 ; |   *****|            $147b (P)
    .byte   %00001111 ; |    ****|            $147c (P)
    .byte   %00000111 ; |     ***|            $147d (P)
    .byte   %00000011 ; |      **|            $147e (P)
    .byte   %00000001 ; |       *|            $147f (P)
    .byte   %11111111 ; |********|            $1480 (P)
    .byte   %11111110 ; |******* |            $1481 (P)
    .byte   %11111100 ; |******  |            $1482 (P)
    .byte   %11111000 ; |*****   |            $1483 (P)
    .byte   %11110000 ; |****    |            $1484 (P)
    .byte   %11100000 ; |***     |            $1485 (P)
    .byte   %11000000 ; |**      |            $1486 (P)
    .byte   %10000000 ; |*       |            $1487 (P)
    .byte   %11111111 ; |********|            $1488 (P)
    .byte   %01111111 ; | *******|            $1489 (P)
    .byte   %00111111 ; |  ******|            $148a (P)
    .byte   %00011111 ; |   *****|            $148b (P)

L148c SUBROUTINE
    lda     #$0a                    ;2         
    sta     ram_C7                  ;3         
    lda     #$00                    ;2         
    sta     ram_D7                  ;3   =  10 
.outerLoop
    lda     ram_D7                  ;3         
    bne     L149e                   ;2/3       
    lda     GL_480                  ;4              switch bank 3 into slice 0
    jsr     L12f1                   ;6   =  15      code in bank 3
L149e
    lda     GL_485                  ;4              switch bank ? into slice 0
    jsr     $100f                   ;6              code in bank ? TODO!
    ldx     GL_481                  ;4              switch bank ? into slice 0
    bcs     L14af                   ;2/3       
    jsr     L16c0                   ;6         
    jmp     L149e                   ;3   =  25 
    
L14af
    dec     ram_C7                  ;5         
    bpl     .outerLoop              ;2/3       
    lda     #$00                    ;2         
    sta     animState               ;3              reset animation state 
    sec                             ;2         
    rts                             ;6   =  20 
    
L14b9 SUBROUTINE
    jsr     L14f0                   ;6         
    jsr     ResetCRC                ;6         
    lda     #$aa                    ;2         
    jsr     TransferByte            ;6         
    lda     ram_D0                  ;3         
    bne     L14cc                   ;2/3       
    dec     ram_D0                  ;5         
    lda     ram_D0                  ;3   =  33 
L14cc
    jsr     SendByte                ;6         
    lda     ram_D1                  ;3         
    jsr     SendByte                ;6         
    rts                             ;6   =  21 
    
SendCRC SUBROUTINE
    lda     crcHi                   ;3         
    jsr     TransferByte            ;6         
    lda     crcLo                   ;3         
    jsr     TransferByte            ;6         
    rts                             ;6   =  24 
    
L14e0 SUBROUTINE
    lda     GL_START_PULSE          ;4         
    jsr     L14fd                   ;6         
    bcc     .exit                   ;2/3       
    lda     ram_C7                  ;3         
    sec                             ;2         
    sbc     #$04                    ;2         
    sta     ram_C7                  ;3   =  22 
.exit
    rts                             ;6   =   6 
    
L14f0 SUBROUTINE
    jsr     L1501                   ;6         
    lda     GL_CBB                  ;4         
    jsr     L1533                   ;6         
    jsr     L1533                   ;6         
    rts                             ;6   =  28 
    
L14fd SUBROUTINE
    ldy     #$00                    ;2         
    beq     L1503                   ;3 =     5

L1501 SUBROUTINE
    ldy     #$01                    ;2   =   2 
L1503
    lda     #$32                    ;2         
    sta     ram_DE                  ;3         
    ldx     L152f,y                 ;4   =   9 
L150a
    lda     #$ff                    ;2         
    sta     T1024T                  ;4   =   6 
L150f
    lda     TIM8T                   ;4         
    bmi     L1524                   ;2/3       
    lda     L1ff8                   ;4         
    and     #$40                    ;2         
    eor     L1531,y                 ;4         
    beq     L152a                   ;2/3       
    ldx     L152f,y                 ;4         
    jmp     L150f                   ;3   =  25 
    
L1524
    dec     ram_DE                  ;5         
    bne     L150a                   ;2/3       
    sec                             ;2         
    rts                             ;6   =  15 
    
L152a
    dex                             ;2         
    bne     L150f                   ;2/3       
    clc                             ;2         
    rts                             ;6   =  12 
    
L152f
    .byte   $00,$32                         ; $152f (D)
L1531
    .byte   $40,$00                         ; $1531 (D)
    
L1533 SUBROUTINE
    pha                             ;3         
    lda     #$75                    ;2         
    sta     T1024T                  ;4   =   9 
L1539
    lda     TIM8T                   ;4         
    bpl     L1539                   ;2/3       
    pla                             ;4         
    rts                             ;6   =  16 
    
TransferByte SUBROUTINE
    sta     ram_CE                  ;3         
    pha                             ;3         
    txa                             ;2         
    pha                             ;3         
    tya                             ;2         
    pha                             ;3         
    lda     #$ff                    ;2         
    sta     TIM8T                   ;4         
    clc                             ;2         
    rol     ram_CE                  ;5         
    ldy     #10                     ;2   =  31 
.loop
    lda     TIM8T                   ;4         
    bpl     .loop                   ;2/3       
    lda     #$85                    ;2         
    sta     TIM8T                   ;4         
    ror     ram_CE                  ;5         
    bcs     .bitSet                 ;2/3       
    lda     GL_CBA                  ;4              bit transfer?
    jmp     .bitClear               ;3   =  26 
    
.bitSet
    lda     GL_CBB                  ;4   =   4      bit transfer?
.bitClear
    dey                             ;2         
    sec                             ;2         
    bne     .loop                   ;2/3 =   6 
.waitTim
    lda     TIM8T                   ;4         
    bpl     .waitTim                ;2/3       
    pla                             ;4         
    tay                             ;2         
    pla                             ;4         
    tax                             ;2         
    pla                             ;4         
    rts                             ;6   =  28 
    
LoadData SUBROUTINE
    lda     ram_CF                  ;3        
    bne     L1581                   ;2/3      
    jsr     L15b9                   ;6         
    bcc     L1581                   ;2/3       
    rts                             ;6   =  19 
    
L1581
    ldy     #8                      ;2              8 bits
    lda     #$00                    ;2         
    sta     ram_CE                  ;3   =   7 
.loop
    ldx     TIM8T                   ;4         
    bpl     .loop                   ;2/3       
    lda     #$fb                    ;2         
    dec     ram_C1                  ;5         
    bne     L1597                   ;2/3       
    ldx     ram_E0                  ;3         
    lda     L1669,x                 ;4   =  22 
L1597
    clc                             ;2         
    adc     TIM1T                   ;4         
    sta     TIM1T                   ;4         
    lda     ram_C1                  ;3         
    bpl     .loop                   ;2/3       
    ldx     ram_E0                  ;3         
    lda     L1665,x                 ;4         
    sta     ram_C1                  ;3         
    bit     L1ff8                   ;4         
    clc                             ;2         
    bpl     L15b0                   ;2/3       
    sec                             ;2   =  35 
L15b0
    ror     ram_CE                  ;5         
    dey                             ;2         
    bne     .loop                   ;2/3       
    lda     ram_CE                  ;3         
    clc                             ;2         
    rts                             ;6   =  20 
    
L15b9 SUBROUTINE
    lda     #100                    ;2         
    sta     ram_C0                  ;3   =   5 
.loop
    lda     #$ba                    ;2         
    sta     TIM64T                  ;4         
    lda     #$00                    ;2         
    sta     ram_CE                  ;3         
    sta     ram_E0                  ;3         
    lda     #$07                    ;2         
    sta     ram_DE                  ;3   =  19 
L15cc
    lda     TIM8T                   ;4         
    bpl     L15d7                   ;2/3       
    dec     ram_C0                  ;5         
    bne     .loop                   ;2/3       
    sec                             ;2         
    rts                             ;6   =  21 
    
L15d7
    bit     L1ff8                   ;4         
    bvc     L15cc                   ;2/3       
    bmi     L15cc                   ;2/3       
    ldy     #$fe                    ;2         
    sty     TIM8T                   ;4   =  14 
L15e3
    lda     TIM1T                   ;4         
    eor     #$ff                    ;2         
    cmp     #$32                    ;2         
    bmi     L15e3                   ;2/3 =  10 
L15ec
    lda     TIM8T                   ;4         
    bmi     .loop                   ;2/3       
    ldx     TIM1T                   ;4         
    lda     L1ff8                   ;4         
    eor     ram_CE                  ;3         
    and     #$80                    ;2         
    beq     L15ec                   ;2/3       
    sty     TIM8T                   ;4         
    lda     L1ff8                   ;4         
    sta     ram_CE                  ;3         
    txa                             ;2         
    eor     #$ff                    ;2         
    tax                             ;2         
    lsr                             ;2         
    cmp     #$19                    ;2         
    bmi     .loop                   ;2/3!      
    cmp     #$73                    ;2         
    bpl     .loop                   ;2/3!      
    txa                             ;2         
    ldx     ram_E0                  ;3         
    beq     L161b                   ;2/3       
    clc                             ;2         
    adc     ram_E0                  ;3         
    ror                             ;2   =  62 
L161b
    sta     ram_E0                  ;3         
    dec     ram_DE                  ;5         
    bne     L15e3                   ;2/3!      
    ldx     #$03                    ;2         
    lda     ram_E0                  ;3   =  15 
L1625
    cmp     FromTbl,x               ;4         
    bmi     L1657                   ;2/3       
    cmp     ToTbl,x                 ;4         
    bpl     L1657                   ;2/3       
    stx     ram_E0                  ;3         
    lda     L166d,x                 ;4         
    clc                             ;2         
    adc     TIM1T                   ;4         
    sta     TIM8T                   ;4         
    inc     ram_CF                  ;5         
    jsr     L17c8                   ;6   =  40 
L1640
    lda     TIM8T                   ;4         
    bpl     L1640                   ;2/3       
    ldx     ram_E0                  ;3         
    lda     L1665,x                 ;4         
    sta     ram_C1                  ;3         
    lda     #$fb                    ;2         
    clc                             ;2         
    adc     TIM1T                   ;4         
    sta     TIM1T                   ;4         
    clc                             ;2         
    rts                             ;6   =  36 
    
L1657
    dex                             ;2         
    bpl     L1625                   ;2/3       
    jmp     .loop                   ;3   =   7 
    
FromTbl
    .byte   $50,$69,$7c,$a0                 ; $165d (D)
ToTbl
    .byte   $68,$7b,$9f,$cd                 ; $1661 (D)
L1665
    .byte   $02,$03,$04,$05                 ; $1665 (D)
L1669
    .byte   $f6,$8f,$28,$f1                 ; $1669 (D)
L166d
    .byte   $2b,$34,$41,$5b                 ; $166d (D)
    
L1671 SUBROUTINE
    lda     ram_DB                  ;3         
    jsr     Div16                   ;6         
    cmp     ram_CC                  ;3         
    bpl     L167e                   ;2/3       
    ldy     #$00                    ;2         
    sty     ram_CD                  ;3   =  19 
L167e
    sta     ram_CC                  ;3         
    tay                             ;2         
    lda     L1697,y                 ;4         
    and     ram_CD                  ;3         
    bne     .exit                   ;2/3       
    lda     L1697,y                 ;4         
    ora     ram_CD                  ;3         
    sta     ram_CD                  ;3         
    jsr     AnimLoad                ;6         
    lda     #$0a                    ;2         
    sta     ram_C7                  ;3   =  35 
.exit
    rts                             ;6   =   6 
    
L1697
    .byte   $01,$02,$04,$08,$10,$20,$40,$80 ; $1697 (D)
    
L169f SUBROUTINE
    jsr     L14b9                   ;6         
    lda     #$10                    ;2         
    jsr     SendByte                ;6         
    lda     #$00                    ;2         
    jsr     SendByte                ;6         
    lda     ram_CD                  ;3         
    jsr     SendByte                ;6         
    lda     #$00                    ;2         
    jsr     SendByte                ;6         
    jsr     SendByte                ;6         
    jsr     SendCRC                 ;6         
    jsr     L14e0                   ;6         
    rts                             ;6   =  63 
    
L16c0 SUBROUTINE
    bit     ram_C8                  ;3         
    bmi     L16e0                   ;2/3       
    ldy     #$00                    ;2         
    sta     (dataPtr2),y            ;6         
    jsr     UpdateCRC               ;6         
    lda     ram_D7                  ;3         
    cmp     ram_D0                  ;3         
    bne     L16ea                   ;2/3       
    inc     dataPtr2                ;5         
    bne     L16d7                   ;2/3       
    inc     dataPtr2+1              ;5   =  39 
L16d7
    dec     ram_C9                  ;5         
    beq     L16dc                   ;2/3       
    rts                             ;6   =  13 
    
L16dc
    sec                             ;2         
    ror     ram_C8                  ;5         
    rts                             ;6   =  13 
    
L16e0
    ldy     #$00                    ;2              hi
    bvc     L16e5                   ;2/3       
    iny                             ;2   =   6      lo
L16e5
    cmp.wy  crcLst,y                ;4         
    beq     L16ed                   ;2/3 =   6 
L16ea
    dec     ram_CF                  ;5         
    rts                             ;6   =  11 
    
L16ed
    bvc     L16dc                   ;2/3       
    lda     ram_C8                  ;3         
    and     #$0f                    ;2         
    beq     L16f8                   ;2/3       
    jmp     L171e                   ;3   =  12 
    
L16f8
    lda     ram_D8                  ;3         
    cmp     ram_D1                  ;3         
    bne     L16ea                   ;2/3       
    lda     ram_DA                  ;3         
    beq     L171e                   ;2/3       
    sta     ram_C9                  ;3         
    lda     ram_DB                  ;3         
    and     #$0f                    ;2         
    tax                             ;2         
    lda     GL_8A0,x                ;4         
    lda     ram_DD                  ;3         
    eor     #>$1800                 ;2         
    sta     dataPtr2+1              ;3         
    lda     ram_DC                  ;3         
    sta     dataPtr2                ;3         
    lda     #$08                    ;2         
    sta     ram_C8                  ;3         
    jsr     ResetCRC                ;6         
    rts                             ;6   =  58 
    
L171e
    dec     ram_CF                  ;5         
    lda     ram_D9                  ;3         
    and     #$f0                    ;2         
    cmp     #$30                    ;2         
    bne     L1738                   ;2/3       
    jsr     L1671                   ;6         
    lda     ram_D9                  ;3         
    and     #$04                    ;2         
    beq     .exit                   ;2/3 =  27 
L1731
    jsr     L169f                   ;6         
    jsr     L14e0                   ;6   =  12 
.exit
    rts                             ;6   =   6 
    
L1738
    cmp     #$40                    ;2         
    bne     .exit                   ;2/3       
    lda     #$01                    ;2         
    sta     ram_CD                  ;3         
    lda     ram_DB                  ;3         
    bpl     L1747                   ;2/3       
    jsr     L169f                   ;6   =  20 
L1747
    lsr     ram_D9                  ;5         
    bcs     L1750                   ;2/3       
    lda     GL_STOP_C80             ;4         
    inc     ram_DF                  ;5   =  16 
L1750
    lsr     ram_D9                  ;5         
    bcs     L175d                   ;2/3       
    lda     GL_480                  ;4              switch bank 3 into slice 0?
    jsr     L12d0                   ;6              code in bank 3
    lda     GL_481                  ;4   =  21      switch bank ? into slice 0?
L175d
    lsr     ram_D9                  ;5         
    bcs     L1791                   ;2/3       
    lsr     ram_D9                  ;5         
    bcs     L179f                   ;2/3       
    lda     ram_DC                  ;3         
    jsr     Div16                   ;6         
    tax                             ;2         
    lda     GL_480,x                ;4         
    lda     ram_DD                  ;3         
    jsr     Div16                   ;6         
    tax                             ;2         
    lda     GL_880,x                ;4         
    lda     ram_DD                  ;3         
    and     #$0f                    ;2         
    tax                             ;2         
    lda     GL_980,x                ;4         
    ldx     #$06                    ;2   =  57 
L1781
    lda     RamCode,x               ;4         
    sta     ramCode,x               ;4         
    dex                             ;2         
    bpl     L1781                   ;2/3       
    lda     ram_DC                  ;3         
    and     #$0f                    ;2         
    tax                             ;2         
    jmp.w   ramCode                 ;3   =  22 
    
L1791
    jsr     L17b4                   ;6         
    jsr     L179c                   ;6         
    lda     ram_DB                  ;3         
    bpl     L1731                   ;2/3       
    rts                             ;6   =  23 
    
L179c SUBROUTINE
    jmp.ind (ram_DC)                ;5   =   5 

L179f
    jsr     L17b4                   ;6         
    stx     ram_82                  ;3         
    ldx     #$ff                    ;2         
    txs                             ;2         
    lda     ram_DC                  ;3         
    ldx     ram_DD                  ;3         
    jmp     (L1003)                 ;5   =  24      -> L1106?

RamCode 
    lda     GL_580,x                ;4         
    jmp     (L1ffc)                 ;5   =   9 

L17b4 SUBROUTINE
    lda     ram_DB                  ;3         
    and     #$0f                    ;2         
    tax                             ;2         
    lda     ram_DD                  ;3         
    eor     #$18                    ;2         
    sta     ram_DD                  ;3         
    lda     GL_880,x                ;4         
    rts                             ;6   =  25 
    
Div16 SUBROUTINE
    lsr                             ;2         
    lsr                             ;2         
    lsr                             ;2         
    lsr                             ;2         
    rts                             ;6   =  14 
    
L17c8 SUBROUTINE
    lda     #$00                    ;2         
    sta     ram_C8                  ;3         
    lda     #<ram_D7                ;2         
    sta     dataPtr2                ;3         
    lda     #>ram_D7                ;2         
    sta     dataPtr2+1              ;3         
    lda     #$07                    ;2         
    sta     ram_C9                  ;3         
    jsr     ResetCRC                ;6         
    rts                             ;6   =  32 
    
ResetCRC SUBROUTINE
    lda     #$00                    ;2         
    sta     crcHi                   ;3         
    sta     crcLo                   ;3         
    rts                             ;6   =  14 
    
UpdateCRC SUBROUTINE
    pha                             ;3         
    eor     crcLo                   ;3         
    sta     crcLo                   ;3         
    lda     crcHi                   ;3         
    asl                             ;2         
    rol     crcLo                   ;5         
    rol     crcHi                   ;5         
    pla                             ;4         
    rts                             ;6   =  34 
    
SendByte SUBROUTINE
    jsr     UpdateCRC               ;6         
    jsr     TransferByte            ;6         
    rts                             ;6   =  18 

    .byte   $4f,$4f,$00,$00,$4f,$4f,$06,$05 ; $17f8 (D)