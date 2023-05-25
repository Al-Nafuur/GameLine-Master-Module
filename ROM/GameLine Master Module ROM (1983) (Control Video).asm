; Disassembly of ~\Documents\Atari\Games\Atarimania\V18\GameLine Master Module ROM (1983) (Control Video Corporation) ~.bin
; Disassembled 05/11/23 15:14:58
; Using Stella 7.0_pre
;
; ROM properties name : GameLine Master Module ROM (1983) (Control Video)
; ROM properties MD5  : f539e32bf6ce39c8ca47cb0cdd2c5cb8
; Bankswitch type     : 4K* (4K)
;
; Legend: *  = CODE not yet run (tentative code)
;         D  = DATA directive (referenced in some way)
;         G  = GFX directive, shown as '#' (stored in player, missile, ball)
;         P  = PGFX directive, shown as '*' (stored in playfield)
;         C  = COL directive, shown as color constants (stored in player color)
;         CP = PCOL directive, shown as color constants (stored in playfield color)
;         CB = BCOL directive, shown as color constants (stored in background color)
;         A  = AUD directive (stored in audio registers)
;         i  = indexed accessed only
;         c  = used by code executed in RAM
;         s  = used by stack
;         !  = page crossed, 1 cycle penalty

    processor 6502


;===============================================================================
; A S S E M B L E R - S W I T C H E S
;===============================================================================

ORIGINAL        = 1         ; 1 = compile 100% identical to dump


;===============================================================================
; C O N S T A N T S
;===============================================================================

; * * * * *  GL-Mapping-Bits  * * * * *

; SLICES (bits 8, 10 & 11):
;   $400: slice 0
;   $500: slice 1 
;   $800: slice 2 
;   $900: slice 3

; BANKS & MODES (bits 0..5): 
; bit 0, 1: 0..3 = mapped bank
; bit 2: 0 = ROM, 1 = RAM
; bit 3: %1101 = ??? (PROM?)
; bit 4: here always 0
; bit 5: 0 = read, 1 = write (RAM only) (-> if bit 5 == 1 then bit 2 == 1)

; GameLine registers:        
SL0_BX          = $480      ; switch ROM bank X into slice 0; X = 1|..
SL0_B3          = SL0_BX    ; switch ROM bank 3 into slice 0
SL0_B0          = $481      ; switch ROM bank 0 into slice 0
SL0_R1          = $485      ; switch RAM bank 1 into slice 0 for reading
SL0_WD          = $4ad      ; switch RAM bank ? into slice 0 for writing (writes to $1000)

SL1_BX          = $580      ; switch ROM bank X into slice 1; X = 1|2|4|..
SL1_B1          = $582      ; switch ROM bank 1 into slice 1
SL1_B2          = $583      ; switch ROM bank 2 into slice 1

SL2_BX          = $880      ; switch ROM bank X into slice 2; X = 4|..  
SL2_B3          = SL2_BX    ; switch ROM bank 3 into slice 2
SL2_R0          = $884      ; switch RAM bank 0 into slice 2 for reading
SL2_R1          = $885      ; switch RAM bank 1 into slice 2 for reading
SL2_WX          = $8a0      ; switch RAM bank X into slice 2 for writing (,X; writes to ???)
SL2_W0          = $8a4      ; switch RAM bank 0 into slice 2 for writing (writes to $1802)
SL2_W1          = $8a5      ; switch RAM bank 1 into slice 2 for writing (writes to $1802, $180c, $180d, $180e..$1815)

SL3_BX          = $980      ; switch ROM bank X into slice 3; X = 0|13|...
SL3_B3          = SL3_BX    ; switch ROM bank 3 into slice 3

GL_680          = $680

GL_STOP_C80     = $c80
GL_START_CA0    = $ca0      ; allows write to $1000 or read from $1ff8
GL_STOP_PULSE   = $cb0      ; could be...
GL_START_PULSE  = $cb8      ; ... vice versa
GL_CBA          = $cba      ; bit transfer (clear)
GL_CBB          = $cbb      ; bit transfer (set)

GL_SEND_TONE    = $d80      ; also ,Y; Y = $10..$1a

GL_INPUT        = $1ff8


;-----------------------------------------------------------
;      Color constants
;-----------------------------------------------------------

BLACK            = $00
YELLOW           = $10
BROWN            = $20
ORANGE           = $30
RED              = $40
MAUVE            = $50
VIOLET           = $60
PURPLE           = $70
BLUE             = $80
BLUE_CYAN        = $90
CYAN             = $a0
CYAN_GREEN       = $b0
GREEN            = $c0
GREEN_YELLOW     = $d0
GREEN_BEIGE      = $e0
BEIGE            = $f0


;-----------------------------------------------------------
;      TIA and IO constants accessed
;-----------------------------------------------------------

INPT4           = $0c  ; (R)
INPT5           = $0d  ; (R)

VSYNC           = $00  ; (W)
VBLANK          = $01  ; (W)
WSYNC           = $02  ; (W)

NUSIZ0          = $04  ; (W)
NUSIZ1          = $05  ; (W)
COLUP0          = $06  ; (W)
COLUP1          = $07  ; (W)
COLUPF          = $08  ; (W)
COLUBK          = $09  ; (W)
CTRLPF          = $0a  ; (W)
PF0             = $0d  ; (W)
PF1             = $0e  ; (W)
PF2             = $0f  ; (W)
RESP0           = $10  ; (W)
RESP1           = $11  ; (W)

AUDC0           = $15  ; (W)
AUDF0           = $17  ; (W)
AUDF1           = $18  ; (W)        ; used due to bug
AUDV0           = $19  ; (W)
GRP0            = $1b  ; (W)
GRP1            = $1c  ; (W)

HMP0            = $20  ; (W)

VDELP0          = $25  ; (W)
VDELP1          = $26  ; (W)

HMOVE           = $2a  ; (W)
HMCLR           = $2b  ; (W)

SWCHA           = $0280
SWCHB           = $0282
TIM1T           = $0294
TIM8T           = $0295
TIM64T          = $0296
T1024T          = $0297


;-----------------------------------------------------------
;      RIOT RAM (zero-page) labels
;-----------------------------------------------------------

ram_80          = $80           ; always 1, never read
slice1Bank      = $81           ; X for SL1_BX,x (X = 2)
slice2Bank      = $82           ; X for SL2_BX,x (X = 4, ?)
slice3Bank      = $83           ; X for SL3_BX,x (X = 13|0)
;---------------------------------------
drawPtrIdx      = $84
drawLst         = $85           ; ..$8a
drawFlags       = drawLst
drawIdx         = drawLst+1
drawEnd         = drawLst+2
drawGap         = drawLst+3    ; = $88

drawPos         = $8b
;---------------------------------------
tmpGfx          = $8c           ; temp. var
color           = $8d
frameCnt        = $8e           ; frame timeCnt
;---------------------------------------
ptrLst          = $8f           ; ..$9a
ram_8F          = ptrLst
ram_90          = ptrLst+1
ram_91          = ptrLst+2
ram_92          = ptrLst+3
ram_93          = ptrLst+4
ram_94          = ptrLst+5
ram_95          = ptrLst+6
ram_96          = ptrLst+7
ram_97          = ptrLst+8
ram_98          = ptrLst+9
ram_99          = ptrLst+10
ram_9A          = ptrLst+11

ramCode         = ptrLst
;---------------------------------------
flashTimer      = $9b
flashState      = $9c
yDial           = $9d
xDial           = $9e
numDigits       = $9f
drawEndCopy     = $a0
lastFire        = $a1
lastSwitches    = $a2
lastJoyDir      = $a3
inputDelay      = $a4
inputLo         = $a5           ; only decreased
inputHi         = $a6           ; input related
;---------------------------------------
digitLst        = $a7           ; ..$a9
digit0          = digitLst        
digit1          = digitLst+1
digit2          = digitLst+2
;---------------------------------------

dataPtr         = $ab           ;..$ac
dataPtr2        = $ad           ;..$ae
ram_AF          = $af
drawBtm         = $b0
ram_B1          = $b1
ram_B2          = $b2
ram_B3          = $b3
unusedPtr       = $b4           ;..$b5
slice3BankB     = $b6
unusedFlag      = $b7

pulseCount      = $ba
jmpIdx          = $bb
dialCount       = $bc
numberIdx       = $bd
numberPtrIdx    = $be
dialType        = $bf           ; unsure!
timeCnt         = $c0           ;..$c1 (*1000 = ~cylces), also used for an unused pointer
numberPtr       = $c2           ;..$c3
dialSpeed       = $c4           ; also type

ram_C6          = $c6           ; never read, written once
retryCnt        = $c7
storeFlags      = $c8
storeCnt        = $c9
crcLst          = $ca           ; ..$cb
crcHi           = crcLst
crcLo           = crcLst+1
bitIdx          = $cc           ; = $db / 16
bitSet          = $cd
transferByte    = $ce
loadFlag        = $cf           ; 0|1
ram_D0          = $d0           ; increased once (hi)
ram_D1          = $d1           ; increased once (low)
ram_D2          = $d2
ram_D3          = $d3
animState       = $d4   
animDelay       = $d5
xCurtain        = $d6
ram_D7          = $d7
ram_D8          = $d8
ram_D9          = $d9
ram_DA          = $da           ; loaded only via ($d7),y; -> storeCnt
ram_DB          = $db           ; ????2222 ?, bank into slice 2
ram_DC          = $dc           ; 00001111 -> banks into slices 0/1
ram_DD          = $dd           ; 22223333 -> banks into slices 2/3
tmpVarDE        = $de
ram_DF          = $df
speedIdx        = $e0

;                 $fa  (is)
;                 $fb  (is)
;                 $fc  (is)
;                 $fd  (is)
;                 $fe  (is)
;                 $ff  (is)


;***********************************************************
;      GL-RAM
;***********************************************************

L1800   = $1800     ; compared with dataPtr2
L1801   = $1801     ; compared with dataPtr2+1
L1802   = $1802     ; 4 bytes (initialized, ...)
L1806   = $1806     ; -> dataPtr2
L1807   = $1807     ; -> dataPtr2+1
L1808   = $1808     ; -> dataPtr, dataPtr2
L1809   = $1809     ; -> dataPtr+1, dataPtr2+1
L180a   = $180a     ; -> dataPtr2
L180b   = $180b     ; -> dataPtr2+1
L180c   = $180c     ; <-> dialType
L180d   = $180d     ; <-> dialSpeed
L180e   = $180e     ; 8 bytes copied from L1c9a to bank 1
L1812   = $1812     ; -> numberPtr, indexed
L1813   = $1813     ; -> numberPtr+1, indexed


;***********************************************************
;      Bank 0
;***********************************************************
; 1K ROM bank #0

    SEG     CODE

    ORG     $1000
    RORG    $1000

L1000
    jmp     L1009                   ;3   =   3

L1003
    .word   ReadData, L139a, L15d8  ; $1003 (D)

L1009
    lda     #$00                    ;2
    tax                             ;2   =   4
L100c
    sta     $00,x                   ;4
    txs                             ;2
    inx                             ;2
    bne     L100c                   ;2/3
    ldx     #$01                    ;2          ROM bank 0 into slice 0
    stx     ram_80                  ;3
    inx                             ;2          ROM bank 1 into slice 1
    stx     slice1Bank              ;3
    ldx     #$04                    ;2          RAM bank 0 into slice 2 for reading
    stx     slice2Bank              ;3
    ldx     #$0d                    ;2          special bank into slice 3 for writing
    stx     slice3Bank              ;3                          
    lda     SL3_B3                  ;4          switch ROM bank 3 into slice 3
    lda     SL2_R0                  ;4          switch RAM bank 0 into slice 2 for reading (L1800..)
    jsr     Check1800               ;6
    bne     Failed                  ;2/3
    lda     SL2_R1                  ;4          switch RAM bank 1 into slice 2 for reading (L1800..)
    jsr     Check1800               ;6
    beq     .success                ;2/3
Failed
    lda     SL2_W0                  ;4          switch RAM bank 0 into slice 2 for writing 
    ldx     #$00                    ;2
    stx     L1802                   ;4
    lda     SL2_W1                  ;4          switch RAM bank 1 into slice 2 for writing
    stx     L1802                   ;4
    lda     #$00                    ;2          ROM bank 3 into slice 3
    sta     slice3Bank              ;3
    lda     #$ff                    ;2
    sta     slice3BankB             ;3          invalid
    lda     #<L1c00                 ;2
    ldx     #>L1c00                 ;2
    jmp     ReadData                ;3   =  35

.success
    lda     SL2_R0                  ;4          switch RAM bank 0 into slice 2 for reading
    lda     L1808                   ;4          undefined!
    ldx     L1809                   ;4          undefined!
    jmp     ReadData                ;3   =  15

XPosSprites SUBROUTINE
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    ldx     #$05                    ;2   =   2
L1061
    dex                             ;2
    bne     L1061                   ;2/3
    nop                             ;2
    nop                             ;2
    lda     #$f0                    ;2
    sta     HMP0                    ;3
    sta     RESP0                   ;3
    sta     RESP1                   ;3
    sta     WSYNC                   ;3   =  22
;---------------------------------------
    sta     HMOVE                   ;3
    lda     #$03                    ;2
    sta     NUSIZ0                  ;3
    lda     #$03                    ;2
    sta     NUSIZ1                  ;3
    sta     VDELP0                  ;3
    sta     VDELP1                  ;3
    sta     HMCLR                   ;3
    rts                             ;6   =  28

    SUBROUTINE
.loop
    sta     GRP0                    ;3
    lda     (ram_91),y              ;5
    sta     GRP1                    ;3
    lda     (ram_93),y              ;5
    sta     GRP0                    ;3
    lda     (ram_95),y              ;5
    tax                             ;2
    lda     (ram_97),y              ;5
    ldy     tmpGfx                  ;3
    stx     GRP1                    ;3
    sta     GRP0                    ;3
    sty     GRP1                    ;3
    sta     GRP0                    ;3
    inc     drawIdx                 ;5   =  51
Draw48Pixel
    ldy     drawIdx                 ;3
    lda     (ram_99),y              ;5
    sta     tmpGfx                  ;3
    lda     (ram_8F),y              ;5
    sta     WSYNC                   ;3   =  19
;---------------------------------------
    cpy     drawEnd                 ;3
    bne     .loop                   ;2/3
    lda     #$00                    ;2
    sta     GRP0                    ;3
    sta     GRP1                    ;3
    sta     GRP0                    ;3
    rts                             ;6   =  22

L10b3 SUBROUTINE
;write $1c00, $1c80, $1d00, $1d80, $1e00, $1e80 into $8f..9a
    ldx     #$00                    ;2
    lda     #<L1c00                 ;2
    ldy     #>L1c00                 ;2   =   6
L10b9 
    sta     ptrLst,x                ;4
    inx                             ;2
    sty     ptrLst,x                ;4
    eor     #$80                    ;2
    bmi     L10c3                   ;2/3
    iny                             ;2   =  16
L10c3
    inx                             ;2
    cpx     #$0c                    ;2
    bne     L10b9                   ;2/3
    rts                             ;6   =  12

L10c9 SUBROUTINE
    lda     SL1_B2                  ;4                  switch ROM bank 2 into slice 1
    lda     #<L1400                 ;2                  "jmp Load" -> ??? (from ROM $1800)
    ldx     #>L1400                 ;2                  
    jmp     SetDataPtr2             ;3   =  11

L10d3 SUBROUTINE
    lda     SL2_R1                  ;4                  switch RAM bank 1 into slice 2 for reading (L1800..)
    lda     L1808                   ;4
    ldx     L1809                   ;4   =  12
SetDataPtr2
    sta     dataPtr2                ;3
    stx     dataPtr2+1              ;3
    jsr     JmpToPtr2               ;6   =  12
SetupBanks
    stx     tmpVarDE                ;3
    ldx     slice1Bank              ;3          2?
    lda     SL1_BX,x                ;4                  switch ROM/RAM bank X(1?) into slice 1
    ldx     slice2Bank              ;3          4?
    lda     SL2_BX,x                ;4                  switch ROM/RAM bank X(3,?) into slice 2
    ldx     slice3Bank              ;3          13|0?
    lda     SL3_BX,x                ;4                  switch ROM/RAM bank X(0,13?) into slice 3
    ldx     tmpVarDE                ;3
    rts                             ;6   =  33

JmpToPtr2 SUBROUTINE
    jmp.ind (dataPtr2)              ;5   =   5          $1ece?|Load???

L10fa SUBROUTINE
    lda     SL2_R0                  ;4                  switch RAM bank 0 into slice 2 for reading
    lda     L1806                   ;4
    ldx     L1807                   ;4
    jmp     SetDataPtr2             ;3   =  15

ReadData SUBROUTINE
    sta     dataPtr                 ;3          L1c00, L1808
    stx     dataPtr+1               ;3          83 = 0,
    jsr     SetupBanks              ;6
    lda     #$00                    ;2
    sta     ram_AF                  ;3
    sta     frameCnt                ;3
    sta     ram_D2                  ;3
    lda     #BROWN|$8               ;2
    sta     color                   ;3
    lda     #$01                    ;2
    sta     CTRLPF                  ;3
    lda     #$0a                    ;2
    sta     inputDelay              ;3
    sta     flashTimer              ;3   =  41
L1123
    jsr     L12bf                   ;6
    ldy     #$08                    ;2          Y = 8
    lda     (dataPtr),y             ;5          $1c08 = $64, (L1810)
    sta     TIM64T                  ;4
    lda     ram_AF                  ;3
    beq     L1134                   ;2/3
    jmp     L11b6                   ;3   =  25

L1134
    lda     #$12                    ;2
    sta     T1024T                  ;4
    ldx     #$ff                    ;2
    stx     flashState              ;3
    inx                             ;2
    stx     numDigits               ;3
    inx                             ;2
    stx     yDial                   ;3
    stx     xDial                   ;3
    ldy     #$00                    ;2          Y = 0
    lda     (dataPtr),y             ;5          $1c00 = $c1; $1808 = 
    sta     ram_AF                  ;3
    jsr     XPosSprites             ;6
    ldy     #$0a                    ;2          Y = $a
    lda     (dataPtr),y             ;5          $1c0a = $7f; $1812 = 
    sta     drawBtm                 ;3
    ldy     #$02                    ;2          Y = 2
    lda     (dataPtr),y             ;5          $1c02 = $00; $180a = 
    beq     L1178                   ;2/3
    sta     ram_C6                  ;3
    iny                             ;2          Y = 3
    lda     (dataPtr),y             ;5          $1c03 = $00; $180b = 
    sta     jmpIdx                  ;3
    iny                             ;2          Y = 4
    lda     (dataPtr),y             ;5          $1c04 = $00; $180c = 
    sta     timeCnt                 ;3          another pointer lo
    iny                             ;2          Y = 5
    lda     (dataPtr),y             ;5          $1c05 = $00; $180d = 
    jsr     GetHiPtrBank            ;6
    sta     timeCnt+1               ;3          another pointer hi (A = $10..$1f)
    lda     SL1_BX,x                ;4          X = (dataPtr),y/16
    lda     #$06                    ;2
    sta     pulseCount              ;3
    jsr     L10d3                   ;6   = 113
L1178
    ldy     #$0e                    ;2          Y = $e
    lda     (dataPtr),y             ;5          $1c0e = $00
    tax                             ;2
    iny                             ;2          Y = $f
    lda     (dataPtr),y             ;5          $1c0f = $00
    beq     L118f                   ;2/3
    stx     unusedPtr               ;3
    jsr     GetHiPtrBank            ;6
    sta     unusedPtr+1             ;3
    stx     slice3BankB             ;3
    lda     #$01                    ;2
    sta     unusedFlag              ;3   =  38
L118f
    ldy     #$10                    ;2          Y = $10
    lda     (dataPtr),y             ;5          $1c10 = $00
    tax                             ;2
    iny                             ;2          Y = $11
    lda     (dataPtr),y             ;5          $1c11 = $00
    beq     L11a0                   ;2/3
    sta     dataPtr2+1              ;3
    stx     dataPtr2                ;3
    jsr     L11b3                   ;6   =  30
L11a0
    ldy     #$01                    ;2          Y = 1
    lda     (dataPtr),y             ;5          $1c01 = $1e
    sta     ram_D2                  ;3
    sta     ram_B1                  ;3
    jsr     WaitTim                 ;6
    lda     #$02                    ;2
    sta     TIM64T                  ;4
    jmp     L12a6                   ;3   =  28

L11b3 SUBROUTINE
    jmp.ind (dataPtr2)              ;5   =   5

L11b6
    ldy     #$09                    ;2          Y = 9
    lda     (dataPtr),y             ;5          $1c09 = $89
    tax                             ;2
    jsr     WaitTim                 ;6
    stx     TIM64T                  ;4
    bit     ram_AF                  ;3
    bpl     L11cb                   ;2/3
    bvs     L11cb                   ;2/3
    lda     #$ff                    ;2
    sta     PF2                     ;3   =  31
L11cb
    lda     #$00                    ;2
    sta     ram_B3                  ;3
    ldy     #$0b                    ;2          Y = $b
    lda     (dataPtr),y             ;5          $1c0b = $10
    beq     L11d9                   ;2/3
    tax                             ;2
    jsr     WaitLines               ;6   =  22
L11d9
    asl                             ;2
    sta     drawPos                 ;3
    ldy     #$14                    ;2
    sty     drawPtrIdx              ;3   =  10
L11e0
    jsr     FillRam85Lst            ;6
    lda     drawFlags               ;3
    bmi     L122a                   ;2/3!
    beq     L11f5                   ;2/3
    cpy     ram_D2                  ;3
    bne     L11f5                   ;2/3
    pha                             ;3
    pla                             ;4
    pha                             ;3
    pla                             ;4
    lda     #$fe                    ;2
    sta     PF1                     ;3   =  37
L11f5 ;.loop?
    lda     drawEnd                 ;3
    clc                             ;2
    adc     drawPos                 ;3
    adc     #$02                    ;2
    cmp     drawBtm                 ;3
    bcs     L1271                   ;2/3
    sta     drawPos                 ;3
    lda     drawEnd                 ;3
    clc                             ;2
    adc     drawIdx                 ;3
    sta     drawEnd                 ;3
    jsr     Draw48Pixel             ;6
    lda     #$00                    ;2
    sta     PF1                     ;3   =  40
L1210
    lda     drawGap                 ;3
    beq     L1227                   ;2/3
    tax                             ;2
    clc                             ;2
    adc     drawPos                 ;3
    cmp     drawBtm                 ;3
    bcs     L1271                   ;2/3
    sta     drawPos                 ;3
    jsr     WaitLines               ;6
    jsr     L10b3                   ;6
    jsr     SetupBanks              ;6   =  38
L1227
    jmp     L11e0                   ;3   =   3

L122a
    asl                             ;2          A = drawFlags
    bmi     L123c                   ;2/3
    lda     SL2_B3                  ;4          switch ROM bank 3 into slice 2
    jsr     DrawDialPad             ;6
    jsr     SetupBanks              ;6
    jsr     XPosSprites             ;6
    jmp     L1210                   ;3   =  29

L123c
    asl                             ;2          A = drawFlags << 1
    bmi     L1261                   ;2/3
; setup 48 pixel pointers:
    ldy     #$06                    ;2
    lda     (dataPtr),y             ;5
    sta     dataPtr2                ;3
    iny                             ;2
    lda     (dataPtr),y             ;5
    sta     dataPtr2+1              ;3
    ldy     #$00                    ;2   =  26
.loop
    lda     (dataPtr2),y            ;5
    sta.wy  ptrLst,y                ;5
    iny                             ;2
    cpy     #$0c                    ;2
    bne     .loop                   ;2/3
    jsr     GetHiPtrBank            ;6          A -> A, X
    sta     ram_9A                  ;3
    lda     SL3_BX,x                ;4          X = (dataPtr2),y / 16
    jmp     L11f5                   ;3   =  32

L1261
    asl                             ;2          A = drawFlags << 2
    bmi     L126d                   ;2/3
    jsr     .jmp2Ptr                ;6
    jmp     L1210                   ;3   =  13

.jmp2Ptr
    jmp.ind (drawIdx)               ;5   =   5  

L126d
    lda     #$ff                    ;2
    sta     ram_B2                  ;3   =   5
L1271
    jsr     WaitTim                 ;6
    lda     #$00                    ;2
    sta     PF2                     ;3
    ldy     #$08                    ;2
    lda     (dataPtr),y             ;5
    iny                             ;2
    clc                             ;2
    adc     (dataPtr),y             ;5
    eor     #$ff                    ;2
    adc     #$20                    ;2
    sta     WSYNC                   ;3   =  34
;---------------------------------------
    sta     TIM64T                  ;4
    ldy     #$12                    ;2
    lda     (dataPtr),y             ;5
    sta     dataPtr2                ;3
    iny                             ;2
    lda     (dataPtr),y             ;5
    beq     L1299                   ;2/3
    sta     dataPtr2+1              ;3
    jsr     L11b3                   ;6   =  32
L1299
    lda     animState               ;3
    bne     L12a3                   ;2/3
    inc     ram_D1                  ;5
    bne     L12a3                   ;2/3
    inc     ram_D0                  ;5   =  17
L12a3
    jsr     HandleInput             ;6   =   6
L12a6
    jsr     WaitTim                 ;6
    jmp     L1123                   ;3   =   9

VerticalSync SUBROUTINE
    lda     #$02                    ;2
    sta     WSYNC                   ;3   =   5
;---------------------------------------
    sta     VBLANK                  ;3
    jsr     Wait3Lines              ;6
    sta     VSYNC                   ;3
    lda     #$00                    ;2
    jsr     Wait3Lines              ;6
    sta     VSYNC                   ;3
    rts                             ;6   =  29

L12bf SUBROUTINE
    jsr     VerticalSync            ;6
    lda     #$5a                    ;2          = ~720 cycles
    sta     TIM8T                   ;4
    lda     #$00                    ;2
    sta     PF0                     ;3
    sta     PF1                     ;3
    sta     PF2                     ;3
    bit     ram_AF                  ;3
    bpl     L12eb                   ;2/3
    lda     color                   ;3
    and     #$f0                    ;2
    sta     COLUP0                  ;3
    sta     COLUP1                  ;3
    bvs     L12e5
    sta     COLUBK                  ;3
    lda     color                   ;3
    sta     COLUPF                  ;3
    bvc     L12f7                   ;2/3 =  14

L12e5
    lda     color                   ;3
    sta     COLUBK                  ;3
    bvs     L12f7                   ;2/3 =   8

L12eb
    bvc     L12f7                   ;2/3
    lda     #$00                    ;2
    sta     COLUBK                  ;3
    lda     #YELLOW|$f              ;2
    sta     COLUP0                  ;3
    sta     COLUP1                  ;3   =  15
L12f7
    jsr     L10b3                   ;6
    ldx     slice3BankB             ;3
    bmi     L1304                   ;2/3!
    lda     SL3_BX,x                ;4
    jsr     L10fa                   ;6   =  21
L1304
    jsr     WaitTim                 ;6
    sta     VBLANK                  ;3
    rts                             ;6   =  15

WaitTim SUBROUTINE
    lda     TIM8T                   ;4
    bpl     WaitTim                 ;2/3 =   6
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    rts                             ;6   =   6

Wait3Lines SUBROUTINE
    ldx     #$03                    ;2   =   2
WaitLines
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    dex                             ;2
    bne     WaitLines               ;2/3
    rts                             ;6   =  10

GetHiPtrBank SUBROUTINE
    pha                             ;3
    lsr                             ;2
    lsr                             ;2
    lsr                             ;2
    lsr                             ;2
    tax                             ;2
    pla                             ;4
    and     #$0f                    ;2
    ora     #$10                    ;2
    rts                             ;6   =  27

FillRam85Lst SUBROUTINE
    ldx     #$00                    ;2
    ldy     drawPtrIdx              ;3
    lda     (dataPtr),y             ;5
    bmi     .loop                   ;2/3
    beq     .loop                   ;2/3
    lda     ram_B3                  ;3
    bne     .loop                   ;2/3
    ldy     ram_B1                  ;3
    sty     drawPtrIdx              ;3
    sty     ram_B3                  ;3   =  28
.loop
    lda     (dataPtr),y             ;5
    sta     drawLst,x               ;4
    iny                             ;2
    inx                             ;2
    cpx     #$06                    ;2
    bne     .loop                   ;2/3
    lda     drawPtrIdx              ;3
    sty     drawPtrIdx              ;3
    sty     ram_B2                  ;3
    tay                             ;2
    rts                             ;6   =  34

Dial SUBROUTINE
    ldy     dialCount               ;3
    bne     L135a                   ;2/3
    dey                             ;2
    sty     dialType                ;3          = $ff
    sty     dialSpeed               ;3
    inc     dialCount               ;5
    jmp     L137a                   ;3   =  21

L135a
    iny                             ;2
    sty     dialCount               ;3
    cpy     #$0a                    ;2
    beq     L1393                   ;2/3
    ldy     #$00                    ;2
    sty     numberPtrIdx            ;3
    inc     dialSpeed               ;5
    lda     dialSpeed               ;3
    cmp     #$02                    ;2
    bne     L137a                   ;2/3
    dey                             ;2
    sty     dialSpeed               ;3          Y = $ff
    inc     dialType                ;5
    lda     dialType                ;3
    cmp     #$01                    ;2
    bne     L137a                   ;2/3
    sty     dialType                ;3   =  46  Y = $ff
L137a
    lda     SL2_R1                  ;4          switch RAM bank 1 into slice 2 for reading (L1800..)
    lda     L1802                   ;4
    bne     L1392                   ;2/3
    lda     SL2_W1                  ;4          switch RAM bank 1 into slice 2 for writing (L1800..)
    lda     dialType                ;3          backup dialType and speed
    sta     L180c                   ;4
    lda     dialSpeed               ;3
    sta     L180d                   ;4
    lda     SL2_R1                  ;4   =  32  switch RAM bank 1 into slice 2 for reading (L1800..)
L1392
    rts                             ;6   =   6

L1393
    lda     #$00                    ;2
    sta     dialCount               ;3
    jmp     Failed                  ;3   =   8 *

L139a
    lda     #$00                    ;2
    sta     dialCount               ;3
    sta     numberPtrIdx            ;3
    jsr     RestoreDialData         ;6   =  14
L13a3
    ldx     #$00                    ;2
    stx     frameCnt                ;3
    dex                             ;2
    stx     jmpIdx                  ;3
    lda     ram_DF                  ;3
    beq     DrawCalling             ;2/3
    jmp     L15d8                   ;3   =  18

DrawCalling
    jsr     XPosSprites             ;6
    ldx     #$00                    ;2
    ldy     #>CallingGfx            ;2
    lda     #<CallingGfx            ;2   =  12
L13ba
    sta     ptrLst,x                ;4
    inx                             ;2
    sty     ptrLst,x                ;4
    inx                             ;2
    clc                             ;2
    adc     #$07                    ;2
    bcc     L13c6                   ;2/3
    iny                             ;2   =  20
L13c6
    cpx     #$0c                    ;2
    bne     L13ba                   ;2/3 =   4
L13ca
    jsr     VerticalSync            ;6
    lda     INPT4                   ;3
    and     INPT5                   ;3
    and     #$80                    ;2
    bne     L13d7                   ;2/3
    sta     ram_DF                  ;3   =  19  = 0
L13d7
    ldx     #$0a                    ;2
    jsr     WaitLines               ;6
    lda     #$00                    ;2
    sta     VBLANK                  ;3
    ldx     #$0f                    ;2
    jsr     WaitLines               ;6
    jsr     L1400                   ;6
    lda     #$00                    ;2
    sta     drawIdx                 ;3
    lda     #$07                    ;2
    sta     drawEnd                 ;3
    jsr     Draw48Pixel             ;6
    jsr     L1435                   ;6          check bits?
    ldx     #$0c                    ;2
    jsr     WaitLines               ;6
    inc     frameCnt                ;5
    jmp     L13ca                   ;3   =  65


;***********************************************************
;      Bank 1
;***********************************************************
; 1K ROM bank #1

;    RORG    $1400

L1400 SUBROUTINE
    lda     frameCnt                ;3
    and     #$e0                    ;2
    clc                             ;2
    adc     #$10                    ;2
    sta     COLUP0                  ;3
    sta     COLUP1                  ;3
    clc                             ;2
    adc     #$02                    ;2
    sta     color                   ;3   =  22
.loop
    jsr     L141d                   ;6
    clc                             ;2
    adc     #$02                    ;2
    sta     color                   ;3
    and     #$0f                    ;2
    bne     .loop                   ;2/3
    rts                             ;6   =  23

L141d SUBROUTINE
    lda     color                   ;3
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     COLUBK                  ;3
    pha                             ;3
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    ldx     #$7b                    ;2          = ~984 cycles
    stx     TIM8T                   ;4
    jsr     CheckBit6               ;6   =  12
L142e
    ldx     TIM8T                   ;4
    bpl     L142e                   ;2/3
    pla                             ;4
    rts                             ;6   =  16

L1435 SUBROUTINE
    dec     color                   ;5
    dec     color                   ;5
    jsr     L141d                   ;6
    and     #$0f                    ;2
    cmp     #$02                    ;2
    bne     L1435                   ;2/3
    rts                             ;6   =  28

CallingGfx
    .byte   %00000000 ; |        |            $1443 (G)
    .byte   %00000000 ; |        |            $1444 (G)
    .byte   %00000000 ; |        |            $1445 (G)
    .byte   %00000000 ; |        |            $1446 (G)
    .byte   %00000000 ; |        |            $1447 (G)
    .byte   %00000000 ; |        |            $1448 (G)
    .byte   %00000000 ; |        |            $1449 (G)
    .byte   %00111011 ; |  ### ##|            $144a (G)     CA
    .byte   %00101010 ; |  # # # |            $144b (G)
    .byte   %00100010 ; |  #   # |            $144c (G)
    .byte   %00100010 ; |  #   # |            $144d (G)
    .byte   %00100011 ; |  #   ##|            $144e (G)
    .byte   %00101010 ; |  # # # |            $144f (G)
    .byte   %00111010 ; |  ### # |            $1450 (G)

    .byte   %10100010 ; |# #   # |            $1451 (G)     LL
    .byte   %10100010 ; |# #   # |            $1452 (G)
    .byte   %10100010 ; |# #   # |            $1453 (G)
    .byte   %10100010 ; |# #   # |            $1454 (G)
    .byte   %10100010 ; |# #   # |            $1455 (G)
    .byte   %10100010 ; |# #   # |            $1456 (G)
    .byte   %10111011 ; |# ### ##|            $1457 (G)

    .byte   %00111010 ; |  ### # |            $1458 (G)     IN
    .byte   %00010010 ; |   #  # |            $1459 (G)
    .byte   %00010011 ; |   #  ##|            $145a (G)
    .byte   %00010010 ; |   #  # |            $145b (G)
    .byte   %00010010 ; |   #  # |            $145c (G)
    .byte   %00010010 ; |   #  # |            $145d (G)
    .byte   %10111010 ; |# ### # |            $145e (G)

    .byte   %01011100 ; | # ###  |            $145f (G)     G
    .byte   %01010100 ; | # # #  |            $1460 (G)
    .byte   %01010000 ; | # #    |            $1461 (G)
    .byte   %11010100 ; |## # #  |            $1462 (G)
    .byte   %01010100 ; | # # #  |            $1463 (G)
    .byte   %01010100 ; | # # #  |            $1464 (G)
    .byte   %01011100 ; | # ###  |            $1465 (G)
    .byte   %00000000 ; |        |            $1466 (G)
    .byte   %00000000 ; |        |            $1467 (G)
    .byte   %00000000 ; |        |            $1468 (G)
    .byte   %00000000 ; |        |            $1469 (G)
    .byte   %00000000 ; |        |            $146a (G)
    .byte   %00000000 ; |        |            $146b (G)
    .byte   %00000000 ; |        |            $146c (G)

RestoreDialData SUBROUTINE
    lda     SL2_R1                  ;4          switch RAM bank 1 into slice 2 for reading (L1800..)
    lda     L180c                   ;4          restore dialType and speed
    sta     dialType                ;3
    lda     L180d                   ;4
    sta     dialSpeed               ;3
    rts                             ;6   =  24

CheckBit6 SUBROUTINE
    lda     ram_DF                  ;3
    bne     .exit                   ;2/3
    lda     jmpIdx                  ;3
    bmi     L14af                   ;2/3        start new sequence
    dec     timeCnt                 ;5
    bne     L148d                   ;2/3
    lda     timeCnt+1               ;3
    beq     L149e                   ;2/3        done waiting
    dec     timeCnt+1               ;5   =  27
L148d
    lda     jmpIdx                  ;3
    cmp     #$04                    ;2
    bne     L1496                   ;2/3
    jmp     WaitBit6Set             ;3   =  10  check bit 6 set

L1496
    cmp     #$05                    ;2
    bne     .exit                   ;2/3
    jmp     WaitBit6Clear           ;3   =   7  check bit 6

.exit
    rts                             ;6   =   6  return from CheckBit6

L149e
    lda     jmpIdx                  ;3
    asl                             ;2
    tay                             ;2
    lda     JmpTbl,y                ;4
    sta     dataPtr2                ;3
    lda     JmpTbl+1,y              ;4
    sta     dataPtr2+1              ;3
    jmp.ind (dataPtr2)              ;5   =  26

L14af
    lda     GL_START_PULSE          ;4
    lda     #$90                    ;2          1680 (= 315 * 5.33)
    sta     timeCnt                 ;3
    lda     #$06                    ;2
    sta     timeCnt+1               ;3
    lda     #0                      ;2
    sta     jmpIdx                  ;3
    lda     #$00                    ;2
    sta     numberIdx               ;3   =   8
.loop
    ldx     numberPtrIdx            ;3
    lda     L1812,x                 ;4          PhoneNum(|$ffff?)
    sta     numberPtr               ;3
    lda     L1813,x                 ;4
    sta     numberPtr+1             ;3
    bpl     .skip                   ;2/3
    jsr     Dial                    ;6
    jmp     .loop                   ;3

.skip
    inc     numberPtrIdx            ;5
    inc     numberPtrIdx            ;5
    rts                             ;6   =  16  return from CheckBit6

DialNumber SUBROUTINE                ;          also from CheckBit6 (jmpIdx = 0)
    ldy     numberIdx               ;3
    lda     (numberPtr),y           ;5          dial a number
    cmp     #$0f                    ;2          last digit done?
    bne     L14ed                   ;2/3         no
    ldy     #$04                    ;2          -> jmpIdx
    ldx     L180e                   ;4          -> timeCnt+1 (=99)
    lda     #$00                    ;2          -> timeCnt
    jmp     SetTimeCnt              ;3   =  17

L14ed
    cmp     #$0e                    ;2          ???
    bne     L14fc                   ;2/3
    lda     #$48                    ;2          = 840 (= 315 * 2.66; = 1680 / 2)
    sta     timeCnt                 ;3
    lda     #$03                    ;2
    sta     timeCnt+1               ;3
    inc     numberIdx               ;5
    rts                             ;6   =  25  return from CheckBit6

L14fc
    cmp     #$0d                    ;2          start of phone number?
    bne     L1510                   ;2/3         no
    lda     dialType                ;3
    bmi     .dialTypeFF             ;2/3
    lda     #$01                    ;2          start dialing
    sta     tmpVarDE                ;3
    jmp     L152c                   ;3   =  17

.dialTypeFF
    inc     numberIdx               ;5          
    jmp     DialNumber              ;3   =   8

L1510
    cmp     #$0c                    ;2          ???
    bne     L1521                   ;2/3
    lda     dialType                ;3
    bmi     .dialTypeFF             ;2/3
    beq     .dialTypeFF             ;2/3
    lda     tmpVarDE                ;3
    inc     tmpVarDE                ;5
    jmp     L152c                   ;3   =  22

L1521
    cmp     #$0b                    ;2          ???
    bne     L152c                   ;2/3
    inc     ram_DF                  ;5          = 1 (error?)    
    inc     numberIdx               ;5          -> $0d
    inc     timeCnt                 ;5
    rts                             ;6   =  25  return from CheckBit6, DialNumber

L152c
    ldy     dialSpeed               ;3
    bmi     .toneDial               ;2/3        A = ram_DE
    cmp     #$00                    ;2
    bne     .notZero                ;2/3
    lda     #10                     ;2   =   2  replace 0 with 10
.notZero
    sta     pulseCount              ;3   =   3
NextPulse
    lda     GL_STOP_PULSE           ;4
    lda     #$0c                    ;2          make some fake noise
    sta     AUDC0                   ;3
    lda     #$1f                    ;2
    sta     AUDF1                   ;3          BUG???
    lda     #$0f                    ;2
    sta     AUDV0                   ;3
    lda     L1612,y                 ;4          -> timeCnt = 26|52 (60%)
    ldy     #$01                    ;2          -> jmpIdx
    jmp     SetTimeCntLo            ;3   =  28

.toneDial
    tay                             ;2          
    asl                             ;2
    adc     #$0a                    ;2          make some fake noise
    sta     AUDF0                   ;3
    lda     #$04                    ;2
    sta     AUDC0                   ;3
    lda     #$0f                    ;2
    sta     AUDV0                   ;3
    lda     L161a,y                 ;4
    tay                             ;2          Y = $10..$1a (unordered!), tone dialing
    lda     GL_SEND_TONE,y          ;4
    lda     #$34                    ;2          -> timeCnt = 52
    ldy     #$03                    ;2          -> jmpIdx
    jmp     SetTimeCntLo            ;3   =  36

L156b SUBROUTINE                    ;           called from CheckBit6 (jmpIdx = 1)
    lda     GL_START_PULSE          ;4
    lda     #$00                    ;2
    sta     AUDV0                   ;3
    ldy     dialSpeed               ;3
    lda     L1614,y                 ;4          -> timeCnt = 18|36 (40%)
    ldy     #$02                    ;2          -> jmpIdx
    jmp     SetTimeCntLo            ;3   =  21

L157c SUBROUTINE                    ;           called from CheckBit6 (jmpIdx = 2)
    ldy     dialSpeed               ;3
    dec     pulseCount              ;5
    bne     NextPulse               ;2/3
    ldy     dialSpeed               ;3
    lda     PulseGapLo,y            ;4          -> timeCnt = 315|630
    ldx     PulseGapHi,y            ;4
    ldy     #$00                    ;2          -> jmpIdx
    inc     numberIdx               ;5
    jmp     SetTimeCnt              ;3   =  31

L1591 SUBROUTINE                    ;           called from CheckBit6 (jmpIdx = 3)
    lda     GL_SEND_TONE            ;4
    lda     #$00                    ;2          stop sound
    sta     AUDV0                   ;3
    lda     #$34                    ;2          -> timeCnt (52)
    ldy     #$00                    ;2          -> jmpIdx
    inc     numberIdx               ;5
    jmp     SetTimeCntLo            ;3   =  21

L15a1 SUBROUTINE                    ;           also called from CheckBit6 (jmpIdx = 4|5)
    lda     GL_STOP_PULSE           ;4
    lda     #$00                    ;2
    sta     ram_DF                  ;3
    ldy     #$06                    ;2          -> jmpIdx
    lda     #$90                    ;2          1680 (= 315 * 5.33)
    ldx     #$06                    ;2   =  15
SetTimeCnt   
    stx     timeCnt+1               ;3   =   3
SetTimeCntLo 
    sta     timeCnt                 ;3
    sty     jmpIdx                  ;3   =   6
.exit
    rts                             ;6   =   6

WaitBit6Set
    bit     GL_INPUT                ;4
    bvs     L15bb                   ;2/3
    rts                             ;6   =  12  return from CheckBit6

L15bb
    lda     #$05                    ;2
    sta     jmpIdx                  ;3
    lda     #$c0                    ;2
    sta     numberIdx               ;3
    rts                             ;6   =  16  return from CheckBit6

WaitBit6Clear
    bit     GL_INPUT                ;4
    bvs     L15ce                   ;2/3
    lda     #$04                    ;2
    sta     jmpIdx                  ;3
    rts                             ;6   =  17

L15ce
    dec     numberIdx               ;5
    bne     .exit                   ;2/3
    lda     #$ff                    ;2          waited too long for bit 6 == 0
    sta     jmpIdx                  ;3          start new sequence
    dec     ram_DF                  ;5   =  17
L15d8
    ldx     #$ff                    ;2
    txs                             ;2
    jsr     L10c9                   ;6          load data???
    jsr     RestoreDialData         ;6
    lda     #$00                    ;2
    sta     PF0                     ;3
    sta     PF1                     ;3
    sta     PF2                     ;3
    sta     AUDV0                   ;3
    jsr     L15a1                   ;6
    jmp     DrawCalling             ;3   =  39

L15f1 SUBROUTINE                    ;           called from CheckBit6 (jmpIdx = 6)
    lda     #$ff                    ;2
    sta     jmpIdx                  ;3
;.loop?
    rts                             ;6   =  11  return from CheckBit6

PhoneNum
    .byte   $0d,$08,$00,$00,$03,$06,$08,$01 ; PhoneNum (D) 8003681242
    .byte   $02,$04,$02,$0f,$00,$00         ; $15fe (D)
JmpTbl
    .word   DialNumber, L156b, L157c, L1591 ; $1604 (D)
    .word   L15a1, L15a1, L15f1             ; $160c (D)
L1612
    .byte   $1a,$34                         ; $1612 (D) 26, 52
L1614
    .byte   $12,$24                         ; $1614 (D) 18, 36

PulseGapLo
    .byte   $3b,$76                         ; $1616 (D) 315, 630
PulseGapHi
    .byte   $01,$02                         ; $1618 (D)

L161a ; fake tone dialing sound frequencies:
    .byte   $17,$10,$14
    .byte   $18,$11,$15
    .byte   $19,$12,$16
    .byte   $1a                             ; $1622 (D)

HandleInput SUBROUTINE
    dec     inputLo                 ;5          
    bne     L1633                   ;2/3
    lda     inputHi                 ;3
    bne     L1631                   ;2/3
    ldx     #$0e                    ;2          trigger some special code after 256 (and every 65536) frames
    jmp     ProcessInput            ;3   =  17  

L1631
    dec     inputHi                 ;5   =   5
L1633
    lda     inputDelay              ;3
    beq     .contInput              ;2/3
    dec     inputDelay              ;5   =  10
.exitInput
    rts                             ;6   =   6

.contInput
    lda     INPT4                   ;3
    and     INPT5                   ;3
    and     #$80                    ;2
    cmp     lastFire                ;3
    bne     .checkPressed           ;2/3
    jmp     SkipFire                ;3   =  16

.checkPressed
    sta     lastFire                ;3
    asl                             ;2          pressed?
    bcs     .exitInput              ;2/3         no
    jsr     SetDelay                ;6
    jsr     FirePressed             ;6
    jsr     GetData                 ;6
    bpl     L165e                   ;2/3
    bvc     StoreDigit              ;2/3
    ldx     #$00                    ;2
    jmp     ProcessInput            ;3   =  34

L165e
    sta     tmpVarDE                ;3
    jsr     GetPointer              ;6
    lsr     tmpVarDE                ;5
    bcc     L1672                   ;2/3
    bne     L167d                   ;2/3 =  18
L1669
    sta     dataPtr+1               ;3
    stx     dataPtr                 ;3
    lda     #$00                    ;2
    sta     ram_AF                  ;3
    rts                             ;6   =  17

L1672
    lsr     tmpVarDE                ;5
    bne     L1688                   ;2/3
    sta     dataPtr2+1              ;3
    stx     dataPtr2                ;3   =  13
L167a
    jmp.ind (dataPtr2)              ;5   =   5

L167d
    sta     ram_D3                  ;3
    stx     ram_D2                  ;3
    lda     #$00                    ;2
    sta     animState               ;3
    jmp     L139a                   ;3   =  14

L1688
    sta     dataPtr2+1              ;3
    stx     dataPtr2                ;3
    ldx     digit0                  ;3
    lda     digit1                  ;3
    asl                             ;2
    asl                             ;2
    asl                             ;2
    asl                             ;2
    ora     digit2                  ;3
    clc                             ;2
    jsr     L167a                   ;6
    bcc     L167d                   ;2/3
    bcs     L1669                   ;3   =  36

GetPointer SUBROUTINE
    iny                             ;2
    iny                             ;2
    iny                             ;2
    iny                             ;2
    lda     (dataPtr),y             ;5
    tax                             ;2
    iny                             ;2
    lda     (dataPtr),y             ;5
    rts                             ;6   =  28

StoreDigit
    tay                             ;2
    lda     xDial                   ;3
    asl                             ;2
    asl                             ;2
    ora     yDial                   ;3
    tax                             ;2
    lda     DialPadNums,x           ;4
    bpl     .isDigit                ;2/3
    asl                             ;2
    bmi     .isAsterisk             ;2/3
    tya                             ;2          hash
    ldy     ram_D2                  ;3
    jmp     L165e                   ;3   =  32

.isDigit
    ldx     numDigits               ;3
    cpx     #$03                    ;2
    beq     .digitsDone             ;2/3
    sta     digitLst,x              ;4
    inc     numDigits               ;5
    rts                             ;6   =  22

.isAsterisk
    lda     #$00                    ;2
    sta     numDigits               ;3   =   5
.exit
    rts                             ;6   =   6

.digitsDone
    jmp     DigitsDone              ;3   =   3

SkipFire
    lda     SWCHB                   ;4
    and     #$03                    ;2
    cmp     lastSwitches            ;3
    beq     .skipSwitches           ;2/3
    sta     lastSwitches            ;3
    jsr     SetDelay                ;6
    lsr                             ;2
    beq     .select                 ;2/3
    bcs     .exit                   ;2/3
    ldx     #$02                    ;2          reset
    bne     ProcessInput            ;3 =  31 

.select
    ldx     #$04                    ;2   =   2
ProcessInput
    ldy     #$0c                    ;2
    lda     (dataPtr),y             ;5
    sta     dataPtr2                ;3
    iny                             ;2
    lda     (dataPtr),y             ;5
    beq     .exitDir                ;2/3!
    sta     dataPtr2+1              ;3
    jmp.ind (dataPtr2)              ;5   =  27  L1c75|???

.skipSwitches
    lda     SWCHA                   ;4
    cmp     lastJoyDir              ;3
    beq     .exitDir                ;2/3
    sta     lastJoyDir              ;3
    jsr     SetDelay                ;6
    asl                             ;2
    asl                             ;2
    asl                             ;2
    asl                             ;2
    and     lastJoyDir              ;3          both joysticks work 
    asl                             ;2
    bcc     .joyRight               ;2/3        right
    asl                             ;2
    bcc     .joyLeft                ;2/3        left
    asl                             ;2
    bcc     .joyDown                ;2/3        down
    asl                             ;2
    bcc     .joyUp                  ;2/3 =  45  up
.exitDir
    rts                             ;6   =   6

.joyDown
    jsr     GetData                 ;6
    bpl     L172f                   ;2/3
    bvc     L1726                   ;2/3
    ldx     #$0a                    ;2
    jmp     ProcessInput            ;3   =  15

L1726
    lda     yDial                   ;3
    cmp     #$03                    ;2
    beq     .exitDir                ;2/3
    inc     yDial                   ;5
    rts                             ;6   =  18

L172f
    jsr     L17a3                   ;6
    bcc     .exitDir                ;2/3
    sty     ram_D2                  ;3
    cpy     #$32                    ;2
    bmi     .exitDir                ;2/3
    lda     ram_B2                  ;3
    sec                             ;2
    sbc     ram_D2                  ;3
    cmp     #$0c                    ;2
    beq     L1745                   ;2/3
    bpl     .exitDir                ;2/3 =  29
L1745
    ldy     ram_B1                  ;3
    jsr     L17a3                   ;6
    bcc     .exitDir                ;2/3
    sty     ram_B1                  ;3
    rts                             ;6   =  20

.joyUp
    jsr     GetData                 ;6
    bpl     L1762                   ;2/3
    bvc     L175b                   ;2/3
    ldx     #$0c                    ;2
    jmp     ProcessInput            ;3   =  15

L175b
    lda     yDial                   ;3
    beq     .exitDir                   ;2/3
    dec     yDial                   ;5
    rts                             ;6   =  16

L1762
    jsr     L17b4                   ;6
    bcc     .exitDir                   ;2/3
    sty     ram_D2                  ;3
    tya                             ;2
    sec                             ;2
    sbc     ram_B1                  ;3
    cmp     #$06                    ;2
    bne     .exitDir                ;2/3
    ldy     ram_B1                  ;3
    jsr     L17b4                   ;6
    bcc     .exitDir                ;2/3
    sty     ram_B1                  ;3
    rts                             ;6   =  42

.joyRight
    jsr     GetData                 ;6
    bpl     L178b                   ;2/3
    bvs     L178b                   ;2/3
    lda     xDial                   ;3
    cmp     #$02                    ;2
    beq     L178a                   ;2/3
    inc     xDial                   ;5   =  22
L178a
    rts                             ;6   =   6

L178b
    ldx     #$06                    ;2
    jmp     ProcessInput            ;3   =   5

.joyLeft
    jsr     GetData                 ;6
    bpl     L179e                   ;2/3
    bvs     L179e                   ;2/3
    lda     xDial                   ;3
    beq     L179d                   ;2/3
    dec     xDial                   ;5   =  20
L179d
    rts                             ;6   =   6

L179e
    ldx     #$08                    ;2
    jmp     ProcessInput            ;3   =   5

L17a3 SUBROUTINE
    lda     (dataPtr),y             ;5
    bmi     L17b2                   ;2/3 =   7
L17a7
    tya                             ;2
    clc                             ;2
    adc     #$06                    ;2
    tay                             ;2
    lda     (dataPtr),y             ;5
    beq     L17a7                   ;2/3
    bpl     L17c3                   ;2/3 =  17
L17b2
    clc                             ;2
    rts                             ;6   =   8

L17b4 SUBROUTINE
    cpy     #$14                    ;2
    beq     L17b2                   ;2/3
    tya                             ;2
    sec                             ;2
    sbc     #$06                    ;2
    tay                             ;2
    lda     (dataPtr),y             ;5
    bmi     L17b2                   ;2/3
    beq     L17b4                   ;2/3 =  21
L17c3
    sec                             ;2
    rts                             ;6   =   8

DialPadNums
    .byte   $01,$04,$07,$c0                 ; $17c5 (D)
    .byte   $02,$05,$08,$00
    .byte   $03,$06,$09,$80                 ; $17cd (D)

GetData SUBROUTINE
    ldy     ram_D2                  ;3
    lda     (dataPtr),y             ;5
    sta     tmpVarDE                ;3
    and     #$0f                    ;2
    bit     tmpVarDE                ;3
    rts                             ;6   =  22

SetDelay SUBROUTINE
    pha                             ;3
    lda     #$05                    ;2
    sta     inputDelay              ;3
    pla                             ;4
    rts                             ;6   =  18

FirePressed SUBROUTINE
    lda     #<FirePressedPtr        ;2
    ldx     #>FirePressedPtr        ;2   =   4
.setPtr
    sta     unusedPtr               ;3          this sets a pointer which is nowhere used
    stx     unusedPtr+1             ;3
    lda     #$01                    ;2
    sta     unusedFlag              ;3
    rts                             ;6   =  17

DigitsDone
    lda     #<DigitsDonePtr         ;2
    ldx     #>DigitsDonePtr         ;2
    jmp     .setPtr                 ;3   =   7

FirePressedPtr
    .byte   $86,$c1,$3f,$88                 ; $17f7 (D)
DigitsDonePtr
    .byte   $86,$c6,$9f,$fe                 ; $17fb (D)
    .byte   $88                             ; $17ff (D)


;***********************************************************
;      Bank 2
;***********************************************************
; 1K ROM bank #2

ram_C0          = $c0               ;   used differently here
ram_C1          = $c1               ;   used differently here

    RORG    $1400                   ;               code for slice 1/3

Load SUBROUTINE
    jmp     .load                   ;3   =   3 

L1403    
    .word   LoadByte                ; $1403 (D)
    
.load
    ldx     #$00                    ;2         
    stx     loadFlag                ;3         
    stx     COLUBK                  ;3         
    lda     #RED|$3                 ;2         
    sta     COLUPF                  ;3   
    inx                             ;2         
    stx     CTRLPF                  ;3              = 1       
    lda     animState               ;3              animation already started?
    bne     .setupPF                ;2/3             yes, continue animation
    inc     animState               ;5               no, start animation
    inx                             ;2         
    stx     animDelay               ;3              = 2   
    lda     #$1b                    ;2         
    sta     xCurtain                ;3         
    jsr     LoadData                ;6         
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
    jsr     SendLoadHeader10        ;6         
    jsr     EndSend                 ;6   =  17 
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

LoadData SUBROUTINE
    lda     #10                     ;2         
    sta     retryCnt                ;3         
    lda     #$00                    ;2         
    sta     ram_D7                  ;3   =  10 
.retryLoop
    lda     ram_D7                  ;3         
    bne     .loopBytes              ;2/3       
    lda     SL0_B3                  ;4              switch ROM bank 3 into slice 0
    jsr     SendLoadHeader20        ;6   =  15      code in bank 3
.loopBytes
    lda     SL0_R1                  ;4              switch RAM bank 1 into slice 0 for executing
    jsr     $100f                   ;6              execute code in RAM bank 1 (LoadByte)
    ldx     SL0_B0                  ;4              switch ROM bank 0 into slice 0
    bcs     .failed                 ;2/3       
    jsr     StoreByte               ;6         
    jmp     .loopBytes              ;3   =  25 
    
.failed ; or done
    dec     retryCnt                ;5         
    bpl     .retryLoop              ;2/3       
    lda     #$00                    ;2         
    sta     animState               ;3              reset animation state 
    sec                             ;2         
    rts                             ;6   =  20 
    
SendHeader SUBROUTINE
    jsr     InitialWait             ;6         
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
    
EndSend SUBROUTINE
    lda     GL_START_PULSE          ;4         
    jsr     Wait4High               ;6         
    bcc     .found                  ;2/3       
    lda     retryCnt                ;3         
    sec                             ;2         
    sbc     #4                      ;2         
    sta     retryCnt                ;3   =  22 
.found
    rts                             ;6   =   6 
    
InitialWait SUBROUTINE
    jsr     Wait4Low                ;6         
    lda     GL_CBB                  ;4         
    jsr     Wait100ms               ;6         
    jsr     Wait100ms               ;6         
    rts                             ;6   =  28 
    
Wait4High SUBROUTINE
; waits ~56 seconds
    ldy     #$00                    ;2         
    beq     L1503                   ;3 =     5

Wait4Low SUBROUTINE
; waits ~11 seconds
    ldy     #$01                    ;2   =   2 
L1503
    lda     #50                     ;2         
    sta     tmpVarDE                ;3         
    ldx     WaitTbl2,y              ;4   =   9  0|50
.retry
    lda     #$ff                    ;2          -> ~219ms
    sta     T1024T                  ;4   =   6 
.loopWait
    lda     TIM8T                   ;4         
    bmi     .waitDone               ;2/3       
    lda     GL_INPUT                ;4          check signal in bit 6
    and     #$40                    ;2         
    eor     SignalTbl,y             ;4          $40|$00   
    beq     .signalFound            ;2/3       
    ldx     WaitTbl2,y              ;4          0|50
    jmp     .loopWait               ;3   =  25 
    
.waitDone
    dec     tmpVarDE                ;5         
    bne     .retry                  ;2/3       
    sec                             ;2         
    rts                             ;6   =  15 
    
.signalFound
    dex                             ;2         
    bne     .loopWait               ;2/3       
    clc                             ;2         
    rts                             ;6   =  12 
    
WaitTbl2
    .byte   0,50                            ; $152f (D)
SignalTbl
    .byte   $40,$00                         ; $1531 (D)
    
Wait100ms SUBROUTINE
    pha                             ;3         
    lda     #$75                    ;2         
    sta     T1024T                  ;4   =   9 
.waitTim
    lda     TIM8T                   ;4         
    bpl     .waitTim                ;2/3       
    pla                             ;4         
    rts                             ;6   =  16 
    
TransferByte SUBROUTINE
    sta     transferByte            ;3         
    pha                             ;3         
    txa                             ;2         
    pha                             ;3         
    tya                             ;2         
    pha                             ;3         
    lda     #$ff                    ;2         
    sta     TIM8T                   ;4         
    clc                             ;2         
    rol     transferByte            ;5         
    ldy     #10                     ;2   =  31 
.loop
    lda     TIM8T                   ;4         
    bpl     .loop                   ;2/3       
    lda     #$85                    ;2              = 1064 + ~31 = ~1095 cycles = ~1 ms.      
    sta     TIM8T                   ;4         
    ror     transferByte            ;5         
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
    
LoadByte SUBROUTINE
    lda     loadFlag                ;3        
    bne     .loadByte               ;2/3      
    jsr     AdjustSpeed             ;6         
    bcc     .loadByte               ;2/3       
    rts                             ;6   =  19 
    
.loadByte
    ldy     #8                      ;2              8 bits
    lda     #$00                    ;2         
    sta     transferByte            ;3   =   7 
.loop
    ldx     TIM8T                   ;4         
    bpl     .loop                   ;2/3       
    lda     #$fb                    ;2         
    dec     ram_C1                  ;5         
    bne     L1597                   ;2/3       
    ldx     speedIdx                ;3         
    lda     SpeedLo,x               ;4   =  22 
L1597
    clc                             ;2         
    adc     TIM1T                   ;4         
    sta     TIM1T                   ;4         
    lda     ram_C1                  ;3         
    bpl     .loop                   ;2/3       
    ldx     speedIdx                ;3         
    lda     SpeedHi,x               ;4         
    sta     ram_C1                  ;3         
    bit     GL_INPUT                ;4         
    clc                             ;2         
    bpl     L15b0                   ;2/3       
    sec                             ;2   =  35 
L15b0
    ror     transferByte            ;5         
    dey                             ;2         
    bne     .loop                   ;2/3       
    lda     transferByte            ;3         
    clc                             ;2         
    rts                             ;6   =  20 
    
AdjustSpeed SUBROUTINE
; adjust transfer speed?
    lda     #100                    ;2         
    sta     ram_C0                  ;3   =   5 
.loop
    lda     #$ba                    ;2         
    sta     TIM64T                  ;4         
    lda     #$00                    ;2         
    sta     transferByte            ;3         
    sta     speedIdx                ;3         
    lda     #$07                    ;2         
    sta     tmpVarDE                ;3   =  19 
L15cc
    lda     TIM8T                   ;4         
    bpl     L15d7                   ;2/3       
    dec     ram_C0                  ;5         
    bne     .loop                   ;2/3       
    sec                             ;2         
    rts                             ;6   =  21 
    
L15d7
    bit     GL_INPUT                ;4         
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
.speedTmp   = speedIdx
    lda     TIM8T                   ;4         
    bmi     .loop                   ;2/3       
    ldx     TIM1T                   ;4         
    lda     GL_INPUT                ;4         
    eor     transferByte            ;3         
    and     #$80                    ;2         
    beq     L15ec                   ;2/3       
    sty     TIM8T                   ;4         
    lda     GL_INPUT                ;4         
    sta     transferByte            ;3         
    txa                             ;2         
    eor     #$ff                    ;2         
    tax                             ;2         
    lsr                             ;2         
    cmp     #$19                    ;2         
    bmi     .loop                   ;2/3!      
    cmp     #$73                    ;2         
    bpl     .loop                   ;2/3!      
    txa                             ;2         
    ldx     .speedTmp               ;3         
    beq     L161b                   ;2/3       
    clc                             ;2         
    adc     .speedTmp               ;3         
    ror                             ;2   =  62 
L161b
    sta     .speedTmp               ;3         
    dec     tmpVarDE                ;5         
    bne     L15e3                   ;2/3!      
    ldx     #$03                    ;2         
    lda     .speedTmp               ;3   =  15 
L1625
    cmp     FromTbl,x               ;4         
    bmi     L1657                   ;2/3       
    cmp     ToTbl,x                 ;4         
    bpl     L1657                   ;2/3       
    stx     speedIdx                ;3         
    lda     L166d,x                 ;4         
    clc                             ;2         
    adc     TIM1T                   ;4         
    sta     TIM8T                   ;4         
    inc     loadFlag                ;5          enable load flag     
    jsr     InitStoreD7             ;6   =  40 
L1640
    lda     TIM8T                   ;4         
    bpl     L1640                   ;2/3       
    ldx     speedIdx                ;3         
    lda     SpeedHi,x               ;4         
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
    .byte   $50,$69,$7c,$a0                 ; $165d (D)  80, 105, 124, 160
ToTbl
    .byte   $68,$7b,$9f,$cd                 ; $1661 (D) 104, 123, 159, 205
SpeedHi
    .byte   $02,$03,$04,$05                 ; $1665 (D) 758, 911, 1064, 1371
SpeedLo
    .byte   $f6,$8f,$28,$f1                 ; $1669 (D)
L166d
    .byte   $2b,$34,$41,$5b                 ; $166d (D)
    
L1671 SUBROUTINE
    lda     ram_DB                  ;3         
    jsr     Div16                   ;6         
    cmp     bitIdx                  ;3         
    bpl     L167e                   ;2/3       
    ldy     #$00                    ;2         
    sty     bitSet                  ;3   =  19 
L167e
    sta     bitIdx                  ;3         
    tay                             ;2         
    lda     Pot2Tbl,y               ;4         
    and     bitSet                  ;3         
    bne     .exit                   ;2/3       
    lda     Pot2Tbl,y               ;4         
    ora     bitSet                  ;3         
    sta     bitSet                  ;3         
    jsr     AnimLoad                ;6         
    lda     #10                     ;2         
    sta     retryCnt                ;3   =  35 
.exit
    rts                             ;6   =   6 
    
Pot2Tbl
    .byte   $01,$02,$04,$08,$10,$20,$40,$80 ; $1697 (D)
    
SendLoadHeader10 SUBROUTINE
    jsr     SendHeader              ;6         
    lda     #$10                    ;2         
    jsr     SendByte                ;6         
    lda     #$00                    ;2         
    jsr     SendByte                ;6         
    lda     bitSet                  ;3         
    jsr     SendByte                ;6         
    lda     #$00                    ;2         
    jsr     SendByte                ;6         
    jsr     SendByte                ;6         
    jsr     SendCRC                 ;6         
    jsr     EndSend                 ;6         
    rts                             ;6   =  63 
    
StoreByte SUBROUTINE
; A = stored byte
    bit     storeFlags              ;3         
    bmi     L16e0                   ;2/3       
    ldy     #$00                    ;2         
    sta     (dataPtr2),y            ;6              = L1308..|(L180a)
    jsr     UpdateCRC               ;6         
    lda     ram_D7                  ;3         
    cmp     ram_D0                  ;3         
    bne     L16ea                   ;2/3       
    inc     dataPtr2                ;5         
    bne     L16d7                   ;2/3       
    inc     dataPtr2+1              ;5   =  39 
L16d7
    dec     storeCnt                ;5         
    beq     L16dc                   ;2/3       
    rts                             ;6   =  13 
    
L16dc
    sec                             ;2         
    ror     storeFlags              ;5         
    rts                             ;6   =  13 
    
L16e0
    ldy     #$00                    ;2          hi
    bvc     L16e5                   ;2/3       
    iny                             ;2   =   6  lo
L16e5
    cmp.wy  crcLst,y                ;4         
    beq     L16ed                   ;2/3 =   6 
L16ea
    dec     loadFlag                ;5          disable load flag  
    rts                             ;6   =  11 
    
L16ed
    bvc     L16dc                   ;2/3       
    lda     storeFlags              ;3         
    and     #$0f                    ;2         
    beq     L16f8                   ;2/3       
    jmp     L171e                   ;3   =  12 
    
L16f8
    lda     ram_D8                  ;3         
    cmp     ram_D1                  ;3         
    bne     L16ea                   ;2/3       
    lda     ram_DA                  ;3         
    beq     L171e                   ;2/3       
    sta     storeCnt                ;3         
    lda     ram_DB                  ;3         
    and     #$0f                    ;2         
    tax                             ;2         
    lda     SL2_WX,x                ;4         
    lda     ram_DD                  ;3         
    eor     #$18                    ;2         
    sta     dataPtr2+1              ;3         
    lda     ram_DC                  ;3         
    sta     dataPtr2                ;3         
    lda     #$08                    ;2         
    sta     storeFlags              ;3         
    jsr     ResetCRC                ;6         
    rts                             ;6   =  58 
    
L171e
    dec     loadFlag                ;5          disable load flag    
    lda     ram_D9                  ;3         
    and     #$f0                    ;2         
    cmp     #$30                    ;2         
    bne     L1738                   ;2/3       
    jsr     L1671                   ;6         
    lda     ram_D9                  ;3         
    and     #$04                    ;2         
    beq     .exit                   ;2/3 =  27 
L1731
    jsr     SendLoadHeader10        ;6         
    jsr     EndSend                 ;6   =  12 
.exit
    rts                             ;6   =   6 
    
L1738
    cmp     #$40                    ;2         
    bne     .exit                   ;2/3       
    lda     #$01                    ;2         
    sta     bitSet                  ;3         
    lda     ram_DB                  ;3         
    bpl     L1747                   ;2/3       
    jsr     SendLoadHeader10        ;6   =  20 
L1747
    lsr     ram_D9                  ;5         
    bcs     L1750                   ;2/3       
    lda     GL_STOP_C80             ;4         
    inc     ram_DF                  ;5   =  16 
L1750
    lsr     ram_D9                  ;5         
    bcs     L175d                   ;2/3       
    lda     SL0_B3                  ;4              switch ROM bank 3 into slice 0
    jsr     L12d0                   ;6              code in bank 3
    lda     SL0_B0                  ;4   =  21      switch ROM bank 0 into slice 0
L175d
    lsr     ram_D9                  ;5         
    bcs     L1791                   ;2/3       
    lsr     ram_D9                  ;5         
    bcs     L179f                   ;2/3       
    lda     ram_DC                  ;3         
    jsr     Div16                   ;6         
    tax                             ;2         
    lda     SL0_BX,x                ;4              
    lda     ram_DD                  ;3         
    jsr     Div16                   ;6         
    tax                             ;2         
    lda     SL2_BX,x                ;4         
    lda     ram_DD                  ;3         
    and     #$0f                    ;2         
    tax                             ;2         
    lda     SL3_BX,x                ;4         
    ldx     #$06                    ;2   =  57 
.loopCopy
    lda     RamCode,x               ;4         
    sta     ramCode,x               ;4         
    dex                             ;2         
    bpl     .loopCopy               ;2/3       
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
    stx     slice2Bank              ;3         
    ldx     #$ff                    ;2         
    txs                             ;2         
    lda     ram_DC                  ;3              -> dataPtr
    ldx     ram_DD                  ;3              -> dataPtr+1
    jmp     (L1003)                 ;5   =  24      -> ReadData?

RamCode 
    lda     SL1_BX,x                ;4         
    jmp     (L1ffc)                 ;5   =   9 

L17b4 SUBROUTINE
    lda     ram_DB                  ;3         
    and     #$0f                    ;2         
    tax                             ;2         
    lda     ram_DD                  ;3         
    eor     #$18                    ;2         
    sta     ram_DD                  ;3         
    lda     SL2_BX,x                ;4         
    rts                             ;6   =  25 
    
Div16 SUBROUTINE
    lsr                             ;2         
    lsr                             ;2         
    lsr                             ;2         
    lsr                             ;2         
    rts                             ;6   =  14 
    
InitStoreD7 SUBROUTINE
    lda     #$00                    ;2         
    sta     storeFlags              ;3         
    lda     #<ram_D7                ;2         
    sta     dataPtr2                ;3         
    lda     #>ram_D7                ;2         
    sta     dataPtr2+1              ;3         
    lda     #$07                    ;2         
    sta     storeCnt                ;3         
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


;***********************************************************
;      Bank 3
;***********************************************************
; 1K ROM bank #3

    RORG    $1c00                           ;               code for slice 3/3

L1c00
    .byte   $c1                             ; $1c00 (D) ram_AF
    .byte   $1e                             ; $1c01 (D) ram_D2+ram_B1
    .byte   $00,$00,$00,$00                 ; $1c02 (D) ram_C6, jmpIdx, timeCnt, timeCnt+1
    .word   MessagePtrs                     ; $1c06
    .byte   $64                             ; $1c08 -> TIM64T
    .byte   $89                             ; $1c09 -> TIM64T
    .byte   $7f                             ; $1c0a -> ram_B0
    .byte   $0a                             ; $1c0b -> WaitLines
    .word   L1c75                               
    .byte   $00,$00,$00                     ; $1c0e (D)    ? ? dataPtr2
    .byte   $00                             ; $1c11 (D)    dataPtr2+1
    .word   L1c69
    .byte   $c0,$00,$05,$03,$00,$00,$c0     ; $1c14 (D)
    .byte   $05,$05,$00,$00,$00,$ff         ; $1c1b (D)

MessagePtrs
    .word   ReadManual0, ReadManual1, ReadManual2
    .word   ReadManual3, ReadManual4, ReadManual5-$1000
ReadManual0
    .byte   %00001110 ; |    ### |            $1c2d (G)     READ MANUAL
    .byte   %00001010 ; |    # # |            $1c2e (G)
    .byte   %11001110 ; |##  ### |            $1c2f (G)
    .byte   %00001100 ; |    ##  |            $1c30 (G)
    .byte   %00001010 ; |    # # |            $1c31 (G)

    .byte   %00001110 ; |    ### |            $1c32 (G)     PRESS RESET
    .byte   %00001010 ; |    # # |            $1c33 (G)
    .byte   %11001110 ; |##  ### |            $1c34 (G)
    .byte   %00001000 ; |    #   |            $1c35 (G)
    .byte   %00001000 ; |    #   |            $1c36 (G)
ReadManual1
    .byte   %11101110 ; |### ### |            $1c37 (G)
    .byte   %10001010 ; |#   # # |            $1c38 (G)
    .byte   %11001110 ; |##  ### |            $1c39 (G)
    .byte   %10001010 ; |#   # # |            $1c3a (G)
    .byte   %11101010 ; |### # # |            $1c3b (G)

    .byte   %11101110 ; |### ### |            $1c3c (G)
    .byte   %10101000 ; |# # #   |            $1c3d (G)
    .byte   %11101100 ; |### ##  |            $1c3e (G)
    .byte   %11001000 ; |##  #   |            $1c3f (G)
    .byte   %10101110 ; |# # ### |            $1c40 (G)
ReadManual2
    .byte   %11000010 ; |##    # |            $1c41 (G)
    .byte   %10100011 ; |# #   ##|            $1c42 (G)
    .byte   %10100010 ; |# #   # |            $1c43 (G)
    .byte   %10100010 ; |# #   # |            $1c44 (G)
    .byte   %11000010 ; |##    # |            $1c45 (G)

    .byte   %11101110 ; |### ### |            $1c46 (G)
    .byte   %10001000 ; |#   #   |            $1c47 (G)
    .byte   %11101110 ; |### ### |            $1c48 (G)
    .byte   %00100010 ; |  #   # |            $1c49 (G)
    .byte   %11101110 ; |### ### |            $1c4a (G)
ReadManual3
    .byte   %00101110 ; |  # ### |            $1c4b (G)
    .byte   %01101010 ; | ## # # |            $1c4c (G)
    .byte   %10101110 ; |# # ### |            $1c4d (G)
    .byte   %00101010 ; |  # # # |            $1c4e (G)
    .byte   %00101010 ; |  # # # |            $1c4f (G)

    .byte   %00111011 ; |  ### ##|            $1c50 (G)
    .byte   %00101010 ; |  # # # |            $1c51 (G)
    .byte   %00111011 ; |  ### ##|            $1c52 (G)
    .byte   %00110010 ; |  ##  # |            $1c53 (G)
    .byte   %00101011 ; |  # # ##|            $1c54 (G)
ReadManual4
    .byte   %10010101 ; |#  # # #|            $1c55 (G)
    .byte   %11010101 ; |## # # #|            $1c56 (G)
    .byte   %11110101 ; |#### # #|            $1c57 (G)
    .byte   %10110101 ; |# ## # #|            $1c58 (G)
    .byte   %10010111 ; |#  # ###|            $1c59 (G)

    .byte   %10111011 ; |# ### ##|            $1c5a (G)
    .byte   %00100010 ; |  #   # |            $1c5b (G)
    .byte   %00111011 ; |  ### ##|            $1c5c (G)
    .byte   %00001010 ; |    # # |            $1c5d (G)
    .byte   %10111011 ; |# ### ##|            $1c5e (G)
ReadManual5
    .byte   %01110100 ; | ### #  |            $1c5f (G)
    .byte   %01010100 ; | # # #  |            $1c60 (G)
    .byte   %01110100 ; | ### #  |            $1c61 (G)
    .byte   %01010100 ; | # # #  |            $1c62 (G)
    .byte   %01010111 ; | # # ###|            $1c63 (G)

    .byte   %10111000 ; |# ###   |            $1c64 (G)
    .byte   %00010000 ; |   #    |            $1c65 (G)
    .byte   %10010000 ; |#  #    |            $1c66 (G)
    .byte   %00010000 ; |   #    |            $1c67 (G)
    .byte   %10010000 ; |#  #    |            $1c68 (G)

L1c69
    inc     frameCnt                ;5
    bne     L1c74                   ;2/3
    lda     color                   ;3
    clc                             ;2
    adc     #$50                    ;2
    sta     color                   ;3   =  17
L1c74
    rts                             ;6   =   6

L1c75 SUBROUTINE
    cpx     #$02                    ;2              RESET?
    beq     L1c7a                   ;2/3             yes
    rts                             ;6   =  10       no, ignore

L1c7a
    ldy     SL2_W1                  ;4              switch RAM bank 1 into slice 2 for writing (L1800..)
    ldx     #$00                    ;2   =   6
.loopCopy
    lda     L1c9a,x                 ;4              copy code into RAM for execution
    sta     L180e,x                 ;5              
    inx                             ;2
    cpx     #$08                    ;2              8 bytes
    bne     .loopCopy               ;2/3
    lda     #$10                    ;2
    sta     ram_D2                  ;3
    lda     #$00                    ;2
    sta     ram_D3                  ;3
    sta     dialCount               ;3
    jsr     Dial                    ;6
    jmp     L13a3                   ;3   =  37

L1c9a
    .byte   99                      ;       L180e   -> timeCnt+1
    jmp     (L1403)                 ;       L180f   -> LoadByte (called via jmp $100f)
    .word   PhoneNum                ;       L1812   -> numberPtr
    .word   $ffff                   ;       L1814   -> 2nd numberPtr

Check1800 SUBROUTINE
    ldx     #$03                    ;2
    lda     #$e9                    ;2   =   4
.loopRAM
    cmp     L1802,x                 ;4              undefined! ($1802..$1805 == $48,$a4,$d2,$e9)
;  IF ORIGINAL
    bne     .failed                 ;2/3
;  ELSE
;    ds      2, $ea
;  ENDIF
    asl                             ;2
    dex                             ;2
    bne     .loopRAM                ;2/3
    lda     #>L1800                 ;2
    sta     dataPtr2+1              ;3
    ldy     #<L1800                 ;2
    sty     dataPtr2                ;3
    tya                             ;2   =  24  = 0
.loop
    clc                             ;2
    adc     (dataPtr2),y            ;5          undefined! 
    inc     dataPtr2                ;5
    bne     .skipHi                 ;2/3
    inc     dataPtr2+1              ;5   =   9
.skipHi
    ldx     L1800                   ;4          undefined! 
    cpx     dataPtr2                ;3
    bne     .loop                   ;2/3
    ldx     L1801                   ;4          undefined! 
    cpx     dataPtr2+1              ;3
    bne     .loop                   ;2/3
  IF ORIGINAL
    cmp     (dataPtr2),y            ;5   =  23
  ELSE
    lda     #0
  ENDIF
.failed
    rts                             ;6   =   6

    .byte   $04,$11,$6c,$33,$fd,$20,$31,$10 ; $1cd2 (D)
    .byte   $c9,$ba,$f0,$de,$c9,$e0         ; $1cda (D)

;===============================================================================

    RORG . - $400                   ;           code for slice 2
    
XPosSprite0 SUBROUTINE
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    ldx     #$07                    ;2   =   2
L18e4
    dex                             ;2
    bne     L18e4                   ;2/3
    nop                             ;2
    sta     RESP0                   ;3
    sta     WSYNC                   ;3   =  12
;---------------------------------------
    lda     #$03                    ;2
    sta     NUSIZ0                  ;3
    lda     #$00                    ;2
    sta     NUSIZ1                  ;3
    sta     VDELP0                  ;3
    sta     VDELP1                  ;3
    rts                             ;6   =  22

DDD SUBROUTINE
.loop 
    nop                             ;2
    lda     (ram_91),y              ;5
    and     ram_96|$100             ;4          = $ff       flash
    eor     ram_99|$100             ;4          = $00|$ff   invert
    tax                             ;2
    lda     (ram_93),y              ;5
    and     ram_97|$100             ;4          = $ff
    eor     ram_9A|$100             ;4          = $00|$ff
    stx     GRP0                    ;3
    jmp     .cont                   ;3   =  36  weird!?

.cont
    sta     GRP0                    ;3
    inc     drawIdx                 ;5
DrawDialDigits
    ldy     drawIdx                 ;3
    lda     (ram_8F),y              ;5
    and     ram_95|$100             ;4          = $ff
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    eor     ram_98|$100             ;4          = $00|$ff
    sta     GRP0                    ;3
    cpy     drawEnd                 ;3
    bne     .loop                   ;2/3!
    lda     #$00                    ;2
    sta     GRP0                    ;3
    rts                             ;6   =  23
    
DrawDialPad SUBROUTINE
    lda     drawIdx                 ;3
    sta     COLUPF                  ;3
    lda     drawEnd                 ;3
    sta     drawEndCopy             ;3   =  12
    lda     #%11111110              ;2
    sta     PF2                     ;3
    jsr     XPosSprite0             ;6
    ldx     #$06                    ;2
    jsr     WaitLines               ;6
    lda     #<DigitCol0             ;2          1 4 7 *
    sta     ram_8F                  ;3
    lda     #<DigitCol1             ;2          2 5 8 0
    sta     ram_91                  ;3
    lda     #<DigitCol2             ;2          3 6 9 #
    sta     ram_93                  ;3
    lda     #>DigitCol0             ;2
    sta     ram_90                  ;3
    lda     #>DigitCol1             ;2
    sta     ram_92                  ;3
    lda     #>DigitCol2             ;2
    sta     ram_94                  ;3
    ldy     #$00                    ;2
    sty     drawIdx                 ;3
    tya                             ;2   =  56
.loopRow
    clc                             ;2
    adc     #DIGIT_H                ;2
    sta     drawEnd                 ;3
    sty     tmpVarDE                ;3
    jsr     L19a7                   ;6
    jsr     DrawDialDigits          ;6
    ldx     #$03                    ;2
    jsr     WaitLines               ;6
    ldy     tmpVarDE                ;3
    iny                             ;2
    lda     drawIdx                 ;3
    cmp     #DIGIT_H*3+1            ;2
    bmi     .loopRow                ;2/3
    ldx     #$06                    ;2
    jsr     WaitLines               ;6
    lda     #$00                    ;2
    sta     PF2                     ;3
    ldx     #$03                    ;2
    jsr     WaitLines               ;6
    lda     #$00                    ;2
    sta     drawIdx                 ;3
    lda     #DIGIT_H                ;2
    sta     drawEnd                 ;3
    jsr     SetupBottomDigits       ;6          prepare digits at bottom squares
    jsr     FlashCurrentDigit       ;6
    jsr     DrawDialDigits          ;6
    lda     #YELLOW|$f              ;2
    sta     COLUPF                  ;3
    lda     #%10011000              ;2
    sta     WSYNC                   ;3   = 101
;---------------------------------------
    sta     PF2                     ;3          underline
    lda     #$00                    ;2
    sta     WSYNC                   ;3   =   8
;---------------------------------------
    sta     PF2                     ;3
    rts                             ;6   =   9
    
L19a7 SUBROUTINE
    jsr     ClearFlash              ;6
    cpy     yDial                   ;3
    bne     .exit                   ;2/3
    ldx     xDial                   ;3
    dec     ram_98,x                ;6   =  20      invert
.exit
    rts                             ;6   =   6

FlashCurrentDigit ;SUBROUTINE
    jsr     ClearFlash              ;6
    ldx     numDigits               ;3
    cpx     #$03                    ;2
    beq     .exit                   ;2/3
    lda     flashState              ;3
    sta     ram_95,x                ;4              flash
    dec     flashTimer              ;5
    bne     .exit                   ;2/3
    lda     flashState              ;3
    eor     #$ff                    ;2
    sta     flashState              ;3
    lda     #$14                    ;2
    sta     flashTimer              ;3
    rts                             ;6   =  46

ClearFlash SUBROUTINE
    ldx     #$ff                    ;2
    stx     ram_95                  ;3
    stx     ram_96                  ;3
    stx     ram_97                  ;3
    inx                             ;2
    stx     ram_98                  ;3
    stx     ram_99                  ;3
    stx     ram_9A                  ;3
    rts                             ;6   =  28
        
SetupBottomDigits SUBROUTINE
    lda     #<BlockGfx              ;2
    sta     ram_8F                  ;3
    sta     ram_91                  ;3
    sta     ram_93                  ;3
    lda     #>BlockGfx              ;2
    sta     ram_90                  ;3
    sta     ram_92                  ;3
    sta     ram_94                  ;3
    ldx     #0                      ;2
    lda     drawEndCopy             ;3
    beq     .drawBlocks             ;2/3!
.loop
    cpx     numDigits               ;3
    bpl     .drawBlocks             ;2/3!
    ldy     digitLst,x              ;4
    lda     DigitPtr,y              ;4
    clc                             ;2
    adc     #<DigitGfx              ;2
    pha                             ;3
    lda     #$00                    ;2
    adc     #>DigitGfx              ;2
    pha                             ;3
    txa                             ;2
    asl                             ;2
    tay                             ;2
    pla                             ;4
    sta.wy  ptrLst+1,y              ;5
    pla                             ;4
    sta.wy  ptrLst,y                ;5
    inx                             ;2
    jmp     .loop                   ;3   =  61

.drawBlocks
    lda     WaitTbl,x               ;4
    beq     .exitWait               ;2/3
    tax                             ;2
    jsr     WaitLines               ;6   =  14
.exitWait
    rts                             ;6   =   6

WaitTbl 
    .byte   $02,$02,$01,$00                 ; $1a20 (D)

DigitGfx
DigitCol0
OneGfx
    .byte   %00000000 ; |        |            $1a24 (G)
    .byte   %00001000 ; |    #   |            $1a25 (G)
    .byte   %00011000 ; |   ##   |            $1a26 (G)
    .byte   %00001000 ; |    #   |            $1a27 (G)
    .byte   %00001000 ; |    #   |            $1a28 (G)
    .byte   %00001000 ; |    #   |            $1a29 (G)
    .byte   %00001000 ; |    #   |            $1a2a (G)
    .byte   %00001000 ; |    #   |            $1a2b (G)
    .byte   %00001000 ; |    #   |            $1a2c (G)
    .byte   %00011100 ; |   ###  |            $1a2d (G)
    .byte   %00000000 ; |        |            $1a2e (G)
DIGIT_H = . - OneGfx
FourGfx
    .byte   %00000000 ; |        |            $1a2f (G)
    .byte   %00101000 ; |  # #   |            $1a30 (G)
    .byte   %00101000 ; |  # #   |            $1a31 (G)
    .byte   %00101000 ; |  # #   |            $1a32 (G)
    .byte   %00101000 ; |  # #   |            $1a33 (G)
    .byte   %00101000 ; |  # #   |            $1a34 (G)
    .byte   %00111100 ; |  ####  |            $1a35 (G)
    .byte   %00001000 ; |    #   |            $1a36 (G)
    .byte   %00001000 ; |    #   |            $1a37 (G)
    .byte   %00001000 ; |    #   |            $1a38 (G)
    .byte   %00000000 ; |        |            $1a39 (G)
SevenGfx
    .byte   %00000000 ; |        |            $1a3a (G)
    .byte   %00111100 ; |  ####  |            $1a3b (G)
    .byte   %00000100 ; |     #  |            $1a3c (G)
    .byte   %00000100 ; |     #  |            $1a3d (G)
    .byte   %00000100 ; |     #  |            $1a3e (G)
    .byte   %00001000 ; |    #   |            $1a3f (G)
    .byte   %00001000 ; |    #   |            $1a40 (G)
    .byte   %00010000 ; |   #    |            $1a41 (G)
    .byte   %00010000 ; |   #    |            $1a42 (G)
    .byte   %00010000 ; |   #    |            $1a43 (G)
    .byte   %00000000 ; |        |            $1a44 (G)
StarGfx
    .byte   %00000000 ; |        |            $1a45 (G)
    .byte   %00000000 ; |        |            $1a46 (G)
    .byte   %01000010 ; | #    # |            $1a47 (G)
    .byte   %00100100 ; |  #  #  |            $1a48 (G)
    .byte   %00011000 ; |   ##   |            $1a49 (G)
    .byte   %01111110 ; | ###### |            $1a4a (G)
    .byte   %00011000 ; |   ##   |            $1a4b (G)
    .byte   %00100100 ; |  #  #  |            $1a4c (G)
    .byte   %01000010 ; | #    # |            $1a4d (G)
    .byte   %00000000 ; |        |            $1a4e (G)
    .byte   %00000000 ; |        |            $1a4f (G)
DigitCol1
TwoGfx
    .byte   %00000000 ; |        |            $1a50 (G)
    .byte   %00011000 ; |   ##   |            $1a51 (G)
    .byte   %00100100 ; |  #  #  |            $1a52 (G)
    .byte   %00000100 ; |     #  |            $1a53 (G)
    .byte   %00000100 ; |     #  |            $1a54 (G)
    .byte   %00001000 ; |    #   |            $1a55 (G)
    .byte   %00010000 ; |   #    |            $1a56 (G)
    .byte   %00100000 ; |  #     |            $1a57 (G)
    .byte   %00100000 ; |  #     |            $1a58 (G)
    .byte   %00111100 ; |  ####  |            $1a59 (G)
    .byte   %00000000 ; |        |            $1a5a (G)
FiveGfx
    .byte   %00000000 ; |        |            $1a5b (G)
    .byte   %00111100 ; |  ####  |            $1a5c (G)
    .byte   %00100000 ; |  #     |            $1a5d (G)
    .byte   %00100000 ; |  #     |            $1a5e (G)
    .byte   %00111000 ; |  ###   |            $1a5f (G)
    .byte   %00100100 ; |  #  #  |            $1a60 (G)
    .byte   %00000100 ; |     #  |            $1a61 (G)
    .byte   %00000100 ; |     #  |            $1a62 (G)
    .byte   %00100100 ; |  #  #  |            $1a63 (G)
    .byte   %00011000 ; |   ##   |            $1a64 (G)
    .byte   %00000000 ; |        |            $1a65 (G)
EightGfx
    .byte   %00000000 ; |        |            $1a66 (G)
    .byte   %00011000 ; |   ##   |            $1a67 (G)
    .byte   %00100100 ; |  #  #  |            $1a68 (G)
    .byte   %00100100 ; |  #  #  |            $1a69 (G)
    .byte   %00100100 ; |  #  #  |            $1a6a (G)
    .byte   %00011000 ; |   ##   |            $1a6b (G)
    .byte   %00100100 ; |  #  #  |            $1a6c (G)
    .byte   %00100100 ; |  #  #  |            $1a6d (G)
    .byte   %00100100 ; |  #  #  |            $1a6e (G)
    .byte   %00011000 ; |   ##   |            $1a6f (G)
    .byte   %00000000 ; |        |            $1a70 (G)
ZeroGfx
    .byte   %00000000 ; |        |            $1a71 (G)
    .byte   %00011000 ; |   ##   |            $1a72 (G)
    .byte   %00100100 ; |  #  #  |            $1a73 (G)
    .byte   %00100100 ; |  #  #  |            $1a74 (G)
    .byte   %00100100 ; |  #  #  |            $1a75 (G)
    .byte   %00100100 ; |  #  #  |            $1a76 (G)
    .byte   %00100100 ; |  #  #  |            $1a77 (G)
    .byte   %00100100 ; |  #  #  |            $1a78 (G)
    .byte   %00100100 ; |  #  #  |            $1a79 (G)
    .byte   %00011000 ; |   ##   |            $1a7a (G)
    .byte   %00000000 ; |        |            $1a7b (G)
DigitCol2
ThreeGfx
    .byte   %00000000 ; |        |            $1a7c (G)
    .byte   %00011000 ; |   ##   |            $1a7d (G)
    .byte   %00100100 ; |  #  #  |            $1a7e (G)
    .byte   %00000100 ; |     #  |            $1a7f (G)
    .byte   %00000100 ; |     #  |            $1a80 (G)
    .byte   %00011000 ; |   ##   |            $1a81 (G)
    .byte   %00000100 ; |     #  |            $1a82 (G)
    .byte   %00000100 ; |     #  |            $1a83 (G)
    .byte   %00100100 ; |  #  #  |            $1a84 (G)
    .byte   %00011000 ; |   ##   |            $1a85 (G)
    .byte   %00000000 ; |        |            $1a86 (G)
SixGfx
    .byte   %00000000 ; |        |            $1a87 (G)
    .byte   %00011000 ; |   ##   |            $1a88 (G)
    .byte   %00100100 ; |  #  #  |            $1a89 (G)
    .byte   %00100000 ; |  #     |            $1a8a (G)
    .byte   %00100000 ; |  #     |            $1a8b (G)
    .byte   %00111000 ; |  ###   |            $1a8c (G)
    .byte   %00100100 ; |  #  #  |            $1a8d (G)
    .byte   %00100100 ; |  #  #  |            $1a8e (G)
    .byte   %00100100 ; |  #  #  |            $1a8f (G)
    .byte   %00011000 ; |   ##   |            $1a90 (G)
    .byte   %00000000 ; |        |            $1a91 (G)
NineGfx
    .byte   %00000000 ; |        |            $1a92 (G)
    .byte   %00011000 ; |   ##   |            $1a93 (G)
    .byte   %00100100 ; |  #  #  |            $1a94 (G)
    .byte   %00100100 ; |  #  #  |            $1a95 (G)
    .byte   %00100100 ; |  #  #  |            $1a96 (G)
    .byte   %00011100 ; |   ###  |            $1a97 (G)
    .byte   %00000100 ; |     #  |            $1a98 (G)
    .byte   %00000100 ; |     #  |            $1a99 (G)
    .byte   %00100100 ; |  #  #  |            $1a9a (G)
    .byte   %00011000 ; |   ##   |            $1a9b (G)
    .byte   %00000000 ; |        |            $1a9c (G)
HashGfx
    .byte   %00000000 ; |        |            $1a9d (G)
    .byte   %00000000 ; |        |            $1a9e (G)
    .byte   %00100100 ; |  #  #  |            $1a9f (G)
    .byte   %01111110 ; | ###### |            $1aa0 (G)
    .byte   %00100100 ; |  #  #  |            $1aa1 (G)
    .byte   %00100100 ; |  #  #  |            $1aa2 (G)
    .byte   %01111110 ; | ###### |            $1aa3 (G)
    .byte   %00100100 ; |  #  #  |            $1aa4 (G)
    .byte   %00100100 ; |  #  #  |            $1aa5 (G)
    .byte   %00000000 ; |        |            $1aa6 (G)
    .byte   %00000000 ; |        |            $1aa7 (G)
BlockGfx
    .byte   %00000000 ; |        |            $1aa8 (G)
    .byte   %11111111 ; |########|            $1aa9 (G)
    .byte   %11111111 ; |########|            $1aaa (G)
    .byte   %11111111 ; |########|            $1aab (G)
    .byte   %11111111 ; |########|            $1aac (G)
    .byte   %11111111 ; |########|            $1aad (G)
    .byte   %11111111 ; |########|            $1aae (G)
    .byte   %11111111 ; |########|            $1aaf (G)
    .byte   %11111111 ; |########|            $1ab0 (G)
    .byte   %11111111 ; |########|            $1ab1 (G)
    .byte   %00000000 ; |        |            $1ab2 (G)
DigitPtr
    .byte   ZeroGfx  - DigitGfx             ; $1ab3 (D)  
    .byte   OneGfx   - DigitGfx   
    .byte   TwoGfx   - DigitGfx   
    .byte   ThreeGfx - DigitGfx
    .byte   FourGfx  - DigitGfx  
    .byte   FiveGfx  - DigitGfx  
    .byte   SixGfx   - DigitGfx
    .byte   SevenGfx - DigitGfx 
    .byte   EightGfx - DigitGfx 
    .byte   NineGfx  - DigitGfx
; ??? TODO
    .byte   $d3,$cc,$ef,$f4,$16,$00         ; $1abd (D)
    .byte   $06,$d3,$e1,$d6,$c5,$04,$12,$00 ; $1ac3 (D)
    .byte   $12,$d4,$d2,$f5,$ee             ; $1acb (D)

;===============================================================================

    RORG    . - $800                ;           code for slice 0/3

L12d0 SUBROUTINE
    lda     #%01111111              ;2
    ldx     #$08                    ;2          noise
    ldy     #$ff                    ;2
    sty     AUDV0                   ;3
    ldy     #$01                    ;2
    sty     AUDC0                   ;3
.loop
    sta     PF1                     ;3
    sta     PF2                     ;3
    stx     AUDF0                   ;3
    jsr     Wait100ms               ;6
    cmp     #$00                    ;2
    beq     .exit                   ;2/3
    dex                             ;2
    lsr                             ;2
    jmp     .loop                   ;3   =  40

.exit
    sta     AUDV0                   ;3
    rts                             ;6   =   9

SendLoadHeader20 SUBROUTINE
    jsr     SendHeader              ;6
    lda     #$20                    ;2
    jsr     SendByte                ;6
    lda     SL2_R1                  ;4          switch RAM bank 1 into slice 2 for reading (L1800..)
    lda     L1802                   ;4
    bne     .isInitialized          ;2/3
    lda     #<L1308                 ;2
    ldx     #>L1308                 ;2
    jmp     .uninitialized          ;3   =  31

L1308
    brk                             ;7   =   7  number of extra bytes to send

.isInitialized
    lda     L180a                   ;4
    ldx     L180b                   ;4
.uninitialized
    sta     dataPtr2                ;3
    stx     dataPtr2+1              ;3
    ldy     #$00                    ;2
    lda     (dataPtr2),y            ;5
    clc                             ;2
    adc     #$05                    ;2
    jsr     SendByte                ;6
    lda     ram_D2                  ;3          = $00|$10
    jsr     SendByte                ;6
    lda     ram_D3                  ;3          = $00
    jsr     SendByte                ;6
    lda     dialSpeed               ;3
    asl                             ;2
    asl                             ;2
    asl                             ;2
    asl                             ;2
    sta     tmpVarDE                ;3
    lda     dialType                ;3
    and     #$0f                    ;2
    ora     tmpVarDE                ;3
    jsr     SendByte                ;6          
    jsr     SendCRC                 ;6
    jsr     SetDelay                ;6
    ldy     #$00                    ;2   =  91
.loopSendA
    lda     $1fda,y                 ;4          serial number from PROM? (..$1fdf)
    jsr     SendByte                ;6
    iny                             ;2
    cpy     #$05                    ;2
    bne     .loopSendA              ;2/3
    ldy     #$00                    ;2
    lda     (dataPtr2),y            ;5          number of bytes to send
    beq     .skipInitialized        ;2/3
    tax                             ;2   =  27
.loopSend
    iny                             ;2
    lda     (dataPtr2),y            ;5
    jsr     SendByte                ;6
    dex                             ;2
    bne     .loopSend               ;2/3 =  17
.skipInitialized
    jsr     SendCRC                 ;6
    jsr     EndSend                 ;6
    rts                             ;6   =  18

    .byte   $14,$2e,$1b,$76,$16,$16,$17,$c4 ; $1f62 (D)
    .byte   $16,$ff,$16,$c3,$15,$b5,$cd,$d3 ; $1f6a (D)
    .byte   $d3,$a0,$d3,$d4,$a0,$d2,$c8,$66 ; $1f72 (D)
    .byte   $16,$ef,$16,$79,$15,$32,$1d,$24 ; $1f7a (D)
    .byte   $5a,$30,$03,$20,$81,$14         ; $1f82 (D)

;===============================================================================

    RORG    . + $c00                ;           code for slice 3/3

CheckSum SUBROUTINE
    ldx     #$00                    ;2
    stx     crcHi                   ;3
    lda     GL_STOP_PULSE           ;4   =   9  
L1f8f
    lda     L1fc0,x                 ;4          ..$1fde, most likely NOT from ROM or RAM
    eor     crcHi                   ;3          
    sta     crcHi                   ;3
    asl                             ;2
    rol     crcHi                   ;5
    inx                             ;2
    cpx     #$1f                    ;2
    bne     L1f8f                   ;2/3
    lda     L1fc0,x                 ;4          $1fdf
    cmp     crcHi                   ;3
  IF ORIGINAL
    bne     .error                  ;2/3
  ELSE
    ds      2, $ea
  ENDIF
    lda     GL_STOP_C80             ;4          
    rts                             ;6   =  42

.error
    lda     #GREEN_BEIGE|$6         ;2
    sta     COLUBK                  ;3
    jmp     .error                  ;3   =   8

Check1000 SUBROUTINE
    lda     GL_START_CA0            ;4          switch something on (allow writing?)
    sty     L1000                   ;4
    lda     GL_INPUT                ;4
    and     #$03                    ;2
    ldy     GL_STOP_C80             ;4          ...and off again? (stop writing?)
    rts                             ;6   =  24

Start SUBROUTINE
    sei                             ;2   =   2
L1fc0
    cld                             ;2
    lda     #$00                    ;2
    tax                             ;2
    ldy     #$a8                    ;2   =   8
.loopS
    txs                             ;2          this code does NOT init anything!
    inx                             ;2
    bne     .loopS                  ;2/3
    dey                             ;2
    bne     .loopS                  ;2/3
    ldx     #$05                    ;2
    lda     SL0_WD                  ;4   =  16  switch RAM bank ? (PROM?) into slice 0 for writing
.loopCheck
    ldy     #$02                    ;2
    jsr     Check1000               ;6
    cmp     #$02                    ;2
  IF ORIGINAL
    bne     L1fe4                   ;2/3 =  12
  ELSE
    ds      2, $ea
  ENDIF
    ldy     #$01                    ;2
    jsr     Check1000               ;6
    cmp     #$01                    ;2
  IF ORIGINAL
    beq     .ok                     ;2/3 =  14
  ELSE
    bne     .ok                     ;2/3 =  14
  ENDIF
L1fe4
    lda     GL_680                  ;4          
    dex                             ;2
    bne     .loopCheck              ;2/3 =   8
.ok  
    lda     GL_680                  ;4          
    jsr     CheckSum                ;6          -> GL_STOP_C80
    lda     SL0_B0                  ;4          switch ROM bank 0 into slice 0
    lda     SL1_B1                  ;4          switch ROM bank 1 into slice 1
    lda     SL2_R0                  ;4          switch RAM bank 0 into slice 2 for reading
    jmp     L1000                   ;3   =   6

L1ffc
    .word   Start,Start                     ; $1ffc (D)
