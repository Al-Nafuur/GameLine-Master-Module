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


ORIGINAL    = 1


GL_481          = $481
GL_4AD          = $4ad      ; allows write to $1000?

GL_580          = $580      ; ,X; X = 1|2|4|variable
GL_582          = $582
GL_583          = $583

GL_680          = $680

GL_850          = $850    
GL_880          = $880      ; also ,X; X = 4  ($880 = map $1c00 into $1800?)
GL_884          = $884
GL_885          = $885      ; allow reading from $1800..
GL_8A4          = $8a4      ; writes to L1802
GL_8A5          = $8a5      ; writes to $1802, $180c, $180d, $180e..$1815

GL_980          = $980      ; ,X; X = 0|13|...

GL_STOP_C80     = $c80
GL_START_CA0    = $ca0      ; allows write to $1000 or read from $1ff8
GL_STOP_PULSE   = $cb0      ; could be...
GL_START_PULSE  = $cb8      ; ... vice versa

GL_SEND_TONE    = $d80      ; also ,Y; Y = $10..$1a


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
AUDC1           = $16  ; (W)
AUDF0           = $17  ; (W)
AUDF1           = $18  ; (W)
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
TIM8T           = $0295
TIM64T          = $0296
T1024T          = $0297


;-----------------------------------------------------------
;      RIOT RAM (zero-page) labels
;-----------------------------------------------------------

ram_80          = $80       ; always 1
ram_81          = $81       ; X for GL_580     ,x (X = 2)
ram_82          = $82       ; X for GL_880,x (X = 4)
ram_83          = $83       ; X for GL_980,x (X = 13|0)
ram_84          = $84
ram_85          = $85
ram_86          = $86
ram_87          = $87
ram_88          = $88

ram_8B          = $8b
ram_8C          = $8c
color           = $8d
ram_8E          = $8e
ptrLst          = $8f
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
ram_9B          = $9b
ram_9C          = $9c
yDial           = $9d
xDial           = $9e
numDigits       = $9f
ram_A0          = $a0
lastFire        = $a1
lastSwitches    = $a2
lastJoyDir      = $a3
inputDelay      = $a4
ram_A5          = $a5
ram_A6          = $a6
digitLst        = $a7   ; ..$a9
digit0          = digitLst        
digit1          = digitLst+1
digit2          = digitLst+2

dataPtr         = $ab   ;..$ac
dataPtr2        = $ad   ;..$ae
ram_AF          = $af
ram_B0          = $b0
ram_B1          = $b1
ram_B2          = $b2
ram_B3          = $b3
ram_B4          = $b4
ram_B5          = $b5
ram_B6          = $b6
ram_B7          = $b7
ram_B8          = $b8

pulseCount      = $ba
jmpIdx          = $bb
ram_BC          = $bc
numberIdx       = $bd
ram_BE          = $be
dialType        = $bf
counter         = $c0   ;..$c1
numberPtr       = $c2   ;..$c3
speed           = $c4

ram_C6          = $c6

ram_C9          = $c9
ram_CA          = $ca

ram_D0          = $d0
ram_D1          = $d1
ram_D2          = $d2
ram_D3          = $d3
ram_D4          = $d4

ram_D8          = $d8

ram_DA          = $da

ram_DE          = $de
ram_DF          = $df
ram_E0          = $e0

ram_E6          = $e6

ram_EB          = $eb

ram_F5          = $f5

ram_FD          = $fd
;                 $fe  (s)
;                 $ff  (s)


;-----------------------------------------------------------
;      Non Locatable Labels
;-----------------------------------------------------------

;L12dc           = $12dc
;L14b9           = $14b9
;L14d5           = $14d5
;L14e0           = $14e0
;L1533           = $1533
;L17f1           = $17f1 ???
;L19a7           = $19a7
;L19f5           = $19f5
;L1cbc           = $1cbc ???


;***********************************************************
;      Bank 0
;***********************************************************

    SEG     CODE
    ORG     $1000

L1000
    jmp     L1009                   ;3   =   3

    .word   L1106, L139a, L15d8     ; $1003 (D)

L1009
    lda     #$00                    ;2
    tax                             ;2   =   4
L100c
    sta     $00,x                   ;4
    txs                             ;2
    inx                             ;2
    bne     L100c                   ;2/3
    ldx     #$01                    ;2
    stx     ram_80                  ;3
    inx                             ;2
    stx     ram_81                  ;3
    ldx     #$04                    ;2
    stx     ram_82                  ;3
    ldx     #$0d                    ;2
    stx     ram_83                  ;3      pages? 1, 2, 4, d
    lda     GL_980                  ;4      page 0 into $900?
    lda     GL_884                  ;4      allow reading from $1800.. (page 4?)
    jsr     Check1800               ;6
    bne     Failed                  ;2/3
    lda     GL_885                  ;4      allow reading from $1800.. (page 5?)
    jsr     Check1800               ;6
    beq     .success                ;2/3
Failed
    lda     GL_8A4                  ;4      page 4 into $800 for writing
    ldx     #$00                    ;2
    stx     L1802                   ;4
    lda     GL_8A5                  ;4      page 5 into $800 for writing
    stx     L1802                   ;4
    lda     #$00                    ;2
    sta     ram_83                  ;3
    lda     #$ff                    ;2
    sta     ram_B6                  ;3
    lda     #<L1c00                 ;2
    ldx     #>L1c00                 ;2
    jmp     L1106                   ;3   =  35

.success
    lda     GL_884                  ;4      page 4 into $800
    lda     L1808                   ;4
    ldx     L1809                   ;4
    jmp     L1106                   ;3   =  15

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
    ldy     ram_8C                  ;3
    stx     GRP1                    ;3
    sta     GRP0                    ;3
    sty     GRP1                    ;3
    sta     GRP0                    ;3
    inc     ram_86                  ;5   =  51
Draw48Pixel
    ldy     ram_86                  ;3
    lda     (ram_99),y              ;5
    sta     ram_8C                  ;3
    lda     (ram_8F),y              ;5
    sta     WSYNC                   ;3   =  19
;---------------------------------------
    cpy     ram_87                  ;3
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
L10b9 ;L14b9?
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
    lda     GL_583                  ;4
    lda     #<L1400                 ;2
    ldx     #>L1400                 ;2
    jmp     L10dc                   ;3   =  11

L10d3 SUBROUTINE
    lda     GL_885                  ;4
;L14d5?
    lda     L1808                   ;4
    ldx     L1809                   ;4   =  12
L10dc
    sta     dataPtr2                ;3
    stx     dataPtr2+1              ;3
;L14e0?
    jsr     L10f7                   ;6   =  12
SetupBanks
    stx     ram_DE                  ;3
    ldx     ram_81                  ;3          2?
    lda     GL_580,x                ;4
    ldx     ram_82                  ;3          4?
    lda     GL_880,x                ;4
    ldx     ram_83                  ;3          13|0?
    lda     GL_980,x                ;4
    ldx     ram_DE                  ;3
    rts                             ;6   =  33

L10f7 SUBROUTINE
    jmp.ind (dataPtr2)              ;5   =   5

L10fa SUBROUTINE
    lda     GL_884                  ;4
    lda     L1806                   ;4
    ldx     L1807                   ;4
    jmp     L10dc                   ;3   =  15

L1106
    sta     dataPtr                 ;3
    stx     dataPtr+1               ;3
    jsr     SetupBanks              ;6
    lda     #$00                    ;2
    sta     ram_AF                  ;3
    sta     ram_8E                  ;3
    sta     ram_D2                  ;3
    lda     #$28                    ;2
    sta     color                   ;3
    lda     #$01                    ;2
    sta     CTRLPF                  ;3
    lda     #$0a                    ;2
    sta     inputDelay              ;3
    sta     ram_9B                  ;3   =  41
L1123
    jsr     L12bf                   ;6
    ldy     #$08                    ;2
    lda     (dataPtr),y             ;5              $1c08 =$64, ($1810)
    sta     TIM64T                  ;4
    lda     ram_AF                  ;3
    beq     L1134                   ;2/3
    jmp     L11b6                   ;3   =  25

L1134
    lda     #$12                    ;2
    sta     T1024T                  ;4
    ldx     #$ff                    ;2
    stx     ram_9C                  ;3
    inx                             ;2
    stx     numDigits               ;3
    inx                             ;2
    stx     yDial                   ;3
    stx     xDial                   ;3
    ldy     #$00                    ;2
    lda     (dataPtr),y             ;5
    sta     ram_AF                  ;3
    jsr     XPosSprites             ;6
    ldy     #$0a                    ;2
    lda     (dataPtr),y             ;5
    sta     ram_B0                  ;3
    ldy     #$02                    ;2
    lda     (dataPtr),y             ;5
    beq     L1178                   ;2/3
    sta     ram_C6                  ;3
    iny                             ;2
    lda     (dataPtr),y             ;5
    sta     jmpIdx                  ;3
    iny                             ;2
    lda     (dataPtr),y             ;5
    sta     counter                 ;3
    iny                             ;2
    lda     (dataPtr),y             ;5
    jsr     L131a                   ;6
    sta     counter+1               ;3
    lda     GL_580,x                ;4      X = (dataPtr),y/ 16
    lda     #$06                    ;2
    sta     pulseCount              ;3
    jsr     L10d3                   ;6   = 113
L1178
    ldy     #$0e                    ;2
    lda     (dataPtr),y             ;5
    tax                             ;2
    iny                             ;2
    lda     (dataPtr),y             ;5
    beq     L118f                   ;2/3
    stx     ram_B4                  ;3
    jsr     L131a                   ;6
    sta     ram_B5                  ;3
    stx     ram_B6                  ;3
    lda     #$01                    ;2
    sta     ram_B7                  ;3   =  38
L118f
    ldy     #$10                    ;2
    lda     (dataPtr),y             ;5
    tax                             ;2
    iny                             ;2
    lda     (dataPtr),y             ;5
    beq     L11a0                   ;2/3
    sta     dataPtr2+1              ;3
    stx     dataPtr2                ;3
    jsr     L11b3                   ;6   =  30
L11a0
    ldy     #$01                    ;2
    lda     (dataPtr),y             ;5
    sta     ram_D2                  ;3
    sta     ram_B1                  ;3
    jsr     WaitTim                 ;6
    lda     #$02                    ;2
    sta     TIM64T                  ;4
    jmp     L12a6                   ;3   =  28

L11b3 SUBROUTINE
    jmp.ind (dataPtr2)              ;5   =   5

L11b6
    ldy     #$09                    ;2
    lda     (dataPtr),y             ;5
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
    ldy     #$0b                    ;2
    lda     (dataPtr),y             ;5
    beq     L11d9                   ;2/3
    tax                             ;2
    jsr     WaitLines               ;6   =  22
L11d9
    asl                             ;2
    sta     ram_8B                  ;3
    ldy     #$14                    ;2
    sty     ram_84                  ;3   =  10
L11e0
    jsr     L1326                   ;6
    lda     ram_85                  ;3
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
L11f5 ;L19f5?
    lda     ram_87                  ;3
    clc                             ;2
    adc     ram_8B                  ;3
    adc     #$02                    ;2
    cmp     ram_B0                  ;3
    bcs     L1271                   ;2/3
    sta     ram_8B                  ;3
    lda     ram_87                  ;3
    clc                             ;2
    adc     ram_86                  ;3
    sta     ram_87                  ;3
    jsr     Draw48Pixel             ;6
    lda     #$00                    ;2
    sta     PF1                     ;3   =  40
L1210
    lda     ram_88                  ;3
    beq     L1227                   ;2/3
    tax                             ;2
    clc                             ;2
    adc     ram_8B                  ;3
    cmp     ram_B0                  ;3
    bcs     L1271                   ;2/3
    sta     ram_8B                  ;3
    jsr     WaitLines               ;6
    jsr     L10b3                   ;6
    jsr     SetupBanks              ;6   =  38
L1227
    jmp     L11e0                   ;3   =   3

L122a
    asl                             ;2
    bmi     L123c                   ;2/3
    lda     GL_880                  ;4          map $1c00 into $1800?
    jsr     DrawDialPad             ;6
    jsr     SetupBanks              ;6
    jsr     XPosSprites             ;6
    jmp     L1210                   ;3   =  29

L123c
    asl                             ;2
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
    jsr     L131a                   ;6
    sta     ram_9A                  ;3
    lda     GL_980,x                ;4      X = (dataPtr2),y / 16
    jmp     L11f5                   ;3   =  32

L1261
    asl                             ;2
    bmi     L126d                   ;2/3
    jsr     L126a                   ;6
    jmp     L1210                   ;3   =  13

L126a
    jmp.ind (ram_86)                ;5   =   5

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
    lda     ram_D4                  ;3
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
    lda     #$5a                    ;2
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
L12dc = . - 1
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
    ldx     ram_B6                  ;3
    bmi     L1304                   ;2/3!
    lda     GL_980,x                ;4
    jsr     L10fa                   ;6   =  21
L1304
    jsr     WaitTim                 ;6
    sta     VBLANK                  ;3
    rts                             ;6   =  15

WaitTim SUBROUTINE
    lda     TIM8T                   ;4
    bpl     WaitTim                 ;2/3 =   6
L130f
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

L131a SUBROUTINE
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

L1326 SUBROUTINE
    ldx     #$00                    ;2
    ldy     ram_84                  ;3
    lda     (dataPtr),y             ;5
    bmi     .loop                   ;2/3
    beq     .loop                   ;2/3
    lda     ram_B3                  ;3
    bne     .loop                   ;2/3
    ldy     ram_B1                  ;3
    sty     ram_84                  ;3
    sty     ram_B3                  ;3   =  28
.loop
    lda     (dataPtr),y             ;5
    sta     ram_85,x                ;4
    iny                             ;2
    inx                             ;2
    cpx     #$06                    ;2
    bne     .loop                   ;2/3
    lda     ram_84                  ;3
    sty     ram_84                  ;3
    sty     ram_B2                  ;3
    tay                             ;2
    rts                             ;6   =  34

Dial SUBROUTINE
    ldy     ram_BC                  ;3
    bne     L135a                   ;2/3
    dey                             ;2
    sty     dialType                ;3          = $ff
    sty     speed                   ;3
    inc     ram_BC                  ;5
    jmp     L137a                   ;3   =  21

L135a
    iny                             ;2
    sty     ram_BC                  ;3
    cpy     #$0a                    ;2
    beq     L1393                   ;2/3
    ldy     #$00                    ;2
    sty     ram_BE                  ;3
    inc     speed                   ;5
    lda     speed                   ;3
    cmp     #$02                    ;2
    bne     L137a                   ;2/3
    dey                             ;2
    sty     speed                   ;3          Y = $ff
    inc     dialType                ;5
    lda     dialType                ;3
    cmp     #$01                    ;2
    bne     L137a                   ;2/3
    sty     dialType                ;3   =  46  Y = $ff
L137a
    lda     GL_885                  ;4
    lda     L1802                   ;4
    bne     L1392                   ;2/3
    lda     GL_8A5                  ;4
    lda     dialType                ;3          backup BF and speed
    sta     L180c                   ;4
    lda     speed                   ;3
    sta     L180d                   ;4
    lda     GL_885                  ;4   =  32
L1392
    rts                             ;6   =   6

L1393
    lda     #$00                    ;2
    sta     ram_BC                  ;3
    jmp     Failed                  ;3   =   8 *

L139a
    lda     #$00                    ;2
    sta     ram_BC                  ;3
    sta     ram_BE                  ;3
    jsr     L146d                   ;6   =  14
L13a3
    ldx     #$00                    ;2
    stx     ram_8E                  ;3
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
    sta     ram_86                  ;3
    lda     #$07                    ;2
    sta     ram_87                  ;3
    jsr     Draw48Pixel             ;6
    jsr     L1435                   ;6          check bits?
    ldx     #$0c                    ;2
    jsr     WaitLines               ;6
    inc     ram_8E                  ;5
    jmp     L13ca                   ;3   =  65

;###############################################################################

L1400 SUBROUTINE
    lda     ram_8E                  ;3
    and     #$e0                    ;2
    clc                             ;2
    adc     #$10                    ;2
    sta     COLUP0                  ;3
    sta     COLUP1                  ;3
    clc                             ;2
    adc     #$02                    ;2
    sta     color                   ;3   =  22
L1410
    jsr     L141d                   ;6
    clc                             ;2
    adc     #$02                    ;2
    sta     color                   ;3
    and     #$0f                    ;2
    bne     L1410                   ;2/3
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

L146d SUBROUTINE
    lda     GL_885                  ;4
    lda     L180c                   ;4          restore BF and speed
    sta     dialType                ;3
    lda     L180d                   ;4
    sta     speed                   ;3
    rts                             ;6   =  24

CheckBit6 SUBROUTINE
    lda     ram_DF                  ;3
    bne     .exit                   ;2/3
    lda     jmpIdx                  ;3
    bmi     L14af                   ;2/3        start new sequence
    dec     counter                 ;5
    bne     L148d                   ;2/3
    lda     counter+1               ;3
    beq     L149e                   ;2/3        done waiting
    dec     counter+1               ;5   =  27
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
    sta     counter                 ;3
    lda     #$06                    ;2
    sta     counter+1               ;3
L14b9 = . - 1
    lda     #0                      ;2
    sta     jmpIdx                  ;3
    lda     #$00                    ;2
    sta     numberIdx               ;3   =   8
.loop
    ldx     ram_BE                  ;3
    lda     L1812,x                 ;4          $15f6(|$ffff?)
    sta     numberPtr               ;3
    lda     L1813,x                 ;4
    sta     numberPtr+1             ;3
    bpl     .skip                   ;2/3
    jsr     Dial                    ;6
    jmp     .loop
L14d5 = . - 1

.skip
    inc     ram_BE                  ;5
    inc     ram_BE                  ;5
    rts                             ;6   =  16  return from CheckBit6

DialNumber SUBROUTINE                ;          also from CheckBit6 (jmpIdx = 0)
    ldy     numberIdx               ;3
    lda     (numberPtr),y           ;5          dial a number
    cmp     #$0f                    ;2          last digit done?
L14e0 = . - 1
    bne     L14ed                   ;2/3         no
    ldy     #$04                    ;2          -> jmpIdx
    ldx     L180e                   ;4          -> counter+1 (=99)
    lda     #$00                    ;2          -> counter
    jmp     SetCounterHi            ;3   =  17

L14ed
    cmp     #$0e                    ;2          ???
    bne     L14fc                   ;2/3
    lda     #$48                    ;2          = 840 (= 315 * 2.66; = 1680 / 2)
    sta     counter                 ;3
    lda     #$03                    ;2
    sta     counter+1               ;3
    inc     numberIdx               ;5
    rts                             ;6   =  25  return from CheckBit6

L14fc
    cmp     #$0d                    ;2          start of phone number?
    bne     L1510                   ;2/3         no
    lda     dialType                ;3
    bmi     .dialTypeFF             ;2/3
    lda     #$01                    ;2          start dialing
    sta     ram_DE                  ;3
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
    lda     ram_DE                  ;3
    inc     ram_DE                  ;5
    jmp     L152c                   ;3   =  22

L1521
    cmp     #$0b                    ;2          ???
    bne     L152c                   ;2/3
    inc     ram_DF                  ;5          = 1 (error?)    
    inc     numberIdx               ;5          -> $0d
    inc     counter                 ;5
    rts                             ;6   =  25  return from CheckBit6, DialNumber

L152c
    ldy     speed                   ;3
    bmi     .toneDial               ;2/3        A = ram_DE
    cmp     #$00                    ;2
    bne     .notZero                ;2/3
L1533 = . - 1
    lda     #10                     ;2   =   2  replace 0 with 10
.notZero
    sta     pulseCount              ;3   =   3
NextPulse
    lda     GL_STOP_PULSE           ;4
    lda     #$0c                    ;2          make some noise
    sta     AUDC0                   ;3
    lda     #$1f                    ;2
    sta     AUDF1                   ;3
    lda     #$0f                    ;2
    sta     AUDV0                   ;3
    lda     L1612,y                 ;4          -> counter = 26|52 (60%)
    ldy     #$01                    ;2          -> jmpIdx
    jmp     SetCounter              ;3   =  28

; Ein einzelner Impuls dauert 100 ms. 
; Das in Deutschland verwendete Impuls-Pause-Verhältnis ist 60/40, 
; was bedeutet, dass die Schleife eine Impulsdauer von 100 ms hat, 
; 60 ms geöffnet und 40 ms geschlossen ist. 
; Nach Ablauf der Impulsfolge bleibt die Schleife geschlossen.
; Die Pause zwischen den gewählten Ziffern beträgt mindestens die Zeit zweier Impulse, also 200 ms

; The specifications of the Bell System in the US required service personnel 
; to adjust dials in customer stations to a precision of 9.5 to 10.5 pulses 
; per second (PPS), but the tolerance of the switching equipment was generally 
; between 8 and 11 PPS.[2] The British (GPO, later Post Office Telecommunications) 
; standard for Strowger switch exchanges has been ten impulses per second 
; (allowable range 7 to 12) and a 66% break ratio (allowable range 63% to 72%).

.toneDial
    tay                             ;2          
    asl                             ;2
    adc     #$0a                    ;2          make some noise
    sta     AUDF0                   ;3
    lda     #$04                    ;2
    sta     AUDC0                   ;3
    lda     #$0f                    ;2
    sta     AUDV0                   ;3
    lda     L161a,y                 ;4
    tay                             ;2          Y = $10..$1a (unordered!), tone dialing
    lda     GL_SEND_TONE,y          ;4
    lda     #$34                    ;2          -> counter = 52
    ldy     #$03                    ;2          -> jmpIdx
    jmp     SetCounter              ;3   =  36

L156b SUBROUTINE                    ;           called from CheckBit6 (jmpIdx = 1)
    lda     GL_START_PULSE          ;4
    lda     #$00                    ;2
    sta     AUDV0                   ;3
    ldy     speed                   ;3
    lda     L1614,y                 ;4          -> counter = 18|36 (40%)
    ldy     #$02                    ;2          -> jmpIdx
    jmp     SetCounter              ;3   =  21

L157c SUBROUTINE                    ;           called from CheckBit6 (jmpIdx = 2)
    ldy     speed                   ;3
    dec     pulseCount              ;5
    bne     NextPulse               ;2/3
    ldy     speed                   ;3
    lda     L1616,y                 ;4          -> counter = 315|630
    ldx     L1618,y                 ;4
    ldy     #$00                    ;2          -> jmpIdx
    inc     numberIdx               ;5
    jmp     SetCounterHi            ;3   =  31

L1591 SUBROUTINE                    ;           called from CheckBit6 (jmpIdx = 3)
    lda     GL_SEND_TONE            ;4
    lda     #$00                    ;2          stop sound
    sta     AUDV0                   ;3
    lda     #$34                    ;2          -> counter (52)
    ldy     #$00                    ;2          -> jmpIdx
    inc     numberIdx               ;5
    jmp     SetCounter              ;3   =  21

L15a1 SUBROUTINE                    ;           also called from CheckBit6 (jmpIdx = 4|5)
    lda     GL_STOP_PULSE           ;4
    lda     #$00                    ;2
    sta     ram_DF                  ;3
    ldy     #$06                    ;2          -> jmpIdx
    lda     #$90                    ;2          1680 (= 315 * 5.33)
    ldx     #$06                    ;2   =  15
SetCounterHi 
    stx     counter+1               ;3   =   3
SetCounter 
    sta     counter                 ;3
    sty     jmpIdx                  ;3   =   6
.exit
    rts                             ;6   =   6

WaitBit6Set
    bit     L1ff8                   ;4
    bvs     L15bb                   ;2/3
    rts                             ;6   =  12  return from CheckBit6

L15bb
    lda     #$05                    ;2
    sta     jmpIdx                  ;3
    lda     #$c0                    ;2
    sta     numberIdx               ;3
    rts                             ;6   =  16  return from CheckBit6

WaitBit6Clear
    bit     L1ff8                   ;4
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
    jsr     L10c9                   ;6
    jsr     L146d                   ;6
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
;L19f5?
    rts                             ;6   =  11  return from CheckBit6

L15f6
    .byte   $0d,$08,$00,$00,$03,$06,$08,$01 ; $15f6 (D) 8003681242
    .byte   $02,$04,$02,$0f,$00,$00         ; $15fe (D)
JmpTbl
    .word   DialNumber, L156b, L157c, L1591 ; $1604 (D)
    .word   L15a1, L15a1, L15f1             ; $160c (D)
L1612
    .byte   $1a,$34                         ; $1612 (D) 26, 52
L1614
    .byte   $12,$24                         ; $1614 (D) 18, 36

L1616
    .byte   $3b,$76                         ; $1616 (D) 315, 630
L1618
    .byte   $01,$02                         ; $1618 (D)

L161a ; pulse or tone dialing?
    .byte   $17,$10,$14
    .byte   $18,$11,$15
    .byte   $19,$12,$16
    .byte   $1a                             ; $1622 (D)

HandleInput SUBROUTINE
    dec     ram_A5                  ;5
    bne     L1633                   ;2/3
    lda     ram_A6                  ;3
    bne     L1631                   ;2/3
    ldx     #$0e                    ;2
    jmp     L16eb                   ;3   =  17

L1631
    dec     ram_A6                  ;5   =   5
L1633
    lda     inputDelay              ;3
    beq     .contInput              ;2/3
    dec     inputDelay              ;5   =  10
.exitFire
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
    bcs     .exitFire               ;2/3         no
    jsr     SetDelay                ;6
    jsr     L17e3                   ;6
    jsr     GetData                 ;6
    bpl     L165e                   ;2/3
    bvc     L16a9                   ;2/3
    ldx     #$00                    ;2
    jmp     L16eb                   ;3   =  34

L165e
    sta     ram_DE                  ;3
    jsr     GetPointer              ;6
    lsr     ram_DE                  ;5
    bcc     L1672                   ;2/3
    bne     L167d                   ;2/3 =  18
L1669
    sta     dataPtr+1               ;3
    stx     dataPtr                 ;3
    lda     #$00                    ;2
    sta     ram_AF                  ;3
    rts                             ;6   =  17

L1672
    lsr     ram_DE                  ;5
    bne     L1688                   ;2/3
    sta     dataPtr2+1              ;3
    stx     dataPtr2                ;3   =  13
L167a
    jmp.ind (dataPtr2)              ;5   =   5

L167d
    sta     ram_D3                  ;3
    stx     ram_D2                  ;3
    lda     #$00                    ;2
    sta     ram_D4                  ;3
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

L16a9
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
    tya                             ;2
    ldy     ram_D2                  ;3
    jmp     L165e                   ;3   =  32

.isDigit
    ldx     numDigits               ;3
    cpx     #$03                    ;2
    beq     L16cf                   ;2/3
    sta     digitLst,x              ;4
    inc     numDigits               ;5
    rts                             ;6   =  22

.isAsterisk
    lda     #$00                    ;2
    sta     numDigits               ;3   =   5
.exit
    rts                             ;6   =   6

L16cf
    jmp     L17f0                   ;3   =   3

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
    bne     L16eb                   ;3 =  31 *

.select
    ldx     #$04                    ;2   =   2
L16eb
    ldy     #$0c                    ;2
    lda     (dataPtr),y             ;5
    sta     dataPtr2                ;3
    iny                             ;2
    lda     (dataPtr),y             ;5
    beq     .exitDir                ;2/3!
    sta     dataPtr2+1              ;3
    jmp.ind (dataPtr2)              ;5   =  27

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
    jmp     L16eb                   ;3   =  15

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
    jmp     L16eb                   ;3   =  15

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
    jmp     L16eb                   ;3   =   5

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
    jmp     L16eb                   ;3   =   5

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
    sta     ram_DE                  ;3
    and     #$0f                    ;2
    bit     ram_DE                  ;3
    rts                             ;6   =  22

SetDelay SUBROUTINE
    pha                             ;3
    lda     #$05                    ;2
    sta     inputDelay              ;3
    pla                             ;4
    rts                             ;6   =  18

L17e3 SUBROUTINE
    lda     #<L17f7                 ;2
    ldx     #>L17f7                 ;2   =   4
L17e7
    sta     ram_B4                  ;3
    stx     ram_B5                  ;3
    lda     #$01                    ;2
    sta     ram_B7                  ;3
    rts                             ;6   =  17

L17f0
    lda     #<L17fb                 ;2
L17f1 = . - 1
    ldx     #>L17fb                 ;2
    jmp     L17e7                   ;3   =   7

L17f7
    .byte   $86,$c1,$3f,$88                 ; $17f7 (D)
L17fb
    .byte   $86,$c6,$9f,$fe                 ; $17fb (D)
    .byte   $88                             ; $17ff (D)

;###############################################################################

L1800
    .byte   $cc                             ; $1800 (D)
L1801
    .byte   $21                             ; $1801 (D)
L1802
    .byte   $00,$cd,$3c,$77                 ; $1802 (D)
L1806
    .byte   $9f                             ; $1806 (D)
L1807
    .byte   $8f                             ; $1807 (D)
L1808
    .byte   $a0                             ; $1808 (D)
L1809
    .byte   $00                             ; $1809 (D)
L180a
    .byte   $4b                             ; $180a (D)
L180b
    .byte   $25                             ; $180b (D)
L180c
    .byte   $ed                             ; $180c (D)
L180d
    .byte   $77                             ; $180d (D)
L180e
    .byte   $db,$c7,$98,$c2                 ; $180e (D)
L1812
    .byte   $a6                             ; $1812 (D)
L1813
    .byte   $a7,$ae,$69,$81,$4b,$2d,$42,$c2 ; $1813 (D)
    .byte   $0c,$33,$f6,$3c,$9f,$e5,$9c,$3d ; $181b (D)
    .byte   $05,$20,$0b,$5f,$d6,$01,$01,$82 ; $1823 (D)
    .byte   $58,$6b,$fb,$93,$7b,$73,$3b,$f9 ; $182b (D)
    .byte   $5e,$9b,$60,$64,$0e,$01,$02,$02 ; $1833 (D)
    .byte   $11,$f8,$fb,$79,$fb,$15,$f3,$0c ; $183b (D)
    .byte   $47,$fc,$50,$71,$9c,$ea,$80,$08 ; $1843 (D)
    .byte   $50,$8d,$ba,$82,$7f,$b0,$10,$86 ; $184b (D)
    .byte   $de,$1f,$df,$49,$ae,$80,$a2,$30 ; $1853 (D)
    .byte   $29,$7f,$a3,$fe,$1b,$06,$ce,$e2 ; $185b (D)
    .byte   $c8,$17,$a8,$c0,$11,$05,$60,$76 ; $1863 (D)
    .byte   $09,$fe,$0f,$ff,$9f,$6b,$7d,$35 ; $186b (D)
    .byte   $3a,$4e,$95,$c1,$61,$00,$00,$0a ; $1873 (D)
    .byte   $57,$81,$d7,$ee,$44,$fa,$b2,$94 ; $187b (D)
    .byte   $36,$8c,$f6,$df,$84,$a0,$15,$81 ; $1883 (D)
    .byte   $0f,$d0,$bf,$35,$b7,$36,$80,$ae ; $188b (D)
    .byte   $95,$9e,$8d,$8f,$51,$0a,$40,$ac ; $1893 (D)
    .byte   $00,$4e,$e7,$3c,$fe,$dc,$f5,$29 ; $189b (D)
    .byte   $db,$35,$52,$f5,$c3,$c1,$44,$91 ; $18a3 (D)
    .byte   $72,$fd,$9d,$f7,$9d,$80,$c2,$6f ; $18ab (D)
    .byte   $78,$0b,$c4,$32,$ab,$00,$20,$82 ; $18b3 (D)
    .byte   $00,$d5,$dc,$dc,$e8,$30,$51,$50 ; $18bb (D)
    .byte   $dc,$41,$e1,$6b,$ab,$56,$31,$8f ; $18c3 (D)
    .byte   $00,$ff,$f3,$fe,$ee,$cf,$65,$95 ; $18cb (D)
    .byte   $f7,$59,$43,$5f,$92,$32,$0a,$99 ; $18d3 (D)
    .byte   $41,$ff,$c5,$bf,$cc             ; $18db (D)
    .byte   $ca,$ca,$56,$9a,$1d,$91,$4b,$10 ; $18e0 (D)
    .byte   $02,$20,$4c,$18,$ab,$af,$4c,$9c ; $18e8 (D)
    .byte   $72,$a4,$0f,$a4,$b0,$7b,$ce,$47 ; $18f0 (D)
    .byte   $03,$31,$63,$35,$ff,$cd,$78,$db ; $18f8 (D)
    .byte   $f1,$06,$18,$79,$e2,$c5,$c3,$95 ; $1900 (D)
    .byte   $c4,$61,$00,$45,$df,$8a,$bd,$f1 ; $1908 (D)
    .byte   $7a,$4a,$95,$ba                 ; $1910 (D)
    .byte   $77,$a3,$73,$b5,$92,$00,$00,$06 ; $1914 (D)
    .byte   $ec,$64,$fd,$8d,$b9,$4f,$94,$84 ; $191c (D)
    .byte   $93,$5f,$73,$ab,$61,$b1,$56     ; $1924 (D)
    .byte   $0b,$fd,$38,$bd,$e9,$fa,$44,$7f ; $192b (D)
    .byte   $5f,$7a,$dc,$3d,$f4,$f0,$05,$56 ; $1933 (D)
    .byte   $43,$ad,$dc,$7f,$e6,$97,$37,$42 ; $193b (D)
    .byte   $63,$11,$d2,$b8,$9a,$46,$11,$62 ; $1943 (D)
    .byte   $03,$c1,$76,$da,$f8,$9c,$be,$4e ; $194b (D)
    .byte   $1c,$e0,$a4,$70,$7b,$82,$07,$04 ; $1953 (D)
    .byte   $23,$b1,$fd,$9f,$75,$26,$13,$92 ; $195b (D)
    .byte   $38,$bb,$e4,$18,$a3,$b6,$43,$01 ; $1963 (D)
    .byte   $25,$f5,$bf,$ba,$cb,$e0,$70,$04 ; $196b (D)
    .byte   $8e,$d7,$71,$8f,$ca,$e0,$00,$41 ; $1973 (D)
    .byte   $06,$ff,$e9,$6f,$92,$ec,$79,$65 ; $197b (D)
    .byte   $d1,$eb,$02,$02,$b7,$01,$00,$00 ; $1983 (D)
    .byte   $02,$bf,$bd,$91,$cf,$93,$1d,$53 ; $198b (D)
    .byte   $60,$89,$98,$32,$55,$41,$4a,$2a ; $1993 (D)
    .byte   $2b,$f2,$db,$df,$bf,$b7,$8e,$5b ; $199b (D)
    .byte   $33,$89,$b2,$f4                 ; $19a3 (D)
L19a7
    .byte   $1e,$91,$c3,$22,$7c,$a8,$e7,$9f ; $19a7 (D)
    .byte   $4f,$e7,$c2,$e0                 ; $19af (D)
L19b3
    .byte   $3f,$07,$0e,$b7,$17,$44,$32,$20 ; $19b3 (D)
    .byte   $04,$ce,$02,$fa,$97,$16,$7a,$e4 ; $19bb (D)
    .byte   $85,$bc,$1e,$01,$98,$22,$0e,$5f ; $19c3 (D)
    .byte   $67,$df,$ed,$90                 ; $19cb (D)
L19cf
    .byte   $ef,$a6,$b2,$69,$63,$d8,$2e,$90 ; $19cf (D)
    .byte   $9b,$d0,$a2,$83,$14,$ad,$ff,$fb ; $19d7 (D)
L19df
    .byte   $fa,$e9,$e8,$fb,$49,$45,$e1,$09 ; $19df (D)
    .byte   $ea,$50,$1e,$03,$e6,$ff,$e5,$fb ; $19e7 (D)
    .byte   $f6,$88,$3d,$ca,$b6,$6d         ; $19ef (D)
L19f5
    .byte   $fd,$b2,$a0,$18,$40,$c0,$2c,$eb ; $19f5 (D)
    .byte   $fc,$b0,$bf,$01,$51,$07,$a2,$85 ; $19fd (D)
    .byte   $fd,$fc,$ba,$b0,$10,$14,$c3,$c0 ; $1a05 (D)
    .byte   $fd,$2f,$97,$38,$18,$0a,$13,$5d ; $1a0d (D)
    .byte   $9e,$af,$d3,$39,$36,$39,$09,$d2 ; $1a15 (D)
    .byte   $3d,$ef,$d8                     ; $1a1d (D)
L1a20
    .byte   $aa,$27,$20,$81,$1f,$fa,$bc,$f4 ; $1a20 (D)
    .byte   $fc,$00,$84,$b7,$f8,$0d,$f6,$5f ; $1a28 (D)
    .byte   $00,$40,$50,$2c,$f3,$fd,$fb,$5d ; $1a30 (D)
    .byte   $8a,$6e,$49,$5f,$99,$fb,$95,$0a ; $1a38 (D)
    .byte   $0e,$c0,$05,$04,$ef,$fb,$ed,$b8 ; $1a40 (D)
    .byte   $5c,$3e,$85,$f4,$95,$b8,$f4,$ad ; $1a48 (D)
    .byte   $04,$04,$91,$04,$1f,$f9,$5d,$be ; $1a50 (D)
    .byte   $eb,$a7,$9b,$e6,$fa,$e9,$eb,$26 ; $1a58 (D)
    .byte   $e5,$7d,$19,$c3,$c7,$3d,$37,$6f ; $1a60 (D)
    .byte   $10,$6d,$01,$51,$cb,$2e,$fe,$f5 ; $1a68 (D)
    .byte   $20,$47,$01,$05,$ad,$ff,$f2,$9d ; $1a70 (D)
    .byte   $bf,$7c,$88,$c7,$f9,$42,$12,$13 ; $1a78 (D)
    .byte   $11,$2f,$60,$68,$32,$fb,$eb,$f5 ; $1a80 (D)
    .byte   $95,$a5,$63,$49,$e8,$c8,$80,$c9 ; $1a88 (D)
    .byte   $b0,$2c,$52,$02,$bf,$6c,$3c,$1a ; $1a90 (D)
    .byte   $ae,$28,$1d,$57,$f9,$ed,$fc,$9d ; $1a98 (D)
    .byte   $21,$82,$1c,$3c,$bf,$bf,$ef,$fc ; $1aa0 (D)
    .byte   $d1,$e9,$0a,$c3,$b5,$57,$5d,$bc ; $1aa8 (D)
    .byte   $11,$1b,$2d                     ; $1ab0 (D)
L1ab3
    .byte   $29,$ed,$8e,$af,$57,$bf,$f8,$5c ; $1ab3 (D)
    .byte   $d0,$bd,$99,$fd,$0e,$82,$28,$00 ; $1abb (D)
    .byte   $26,$87,$ef,$f8,$f8,$b3,$6d,$3f ; $1ac3 (D)
    .byte   $1e,$29,$9b,$98,$62,$8e,$11,$02 ; $1acb (D)
    .byte   $0a,$74,$f3,$dd,$4f,$99,$44,$aa ; $1ad3 (D)
    .byte   $fa,$46,$f3,$af,$03,$98,$c4,$80 ; $1adb (D)
    .byte   $c1,$b8,$bd,$fd,$eb,$86,$3a,$54 ; $1ae3 (D)
    .byte   $33,$89,$81,$ee,$a1,$82,$80,$03 ; $1aeb (D)
    .byte   $29,$d7,$1d,$7b,$df,$3f,$98,$c4 ; $1af3 (D)
    .byte   $5c,$54,$03,$71,$b5,$5b,$e0,$4e ; $1afb (D)
    .byte   $4d,$1f,$f9,$bb,$b9,$48,$ea,$14 ; $1b03 (D)
    .byte   $bb,$66,$c5,$ea,$49,$10,$98,$8b ; $1b0b (D)
    .byte   $c2,$67,$9f,$19,$6d,$9c,$39,$67 ; $1b13 (D)
    .byte   $59,$b7,$d8,$49,$a1,$64,$01,$54 ; $1b1b (D)
    .byte   $12,$f4,$b3,$ee,$85,$c2,$3f,$50 ; $1b23 (D)
    .byte   $54,$2b,$93,$0f,$2b,$00,$60,$e6 ; $1b2b (D)
    .byte   $95,$14,$37,$be,$6f,$a6,$bd,$c5 ; $1b33 (D)
    .byte   $e0,$36,$21,$eb,$28,$84,$12,$80 ; $1b3b (D)
    .byte   $01,$16,$f7,$d5,$1a,$ac,$92,$8b ; $1b43 (D)
    .byte   $78,$4b,$5d,$2b,$0d,$00,$41,$82 ; $1b4b (D)
    .byte   $0a,$3f,$a9,$ff,$fe,$91,$e0,$9a ; $1b53 (D)
    .byte   $07,$c6,$b5,$eb,$9e,$c9,$c9,$ed ; $1b5b (D)
    .byte   $32,$5f,$fe,$eb,$de,$1c,$85,$c6 ; $1b63 (D)
    .byte   $ae,$47,$a9,$5c,$af,$50,$cf,$0d ; $1b6b (D)
    .byte   $b9,$53,$f9,$33,$eb,$5f,$ab,$ce ; $1b73 (D)
    .byte   $2c,$9e,$6f,$ac,$94,$08,$80,$10 ; $1b7b (D)
    .byte   $09,$8f,$7f,$9b,$c6,$41,$51,$52 ; $1b83 (D)
    .byte   $48,$24,$15,$12,$e7,$11,$0a,$02 ; $1b8b (D)
    .byte   $a4,$4e,$be,$f1,$ee,$24,$cd,$70 ; $1b93 (D)
    .byte   $6b,$a7,$24,$36,$42,$b7,$ae,$00 ; $1b9b (D)
    .byte   $00,$ef,$df,$7f,$7f,$3a,$5c,$c9 ; $1ba3 (D)
    .byte   $9f,$b4,$ad,$7b,$07,$b0,$c1,$94 ; $1bab (D)
    .byte   $91,$af,$35,$2b,$f6,$5e,$6f,$03 ; $1bb3 (D)
    .byte   $6a,$f4,$e0,$fd,$e0,$91,$00,$80 ; $1bbb (D)
    .byte   $c3,$e4,$fe,$76,$f1,$ac,$5e,$53 ; $1bc3 (D)
    .byte   $05,$c7,$1f,$5c,$43,$28,$21,$16 ; $1bcb (D)
    .byte   $51,$cf,$fe,$97,$fe,$ad,$5f,$37 ; $1bd3 (D)
    .byte   $f1,$00,$3e,$db,$ee,$00,$24,$0d ; $1bdb (D)
    .byte   $4a,$db,$b8,$df,$7f,$bf,$92,$21 ; $1be3 (D)
    .byte   $3a,$cf,$7b,$d8,$6f,$20,$22,$b8 ; $1beb (D)
    .byte   $20,$fe,$e3,$cc,$df,$da,$46,$d4 ; $1bf3 (D)
    .byte   $ac,$60,$b2,$ad,$a5             ; $1bfb (D)

;###############################################################################

L1c00
    .word   $1ec1                           ; $1c00 (D) ram_AF, ram_D2+ram_B1
    .byte   $00,$00,$00,$00                 ; $1c02 (D) ram_C6, jmpIdx, counter, counter+1
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
    inc     ram_8E                  ;5
    bne     L1c74                   ;2/3
    lda     color                   ;3
    clc                             ;2
    adc     #$50                    ;2
    sta     color                   ;3   =  17
L1c74
    rts                             ;6   =   6

L1c75 SUBROUTINE
    cpx     #$02                    ;2
    beq     L1c7a                   ;2/3
    rts                             ;6   =  10

L1c7a
    ldy     GL_8A5                  ;4
    ldx     #$00                    ;2   =   6
.loop
    lda     L1c9a,x                 ;4
    sta     L180e,x                 ;5
    inx                             ;2
    cpx     #$08                    ;2
    bne     .loop                   ;2/3
    lda     #$10                    ;2
    sta     ram_D2                  ;3
    lda     #$00                    ;2
    sta     ram_D3                  ;3
    sta     ram_BC                  ;3
    jsr     Dial                    ;6
    jmp     L13a3                   ;3   =  37

L1c9a
    .byte   $63,$6c,$03,$14
    .word   L15f6,$ffff             ; $1c9a (D)

Check1800 SUBROUTINE
    ldx     #$03                    ;2
    lda     #$e9                    ;2   =   4
L1ca6
    cmp     L1802,x                 ;4
  IF ORIGINAL
    bne     .failed                 ;2/3
  ELSE
    ds      2, $ea
  ENDIF
    asl                             ;2
    dex                             ;2
    bne     L1ca6                   ;2/3
    lda     #>L1800                 ;2
    sta     dataPtr2+1              ;3
    ldy     #<L1800                 ;2
    sty     dataPtr2                ;3
    tya                             ;2   =  24      = 0
.loop
    clc                             ;2
    adc     (dataPtr2),y            ;5
    inc     dataPtr2                ;5
    bne     .skipHi                 ;2/3
    inc     dataPtr2+1              ;5   =   9
.skipHi
    ldx     L1800                   ;4
    cpx     dataPtr2                ;3
    bne     .loop                   ;2/3
    ldx     L1801                   ;4
    cpx     dataPtr2+1              ;3
    bne     .loop                   ;2/3
;  IF ORIGINAL
    cmp     (dataPtr2),y            ;5   =  23
;  ELSE
;    lda     #0
;  ENDIF
.failed
    rts                             ;6   =   6

    .byte   $04,$11,$6c,$33,$fd,$20,$31,$10 ; $1cd2 (D)
    .byte   $c9,$ba,$f0,$de,$c9,$e0         ; $1cda (D)

    SUBROUTINE
XPosSprite0 = $18e0
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    ldx     #$07                    ;2   =   2
L1ce4
    dex                             ;2
    bne     L1ce4                   ;2/3
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

    SUBROUTINE
.loop ;L18f9?
    nop                             ;2
    lda     (ram_91),y              ;5
    and     ram_96|$100             ;4      = $ff       flash
    eor     ram_99|$100             ;4      = $00|$ff   invert
    tax                             ;2
    lda     (ram_93),y              ;5
    and     ram_97|$100             ;4      = $ff
    eor     ram_9A|$100             ;4      = $00|$ff
    stx     GRP0                    ;3
    jmp     .cont                   ;3   =  36

.cont = $1910
    sta     GRP0                    ;3
    inc     ram_86                  ;5
DrawDialDigits = $1914
    ldy     ram_86                  ;3
    lda     (ram_8F),y              ;5
    and     ram_95|$100             ;4      = $ff
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    eor     ram_98|$100             ;4      = $00|$ff
    sta     GRP0                    ;3
    cpy     ram_87                  ;3
    bne     .loop                   ;2/3!
    lda     #$00                    ;2
    sta     GRP0                    ;3
    rts                             ;6   =  23

    SUBROUTINE
DrawDialPad = $192b
    lda     ram_86                  ;3
    sta     COLUPF                  ;3
    lda     ram_87                  ;3
    sta     ram_A0                  ;3   =  12
L1d33; = L1933,L1533?
    lda     #$fe                    ;2
    sta     PF2                     ;3
    jsr     XPosSprite0             ;6
    ldx     #$06                    ;2
    jsr     WaitLines               ;6
    lda     #<L1a24                 ;2          1 4 7 *
    sta     ram_8F                  ;3
    lda     #<L1a50                 ;2          2 5 8 0
    sta     ram_91                  ;3
    lda     #<L1a7c                 ;2          3 6 9 #
    sta     ram_93                  ;3
    lda     #>$1a24                 ;2
    sta     ram_90                  ;3
    lda     #>$1a50                 ;2
    sta     ram_92                  ;3
    lda     #>$1a7c                 ;2
    sta     ram_94                  ;3
    ldy     #$00                    ;2
    sty     ram_86                  ;3
    tya                             ;2   =  56
.loopRow
    clc                             ;2
    adc     #$0b                    ;2
    sta     ram_87                  ;3
    sty     ram_DE                  ;3
    jsr     L19a7                   ;6
    jsr     DrawDialDigits          ;6
    ldx     #$03                    ;2
    jsr     WaitLines               ;6
    ldy     ram_DE                  ;3
    iny                             ;2
    lda     ram_86                  ;3
    cmp     #$22                    ;2
    bmi     .loopRow                ;2/3
    ldx     #$06                    ;2
    jsr     WaitLines               ;6
    lda     #$00                    ;2
    sta     PF2                     ;3
    ldx     #$03                    ;2
    jsr     WaitLines               ;6
    lda     #$00                    ;2
    sta     ram_86                  ;3
    lda     #$0b                    ;2
    sta     ram_87                  ;3
    jsr     L19df                   ;6          draw digits at bottom squares
    jsr     L19b3                   ;6
    jsr     DrawDialDigits          ;6
    lda     #YELLOW|$f              ;2
    sta     COLUPF                  ;3
    lda     #$98                    ;2
    sta     WSYNC                   ;3   = 101
;---------------------------------------
    sta     PF2                     ;3
    lda     #$00                    ;2
    sta     WSYNC                   ;3   =   8
;---------------------------------------
    sta     PF2                     ;3
    rts                             ;6   =   9

;L19a7
    jsr     L19cf                   ;6
    cpy     yDial                   ;3
    bne     L1db2                   ;2/3
    ldx     xDial                   ;3
    dec     ram_98,x                ;6   =  20      invert
L1db2
    rts                             ;6   =   6

;L19b3
    jsr     L19cf                   ;6
    ldx     numDigits               ;3
    cpx     #$03                    ;2
    beq     L1db2                   ;2/3
    lda     ram_9C                  ;3
    sta     ram_95,x                ;4              flash
    dec     ram_9B                  ;5
    bne     L1db2                   ;2/3
    lda     ram_9C                  ;3
    eor     #$ff                    ;2
    sta     ram_9C                  ;3
    lda     #$14                    ;2
    sta     ram_9B                  ;3
    rts                             ;6   =  46

;L19cf
    ldx     #$ff                    ;2
    stx     ram_95                  ;3
    stx     ram_96                  ;3
    stx     ram_97                  ;3
    inx                             ;2
    stx     ram_98                  ;3
    stx     ram_99                  ;3
    stx     ram_9A                  ;3
    rts                             ;6   =  28

;L19df
    lda     #<L1aa8                 ;2
    sta     ram_8F                  ;3
    sta     ram_91                  ;3
    sta     ram_93                  ;3
    lda     #>L1aa8                 ;2
    sta     ram_90                  ;3
    sta     ram_92                  ;3
    sta     ram_94                  ;3
    ldx     #0                      ;2
    lda     ram_A0                  ;3
    beq     L1e16                   ;2/3!
;L19f5?
    cpx     numDigits               ;3
    bpl     L1e16                   ;2/3!
    ldy     digitLst,x              ;4
    lda     L1ab3,y                 ;4
    clc                             ;2
    adc     #<L1a24                 ;2
    pha                             ;3
    lda     #$00                    ;2
    adc     #>$1a24                 ;2
    pha                             ;3
    txa                             ;2
    asl                             ;2
    tay                             ;2
    pla                             ;4
    sta.wy  ptrLst+1,y              ;5
    pla                             ;4
    sta.wy  ptrLst,y                ;5
    inx                             ;2
    jmp     L19f5                   ;3   =  61

L1e16
    lda     L1a20,x                 ;4
    beq     L1e1f                   ;2/3
    tax                             ;2
    jsr     WaitLines               ;6   =  14
L1e1f
    rts                             ;6   =   6

;L1a20?
    .byte   $02,$02,$01,$00                 ; $1e20 (D)
L1a24 ;?
    .byte   %00000000 ; |        |            $1e24 (G)
    .byte   %00001000 ; |    #   |            $1e25 (G)
    .byte   %00011000 ; |   ##   |            $1e26 (G)
    .byte   %00001000 ; |    #   |            $1e27 (G)
    .byte   %00001000 ; |    #   |            $1e28 (G)
    .byte   %00001000 ; |    #   |            $1e29 (G)
    .byte   %00001000 ; |    #   |            $1e2a (G)
    .byte   %00001000 ; |    #   |            $1e2b (G)
    .byte   %00001000 ; |    #   |            $1e2c (G)
    .byte   %00011100 ; |   ###  |            $1e2d (G)
    .byte   %00000000 ; |        |            $1e2e (G)
    .byte   %00000000 ; |        |            $1e2f (G)
    .byte   %00101000 ; |  # #   |            $1e30 (G)
    .byte   %00101000 ; |  # #   |            $1e31 (G)
    .byte   %00101000 ; |  # #   |            $1e32 (G)
    .byte   %00101000 ; |  # #   |            $1e33 (G)
    .byte   %00101000 ; |  # #   |            $1e34 (G)
    .byte   %00111100 ; |  ####  |            $1e35 (G)
    .byte   %00001000 ; |    #   |            $1e36 (G)
    .byte   %00001000 ; |    #   |            $1e37 (G)
    .byte   %00001000 ; |    #   |            $1e38 (G)
    .byte   %00000000 ; |        |            $1e39 (G)
    .byte   %00000000 ; |        |            $1e3a (G)
    .byte   %00111100 ; |  ####  |            $1e3b (G)
    .byte   %00000100 ; |     #  |            $1e3c (G)
    .byte   %00000100 ; |     #  |            $1e3d (G)
    .byte   %00000100 ; |     #  |            $1e3e (G)
    .byte   %00001000 ; |    #   |            $1e3f (G)
    .byte   %00001000 ; |    #   |            $1e40 (G)
    .byte   %00010000 ; |   #    |            $1e41 (G)
    .byte   %00010000 ; |   #    |            $1e42 (G)
    .byte   %00010000 ; |   #    |            $1e43 (G)
    .byte   %00000000 ; |        |            $1e44 (G)
    .byte   %00000000 ; |        |            $1e45 (G)
    .byte   %00000000 ; |        |            $1e46 (G)
    .byte   %01000010 ; | #    # |            $1e47 (G)
    .byte   %00100100 ; |  #  #  |            $1e48 (G)
    .byte   %00011000 ; |   ##   |            $1e49 (G)
    .byte   %01111110 ; | ###### |            $1e4a (G)
    .byte   %00011000 ; |   ##   |            $1e4b (G)
    .byte   %00100100 ; |  #  #  |            $1e4c (G)
    .byte   %01000010 ; | #    # |            $1e4d (G)
    .byte   %00000000 ; |        |            $1e4e (G)
    .byte   %00000000 ; |        |            $1e4f (G)
L1a50
    .byte   %00000000 ; |        |            $1e50 (G)
    .byte   %00011000 ; |   ##   |            $1e51 (G)
    .byte   %00100100 ; |  #  #  |            $1e52 (G)
    .byte   %00000100 ; |     #  |            $1e53 (G)
    .byte   %00000100 ; |     #  |            $1e54 (G)
    .byte   %00001000 ; |    #   |            $1e55 (G)
    .byte   %00010000 ; |   #    |            $1e56 (G)
    .byte   %00100000 ; |  #     |            $1e57 (G)
    .byte   %00100000 ; |  #     |            $1e58 (G)
    .byte   %00111100 ; |  ####  |            $1e59 (G)
    .byte   %00000000 ; |        |            $1e5a (G)
    .byte   %00000000 ; |        |            $1e5b (G)
    .byte   %00111100 ; |  ####  |            $1e5c (G)
    .byte   %00100000 ; |  #     |            $1e5d (G)
    .byte   %00100000 ; |  #     |            $1e5e (G)
    .byte   %00111000 ; |  ###   |            $1e5f (G)
    .byte   %00100100 ; |  #  #  |            $1e60 (G)
    .byte   %00000100 ; |     #  |            $1e61 (G)
    .byte   %00000100 ; |     #  |            $1e62 (G)
    .byte   %00100100 ; |  #  #  |            $1e63 (G)
    .byte   %00011000 ; |   ##   |            $1e64 (G)
    .byte   %00000000 ; |        |            $1e65 (G)
    .byte   %00000000 ; |        |            $1e66 (G)
    .byte   %00011000 ; |   ##   |            $1e67 (G)
    .byte   %00100100 ; |  #  #  |            $1e68 (G)
    .byte   %00100100 ; |  #  #  |            $1e69 (G)
    .byte   %00100100 ; |  #  #  |            $1e6a (G)
    .byte   %00011000 ; |   ##   |            $1e6b (G)
    .byte   %00100100 ; |  #  #  |            $1e6c (G)
    .byte   %00100100 ; |  #  #  |            $1e6d (G)
    .byte   %00100100 ; |  #  #  |            $1e6e (G)
    .byte   %00011000 ; |   ##   |            $1e6f (G)
    .byte   %00000000 ; |        |            $1e70 (G)
    .byte   %00000000 ; |        |            $1e71 (G)
    .byte   %00011000 ; |   ##   |            $1e72 (G)
    .byte   %00100100 ; |  #  #  |            $1e73 (G)
    .byte   %00100100 ; |  #  #  |            $1e74 (G)
    .byte   %00100100 ; |  #  #  |            $1e75 (G)
    .byte   %00100100 ; |  #  #  |            $1e76 (G)
    .byte   %00100100 ; |  #  #  |            $1e77 (G)
    .byte   %00100100 ; |  #  #  |            $1e78 (G)
    .byte   %00100100 ; |  #  #  |            $1e79 (G)
    .byte   %00011000 ; |   ##   |            $1e7a (G)
    .byte   %00000000 ; |        |            $1e7b (G)
L1a7c
    .byte   %00000000 ; |        |            $1e7c (G)
    .byte   %00011000 ; |   ##   |            $1e7d (G)
    .byte   %00100100 ; |  #  #  |            $1e7e (G)
    .byte   %00000100 ; |     #  |            $1e7f (G)
    .byte   %00000100 ; |     #  |            $1e80 (G)
    .byte   %00011000 ; |   ##   |            $1e81 (G)
    .byte   %00000100 ; |     #  |            $1e82 (G)
    .byte   %00000100 ; |     #  |            $1e83 (G)
    .byte   %00100100 ; |  #  #  |            $1e84 (G)
    .byte   %00011000 ; |   ##   |            $1e85 (G)
    .byte   %00000000 ; |        |            $1e86 (G)
    .byte   %00000000 ; |        |            $1e87 (G)
    .byte   %00011000 ; |   ##   |            $1e88 (G)
    .byte   %00100100 ; |  #  #  |            $1e89 (G)
    .byte   %00100000 ; |  #     |            $1e8a (G)
    .byte   %00100000 ; |  #     |            $1e8b (G)
    .byte   %00111000 ; |  ###   |            $1e8c (G)
    .byte   %00100100 ; |  #  #  |            $1e8d (G)
    .byte   %00100100 ; |  #  #  |            $1e8e (G)
    .byte   %00100100 ; |  #  #  |            $1e8f (G)
    .byte   %00011000 ; |   ##   |            $1e90 (G)
    .byte   %00000000 ; |        |            $1e91 (G)
    .byte   %00000000 ; |        |            $1e92 (G)
    .byte   %00011000 ; |   ##   |            $1e93 (G)
    .byte   %00100100 ; |  #  #  |            $1e94 (G)
    .byte   %00100100 ; |  #  #  |            $1e95 (G)
    .byte   %00100100 ; |  #  #  |            $1e96 (G)
    .byte   %00011100 ; |   ###  |            $1e97 (G)
    .byte   %00000100 ; |     #  |            $1e98 (G)
    .byte   %00000100 ; |     #  |            $1e99 (G)
    .byte   %00100100 ; |  #  #  |            $1e9a (G)
    .byte   %00011000 ; |   ##   |            $1e9b (G)
    .byte   %00000000 ; |        |            $1e9c (G)
    .byte   %00000000 ; |        |            $1e9d (G)
    .byte   %00000000 ; |        |            $1e9e (G)
    .byte   %00100100 ; |  #  #  |            $1e9f (G)
    .byte   %01111110 ; | ###### |            $1ea0 (G)
    .byte   %00100100 ; |  #  #  |            $1ea1 (G)
    .byte   %00100100 ; |  #  #  |            $1ea2 (G)
    .byte   %01111110 ; | ###### |            $1ea3 (G)
    .byte   %00100100 ; |  #  #  |            $1ea4 (G)
    .byte   %00100100 ; |  #  #  |            $1ea5 (G)
    .byte   %00000000 ; |        |            $1ea6 (G)
    .byte   %00000000 ; |        |            $1ea7 (G)
L1aa8 = . - $400
    .byte   %00000000 ; |        |            $1ea8 (G)
    .byte   %11111111 ; |########|            $1ea9 (G)
    .byte   %11111111 ; |########|            $1eaa (G)
    .byte   %11111111 ; |########|            $1eab (G)
    .byte   %11111111 ; |########|            $1eac (G)
    .byte   %11111111 ; |########|            $1ead (G)
    .byte   %11111111 ; |########|            $1eae (G)
    .byte   %11111111 ; |########|            $1eaf (G)
    .byte   %11111111 ; |########|            $1eb0 (G)
    .byte   %11111111 ; |########|            $1eb1 (G)
    .byte   %00000000 ; |        |            $1eb2 (G)
;L1ab3?
    .byte   $4d,$00,$2c,$58,$0b,$37,$63,$16 ; $1eb3 (D)
    .byte   $42,$6e,$d3,$cc,$ef,$f4,$16,$00 ; $1ebb (D)
    .byte   $06,$d3,$e1,$d6,$c5,$04,$12,$00 ; $1ec3 (D)
    .byte   $12,$d4,$d2,$f5,$ee             ; $1ecb (D)

L1ed0
    lda     #$7f                    ;2
    ldx     #$08                    ;2
    ldy     #$ff                    ;2
    sty     AUDV0                   ;3
    ldy     #$01                    ;2
    sty     AUDC0                   ;3
;L12dc?
    sta     PF1                     ;3
    sta     PF2                     ;3
    stx     AUDF0                   ;3
    jsr     L1533                   ;6
    cmp     #$00                    ;2
    beq     L1eee                   ;2/3
    dex                             ;2
    lsr                             ;2
    jmp     L12dc                   ;3   =  40

L1eee
    sta     AUDV0                   ;3
    rts                             ;6   =   9

;L1ef1
    jsr     L14b9                   ;6
    lda     #$20                    ;2
    jsr     L17f1                   ;6
    lda     GL_885                  ;4
    lda     L1802                   ;4
    bne     L1f09                   ;2/3
    lda     #<L1308                 ;2
    ldx     #>$1308                 ;2
    jmp     L130f                   ;3   =  31

L1308 ;?
    brk                             ;7   =   7

L1f09
    lda     L180a                   ;4
    ldx     L180b                   ;4
;L130f?
    sta     dataPtr2                ;3
    stx     dataPtr2+1              ;3
    ldy     #$00                    ;2
    lda     (dataPtr2),y            ;5
    clc                             ;2
    adc     #$05                    ;2
    jsr     L17f1                   ;6
    lda     ram_D2                  ;3
    jsr     L17f1                   ;6
    lda     ram_D3                  ;3
    jsr     L17f1                   ;6
    lda     speed                   ;3
    asl                             ;2
    asl                             ;2
    asl                             ;2
    asl                             ;2
    sta     ram_DE                  ;3
    lda     dialType                ;3
    and     #$0f                    ;2
    ora     ram_DE                  ;3
    jsr     L17f1                   ;6          missing code!
    jsr     L14d5                   ;6
    jsr     SetDelay                ;6
    ldy     #$00                    ;2   =  91
L1f40
    lda     L1fda,y                 ;4
    jsr     L17f1                   ;6
    iny                             ;2
    cpy     #$05                    ;2
    bne     L1f40                   ;2/3
    ldy     #$00                    ;2
    lda     (dataPtr2),y            ;5
    beq     L1f5b                   ;2/3
    tax                             ;2   =  27
L1f52
    iny                             ;2
    lda     (dataPtr2),y            ;5
    jsr     L17f1                   ;6
    dex                             ;2
    bne     L1f52                   ;2/3 =  17
L1f5b
    jsr     L14d5                   ;6
    jsr     L14e0                   ;6
    rts                             ;6   =  18

    .byte   $14,$2e,$1b,$76,$16,$16,$17,$c4 ; $1f62 (D)
    .byte   $16,$ff,$16,$c3,$15,$b5,$cd,$d3 ; $1f6a (D)
    .byte   $d3,$a0,$d3,$d4,$a0,$d2,$c8,$66 ; $1f72 (D)
    .byte   $16,$ef,$16,$79,$15,$32,$1d,$24 ; $1f7a (D)
    .byte   $5a,$30,$03,$20,$81,$14         ; $1f82 (D)

CheckSum SUBROUTINE
    ldx     #$00                    ;2
    stx     ram_CA                  ;3
    lda     GL_STOP_PULSE           ;4   =   9  
L1f8f
    lda     L1fc0,x                 ;4          ..$1fdf
    eor     ram_CA                  ;3
    sta     ram_CA                  ;3
    asl                             ;2
    rol     ram_CA                  ;5
    inx                             ;2
    cpx     #$1f                    ;2
    bne     L1f8f                   ;2/3
    lda     L1fc0,x                 ;4
    cmp     ram_CA                  ;3
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

L1fb0 SUBROUTINE
    lda     GL_START_CA0            ;4          page 0 into $c00 for writing
    sty     L1000                   ;4
    lda     L1ff8                   ;4
    and     #$03                    ;2
    ldy     GL_STOP_C80             ;4          page 0 into $c00?
    rts                             ;6   =  24

Start SUBROUTINE
    sei                             ;2   =   2
L1fc0
    cld                             ;2
    lda     #$00                    ;2
    tax                             ;2
    ldy     #$a8                    ;2   =   8
L1fc6
    txs                             ;2
    inx                             ;2
    bne     L1fc6                   ;2/3
    dey                             ;2
    bne     L1fc6                   ;2/3
    ldx     #$05                    ;2
    lda     GL_4AD                  ;4   =  16  page 13 into $400 for writing
L1fd2
    ldy     #$02                    ;2
    jsr     L1fb0                   ;6
    cmp     #$02                    ;2
  IF ORIGINAL
    bne     L1fe4                   ;2/3 =  12
  ELSE
    ds      2, $ea
  ENDIF
L1fda = . - 1
    ldy     #$01                    ;2
    jsr     L1fb0                   ;6
    cmp     #$01                    ;2
  IF ORIGINAL
    beq     L1fea                   ;2/3 =  14
  ELSE
    bne     L1fea                   ;2/3 =  14
  ENDIF
L1fe4
    lda     GL_680                  ;4          page 0 into $600?
    dex                             ;2
    bne     L1fd2                   ;2/3 =   8
L1fea
    lda     GL_680                  ;4          page 0 into $600? 
    jsr     CheckSum                ;6          -> $c80
    lda     GL_481                  ;4          page 1 into $400? page 1 into slice 1?
    lda     GL_582                  ;4          page 2 into $500? page 2 into slice 2?
    lda     GL_884                  ;4          page 4 into $800?
L1ff8 = . - 1
    jmp     L1000                   ;3   =   6

    .word   Start,Start                     ; $1ffc (D)
