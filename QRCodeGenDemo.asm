; QR Code generator demo V0.3
; (C) 2021 Thomas Jentzsch

; TODOs
; + get it working!
; + reduce RAM usage
;   + reverse data
;   + overlap data with QR code
; + multiple pattern formats
;   + apply pattern
;   x evaluate pattern (very slow!)
;   + optimize single pattern for space
; x support multiple QR code versions
; o support multiple QR code levels
; x try to optimize function pattern (SetPixel)
; x add logo (does NOT work for such small sizes)

;---------------------------------------------------------------
; QR code data bytes (version 2):
; - 13 into right sprite column
; - 17 until horizontal timer line
; - 36 into right and middle sprite column
; -  8 in left sprite column

    processor 6502
  LIST OFF
    include vcs.h
  LIST ON


;===============================================================================
; A S S E M B L E R - S W I T C H E S
;===============================================================================

BASE_ADR        = $f000

NTSC            = 1     ; 0 = PAL50

; QR Code Generator Switches
QR_VERSION      = 2     ; 1, 2 or 3 (TODO 1 and 3)
QR_LEVEL        = 1     ; 0 (L), 1 (M), 2 (Q) and 3 (H))
QR_PADDING      = 1     ; (+22) add padding bytes

; Atari 2600 specific QR settings (set to 0 for other platforms)
QR_OVERLAP      = 1     ; overlaps input and output data to save RAM (defined for version 2 only!)
QR_SINGLE_MASK  = 0     ; (-255) if 1 uses only 1 of the 8 mask pattern

  IF QR_VERSION = 1 || QR_VERSION = 3
    ECHO    ""
    ECHO    "ERROR: Version", [QR_VERSION]d, "unsupported by demo code"
    ERR
  ENDIF


;===============================================================================
; C O N S T A N T S
;===============================================================================

NUM_FIRST   = 1     ; left top 9 and bottom 8 bits are fixed!
RND_EOR_VAL = $b4

_QR_TOTAL   SET 0


;===============================================================================
; Z P - V A R I A B L E S
;===============================================================================

    SEG.U   variables
    ORG     $80

random      .byte

;---------------------------------------
; QR code variables
; all byte counts based on version 2, level M QR code
tmpVars     ds 6

msgIdx      = tmpVars + 3
  IF QR_SINGLE_MASK = 0
qrPattern   .byte
  ENDIF
;---------------------------------------
data        ds QR_TOTAL        ; 44 bytes
remainder   = data              ; (QR_DEGREE = 16 bytes)
msgData     = data + QR_DEGREE  ; (QR_MAX_DATA = 28 bytes)
;- - - - - - - - - - - - - - - - - - - -
; The QR code overlaps the data! It overwrites the data while being drawn.
  IF QR_OVERLAP
qrCodeLst   = data + 6   ; all but 6 bytes overlap (version 2 only!)
            ds NUM_FIRST + QR_SIZE*3 - QR_TOTAL + 6     ; 38 bytes
  ELSE
qrCodeLst   ds NUM_FIRST + QR_SIZE*3                    ; 76 bytes
  ENDIF
grp0LLst    = qrCodeLst + QR_SIZE * 0
firstMsl    = qrCodeLst + QR_SIZE * 1
grp1Lst     = qrCodeLst + NUM_FIRST + QR_SIZE * 1
grp0RLst    = qrCodeLst + NUM_FIRST + QR_SIZE * 2
CODE_LST_SIZE = . - qrCodeLst
;---------------------------------------
; QR code total = 89/127 bytes

    ECHO    "RAM:", [$100 - .]d, "bytes free"
    ECHO    ""


;===============================================================================
; M A C R O S
;===============================================================================

  MAC BIT_B
    .byte   $24
  ENDM

  MAC BIT_W
    .byte   $2c
  ENDM

  MAC SLEEP
    IF {1} = 1
      ECHO "ERROR: SLEEP 1 not allowed !"
      END
    ENDIF
    IF {1} & 1
      nop $00
      REPEAT ({1}-3)/2
        nop
      REPEND
    ELSE
      REPEAT ({1})/2
        nop
      REPEND
    ENDIF
  ENDM

;-----------------------------------------------------------
  MAC NEXT_RANDOM
;-----------------------------------------------------------
; update random value:
    lda     random                                  ; 3
    lsr                                             ; 2
    bcc     .skipEOR                                ; 2/3
    eor     #RND_EOR_VAL                            ; 2
.skipEOR
    sta     random                                  ; 3 = 14/19
  ENDM

; Platform specific macros
 IF QR_OVERLAP = 0
;-----------------------------------------------------------
  MAC _BLACK_FUNC
;-----------------------------------------------------------
; blacks all function/alignment and timing pattern
    ldx     #CODE_LST_SIZE-1
.loopBlack
    lda     BlackGfx,x
    sta     qrCodeLst,x
    dex
    bpl     .loopBlack
  ENDM

 ELSE

;-----------------------------------------------------------
  MAC _BLACK_LEFT
;-----------------------------------------------------------
; blacks all function/alignment and timing pattern of the left column
    ldx     #NUM_FIRST + QR_SIZE-1-8
.loopBlackLeft
    lda     LeftBlack+8,x
    sta     qrCodeLst+8,x
    dex
    bpl     .loopBlackLeft
  ENDM

;-----------------------------------------------------------
  MAC _BLACK_MIDDLE
;-----------------------------------------------------------
; blacks all function/alignment and timing pattern of the middle column
    ldx     #QR_SIZE-1
.loopBlackMiddle
    lda     GRP1Black,x
    sta     grp1Lst,x
    dex
    bpl     .loopBlackMiddle
  ENDM

;-----------------------------------------------------------
  MAC _BLACK_RIGHT
;-----------------------------------------------------------
; blacks all function/alignment and timing pattern of the right column
    ldx     #QR_SIZE
.loopBlackRight
    lda     GRP0RBlack-1,x
    sta     grp0RLst-1,x
    dex
    bne     .loopBlackRight
  ENDM
 ENDIF ; /QR_OVERLAP

;-----------------------------------------------------------
  MAC _DRAW_FUNC
;-----------------------------------------------------------
    ldx     #CODE_LST_SIZE-1
.loopBlack
    lda     qrCodeLst,x
    ora     BlackGfx,x
    eor     EorGfx,x
    sta     qrCodeLst,x
    dex
    bpl     .loopBlack

   IF QR_SINGLE_MASK && (QR_VERSION = 3 || QR_VERSION = 4)
; invert 1st pixel of 1st copy (bit 0 not set in formats Q and H)
    ldy     #0
    ldx     #QR_SIZE-9
    jsr     InvertPixel
   ENDIF
  ENDM

    include QRCodeGen.inc


;===============================================================================
; R O M - C O D E
;===============================================================================
    SEG     Bank0
    ORG     BASE_ADR

;---------------------------------------------------------------
DrawScreen SUBROUTINE
;---------------------------------------------------------------
    ldx     #227
.waitTim:
    lda     INTIM
    bne     .waitTim
    sta     WSYNC
    sta     VBLANK
    stx     TIM64T
;---------------------------------------------------------------
    ldx     #3
    bit     SWCHB
    bvs     .skipCentering
; some vertical centering
    ldx     #(192-QR_SIZE*2)/2
.skipCentering
.waitTop
    sta     WSYNC
    dex
    bne     .waitTop

    ldx     #QR_SIZE-1
    lda     #%1             ;           1st top left fixed pixel
    bne     .enterLoop

.tmpFirst    = tmpVars

; the QR code kernel
.loopKernel                 ;           @53
    lda     FirstIdxTbl,x   ; 4*
    cmp     #1              ; 2
    bcs     .newFirst       ; 2/3
    lsr     .tmpFirst       ; 5
    bpl     .endFirst       ; 3 = 16    unconditional

.newFirst                   ;           @62
; $bf/$3f | $01 | $fe
    bne     .enterLoop      ; 2/3
    lda     firstMsl        ; 3
.enterLoop
    sta     .tmpFirst       ; 3 =  8
.endFirst                   ;           @70/69
    ldy     #-2             ; 2
.loopBlock
    sta     WSYNC           ; 3         @75/74
;---------------------------------------
;M1-P0-P1-P0
    lda     .tmpFirst       ; 3
    asl                     ; 2
    sta     ENAM1           ; 3 =  8
    lda     grp1Lst,x       ; 4
    sta     GRP1            ; 3
    lda     grp0LLst,x      ; 4
    sta     GRP0            ; 3
    SLEEP   10              ;10 = 24
    lda     grp0RLst,x      ; 4
    iny                     ; 2
    cpy     #1              ; 2
    bcc     .isZero         ; 2/3
    BIT_B                   ; 1
.isZero
    dex                     ; 2
    sta     GRP0            ; 3 = 16    @48
    bcs     .loopBlock      ; 2/3
    bpl     .loopKernel     ; 3/2=5/4
    sta     WSYNC
;---------------------------------------------------------------
    sty     ENAM1
    sty     GRP1
    sty     GRP0

    ldx     #2
.waitScreen:
    lda     INTIM
    bne     .waitScreen
    sta     WSYNC
    stx     VBLANK
    rts
; DrawScreen

;---------------------------------------------------------------
Start SUBROUTINE
;---------------------------------------------------------------
    cld                         ;           Clear BCD math bit.
    lda     #0
    tax
    dex
    txs
.clearLoop:
    tsx
    pha
    bne     .clearLoop

    lda     INTIM
    ora     #$10
    sta     random

    jsr     InitDemo

.mainLoop:
    jsr     VerticalBlank
    jsr     DrawScreen
    jsr     OverScan
    jmp     .mainLoop

;---------------------------------------------------------------
InitDemo SUBROUTINE
;---------------------------------------------------------------
    sta     WSYNC
;---------------------------------------
    lda     #$0e
    sta     COLUBK
    lda     #$00
    sta     COLUP0
    sta     COLUP1

    lda     #%001
    sta     NUSIZ0
    sta     VDELP1

    ldx     #$3f
    stx     HMP0
    inx
    stx     HMP1
    lda     #$a0
    sta     HMM1

    SLEEP   3

    sta     RESM1
    sta     RESP0
    sta     RESP1

    sta     WSYNC
;---------------------------------------
    sta     HMOVE

    jmp     GenerateQR
; GameInit

;---------------------------------------------------------------
VerticalBlank SUBROUTINE
;---------------------------------------------------------------
    lda     #%00001110
.loopVSync:
    sta     WSYNC
    sta     VSYNC
    lsr
    bne     .loopVSync

  IF NTSC
    lda     #44
  ELSE
    lda     #77
  ENDIF
    sta     TIM64T

    bit     INPT4
    bmi     .skipRegen

    jsr     GenerateQR
.skipRegen
    NEXT_RANDOM
    rts
; VerticalBlank

;---------------------------------------------------------------
OverScan SUBROUTINE
;---------------------------------------------------------------
  IF NTSC
    lda     #36
  ELSE
    lda     #63
  ENDIF
    sta     TIM64T

.waitTim:
    lda     INTIM
    bne     .waitTim
    rts
; OverScan

;---------------------------------------------------------------
GenerateQR SUBROUTINE
;---------------------------------------------------------------
; *** Generate QR code from message ***
  IF QR_SINGLE_MASK = 0
    lda     random
    and     #$07
;  lda   #0
    sta     qrPattern
  ENDIF

MessageCode
; convert the message into a data stream
.msgLen     = tmpVars
.msgPtr     = tmpVars+1
    lda     random
    lsr
    lsr
    lsr
    and     #$0f
    tay
;  ldy     #0
    lda     MessagePtrLo,y
    sta     .msgPtr
    lda     MessagePtrHi,y
    sta     .msgPtr+1
    lda     MessagePtrLo+1,y
    sec
    sbc     .msgPtr
    sta     .msgLen
    START_MSG
    ldy     #0
.loopMsg
    lda     (.msgPtr),y
    ADD_MSG_BYTE
    iny
    cpy     .msgLen
    bcc     .loopMsg
    STOP_MSG

    ECHO    "QR Code message code:", [. - MessageCode]d, "bytes"
_QR_TOTAL SET _QR_TOTAL + . - MessageCode

    GEN_QR_CODE
    rts

BitMapCode
;---------------------------------------------------------------
CheckPixel SUBROUTINE
;---------------------------------------------------------------
; Platform and version specific code. Must NOT change X and Y registers!
; X = y; Y = x
; determine 8 bit column (0..2) or missile columns
    tya
    bne     .notMissile
; check if single missile byte is affected
    cpx     #8
    bcc     .alwaysSet
    cpx     #8*2
    bcs     .alwaysSet
    lda     firstMsl
    and     BitMask-8,x
    rts

.alwaysSet
    lda     #1
    rts

.notMissile
    cpy     #1+8
    bcs     .notGRP0L
  IF QR_OVERLAP
    cpx     #8              ; bottom left eye (partially) shared with data!
    bcc     .alwaysSet
  ENDIF
    lda     grp0LLst,x
    and     BitMask-1,y
    rts

.notGRP0L
    cpy     #1+8*2
    bcs     .notGRP1
    lda     grp1Lst,x
    and     BitMask-1-8,y
    rts

.notGRP1
; must be GRP0R then
    lda     grp0RLst,x
    and     BitMask-1-8*2,y
    rts

;---------------------------------------------------------------
InvertPixel SUBROUTINE
;---------------------------------------------------------------
; Platform and version specific code. Must NOT change X and Y registers!
; X = y; Y = x
; determine 8 bit column (0..2) or missile column
    tya
    bne     .notMissile
; check if single missile byte is affected
    cpx     #8
    bcc     .ignore
    cpx     #8*2
    bcs     .ignore
    lda     BitMask-8,x
    eor     firstMsl
    sta     firstMsl
.ignore
    rts

.notMissile
    cpy     #1+8
    bcs     .notGRP0L
    lda     grp0LLst,x
    eor     BitMask-1,y
    sta     grp0LLst,x
    rts

.notGRP0L
    cpy     #1+8*2
    bcs     .notGRP1
    lda     grp1Lst,x
    eor     BitMask-1-8,y
    sta     grp1Lst,x
    rts

.notGRP1
; must be GRP0R then
    lda     grp0RLst,x
    eor     BitMask-1-8*2,y
    sta     grp0RLst,x
    rts

    ECHO    "QR Code bitmap code:", [. - BitMapCode]d, "bytes"
_QR_TOTAL SET _QR_TOTAL + . - BitMapCode

;===============================================================================
; R O M - T A B L E S (Bank 0)
;===============================================================================
    org     BASE_ADR + $600

FunctionModulesData
; Platform and version specific function module data definition

  IF QR_VERSION = 1
    ERR ; TODO
  ENDIF

  IF QR_VERSION = 2
   IF QR_SINGLE_MASK
    include FuncDataV2S.inc ; TODO: special pattern
   ELSE
    include FuncDataV2.inc
   ENDIF
  ENDIF

  IF QR_VERSION = 3
    ERR ; TODO
  ENDIF

    ECHO    "QR Code function modules data:", [. - FunctionModulesData]d, "bytes"
_QR_TOTAL SET _QR_TOTAL + . - FunctionModulesData

    QR_CODE_DATA

    .byte   " QR Code Generator Demo V0.3 - (C)2021 Thomas Jentzsch "

; messages MUST NOT be longer than 26 bytes for version 2, level M!
; Galadriel:
MessageTbl
Message0
    .byte   "2002 - Thrust+ Platinum"
Message1
    .byte   "2001 - Jammed "
Message2
    .byte   "2005 - SWOOPS!"
Message3
    .byte   "2017 - ZeroPage Homebrew"
Message4
    .byte   "2019 - Aardvark"
Message5
    .byte   "2012 - Boulder Dash"
Message6
    .byte   "2021 - VROOM!"
Message7
    .byte   "2020 - Robot City"
Message8
    .byte   "2015 - Star Castle Arcade"
Message9
    .byte   "2001 - AtariAge"
Message10
    .byte   "1998 - Atari 2600 Nexus"
Message11
    .byte   "2009 - RAM Pong"
Message12
    .byte   "1977 - Atari 2600"
Message13
    .byte   "2014 - Three·s"
Message14
    .byte   "1996 - Stella Mailing List"
Message15
    .byte   "1996 - Stella Emulator"
MessageEnd
;    .byte   "..the single hardest thing"

MessagePtrLo
    .byte   <Message0, <Message1, <Message2, <Message3
    .byte   <Message4, <Message5, <Message6, <Message7
    .byte   <Message8, <Message9, <Message10, <Message11
    .byte   <Message12, <Message13, <Message14, <Message15
    .byte   <MessageEnd
MessagePtrHi
    .byte   >Message0, >Message1, >Message2, >Message3
    .byte   >Message4, >Message5, >Message6, >Message7
    .byte   >Message8, >Message9, >Message10, >Message11
    .byte   >Message12, >Message13, >Message14, >Message15

    .byte   "JTZ"

    org     BASE_ADR + $ffc
    .word   Start
    .word   Start

    ECHO    "----------------------------------------"
    ECHO    "QR Code total:", [_QR_TOTAL]d, "bytes"
    ECHO    ""
    ECHO    "QR Code Version", [QR_VERSION]d, ", Level (Degree)", [QR_LEVEL]d, "(", [QR_DEGREE]d, "), Capacity", [QR_CAPACITY]d, "bytes"
    ECHO    "  -> Message Space", [QR_CAPACITY-QR_DEGREE-2]d, "bytes"
