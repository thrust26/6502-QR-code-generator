;===============================================================================
; U S E R - D E F I N E D   Q R   C O D E   C O N S T A N T S
;===============================================================================

  IFNCONST QR_VERSION
QR_VERSION      = 2     ; 1..3, QR code size (21, 25, 29)
  ENDIF
  IFNCONST QR_LEVEL
QR_LEVEL        = 0     ; 0..4, error correction levels L, M, Q, H
  ENDIF
  IFNCONST QR_SINGLE_MASK
QR_SINGLE_MASK  = 0     ; 0|1, use only 1 of the 8 mask pattern
  ENDIF
  IFNCONST QR_PADDING
QR_PADDING      = 1     ; 0|1, add padding bytes add the end of test message text
  ENDIF
  IFNCONST QR_GENERATE
QR_GENERATE     = 0     ; 0|1, generate Reed-Solomon ECC generator polynomial on-the-fly
  ENDIF


;===============================================================================
; C A L C U L A T E D   Q R   C O D E   C O N S T A N T S
;===============================================================================

; Do NOT change the following constants!

  IFNCONST QR_OVERLAP
QR_OVERLAP      = 0
  ENDIF
  IFNCONST QR_SINGLE_MASK
QR_SINGLE_MASK  = 0
  ENDIF
  IFNCONST QR_DIRECT_DRAW
QR_DIRECT_DRAW  = 0
  ENDIF

  IF QR_VERSION = 1 ; 21x21 QR code
   IF QR_LEVEL = 0
QR_DEGREE      = 7  ; for version 1, level L QR codes
   ENDIF
   IF QR_LEVEL = 1
QR_DEGREE      = 10 ; for version 1, level M QR codes
   ENDIF
   IF QR_LEVEL = 2
QR_DEGREE      = 13 ; for version 1, level Q QR codes
   ENDIF
   IF QR_LEVEL = 3
QR_DEGREE      = 17 ; for version 1, level H QR codes
   ENDIF
  ENDIF

  IF QR_VERSION = 2 ; 25x25 QR code
   IF QR_LEVEL = 0
QR_DEGREE      = 10 ; for version 2, level L QR codes
   ENDIF
   IF QR_LEVEL = 1
QR_DEGREE      = 16 ; for version 2, level M QR codes
   ENDIF
   IF QR_LEVEL = 2
QR_DEGREE      = 22 ; for version 2, level Q QR codes
   ENDIF
   IF QR_LEVEL = 3
QR_DEGREE      = 28 ; for version 2, level H QR codes
   ENDIF
  ENDIF

  IF QR_VERSION = 3 ; 29x29 QR code
   IF QR_LEVEL = 0
QR_DEGREE      = 15 ; for version 3, level L QR codes
   ENDIF
   IF QR_LEVEL = 1
QR_DEGREE      = 26 ; for version 3, level M QR codes
   ENDIF
   IF QR_LEVEL = 2 || QR_LEVEL = 3
    ECHO    ""
    ECHO    "ERROR: Version 3, Level", [QR_LEVEL]d, "not supported"
    ERR
   ENDIF
  ENDIF

QR_SIZE = 17 + QR_VERSION * 4

; Calculate capacity based on version
_QR_VAL SET (QR_VERSION * 16 + 128) * QR_VERSION + 64
  IF QR_VERSION >= 2
_QR_NUM_ALIGN = QR_VERSION / 7 + 2
_QR_VAL SET _QR_VAL - ((25 * _QR_NUM_ALIGN - 10) * _QR_NUM_ALIGN - 55)
   IF QR_VERSION >= 7
_QR_VAL SET _QR_VAL - 36
   ENDIF
  ENDIF
QR_CAPACITY = _QR_VAL / 8

QR_MAX_DATA = QR_CAPACITY - QR_DEGREE

QR_MODE     = %0100 ; byte mode
QR_POLY     = $11d  ; GF(2^8) is based on 9 bit polynomial
                    ; x^8 + x^4 + x^3 + x^2 + 1 = 0x11d
QR_FORMATS  = 15    ; 15 type information bits

QR_MAX_MSG  = QR_MAX_DATA - 2
QR_TOTAL    = QR_MAX_DATA + QR_DEGREE ; e.g. 44


;===============================================================================
; V A R I A B L E S
;===============================================================================

qrRemainder = qrData                ; (QR_DEGREE = e.g. 16 bytes)
qrMsgData   = qrData + QR_DEGREE    ; (QR_MAX_DATA = e.g. 28 bytes)
qrMsgIdx    = qrTmpVars + 3


;===============================================================================
; Q R   C O D E   M A C R O S
;===============================================================================

; The following code has been partially converted from the C code of the
; QR code generator found at https://github.com/nayuki/QR-Code-generator

;-----------------------------------------------------------
; Returns the product of the two given field elements modulo GF(2^8/0x11D).
; All inputs are valid.
  MAC _RS_MULT
;-----------------------------------------------------------
; Russian peasant multiplication (x * y)
; Input: A = x, Y = y
; Result: A
.x      = qrTmpVars
.y      = qrTmpVars+1

    sta     .x
    sty     .y
; uint8_t z = 0;
    lda     #0
; for (int i = 7; i >= 0; i--) {
    ldy     #7
.loopI
;   z = (uint8_t)((z << 1) ^ ((z >> 7) * 0x11D));
    asl
    bcc     .skipEorPoly
    eor     #<QR_POLY
.skipEorPoly
;   z ^= ((y >> i) & 1) * x;
    asl     .y
    bcc     .skipEorX
    eor     .x
.skipEorX
; }
    dey
    bpl     .loopI
  ENDM

  IF QR_GENERATE
; Computes a Reed-Solomon ECC generator polynomial for degree 16, storing in result[0 : 16-1].
; This is implemented as a lookup table too (Generator)
;-----------------------------------------------------------
  MAC _RS_DIVISOR
;-----------------------------------------------------------
; E.g. g(x)=(x+1)(x+?)(x+?^2)(x+?^3)...(x+?^15)
;          = x^16+3Bx^15+0Dx^14+68x^13+BDx^12+44x^11+d1x^10+1e^x9+08x^8
;            +A3x^7+41x^6+29x^5+E5x^4+62x^3+32x^2+24x+3B
.root   = qrTmpVars+2
.i      = qrTmpVars+3

; memset(qrGenerator, 1, 16);
    ldx     #QR_DEGREE-1
    stx     .i
    ldy     #0
.loopClear
    sty     qrGenerator,x
    dex
    bne     .loopClear
; qrGenerator[0] = 1;  // Start off with the monomial x^0
    iny
    sty     qrGenerator
; uint8_t root = 1;
    sty     .root
; for (int i = 0; i < degree; i++) {
.loopI
; // Multiply the current product by (x - r^i)
; for (int j = degree-1; j >= 0; j--) {
    ldx     #QR_DEGREE-1
.loopJ
; qrGenerator[j] = reedSolomonMultiply(qrGenerator[j], root);
    lda     qrGenerator,x
    ldy     .root
    jsr     RSMult
; if (j)
    dex
    bmi     .skipEor
; qrGenerator[j] ^= qrGenerator[j - 1];
    eor     qrGenerator,x
.skipEor
    sta     qrGenerator+1,x
    txa
    bpl     .loopJ
; root = reedSolomonMultiply(root, 0x02);
    lda     .root
    ldy     #$02
    jsr     RSMult
    sta     .root
    dec     .i
    bpl     .loopI
  ENDM

;-----------------------------------------------------------
RSMult SUBROUTINE
    _RS_MULT
    rts
;-----------------------------------------------------------
  ENDIF ; /QR_GENERATE

;-----------------------------------------------------------
  MAC _RS_REMAINDER
;-----------------------------------------------------------
.i      = qrTmpVars+2
.factor = qrTmpVars+3

; memset(result, 0, 16); // done in START_TEXT
; for (int i = dataLen-1; i >= 0; i--) {  // Polynomial division
    ldx     #QR_MAX_DATA-1
.loopI
    stx     .i
;   uint8_t factor = qrMsgData[i] ^ qrRemainder[degree - 1];
    lda     qrMsgData,x
    eor     qrRemainder + QR_DEGREE - 1
    sta     .factor
;   memmove(&qrRemainder[1], &qrRemainder[0], (size_t)(16 - 1) * sizeof(qrRemainder[0]));
    ldx     #QR_DEGREE-1
.loopMove
    lda     qrRemainder-1,x
    sta     qrRemainder,x
    dex
    bne     .loopMove
;   qrRemainder[0] = 0;
    lda     #0
    sta     qrRemainder
;   for (int j = 16-1; j >= 0; j--)
    ldx     #QR_DEGREE-1
.loopJ
;     qrRemainder[j] ^= reedSolomonMultiply(generator[j], factor);
    ldy     .factor
  IF QR_GENERATE
    lda     qrGenerator,x
    jsr     RSMult
  ELSE
    lda     Generator,x
    _RS_MULT
  ENDIF
    eor     qrRemainder,x
    sta     qrRemainder,x
;   }
    dex
    bpl     .loopJ
; }
    ldx     .i
    dex
    bpl     .loopI
  ENDM

 IF QR_DIRECT_DRAW = 0
;-----------------------------------------------------------
  MAC _BLACK_FUNC
;-----------------------------------------------------------
; Blacks all function/alignment and timing pattern areas

; assume bitmap is clear, only draw the pattern
    ldy     #4+2            ; pattern + timing lines
.loopPattern
    lda     #$ff            ; fill pattern
    jsr     DrawPattern
    bpl     .loopPattern
  ENDM

;-----------------------------------------------------------
BlackFunc SUBROUTINE
;-----------------------------------------------------------
    _BLACK_FUNC
    rts

;-----------------------------------------------------------
  MAC _DRAW_FUNC
;-----------------------------------------------------------
; Draws all function/alignment and timing pattern over existing codewords

; blacken functions pattern first
    jsr     BlackFunc

; draw function pattern
    ldy     #4
.loopPattern
    lda     #$00            ; draw pattern
    jsr     DrawPattern
    bne     .loopPattern

; draw horizontal timing pattern
    ldy     #QR_SIZE-1 - 9
    ldx     #QR_SIZE-1 - 6
.loopTimingH
    jsr     InvertPixel
    dey
    dey
    cpy     #9
    bcs     .loopTimingH

; draw vertical timing pattern
    ldy     #6
    ldx     #QR_SIZE-1 - 9
.loopTimingV
    jsr     InvertPixel
    dex
    dex
    cpx     #9
    bcs     .loopTimingV
  ENDM ;

;---------------------------------------------------------------
DrawPattern SUBROUTINE
;---------------------------------------------------------------
; Y = index, A = fill
.index      = qrTmpVars
.width      = qrTmpVars+1
.height     = qrTmpVars+2
.offset     = qrTmpVars+3
.tmpPat     = qrTmpVars+4
.fill       = qrTmpVars+5

    sty     .index
    sta     .fill
    ldx     PatternY,y
    lda     PatternHeight,y
    sta     .height
    lda     PatternOffset,y
    tay
.loopRow
    sty     .offset
    lda     PatternStart,y
    ora     .fill
    sta     .tmpPat
    ldy     .index
    lda     PatternWidth,y
    sta     .width
    lda     PatternX,y
    tay
; draw one row
.loopBit
    sec                     ; allows for >8 black pixel
    ror     .tmpPat
    bcc     .clear
;.set
    jsr     CheckPixel
    bne     .skipInvert
.invert
    jsr     InvertPixel
.skipInvert
    iny                     ; x++
    dec     .width
    bne     .loopBit
    dex                     ; y--
    ldy     .offset
    iny
    dec     .height
    bne     .loopRow
.exit
    ldy     .index
    dey
    rts

.clear
    jsr     CheckPixel
    bne     .invert
    beq     .skipInvert
 ENDIF ; /QR_DIRECT_DRAW = 0

;-----------------------------------------------------------
; Draws the raw codewords (including data and ECC) onto the given QR Code. This requires the initial state of
; the QR Code to be black at function modules and white at codeword modules (including unused remainder bits).
  MAC _DRAW_CODEWORDS
;-----------------------------------------------------------
; Note: This part has the maximum RAM usage
.vert   = qrTmpVars+0
.j      = qrTmpVars+1
.y      = qrTmpVars+2
  IF QR_DIRECT_DRAW
.iBit   = qrTmpVars+3
.iByte  = qrTmpVars+4
.right1 = qrTmpVars+5
  ELSE
; must not overlap with DrawPattern space
.iBit   = qrTmpVars+6
.iByte  = qrTmpVars+7
.right1 = qrTmpVars+8
  ENDIF

; blacken the (right) function modules in the bitmap
  IF QR_OVERLAP
    _BLACK_RIGHT        ; returns with X = 0
  ELSE
   IF QR_DIRECT_DRAW
    _BLACK_FUNC         ; returns with X = 0
   ELSE
    jsr     BlackFunc
    ldx     #0
   ENDIF
  ENDIF
; int i = 0;  // Bit index into the data
;    ldx     #0
; 2600 code has data in reversed order
    stx     .iBit
    lda     #QR_TOTAL-1
    sta     .iByte
; // Do the funny zigzag scan
; Note: 2600 code has .right1 increased by 1
; for (int right = qrsize - 1; right >= 1; right -= 2) {  // Index of right column in each column pair
    ldy     #QR_SIZE-1+1
.loopRight
;  if (right == 6)
    cpy     #6+1
    bne     .not6
;    right = 5;
    dey                 ; skip the timing column
.not6
    sty     .right1
  IF QR_OVERLAP
; overwrite shared data
    cpy     #16+1
    bne     .skipBlackMiddle
; blacken the middle function modules in the bitmap
    _BLACK_MIDDLE
  IF QR_DIRECT_DRAW = 0
    ldy     .right1     ; DrawPattern overwrites Y
  ENDIF
.skipBlackMiddle
    cpy     #8+1
    bne     .skipBlackLeft
; blacken the left function modules in the bitmap
    _BLACK_LEFT
.skipBlackLeft
  ENDIF
;   for (int vert = 0; vert < qrsize; vert++) {  // Vertical counter
    ldy     #QR_SIZE-1
.loopVert
    sty     .vert
;       bool upward = ((right + 1) & 2) != 0; // 2600 code works in reverse
    lda     .right1
    and     #$02
    bne     .notUp
;       int y = upward ? qrsize - 1 - vert : vert;  // Actual y coordinate
    lda     #QR_SIZE-1;+1
    sec
    sbc     .vert
    tay
.notUp
    sty     .y
;     for (int j = 0; j < 2; j++) {
; some tricky code with .j here
    ldy     .right1
    BIT_B
.loopJ
    dey
    sty     .j
;       int x = right - j;  // Actual x coordinate
    dey
;       if (!getModule(qrcode, x, y) && i < dataLen * 8) {
;    ldy     .x
    ldx     .y
    jsr     CheckPixel
    bne     .skipPixel
;         bool black = getBit(qrData[i >> 3], 7 - (i & 7));
    ldx     .iByte
    lda     qrData,x
    ldx     .iBit
    and     BitMask,x
    beq     .skipInv
;         setModule(qrcode, x, y, black);
;    ldy     .x
    ldx     .y
    jsr     InvertPixel
    ldx     .iBit
.skipInv
;         i++;
    inx
    txa
    and     #$07
    sta     .iBit
    bne     .skipByte
    dec     .iByte
    bmi     .exitDraw       ; 2600 code exits here!
.skipByte
;       }
.skipPixel
    ldy     .j
    cpy     .right1
    beq     .loopJ
;     } // for j
    ldy     .vert
    dey
    bpl     .loopVert
;   } // for vert
    ldy     .right1
    dey
    dey
  IF QR_OVERLAP
    jmp     .loopRight
  ELSE
    bpl     .loopRight      ; unconditional!
  ENDIF
; } // for right
.exitDraw
  ENDM

;-----------------------------------------------------------
  MAC _APPLY_MASK
;-----------------------------------------------------------
   IF QR_SINGLE_MASK
.y          = qrTmpVars
    ldx     #QR_SIZE - 1
.loopY
    stx     .y
    ldy     #QR_SIZE - 1
.loopX
; 0: (x + y) % 2 == 0
    tya
    eor     .y
    lsr
    bcs     .skipInvert
    jsr     InvertPixel
.skipInvert
    dey
    bpl     .loopX
    dex
    bpl     .loopY
   ELSE
.y          = qrTmpVars
;.x      = qrTmpVars+2
.xMod3      = qrTmpVars+1
.yMod3      = qrTmpVars+2
.xDiv3      = qrTmpVars+3
.tmp        = qrTmpVars+4

    lda     #0
    sta     .yMod3
    ldx     #QR_SIZE - 1
.loopY
    stx     .y
    lda     #0
    sta     .xMod3
    sta     .xDiv3
    ldy     #QR_SIZE - 1
.loopX
;    sty     .x
    lda     qrPattern
    bne     .not0
; 0: (x + y) % 2 == 0
    tya
    eor     .y
    bpl     .checkMod2

.not0
    cmp     #4
    bne     .not4
; 4: (x / 3 + y / 2) % 2 == 0
    lda     .y
    lsr
    adc     .xDiv3
    bpl     .checkMod2

.not4
    bcs     .above4

    lsr
    bcs     .not2
; 2: x % 3 == 0
    lda     .xMod3
    beq     .invert
    bne     .skipInvert

.not2
    bne     .is3
; 1: y % 2 == 0
    txa
    bpl     .checkMod2

.is3
; 3: (x + y) % 3 == 0
    lda     .xMod3
;    sec
    sbc     .yMod3
    beq     .invert
    bne     .skipInvert

.above4
    cmp     #6
    beq     .is6
    bcs     .is7
.is6
    php
; 5: x * y % 2 + x * y % 3 == 0
; 6: (x * y % 2 + x * y % 3) % 2 == 0
    lda     .xMod3
    beq     .modEven56
    lda     .yMod3
    beq     .modEven56
    clc
    adc     .xMod3
    BIT_W
.modEven56
    lda     #0
    sta     .tmp
    tya
    lsr
    bcc     .even56
    txa
    lsr
    bcc     .even56
    inc     .tmp
.even56
    plp
    lda     .tmp
    bcs     .checkMod2
    beq     .invert
    bne     .skipInvert

.is7
; 7: ((x + y) % 2 + x * y % 3) % 2 == 0
    tya
    eor     .y
    sta     .tmp
    lda     .xMod3
    beq     .modEven7
    lda     .yMod3
    beq     .modEven7
    clc
    adc     .xMod3
    adc     .tmp
    sta     .tmp
.modEven7
    lda     .tmp
.checkMod2
    lsr
    bcs     .skipInvert
.invert
    jsr     InvertPixel
.skipInvert
; next X
    dec     .xMod3
    bpl     .xMod3OK
    lda     #2
    sta     .xMod3
    inc     .xDiv3
.xMod3OK
    dey
    bpl     .loopX
; next Y
    dec     .yMod3
    bpl     .yMod3OK
    lda     #2
    sta     .yMod3
.yMod3OK
    dex
    bmi     .exitLoopY
    jmp     .loopY

.exitLoopY
   ENDIF ; !QR_SINGLE_MASK
  ENDM

;-----------------------------------------------------------
  MAC _DRAW_FORMAT
;-----------------------------------------------------------
   IF QR_SINGLE_MASK = 0 || QR_DIRECT_DRAW = 0
.idx    = qrTmpVars

    ldy     #QR_FORMATS-1
.loopFormat
    sty     .idx
    cpy     #8
  IF QR_SINGLE_MASK
    ldx     #0
  ELSE
    ldx     qrPattern
  ENDIF
    lda     FormatLo,x
    and     BitMask,y
    bcc     .lowFormat
    lda     FormatHi,x
    and     BitMask-8,y
.lowFormat
   IF QR_DIRECT_DRAW
    beq     .skipFormat
   ELSE
    bne     .skipFormat
   ENDIF
    ldx     FormatY1,y
    lda     FormatX1,y
    tay
    jsr     InvertPixel
    ldy     .idx
    ldx     FormatY2,y
    lda     FormatX2,y
    tay
    jsr     InvertPixel
    ldy     .idx
.skipFormat
    dey
    bpl     .loopFormat
   ENDIF
  ENDM

; ********** The user macros start here: **********

;-----------------------------------------------------------
  MAC START_MSG
;-----------------------------------------------------------
; A = message length
; add mode and length to message data
    tax
    lsr
    lsr
    lsr
    lsr
    ora     #(QR_MODE << 4)
; (QR_MODE << 4) | (MSG_LEN >> 4)
    sta     qrMsgData + QR_MAX_DATA - 1
    txa
    asl
    asl
    asl
    asl
; (MSG_LEN << 4)
    sta     qrMsgData + QR_MAX_DATA - 2
    lda     #QR_MAX_DATA - 3
    sta     qrMsgIdx
; clear the remaining data buffer
    ldx     #QR_TOTAL-3
    lda     #0
.loopClear
    sta     qrData,x
    dex
    bpl     .loopClear
  ENDM

;---------------------------------------------------------------
  MAC ADD_MSG_BYTE
;---------------------------------------------------------------
; A = byte to add
    ldx     qrMsgIdx
    pha
    lsr
    lsr
    lsr
    lsr
    ora     qrMsgData + 1,x
    sta     qrMsgData + 1,x
    pla
    asl
    asl
    asl
    asl
    sta     qrMsgData,x
    dec     qrMsgIdx
  ENDM

;-----------------------------------------------------------
  MAC STOP_MSG
;-----------------------------------------------------------
   IF QR_PADDING
.msgLen     = qrTmpVars
; pad with optional filler bytes (QR code works without too)
    lda     #QR_MAX_MSG - 1
    sec
    sbc     .msgLen
    bcc     .noPadding
    tax
.loopPadding
    lda     #$ec            ; defined by QR standard
    sta     qrMsgData,x
    dex
    bmi     .noPadding
    lda     #$11            ; defined by QR standard
    sta     qrMsgData,x
    dex
    bpl     .loopPadding
.noPadding
   ENDIF
  ENDM

;-----------------------------------------------------------
  MAC GEN_QR_CODE
;-----------------------------------------------------------
; This is the main macro to use!
QRCodeCode

  IF QR_GENERATE
; generate the Reed-Solomon ECC generator polynomial
RSDevisor
    _RS_DIVISOR
  ENDIF
; calculate the ECC
RSRemainder
    _RS_REMAINDER
; draw the code words onto the bitmap
DrawCodes
    _DRAW_CODEWORDS
; apply the pattern mask
ApplyMask
    _APPLY_MASK
; blacken the function modules in the bitmap again
; and draw the function modules in the bitmap
DrawFunc
    _DRAW_FUNC
; draw the format bits
DrawFormat
    _DRAW_FORMAT

    ECHO    "QR Code encoding code:", [. - QRCodeCode]d, "bytes"
_QR_TOTAL SET _QR_TOTAL + . - QRCodeCode
  ENDM

;-----------------------------------------------------------
  MAC QR_CODE_DATA
;-----------------------------------------------------------
; Add this to your code's data area
QRCodeData

   IF QR_DIRECT_DRAW = 0
; Function pattern parameters
PatternX ; left
    .byte   6                       ; left, vertical timing line
    .byte   0, 0, QR_SIZE-8         ; eye pattern
    .byte   QR_SIZE-8, QR_SIZE-9    ; alignment pattern (right/middle part)
    .byte   9                       ; top, horizontal timing line
PatternY ; top
    .byte   QR_SIZE - 10            ; left, vertical timing line
    .byte   QR_SIZE-1, 7, QR_SIZE-1 ; eye pattern
    .byte   8, 8                    ; alignment pattern (right/middle part)
    .byte   QR_SIZE - 7             ; top, horizontal timing line
PatternOffset = . - 1
    .byte   EyeLeftTop-PatternStart     ; 1
    .byte   EyeLeftBottom-PatternStart  ; 2
    .byte   EyeRightTop-PatternStart    ; 3
    .byte   AlignRightBottom-PatternStart
    .byte   AlignRightBottom-PatternStart
; timing lines need no pattern, always drawn filled here
PatternWidth
    .byte   1                       ; left, vertical timing line
    .byte   9, 9, 8                 ; eye pattern
    .byte   4, 1                    ; alignment pattern (right/middle part)
;    .byte   QR_SIZE - 9 - 8         ; top, horizontal timing line
PatternHeight
    .byte   QR_SIZE - 9 - 8         ; left, vertical timing line
    .byte   9, 8, 9                 ; eye pattern
    .byte   5, 5                    ; alignment pattern (right/middle part)
    .byte   1                       ; top, horizontal timing line

PatternStart
EyeRightTop
    .byte   %11111110
    .byte   %10000010
    .byte   %10111010
    .byte   %10111010
    .byte   %10111010
    .byte   %10000010
    .byte   %11111110
    .byte   %00000000
    .byte   %11111111
AlignRightBottom = . - 1
;    .byte   %1111
    .byte   %1000
    .byte   %1010
    .byte   %1000
    .byte   %1111

EyeLeftBottom
    .byte   %00000000
;    .byte   %01111111
;    .byte   %01000001
;    .byte   %01011101
;    .byte   %01011101
;    .byte   %01011101
;    .byte   %01000001
;    .byte   %01111111
EyeLeftTop
    .byte   %01111111
    .byte   %01000001
    .byte   %01011101
    .byte   %01011101
    .byte   %01011101
    .byte   %01000001
    .byte   %01111111
    .byte   %00000000
    .byte   %11111111
   ENDIF ; /QR_DIRECT_DRAW

; Format Information Strings
  IF QR_SINGLE_MASK = 0 || QR_DIRECT_DRAW = 0
   IF QR_LEVEL = 0 ; L
FormatLo
    .byte   %11101111
    .byte   %11100101
    .byte   %11111011
    .byte   %11110001
    .byte   %11001100
    .byte   %11000110
    .byte   %11011000
    .byte   %11010010
FormatHi
    .byte   %10001000
    .byte   %11100110
    .byte   %01010100
    .byte   %00111010
    .byte   %01011110
    .byte   %00110000
    .byte   %10000010
    .byte   %11101100
   ENDIF
   IF QR_LEVEL = 1 ; M
FormatLo
    .byte   %10101000
    .byte   %10100010
    .byte   %10111100
    .byte   %10110110
    .byte   %10001011
    .byte   %10000001
    .byte   %10011111
    .byte   %10010101
FormatHi
    .byte   %00100100
    .byte   %01001010
    .byte   %11111000
    .byte   %10010110
    .byte   %11110010
    .byte   %10011100
    .byte   %00101110
    .byte   %01000000
   ENDIF
   IF QR_LEVEL = 2 ; Q
FormatLo
    .byte   %01101010
    .byte   %01100000
    .byte   %01111110
    .byte   %01110100
    .byte   %01001001
    .byte   %01000011
    .byte   %01011101
    .byte   %01010111
FormatHi
    .byte   %10111110
    .byte   %11010000
    .byte   %01100010
    .byte   %00001100
    .byte   %01101000
    .byte   %00000110
    .byte   %10110100
    .byte   %11011010
   ENDIF
   IF QR_LEVEL = 3 ; H
FormatLo
    .byte   %00101101
    .byte   %00100111
    .byte   %00111001
    .byte   %00110011
    .byte   %00001110
    .byte   %00000100
    .byte   %00011010
    .byte   %00010000
FormatHi
    .byte   %00010010
    .byte   %01111100
    .byte   %11001110
    .byte   %10100000
    .byte   %11000100
    .byte   %10101010
    .byte   %00011000
    .byte   %01110110
   ENDIF

; positions of the 15 type information bits
FormatX1
    .byte   0, 1, 2, 3, 4, 5, 7, 8
;    ds      7, 8           ; shared
FormatX2
    ds      7, 8
    .byte   QR_SIZE-8, QR_SIZE-7, QR_SIZE-6, QR_SIZE-5
    .byte   QR_SIZE-4, QR_SIZE-3, QR_SIZE-2, QR_SIZE-1
FormatY2
    .byte   0, 1, 2, 3, 4, 5, 6
;    ds      8, QR_SIZE-9   ; shared
FormatY1
    ds      8, QR_SIZE-9
    .byte   QR_SIZE-8, QR_SIZE-6, QR_SIZE-5, QR_SIZE-4
    .byte   QR_SIZE-3, QR_SIZE-2, QR_SIZE-1
  ENDIF ; /QR_SINGLE_MASK = 0 || QR_DIRECT_DRAW = 0

BitMask
    .byte   $80, $40, $20, $10, $8, $4, $2, $1

 IF QR_GENERATE = 0
Generator ; data in reversed order!
  IF QR_DEGREE = 7
    .byte   $75, $44, $0b, $a4, $9a, $7a, $7f
  ENDIF
  IF QR_DEGREE = 10
    .byte   $c1, $9d, $71, $5f, $5e, $c7, $6f, $9f
    .byte   $c2, $d8
  ENDIF
  IF QR_DEGREE = 13
    .byte   $87, $84, $53, $2b, $2e, $0d, $34, $11
    .byte   $b1, $11, $e3, $49, $89
  ENDIF
  IF QR_DEGREE = 15
    .byte   $1a, $86, $20, $97, $84, $8b, $69, $69
    .byte   $0a, $4a, $70, $a3, $6f, $c4, $1d
  ENDIF
  IF QR_DEGREE = 16
; Reed-Solomon ECC generator polynomial for degree 16
; g(x)=(x+1)(x+?)(x+?^2)(x+?^3)...(x+?^15)
; = x^16+3bx^15+0dx^14+68x^13+bdx^12+44x^11+d1x^10+1e^x9+08x^8
;   +a3x^7+41x^6+29x^5+e5x^4+62x^3+32x^2+24x+3b
    .byte   $3b, $24, $32, $62, $e5, $29, $41, $a3
    .byte   $08, $1e, $d1, $44, $bd, $68, $0d, $3b
  ENDIF
  IF QR_DEGREE = 17
    .byte   $4f, $63, $7d, $35, $55, $86, $8f, $29
    .byte   $f9, $53, $c5, $16, $77, $78, $53, $42
    .byte   $77
  ENDIF
  IF QR_DEGREE = 22
    .byte   $f5, $91, $1a, $e6, $da, $56, $fd, $43
    .byte   $7b, $1d, $89, $1c, $28, $45, $bd, $13
    .byte   $f4, $b6, $b0, $83, $b3, $59
  ENDIF
  IF QR_DEGREE = 26
    .byte   $5e, $2b, $4d, $92, $90, $46, $44, $87
    .byte   $2a, $e9, $75, $d1, $28, $91, $18, $ce
    .byte   $38, $4d, $98, $c7, $62, $88, $04, $b7
    .byte   $44, $f6
  ENDIF
  IF QR_DEGREE = 28
    .byte   $c5, $3a, $4a, $b0, $93, $79, $64, $b5
    .byte   $7f, $e9, $77, $75, $38, $f7, $0c, $a7
    .byte   $29, $64, $ae, $67, $96, $d0, $fb, $12
    .byte   $0d, $1c, $09, $fc
  ENDIF

QR_DEGREE = . - Generator  ; verify data size

 ENDIF ; /QR_GENERATE

    ECHO    "QR Code encoding data:", [. - QRCodeData]d, "bytes"
_QR_TOTAL SET _QR_TOTAL + . - QRCodeData

  ENDM