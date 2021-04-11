;===============================================================================
; Q R   C O D E   C O N S T A N T S
;===============================================================================

  IF QR_VERSION = 1 ;{
QR_SIZE     = 21    ; 21x21 QR code
QR_CAPACITY = 26
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
  ENDIF ;}

  IF QR_VERSION = 2
QR_SIZE     = 25    ; 25x25 QR code
QR_CAPACITY = 44
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

  IF QR_VERSION = 3 ;{
QR_SIZE     = 29    ; 29x29 QR code
QR_CAPACITY = 70
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
  ENDIF ;}

QR_MAX_DATA = QR_CAPACITY - QR_DEGREE

QR_MODE     = %0100 ; byte mode
QR_POLY     = $11d  ; GF(2^8) is based on 9 bit polynomial
                    ; x^8 + x^4 + x^3 + x^2 + 1 = 0x11d
QR_FORMATS  = 15    ; 15 type information bits

QR_MAX_MSG  = QR_MAX_DATA - 2
QR_TOTAL    = QR_MAX_DATA + QR_DEGREE ; 44


;===============================================================================
; Q R   C O D E   M A C R O S
;===============================================================================

; The following code has been partially converted from the C code of the
; QR code generator found at https://github.com/nayuki/QR-Code-generator

;-----------------------------------------------------------
  MAC _RS_REMAINDER
;-----------------------------------------------------------
.i      = tmpVars+2
.factor = tmpVars+3

; memset(result, 0, 16); // done in START_TEXT
; for (int i = dataLen-1; i >= 0; i--) {  // Polynomial division
    ldx     #QR_MAX_DATA-1
.loopI
    stx     .i
;   uint8_t factor = data[i] ^ result[degree - 1];
    lda     msgData,x
    eor     remainder + QR_DEGREE - 1
    sta     .factor
;   memmove(&result[1], &result[0], (size_t)(16 - 1) * sizeof(result[0]));
    ldx     #QR_DEGREE-1
.loopMove
    lda     remainder-1,x
    sta     remainder,x
    dex
    bne     .loopMove
;   result[0] = 0;
    lda     #0
    sta     remainder
;   for (int j = 16-1; j >= 0; j--)
    ldx     #QR_DEGREE-1
.loopJ
;     result[j] ^= reedSolomonMultiply(generator[j], factor);
    lda     Generator,x
    ldy     .factor
    _RS_MULT
    eor     remainder,x
    sta     remainder,x
;   }
    dex
    bpl     .loopJ
; }
    ldx     .i
    dex
    bpl     .loopI
  ENDM

;-----------------------------------------------------------
; Returns the product of the two given field elements modulo GF(2^8/0x11D).
; All inputs are valid.
  MAC _RS_MULT
;-----------------------------------------------------------
; Russian peasant multiplication (x * y)
; Input: A = x, Y = y
; Result: A
.x      = tmpVars
.y      = tmpVars+1

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

;-----------------------------------------------------------
; Draws the raw codewords (including data and ECC) onto the given QR Code. This requires the initial state of
; the QR Code to be black at function modules and white at codeword modules (including unused remainder bits).
  MAC _DRAW_CODEWORDS
;-----------------------------------------------------------
; Note: This part has the maximum RAM usage
.right1  = tmpVars+0
.vert   = tmpVars+1
.j      = tmpVars+2
.y      = tmpVars+3
.iBit   = tmpVars+4
.iByte  = tmpVars+5

; blacken the (right) function modules in the bitmap
  IF QR_OVERLAP
    _BLACK_RIGHT        ; returns with X = 0
  ELSE
    _BLACK_FUNC         ; returns with X = 0
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
;         bool black = getBit(data[i >> 3], 7 - (i & 7));
    ldx     .iByte
    lda     data,x
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
    bpl     .loopRight      ; unconditional!
; } // for right
.exitDraw
  ENDM

;-----------------------------------------------------------
  MAC _APPLY_MASK
;-----------------------------------------------------------
   IF QR_SINGLE_MASK
.y          = tmpVars
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
.y          = tmpVars
;.x      = tmpVars+2
.xMod3      = tmpVars+1
.yMod3      = tmpVars+2
.xDiv3      = tmpVars+3
.tmp        = tmpVars+4

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
  IF QR_SINGLE_MASK = 0
.idx    = tmpVars

    ldy     #QR_FORMATS-1
.loopFormat
    sty     .idx
    cpy     #8
;  IF QR_SINGLE_MASK
;    lda     #%10101000
;    and     BitMask,y
;    bcc     .lowFormat
;    lda     #%00100100
;  ELSE
    ldx     qrPattern
    lda     FormatLo,x
    and     BitMask,y
    bcc     .lowFormat
    lda     FormatHi,x
;  ENDIF
    and     BitMask-8,y
.lowFormat
    beq     .skipFormat
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
    sta     msgData + QR_MAX_DATA - 1
    txa
    asl
    asl
    asl
    asl
; (MSG_LEN << 4)
    sta     msgData + QR_MAX_DATA - 2
    lda     #QR_MAX_DATA - 3
    sta     msgIdx
; clear the remaining data buffer
    ldx     #QR_TOTAL-3
    lda     #0
.loopClear
    sta     data,x
    dex
    bpl     .loopClear
  ENDM

;---------------------------------------------------------------
  MAC ADD_MSG_BYTE
;---------------------------------------------------------------
; A = byte to add
    ldx     msgIdx
    pha
    lsr
    lsr
    lsr
    lsr
    ora     msgData + 1,x
    sta     msgData + 1,x
    pla
    asl
    asl
    asl
    asl
    sta     msgData,x
    dec     msgIdx
  ENDM

;-----------------------------------------------------------
  MAC STOP_MSG
;-----------------------------------------------------------
   IF QR_PADDING
.msgLen     = tmpVars
; pad with optional filler bytes (QR code works without too)
    lda     #QR_MAX_MSG - 1
    sec
    sbc     .msgLen
    bcc     .noPadding
    tax
.loopPadding
    lda     #$ec            ; defined by QR standard
    sta     msgData,x
    dex
    bmi     .noPadding
    lda     #$11            ; defined by QR standard
    sta     msgData,x
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

; Format Information Strings
  IF QR_SINGLE_MASK = 0
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
; TODO: levels Q and H

; position of the 15 type information bits
FormatX1
    .byte   0, 1, 2, 3, 4, 5, 7, 8
    .byte   8, 8, 8, 8, 8, 8, 8
FormatY2
    .byte   0, 1, 2, 3, 4, 5, 6
;    ds      8, QR_SIZE-9   ; shared
FormatY1
    ds      8, QR_SIZE-9
    .byte   QR_SIZE-8, QR_SIZE-6, QR_SIZE-5, QR_SIZE-4
    .byte   QR_SIZE-3, QR_SIZE-2, QR_SIZE-1
FormatX2
    ds      7, 8
    .byte   QR_SIZE-8, QR_SIZE-7, QR_SIZE-6, QR_SIZE-5
    .byte   QR_SIZE-4, QR_SIZE-3, QR_SIZE-2, QR_SIZE-1
  ENDIF ; QR_SINGLE_MASK = 0

BitMask
    .byte   $80, $40, $20, $10, $8, $4, $2, $1

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

QR_DEGREE = . - Generator  ; verify data

    ECHO    "QR Code encoding data:", [. - QRCodeData]d, "bytes"
_QR_TOTAL SET _QR_TOTAL + . - QRCodeData

  ENDM