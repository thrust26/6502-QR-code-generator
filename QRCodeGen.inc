; The following code has been partially converted from the C code of the
; QR code generator found at https://github.com/nayuki/QR-Code-generator

;-----------------------------------------------------------
  MAC _RS_REMAINDER
;-----------------------------------------------------------
.i      = tmpVars+2
.factor = tmpVars+3

; memset(result, 0, 16); // done in START_TEXT
; for (int i = dataLen-1; i >= 0; i--) {  // Polynomial division
    ldx     #MAX_DATA-1
.loopI
    stx     .i
;   uint8_t factor = data[i] ^ result[degree - 1];
    lda     msgData,x
    eor     remainder + DEGREE - 1
    sta     .factor
;   memmove(&result[1], &result[0], (size_t)(16 - 1) * sizeof(result[0]));
    ldx     #DEGREE-1
.loopMove
    lda     remainder-1,x
    sta     remainder,x
    dex
    bne     .loopMove
;   result[0] = 0;
    lda     #0
    sta     remainder
;   for (int j = 16-1; j >= 0; j--)
    ldx     #DEGREE-1
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
    eor     #<POLY
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
.right  = tmpVars+0
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
; 2600 code has data in reversed order
    stx     .iBit
    lda     #TOTAL_LEN-1
    sta     .iByte
; // Do the funny zigzag scan
; for (int right = qrsize - 1; right >= 1; right -= 2) {  // Index of right column in each column pair
    ldy     #QR_SIZE - 1
.loopRight
;  if (right == 6)
    cpy     #6
    bne     .not6
;    right = 5;
    dey                 ; skip the timing column
.not6
    sty     .right
  IF QR_OVERLAP
; overwrite shared data
    cpy     #16
    bne     .skipBlackMiddle
; blacken the middle function modules in the bitmap
    _BLACK_MIDDLE
.skipBlackMiddle
    cpy     #8
    bne     .skipBlackLeft
; blacken the left function modules in the bitmap
    _BLACK_LEFT
.skipBlackLeft
  ENDIF
;   for (int vert = 0; vert < qrsize; vert++) {  // Vertical counter
    ldy     #QR_SIZE-1
.loopVert
    sty     .vert
;     for (int j = 0; j < 2; j++) {
    ldy     #-1
.loopJ
    iny
    sty     .j
;       bool upward = ((right + 1) & 2) != 0; // 2600 code works in reverse
    ldx     .vert
    lda     .right
    clc
    adc     #1
    and     #$2
    bne     .notUp
;       int y = upward ? qrsize - 1 - vert : vert;  // Actual y coordinate
    lda     #QR_SIZE-1
    sec
    sbc     .vert
    tax
.notUp
    stx     .y
;       int x = right - j;  // Actual x coordinate
    lda     .right
    sec
    sbc     .j
;    sta     .x
    tay
;       if (!getModule(qrcode, x, y) && i < dataLen * 8) {
;    ldy     .x
;    ldx     .y
    jsr     CheckPixel
    bne     .skipPixel
    ldx     .iByte
    bmi     .skipPixel
;         bool black = getBit(data[i >> 3], 7 - (i & 7));
    lda     data,x
    ldx     .iBit
    and     BitMask,x
    beq     .skipInv
;         setModule(qrcode, x, y, black);
;    ldy     .x
    ldx     .y
    jsr     InvertPixel
.skipInv
;         i++;
    ldy     .iBit
    iny
    tya
    and     #$07
    sta     .iBit
    bne     .skipByte
    dec     .iByte
.skipByte
;       }
.skipPixel
    ldy     .j
    beq     .loopJ
;     } // for j
    ldy     .vert
    dey
    bpl     .loopVert
;   } // for vert
    ldy     .right
    dey
    dey
    bpl     .loopRight
; } // for right
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
.idx    = tmpVars

    ldy     #NUM_FORMAT-1
.loopFormat
    sty     .idx
    cpy     #8
  IF QR_SINGLE_MASK
    lda     #%10101000
    and     BitMask,y
    bcc     .lowFormat
    lda     #%00100100
  ELSE
    ldx     qrPattern
    lda     FormatLo,x
    and     BitMask,y
    bcc     .lowFormat
    lda     FormatHi,x
  ENDIF
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
    ora     #(MODE << 4)
; (MODE << 4) | (MSG_LEN >> 4)
    sta     msgData + MAX_DATA - 1
    txa
    asl
    asl
    asl
    asl
; (MSG_LEN << 4)
    sta     msgData + MAX_DATA - 2
    lda     #MAX_DATA - 3
    sta     msgIdx
; clear the remaining data buffer
    ldx     #TOTAL_LEN-3
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
    lda     #MAX_MSG - 1
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
  ENDIF

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

BitMask
    .byte   $80, $40, $20, $10, $8, $4, $2, $1

Generator ; data in reversed order!
  IF DEGREE = 7
    .byte   $75, $44, $0b, $a4, $9a, $7a, $7f
  ENDIF
  IF DEGREE = 10
    .byte   $c1, $9d, $71, $5f, $5e, $c7, $6f, $9f
    .byte   $c2, $d8
  ENDIF
  IF DEGREE = 15
    .byte   $1a, $86, $20, $97, $84, $8b, $69, $69
    .byte   $0a, $4a, $70, $a3, $6f, $c4, $1d
  ENDIF
  IF DEGREE = 16
; Reed-Solomon ECC generator polynomial for degree 16
; g(x)=(x+1)(x+?)(x+?^2)(x+?^3)...(x+?^15)
; = x^16+3bx^15+0dx^14+68x^13+bdx^12+44x^11+d1x^10+1e^x9+08x^8
;   +a3x^7+41x^6+29x^5+e5x^4+62x^3+32x^2+24x+3b
    .byte   $3b, $24, $32, $62, $e5, $29, $41, $a3
    .byte   $08, $1e, $d1, $44, $bd, $68, $0d, $3b
  ENDIF
  IF DEGREE = 26
    .byte   $5e, $2b, $4d, $92, $90, $46, $44, $87
    .byte   $2a, $e9, $75, $d1, $28, $91, $18, $ce
    .byte   $38, $4d, $98, $c7, $62, $88, $04, $b7
    .byte   $44, $f6
  ENDIF
DEGREE = . - Generator  ; verify data

    ECHO    "QR Code encoding data:", [. - QRCodeData]d, "bytes"
_QR_TOTAL SET _QR_TOTAL + . - QRCodeData

  ENDM