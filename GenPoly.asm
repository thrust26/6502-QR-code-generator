; Generate ECC Reed-Solomon ECC generator polygon for given degree

    processor 6502

BASE_ADR    = $f800

DEGREE      = 26
POLY        = $11d  ; GF(2^8) is based on 9 bit polynomial
                    ; x^8 + x^4 + x^3 + x^2 + 1 = 0x11d


;===============================================================================
; Z P - V A R I A B L E S
;===============================================================================

    SEG.U   variables
    ORG     $80

tmpVars     ds 4

    ALIGN   16

result      ds DEGREE


;===============================================================================
; R O M - C O D E
;===============================================================================
    SEG     Bank0
    ORG     BASE_ADR

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

    RS_DIVISOR
.wait
    jmp     .wait


; Computes a Reed-Solomon ECC generator polynomial for degree 16, storing in result[0 : 16-1].
; This is now implemented as a lookup table (Generator)
; g(x)=(x+1)(x+?)(x+?^2)(x+?^3)...(x+?^15)
; = x^16+3Bx^15+0Dx^14+68x^13+BDx^12+44x^11+d1x^10+1e^x9+08x^8
;   +A3x^7+41x^6+29x^5+E5x^4+62x^3+32x^2+24x+3B
  MAC RS_DIVISOR
.root   = tmpVars+2
.i      = tmpVars+3

; memset(result, 0, 16);
    ldx     #DEGREE-2
    ldy     #0
.loopClear
    sty     result,x
    dex
    bpl     .loopClear
; result[16 - 1] = 1;  // Start off with the monomial x^0
    iny
    sty     result + DEGREE - 1
; uint8_t root = 1;
    sty     .root
; for (int i = 0; i < 16; i++) {
    lda     #DEGREE-1   ; just loop 16 times
    sta     .i
.loopI
; // Multiply the current product by (x - r^i)
; for (int j = 0; j < 16; j++) {
    ldx     #0
.loopJ
; result[j] = reedSolomonMultiply(result[j], root);
    lda     result,x
    ldy     .root
;    RS_MULT
    jsr     RSMult
; if (j != 16 - 1)
    cpx     #DEGREE - 1
    bcs     .skipEor
; result[j] ^= result[j + 1];
    eor     result+1,x
.skipEor
    sta     result,x
    inx
    cpx     #DEGREE
    bcc     .loopJ
; root = reedSolomonMultiply(root, 0x02);
    lda     .root
    ldy     #$02
;    RS_MULT
    jsr     RSMult
    sta     .root
    dec     .i
    bpl     .loopI
  ENDM

; Returns the product of the two given field elements modulo GF(2^8/0x11D).
; All inputs are valid.
RSMult SUBROUTINE
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
; z = (uint8_t)((z << 1) ^ ((z >> 7) * 0x11D));
    asl
    bcc     .skipEorPoly
    eor     #<POLY
.skipEorPoly
; z ^= ((y >> i) & 1) * x;
    asl     .y
    bcc     .skipEorX
    eor     .x
.skipEorX
    dey
    bpl     .loopI
    rts

    .byte   "JTZ"

    org     BASE_ADR + $7fc
    .word   Start
    .word   Start
