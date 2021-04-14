  MAC EOR_GFX
EorGfx
;GRP0LEor
    .byte   %00000010 | (({1} >> 7) & %1) ; constant, bit 0 of 2nd format copy, level
    .byte   %11111010 | (({1} >> 6) & %1) ; constant, bit 1 of 2nd format copy, level
    .byte   %10001010 | (({1} >> 5) & %1) ; constant, bit 2 of 2nd format copy, pattern
    .byte   %10001010 | (({1} >> 4) & %1) ; constant, bit 3 of 2nd format copy, pattern
    .byte   %10001010 | (({1} >> 3) & %1) ; constant, bit 4 of 2nd format copy, pattern
    .byte   %11111010 | (({1} >> 2) & %1) ; constant, bit 5 of 2nd format copy, ECC
    .byte   %00000010 | (({1} >> 1) & %1) ; constant, bit 6 of 2nd format copy, ECC
    .byte   %11111110 ; constant, 1 (dark module)
    .byte   %00000000
    .byte   %00000100
    .byte   %00000000
    .byte   %00000100
    .byte   %00000000
    .byte   %00000100
    .byte   %00000000
    .byte   %00000100
    .byte   (({1}<<1) & %11111000) | ({1} & %11) ; constant, bits 1..7 of 1st format copy
    .byte   %11111110 | (({2} >> 7) & %1) ; constant, bit  8 of 1st format copy, ECC
    .byte   %00000010 ; constant, 1 (timing bit)
    .byte   %11111010 | (({2} >> 6) & %1) ; constant, bit  9 of 1st format copy, ECC
    .byte   %10001010 | (({2} >> 5) & %1) ; constant, bit 10 of 1st format copy, ECC
    .byte   %10001010 | (({2} >> 4) & %1) ; constant, bit 11 of 1st format copy, ECC
    .byte   %10001010 | (({2} >> 3) & %1) ; constant, bit 12 of 1st format copy, ECC
    .byte   %11111010 | (({2} >> 2) & %1) ; constant, bit 13 of 1st format copy, ECC
    .byte   %00000010 | (({2} >> 1) & %1) ; constant, bit 14 of 1st format copy, ECC
;FirstEor
    .byte   %00000000
;GRP1Eor
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %10101010
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
;GRP0REor
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %11100000
    .byte   %10100000
    .byte   %11100000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   (({1} << 7) & $80) | ({2} >> 1) ; bits 7..14 of 2nd format copy
    .byte   %11111111 ; constant
    .byte   %10000000 ; constant
    .byte   %10111110 ; constant
    .byte   %10100010 ; constant
    .byte   %10100010 ; constant
    .byte   %10100010 ; constant
    .byte   %10111110 ; constant
    .byte   %10000000 ; constant
  ENDM

BlackGfx
LeftBlack
;GRP0LBlack
    .byte   %11111111 ; constant, bit 0 of 2nd format copy, level
    .byte   %11111111 ; constant, bit 1 of 2nd format copy, level
    .byte   %11111111 ; constant, bit 2 of 2nd format copy, pattern
    .byte   %11111111 ; constant, bit 3 of 2nd format copy, pattern
    .byte   %11111111 ; constant, bit 4 of 2nd format copy, pattern
    .byte   %11111111 ; constant, bit 5 of 2nd format copy, ECC
    .byte   %11111111 ; constant, bit 6 of 2nd format copy, ECC
    .byte   %11111111 ; constant, 1 (dark module)
    .byte   %00000100
    .byte   %00000100
    .byte   %00000100
    .byte   %00000100
    .byte   %00000100
    .byte   %00000100
    .byte   %00000100
    .byte   %00000100
    .byte   %11111111 ; constant, bits 1..7 of 1st format copy
    .byte   %11111111 ; constant, bit  8 of 1st format copy, ECC
    .byte   %11111111 ; constant, 1 (timing bit)
    .byte   %11111111 ; constant, bit  9 of 1st format copy, ECC
    .byte   %11111111 ; constant, bit 10 of 1st format copy, ECC
    .byte   %11111111 ; constant, bit 11 of 1st format copy, ECC
    .byte   %11111111 ; constant, bit 12 of 1st format copy, ECC
    .byte   %11111111 ; constant, bit 13 of 1st format copy, ECC
    .byte   %11111111 ; constant, bit 14 of 1st format copy, ECC
;FirstBlack
    .byte   %00000000
GRP1Black
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000001
    .byte   %00000001
    .byte   %00000001
    .byte   %00000001
    .byte   %00000001
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %11111111
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
GRP0RBlack
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %11110000
    .byte   %11110000
    .byte   %11110000
    .byte   %11110000
    .byte   %11110000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %11111111 ; constant, bits 7..14 of 2nd format copy
    .byte   %11111111 ; constant
    .byte   %11111111 ; constant
    .byte   %11111111 ; constant
    .byte   %11111111 ; constant
    .byte   %11111111 ; constant
    .byte   %11111111 ; constant
    .byte   %11111111 ; constant
    .byte   %11111111 ; constant

  IF QR_LEVEL = 0
    EOR_GFX ~%11101111, ~%10001000
  ENDIF
  IF QR_LEVEL = 1
    EOR_GFX ~%10101000, ~%00100100
  ENDIF
