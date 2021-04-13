# 6502-QR-code-generator
A simple DASM assembler library for creating small QR codes

## Introduction
This project provides a simple QR code generator for the 650x family. 

Note: 
The code is mostly specifically taylored for the Atari 2600 and a version 2 QR code. Other platforms or QR code versions have **not** been tested. But it should be adaptable without too major problems.

If you make use of my code or have questions, please let me know.

## Features
- easy to use DASM macros
- assembler switches to taylor generator to your needs
- code size optimized for minimal RAM and ROM space 
- all eight mask pattern supported
- Atari 2600 demo code (randomly generates some Atari 2600 related messages)
- generator code for Reed-Solomon ECC generator polygons accompanied (can be integrated to compute on-the-fly)

## Limitations
- only small, single block QR codes supported
- only byte mode supported
- no automatic mask pattern evaluation
- tested only for version 2 QR codes (25x25)
- memory organization and pixel checking/drawing has to be implemented platform specific; this includes the finder, alignment and timing pattern
