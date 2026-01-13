; main.rkt
; runs all the necessary racket code and passed
; the output to the C section of the compiler
; 
; created: 09/01/2026

#lang racket

(require "parser.rkt")
(require "lexer.rkt")

(define string-to-parse
    "add: func num (x: imut num y: imut num) {
       a : fixed <- 10
       return x+y
     }")

(define paren-test
  "()(())((()))")

(define var-decl-test
  "a: num
   b: num <- 10
   c: float
   d: float <- 10.0
   e: fixed
   f: fixed <- 10.0f
   g: bool
   h: bool <- true")

(define lexer-output (lex string-to-parse)
(lexer-verify-nesting lexer-output)
;(generate-ast lexer-output 0)
