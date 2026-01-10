; main.rkt
; runs all the necessary racket code and passed
; the output to the C section of the compiler
; 
; created: 09/01/2026

#lang racket

(require "parser.rkt")
(require "lexer.rkt")

(define string-to-parse
    "add: num (x: imut num, y: imut num) {
       a : fixed <- 10
       return x+y
     }")

(define paren-test
  "()(())((()))")

(define lexer-output (lex string-to-parse))
(define nesting-balance (lexer-verify-nesting lexer-output))
(cond
  [(eq? #t nesting-balance)
   (printf "[NESTING] PASSED\n")]
  [(< 0 nesting-balance)
   ((printf "[NESTING] ERROR\n")
    (raise 'unbalanced-left-side-nesting #f))]
  [(> 0 nesting-balance)
   ((printf "[NESTING] ERROR\n")
    (raise 'unbalanced-right-side-nesting #f))])
