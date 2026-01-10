; compiler.rkt
; <insert notes here>
; 
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

(generate-ast (lex paren-test))
;(parse-ast '())
