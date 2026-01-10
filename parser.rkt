; parser.rkt
; code for AST generation and parsing
; 
; 
; created: 09/01/2026

(module parser racket
  (provide generate-ast)
  (define (generate-ast lexemes)
    (if (not (null? lexemes))
        ((cond
           [(equal? 'LPAR (car (car lexemes)))
            (printf "LPAR: ~a\n" (car (cdr (car lexemes))))]
           
           [(equal? 'RPAR (car (car lexemes)))
            (printf "RPAR: ~a\n" (car (cdr (car lexemes))))]
           [else
            (printf "~a\n" (car lexemes))])
         (generate-ast (cdr lexemes)))
        `())
    (raise 'not-implemented #f))

  (provide parse-ast)
  (define (parse-ast ast)
    (raise 'not-implemented #f))
)
