; analyze.rkt
; code for lexing
; 
; 
; created: 08/01/2026

(module lexer racket
  (require parser-tools/lex)
  (require (prefix-in : parser-tools/lex-sre))

  ; verify that nesting is balanced. returns #t if balanced.
  ; if nesting is uneven on the left side, return a positive number
  ; if nesting is uneven on the right side, return a negative number
  (provide lexer-verify-nesting)
  (define (lexer-verify-nesting lexemes)
    (define nest-sum 0)
    (for ([i (in-range (length lexemes))])
      (cond
        [(or (or (equal? 'LPAR (car (list-ref lexemes i)))
                 (equal? 'RPAR (car (list-ref lexemes i))))
             (or (equal? 'LBRACE (car (list-ref lexemes i)))
                 (equal? 'RBRACE (car (list-ref lexemes i)))))
         (set! nest-sum (+ nest-sum (car (cdr (list-ref lexemes i)))))])
      )
    (cond
      [(= 0 nest-sum)
       ((printf "[NESTING] PASSED\n")
        #t)]
      [(< 0 nest-sum)
       (raise 'unbalanced-left-side-nesting #f)]
      [(> 0 nest-sum)
       (raise 'unbalanced-right-side-nesting #f)]))
  
  (provide lex)
  (define (lex input)
    ; grammar
    ; comments: "--"
    ; identifiers: [a-zA-Z\-]+
    ; parens: "(" | ")" | "{" | "}"
    ; integers: -?[0-9]+
    ; floats: -?[0-9]+\.[0-9]+
    ; fixed points: -?[0-0]+\.[0-9]+"f"
    ; bools: "true" | "false"
    ; typenames: "num" | "float" | "fixed" | "bool"
    ; literal: integer | float | fixed | bool
    ; operators: "+" | "-" | "*" | "/"
    ; assignment: "<-"
    ; seperators: ","
    ; qualifiers: "imut"
    ; whitespace: [ \n]+a

    (define-lex-abbrevs
      [identifier
       (:+ (:or (:or (char-range #\a #\z) (char-range #\A #\Z)) #\-))]

      [integer
       (:: (:? #\-) (:+ (char-range #\0 #\9)))]

      [float
       (:: (:? #\-) (:+ (char-range #\0 #\9)) (:? #\.) (:+ (char-range #\0 #\9)))]

      [fixed
       (:: float #\f)]

      [bool
       (union "true" "false")]

      [typename
       (union "num" "real" "fixed" "bool")]
      
      [literal
       (union integer float fixed bool)]
      
      [operator
       (union "+" "-" "*" "/")]

      [qualifier
       (union "imut" "func")]
      
      [assignment
       "<-"]

      [typecast
       #\:]

      [lpar
       #\(]

      [rpar
       #\)]

      [lbrace
       #\{]

      [rbrace
       #\}]

      [seperator
       #\,]
      
      [comment
       "--"])

    (define (strip-last-char str)
      (list->string (reverse (cdr (reverse (string->list str))))))

    (define paren-depth 1)
    (define brace-depth 1)
    (define prev-paren-depth 1)
    (define prev-brace-depth 1)

    (define (renumber-paren depth)
      (set! paren-depth depth)
      (set! prev-paren-depth (max 1 (- depth 1)))
      `,prev-paren-depth)

    (define (renumber-brace depth)
      (set! brace-depth depth)
      (set! prev-brace-depth (max 1 (- depth 1)))
      `,prev-brace-depth)
    
    (define interior-lex      
      (lexer
       [comment
        (comment-lex input-port)]
       
       ; parens
       ; "(" | ")" | "{" | "}"
       [lpar
        (cons `(LPAR ,(renumber-paren (+ paren-depth 1)))
              (interior-lex input-port))]
       
       [rpar
        (cons `(RPAR ,(* -1 prev-paren-depth))
              (handle-paren input-port))]
       
       [lbrace
        (cons `(LBRACE ,(renumber-brace (+ brace-depth 1)))
              (interior-lex input-port))]
       
       [rbrace
        (cons `(RBRACE ,(* -1 prev-brace-depth))
              (handle-brace input-port))]
       
       ; integers
       ; -?[0-9]+
       [integer
        (cons `(INT ,(string->number lexeme))
              (interior-lex input-port))]
       
       ; floats
       ; -?[0-9.0-9]+
       [float
        (cons `(FLOAT ,(string->number lexeme))
              (interior-lex input-port))]
       
       [fixed
        (cons `(FIXED ,(string->number (strip-last-char lexeme)))
              (interior-lex input-port))]
       
       [bool
        (cons `(BOOL ,(string->symbol lexeme))
              (interior-lex input-port))]
       
       ; operators
       ; "+" | "-" | "*" | "/"
       [operator
        (cons `(OP ,(string->symbol lexeme))
              (interior-lex input-port))]
       
       [typecast
        (cons `(CAST)
              (interior-lex input-port))]
       
       [typename
        (cons `(TYPE ,(string->symbol lexeme))
              (interior-lex input-port))]
       
       [assignment
        (cons `(ASSIGN)
              (interior-lex input-port))]
       
       [qualifier
        (cons `(QUALIFIER ,(string->symbol lexeme))
              (interior-lex input-port))]
       
       ; identifiers
       ; [a-zA-Z\-]+
       [identifier
        (cons `(ID ,(string->symbol lexeme))
              (interior-lex input-port))]
       
       [seperator
        (cons `(SEP)
              (interior-lex input-port))]
       
       ; whitespace
       ; [ \n]+
       [whitespace
        (interior-lex input-port)]
       
       ; eof
       [(eof)
        '()]))

      (define comment-lex
        (lexer
         ; end comment
         [#\newline
          (interior-lex input-port)]
         
         [any-char
          (comment-lex input-port)]
         ))

    (define (handle-paren input)
      (renumber-paren prev-paren-depth)
      (interior-lex input))

    (define (handle-brace input)
      (renumber-brace prev-brace-depth)
      (interior-lex input))
    
    (interior-lex (open-input-string input))
    )  
)
