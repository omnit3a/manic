# manic
manic is a low-level programming langauge designed to do what C does, but with syntax that i like more.

# roadmap
- [ ] lexer (in progress)
- [ ] AST generation (in progress)
- [ ] compiler
    - [ ] machine code generation
     
# compiler architecture
the compiler will use racket for lexing and AST generation, and will then pass the generated AST to a c program, 
which will handle codegen and optimizations
