Simple compiler written in Scala that compiles dialect of BASIC to C.

Language supports:
- Numerical variables
- Basic arithmetic
- If statements
- While loops
- Print text and numbers
- Input numbers
- Labels and goto
- Comments

Example code:

    # Compute average of given values.
    
    LET a = 0
    WHILE a < 1 REPEAT
    PRINT "Enter number of scores: "
    INPUT a
    ENDWHILE
    
    LET b = 0
    LET s = 0
    PRINT "Enter one value at a time: "
    WHILE b < a REPEAT
    INPUT c
    LET s = s + c
    LET b = b + 1
    ENDWHILE

    PRINT "Average: "
    PRINT s / a

What I learned while this project:
- [Lexical analysis](https://en.wikipedia.org/wiki/Lexical_analysis)
- [Syntax analysis](https://en.wikipedia.org/wiki/Parsing)
- [Abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
- [Semantic analysis](https://en.wikipedia.org/wiki/Semantic_analysis_(compilers))
- [Code generation](https://en.wikipedia.org/wiki/Code_generation_(compiler))