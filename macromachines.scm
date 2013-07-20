(module macromachines
  (export syntax-rules+)
  (import r5rs)

  "Helper to define a new macro, using syntax-rules. It provides
  3 reserved words, that can be passed as argument to the macro in order
  to see the macro itself.
  <literals> - shows the literals of the macro
  <patterns> - shows the accepted patterns for this macro
  <expand> - expands the macro with the provided args.  "
  (define-syntax syntax-rules+
    (syntax-rules ()
                  ((syntax-rules+ (lit ...) ((name . args) body) ...)
                   (syntax-rules (<literals> <patterns> <expand> lit ...)
                                 ((_ <literals>) '(lit ...))
                                 ((_ <patterns>) '((name . args) ...))
                                 ((name <expand> . args) 'body) ...
                                 ((name . args) body) ...)))))
