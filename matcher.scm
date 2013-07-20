(module matcher (match match% match%%)
        (import r5rs)
  ;; ok. This version returns #f if the two things don't match.
  ;; WHAT     PATTERN    MATCH
  ;; (w 1)     (w a)       Y
  ;; (2 2)     (w a)       Y
  ;; (w 2)     (w 2)       N
  ;; (a b)     (a)         N
  (define (match what pattern)
    (cond ((and (pair? what) (pair? pattern))
           (and (match (car what) (car pattern))
                (match (cdr what) (cdr pattern))))
          ((and (null? what) (null? pattern)) #t)
          ((symbol? pattern) #t)
          (else #f)))

  ;; matches a pattern to an instantion. Needs two continuations, one
  ;; for the failure and one for the success. Returns a list of
  ;; dotted pair (var . matching).
  (define (match% what pattern sc fc)
    (cond ((and (pair? pattern) (pair? what))
           (match% (car what) (car pattern) 
                   (lambda (next) 
                     (match% (cdr what) (cdr pattern) 
                             (lambda (bindings) 
                               (sc (append next bindings))) 
                             fc))
                   fc))
          ((and (null? what) (null? pattern))
           ;; if the matching ended, we should just append the empty list
           (sc '()))
          ((variable? pattern) 
           (sc (list (cons pattern what))))
          (else (fc))))

  ;; returns #f if `what' doesn't match `pattern'. Returns an association list
  ;; otherwise. 
  (define (match%% what pattern)
    (define (helper what pattern acc)
      (cond ((and (pair? pattern) (pair? what))
             (let ((match-subexp (helper (car what) (car pattern) '())))
               (if match-subexp
                 (helper (cdr what) (cdr pattern)
                         (append acc match-subexp))
                 #f)))
            ((and (null? what) (null? pattern)) acc)
            ((variable? pattern)
             (cons (cons pattern what) acc))
            (else #f)))
    (helper what pattern '()))



  ;; point-free, anyone?
  (define variable? symbol?)

  (match '(w 1 2) '(w a b)))
