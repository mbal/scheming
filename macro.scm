(define-syntax parse-tokens
  (syntax-rules (=>)
    ((parse-tokens "grouped" ((c => return) ...))
     (lambda (x) (cond ((equal? x c) return) ...)))))

(define-syntax group3
  (syntax-rules ()
    ((group3 "1" acc ()) (parse-tokens "grouped" (acc)))
    ((group3 "2" acc ()) (parse-tokens "grouped" acc))
    ((group3 "1" (a b c))
     (group3 "1" (a b c) ()))
    ((group3 "1" (a b c rest ...))
     (group3 "2" (a b c) (rest ...)))
    ((group3 "2" prev (a b c rest ...))
     (group3 "2" (prev . ((a b c))) (rest ...)))
    ((group3 a b c other ...)
     (group3 "1" (a b c other ...)))))

(define-syntax dispatcher
  (syntax-rules (=>)
    ((dispatcher name f return ...)
     (define name
       (let ((o f))
         (succeed ((group3 return ...) o)))))))

;(dispatcher fun value
;            case-1 => return1
;            case-2 => return2
;            case-3 => return3)
;

