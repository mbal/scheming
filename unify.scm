;; Given the two formulae:
;; a = ?b and c = ?b
;; we can unify to get that c = a and a = ?b (or c = ?b)
;; g(?a, f(?a)) = g(?b, ?b) unifies to
;; ?a = ?b and f(?a) = ?b ==> f(?b) = ?b
;;
;; Unification is quite similar to the pattern matching performed
;; by the scheme syntax-rules macros, with the introduction of 
;; variables.

;; a variable is a symbol that starts with ?
(require-extension srfi-13)

(define (variable? x)
  (and (symbol? x) (string-prefix? "?" (symbol->string x))))

(define bind cons)
(define constant? atom?)

(define bind-value cdr)

(define (binding-in-environment var env)
  (let loop ((xs env))
    (cond ((null? xs) #f)
          ((equal? var (caar xs)) (car xs))
          (else (loop (cdr xs))))))

;; tries to unify a variable with a value.
;; If both var and value are equal (both the same variable), the unification is
;; trivial (basically ?x = ?x), so we don't even put it in the environment
;;
;; if the variable have already a binding, we search it in the environment
;; and perform unification between the previous binding and the new one.
;;
;; f(?x, g(?x)) and f(?y, ?y) should not be allowed, since it provides
;; a bogus unification, in which ?y = ?x and ?y = f(?x), so ?x = f(?x):
;; sometimes is valid, but not in general, and it evolves in an infinite
;; term. In order to delete this possibility, we ought to check, if the
;; previous binding is free with respect to the variable.
;; In our previous example, ?y = f(?x) is not free for the variable ?x
;; (and, of course, ?y = ?x is not free either for the variable x)

;; a variable is free in an expression `e' (and an environment) when:
;; 1. e is a constant
;; 2. e is a variable and:
;;      2.1. is not bound or
;;      2.2. is bound but the variable is free in the value of e
;; 3. e is a complex expression and:
;;      3.1. the var is free in the car and in the cdr
;; In all the other cases, var is bound in e.

(define (free? var expr env)
  (cond ((variable? expr)
         (if (equal? var expr) 
           #f
           (let ((result (binding-in-environment expr env)))
             (if (not result)
               #t
               (free? var (bind-value result) env)))))
        ((constant? expr) #t)
        ((free? var (car expr) env) (free? var (cdr expr) env))
        (else #f)))


(define (unify-variable var what env)
  (if (equal? var what)
    env
    (let ((result (binding-in-environment var env)))
      (if (not result)
        (cons (bind var (list what)) env)
        (if (free? var what env)
          (unify (bind-value result)
                 what
                 env)
          (error "can't unify: infinite expansion"))))))

(define (unify f g acc)
  (cond ((and (pair? f) (pair? g))
         (unify (cdr f) (cdr g) (unify (car f) (car g) acc)))
        ;; a variable matches with anything
        ((variable? f) (unify-variable f g acc))
        ((variable? g) (unify-variable g f acc))
        ((and (constant? f) (constant? g))
         (if (equal? f g)
           acc
           (error "couldn't unify " f g)))
        (else (error "couldn't unify"))))


;; what can we do with a unification algorithm?
;; 1. type system. Given
;;      f :: ?a -> ?a -> ?a
;;      and ?result = f(3, ?x)
;;      we can infer that ?a == type(3), ?a == type(?x) and that
;;      ?a == type(?result). ?a is an INT, from which it follows
;;      that both ?x and ?result must be integer to.
;; 2. solve constraints problem
