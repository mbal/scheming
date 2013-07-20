(module prelude (take drop take-while drop-while flatten any? all?)
    (import r5rs)
    ;; take and drop
    (define (take n xs)
      (let loop ((n n) (xs xs) (acc '()))
        (if (or (null? xs) (= 0 n)) 
          (reverse acc)
          (loop (- n 1) (cdr xs) (cons (car xs) acc)))))

    (define (drop n xs)
      (if (or (null? xs) (= 0 n)) 
        xs
        (drop (- n 1) (cdr xs))))

    (define (take-while pred xs)
      (let loop ((xs xs) (acc '()))
        (if (or (null? xs) (not (pred (car xs))))
          (reverse acc)
          (loop (cdr xs) (cons (car xs) acc)))))

    (define (drop-while pred xs)
      (if (or (null? xs) (not (pred (car xs))))
          xs
          (drop pred (cdr xs))))

    (define (flatten xs)
      (cond ((null? xs) '())
            ((pair? xs) 
             (append (flatten (car xs)) (flatten (cdr xs))))
            (else (list xs))))

    (define (any? pred xs)
      (cond ((null? xs) #f)
            ((pred (car xs)) #t)
            (else (any? pred (cdr xs)))))

    (define (all? pred xs)
      (cond ((null? xs) #t)
            ((pred (car xs)) (all? pred (cdr xs)))
            (else #f)))
)


