;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define est_croissante
  (lambda (L)
    (let ((compare
           (lambda (x y)
             (if (< x y) #t #f))))
      (cond
       ((null? L) #t)
       ((null? (cdr L)) #t)
       (else (compare (car L) (est_croissante (cdr L))))))))

(est_croissante '())
(est_croissante '(1))
(est_croissante '(1 3 6 8 9))
(est_croissante '(1 4 5 6 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
