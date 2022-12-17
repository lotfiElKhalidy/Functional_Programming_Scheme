;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Exercice 1 ;

(define qqs?
  (lambda (p L)
    (apply and (append-map p L)))) ; Cette version ne marche pas ; 

(define qqs?
  (lambda (p L)
    (if (null? L)
        #t
        (and (p (car L))
             (qqs? p (cdr L))))))

(qqs? (lambda (x) (= x 0)) '())
(qqs? (lambda (x) (= x 0)) '(0 0 0 0))

(define existe?
  (lambda (p L)
    (not (qqs? (lambda (x)
                 (not (p x)))
               L))))

(define existe?
  (lambda (p L)
    (if (null? L)
        #f
        (or (p (car L))
            (existe? p (cdr L))))))

(existe? (lambda (x) (= x 0)) '())
(existe? (lambda (x) (= x 0)) '(5 6 7 3 0))

(define tous_egaux
  (lambda (L)
    (if (null? L)
        #t
        (qqs? (lambda (x)
                (= x (car L)))
              L))))

(define tous_egaux
  (lambda (L)
    (if (null? L)
        #t
        (not (existe? (lambda (x)
                        (not (= x (car L))))
                      L))))) 

(tous_egaux '())
(tous_egaux '(2 4 2 2 2))
(tous_egaux '(2 2 2 2 2))

(define tous_diff
  (lambda (L)
    (if (null? L)
        #t
        (and (qqs? (lambda (x)
                     (not (= x (car L))))
                   (cdr L))
             (tous_diff (cdr L))))))

(define tous_diff
  (lambda (L)
    (if (null? L)
        #t
        (not (or (existe? (lambda (x)
                            (= x (car L)))
                          (cdr L))
                 (not (tous_diff (cdr L))))))))

(tous_diff '())
(tous_diff '(24 4 25 22 28))
(tous_diff '(2 2 2 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Exercice 2 ;

(define RS
  (lambda (f U0 n)
    (if (= n 0)
        U0
        (f (RS f U0 (- n 1)) n))))

(define RS*
  (lambda (n f n0 B)
    (if (= n n0)
        B
        (f (RS (- n 1) f n0 B) n))))

(define RS2
  (lambda (f U0 U1 n)
    (if (= n 0)
        U0
        (if (= n 1)
            U1
            (f (RS2 f U0 U1 (- n 1)) n)))))

(RS2 + 0 1 6)

(define somme_intnnl
  (lambda (n)
    (RS + 0 n)))

(somme_intnnl 0)
(somme_intnnl 3)

(define somm_carre
  (lambda (n)
    (RS (lambda (x y)
           (+ x (* y y)))
        0
        n)))

(somm_carre 0)
(somm_carre 4)

 ; Suite arithmétique ;
(define SA
  (lambda (n U0 raison)
    (RS  n
         U0
         (lambda (x y)
           (+ x raison)))))

(define fibo
  (lambda (n)
    (car (RS n
             (lambda (x y) (+ (car x) (cadr x)))
             1)
         '(1 1))))

(fibo 0)
(fibo 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Exercice 3 ;

(define reflexive
  (lambda (E R)
    (if (null? E)
        #t
        (if (null? (cdr E))
            #t
            (and (R (car E) (cadr E)) (reflexive (cdr E) R))))))

(define reflexive
  (lambda (E R)
    (if (null? E)
        #t
        (qqs? (lambda (x) (R x x)) E))))

(reflexive '(0 2 4 6)
           (lambda (x y)
             (= (modulo (+ x y) 2) 0)))

(define symetrique
  (lambda (E R)
    (qqs? (lambda (x)
            (qqs? (lambda (y)
                    (or (not (R x y)) (R y x))) E))
          E)))

(symetrique '(0 2 4 6)
           (lambda (x y)
             (= (modulo (+ x y) 2) 0)))

(define transitive
  (lambda (E R)
    (qqs? (lambda (x)
            (qqs? (lambda (y)
                    (qqs? (lambda (z)
                                  (or (not (and (R x y) (R y z)))
                                      (R x z)))
                          E))
                  E))
          E)))

(transitive '(0 2 4 6)
           (lambda (x y)
             (= (modulo (+ x y) 2) 0)))

(define insere
  (lambda (x L)
    (if (null? L)
        (list x)
        )))

(define quotient
  (lambda  (E R)
  (if (null? E)
      ()
      (insere (car L) (quotient (cdr L))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Exercice 4 ;

(define PC
  (lambda (L n)
    (if (= n 0)
	'(())  
	(append-map (lambda (x)
		      (map (lambda (L)
			     (cons x L))
			   (PC L (- n 1))))
		    L)))) 

(PC '() 3)
(PC '(0 1) 0)
(PC '(0 1) 1)
(PC '(0 1) 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Exercice 5 ;

(define chemin
  (lambda (dep but G)
    (cond
     ((null? G) ())
     ((= (caar G) dep) (cond
                        ((= (cadr G) but) (append (list dep but)
                                                  (chemin (cdar G) but G)))
                        (else (chemin dep but (car G)))))
     (else (chemin dep but (cdr G))))))

(chemin 'A 'B '((A (B))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Exercice 6 ;

(define trace
  (lambda (M)
    (letrec ((somme_list
           (lambda (L)
             (if (null? L)
                 0
                 (+ (car L) (somme_list (cdr L)))))))
      (if (null? M)
          0
          (+ (somme_list (car M)) (trace (cdr M)))))))

(trace '())
(trace '(() () (3 5 7)))
(trace '((2 1 3) (-1 3 -2) (1 0 2)))

(define transp
  (lambda (M)
    (map list (car M) (cadr M) (caddr M)))) ; Marche pour les matrices carées de
					; taille 3 ; 

(define transp
  (lambda (L)
    (append-map (lambda (L)
	   (list (car L)))
	 L)))

(define transp
  (lambda (L)
    (if (null? L)
	'(())
	(map (lambda (L)
	       (append-map (lambda (L)
			     (list (car L)))
			   L))
	     L))))

(transp '())
(transp '((11 21 31) (12 22 32) (13 23 33)))

(define MV
  (lambda (M V)
    (letrec ((colMV
              (lambda (Mi V)
                (if (null? Mi)
                    0
                    (+ (* (car Mi) (car V)) (colMV (cdr Mi) (cdr V)))))))
      (if (null? M)
        ()
        (cons (colMV (car M) V) (MV (cdr M) V))))))

(MV '() '(1 2 1))
(MV '((1 2 1) (0 1 2) (3 2 1)) '(1 0 1))

(define AL
  (lambda (M)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Exercice 7 ;

(define compose
  (lambda (f g)
    (let ((f
           (lambda (x)
             (f x)))
          (g
           (lambda (x)
             (g x))))
      (f (g x)))))

(define trace
  (lambda (f n)
    (if (zero? n)
          (list 'Id)
	  (cons (compose f f) (trace f (- n 1))))))

(define applique
  (lambda (Lf x)
    (if (null? Lf)
        ()
        (cons ((car Lf) x) (applique ((cdr Lf) x))))))

(applique (trace (lambda (x) (* x x) 3)) 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Exercice 8 ;

(define P
  (lambda (E)
    (if (null? E)
        (())
        (append (map (lambda (x)
                       (list x))
                     E)
                (append-map (lambda (x)
                              (map (lambda (y) (list x y))
                                   E))
                            E)
                (list E)))))

(P '(1 2 3 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Exercice 9 ;

(define P2
  (lambda (E)
    (let ((listeReduite
           (lambda (E x)
             (if (null? E)
                 ()
                 (if (= (car E) x)
                     (cdr E)
                     (cons (car E) (listeReduite (cdr E))))))))
      (append (map (lambda (x)
                     (map (lambda (y)
                            (list x))
                          (listeReduite (E (car E)))))
                   E)))))

(P2 '())
(P2 '(2 3 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					; Exercice 10 ;



