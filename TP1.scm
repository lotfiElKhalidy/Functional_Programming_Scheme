;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;1. Puissance Quatre : quad ;

(define quad
  (lambda (n)
    (let ((carree
           (lambda (n)
             (* n n))))
      (* (carree n) (carree n)))))

(quad 0)
(quad 2)
(quad 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;2. Circonference et surface d'un cercle : cercle ;

(define cercle
  (lambda (r)
    (append (cons (* 3.14 (* 2 r)) ())
            (cons (* 3.14 (* r r)) ())))) 
; Here is a much easier way to do it ;

(define cercle_easy
  (lambda (r)
    (list (* 3.14 (* 2 r)) (* 3.14 (* r r)))))

(cercle 0)
(cercle 1)
(cercle 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;3. Factoriellle : fact ;

(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))

(fact 0)
(fact 2)
(fact 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
;4. Somme des entiers non nuls d'une liste : somme_int;

(define somme_int
  (lambda (n)
    (if (zero? n)
        0
        (+ n (somme_int (- n 1))))))

(somme_int 0)
(somme_int 3)
(somme_int 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;5. Longueur de la liste : Long;

(define long
  (lambda (L)
    (if (null? L)
        0
        (+ 1 (long (cdr L))))))

(long '())
(long '(1))
(long '(1 9 987986 9876875 98756867 4321453))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;6. Liste renversée : mirroir ;

(define mirroir
  (lambda (L)
    (if (null? L)
        ()
        (append (mirroir (cdr L)) (list (car L))))))

(mirroir '())
(mirroir '(1))
(mirroir '(1 5 7 9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;7. Liste des carrées : carre ;

(define carre
  (lambda (L)
    (let ((carree
           (lambda (x) (* x x))))
      (if (null? L)
        ()
        (append (list (carree (car L))) (carre (cdr L)))))))

; I feel like, in this case, this code is much better ;

(define carre
  (lambda (L)
    (if (null? L)
        ()
        (cons (* (car L) (car L)) (carre (cdr L))))))

(carre '())
(carre '(3))
(carre '(1 2 3 4 5 6 7 8 9 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;8. Le nombre des entiers positifs dans une liste : nbpos;

(define nbpos
  (lambda (L)
    (let ((pos
           (lambda (n)
             (if (> n 0) 1 0))))
      (if (null? L)
          0
          (+ (pos (car L)) (nbpos (cdr L)))))))

; Another way to do it ;

(define nbpos
  (lambda (L)
    (if (null? L)
        0
        (if (> (car L) 0)
            (+ 1 (nbpos (cdr L)))
            (nbpos (cdr L))))))

(nbpos '())
(nbpos '(-1))
(nbpos '(1 -5 6 8 -9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;9. x est membre ou non de L : membre;

(define membre
  (lambda (x L)
    (if (null? L)
        #f ; (display "Erreur") affiche message d'erreur ;
        (if (equal? x (car L))
            #t
            (membre x (cdr L))))))

(membre 4 '())
(membre 7 '(1 26 45 2))
(membre 8 '(1 3 8 43 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;10. Liste sans double : epure;

(define epure
  (lambda (L)
    (if (null? L)
        ()
        (if (membre (car L) (cdr L))
            (epure (cdr L))
            (cons (car L) (epure (cdr L)))))))

(epure '())
(epure '(1 98))
(epure '(1 4 76 3 5 1 3 4 2 2))

; Ce n'est pas la solution optimale : Complexité = n**2/2
; La solution optimale c'est de trier la liste ( complexité = n*log(n) ) et puis
; supprimer les doublons 
; complexité = n ; parce qu'on ne parcoure la liste qu'une seule fois ;
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;11. Le n i ème élément de la liste : nieme;

(define nieme
  (lambda (n L)
    (if (null? L)
        ()
        (if (= n 1)
            (car L)
            (nieme (- n 1) (cdr L))))))

(nieme 4 '())
(nieme 2 '(4))
(nieme 3 '(2 7 9 3))

; (append (list x) L) --
; (cons x L) +++++

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;12. Insérer un élément dans la nième position : insere;

(define insere
  (lambda (n x L)
    (if (null? L)
        ()
        (if (= n 1)
            (cons x L)
            (cons (car L) (insere (- n 1) x (cdr L)))))))

(insere 2 6 '())
(insere 1 7 '(3 4 6))
(insere 19 5 '(1 2 3 4 6 7 8 9)) ; Retourne la liste L parce que l'insertion
                                 ; n'est pas possible ; 
(insere 5 5 '(1 2 3 4 6 7 8 9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;13. Union de deux listes : Union ;

(define union
  (lambda (L1 L2)
    (cond
     ((null? L1) L2)
     ((null? L2) L1)
     (else (epure (append L1 L2))))))

(union '() '())
(union '() '(2 4 6))
(union '(5 6 9 87) '())
(union '(1 5 8 3) '(2 7 90))
(union '(1 3) '(1 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;14. Intersection de deux listes : inter;

(define inter
  (lambda (L1 L2)
    (cond
     ((null? L1) ())
     ((null? L2) ())
     ((membre (car L1) L2) (cons (car L1) (inter (cdr L1) L2)))
     ((membre (car L2) L1) (cons (car L2) (inter L1 (cdr L2))))
     (else (inter (cdr L1) (cdr L2))))))

(inter '() '(3 6 90))
(inter '(6 3 8) '())
(inter '(1 2 34) '(5 6 7))
(inter '(2 5 7) '(32 6 5 7 9 7))
(inter '(2 4 6) '(3 4 7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;15. mise à plat : niv0;

(define niv0
  (lambda (L)
    (if (null? L)
        ()
        (if (not (list? (car L)))
            (cons (car L) (niv0 (cdr L)))
            (append (niv0 (car L)) (niv0 (cdr L)))))))

(niv0 '())
(niv0 '(()))
(niv0 '((1 (2 (3) (4 5)) (6 7))))

; copier : Alt + W
; coller : Ctrl + Y

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;16. Couples d'èlèments de position identique : zip;

(define zip
  (lambda (L1 L2)
    (map list L1 L2)))

(define zip
  (lambda (L1 L2)
    (letrec ((lisp1
              (lambda (L)
                (list (cons (car L) (())) (lisp (cdr L)))))
             (lisp2
              (lambda (L)
                (list (cons () (cons (car L) ())) (lisp (cdr L)))))
             (cond
              ((null? L1) (lisp2 L2))
              ((null? L2) (lisp1 L1))
              (else (list (cons (car L1) (cons (car L2) ()))) (zip (cdr L1) (cdr L2))))))))

(zip '(1 2 3) '())
(zip '() '(8 4 2 4 9))
(zip '(3 5 8 1 0 17 6) '(1 2 3 4 5))
(zip '(6 3 8 5) '(1 2 3 4 5 6 7))
(zip '(3 5 1 8 7) '(1 2 3 4 5))

; Ça marche pas correctement ;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;17. Ensemble des couples du produit cartésien entre L1 et L2 : prod;

(define prodc
  (lambda (L1 L2)
    (append-map (lambda (x)
                  (map (lambda (y) (list x y))
                       L2))
                L1)))
  
  
(define prod
  (lambda (L1 L2)
    (if (null? L1)
        (zip L1 L2)
        (append (zip (list (car L1)) L2) (prod (cdr L1) L2)))))

(prodc '() '())
(prodc '() '(1 2 3 4 5 6)) ; Est-ce que c'est logique ?;
(prodc '(64 7 7 6 3) '())  ; Est-ce que c'est logique ?;
(prodc '(1 2 3 5 6) '(1 5))
(prodc '(1 6 8) '(6 2 9 1 5 9 643))
(prodc '(1 2 3 4 5 6 7 8) '(3 5 6 2 4 7 8 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;18. Somme des éléments d'une liste : som_list;

(define som_list
  (lambda (L)
    (if (null? L)
        0
        (+ (car L) (som_list (cdr L))))))

(som_list '())
(som_list '(2))
(som_list '(4 2 1 4 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;19. triangle ;

(define triang
  (lambda (n)
    (letrec ((listeSimple
              (lambda (n)
                (if (= n 0)
                    ()
                    (cons n  (listeSimple (- n 1))))))) ;j'ai fait (triang (n-1))
      (let* ((L1 (listeSimple n))
             (L2 (mirroir L1)))
        (append L2 (cdr L1)))))) ;Pour éviter d'afficher le n 2 fois;
    

(triang 0)
(triang 3)
(triang 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;20. Suite de fibonacci : fibo;

(define fibo
  (lambda (L)
    (map + (append L '(1)) (append (cdr L) '(0 0)))))

(define fibo
  (lambda (L)
    (append (map + (mirroir (cdr (mirroir L))) (cdr L))
            '(1 1))))

; None of the above is working !!!!!!;

(define fibo
  (lambda (n)
    (cond
     ((= n 0) 1)
     ((= n 1) 1)
     (else (+ (fibo (- n 1)) (fibo (- n 2)))))))

(fibo 0)
(fibo 1)
(fibo 2)
(fibo 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;21. Moyenne des éléments numériques d'une liste : moy;

(define moy
  (lambda (L)
    (if (null? L)
        0
        (/ (som_list L) (long L)))))

(moy '())
(moy '(3))
(moy '(10 2 4 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;22. Liste des listes : ies;

(define ies
  (lambda (L n)
    (letrec ((sup
              (lambda (L n)
                (if (null? L)
                    ()
                    (if (> (car L) n)
                        (cons (car L) (sup (cdr L) n))
                        (sup (cdr L) n)))))
             (inf
              (lambda (L n)
                (if (null? L)
                    ()
                    (if (< (car L) n)
                        (cons (car L) (inf (cdr L) n))
                        (inf (cdr L) n))))))
      (list (inf L n) (list n) (sup L n)))))

(ies '() 6)
(ies '(2 4 7 3 5 -7 -6 -9) 0)
(ies '( 1 2 3 4 5 6 7 8 9 10) 7)


 ;heapsort;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 23. Tri par insertion : tri_ins;

(define tri_ins
  (lambda (L)
    (letrec ((insere
	   (lambda (e L)
	     (if (null? L)
		 (list e)
		 (if (< e (car L))
		     (cons e L)
		     (cons (car L) (insere e (cdr L))))))))
      (if (null? L)
	  ()
	  (insere (car L) (tri_ins (cdr L)))))))

(tri_ins '())
(tri_ins '(2))
(tri_ins '(7 9 3 2 6 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	; 24. Tri par sélection : tri_sel ;

(define tri_sel
  (lambda (L)
    (letrec ((min
	      (lambda (L)
		(if (null? (cdr L))
		    (car L)
		    (if (< (car L) (min (cdr L)))
			(car L)
			(min (cdr L))))))
	     (listeReduite
	      (lambda (L Val)
		(if (null? L)
		    ()
		    (if (not (= (car L) Val))
			(cons (car L) (listeReduite (cdr L) Val))
			(cdr L))))))
      (if (null? L)
	  ()
	  (tri_sel (cons (min L) (listeReduite (cdr L) (min L))))))))

(tri_sel '())
(tri_sel'(4))
(tri_sel '(5 6 8 3 2 4 9 7)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		   ; 25. Tri à bulles : tri_bul ;

(define echange
  (lambda (L)
    (if (null? L)
	()
	(cons (cadr L)
	      (cons (car L)
		    (cdr (echange (cdr L))))))))

(echange '(3 2 1 5 7 4))

(define tri_bul
  (lambda (L)
    (if (null? L)
	()
	( ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; filter : fonction qui renvoie la liste des éléments qui vérrifient le prédicat
; p ;

(define filter
  (lambda (p L)
    (append (map (lambda (u)
                   (if (p u)
                       (list u)))
                 L))))

(filter (lambda (x)
          (= (remainder x 3) 0))
        ())
(filter (lambda (x)
          (= (remainder x 3) 0))
        '(0 1 2 3 4 5 6 7 8 9))
