;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ; Schéma récursif : SR;

(define SR
  (lambda (L valVide fTeteQueue)
    (if (null? L)
        valVide
        (fTeteQueue (car L)
                    (SR (cdr L) valVide fTeteQueue)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ; Schéma itératif : SI;

(define SI
  (lambda (acc L f)
    (if (null? L)
        acc
        (SI (f acc (car L))
            (cdr L)
            f))))
