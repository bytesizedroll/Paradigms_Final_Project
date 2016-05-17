(define (check-solution guess pattern)
  
  (define (count-occurrences x col)
    (cond ((null? x) 0)
          (else (if (eq? (car x) col) (+ 1 (count-occurrences (cdr x) col)) (+ 0 (count-occurrences (cdr x) col))))))
  
  (define (nth-occur-of-color x index counter)
    (let ((color-at (list-ref guess index)))
      (cond ((>= counter index) 0)
            (else
             (cond
               ((eq? color-at (car x)) (+ 1 (nth-occur-of-color (cdr x) index (+ 1 counter))))
               (else (nth-occur-of-color (cdr x) index (+ 1 counter)))) ))))
  (define (number-of-correct col)
    (apply +
           (map (lambda (a b) (if (and (eq? a b) (eq? a col)) 1 0)) guess pattern)))
  
  (define (check-iter index)
    (cond ((>= index (length pattern)) '())
          (else 
           (cond ((eq? (list-ref guess index) (list-ref pattern index)) (cons 'b (check-iter (+ 1 index))))
                 ((and
                   (not (zero? (count-occurrences pattern (list-ref guess index))))
                   (<= (+ (nth-occur-of-color guess index 0) (number-of-correct (list-ref guess index)))  (count-occurrences pattern (list-ref guess index))))
                  (cons 'w (check-iter (+ 1 index))))
                 (else (cons 'e (check-iter (+ 1 index))))))))
  
  (check-iter 0))