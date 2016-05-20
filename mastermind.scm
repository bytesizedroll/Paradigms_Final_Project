(define (check-solution board actual)
  
  (define (count-occurrences x col)
    (cond ((null? x) 0)
          (else (if (eq? (car x) col) (+ 1 (count-occurrences (cdr x) col)) (+ 0 (count-occurrences (cdr x) col))))))
  
  (define (nth-occur-of-color x index counter)
    (let ((color-at (list-ref board index)))
      (cond ((>= counter index) 0)
            (else
             (cond
               ((eq? color-at (car x)) (+ 1 (nth-occur-of-color (cdr x) index (+ 1 counter))))
               (else (nth-occur-of-color (cdr x) index (+ 1 counter)))) ))))
  (define (number-of-correct col)
    (apply +
           (map (lambda (a b) (if (and (eq? a b) (eq? a col)) 1 0)) board actual)))
  
  (define (check-iter index)
    (cond ((>= index (length actual)) '())
          (else 
           (cond ((eq? (list-ref board index) (list-ref actual index)) (cons 'b (check-iter (+ 1 index))))
                 ((and
                   (not (zero? (count-occurrences actual (list-ref board index))))
                   (<= (+ (nth-occur-of-color board index 0) (number-of-correct (list-ref board index)))  (count-occurrences actual (list-ref board index))))
                  (cons 'w (check-iter (+ 1 index))))
                 (else (cons 'e (check-iter (+ 1 index))))))))
  
  (check-iter 0))

(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else (op (car seq) (accumulate op init (cdr seq))))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-integers a b)
  (cond ((> a b) '())
        (else (cons a (enumerate-integers (+ 1 a) b)))))

(define (filter seq p?)
  (cond ((null? seq) '())
        ((p? (car seq)) (cons (car seq) (filter (cdr seq) p?)))
        (else (filter (cdr seq) p?))))

;Generates all possible quadruples given a list of four lists
(define (quads qlist)
   (flatmap (lambda (i)
              (flatmap (lambda (j)
                     (flatmap (lambda (k)
                            (map (lambda (l)
                                 (list i j k l)) (cadddr qlist))) (caddr qlist))) (cadr qlist)))  (car qlist)))
(define (count-occurrences x col)
    (cond ((null? x) 0)
          (else (if (eq? (car x) col) (+ 1 (count-occurrences (cdr x) col)) (+ 0 (count-occurrences (cdr x) col))))))
(define myand (lambda (x y) (and x y)))


;Filters a list of quadruples based on requirement lists (req lists -> list of pairs specifying minimums
;ie ((w 2) (b 1)) means there must be 2 ws and 1 b)

(define (meets-requirements megalist requirements)
  (filter megalist (lambda (lst) (accumulate myand #t (map (lambda (req) (>= (count-occurrences lst (car req)) (cadr req))) requirements)))))

;Test case

(define check-my-solution
  (lambda (board) (check-solution board '(r g g k))))
(map check-my-solution (quads '((r g b y) (r g b y) (r g b y) (r g b y))))

;This solves the problem.
(define colorarray (list 'r 'g 'b 'y 'w 'k 'o 'p))
(quads (list colorarray colorarray colorarray colorarray))
(display "CORRECT SOLUTION:")
(filter (quads (list colorarray colorarray colorarray colorarray)) (lambda (q) (equal? (check-my-solution q) '(b b b b))))