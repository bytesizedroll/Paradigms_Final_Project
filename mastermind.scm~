(load "Untitled.rkt")

; right-now : -> moment
(define (right-now)
  (call-with-current-continuation 
   (lambda (cc) 
     (cc cc))))

; go-when : moment -> ...
(define (go-when then)
  (then then))

;Function to check a single guess agains the correct result, returns feedback in the form of a list.
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

;Helper functions
(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else (op (car seq) (accumulate op init (cdr seq))))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-integers a b)
  (cond ((> a b) '())
        (else (cons a (enumerate-integers (+ 1 a) b)))))

(define (mfilter seq p?)
  (cond ((null? seq) '())
        ((p? (car seq)) (cons (car seq) (mfilter (cdr seq) p?)))
        (else (mfilter (cdr seq) p?))))

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
  (mfilter megalist (lambda (lst) (accumulate myand #t (map (lambda (req) (>= (count-occurrences lst (car req)) (cadr req))) requirements)))))


;Now using amb...
;All we need now is the updating of requirements
(define (meets-reqs? guess requirements)
  (accumulate myand #t (map (lambda (req) (>= (count-occurrences guess (car req)) (cadr req))) requirements)))


;Update function to update list of possibilities for each peg based on previous guess and result
(define (update total-guess total-feedback)
  (define (inner-update poss feedback guess)
    (cond ((eq? feedback 'b) (list guess))
          ((eq? feedback 'w) (remove guess poss))
          ((eq? feedback 'e) (remove guess poss))))
  (list (inner-update (car poss) (car total-feedback) (car total-guess))
        (inner-update (cadr poss) (cadr total-feedback) (cadr total-guess))
        (inner-update (caddr poss) (caddr total-feedback) (caddr total-guess))
        (inner-update (cadddr poss) (cadddr total-feedback) (cadddr total-guess))))


(define reqs '((b 0))) ;Minimum of each color

;Create of all color possibilities for one spot.
(define colorarray (list 'r 'g 'b 'y 'w 'k 'o 'p))

;Optns is list of lists that contain possible pegs for each spot.
(define optns (list colorarray colorarray colorarray colorarray))

;Function to generate guess using amb
(define (make-guess array)
  (cons (an-element-of (car array)) (cons (an-element-of (cadr array)) (cons (an-element-of (caddr array)) (cons (an-element-of (cadddr array)) '() )))))

;Update function to update guess after receiving the feedback from the previous guess.
(define (update total-guess total-feedback)
  (define (inner-update optns feedback guess)
    (cond ((eq? feedback 'b) (list guess))
          ((eq? feedback 'w) (remove guess optns))
          ((eq? feedback 'e) (remove guess optns))))
  (list (inner-update (car optns) (car total-feedback) (car total-guess))
        (inner-update (cadr optns) (cadr total-feedback) (cadr total-guess))
        (inner-update (caddr optns) (caddr total-feedback) (caddr total-guess))
        (inner-update (cadddr optns) (cadddr total-feedback) (cadddr total-guess))))

;Pattern variable that sets what the correct pattern is.
(define pattern '(o g b k))

;Actual program, tests guess and updates guess until the correct pattern is reached.
(let ((the-beginning (right-now)))
  (let ((guess (make-guess optns)))
    (assert (meets-reqs? guess reqs))
    (newline)
    (display "MY GUESS:")
    (newline)
    (display guess)
    (newline)
    (display "THE RESULT:")
    (newline)
    (display (check-solution guess pattern))
    (cond ((equal? (check-solution guess pattern) '(b b b b)) (newline) (newline) (display "____We Did It!!____") (newline) (display"|     (°_o)/¯     |"))
          (else (begin (set! optns (update guess (check-solution guess pattern)))   
                       (go-when the-beginning))))))
    