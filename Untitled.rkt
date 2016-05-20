; amb macros from 'Teach Yourself Scheme in Fixnum Days', by Dorai Sitaram
; available from the plt-scheme helpdesk

; see also section 4.3 in Abelson and Sussman


;(require (lib "defmacro.ss"))

;;;; use the language Pretty Big -- r5rs will not permit 'require'

(require compatibility/defmacro)

(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
          (lambda ()
            (error "amb tree exhausted")))))


(initialize-amb-fail)

(define-macro amb
  (lambda alts...
    `(let ((+prev-amb-fail amb-fail))
       (call/cc
        (lambda (+sk)
          
          ,@(map (lambda (alt)
                   `(call/cc
                     (lambda (+fk)
                       (set! amb-fail
                             (lambda ()
                               (set! amb-fail +prev-amb-fail)
                               (+fk 'fail)))
                       (+sk ,alt))))
                 alts...)
          
          (+prev-amb-fail))))))

(define assert
  (lambda (pred)
    (if (not pred) (amb))))

(define-macro bag-of
  (lambda (e)
    `(let ((+prev-amb-fail amb-fail)
           (+results '()))
       (if (call/cc
            (lambda (+k)
              (set! amb-fail (lambda () (+k #f)))
              (let ((+v ,e))
                (set! +results (cons +v +results))
                (+k #t))))
           (amb-fail))
       (set! amb-fail +prev-amb-fail)
       (reverse! +results))))


(define (an-element-of items)
  (assert (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))
