;; main
(load "lambda.rkt")

(define program 
	`(- 5 4)
)

(define (λ-to-ski f)
  (match f
    [`(λ (,a) (λ (,b) ,c)) (λ-to-ski `(λ (,a) ,(λ-to-ski `(λ (,b) ,c))))]
    [`(λ (,a) (,exp1 ,exp2)) `((S ,(λ-to-ski `(λ (,a) ,exp1))) ,(λ-to-ski `(λ 
(,a) ,exp2)))]
    [`(λ (,a) ,a) 'I]
    [`(λ (,a) ,b) `(K ,(λ-to-ski b))]
    [`(,a ,b) `(,(λ-to-ski a) ,(λ-to-ski b))]
    [_ f]))

(define (ski-to-lisplisp f)
  (cond ((pair? f) (cons (ski-to-lisplisp (car f)) (ski-to-lisplisp (cdr f))))
        ((eq? 'S f) '(lisp (lisp (lisp (lisp lisp)))))
        ((eq? 'K f) '(lisp (lisp (lisp lisp))))
        ((eq? 'I f) '(lisp lisp))
        (else '())))

(ski-to-lisplisp (λ-to-ski (compile program)))
