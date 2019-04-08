(define (replace source target replacement)
  (cond ((null? source)'())
        ((equal? source target) replacement)
        (else (let ((next (car source))
                    (rest (cdr source)))
                (cons (if (not (list? next))
                          next
                          (replace next target replacement))
                      (replace rest target replacement))))))
                    
(define (replace-S source)
    (replace source '(lisp (lisp (lisp (lisp lisp)))) 's))
(define (replace-K source)
    (replace source '(lisp (lisp (lisp lisp))) 'k))
(define (replace-I source)
    (replace source '(lisp lisp) 'i))

(define (parse program)
    (let* ( [step1 (replace-S program)]
            [step2 (replace-K step1)]
            [step3 (replace-I step2)] )
        step3))

        
(define (expr-dispatch expr leaf appl lamb)
  (if (pair? expr)
      (if (eq? (car expr) 'lambda)
          (lamb (caadr expr) (caddr expr))
          (appl (car expr) (cadr expr)) )
      (leaf expr) ))

(define (print-as-generic aply k s i)
    (lambda (lazified-code)
        (let self ((code lazified-code))
            (expr-dispatch code
                (lambda (leaf)
                    (cond ((eq? leaf 'i) (display i))
                          ((eq? leaf 'k) (display k))
                          ((eq? leaf 's) (display s))
                        (else (display "[") (display leaf) (display "]")) ))
                (lambda (f g)
                    (display aply)
                    (self f)
                    (self g) )
                (lambda (var body)
                    (error "Can't print lambdas as Lazy code!") )))
        (newline) ))

(define print-as-unlambda (print-as-generic "`" "k" "s" "i"))
(print-as-unlambda (parse (read)))
