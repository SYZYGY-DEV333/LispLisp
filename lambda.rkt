
;; <exp> ::= <var>

;;        |  #t
;;        |  #f
;;        |  (if  <exp> <exp> <exp>)
;;        |  (and <exp> <exp>)
;;        |  (or  <exp> <exp>)

;;        |  <nat>
;;        |  (zero? <exp>)
;;        |  (- <exp> <exp>)
;;        |  (= <exp> <exp>)
;;        |  (+ <exp> <exp>)
;;        |  (* <exp> <exp>)

;;        |  <lam>
;;        |  (let ((<var> <exp>) ...) <exp>)
;;        |  (letrec ((<var> <lam>)) <exp>)

;;        |  (cons <exp> <exp>)
;;        |  (car  <exp>)
;;        |  (cdr  <exp>)
;;        |  (pair? <exp>)
;;        |  (null? <exp>)
;;        |  '()

;;        |  (<exp> <exp> ...)

;; <lam> ::= (λ (<var> ...) <exp>)


; Void.
(define VOID  `(λ (void) void))

; Error.
(define ERROR '(λ (_) 
                 ((λ (f) (f f)) (λ (f) (f f)))))

; Booleans.
(define TRUE  `(λ (t) (λ (f) (t ,VOID))))
(define FALSE `(λ (t) (λ (f) (f ,VOID))))

; Church numerals.
(define (church-numeral n)
  
  (define (apply-n f n z)
    (cond
      [(= n 0)  z]
      [else     `(,f ,(apply-n f (- n 1) z))]))
       
  (cond
    [(= n 0)    `(λ (f) (λ (z) z))]
    [else       `(λ (f) (λ (z) 
                          ,(apply-n 'f n 'z)))]))

(define ZERO? `(λ (n)
                 ((n (λ (_) ,FALSE)) ,TRUE)))
                  
(define SUM '(λ (n)
               (λ (m)
                 (λ (f)
                   (λ (z)
                     ((m f) ((n f) z)))))))

(define MUL '(λ (n)
               (λ (m)
                 (λ (f)
                   (λ (z)
                     ((m (n f)) z))))))
                     
(define PRED '(λ (n)
                (λ (f)
                  (λ (z)
                    (((n (λ (g) (λ (h) 
                                  (h (g f)))))
                      (λ (u) z))
                     (λ (u) u))))))

(define SUB `(λ (n)
               (λ (m)
                 ((m ,PRED) n))))


; Lists.
(define CONS `(λ (car) 
                (λ (cdr)
                  (λ (on-cons)
                    (λ (on-nil)
                      ((on-cons car) cdr))))))

(define NIL `(λ (on-cons)
               (λ (on-nil)
                 (on-nil ,VOID))))

(define CAR `(λ (list)
               ((list (λ (car)
                       (λ (cdr)
                         car)))
                ,ERROR)))

(define CDR `(λ (list)
               ((list (λ (car)
                       (λ (cdr)
                         cdr)))
                ,ERROR)))

(define PAIR? `(λ (list)
                 ((list (λ (_) (λ (_) ,TRUE)))
                  (λ (_) ,FALSE))))

(define NULL? `(λ (list)
                 ((list (λ (_) (λ (_) ,FALSE)))
                  (λ (_) ,TRUE))))


; Recursion.
(define Y '((λ (y) (λ (F) (F (λ (x) (((y y) F) x))))) 
            (λ (y) (λ (F) (F (λ (x) (((y y) F) x)))))))


; Compilation:
(define (compile exp)
  (match exp
    
    ; Symbols stay the same:
    [(? symbol?)     exp]
    
    ; Boolean and conditionals:
    [#t              TRUE]
    [#f              FALSE]
    [`(if ,cond ,t ,f)
     ; =>
     (compile `(,cond (λ () ,t) (λ () ,f)))]

    [`(and ,a ,b)
     ; =>
     (compile `(if ,a ,b #f))]
    
    [`(or ,a ,b)
     ; =>
     (compile `(if ,a #t ,b))]

    ; Numerals:
    [(? integer?)     (church-numeral exp)]
    [`(zero? ,exp)   `(,ZERO? ,(compile exp))]
    [`(- ,x ,y)      `((,SUB ,(compile x)) ,(compile y))]
    [`(+ ,x ,y)      `((,SUM ,(compile x)) ,(compile y))]
    [`(* ,x ,y)      `((,MUL ,(compile x)) ,(compile y))]
    [`(= ,x ,y)      (compile `(and (zero? (- ,x ,y))
                                    (zero? (- ,y ,x))))]
    
    ; Lists:
    [ (quote '())         NIL]
    [`(cons  ,car ,cdr)  `((,CONS ,(compile car)) 
                           ,(compile cdr))]
    [`(car   ,list)      `(,CAR   ,(compile list))]
    [`(cdr   ,list)      `(,CDR   ,(compile list))]
    [`(pair? ,list)      `(,PAIR? ,(compile list))]
    [`(null? ,list)      `(,NULL? ,(compile list))]
    
    ; Lambdas:
    [`(λ () ,exp)  
     ; =>
     `(λ (_)  ,(compile exp))] 
    
    [`(λ (,v) ,exp)         
     ; =>
     `(λ (,v) ,(compile exp))]
    
    [`(λ (,v ,vs ...) ,exp)
     ; =>
     `(λ (,v)
        ,(compile `(λ (,@vs) ,exp)))]

    ; Binding forms:
    [`(let ((,v ,exp) ...) ,body)
     ; =>
     (compile `((λ (,@v) ,body) ,@exp))]
    
    [`(letrec [(,f ,lam)] ,body)
     ; =>
     (compile `(let ((,f (,Y (λ (,f) ,lam))))
                 ,body))]
    
    ; Application -- must be last:
    [`(,f) 
     ; =>
     (compile `(,(compile f) ,VOID))]
    
    [`(,f ,exp)
     ; =>
     `(,(compile f) ,(compile exp))]
    
    [`(,f ,exp ,rest ...)
     ; =>
     (compile `((,f ,exp) ,@rest))]
    
    [else  
     ; =>
     (display (format "unknown exp: ~s~n" exp))
     (error "unknown expression")]))


; Unchurchification.
(define (succ n) (+ n 1))

(define (natify church-numeral)
  ((church-numeral succ) 0))

(define (boolify church-boolean)
  ((church-boolean (λ (_) #t)) (λ (_) #f)))

(define (listify f church-list)
  ((church-list
    (λ (car) (λ (cdr) (cons (f car) (listify f cdr)))))
   (λ (_) '())))



