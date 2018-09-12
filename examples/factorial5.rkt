(letrec [(f (λ (n) 
                                   (if (= n 0)
                                       1
                                       (* n (f (- n 1))))))]
                       (f 5))
