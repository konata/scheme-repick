(define Y
  (lambda (le)
    ((lambda (f) (f f))
    (lambda (f)
    (le (lambda (x) ((f f) x)))))))
    
    
(define le (lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))
      
      
