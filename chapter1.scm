#lang scheme

(define p 
  (lambda (arg)
    (display arg)
    (newline)))


(define sqrt 
  (lambda (x)
    (* x x)))

(p (sqrt 100))

;; chapter 8

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))



(define multiinsertR 
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat)) old)
      (cons old (cons new multiinsertR new old (cdr lat)))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertLR 
  (lambda (new oldL oldR lat)
    (cond 
      ((null? lat) `())
      ((eq? (car lat) oldL) 
       (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else 
       (cons (car lat)
             (multiinsertLR new oldL oldR (cdr lat)))))))



(p (multiinsertLR `new `l `r '(1 2 3 4 l 23 r l f 78 l r)))


(define add1
  (lambda (x) (+ 1 x)))



(define multiLR
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) 
       (col '() 0 0))
      ((eq? (car lat) oldL) 
       (multiLR new oldL oldR (cdr lat) 
                (lambda (newlat L R) 
                  (col (cons new (cons oldL newlat)) (add1 L) R))))
      ((eq? (car lat) oldR)
       (multiLR new oldL oldR (cdr lat)
                (lambda (newlat L R)
                  (col (cons oldR (cons new newlat)) L (add1 R)))))
      (else
       (multiLR new oldL oldR (cdr lat) 
                (lambda (newlat L R) 
                  (col (cons (car lat) newlat) L R)))))))


(p (multiLR 'new 'l 'r '(1 2 3 l r 1 2 and l r or that is all 1 2) (lambda (lst l r) lst)))

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  