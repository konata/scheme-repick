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

(define lst '(1 2 3 l r 1 2 and l r or that is all 1 2))
(p lst)
(p (multiLR 'new 'l 'r lst (lambda (lst l r) lst)))





;; page-144 evens-only*
;(define even?
;  (lambda (x) 
;    (= (* 2 (/ x 2)) x)))

(define even?
  (lambda (x)
    (cond 
      ((= 1 x) #t)
      ((= 0 x) #f)
      (else (even? (- x 2))))))

(define evens-only
  (lambda (l)
    (cond
      ((null? l) '())
      ((number? (car l)) 
       (cond 
         ((even? (car l)) (cons (car l) (evens-only (cdr l)))) 
         (else (evens-only (cdr l)))))
      (else (cons (evens-only (car l)) (evens-only (cdr l))))))) 

(define l '(1 2 3 4 5 (1 3 4 5 7 9 9 11) 1 2 4 (1 2 3 (1 4 5 6 (7 8 9)))))
(p (evens-only l))


(define atom?
  (lambda (x)
    (not (list? x))
    ))


;page-145


(define even-op
  (lambda (l col)
    (cond
      ((null? l) (col '() 1 0)) ;; recursive end
      ((list? (car l)) ;; list
       (even-op (car l) (lambda (thisLat thisMul thisAdd) 
                          (even-op (cdr l) 
                                   (lambda (oldLat oldMul oldAdd)
                                     (col (cons thisLat oldLat) (* thisMul oldMul) (+ thisAdd oldAdd)))))))
      (else (let ((head (car l)) (tail (cdr l))) ;; atom
              (cond 
                ((even? head)
                 (even-op tail (lambda (oldLat oldMul oldAdd) (col (cons head oldLat) (* head oldMul) oldAdd))))
                (else
                 (even-op tail (lambda (oldLat oldMul oldAdd) (col oldLat oldMul (+ oldAdd head)))))))))))


(define numbers '(1 2 3 4 5 6 7 (1 2 3 4 5 6 7) (2 3 4 5 (4 5 6 7))))

(even-op numbers (lambda (x mul add) (p mul) (p add) (p x)))























