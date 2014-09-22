#lang scheme

(define p
  (lambda (x)
    (display x)
    (newline)))

(define one?
  (lambda (x)
    (= x 1)))

;; page 150
;(define keep-looking
;  (lambda (a sorn lat)
;    (cond 
;      ((number? sorn) (keep-looking a (pick sorn lat) lat))
;      (else (eq? sorn a)))))


(define eternity
  (lambda (x)
    (eternity x)))

       

(define C
  (lambda (n)
    (cond 
      ((one? n) 1)
      (else 
       (cond 
         ((even? n) (C (/ n 2)))
         (else (C (add1 (* 3 n)))))))))


(define A
  (lambda (n m)
    (cond 
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n) (A n (sub1 m)))))))

(p (A 3 4))


;(define last-try
;  (lambda (x)
;    (and (will-stop? last-try) (eternity x))))



(define length
  (lambda (l)
    (cond 
      ((null? l) 0)
      (else (add1 length (cdr l))))))

(define length0
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (eternity (cdr l)))))))


(lambda (l)
  (cond 
    ((null? l) 0)
    (else (add1 (length0 cdr l)))))


;; page 161
(define length<=1
(lambda (l)
  (cond
    ((null? l) 0)
    (else
     (add1 ((lambda (l)
             (cond
               ((null? l) 0)
               (else (add1 (eternity (cdr l)))))) (cdr l)))))))


(p (length<=1 '(1)))

(define length<=2
  (lambda (l)
    (cond 
      ((null? l) 0)
      (else
       (add1 
        ((lambda (l)
           (cond 
             ((null? l) 0)
             (else (add1
                   ((lambda (l)
                      (cond
                        ((null? l) 0)
                        (else (add1 (eternity (cdr l)))))) 
                    (cdr l)))))) 
         (cdr l)))))))

(p (length<=2 '(1 2)))
(p "done")



((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))) eternity)

;; length<=1

(lambda (f)
  (lambda (l)
    (cond 
      ((null? l) 0)
      (else (add1 (f (cdr l))))))
    
    ((lambda (g)
       (lambda (l)
         (cond
           ((null? l) 0)
           (else (add1 (g (cdr l))))))) eternity))
              

;; length <=2
(define len<=2 
((lambda (remain-length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (remain-length (cdr l)))))))

 (
  (lambda (remain-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (remain-length (cdr l)))))))
  
  (lambda(l)
    (cond
      ((null? l) 0)
      (else (eternity (cdr l))))))
))

(p (len<=2 '(1 2)))
   

;; length0
((lambda (mk-length)
  (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;; length<=1
((lambda (mk-length)
   (mk-length (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;; length<=2
((lambda (mk-length)
  (mk-length (mk-length (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;; length<=3
(define length<=3

(
(lambda (mk-length)
  (mk-length (mk-length (mk-length (mk-length eternity)))))

(lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))

)
)

(p (length<=3 '(1 2 4)))


                        
(((lambda (mk-length)
   (mk-length mk-length))
  
  (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((mk-length mk-length) (cdr l)))))))) '(apples and this is items))


 



