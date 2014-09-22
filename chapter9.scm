#lang scheme

(define p
  (lambda (x)
    (display x)
    (newline)))


(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))
    


; (define looking 
;   (lambda (a lat)
;     (keep-looking a (pick 1 lat) lat)))




(define eternity
  (lambda (x)
    (eternity x)))

; (define shift
;  (lambda (pair)
;    (build (first (first pair))
;           (build (sencond (first pair))
;                  (second pair)))))





;; page 152
;(define align
;  (lambda (pora)
;    (cond 
;      ((atom? pora) pora)
;      ((pair? (first pora)) (align (shift pora)))
;      (else (build (first pora) (align (second pora)))))))
;



(define weight
  (lambda (para)
    (cond 
      ((atom? para) 1)
      (else (+ (weight (first para)) 2)
            (weight (second para))))))

(p (weight '((a b) (c d (e f)))))


                  