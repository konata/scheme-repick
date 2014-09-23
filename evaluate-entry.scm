#lang scheme
;;chapter 10

;; function Y
(define Y
  (lambda (le)
    ((lambda (f)
       (f f))
     (lambda (f)
       (le (lambda(l)
             ((f f) l)))))))

;; function accept length and list and apply it Y will get length
(define mk-length
  (lambda (length)
    (lambda (l)
      (cond 
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))

;; define length using y-combinator
(define length
  (Y mk-length))

(define p
  (lambda (x)
    (display x)
    (newline)))

;; build entries
(define build
  (lambda (key value)
    (if (= (length key) (length value)) 
        (cons key value) `())))

(define new-entry build)

;; find in entry
(define lookup-in-entry   
  (lambda (key entry)
    (define find
      (lambda (keylist valuelist key)
        (cond
          ((null? keylist) '())
          ((eq? (car keylist) key) (car valuelist))
          (else (find (cdr keylist) (cdr valuelist) key))
          )))
    (find (car entry) (cdr entry) key)))

;; tests
(define item (new-entry '(1 2 3 4 5 6) '(a b c d e f)))
(p item)
(p (lookup-in-entry 4 item))
(p (lookup-in-entry 'good item))



