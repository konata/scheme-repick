#lang scheme
;;chapter 10
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


;; scheme intepreter

(define build
  (lambda (first second)
    (cons first (cons second '()))))

(define first
  (lambda (set)
    (car set)))

(define second
  (lambda (set)
    (cdar set)))

(define third
  (lambda (set)
    (cddar set)))

(define new-entry build)
(define keys first)
(define values second)


(define lookup-in-entry-help
  (lambda (name names values fn)
    (cond 
      ((null? names) (fn name))
      ((eq? (car names) name) (car value))
      (else (lookup-in-entry-help name (cdr names) (cdr values) fn)))))

(define lookup-in-entry
  (lambda (name entry fn)
    (lookup-in-entry-help name (first entry) (second entry) fn)))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table fn)
    (cond
      ((null? table) (fn name))
      (else (lookup-in-entry name (cdr table) 
                             (lambda (name)
                               (lookup-in-table name (cdr table) fn)))))))

(define atom?
  (lambda (x)
    ((not (or (pair? x) (null? x))))))



(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))



(define atom-to-action
  (lambda (e)
    (cond 
      ((number? e) 'const)
      ((or (eq? e #t) (eq? e #f) *const))
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (cdr e))
       (cond
         ((eq? (cdr e) 'quote) *quote)
         ((eq? (cdr e) 'lambda) *lambda)
         ((eq? (cdr e) 'cond) *cond)
         ((eq? (cdr e) 'application) *application))))))


(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))


(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build  'primitive e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table 
  (lambda (name)
    (car'())))

(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table cdr e))))


(define table-of first)
(define formals-of second)
(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines))) (meaning (answer-of (cdr lines)) table))
      ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (and (atom? x) (eq? x 'else))))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)


(define evlis 
  (lambda (args table)
    (cond
      ((null? args) '())
      (else (cons (meaning (car args) table)
                  (evlis (cdr args) table))))))


(define *application
  (lambda (e table)
    (apply 
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))


(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))


(define apply
  (lambda (fun vals)
    (cond 
      ((primitive? fun)
       (apply-primitive (second fun) vals))
      ((non-primitive? fun)
       (apply-closure (second fun) vals)))))


(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons) (cons (first vals) (second vals)))
      ((eq? name 'car) (first vals))
      ((eq? name 'cdr) (second vals))
      ((eq? name 'null?) (null? (first vals)))
      ((eq? name 'eq?) (eq? (first vals) (second vals)))
      ((eq? name 'atom?) (atom? (first vals)))
      ((eq? name 'zero?) (zero? (first vals)))
      ((eq? name 'add1) (add1 (first vals)))
      ((eq? name 'sub1) (sub1 (first vals)))
      ((eq? name 'number?) (number? (first vals))))))
      
(define :atom?
  (lambda (x)
    (cond 
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) 'primitive) #t)
      ((eq? (car x) 'non-primitive) #t)
      (else #f))))
                 

       
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry (formals-of closure) vals)
              (table-of closure)))))




              











