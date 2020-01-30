#lang racket
(require srfi/17)
(require (only-in srfi/1 zip))
(require racket/match)
(require "grand-syntax.rkt")
(provide is isnt fold-right fold-left find for memoized zip
	 define/memoized
	 define# lambda#
	 out merge ->string set! while)

(define (fold-left op e l)
  (match l
    (`(,h . ,t)
     (fold-left op (op e h) t))
    (_
     e)))

(define (fold-right op e l)
  (match l
    (`(,h . ,t)
     (op h (fold-right op e t)))
    (_
     e)))

(define find findf)

(define-syntax infix/postfix
  (syntax-rules ()
    
    ((infix/postfix x somewhat?)
     (somewhat? x))

    ((infix/postfix left related-to? right)
     (related-to? left right))

    ((infix/postfix left related-to? right . likewise)
     (let ((right* right))
       (and (infix/postfix left related-to? right*)
	    (infix/postfix right* . likewise))))
    ))

(define-syntax extract-placeholders
  (syntax-rules (_)
    ((extract-placeholders final () () body)
     (final (infix/postfix . body)))

    ((extract-placeholders final () args body)
     (lambda args (final (infix/postfix . body))))

    ((extract-placeholders final (_ op . rest) (args ...) (body ...))
     (extract-placeholders final rest (args ... arg) (body ... arg op)))

    ((extract-placeholders final (arg op . rest) args (body ...))
     (extract-placeholders final rest args (body ... arg op)))

    ((extract-placeholders final (_) (args ...) (body ...))
     (extract-placeholders final () (args ... arg) (body ... arg)))

    ((extract-placeholders final (arg) args (body ...))
     (extract-placeholders final () args (body ... arg)))
    ))

(define-syntax (identity-syntax form)
  form)

(define-syntax (is . something)
  (extract-placeholders identity-syntax something () ()))

(define-syntax (isnt . something)
  (extract-placeholders not something () ()))

(define-syntax for
  (syntax-rules (in =>)
    ((for (key => value) in hash-map actions . *)
     (hash-for-each (lambda (key value) actions . *) hash-map))
    
    ((for x in list actions . *)
     (for-each (lambda (x) actions . *) list))))

(define (memoized proc)
  (let ((cache (make-weak-hash)))
    (lambda args
      (if (hash-has-key? cache args)
	  (apply values (hash-ref cache args))
	  (call-with-values (lambda () (apply proc args))
	    (lambda result
	      (hash-set! cache args result)
	      (apply values result)))))))

(define-syntax (define/memoized (name . args) . body)
  (define name (memoized (lambda args . body))))

(define (->string expression)
  (with-output-to-string
    (lambda ()
      (display expression))))

(define (out . messages)
  (for-each display messages)
  (newline)
  (flush-output))

(define-syntax (lambda# params body . *)
  (let ((default (lambda params body . *))
	(patches (make-hash)))
    (getter-with-setter
     (lambda args
       (if (hash-has-key? patches args)
	   (hash-ref patches args)
	   (apply default args)))
     (lambda args
       (hash-set! patches (drop-right args 1) (last args))))))

(define-syntax (define# (mapping . args) body . *)
  (define mapping (lambda# args body . *)))

(define (merge list-a list-b <)
  (cond ((null? list-a)
	 list-b)
	((null? list-b)
	 list-a)
	(else
	 (let ((`(,ha . ,ta) list-a)
               (`(,hb . ,tb) list-b))
	   (if (is ha < hb)
	       `(,ha . ,(merge ta list-b <))
	       `(,hb . ,(merge list-a tb <)))))))


(define-syntax (while condition actions * ...)
  (let loop ()
    (when condition
      actions * ... (loop))))
