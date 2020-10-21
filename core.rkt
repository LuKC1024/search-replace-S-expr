#lang racket
(require (only-in racket/hash
                  hash-union))
(require rackunit)

(define (atom? e)
  (or (number? e)
      (symbol? e)
      (string? e)))

;; match-s-expr : Pattern × SExpr → #f ∪ (Hash Id SExpr)
(define (consume-e p e)
  (cond
    [(atom? p) (and (eqv? p e) (hash))]
    [(null? p) (and (null? e) (hash))]
    [(pair? p)
     (case (car p)
       [(unquote)
        (match (cdr p)
          [`(,x) #:when (symbol? x) (hash x e)]
          [`(quote ,e*) (and (equal? e* e) (hash))]
          [_ (error 'match-s-expr "unsupported pattern ~a" p)])]
       [else
        (and (pair? e)
             (let ([h1 (consume-e (car p) (car e))]
                   [h2 (consume-e (cdr p) (cdr e))])
               (and h1 h2
                    (hash-union h1 h2 
                                #:combine/key
                                (lambda (k a b)
                                  (error 'match-s-expr "identifier ~a is bound twice" k))))))])]
    [else (error 'match-s-expr "unsupported pattern ~a" p)]))

(check-equal? (consume-e '() '()) (hash))
(check-equal? (consume-e 'a 'a) (hash))
(check-equal? (consume-e 'a 'b) #f)
(check-equal? (consume-e '(,a ,b) '(1 2)) #hash((a . 1) (b . 2)))
(check-equal? (consume-e '(,a . ,b) '(1 2)) #hash((a . 1) (b . (2))))
(check-equal? (consume-e '(,a b) '(1 2)) #f)

(define (produce-e h p)
  (cond
    [(atom? p) p]
    [(null? p) p]
    [(pair? p)
     (case (car p)
       [(unquote)
        (match (cdr p)
          [`(,x) #:when (symbol? x)
           (hash-ref h x (lambda () (error 'produce-s-expr "unbound identifier ~a" x)))]
          [`(quote ,e*) e*]
          [_ (error 'produce-s-expr "unsupported pattern ~a" p)])]
       [else
        (cons (produce-e h (car p))
              (produce-e h (cdr p)))])]
    [else (error 'produce-s-expr "unsupported pattern ~a" p)]))

(check-equal? (produce-e #hash((a . 1) (b . 2)) '(,b ,a)) '(2 1))
(check-equal? (produce-e #hash((a . 1) (b . 2)) '(,a ,b ,a)) '(1 2 1))

(define (search-replace pc pp e)
  (cond
    [(consume-e pc e)
     => (lambda (h) (produce-e h pp))]
    [(pair? e)
     (cons (search-replace pc pp (car e))
           (search-replace pc pp (cdr e)))]
    [else e]))