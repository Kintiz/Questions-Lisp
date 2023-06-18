#lang racket

; Meu nome

(define (erro-de-uso s)
  (string-append s " sufixo estiloso"))

(define (erro-uso2 s)
  (unless (string? s)
    (error 'erro-uso2 
           "esperava uma string, mas recebi ~a" s))
  (string-append s " sufixo estiloso"))


(define/contract (erro-uso3 s)
  (string? . -> . string?)
  (string-append s " sufixo estiloso"))

#|
(let ([cons (lambda (x y) (+ x y))])
     (cons 2 3))
|#

(define (rem* l1 l2 [f equal?])
  (if (null? l1)
      l2
      (rem* (rest l1)
            (rem-todos (first l1) l2 f)
            f)
      ))

(define (rem-todos e l f)
  (cond
    [(null? l) '()]
    [(f e (first l))
     (rem-todos e (rest l) f)]
    [else (cons (first l) (rem-todos e (rest l) f))]
    ))

(define (meu-map-v1 f l)
  (if (null? l)
      null
      (cons (f (first l))
            (meu-map-v1 f (rest l)))
      ))

(define (meu-map-v2 f . ll)
  (if (null? (first ll))
      null
      (cons (apply f (meu-map-v1 first ll))
            (apply meu-map-v2
                   f
                   (meu-map-v1 rest ll)))
      ))


(define (meu-foldl-v1 func init lista)
  (if (null? lista)
      init
      (meu-foldl-v1 func
                    (func (first lista) init)
                    (rest lista))))

(define (meu-foldl-v2 func init . ll)
  (if (null? (first ll))
      init
      (apply meu-foldl-v2
             func
             (apply func
                    (append (meu-map-v1 first ll)
                            (list init)))
             (meu-map-v1 rest ll))))


(define (encontre a l #:fc [f eqv?] #:key [c identity])
    (cond
      [(null? l) #f]
      [(f (c (first l)) a) (first l)]
      (else (encontre a (rest l) #:fc f #:key c))   
     )
  )


(define (mergesort l)
  (cond
    [(null? l) '()]
    [(null? (rest l)) l]
    [else
       (let-values ([(l1 l2) (divide-lista l)])
         (meu-merge (mergesort l1) (mergesort l2))
         )]
    ))

(define (divide-lista l [l1 '()] [l2 '()])
  (if (null? l)
      (values l1 l2)
      (divide-lista (rest l)
                    l2
                    (cons (first l) l1))
      ))

(define (divide-lista2 l [flg #t])
  (cond
    [(null? l) (values '() '())]
    [flg
       (let-values
           ([(l1 l2) (divide-lista2 (rest l) #f)])
           (values (cons (first l) l1)
                  l2))]
    [else
     (let-values
           ([(l1 l2) (divide-lista2 (rest l) #t)])
          (values l1
                  (cons (first l) l2)))]
    ))
     

                  
(define (meu-merge l1 l2)
  (cond
    [(null? l1) l2]
    [(null? l2) l1]
    [(<= (first l1) (first l2))
        (cons (first l1)
              (meu-merge (rest l1) l2))]
    [else
        (cons (first l2)
              (meu-merge l1 (rest l2)))]
    ))

  
; ==========-
(define (mergesort2 l
                    #:func [f <=]
                    #:key [k identity])
  (cond
    [(null? l) '()]
    [(null? (rest l)) l]
    [else
       (let-values ([(l1 l2) (divide-lista2 l)])
         (meu-merge2
            (mergesort2 l1 #:func f #:key k)
            (mergesort2 l2 #:func f #:key k)
            #:func f
            #:key k)
         )]
    ))

(define (meu-merge2 l1 l2
                    #:func [f <=]
                    #:key [k identity])
  (cond
    [(null? l1) l2]
    [(null? l2) l1]
    [(f (k (first l1)) (k (first l2)))
        (cons (first l1)
              (meu-merge2 (rest l1) l2
                          #:func f
                          #:key k))]
    [else
        (cons (first l2)
              (meu-merge2 l1 (rest l2)
                          #:func f
                          #:key k))]
    ))

; ========-
(define (remove-tudo-v3 e l #:fc [f eqv?] #:cc [c identity])
  (cond
    [(null? l) l]
    [(f e (c (first l)))
        (remove-tudo-v3 e (rest l) #:fc f #:cc c)]
    [else
        (cons (first l)
              (remove-tudo-v3 e (rest l) #:fc f #:cc c))]
    ))

(define (rem-enc e l #:fc [f eq?] #:cc [c identity])
  (let [(dado (remove-tudo-v3 e l #:fc f #:cc c))]
    (lambda(p)
      (cond
        [(eq? p 'r) dado]
        [(eq? p 'l) (length dado)]
        [(exact-nonnegative-integer? p) (list-ref dado p)])
     )
  ))


(define (cxr cmdString)
 (lambda (lista)
  (cond
    ; se a string está vazia, retorna a propria lista
    [(= 0 (string-length cmdString)) lista]
    ; se tem um 'a', chama car sobre a chamada recursiva cxr
    [(char=? #\a (string-ref cmdString 0))
     (car ((cxr (substring cmdString 
                           1 
                           (string-length cmdString)))
                           lista))]
    ; se tem um 'd', chama cdr sobre a chamada recursiva cxr
    [(char=? #\d (string-ref cmdString 0))
     (cdr ((cxr (substring cmdString 
                           1 
                           (string-length cmdString)))
                           lista))]
)))


(define (cria-oper o)
  (lambda (x y)
    (cond
      [(eq? o 'somatorio) (foldl + 0 (range x (+ y 1)))]
      [(eq? o 'mul) (* x y)]
      [(eq? o 'soma) (+ x y)]))
  )

  
(define cria-oper2
 (lambda (o)
  (lambda (x y)
    (cond
      [(eq? o 'somatorio) (foldl + 0 (range x (+ y 1)))]
      [(eq? o 'mul) (* x y)]
      [(eq? o 'soma) (+ x y)]))
  ))



