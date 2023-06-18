#lang racket
;;01
(printf  "1\n")

(define (concatenar1 l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (concatenar1 (cdr l1) l2))))
(concatenar1 '(a b c) '(d e f g h))
;;02
(printf  "2\n" )

(define (concatenarInv l1 l2)
  (cond ((null? l2) l1)
        (else (cons (car l2) (concatenarInv l1 (cdr l2))))))
(concatenarInv '(a b c) '(d e f g h))
;;03
(printf  "3\n" )

(define (concatenar2 ll)
  (define (concatenar-helper result lst)
    (cond
      [(null? lst) result]
      [(pair? (car lst))
       (concatenar-helper (append result (car lst)) (cdr lst))]
      [else (concatenar-helper result (cdr lst))]))
  
  (concatenar-helper '() ll))
(concatenar2 '((a b) (c) (d e f g)))
;;04
(printf  "4\n" )

(define (concatenar3 . lsts)
  (define (concatenar-helper lst result)
    (if (null? lst)
        result
        (concatenar-helper (cdr lst) (concatenate-lists result (car lst)))))

  (define (concatenate-lists lst1 lst2)
    (if (null? lst1)
        lst2
        (cons (car lst1) (concatenate-lists (cdr lst1) lst2))))

  (concatenar-helper lsts '()))
(concatenar3 '(a b) '(c) '(d e f g))
;;05
(printf  "5\n" )

(define (juntar l1 l2)
  (define (intercalar lst1 lst2 result)
    (cond
      ((null? lst1) (append result lst2))
      ((null? lst2) (append result lst1))
      (else (intercalar (cdr lst1) (cdr lst2) (append result (list (car lst1) (car lst2)))))))

  (intercalar l1 l2 '()))
(juntar '(a b c) '(d e f g h))
;;06
(printf  "6\n" )

(define (intercala n e1 e2)
  (define (helper count elem1 elem2 result)
    (cond
      ((= count 0) result)
      ((= count 1) (cons elem1 result))
      ((= count 2) (cons elem2 (cons elem1 result)))
      (else (helper (- count 2) elem1 elem2 (cons elem2 (cons elem1 result))))))

  (reverse (helper n e1 e2 '())))
(intercala 5 'x 'y)
;;07 \\\\\\\\\\\
(printf  "7\n" )

(define (intercala2 n . elements)
  (define m (length elements))
  
  (define (helper count elems result)
    (cond
      ((= count 0) result)
      ((null? elems) (helper count elements result))
      ((null? (cdr elems)) (helper (- count 1) elements (cons (car elems) result)))
      (else (helper (- count 1) (cdr elems) (cons (car elems) (cons (cadr elems) result))))))

  (reverse (helper n elements '())))

(intercala2 5 'x 'y 'z)
;;08
(printf  "8\n" )

(define (parear e l)
  (cond ((null? l) '())
        (else (cons (list e (car l)) (parear e (cdr l))))))

(parear 'x '(a b c))
;;09
(printf  "9\n" )

(define (pares l)
  (cond ((null? l) '())
        (else (append (gerar-pares (car l) (cdr l)) (pares (cdr l))))))

(define (gerar-pares e lst)
  (cond ((null? lst) '())
        (else (cons (list e (car lst)) (gerar-pares e (cdr lst))))))

(pares '(a b c d))
;;10
(printf  "10\n" )

(define (conjunto? l)
  (cond ((null? l) #t)
        ((member (car l) (cdr l)) #f)
        (else (conjunto? (cdr l)))))

(conjunto? '(a b c d))
(conjunto? '(a b c d b))
;;11
(printf  "11\n" )

(define (prefixo? l1 l2)
  (cond ((null? l1) #t)
        ((null? l2) #f)
        ((not (equal? (car l1) (car l2))) #f)
        (else (prefixo? (cdr l1) (cdr l2)))))

(prefixo? '(a b c) '(a b c d e f g))
(prefixo? '(a b c) '(a b f g))

;;12 \\\\\\\\\\\
(printf  "12\n" )

(define (subsequência? l1 l2)
  (cond ((null? l1) #t)
        ((null? l2) #f)
        ((equal? (car l1) (car l2)) (subsequência? (cdr l1) (cdr l2)))
        (else (subsequência? l1 (cdr l2)))))

(subsequência? '(a b c) '(d z a b c f g))
(subsequência? '(a b c) '(d z a b f c g))
;;13
(printf  "13\n" )

(define (iguais-lg? lg1 lg2)
  (cond
    ((and (null? lg1) (null? lg2)) #t)        ; Ambas as listas estão vazias, são iguais
    ((or (null? lg1) (null? lg2)) #f)         ; Uma lista está vazia e a outra não, são diferentes
    ((list? (car lg1))                       ; O primeiro elemento é uma lista
     (and (iguais-lg? (car lg1) (car lg2))    ; Recursivamente compara as sublistas
          (iguais-lg? (cdr lg1) (cdr lg2))))
    (else                                    ; O primeiro elemento não é uma lista
     (and (equal? (car lg1) (car lg2))        ; Compara os elementos
          (iguais-lg? (cdr lg1) (cdr lg2))))))
(iguais-lg? '(a (b c)) '(a (b c)))
(iguais-lg? '(a b c) '(d z a b f c g))
;;14
(printf  "14\n" )

(define (substitui-lg old new lg)
  (cond
    ((null? lg) '())                      ; Caso base: lista vazia, retorna uma lista vazia
    ((list? (car lg))                      ; O primeiro elemento é uma lista
     (cons (substitui-lg old new (car lg)) ; Recursivamente substitui na sublista
           (substitui-lg old new (cdr lg))))
    ((equal? (car lg) old)                  ; O primeiro elemento é igual a "old", substitui por "new"
     (cons new (substitui-lg old new (cdr lg))))
    (else                                  ; O primeiro elemento é diferente de "old"
     (cons (car lg) (substitui-lg old new (cdr lg))))))
(substitui-lg 'c 'manoel '(a (b c)))
;;15
(printf  "15\n" )

(define (aplanar lg)
  (cond
    ((null? lg) '())                      ; Caso base: lista vazia, retorna uma lista vazia
    ((list? (car lg))                      ; O primeiro elemento é uma lista
     (append (aplanar (car lg))            ; Recursivamente aplanar a sublista
             (aplanar (cdr lg))))
    (else                                  ; O primeiro elemento é um átomo
     (cons (car lg) (aplanar (cdr lg)))))) ; Adiciona o átomo à lista resultante

(aplanar '(a (b c (d)) (((e)))))
