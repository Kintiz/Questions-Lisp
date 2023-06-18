#lang racket
;; Júlio César Oliveira Rios

(provide
 CONJUNTO-VAZIO
 membro-p-n?
 conjunto-vazio?
 conjunto?
 cardinalidade-c
 iguais-c?
 intersecta?
 intersecao
 uniao
 dif-conj
 dif-sim
 intersecao-lc
 uniao-lc
 ocorre-uma-vez-lc
 dif-sim-lc)

(define CONJUNTO-VAZIO '())

;; (membro-p-n? e l)
;; (membro-p-n? any/c list?) -> boolean?
;; Testa se uma elemento E é membro de uma lista L
;; L é uma lista simples e E é qualquer coisa,
;; inclusive listas, strings e vetores
(define (membro-p-n? e l)
 (cond
 [(null? l) #f]
 [(equal? e (first l)) #t]
 [else
 (membro-p-n? e (rest l))]
 ))

;;1
;; (conjunto-vazio? c)
;; (conjunto-vazio? any/c) -> boolean?
;; Testa se uma elemento C é um conjunto vazio
;; C é qualquer coisa,
;; inclusive listas, strings e vetores
(define (conjunto-vazio? c)
  (if(null? c)#t #f))

;;2
;; (conjunto? e)
;; (conjunto? any/c) -> boolean?
;; Testa se uma elemento E é um conjunto
;; E é qualquer coisa,
;; inclusive listas, strings e vetores
(define (conjunto? e)
  (cond ((not (list? e)) #f)
        ((null? e) #t)
        ((member (car e) (cdr e)) #f)
        (else (conjunto? (cdr e)))))

;;3
;; (cardinalidade-c c)
;; (cardinalidade-c list) -> number
;; Verifica quantos elementos tem em C
;; C é uma lista
(define (cardinalidade-c c)
  (cond
    [(null? c) 0]
        [else (+ 1 (cardinalidade-c (cdr c)))]
        ))

;;4
;; (iguais-c? c1 c2)
;; (iguais-c? list? list?) -> boolean?
;; Verifica se duas listas são iguais
;; C1 e C2 são Listas
(define (iguais-c? c1 c2)
  (and (= (cardinalidade-c c1) (cardinalidade-c c2))
       (subset? c1 c2)))

(define (subset? c1 c2)
  (cond
    [(null? c1) #t]
    [(member (car c1) c2) (subset? (cdr c1) c2)]
    [else #f]))

;;5
;; (intersecta? c1 c2)
;; (intersecta? list? list?) -> boolean?
;; Verifica se algum dos elmentos das duas listas são iguais
;; C1 e C2 são Listas
(define (intersecta? c1 c2)
  (or (member-intersect? c1 c2)
      (member-intersect? c2 c1)))

(define (member-intersect? c1 c2)
  (cond
    [(null? c1) #f]
    [(member (car c1) c2) #t]
    [else (member-intersect? (cdr c1) c2)]))

;;6
;; (intersecao c1 c2)
;; (intersecao list? list?) -> boolean? || List?
;; Verifica se tem elmentos iguais nas duas listas e retorna os elementos iguais
;; C1 e C2 são Listas
(define (intersecao c1 c2)
  (or (member-intersected? c1 c2)
      (member-intersected? c2 c1)))

(define (member-intersected? c1 c2)
  (cond
    [(null? c1) #f]
    [(member (car c1) c2) c1]
    [else (member-intersected? (cdr c1) c2)]))

;;7
;; (uniao c1 c2)
;; (uniao list? list?) -> List?
;; Retorna todos os elementos das listas em uma unica sem repetir os elementos
;; C1 e C2 são Listas
(define (uniao c1 c2)
  (remover-duplicatas (append c1 c2)))

(define (remover-duplicatas lst)
  (cond
    [(null? lst) '()]
    [(member (car lst) (cdr lst)) (remover-duplicatas (cdr lst))]
    [else (cons (car lst) (remover-duplicatas (cdr lst)))]))

;;8
;; (dif-conj c1 c2)
;; (dif-conj list? list?) -> List?
;; Retorna os elementos da C1 que não tem na C2
;; C1 e C2 são Listas
(define (dif-conj c1 c2)
  (remover-duplicatas (dif-aux c1 c2)))

(define (dif-aux c1 c2)
  (cond ((null? c1) '())
        ((member (car c1) c2) (dif-aux (cdr c1) c2))
        (else (cons (car c1) (dif-aux (cdr c1) c2)))))

;;9
;; (dif-sim c1 c2)
;; (dif-sim list? list?) -> List?
;; Retorna os elementos da C1 que não tem na C2 e da C2 que não tem na C1
;; C1 e C2 são Listas
(define (dif-sim c1 c2)
  (remover-duplicatas (append (dif-aux c1 c2) (dif-aux c2 c1))))

;;10
;; (intersecao-lc . conjuntos)
;; (intersecao-lc ... list?) -> List?
;; Retorna os elementos que se repetem em todas as listas
;; . atribui a parametros de valor indeterminado conjuntos é uma lista
(define (intersecao-lc . conjuntos)
  (if (null? conjuntos)
      '()
      (foldl intersecao-l (car conjuntos) (cdr conjuntos))))

(define (intersecao-l c1 c2)
  (cond ((null? c1) '())
        ((member (car c1) c2) (cons (car c1) (intersecao-l (cdr c1) c2)))
        (else (intersecao-l (cdr c1) c2))))

;;11
;; (uniao-lc . conjuntos)
;; (uniao-lc ... list?) -> List?
;; Retorna todos os elementos das listas
;; . atribui a parametros de valor indeterminado conjuntos é uma lista
 (define (uniao-lc . conjuntos)
  (apply append (map remover-duplicatas conjuntos)))

;;12
;; (ocorre-uma-vez-lc . conjuntos)
;; (ocorre-uma-vez-lc ... list?) -> List?
;; Retorna o conjunto dos elementos que só ocorrem em um dos conjuntos
;; . atribui a parametros de valor indeterminado conjuntos é uma lista
;; não sei pq não funcionou >:0
(define (ocorre-uma-vez-lc . conjuntos)
  (let ((uniao (apply uniao-lc conjuntos)))
    (let ((intersecao (apply intersecao-lc conjuntos)))
      (dif-conj uniao intersecao))))

;;13
;; (dif-sim-lc . conjuntos)
;; (dif-sim-lc ... list?) -> List?
;; Retorna os elementos que não se repetem nas listas e os elementos que se repetem a partir de 3 listas
;; . atribui a parametros de valor indeterminado conjuntos é uma lista
;; sei e não sei pq não funcionou <:0
(define (dif-sim-lc . conjuntos)
  (let ((uniao (apply uniao-lc conjuntos)))
    (let ((intersecao (apply intersecao-lc conjuntos)))
      (dif-conj uniao intersecao))))


