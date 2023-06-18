#lang racket

;Marivane Souza Barbosa

Lista de exercícios 2

QUESTÃO 1
#lang racket

;; Verifica se um conjunto é vazio
(define (conjunto-vazio? C)
  (null? C))

(provide conjunto-vazio?)

QUESTÃO 2
#lang racket

;; Verifica se um valor é um conjunto
(define (conjunto? E)
  (and (list? E) (not (pair? E))))

(provide conjunto?)

QUESTÃO 3
#lang racket

;; Verifica se um valor é um conjunto
(define (conjunto? E)
  (and (list? E) (not (pair? E))))

;; Retorna a cardinalidade do conjunto C
(define (cardinalidade-c C)
  (cond
    [(conjunto-vazio? C) 0]
    [else (length C)]))

;; Verifica se o conjunto C é vazio
(define (conjunto-vazio? C)
  (null? C))

(provide conjunto? Cardinalidade-c conjunto-vazio?)

QUESTÃO 4
#lang racket

;; Verifica se um valor é um conjunto
(define (conjunto? E)
  (and (list? E) (not (pair? E))))

;; Retorna a cardinalidade do conjunto C
(define (cardinalidade-c C)
  (cond
    [(conjunto-vazio? C) 0]
    [else (length C)]))

;; Verifica se o conjunto C é vazio
(define (conjunto-vazio? C)
  (null? C))

;; Verifica se os conjuntos C1 e C2 são iguais
(define (iguais-c? C1 C2)
  (and (subset? C1 C2) (subset? C2 C1)))

;; Verifica se todos os elementos de A estão em B
(define (subset? A B)
  (cond
    [(conjunto-vazio? A) #t]
    [else (and (memoria? (car A) B) (subset? (cdr A) B))]))

;; Verifica se o elemento E está em A
(define (memoria? E A)
  (cond
    [(conjunto-vazio? A) #f]
    [(equal? E (car A)) #t]
    [else (memoria? E (cdr A))]))

(provide conjunto? cardinalidade-c conjunto-vazio? iguais-c?)

QUESTÃO 5
#lang racket

;; Verifica se um valor é um conjunto
(define (conjunto? E)
  (and (list? E) (not (pair? E))))

;; Retorna a cardinalidade do conjunto C
(define (cardinalidade-c C)
  (cond
    [(conjunto-vazio? C) 0]
    [else (length C)]))

;; Verifica se o conjunto C é vazio
(define (conjunto-vazio? C)
  (null? C))

;; Verifica se os conjuntos C1 e C2 são iguais
(define (iguais-c? C1 C2)
  (and (subset? C1 C2) (subset? C2 C1)))

;; Verifica se os conjuntos C1 e C2 têm elementos em comum
(define (intersecta? C1 C2)
  (cond
    [(conjunto-vazio? C1) #f]
    [(memoria? (car C1) C2) #t]
    [else (intersecta? (cdr C1) C2)]))

;; Verifica se todos os elementos de A estão em B
(define (subset? A B)
  (cond
    [(conjunto-vazio? A) #t]
    [else (and (memoria? (car A) B) (subset? (cdr A) B))]))

;; Verifica se o elemento E está em A
(define (memoria? E A)
  (cond
    [(conjunto-vazio? A) #f]
    [(equal? E (car A)) #t]
    [else (memoria? E (cdr A))]))

(provide conjunto? Cardinalidade-c conjunto-vazio? Iguais-c? intersecta

QUESTÃO 6
#lang racket

;; Verifica se um valor é um conjunto
(define (conjunto? E)
  (and (list? E) (not (pair? E))))

;; Retorna a cardinalidade do conjunto C
(define (cardinalidade-c C)
  (cond
    [(conjunto-vazio? C) 0]
    [else (length C)]))

;; Verifica se o conjunto C é vazio
(define (conjunto-vazio? C)
  (null? C))

;; Verifica se os conjuntos C1 e C2 são iguais
(define (iguais-c? C1 C2)
  (and (subset? C1 C2) (subset? C2 C1)))

;; Verifica se os conjuntos C1 e C2 têm elementos em comum
(define (intersecta? C1 C2)
  (cond
    [(conjunto-vazio? C1) #f]
    [(memoria? (car C1) C2) #t]
    [else (intersecta? (cdr C1) C2)]))

;; Retorna a interseção entre os conjuntos C1 e C2
(define (intersecao C1 C2)
  (cond
    [(conjunto-vazio? C1) ‘()]
    [(memoria? (car C1) C2)
     (cons (car C1) (intersecao (cdr C1) C2))]
    [else (intersecao (cdr C1) C2)]))

;; Verifica se todos os elementos de A estão em B
(define (subset? A B)
  (cond
    [(conjunto-vazio? A) #t]
    [else (and (memoria? (car A) B) (subset? (cdr A) B))]))

;; Verifica se o elemento E está em A
(define (memoria? E A)
  (cond
    [(conjunto-vazio? A) #f]
    [(equal? E (car A)) #t]
    [else (memoria? E (cdr A))]))

(provide conjunto? Cardinalidade-c conjunto-vazio? Iguais-c? intersecta? Intersecao)

QUESTÃO 7
#lang racket

;; Verifica se um valor é um conjunto
(define (conjunto? E)
  (and (list? E) (not (pair? E))))

;; Retorna a cardinalidade do conjunto C
(define (cardinalidade-c C)
  (cond
    [(conjunto-vazio? C) 0]
    [else (length C)]))

;; Verifica se o conjunto C é vazio
(define (conjunto-vazio? C)
  (null? C))

;; Verifica se os conjuntos C1 e C2 são iguais
(define (iguais-c? C1 C2)
  (and (subset? C1 C2) (subset? C2 C1)))

;; Verifica se os conjuntos C1 e C2 têm elementos em comum
(define (intersecta? C1 C2)
  (cond
    [(conjunto-vazio? C1) #f]
    [(memoria? (car C1) C2) #t]
    [else (intersecta? (cdr C1) C2)]))

;; Retorna a interseção entre os conjuntos C1 e C2
(define (intersecao C1 C2)
  (cond
    [(conjunto-vazio? C1) ‘()]
    [(memoria? (car C1) C2)
     (cons (car C1) (intersecao (cdr C1) C2))]
    [else (intersecao (cdr C1) C2)]))

;; Retorna a união entre os conjuntos C1 e C2
(define (uniao C1 C2)
  (append C1 (diferenca C2 C1)))

;; Verifica se todos os elementos de A estão em B
(define (subset? A B)
  (cond
    [(conjunto-vazio? A) #t]
    [else (and (memoria? (car A) B) (subset? (cdr A) B))]))

;; Verifica se o elemento E está em A
(define (memoria? E A)
  (cond
    [(conjunto-vazio? A) #f]
    [(equal? E (car A)) #t]
    [else (memoria? E (cdr A))]))

(provide conjunto? Cardinalidade-c conjunto-vazio? Iguais-c? intersecta? Intersecao uniao)

QUESTÃO 8
#lang racket

;; Verifica se um valor é um conjunto
(define (conjunto? E)
  (and (list? E) (not (pair? E))))

;; Retorna a cardinalidade do conjunto C
(define (cardinalidade-c C)
  (cond
    [(conjunto-vazio? C) 0]
    [else (length C)]))

;; Verifica se o conjunto C é vazio
(define (conjunto-vazio? C)
  (null? C))

;; Verifica se os conjuntos C1 e C2 são iguais
(define (iguais-c? C1 C2)
  (and (subset? C1 C2) (subset? C2 C1)))

;; Verifica se os conjuntos C1 e C2 têm elementos em comum
(define (intersecta? C1 C2)
  (cond
    [(conjunto-vazio? C1) #f]
    [(memoria? (car C1) C2) #t]
    [else (intersecta? (cdr C1) C2)]))

;; Retorna a interseção entre os conjuntos C1 e C2
(define (intersecao C1 C2)
  (cond
    [(conjunto-vazio? C1) ‘()]
    [(memoria? (car C1) C2)
     (cons (car C1) (intersecao (cdr C1) C2))]
    [else (intersecao (cdr C1) C2)]))

;; Retorna a união entre os conjuntos C1 e C2
(define (uniao C1 C2)
  (append C1 (diferenca C2 C1)))

;; Retorna a diferença entre os conjuntos C1 e C2
(define (dif-conj C1 C2)
  (cond
    [(conjunto-vazio? C1) ‘()]
    [(memoria? (car C1) C2)
     (dif-conj (cdr C1) C2)]
    [else (cons (car C1) (dif-conj (cdr C1) C2))]))

;; Verifica se todos os elementos de A estão em B
(define (subset? A B)
  (cond
    [(conjunto-vazio? A) #t]
    [else (and (memoria? (car A) B) (subset? (cdr A) B))]))

;; Verifica se o elemento E está em A
(define (memoria? E A)
  (cond
    [(conjunto-vazio? A) #f]
    [(equal? E (car A)) #t]
    [else (memoria? E (cdr A))]))

(provide conjunto? Cardinalidade-c conjunto-vazio? Iguais-c? intersecta? Intersecao uniao dif-conj)

QUESTÃO 9
#lang racket

;; Verifica se um valor é um conjunto
(define (conjunto? E)
  (and (list? E) (not (pair? E))))

;; Retorna a cardinalidade do conjunto C
(define (cardinalidade-c C)
  (cond
    [(conjunto-vazio? C) 0]
    [else (length C)]))

;; Verifica se o conjunto C é vazio
(define (conjunto-vazio? C)
  (null? C))

;; Verifica se os conjuntos C1 e C2 são iguais
(define (iguais-c? C1 C2)
  (and (subset? C1 C2) (subset? C2 C1)))

;; Verifica se os conjuntos C1 e C2 têm elementos em comum
(define (intersecta? C1 C2)
  (cond
    [(conjunto-vazio? C1) #f]
    [(memoria? (car C1) C2) #t]
    [else (intersecta? (cdr C1) C2)]))

;; Retorna a interseção entre os conjuntos C1 e C2
(define (intersecao C1 C2)
  (cond
    [(conjunto-vazio? C1) ‘()]
    [(memoria? (car C1) C2)
     (cons (car C1) (intersecao (cdr C1) C2))]
    [else (intersecao (cdr C1) C2)]))

;; Retorna a união entre os conjuntos C1 e C2
(define (uniao C1 C2)
  (append C1 (dif-conj C2 C1)))

;; Retorna a diferença entre os conjuntos C1 e C2
(define (dif-conj C1 C2)
  (cond
    [(conjunto-vazio? C1) ‘()]
    [(memoria? (car C1) C2)
     (dif-conj (cdr C1) C2)]
    [else (cons (car C1) (dif-conj (cdr C1) C2))]))

;; Retorna a diferença simétrica entre os conjuntos C1 e C2
(define (dif-sim C1 C2)
  (uniao (dif-conj C1 C2) (dif-conj C2 C1)))

;; Verifica se todos os elementos de A estão em B
(define (subset? A B)
  (cond
    [(conjunto-vazio? A) #t]
    [else (and (memoria? (car A) B) (subset? (cdr A) B))]))

;; Verifica se o elemento E está em A
(define (memoria? E A)
  (cond
    [(conjunto-vazio? A) #f]
    [(equal? E (car A)) #t]
    [else (memoria? E (cdr A))]))

(provide conjunto? Cardinalidade-c conjunto-vazio? Iguais-c? intersecta? Intersecao uniao dif-conj dif-sim)

QUESTÃO 10
#lang racket

;; Verifica se um valor é um conjunto
(define (conjunto? E)
  (and (list? E) (not (pair? E))))

;; Retorna a cardinalidade do conjunto C
(define (cardinalidade-c C)
  (cond
    [(conjunto-vazio? C) 0]
    [else (length C)]))

;; Verifica se o conjunto C é vazio
(define (conjunto-vazio? C)
  (null? C))

;; Verifica se os conjuntos C1 e C2 são iguais
(define (iguais-c? C1 C2)
  (and (subset? C1 C2) (subset? C2 C1)))

;; Verifica se os conjuntos C1 e C2 têm elementos em comum
(define (intersecta? C1 C2)
  (cond
    [(conjunto-vazio? C1) #f]
    [(memoria? (car C1) C2) #t]
    [else (intersecta? (cdr C1) C2)]))

;; Retorna a interseção entre os conjuntos C1 e C2
(define (intersecao C1 C2)
  (cond
    [(conjunto-vazio? C1) ‘()]
    [(memoria? (car C1) C2)
     (cons (car C1) (intersecao (cdr C1) C2))]
    [else (intersecao (cdr C1) C2)]))

;; Retorna a união entre os conjuntos C1 e C2
(define (uniao C1 C2)
  (append C1 (dif-conj C2 C1)))

;; Retorna a diferença entre os conjuntos C1 e C2
(define (dif-conj C1 C2)
  (cond
    [(conjunto-vazio? C1) ‘()]
    [(memoria? (car C1) C2)
     (dif-conj (cdr C1) C2)]
    [else (cons (car C1) (dif-conj (cdr C1) C2))]))

;; Retorna a diferença simétrica entre os conjuntos C1 e C2
(define (dif-sim C1 C2)
  (uniao (dif-conj C1 C2) (dif-conj C2 C1)))

;; Faz a interseção entre todos os conjuntos passados como argumentos
(define (intersecao-lc . conjuntos)
  (cond
    [(null? Conjuntos) ‘()]
    [(null? (cdr conjuntos)) (car conjuntos)]
    [else (apply intersecao (car conjuntos) (apply intersecao-lc (cdr conjuntos)))]))

;; Verifica se todos os elementos de A estão em B
(define (subset? A B)
  (cond
    [(conjunto-vazio? A) #t]
    [else (and (memoria? (car A) B) (subset? (cdr A) B))]))

;; Verifica se o elemento E está em A
(define (memoria? E A)
  (cond
    [(conjunto-vazio? A) #f]
    [(equal? E (car A)) #t]
    [else (memoria? E (cdr A))]))

(provide conjunto? Cardinal

QUESTÃO 11
#lang racket

;; Verifica se um valor é um conjunto
(define (conjunto? E)
  (and (list? E) (not (pair? E))))

;; Retorna a cardinalidade do conjunto C
(define (cardinalidade-c C)
  (cond
    [(conjunto-vazio? C) 0]
    [else (length C)]))

;; Verifica se o conjunto C é vazio
(define (conjunto-vazio? C)
  (null? C))

;; Verifica se os conjuntos C1 e C2 são iguais
(define (iguais-c? C1 C2)
  (and (subset? C1 C2) (subset? C2 C1)))

;; Verifica se os conjuntos C1 e C2 têm elementos em comum
(define (intersecta? C1 C2)
  (cond
    [(conjunto-vazio? C1) #f]
    [(memoria? (car C1) C2) #t]
    [else (intersecta? (cdr C1) C2)]))

;; Retorna a interseção entre os conjuntos C1 e C2
(define (intersecao C1 C2)
  (cond
    [(conjunto-vazio? C1) ‘()]
    [(memoria? (car C1) C2)
     (cons (car C1) (intersecao (cdr C1) C2))]
    [else (intersecao (cdr C1) C2)]))

;; Retorna a união entre os conjuntos C1 e C2
(define (uniao C1 C2)
  (append C1 (dif-conj C2 C1)))

;; Retorna a diferença entre os conjuntos C1 e C2
(define (dif-conj C1 C2)
  (cond
    [(conjunto-vazio? C1) ‘()]
    [(memoria? (car C1) C2)
     (dif-conj (cdr C1) C2)]
    [else (cons (car C1) (dif-conj (cdr C1) C2))]))

;; Retorna a diferença simétrica entre os conjuntos C1 e C2
(define (dif-sim C1 C2)
  (uniao (dif-conj C1 C2) (dif-conj C2 C1)))

;; Faz a união entre todos os conjuntos passados como argumentos
(define (uniao-lc . conjuntos)
  (cond
    [(null? Conjuntos) ‘()]
    [(null? (cdr conjuntos)) (car conjuntos)]
    [else (apply uniao (car conjuntos) (apply uniao-lc (cdr conjuntos)))]))

;; Faz a interseção entre todos os conjuntos passados como argumentos
(define (intersecao-lc . conjuntos)
  (cond
    [(null? Conjuntos) ‘()]
    [(null? (cdr conjuntos)) (car conjuntos)]
    [else (apply intersecao (car conjuntos) (apply intersecao-lc (cdr conjuntos)))]))

;; Verifica se todos os elementos de A estão em B
(define (subset? A B)
  (cond
    [(conjunto-vazio? A) #t]
    [else (and (memoria? (car A)

QUESTÃO 12
#lang racket

;; Verifica se um valor é um conjunto
(define (conjunto? E)
  (and (list? E) (not (pair? E))))

;; Retorna a cardinalidade do conjunto C
(define (cardinalidade-c C)
  (cond
    [(conjunto-vazio? C) 0]
    [else (length C)]))

;; Verifica se o conjunto C é vazio
(define (conjunto-vazio? C)
  (null? C))

;; Verifica se os conjuntos C1 e C2 são iguais
(define (iguais-c? C1 C2)
  (and (subset? C1 C2) (subset? C2 C1)))

;; Verifica se os conjuntos C1 e C2 têm elementos em comum
(define (intersecta? C1 C2)
  (cond
    [(conjunto-vazio? C1) #f]
    [(memoria? (car C1) C2) #t]
    [else (intersecta? (cdr C1) C2)]))

;; Retorna a interseção entre os conjuntos C1 e C2
(define (intersecao C1 C2)
  (cond
    [(conjunto-vazio? C1) ‘()]
    [(memoria? (car C1) C2)
     (cons (car C1) (intersecao (cdr C1) C2))]
    [else (intersecao (cdr C1) C2)]))

;; Retorna a união entre os conjuntos C1 e C2
(define (uniao C1 C2)
  (append C1 (dif-conj C2 C1)))

;; Retorna a diferença entre os conjuntos C1 e C2
(define (dif-conj C1 C2)
  (cond
    [(conjunto-vazio? C1) ‘()]
    [(memoria? (car C1) C2)
     (dif-conj (cdr C1) C2)]
    [else (cons (car C1) (dif-conj (cdr C1) C2))]))

;; Retorna a diferença simétrica entre os conjuntos C1 e C2
(define (dif-sim C1 C2)
  (uniao (dif-conj C1 C2) (dif-conj C2 C1)))

;; Faz a união entre todos os conjuntos passados como argumentos
(define (uniao-lc . conjuntos)
  (cond
    [(null? Conjuntos) ‘()]
    [(null? (cdr conjuntos)) (car conjuntos)]
    [else (apply uniao (car conjuntos) (apply uniao-lc (cdr conjuntos)))]))

;; Faz a interseção entre todos os conjuntos passados como argumentos
(define (intersecao-lc . conjuntos)
  (cond
    [(null? Conjuntos) ‘()]
    [(null? (cdr conjuntos)) (car conjuntos)]
    [else (apply intersecao (car conjuntos) (apply intersecao-lc (cdr conjuntos)))]))

;; Retorna o conjunto dos elementos que ocorrem em apenas um dos conjuntos
(define (ocorre-uma-vez-lc . conjuntos)
  (define (auxiliar conjuntos resultado)
   
QUESTÃO 13
#lang racket

;; Verifica se um valor é um conjunto
(define (conjunto? E)
  (and (list? E) (not (pair? E))))

;; Retorna a cardinalidade do conjunto C
(define (cardinalidade-c C)
  (cond
    [(conjunto-vazio? C) 0]
    [else (length C)]))

;; Verifica se o conjunto C é vazio
(define (conjunto-vazio? C)
  (null? C))

;; Verifica se os conjuntos C1 e C2 são iguais
(define (iguais-c? C1 C2)
  (and (subset? C1 C2) (subset? C2 C1)))

;; Verifica se os conjuntos C1 e C2 têm elementos em comum
(define (intersecta? C1 C2)
  (cond
    [(conjunto-vazio? C1) #f]
    [(memoria? (car C1) C2) #t]
    [else (intersecta? (cdr C1) C2)]))

;; Retorna a interseção entre os conjuntos C1 e C2
(define (intersecao C1 C2)
  (cond
    [(conjunto-vazio? C1) ‘()]
    [(memoria? (car C1) C2)
     (cons (car C1) (intersecao (cdr C1) C2))]
    [else (intersecao (cdr C1) C2)]))

;; Retorna a união entre os conjuntos C1 e C2
(define (uniao C1 C2)
  (append C1 (dif-conj C2 C1)))

;; Retorna a diferença entre os conjuntos C1 e C2
(define (dif-conj C1 C2)
  (cond
    [(conjunto-vazio? C1) ‘()]
    [(memoria? (car C1) C2)
     (dif-conj (cdr C1) C2)]
    [else (cons (car C1) (dif-conj (cdr C1) C2))]))

;; Retorna a diferença simétrica entre os conjuntos C1 e C2
(define (dif-sim C1 C2)
  (uniao (dif-conj C1 C2) (dif-conj C2 C1)))

;; Faz a união entre todos os conjuntos passados como argumentos
(define (uniao-lc . conjuntos)
  (cond
    [(null? Conjuntos) ‘()]
    [(null? (cdr conjuntos)) (car conjuntos)]
    [else (apply uniao (car conjuntos) (apply uniao-lc (cdr conjuntos)))]))

;; Faz a interseção entre todos os conjuntos passados como argumentos
(define (intersecao-lc . conjuntos)
  (cond
    [(null? Conjuntos) ‘()]
    [(null? (cdr conjuntos)) (car conjuntos)]
    [else (apply intersecao (car conjuntos) (apply intersecao-lc (cdr conjuntos)))]))

;; Retorna a diferença simétrica entre todos os conjuntos
