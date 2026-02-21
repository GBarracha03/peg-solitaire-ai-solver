;;;; ====================================================================================
;;;; Ficheiro: procura.lisp
;;;; Descrição: Implementação dos algoritmos de procura (BFS, DFS, A*).
;;;;            Este módulo é independente do domínio do problema.
;;;; Autores: Gonçalo Barracha 202200187, Rodrigo Cardoso 202200197
;;;; ====================================================================================


(defparameter *nos-expandidos* 0 "Contador global de nós expandidos")



;;; ------------------------------------------------------------------------------------
;;; Estrutura genérica de nós e seletores
;;; ------------------------------------------------------------------------------------

(defun cria-no (tabuleiro &optional (g 0) (pai nil) (valor-heuristica 0))
  "Construtor de um nó.
   Retorna uma lista: (tabuleiro profundidade heuristica pai)"
  (list tabuleiro g valor-heuristica pai))

(defun no-estado (no)
  "Retorna o estado (tabuleiro) de um nó."
  (cond ((null no) (format T "O nó está vazio - estado"))
        (T (car no))))

(defun no-profundidade (no)
  "Retorna a profundidade (custo g) de um nó."
  (cond ((null no) (format T "O nó está vazio - profundidade"))
        (T (cadr no))))

(defun no-pai (no)
  "Retorna o nó pai."
  (cond ((null no) (format T "O nó está vazio - pai"))
        (T (cadddr no))))

(defun no-heuristica (no)
  "Retorna o valor heurístico (h) de um nó."
  (cond ((null no) (format T "O nó está vazio - heuristica"))
        (T (caddr no))))

(defun no-custo (no)
  "Calcula o custo total f(n) = g(n) + h(n)."
  (cond ((null no) (format T "O nó está vazio - custo"))
        (T (+ (no-heuristica no) (no-profundidade no)))))



;;; ------------------------------------------------------------------------------------
;;; Utilitários genéricos para listas de nós e abertos/fechados
;;; ------------------------------------------------------------------------------------

(defun abertos-bfs(lista-abertos lista-sucessores)
(append lista-abertos lista-sucessores))

(defun abertos-dfs(lista-abertos lista-sucessores)
(append lista-sucessores lista-abertos))

(defun ordenar-nos(nos)
  "Ordena nós por custo crescente."
(sort (copy-list nos) #'< :key #'(lambda (no) (no-custo no))))

(defun colocar-sucessores-em-abertos(lista-abertos lista-sucessores)
  (ordenar-nos (append lista-abertos lista-sucessores)))

(defun no-existep(no lista-nos func)
  "Verifica se um nó com o mesmo estado já existe na lista."
  (cond ((null lista-nos) nil)
      ((equal (no-estado no) (no-estado (car lista-nos))))
      (T (no-existep no (cdr lista-nos) func))))

(defun no-devolve-existep(no lista-nos func)
  "Retorna o nó da lista se existir um com o mesmo estado."
(cond ((null lista-nos) nil)
      ((equal (no-estado no) (no-estado (car lista-nos))) (car lista-nos))
      (T (no-devolve-existep no (cdr lista-nos) func))))



;;; ------------------------------------------------------------------------------------
;;; Estatísticas e Cálculo do Fator de Ramificação
;;; ------------------------------------------------------------------------------------

(defun penetrancia (l t-nos)
  "Calcula P = L / T. Recebe L (profundidade) e T-nos (nós gerados)."
  (cond 
    ((or (= l 0) (= t-nos 0)) 0)
    (T (float (/ l t-nos)))))

(defun calcular-polinomio (b L)
  "Soma de potências usada no cálculo do fator de ramificação."
  (let ((soma 0))
    (dotimes (i (+ L 1))
      (incf soma (expt b i)))
    soma))

(defun derivada-calcular-polinomio (b L)
  "Derivada do polinómio usado na iteração de Newton."
  (let ((soma 0))
    (dotimes (i L)
      (incf soma (* i (expt b (1- i)))))
    soma))

(defun fator-ramificacao-media (L T-nos)
  "Calcula o fator de ramificação média (B*) usando o método híbrido Bissecção + Newton."
  (cond 
    ((= L 0) 0)
    ((= T-nos L) 1)
    (t
     (bisection-with-Newton-Raphson
      #'(lambda (b) (- (calcular-polinomio b L) T-nos))
      #'(lambda (b) (derivada-calcular-polinomio b L))
      1.0
      (float T-nos)))))

(defun bisection-with-Newton-Raphson
       (f f-prime x-left x-right
        &key (accuracy 0.0001)
             (maximum-number-of-iterations 100))
  (let ((f-low (funcall f x-left))
        (f-high (funcall f x-right))
        df f-new x-low x-high rtsafe dxold dx)

    (when (>= (* f-low f-high) 0.0)
      (return-from bisection-with-Newton-Raphson nil))

    (if (< f-low 0.0)
        (setf x-low x-left x-high x-right)
        (setf x-high x-left x-low x-right))

    (setf rtsafe (* 0.5 (+ x-left x-right))
          dxold (abs (- x-right x-left))
          dx dxold
          f-new (funcall f rtsafe)
          df (funcall f-prime rtsafe))

    (dotimes (j maximum-number-of-iterations rtsafe)

      (cond
        ((or (<= (abs df) 0.0000001)
             (> (abs (* 2.0 f-new)) (abs (* dxold df))))
        
         (setf dxold dx
               dx (* 0.5 (- x-high x-low))
               rtsafe (+ x-low dx)))
        (t
        
         (setf dxold dx
               dx (/ f-new df)
               rtsafe (- rtsafe dx))))

      (when (< (abs dx) accuracy)
        (return (values rtsafe j)))

      (setf f-new (funcall f rtsafe)
            df (funcall f-prime rtsafe))

      (if (< f-new 0.0)
          (setf x-low rtsafe)
          (setf x-high rtsafe)))))

;;; ------------------------------------------------------------------------------------
;;; Algoritmo: Procura em Largura (BFS - Breadth-First Search)
;;; ------------------------------------------------------------------------------------
(defun bfs (no no-solucaop sucessores operadores &optional (abertos (list no)) fechados)
"Implementa a procura em largura (BFS).
   Argumentos:
     no            - Nó inicial.
     no-solucaop   - Função predicado que verifica se um nó é solução.
     sucessores    - Função que gera os sucessores de um nó.
     operadores    - Lista de operadores disponíveis.
     abertos       - Lista de nós a explorar.
     fechados      - Lista de nós já explorados.
   Retorna:
     O nó solução se encontrado, ou NIL se falhar."
  (cond ((null abertos)
          (format T "Lista de abertos vazia!"))

         (T
          (let* ((no-atual (car abertos)))
            (incf *nos-expandidos*)
                 (let* ((novos-fechados (cons no-atual fechados))
                 (novos-sucessores (funcall sucessores no-atual operadores 'bfs))
                 (sucessores-validos
                  (remove-if
                   (lambda (no-sucessor)
                     (or (no-existep no-sucessor fechados 'bfs) (no-existep no-sucessor abertos 'bfs)))
                   novos-sucessores))
                 (novos-abertos (abertos-bfs (cdr abertos) sucessores-validos))
                 (solucao-encontrada (find-if no-solucaop sucessores-validos)))
            
            (cond (solucao-encontrada solucao-encontrada)   
                  (T (bfs (car novos-abertos)
                          no-solucaop
                          sucessores
                          operadores
                          novos-abertos
                          novos-fechados))))))))


;;; ------------------------------------------------------------------------------------
;;; Algoritmo: Procura em Profundidade (DFS - Depth-First Search)
;;; ------------------------------------------------------------------------------------
(defun dfs (no no-solucaop sucessores operadores max-profundidade &optional (abertos (list no)) fechados)
"Implementa a procura em profundidade (DFS) com limite de profundidade.
   Argumentos:
     no            - Nó inicial.
     no-solucaop   - Função predicado que verifica se um nó é solução.
     sucessores    - Função que gera os sucessores de um nó.
     operadores    - Lista de operadores disponíveis.
     abertos       - Lista de nós a explorar.
     fechados      - Lista de nós já explorados.
     max-profundidade - Inteiro que define o limite máximo de exploração.
   Retorna:
     O nó solução ou NIL."
(cond ((null abertos)
     (format t "Lista de abertos vazia!"))

    ((> (no-profundidade (car abertos)) max-profundidade)
     (dfs (cadr abertos) no-solucaop sucessores operadores max-profundidade (cdr abertos) fechados))

    (T
     (let* ((no-atual (car abertos)))
       (incf *nos-expandidos*)
            (let* ((novos-sucessores (funcall sucessores no-atual operadores 'dfs nil  max-profundidade))
            (sucessores-validos
              (remove-if
                (lambda (no-sucessor)
                  (or (no-existep no-sucessor abertos 'dfs)
                      (no-existep no-sucessor fechados 'dfs)))
                novos-sucessores))
            (novos-abertos (abertos-dfs (cdr abertos) sucessores-validos))
            (solucao-encontrada (find-if no-solucaop sucessores-validos))
            (novos-fechados (cons no-atual fechados)))
     
    (cond (solucao-encontrada solucao-encontrada) 
       (T (dfs (car novos-abertos)
            no-solucaop
            sucessores
            operadores
            max-profundidade
            novos-abertos
            novos-fechados))))))))



;;; ------------------------------------------------------------------------------------
;;; Algoritmo: A* (A-Star)
;;; ------------------------------------------------------------------------------------
(defun a* (no no-solucaop sucessores operadores func-heuristica &optional (abertos (list no)) fechados)
"Implementa o algoritmo A* (Best-First Search com f(n) = g(n) + h(n)).
   Argumentos:
     no            - Nó inicial.
     no-solucaop   - Função predicado que verifica se um nó é solução.
     sucessores    - Função que gera os sucessores de um nó.
     operadores    - Lista de operadores disponíveis.
     abertos       - Lista de nós a explorar.
     fechados      - Lista de nós já explorados.
     func-heuristica - Função para estimar o custo até ao objetivo.
   Retorna:
     O nó solução ou NIL."
  (cond
    ((null abertos)
     (format t "Lista de abertos vazia!") nil)

    ((funcall no-solucaop (car abertos))
     (car abertos))

    (T
     (let* ((no-atual (car abertos)))
       
       (incf *nos-expandidos*)
       
       (let* ((novos-fechados (cons no-atual fechados))
              (novos-sucessores
               (funcall sucessores no-atual operadores 'a* func-heuristica))
              
              (sucessores-validos
               (remove-if
                (lambda (no-sucessor)
                  (let ((no-em-fechados (no-devolve-existep no-sucessor fechados 'a*))
                        (no-em-abertos  (no-devolve-existep no-sucessor abertos 'a*)))
                    (or
                     (and no-em-fechados 
                          (<= (no-custo no-em-fechados) (no-custo no-sucessor)))
                     (and no-em-abertos 
                          (<= (no-custo no-em-abertos) (no-custo no-sucessor))))))
                novos-sucessores))
              
              (novos-abertos
               (colocar-sucessores-em-abertos (cdr abertos) sucessores-validos)))
         
         (a* (car novos-abertos)
             no-solucaop
             sucessores
             operadores
             func-heuristica
             novos-abertos
             novos-fechados))))))