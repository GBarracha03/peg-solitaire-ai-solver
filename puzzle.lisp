;;;; ====================================================================================
;;;; Ficheiro: puzzle.lisp
;;;; Descrição: Definição do domínio do Puzzle Solitário (Estados, Operadores, Heurísticas).
;;;; Autores: Gonçalo Barracha 202200187, Rodrigo Cardoso 202200197
;;;; ====================================================================================

(defparameter *nos-gerados* 0 "Contador global de nós gerados.")



;;; ------------------------------------------------------------------------------------
;;; Operadores e Lógica do Tabuleiro
;;; ------------------------------------------------------------------------------------

(defun operadores ()
  "Retorna a lista de nomes das funções operadoras."
  (list 'operador-ce 'operador-cd 'operador-cc 'operador-cb))

(defun linha (indice tabuleiro)
"Função que recebe um indíce e o tabuleiro e retorna a lista que representa a linha do tabueliro do indíce indicado"
(cond ((or (> indice 7) (< indice 1)) nil)
      ((null tabuleiro) (format T "Tabuleiro inválido!"))
      (T (nth (1- indice) tabuleiro))))

(defun coluna (indice tabuleiro)
"Função que recebe um indíce e o tabuleiro e retorna a lista que representa a coluna do tabuleiro do indíce indicado"
(cond ((or (> indice 7) (< indice 1)) nil)
      ((null tabuleiro) (format T "Tabuleiro inválido!"))
      (T (mapcar (lambda (linha) (nth (1- indice) linha)) tabuleiro))))

(defun celula (indice-horizontal indice-vertical tabuleiro)
  "Retorna o valor da célula (linha, coluna). Nil se inválido."
  (cond ((or (> indice-horizontal 7) (< indice-horizontal 1) (> indice-vertical 7) (< indice-vertical 1)) nil)
      ((null tabuleiro) (format T "Tabuleiro inválido"))
      (T (nth (1- indice-vertical) (nth (1- indice-horizontal) tabuleiro)))))

(defun celula-valida (indice-horizontal indice-vertical tabuleiro)
  "Verifica se uma coordenada corresponde a uma célula válida do tabuleiro."
  (cond ((null (celula indice-horizontal indice-vertical tabuleiro))nil)
      (T T)))

(defun substituir-posicao (indice lista valor)
  "Substitui o valor numa posição específica de uma lista (linha)."
  (cond ((or (> indice 7) (< indice 1)) (format T "Indíce ~d inválido, tem de indicar um indíce entre 1-7"))
      ((null lista) (format T "Lista inválida!"))
      ((or (> valor 1) (< valor 0)) (format T "Valor ~d inválido, valor tem de ser 0 ou 1"))
      ((= indice 1) (cons valor (cdr lista)))
      (T (cons (car lista) (substituir-posicao (1- indice) (cdr lista) valor)))
))

(defun substituir (indice-horizontal indice-vertical tabuleiro valor)
  "Substitui o valor numa célula (coluna h, linha v) do tabuleiro."
  (cond ((null tabuleiro) (format T "Tabuleiro inválido!"))
      ((null (celula-valida indice-horizontal indice-vertical tabuleiro)) (format T "Célula inválida!"))
      ((= indice-horizontal 1) (cons (substituir-posicao indice-vertical (linha indice-horizontal tabuleiro) valor) (cdr tabuleiro)))
      (T (cons (car tabuleiro) (substituir (1- indice-horizontal) indice-vertical (cdr tabuleiro) valor)))))



;;; ------------------------------------------------------------------------------------
;;; Operadores de Movimento
;;; ------------------------------------------------------------------------------------

(defun operador-cd (indice-horizontal indice-vertical tabuleiro)
  "Tenta mover a peça em (h,v) para a DIREITA (h, v+2)."
  (cond 
 ((or (> indice-horizontal 7) (< indice-horizontal 1) (> indice-vertical 7) (< indice-vertical 1))
  nil)
 ((or(null(celula-valida indice-horizontal (+ indice-vertical 2) tabuleiro))
     (null(celula-valida indice-horizontal indice-vertical tabuleiro))) 
  nil)
 ((and (= (celula indice-horizontal indice-vertical tabuleiro) 1)
       (= (celula indice-horizontal (1+ indice-vertical) tabuleiro) 1)
       (= (celula indice-horizontal (+ indice-vertical 2) tabuleiro) 0))
  (substituir indice-horizontal (+ indice-vertical 2)
                                    (substituir indice-horizontal (1+ indice-vertical)
                                               (substituir indice-horizontal indice-vertical tabuleiro 0) 0) 1))
  (T nil)))

(defun operador-ce (indice-horizontal indice-vertical tabuleiro)
  "Tenta mover a peça em (h,v) para a ESQUERDA (h, v-2)."
  (cond 
 ((or (> indice-horizontal 7) (< indice-horizontal 1) (> indice-vertical 7) (< indice-vertical 1))
  nil)
 ((or(null(celula-valida indice-horizontal (- indice-vertical 2) tabuleiro))
     (null(celula-valida indice-horizontal indice-vertical tabuleiro))) 
   nil)
 ((and (= (celula indice-horizontal indice-vertical tabuleiro) 1)
       (= (celula indice-horizontal (1- indice-vertical) tabuleiro) 1)
       (= (celula indice-horizontal (- indice-vertical 2) tabuleiro) 0))
  (substituir indice-horizontal (- indice-vertical 2)
                                    (substituir indice-horizontal (1- indice-vertical)
                                               (substituir indice-horizontal indice-vertical tabuleiro 0) 0) 1))
  (T nil)))

(defun operador-cc (indice-horizontal indice-vertical tabuleiro)
  "Tenta mover a peça em (h,v) para CIMA (h-2, v)."
  (cond 
 ((or (> indice-horizontal 7) (< indice-horizontal 1) (> indice-vertical 7) (< indice-vertical 1))
  nil)
 ((or(null(celula-valida (- indice-horizontal 2) indice-vertical tabuleiro))
     (null(celula-valida indice-horizontal indice-vertical tabuleiro))) nil)
 ((and (= (celula indice-horizontal indice-vertical tabuleiro) 1)
       (= (celula (1- indice-horizontal) indice-vertical tabuleiro) 1)
       (= (celula (- indice-horizontal 2)  indice-vertical tabuleiro) 0))
  (substituir (- indice-horizontal 2) indice-vertical
                                    (substituir (1- indice-horizontal) indice-vertical
                                               (substituir indice-horizontal indice-vertical tabuleiro 0) 0) 1))
  (T nil)))

(defun operador-cb (indice-horizontal indice-vertical tabuleiro)
  "Tenta mover a peça em (h,v) para BAIXO (h+2, v)."
  (cond 
 ((or (> indice-horizontal 7) (< indice-horizontal 1) (> indice-vertical 7) (< indice-vertical 1))
  nil)
 ((or(null(celula-valida (+ indice-horizontal 2) indice-vertical tabuleiro))
     (null(celula-valida indice-horizontal indice-vertical tabuleiro))) 
  nil)
 ((and (= (celula indice-horizontal indice-vertical tabuleiro) 1)
       (= (celula (1+ indice-horizontal) indice-vertical tabuleiro) 1)
       (= (celula (+ indice-horizontal 2)  indice-vertical tabuleiro) 0))
  (substituir (+ indice-horizontal 2) indice-vertical
                                    (substituir (1+ indice-horizontal) indice-vertical
                                               (substituir indice-horizontal indice-vertical tabuleiro 0) 0) 1))
  (T nil)))



;;; ------------------------------------------------------------------------------------
;;; Funções de Geração de Sucessores e Objetivos
;;; ------------------------------------------------------------------------------------

(defun no-solucaop (no)
  "Verifica se o tabuleiro é solução (tem exatamente 1 pino)."
  (let ((tabuleiro-solucao (no-estado no)))
    (cond 
      ((null tabuleiro-solucao) nil)
      (T (= 1 (apply #'+ 
                   (mapcar (lambda (linha) (count 1 linha)) 
                           tabuleiro-solucao)))))))

(defun gerar-todos-estados-filhos (estado-atual lista-operadores)
  "Gera todos os tabuleiros possíveis a partir de um estado, aplicando todos os operadores a todas as peças."
  (let ((lista-estados-filhos '()))
    (dotimes (i 7)
      (let ((linha (1+ i)))
        (dotimes (j 7)
          (let ((coluna (1+ j)))
            
            (when (and (numberp (celula coluna linha estado-atual))
                       (= (celula coluna linha estado-atual) 1))
              
              (dolist (op lista-operadores)
                (let ((resultado (funcall op coluna linha estado-atual)))
                  
                  (when (and resultado (listp resultado))
                    (push resultado lista-estados-filhos)

))))))))
    lista-estados-filhos))

(defun criar-no-sucessor (estado-novo no-pai &optional func-heuristica)
  "Cria um novo nó sucessor e incrementa o contador de nós gerados."
  (let* ((profundidade-old (no-profundidade no-pai))
         (h-val (if func-heuristica (funcall func-heuristica estado-novo) 0)))
     (incf *nos-gerados*)
    (cria-no estado-novo (+ profundidade-old 1) no-pai h-val)))

(defun sucessores (no-pai operadores func &optional func-heuristica dfs-depth)
  "Função principal que devolve a lista de nós sucessores para os algoritmos de procura."
  (cond 
    ((and (eql func 'dfs) dfs-depth (>= (no-profundidade no-pai) dfs-depth)) nil)
    
    (T
     (let ((estados-possiveis (gerar-todos-estados-filhos (no-estado no-pai) operadores)))
       
       (mapcar (lambda (estado) 
                 (criar-no-sucessor estado no-pai func-heuristica))
               estados-possiveis)))))



;;; ------------------------------------------------------------------------------------
;;; Heurísticas
;;; ------------------------------------------------------------------------------------

(defun heuristica(estado)
  "Heurística Base: Inverso do número de movimentos possíveis (quanto mais, melhor)."
(cond ((null estado) (format T "Estado inválido"))
      (T (/ 1 (1+ (length (gerar-todos-estados-filhos estado (operadores))))))
))

(defun heuristica-isolamento (estado)
  "Heurística 2: Penaliza número de pinos + pinos isolados."
  (let ((pinos 0)
        (isolados 0))
    (dotimes (i 7)
      (dotimes (j 7)
        (let ((v (nth j (nth i estado))))
          (when (eql v 1)
            (incf pinos)
            (let* ((up    (and (> i 0)     (nth j (nth (1- i) estado))))
                   (down  (and (< i 6)     (nth j (nth (1+ i) estado))))
                   (left  (and (> j 0)     (nth (1- j) (nth i estado))))
                   (right (and (< j 6)     (nth (1+ j) (nth i estado)))))
              (when (and (not (eql up 1))
                         (not (eql down 1))
                         (not (eql left 1))
                         (not (eql right 1)))
                (incf isolados)))))))
    (+ pinos isolados)))
