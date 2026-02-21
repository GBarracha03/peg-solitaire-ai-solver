;;;; ====================================================================================
;;;; Ficheiro: projeto.lisp
;;;; Descrição: Interface com o utilizador, gestão de ficheiros e função Main.
;;;; Autores: Gonçalo Barracha 202200187, Rodrigo Cardoso 202200197
;;;; ====================================================================================

(defparameter *caminho-problemas* "problemas.dat")
(defparameter *caminho-log* "log.dat")

(load "procura.lisp")
(load "puzzle.lisp")



;;; ------------------------------------------------------------------------------------
;;; Leitura e Interface
;;; ------------------------------------------------------------------------------------
(defun ler-e-escolher-problema ()
  "Lê os problemas do ficheiro e mostra-os graficamente no terminal."
  (if (probe-file *caminho-problemas*)
      (let ((todos-problemas nil))
        (with-open-file (f *caminho-problemas* :direction :input)
          (loop for problema = (read f nil 'eof)
                until (eq problema 'eof)
                do (push problema todos-problemas)))

        (setf todos-problemas (reverse todos-problemas))

        (let ((total (length todos-problemas)))
          (cond
            ((= total 0)
             (format t "Ficheiro de problemas está vazio!~%")
             nil)

            (t
             (format t "~%============================================~%")
             (format t "          PROBLEMAS DISPONÍVEIS~%")
             (format t "============================================~%~%")

             
             (loop for tabuleiro in todos-problemas
      for idx from 1 do
        (format t "[~D]~%" idx)
        (imprimir-tabuleiro tabuleiro t)
        (format t "~%"))

             (format t "--------------------------------------------~%")
             (format t "Escolha o problema (1-~D): " total)

             (let ((escolha (read)))
               (if (and (integerp escolha) (> escolha 0) (<= escolha total))
                   (nth (1- escolha) todos-problemas)
                   (progn
                     (format t "Escolha inválida. A sair.~%")
                     nil)))))))

      (progn
        (format t "Ficheiro de problemas não encontrado!~%")
        nil)))



(defun ler-algoritmo ()
  "Menu visual para escolher o algoritmo de procura."
  (format t "~%============================================~%")
  (format t "      SELECIONE O ALGORITMO DE PROCURA~%")
  (format t "============================================~%")
  (format t "  [1]  Procura em Largura      (BFS)~%")
  (format t "  [2]  Procura em Profundidade (DFS)~%")
  (format t "  [3]  Procura Informada       (A*)~%")
  (format t "--------------------------------------------~%")
  (format t "Opção: ")
  (let ((r (read)))
    (cond ((= r 1) 'bfs)
          ((= r 2) 'dfs)
          (t 'a*))))



(defun ler-profundidade ()
  "Menu de input de profundidade."
  (format t "~%--------------------------------------------~%")
  (format t "Profundidade limite para DFS: ")
  (read))



(defun ler-heuristica ()
  "Menu para escolher heurística."
  (format t "~%============================================~%")
  (format t "        SELECIONE A HEURÍSTICA~%")
  (format t "============================================~%")
  (format t "  [1]  Heurística padrão~%")
  (format t "  [2]  Heurística de isolamento~%")
  (format t "--------------------------------------------~%")
  (format t "Opção: ")

  (let ((e (read)))
    (cond
      ((= e 1) #'heuristica)
      ((= e 2) #'heuristica-isolamento)
      (t
       (format t "Opção inválida. Usando heurística padrão.~%")
       #'heuristica))))




;;; ------------------------------------------------------------------------------------
;;; Escrita e Main
;;; ------------------------------------------------------------------------------------

(defun obter-caminho (no)
  "Reconstrói o caminho desde a raiz até ao nó."
  (cond ((null no) nil)
        (T (cons no (obter-caminho (no-pai no))))))

(defun imprimir-tabuleiro (tabuleiro stream)
  "Imprime um tabuleiro de forma legível no ficheiro especificado."
  (format stream "+-------------------------------+~%")
  (dolist (linha tabuleiro)
    (format stream "| ")
    (dolist (cel linha)
      (format stream "~A " (if (null cel) "." cel)))
    (format stream "|~%"))
  (format stream "+-------------------------------+~%"))


(defun escrever-solucao (no algoritmo tempo &optional (ficheiro *caminho-log*))
  "Escreve relatório final de execução com formatação visual."
  (with-open-file (f ficheiro
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)

    (format f "~%---------------------------------------------------~%")
    (format f "              RELATORIO DA EXECUCAO~%")
    (format f "---------------------------------------------------~%")
    (format f "Algoritmo: ~A~%" algoritmo)
    (format f "Tempo de Execucao: ~,3F s~%" tempo)
    (format f "~%ESTATISTICAS~%")
    (format f "  Nos Gerados:      ~D~%" *nos-gerados*)
    (format f "  Nos Expandidos:   ~D~%" *nos-expandidos*)
    (format f "  Penetrancia:      ~F~%" (penetrancia (no-profundidade no) *nos-gerados*))
    (format f "  Fator Ramificacao Media: ~F~%"
            (fator-ramificacao-media (no-profundidade no) (1+ *nos-gerados*)))

    (format f "~%ESTADO INICIAL~%")
    (let ((caminho (reverse (obter-caminho no))))
      (imprimir-tabuleiro (no-estado (first caminho)) f)

      (format f "~%CAMINHO DA SOLUCAO (~D movimentos)~%"
              (no-profundidade no))
      (format f "--------------------------------------------~%~%")

      (loop for passo from 0
            for nodo in caminho do
              (format f "Passo ~D:~%" passo)
              (imprimir-tabuleiro (no-estado nodo) f)
              (format f "~%")))

    (format f "---------------------------------------------------~%")))






(defun iniciar ()
  "Função Principal: ciclo de resolução de problemas com menus melhorados."
  (loop
    (format t "~%============================================~%")
    (format t "          SOLITÁRIO – IA 2025/2026~%")
    (format t "============================================~%")

    (let ((tabuleiro-inicial (ler-e-escolher-problema)))
      (cond
        ((null tabuleiro-inicial)
         (format t "~%Não foi possível carregar um problema.~%"))
        (t
         (format t "~%Problema carregado com sucesso!~%")

         (setf *nos-gerados* 0)
         (setf *nos-expandidos* 0)

         (let* ((algoritmo (ler-algoritmo))
                (func-heuristica
                 (if (equal algoritmo 'a*) (ler-heuristica) 'heuristica))
                (profundidade
                 (if (eql algoritmo 'dfs) (ler-profundidade) 9999))

                (no (cria-no tabuleiro-inicial))

                (tempo-inicio (get-internal-run-time))

                (solucao
                 (cond
                   ((equal algoritmo 'bfs)
                    (funcall algoritmo no 'no-solucaop 'sucessores (operadores)))
                   ((equal algoritmo 'dfs)
                    (funcall algoritmo no 'no-solucaop 'sucessores
                             (operadores) profundidade))
                   ((equal algoritmo 'a*)
                    (funcall algoritmo no 'no-solucaop 'sucessores
                             (operadores) func-heuristica))))

                (tempo-fim (get-internal-run-time))
                (duracao (float (/ (- tempo-fim tempo-inicio)
                                   internal-time-units-per-second))))

           (escrever-solucao solucao algoritmo duracao)

           (format t "~%============================================~%")
           (format t "        Execução terminada~%")
           (format t "        Resultado guardado em log.dat~%")
           (format t "============================================~%")))))

    (format t "~%--------------------------------------------~%")
    (format t "Deseja resolver outro problema?~%")
    (format t "  [1]  Sim~%")
    (format t "  [2]  Não (sair)~%")
    (format t "--------------------------------------------~%")
    (format t "Opção: ")
    (let ((resp (read)))
      (when (not (= resp 1))
        (format t "~%A terminar programa... Até à próxima!~%")
        (return)))))
