# Manual Técnico
## Projeto Nº1 – Solucionador do Puzzle Solitário
## Inteligência Artificial – ESTSetúbal (2025/2026)

**Autores:**
- Gonçalo Barracha – 202200187
- Rodrigo Cardoso – 202200197

---

## 1. Algoritmo Geral
O programa está dividido em três módulos principais:

### **1. `procura.lisp`**
Implementa:
- Criação de nós
- Estruturas (estado, profundidade, heurística, pai)
- BFS, DFS (com profundidade limite), A*
- Cálculo de estatísticas (penetrância, fator de ramificação)

### **2. `puzzle.lisp`**
Define o domínio:
- Representação do tabuleiro (7×7)
- Acesso a células, linhas e colunas
- Operadores: mover esquerda, direita, cima, baixo
- Geração de sucessores
- Heurísticas: padrão e isolamento

### **3. `projeto.lisp`**
Controla:
- Menus e interface textual
- Leitura do ficheiro `problemas.dat`
- Execução de algoritmos
- Escrita do relatório em `log.dat`

---

## 2. Estrutura do Nó
Cada nó é uma lista:
```
(tabuleiro profundidade heuristica pai)
```

### Seletores:
- `no-estado`
- `no-profundidade`
- `no-heuristica`
- `no-pai`
- `no-custo` → g(n) + h(n)

---

## 3. Operadores
Cada operador verifica:
- Se a origem tem um pino
- Se a célula intermediária tem pino
- Se o destino está vazio

Movimentos:
- Esquerda (`operador-ce`)
- Direita (`operador-cd`)
- Cima (`operador-cc`)
- Baixo (`operador-cb`)

---

## 4. Algoritmos de Procura

### **BFS**
- Expande por níveis
- Usa fila
- Garante solução ótima
- Elevado consumo de memória

### **DFS**
- Expande o ramo mais profundo primeiro
- Requer limite de profundidade
- Pode falhar

### **A\***
Usa custo:
```
f(n) = g(n) + h(n)
```
Ordena abertos por menor custo.

---

## 5. Heurísticas

### **Heurística padrão**
```
h(x) = 1 / (o(x) + 1)
```

### **Heurística de isolamento**
Soma:
- Nº de pinos
- Nº de pinos isolados

---

## 6. Estatísticas
Geradas automaticamente:

### **Penetrância**
```
P = L / T
```

### **Fator de Ramificação Médio**
Calculado via método híbrido:
- Bissecção
- Newton-Raphson

---

## 7. Análise do Log (exemplo real)

### Heuristicas

#### A * (heuristica padrão)
Tempo de Execucao: 8.718 s

##### ESTATISTICAS
  Nos Gerados:      1695
  Nos Expandidos:   715
  Penetrancia:      0.005899705
  Fator Ramificacao Media: 1.9585552

#### A* (heuristica de isolamento)
Tempo de Execucao: 1.547 s

##### ESTATISTICAS
  Nos Gerados:      496
  Nos Expandidos:   138
  Penetrancia:      0.02016129
  Fator Ramificacao Media: 1.7035396

**Conclusão:**  
→ A heuristica de isolamento foi a mais eficiente , com um tempo de execução menor e menos nós gerados e expandidos.

### Algoritmos 

`Para o tabuleiro E:`

#### A*
Tempo de Execucao: 1.125 s

##### ESTATISTICAS
  Nos Gerados:      496
  Nos Expandidos:   138
  Penetrancia:      0.02016129
  Fator Ramificacao Media: 1.7035396


#### BFS
Tempo de Execucao: 3.000 s

##### ESTATISTICAS
  Nos Gerados:      1693
  Nos Expandidos:   714
  Penetrancia:      0.0059066746
  Fator Ramificacao Media: 1.9582975

**Conclusão**
Verifica-se que o algoritmo A* apresenta um desempenho significativamente superior ao BFS.
Embora ambos encontrem a mesma solução ótima, o A* gera e expande muito menos nós, reduzindo o espaço de procura e o tempo de execução.


---

## 8. Trabalho Futuro
- Implementação do algoritmo Negamax/AlfaBeta
- Adição do domínio do jogo Solitário 2 (2 jogadores)
- Implementação de mais 4 operadores de movimento simples do Solitário 2(e,d,c,b)
- Implementação de um novo módulo responsável pela interação jogo/algoritmo (jogar)
---

## 9. Conclusão
O sistema cumpre todos os requisitos do enunciado: representação correta do domínio, implementação funcional dos algoritmos de procura e geração automática de relatórios detalhados.
