# Peg Solitaire Puzzle Solver

Project developed for the **Artificial Intelligence** course at Escola Superior de Tecnologia de Setúbal (Academic Year 2025/2026).

## About the Project
This program was developed in Lisp and automatically solves the Peg Solitaire puzzle. The goal of the game is to make valid moves until only one peg is left on the board.

The system allows the user to choose the initial problem and solves it using different search algorithms, generating a detailed report (`log.dat`) with execution statistics at the end.

### Features and Algorithms
The code is divided into three main modules (`procura.lisp`, `puzzle.lisp`, and `projeto.lisp`) and supports:
* **Search Algorithms:** BFS, DFS (with depth limit), and A*.
* **Heuristics (for A*):** Standard Heuristic and Isolation Heuristic.
* **Statistics:** Automatic calculation of execution time, generated/expanded nodes, penetrance, and average branching factor.

## How to Run

To run the project, you need a configured Lisp environment. Follow these steps:

1. Set the working directory to the project folder:
   ```lisp
   (cd "path\to\project")
2. Compile the project:
   ```lisp
   (compile-file "projeto.lisp")
3. Load the project:
   ```lisp
   (load "projeto")
4. Start the program:
   ```lisp
   (iniciar)

Follow the interactive menu instructions to choose the board, algorithm, and heuristic.

## Documentation
For more detailed information about the code architecture or step-by-step instructions, check our manuals (currently available in Portuguese):

* manual_tecnico

* manual_utilizador


## Authors
* Gonçalo Barracha - 202200187

* Rodrigo Cardoso - 202200197

Note: Some UI optimizations and specific functions (such as the combined bisection/Newton-Raphson method and child state generation) were assisted by Artificial Intelligence (ChatGPT).