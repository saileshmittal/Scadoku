# Scadoku
Scala [Sudoku][id] Solver

[id]:http://en.wikipedia.org/wiki/Sudoku

### My First ever...
* This is my first ever Sudoku Solver.
* This is my first ever Scala program.
* This is my first ever functional-programming-language-anything.

Apply the constraints and when this fails, guess a possible value at a location, and loop again (using Depth First Search).

## How to Run

The file `input.txt` contains the sudoku puzzle to be solved. The blanks are represented by `0`, and each row is separated by a `newline`.

To run the program, you need to have `scala` installed on your system. Then run:

    $ scala Scadoku.scala
    