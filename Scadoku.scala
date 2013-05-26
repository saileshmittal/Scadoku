import scala.io.Source

val inputFilename = "input.txt"

println("Scadoku")

val sudoku = Source.
    fromFile(inputFilename).
    filter(x => '0'<=x && x<='9').  // Remove white spaces and new lines
    mkString.                       // Convert to string
    grouped(9).
    toList.
    map(ls => ls.toList.map(x => (x-'0').toInt))

show(sudoku)
println
readLine("Press enter to continue...")
println("Solved:")
show(solve(sudoku))

def solve(ls:List[List[Int]]) = {
    ls
}

def show(ls:List[List[Int]]) = {
    ls.foreach(l => {
        println()
        l.foreach(x => {
            if (x>0)
            print(" " + x)
            else
            print(" _")
        })
    })
    println()
}
