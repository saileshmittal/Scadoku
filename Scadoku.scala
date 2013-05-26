import scala.io.Source

val inputFilename = "input.txt"

println("Scadoku")

val sudoku = getSudoku

// show(sudoku)
println
// readLine("Press enter to continue...")
show(solve(sudoku))

///////////////////////////////////

def getSudoku = Source.
    fromFile(inputFilename).
    filter(x => '0'<=x && x<='9').  // Remove white spaces and new lines
    mkString.                       // Convert to string
    grouped(9).
    toArray.
    map(ls => ls.toArray.map(x => (x-'0').toInt))

def solve(ls:Array[Array[Int]]) = {
    ls
}

def getRowForIndex(index:Int, ls:Array[Int]):Array[Int] = {
    val start = 9*(index/9)
    ls.slice(start, start+9)
}

def getColForIndex(index:Int, ls:Array[Int]):Array[Int] = {
    val mod = index%9
    val indices = Range(0,81).filter(x => x%9 == mod)
    indices.map(i => ls(i)).toArray
}

def getBoxForIndex(index:Int, ls:Array[Int]):Array[Int] = {
    val rowStart = 27*(index/27)
    val colStart = 3*((index%9)/3)
    val colEnd = colStart+3
    val rows = Array(rowStart, rowStart+9, rowStart+18)
    rows.map(i => getRowForIndex(i, ls).slice(colStart,colEnd)).flatten
}

def show(ls:Array[Array[Int]]) = {
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
