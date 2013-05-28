import scala.io.Source

val inputFilename = "input.txt"

println("Scadoku")

val sudoku = getSudoku

// show(sudoku)
println
// readLine("Press enter to continue...")
show(solve(sudoku))

///////////////////////////////////

def solve(ls:Array[Array[Int]]):Array[Array[Int]] = {
    val flat = ls.flatten
    val allIndices = Range(0,81)
    val indices = allIndices.filter(i => flat(i) == 0)
    val validValues = indices.map(i => {
        getValidValuesAtIndex(i, flat)
    })
    val solution = allIndices.map(i => i match {
        case i if(flat(i) != 0) => flat(i)
        case i if(validValues(indices.indexOf(i)).size == 1) => validValues(indices.indexOf(i)).head
        case _ => 0
    }).toArray
    solution match {
        case x if(x.contains(0)) => solve(x.grouped(9).toArray.map(a => a.toArray))
        case x => x.grouped(9).toArray.map(a => a.toArray)
    }
}

def getValidValuesAtIndex(index:Int, ls:Array[Int]):Set[Int] = {
    Range(1,10).toSet[Int] --
        getRowForIndex(index, ls) --
        getColForIndex(index, ls) --
        getBoxForIndex(index, ls)
}

def getSudoku = Source.
    fromFile(inputFilename).
    filter(x => '0'<=x && x<='9').  // Remove white spaces and new lines
    mkString.                       // Convert to string
    grouped(9).
    toArray.
    map(ls => ls.toArray.map(x => (x-'0').toInt))

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
