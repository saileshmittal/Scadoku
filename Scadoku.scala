import scala.io.Source
import collection.immutable._

val inputFilename = "input.txt"

println("Scadoku")

val sudoku = getSudoku

// show(sudoku)
println
// readLine("Press enter to continue...")
show(solve(sudoku))

///////////////////////////////////

def solve(ls:IndexedSeq[IndexedSeq[Int]]):IndexedSeq[IndexedSeq[Int]] = {
  show(ls)
  println
  readLine("Press enter to continue...")
  val flat = ls.flatten
  val allIndices = Range(0,81)
  val validValues = allIndices.map(i => i match {
    case x if (flat(x) == 0) => getValidValuesAtIndex(x, flat)
    case _ => Set[Int]()
  })
  val reducedValidValues = allIndices.map(i => i match {
    case x if (validValues(x).size > 1) => reduceValidValuesAtIndex(x, validValues)
    case x => validValues(x)
  })
  val solution = allIndices.map(i => i match {
    case x if(flat(x) != 0) => flat(x)
    case x if(reducedValidValues(x).size == 1) => reducedValidValues(x).head
    case _ => 0
  })
  solution match {
    case x if(x == flat) => ls
    case x if(x.contains(0)) => solve(x.grouped(9).toIndexedSeq)
    case x => x.grouped(9).toIndexedSeq
  }
}

def getValidValuesAtIndex(index:Int, ls:IndexedSeq[Int]):Set[Int] = {
  Range(1,10).toSet --
    getRowIndicesForIndex(index).map(i => ls(i)).toSet --
    getColIndicesForIndex(index).map(i => ls(i)).toSet --
    getBoxIndicesForIndex(index).map(i => ls(i)).toSet
}

def reduceValidValuesAtIndex(index:Int, ls:IndexedSeq[Set[Int]]):Set[Int] = {
  List(getRowIndicesForIndex(index),getColIndicesForIndex(index),getBoxIndicesForIndex(index)).
    map(s => (s.toSet - index).map(i => ls(i)).foldLeft(ls(index)) { (ret, s) => ret -- s}).
    find(s => s.size == 1).getOrElse(Set[Int]())
}



def getSudoku = Source.
  fromFile(inputFilename).
  filter(x => '0'<=x && x<='9').  // Remove white spaces and new lines
  mkString.                       // Convert to string
  grouped(9).
  toIndexedSeq.
  map((ls:String) => ls.toIndexedSeq.map((x:Char) => (x-'0').toInt))

def getRowIndicesForIndex(index:Int):IndexedSeq[Int] = {
  val start = 9*(index/9)
  Range(0,81).slice(start, start+9)
}

def getColIndicesForIndex(index:Int):IndexedSeq[Int] = {
  val mod = index%9
  Range(0,81).filter(x => x%9 == mod)
}

def getBoxIndicesForIndex(index:Int):IndexedSeq[Int] = {
  val rowStart = 27*(index/27)
  val colStart = 3*((index%9)/3)
  val colEnd = colStart+3
  val rows = IndexedSeq(rowStart, rowStart+9, rowStart+18)
  rows.map(i => getRowIndicesForIndex(i).slice(colStart,colEnd)).flatten
}

def show(ls:IndexedSeq[IndexedSeq[Int]]) = {
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
