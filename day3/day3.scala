import scala.io.Source
import scala.util.control.Breaks._

val EMPTY: Char = '.'

case class Cell(x: Int, y: Int)

def loadLines(filename: String): List[String] = Source.fromFile(filename).getLines().toList

def part1(filename: String): Int = {
  var nums: List[Int] = Nil;
  val lines = loadLines(filename)

  for (row <- 0 until lines.length) {
    var buf: List[Int] = Nil
    for (col <- 0 until lines(0).length) {
      if (lines(row)(col).isDigit) {
        buf = buf :+ col
        // Handle case where a number is at the end of a row
        if (col == lines(0).length - 1 && isAdjacentToSymbol(lines, buf, row)) {
          nums = nums :+ convertToInt(buf, lines, row)
        }
      } else {
        if (buf.length > 0 && isAdjacentToSymbol(lines, buf, row)) {
          nums = nums :+ convertToInt(buf, lines, row)
        }
        buf = Nil
      }
    }
  }

  nums.sum
}

// Converts a list of indexes into the full number
def convertToInt(indexes: List[Int], lines: List[String], row: Int) = indexes.map((n) => lines(row)(n)).foldLeft("")((x,y) => x + String.valueOf(y)).toInt

def isAdjacentToSymbol(lines: List[String], numberIndexes: List[Int], lineNum: Int): Boolean = 
  isSymbol(Cell(numberIndexes(0) - 1, lineNum), lines) ||
  isSymbol(Cell(numberIndexes(0) - 1, lineNum + 1), lines) ||
  isSymbol(Cell(numberIndexes(0) - 1, lineNum - 1), lines) ||
  isSymbol(Cell(numberIndexes.last + 1, lineNum), lines) ||
  isSymbol(Cell(numberIndexes.last + 1, lineNum - 1), lines) ||
  isSymbol(Cell(numberIndexes.last + 1, lineNum + 1), lines) ||
  aboveOrBelow(lines, numberIndexes, lineNum)

def aboveOrBelow(lines: List[String], numberIndexes: List[Int], lineNum: Int): Boolean = {
  var res = false
  for (i <- numberIndexes) {
    if (isSymbol(Cell(i, lineNum - 1), lines) || isSymbol(Cell(i, lineNum + 1), lines)) res = true
  }
  res
}

def isSymbol(c: Cell, lines: List[String]): Boolean = {
  def test(c: Char): Boolean = c != EMPTY && !c.isDigit
  c match {
    case Cell(x, y) if (x >= 0 && x < lines(0).length && y >= 0 && y < lines.length) => test(lines(y)(x))
    case _ => false
  }
}

var GEAR: Char = '*'

def part2(filename: String) = {
  var nums: List[Int] = Nil;
  val lines = loadLines(filename)

  var gearsMap = Map[Cell, List[(Cell, Int)]]() 

  for (row <- 0 until lines.length) {
    var buf: List[Int] = Nil
    for (col <- 0 until lines(0).length) {
      if (lines(row)(col).isDigit) {
        buf = buf :+ col
        if (col == lines(0).length - 1) {
          var gears = getAdjacentGears(lines, buf, row)
          if (gears.length > 0) {
            for (gear <- gears) {
              if (gearsMap.contains(gear) && gearsMap(gear).forall((x) => x._1 != Cell(col, row))) {
                gearsMap = gearsMap + (gear -> (gearsMap(gear) :+ (Cell(col, row), convertToInt(buf, lines, row))))
              } else {
                gearsMap = gearsMap + (gear -> List((Cell(col, row), convertToInt(buf, lines, row))))
              }
            }
          }
        }
      } else {
        if (buf.length > 0) {
          var gears = getAdjacentGears(lines, buf, row)
          if (gears.length > 0) {
            for (gear <- gears) {
              if (gearsMap.contains(gear) && gearsMap(gear).forall((x) => x._1 != Cell(col, row))) {
                gearsMap = gearsMap + (gear -> (gearsMap(gear) :+ (Cell(col, row), convertToInt(buf, lines, row))))
              } else {
                gearsMap = gearsMap + (gear -> List((Cell(col, row), convertToInt(buf, lines, row))))
              }
            }
          }
        }
        buf = Nil
      }
    }
  }

  gearsMap.foreach((x) => {
    if (x._2.length == 2) {
      nums = nums :+ x._2.map((y) => y._2).product
    }
  })
  nums.sum
}

def getAdjacentGears(lines: List[String], numberIndexes: List[Int], lineNum: Int): List[Cell] = 
  getGearCell(Cell(numberIndexes(0) - 1, lineNum), lines) ++
  getGearCell(Cell(numberIndexes(0) - 1, lineNum + 1), lines) ++
  getGearCell(Cell(numberIndexes(0) - 1, lineNum - 1), lines) ++
  getGearCell(Cell(numberIndexes.last + 1, lineNum), lines) ++
  getGearCell(Cell(numberIndexes.last + 1, lineNum - 1), lines) ++
  getGearCell(Cell(numberIndexes.last + 1, lineNum + 1), lines) ++
  getAboveOrBelowGear(lines, numberIndexes, lineNum)

def getAboveOrBelowGear(lines: List[String], numberIndexes: List[Int], lineNum: Int): List[Cell] = {
  var gears = List[Cell]()
  for (i <- numberIndexes) {
    if (isSymbol(Cell(i, lineNum - 1), lines)) gears = gears :+ Cell(i, lineNum - 1)
    if (isSymbol(Cell(i, lineNum + 1), lines)) gears = gears :+ Cell(i, lineNum + 1)
  }
  gears
}

def getGearCell(c: Cell, lines: List[String]): List[Cell] = c match {
  case Cell(x, y) if (x >= 0 && x < lines(0).length && y >= 0 && y < lines.length) => if ( lines(y)(x) == GEAR ) { List(Cell(x, y)) } else { Nil }
  case _ => Nil
}
