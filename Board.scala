import scala.math._

class Board(val rows: Int, val cols: Int) {

  def this(b: Board) {
    this(b.rows, b.cols)
  }

  class Tile(val x: Int, val y: Int) {

    def value() = Board.this.tiles(x)(y)

    def allMoves() = {
      for(dx <- -1 to 1; dy <- -1 to 1; if((x + dx >= 0 && x + dx < rows) && (y + dy >= 0 && y + dy < cols) && abs(dx) + abs(dy) == 1)) yield {
          new Tile(x + dx, y + dy)
      }
    }

    override def equals(that: Any): Boolean = {
      that.isInstanceOf[Tile] && this.hashCode == that.hashCode
    }

    override def hashCode: Int = {
        return x * 31 + y
    }

    override def toString = s"x $x y $y"
  }

  var tiles = ((1 to ((cols * rows)-1)) :+ 0).grouped(cols).toList
  var blank = new Tile(rows-1, cols-1)

  def tile(x: Int, y: Int): Tile = {
    new Tile(x, y)
  }

  def allMoves() = {
    blank.allMoves()
  }

  def allTilePos() = {
    for(i <- 0 until rows; j <- 0 until cols) yield (i, j)
  }

  def allTiles() = {
    allTilePos().map({case(x,y) => new Tile(x, y)})
  }

  def allAdjBoards() = {
    blank.allMoves().map(mv => move(mv))
  }

  def move(tile: Tile) = {
    if(!allMoves.contains(tile)) throw new Exception()
    val b2 = new Board(this)
    b2.blank = new b2.Tile(tile.x, tile.y)
    b2.tiles = this.tiles.zipWithIndex.map({
      case(e, x) => 
        e.zipWithIndex.map({
          case(e2, y) =>
            if(x == tile.x && y == tile.y)
              0
            else if(x == this.blank.x && y == this.blank.y)
              tile.value()
            else
              e2 })}).toList
    b2
  }

  override def equals(that: Any): Boolean = {
    // that.isInstanceOf[Board] && this.hashCode == that.hashCode
    if(!that.isInstanceOf[Board])
      return false
    allTilePos.foreach({case(x, y) =>
      if (tiles(x)(y) != that.asInstanceOf[Board].tiles(x)(y))
        return false
    })
    return true
  }

  override def hashCode: Int = {
    allTiles().scanLeft(0){
      case(r, t) => r + (t.value() * rows * cols)
    }.sum
  }

  override def toString = tiles.map(_.mkString(" ")).mkString("\n")
}

object Board {

  val SOLVED = new Board(4, 4)
  def shuffle(brd:Board, cnt:Int): Board = {
    if(cnt == 0) brd else {
      val moves = brd.blank.allMoves
      shuffle(brd.move(moves(scala.util.Random.nextInt(moves.size))), cnt-1)
    }
  }

  def dijkstraSolve(current: Board, cnt: Int, visited: Map[Board, Board], toVisit: List[Board]): Int = {
    if (current == SOLVED) {
      def solution(b: Board): List[Board] = if(b==visited(b)) List(b) else b +: solution(visited(b))
      println(s"Solution found in $cnt moves!\n${solution(current).reverse.mkString("\n\n")}")
      return cnt
    } else {
      val adjBoards = current.allAdjBoards.filterNot(visited.contains(_))
      val tv = toVisit ++ adjBoards
      if (tv.size > 0) {
        dijkstraSolve(tv.head, cnt+1, visited ++ adjBoards.map(_ -> current), tv.tail)
      } else {
        println("No solution is found")
        return -1
      }
    }
  }

  def ds(b: Board): Int = {
    val predecessor = scala.collection.mutable.Map[Board, Board]((b -> b))
    val toVisit = scala.collection.mutable.ArrayBuffer[Board](b)
    var cnt = 0
    
    while(toVisit.size > 0) {
      val candidate = toVisit.remove(0)
      if(cnt % 1000 == 0) println(s"Considered $cnt positions")

      if(candidate == SOLVED) {
        var solution = scala.collection.mutable.ArrayBuffer[Board]()
        var backtrace = candidate
        while(predecessor(backtrace) != backtrace){
          solution.prepend(backtrace)
          backtrace = predecessor(backtrace)
        }
        println(s"Solved in $cnt moves\n${solution.mkString("\n\n")}")
        return cnt
      }
      val adjBoards = candidate.allAdjBoards.filterNot(predecessor.contains(_))
      predecessor ++= adjBoards.map(_ -> candidate)
      toVisit.appendAll(adjBoards)
      cnt+=1
    }
    println("No solution is found")
    return -1
  }

  def main(args: Array[String] = Array()): Unit = {
    val b = shuffle(SOLVED, 17)
    // println(s"\nMutable\n$b\n-----------------------")
    // ds(b)

    println(s"\nImmutable\n$b\n-----------------------")
    dijkstraSolve(b, 0, Map(b -> b), List())
  }
}
