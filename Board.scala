import scala.collection.immutable.Queue
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
    that.isInstanceOf[Board] && this.hashCode == that.hashCode
  }

  override def hashCode: Int = {
    allTiles().scanLeft(0){
      case(r, t) => r + (t.value() * rows * cols)
    }.sum
  }

  override def toString = tiles.map(_.mkString(" ")).mkString("\n")
}

object Board {

  val SOLVED = new Board(2, 2)
  def shuffle(brd:Board, cnt:Int): Board = {
    if(cnt == 0) brd else {
      val moves = brd.blank.allMoves
      shuffle(brd.move(moves(scala.util.Random.nextInt(moves.size))), cnt-1)
    }
  }

  def dijkstraSolve(current: Board, cnt: Int, visited: Map[Board, Board], toVisit: Seq[Board]): Unit = {
    if (current == SOLVED || cnt > 1000) {
      println(s"Solution found in $cnt moves!")
      def solution(b: Board): List[Board] = {
        if(b==visited(b)) List(b) else b +: solution(visited(b))
      }
      println(solution(current).reverse.mkString("\n\n"))
    } else {
      val adjBoards = current.allAdjBoards.filterNot(visited.contains(_))
      val tv = adjBoards ++ toVisit
      val candidate = tv.head

      dijkstraSolve(candidate, cnt+1, visited.updated(candidate, current), tv.tail)
    }
  }

  def main(args: Array[String] = Array()): Unit = {
    val b = shuffle(SOLVED, 3)
    println("------------")
    print(b)
    println("------------")
    val res = dijkstraSolve(b, 0, Map(b -> b), Seq())
  }
}
