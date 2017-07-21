import scala.math._

class Board(val rows: Int, val cols: Int) {

  def this(board: Board) = {
    new Board(rows, co
  }

  class Tile(val x: Int, val y: Int) {

    def value() = Board.this.tiles(x)(y)

    def allMoves() = {
      for(dx <- -1 to 1; dy <- -1 to 1; if((x + dx >= 0 && x + dx < rows) && (y + dy >= 0 && y + dy < cols) && abs(dx) + abs(dy) == 1)) yield {
          new Tile(x + dx, y + dy)
      }
    }

    override def equals(that: Any): Boolean = {
      if (that.isInstanceOf[Tile]) this.hashCode == that.hashCode else false
    }

    override def hashCode: Int = {
        return x * 31 + y
    }

    override def toString = s"x $x y $y"
  }

  val tiles = ((1 to ((cols * rows)-1)) :+ 0).grouped(cols).toList
  val blank = new Tile(rows-1, cols-1)

  def tile(x: Int, y: Int): Tile = {
    new Tile(x, y)
  }

  def allTilePos() = {
    for(i <- 0 until rows; j <- 0 until cols) yield (i, j)
  }

  def allAdjBoards(tile: Tile) = {
    
  }

  def move(tile: Tile) = {
  }

  override def equals(that: Any): Boolean = {
    if (that.isInstanceOf[Tile]) this.hashCode == that.hashCode else false
  }

  override def hashCode: Int = {
      return 1
  }

  override def toString = s"$tiles"
}

object Board {
  def main(args: Array[String] = Array()) = {
    val b = new Board(3, 5)
    val t = b.tile(1, 1)
    println(t.allMoves())
  }
}
