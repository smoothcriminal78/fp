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
      if (that.isInstanceOf[Tile]) this.hashCode == that.hashCode else false
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

  def allTilePos() = {
    for(i <- 0 until rows; j <- 0 until cols) yield (i, j)
  }

  def allAdjBoards() = {
    println(blank.allMoves())
    blank.allMoves().map(mv => move(mv))
  }

  def move(tile: Tile) = {
    val b2 = new Board(this)
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
    b2.blank = b2.tile(tile.x, tile.y)
    b2
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
