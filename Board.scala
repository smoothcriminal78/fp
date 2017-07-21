class Board(val rows: Int, val cols: Int) {

  class Tile(val x: Int, val y: Int) {

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

  def allTilePos() = {
    for(i <- 0 until rows; j <- 0 until cols) yield (i, j)
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

}
