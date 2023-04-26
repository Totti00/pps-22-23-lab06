package u06lab.solution

object Game:
  type Position = (Int, Int)
  def Board(width: Int, height: Int): Solitaire = new SolitaireImpl(width, height)
  trait Solitaire:
    def placeMarks(size: Int): Seq[Iterable[Position]]
    def render(solution: Iterable[Position]): String

  class SolitaireImpl(width: Int, height: Int) extends Solitaire:

    override def placeMarks(size: Int): Seq[Iterable[Position]] = size match
      case 1 => List(Set((width / 2, height / 2)))
      case _ =>
        for
          board <- placeMarks(size - 1)
          x <- 0 until width
          y <- 0 until height
          cell = (x, y)
          if !board.toSeq.contains(cell)
          if legalMove(cell, board.last)
        yield board.toSeq :+ cell

    override def render(solution: Iterable[Position]): String =
      val reversed = solution.toSeq.reverse
      val rows =
        for y <- 0 until height
            row = for x <- 0 until width
                      number = reversed.indexOf((x, y)) + 1
            yield if number > 0 then "%-2d ".format(number) else "X  "
        yield row.mkString
      rows.mkString("\n")

    def legalMove(pos: Position, move: Position): Boolean =
      ((pos._1 - move._1).abs + (pos._2 - move._2).abs) == 2


@main def testSolitaire(): Unit =
  import Game.*
  val board = Board(3, 3)
  val solutions = board.placeMarks(5)
  solutions foreach (sol => println(board.render(sol) + "\n"))

  println("There are " + solutions.size + " solutions.")