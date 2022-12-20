import scala.util.Random
import scala.collection.mutable.Buffer
import o1.grid.*
import o1.grid.CompassDir.{North, West, South, East}
import scala.collection.mutable.ArrayBuffer

/**
 * Map of the world - randomly generated with the wave function collapse algorithm.
 * @param width width of the world
 * @param height height of the world
 */
class WaveFunctionCollapse(width: Int, height: Int) extends Grid[Square](width, height):
  val noise = Noise(Random.nextInt(), 0.5)

  // Creates the elements of the map - used internally
  def initialElements =
    val allLocations = (0 until this.size).map( n => GridPos(n % this.width, n / this.width) )
    allLocations.map( loc => Square(this, loc) )

  /**
   * Selects the square with the smallest entropy and collapses it.
   */
  def collapseOne() =
    val best = this.allElements
      .filter(_.tile.isEmpty)
      .maxByOption(_.entropy)

    best.foreach(_.collapse())

  /**
   * Returns a String representation of the map for rendering purposes.
   * @return a vector of strings representing the state of the grid
   */
  def renderableElements : Vector[String] =
    
    this.allElements.map(
      square => square.tile match
        case Some(tile) => tile
        case None => "00000000a"
    )
    
      

end WaveFunctionCollapse

/**
 * Helper class for storing where a square's neighbors are and which boundaries in this square are adjacent to boundaries in the other square.
 * @param mine my boundary index
 * @param dir compass direction where to find the neighboring square
 * @param theirs the other square's boundary index
 */

case class Neighbor(mine: Int, dir: CompassDir, theirs: Int)

/**
 * List of neighbors and boundary indices.
 */

val neighborBoundaries = Vector(
    Neighbor(0, West,  2),
    Neighbor(0, North, 6),
    Neighbor(1, North, 5),
    Neighbor(2, North, 4),
    Neighbor(2, East,  0),
    Neighbor(3, East,  7),
    Neighbor(4, East,  6),
    Neighbor(4, South, 2),
    Neighbor(5, South, 1),
    Neighbor(6, South, 0),
    Neighbor(6, West,  4),
    Neighbor(7, West,  3),
)


