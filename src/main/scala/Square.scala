import o1.grid.GridPos
import o1.grid.CompassDir
import scala.util.Random
import scala.collection.mutable.{ArrayBuffer, Buffer}

class Square(grid: WaveFunctionCollapse, val location: GridPos):
  var tile: Option[String] = None
  val boundary       = Buffer.fill[Option[Char]](8)(None)
  val plausibleTiles = ArrayBuffer[String]() ++ AllTiles.allStrings

  // https://www.geogebra.org/calculator/egdwwxmu
  def distFromCenter(x: Int, y: Int): Double = {
    //    val v =
    Math.sqrt(math.pow(x - (grid.height / 2), 2) + math.pow(y - (grid.width / 2), 2)) / (((grid.height / 2) + (grid.width / 2)) / 2)
    //    1 - v.max(0).min(1)
  }

  /**
    * Fixes a single element in the boundary and removes incompatible tiles.
    * @param placeOnBoundary index of the element on the boundary
    * @param value value to be set in the boundary
    */
  def fix(placeOnBoundary: Int, value: Char): Unit =
    this.plausibleTiles --= this.plausibleTiles.filter(tile => tile(placeOnBoundary) != value)
  end fix

  /**
   * Calculates the entropy of the tile, that is - the number of plausible tiles.
   * @return the number of plausible tiles
   */
  def entropy: Int = plausibleTiles.size

  /**
  * Selects a random tile from all plausible tiles. Updates neighboring tiles if necessary.
  */
  def collapse(): Unit = {
    val noiseValue = grid.noise.noise(location.x, location.y) * 1.5
//    val noiseValue = Random.nextFloat()
    val distanceFromCenter = distFromCenter(location.x, location.y) + noiseValue * 0.2
    val tileIndex = (math.pow(distanceFromCenter, 4) * entropy).toInt.min(entropy - 1).max(0)
    println(s"Noise: ${location.x}, ${location.y}: ${noiseValue} - $distanceFromCenter")

    this.tile = Some(plausibleTiles.sortWith((a, b) => a > b)(tileIndex))
    for (i <- this.boundary.indices) do
      this.propagateChange(i, this.tile.get(i))

  }

  /**
   * Propagates the change in this square's boundary to boundaries and plausible tiles of neighboring squares.
   * @param boundaryIndex index in this squares boundary
   * @param fixedValue the value to be set in the boundary
   */
  def propagateChange(boundaryIndex: Int, fixedValue: Char): Unit =
    
    boundaryIndex match
      case 0 =>
        if grid.contains(location.neighbor(CompassDir.West)) then
          grid.elementAt(location.neighbor(CompassDir.West)).fix(2, fixedValue)
          
        if grid.contains(location.neighbor(CompassDir.North)) then
          grid.elementAt(location.neighbor(CompassDir.North)).fix(6, fixedValue)
        
        if grid.contains(location.neighbor(CompassDir.West)) && grid.contains(location.neighbor(CompassDir.North)) then
          grid.elementAt(grid.elementAt(location.neighbor(CompassDir.West)).location.neighbor(CompassDir.North)).fix(4, fixedValue)
          
          

      case 1 =>
        if grid.contains(location.neighbor(CompassDir.North)) then
          grid.elementAt(location.neighbor(CompassDir.North)).fix(5, fixedValue)
          

      case 2 =>
        if grid.contains(location.neighbor(CompassDir.North)) then
          grid.elementAt(location.neighbor(CompassDir.North)).fix(4, fixedValue)
          
        if grid.contains(location.neighbor(CompassDir.East)) then
          grid.elementAt(location.neighbor(CompassDir.East)).fix(0, fixedValue)
          
        if grid.contains(location.neighbor(CompassDir.North)) && grid.contains(location.neighbor(CompassDir.East)) then
          grid.elementAt(grid.elementAt(location.neighbor(CompassDir.North)).location.neighbor(CompassDir.East)).fix(6, fixedValue)
          

      case 3 =>
        if grid.contains(location.neighbor(CompassDir.East)) then
          grid.elementAt(location.neighbor(CompassDir.East)).fix(7, fixedValue)
          

      case 4 =>
        if grid.contains(location.neighbor(CompassDir.East)) then
          grid.elementAt(location.neighbor(CompassDir.East)).fix(6, fixedValue)
          
        if grid.contains(location.neighbor(CompassDir.South)) then
          grid.elementAt(location.neighbor(CompassDir.South)).fix(2, fixedValue)
        
        if grid.contains(location.neighbor(CompassDir.East)) && grid.contains(location.neighbor(CompassDir.South)) then
          grid.elementAt(grid.elementAt(location.neighbor(CompassDir.East)).location.neighbor(CompassDir.South)).fix(0, fixedValue)
          

      case 5 =>
        if grid.contains(location.neighbor(CompassDir.South)) then
          grid.elementAt(location.neighbor(CompassDir.South)).fix(1, fixedValue)
          

      case 6 =>
        if grid.contains(location.neighbor(CompassDir.South)) then
          grid.elementAt(location.neighbor(CompassDir.South)).fix(0, fixedValue)
          
        if grid.contains(location.neighbor(CompassDir.West)) then
          grid.elementAt(location.neighbor(CompassDir.West)).fix(4, fixedValue)
        
        if grid.contains(location.neighbor(CompassDir.South)) && grid.contains(location.neighbor(CompassDir.West)) then
          grid.elementAt(grid.elementAt(location.neighbor(CompassDir.South)).location.neighbor(CompassDir.West)).fix(2, fixedValue)

      case 7 =>
        if grid.contains(location.neighbor(CompassDir.West)) then
          grid.elementAt(location.neighbor(CompassDir.West)).fix(3, fixedValue)




  end propagateChange

end Square
