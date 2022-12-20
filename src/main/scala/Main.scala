import scala.swing.*
import scala.swing.event.*
import o1.*

import java.awt.Image
import java.io.File
import javax.imageio.ImageIO
import javax.swing.ImageIcon
import javax.swing.SwingUtilities
/**
 * A swing application that uses wave function collapse to randomly generate landmasses and water
 *
 * Art is by https://hellchains.itch.io/water-cliff-tileset
 */

object Main extends SimpleSwingApplication:
  val WIDTH = 10
  val HEIGHT = 10

  // Contructs a new world
  var world = getWorld()

  // Replaces the world (used by the UI when a button is pressed)
  def getWorld() =
    val w = WaveFunctionCollapse(WIDTH, HEIGHT)
    w.apply(GridPos(WIDTH / 2, HEIGHT / 2)).collapse()
    w

  // Construsts the main window
  def top = new MainFrame {
    title = "Land and sea"

    // Button for restarting the algorithm and clearing the window
    val button = new Button {
      text = "New world"
    }


    /**
     * Collapses a single tile and redraws the grid.
     */
    def update() =
      world.collapseOne()
      redrawGrid()

    // Collapse one tile in every 0.1 seconds.
    javax.swing.Timer(10, e => update()).start()


    //gets all elements we want to render and places them in a grid. They will appear in order from left to right one row at a time

    val grid = new GridPanel(WIDTH,HEIGHT) {
      world.renderableElements
        .map(tile => new Label{icon = AllTiles.images(tile)})
        .foreach(contents += _)
    }

    // places the grid and the button inside the MainFrame on top of each other

    contents = new BoxPanel(Orientation.Vertical) {
      contents += grid
      contents += button
      border = Swing.EmptyBorder(30, 30, 10, 30)
    }

    /**
     * Redraws the while grid by rebuilding the whole interface. The approach is ineffective and is only used here to keep the example simple.
     */
    def redrawGrid() =
      //clears  the grid
      grid.contents.clear()
      //and re-renders new elements
      world.renderableElements
        .map(tile => new Label{icon = AllTiles.images(tile)})
        .foreach(grid.contents += _)
      grid.revalidate()


    listenTo(button)

    reactions += {
      case ButtonClicked(b) =>
        world = getWorld()
        redrawGrid()
    }

  }
end Main
