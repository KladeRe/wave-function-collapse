import java.io.File
import java.awt.Image
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.ImageIcon

/**
 * This file contains all data used by the program
 * allStrings contains all filenames of all tiles
 */
object AllTiles extends App:

  private def readScaledImage(filename: String) =
    ImageIcon(ImageIO.read(File("./Tiles/" + filename)).getScaledInstance(64, 64, Image.SCALE_DEFAULT))

  private val filenames    = File("./Tiles").list

  def stripEnding(s: String) = s.takeWhile(_ != '.')

  lazy val allStrings    = filenames.map(stripEnding(_))
  lazy val images        = filenames.map(name => (stripEnding(name), readScaledImage(name))).toMap

end AllTiles
