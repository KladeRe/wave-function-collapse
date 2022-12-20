import scala.util.Random

class Noise(val seed: Int, val freq: Double = 1) {
  private val random = Random(seed)
  private val xAdd = random.nextFloat() * 1000
  private val yAdd = random.nextFloat() * 1000

  def noise(x: Int, y: Int): Double = {
    PerlinNoise.noise(x * freq + xAdd, y * freq + yAdd, 1)
  }
}
