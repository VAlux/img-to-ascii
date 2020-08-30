import java.awt.image.BufferedImage
import java.io.{File, FileOutputStream, PrintWriter}

import javax.imageio.ImageIO

import scala.language.postfixOps
import scala.util.{Failure, Success, Using}

object ImageToAscii {

  val imageFileNames = List("test.jpg", "test2.jpg", "test3.gif")

  lazy val imageFiles: Seq[(String, BufferedImage)] =
    imageFileNames.map(name => name -> ImageIO.read(new File(name)))

  lazy val charactersMap: Seq[((Int, Int), Char)] = generateColorToCharacterMap("  ░░▒▒▓▓██")

  def main(args: Array[String]): Unit = {
    imageFiles.foreach { case (name, file) =>
      Using(new PrintWriter(new FileOutputStream(s"out_$name.txt"))) { writer =>
        convert(file, 5).foreach(line => writer.println(line mkString))
      } match {
        case Success(_) => println(s"Conversion of $name finished!")
        case Failure(exception) => println(s"Conversion of $name failure! ${exception.getMessage}")
      }
    }
  }

  def convert(image: BufferedImage, segmentSize: Int): Iterator[Seq[Char]] = {
    val windowStep = (image.getWidth.toFloat / segmentSize).ceil.toInt

    splitToSegments(image, segmentSize)
      .sliding(windowStep, windowStep - 1)
      .map(segmentsToCharacters)
  }

  def splitToSegments(image: BufferedImage, segmentSize: Int): Seq[BufferedImage] = for {
    segmentY <- 0 until image.getHeight - segmentSize by segmentSize
    segmentX <- 0 until image.getWidth - segmentSize by segmentSize
    subImage = image.getSubimage(segmentX, segmentY, segmentSize, segmentSize)
  } yield subImage

  def segmentsToCharacters(segments: Seq[BufferedImage]): Seq[Char] = for {
    segment <- segments
    character = convertImageToCharacter(segment)
  } yield character

  def convertImageToCharacter(image: BufferedImage): Char = {
    val height = image.getHeight
    val width = image.getWidth
    val totalPixelsAmount = width * height

    val avgColor: Int = sumColorComponents(extractColorComponents(image, height, width)) match {
      case (r, g, b) => ((r / totalPixelsAmount) + (g / totalPixelsAmount) + (b / totalPixelsAmount)) / 3
    }

    charactersMap
      .find { case ((start, end), _) => start <= avgColor && end > avgColor }
      .map(_._2)
      .getOrElse('X')
  }

  private def extractColorComponents(image: BufferedImage, height: Int, width: Int): Seq[(Int, Int, Int)] = for {
    y <- 0 until height
    x <- 0 until width
    color = image.getRGB(x, y)
    red = (color & 0x00ff0000) >> 16
    green = (color & 0x0000ff00) >> 8
    blue = color & 0x000000ff
  } yield (red, green, blue)

  private def sumColorComponents(colors: Seq[(Int, Int, Int)]): (Int, Int, Int) =
    colors.foldLeft((0, 0, 0)) { case (acc, (r, g, b)) =>
      acc match {
        case (ar, ag, ab) => (ar + r, ab + b, ag + g)
      }
    }

  def generateColorToCharacterMap(symbols: String): Seq[((Int, Int), Char)] = {
    val step = 255 / (symbols.length - 1)
    for {
      index <- 0 until symbols.length
      point = index * step
      range = (point, point + step)
      symbol = symbols.charAt(index)
    } yield range -> symbol
  }
}