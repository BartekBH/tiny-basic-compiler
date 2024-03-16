import java.io.File
import scala.io.Source

object App {
  def main(args: Array[String]): Unit = {

    println("Tiny Basic Compiler")

    // Input
    val path = new File(getClass.getClassLoader.getResource("hello.tiny").getPath)
    val inputFile = scala.io.Source.fromFile(path).buffered

    // Init lexer, emitter and parser
    val lexer = Lexer(inputFile)
    val emitter = Emitter("out.c")
    val parser = Parser(lexer, emitter)

    // Start the parser
    parser.program

    // Write the output to file
    emitter.writeFile

    println("Parsing completed")
  }
}
