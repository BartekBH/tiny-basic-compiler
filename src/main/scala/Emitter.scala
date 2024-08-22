import java.io.{File, PrintWriter}

// Keeps track of the generated code and outputs it
class Emitter(path: String) {

  val header = StringBuilder("")
  val code = StringBuilder("")

  def emit(c: String) = code ++= c

  def emitLine(c: String) = code ++= c += '\n'

  def headerLine(c: String) = header ++= c += '\n'

  def writeFile = {
    val file = new File(path)
    file.createNewFile()
    val printWriter = new PrintWriter(file)
    printWriter.write((header ++= code).toString())
    printWriter.close
  }

}
