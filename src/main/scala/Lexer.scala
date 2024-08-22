import TokenType.IDENT

import scala.collection.BufferedIterator

class Lexer(buffer: BufferedIterator[Char]) {

  // Invalid token found, print error message and exit
  def abort(message: String) = {
    Console.err.println("Lexing error. " + message)
    sys.exit(0)
  }

  // Skip comments in the code
  def skipComment: Token = buffer.nextOption() match {
    case None => Token(TokenType.EOF)
    case Some(c) => c match {
      case '\n' => getToken
      case _ => skipComment
    }
  }

  // Return string token
  def getString(acc: StringBuilder = StringBuilder("")): Token = buffer.headOption match {
    case None => abort("Unclosed string")
    case Some(c) => c match {
      case c if List('\r', '\n', '\t', '\\', '%').contains(c) => abort("Illegal character in string")
      case '\"' => buffer.next(); Token(TokenType.STRING, acc.toString())
      case _ => buffer.next(); getString(acc += c)
    }
  }

  // Return number token
  def getNumber(acc: StringBuilder = StringBuilder("")): Token = buffer.headOption match {
    case None => Token(TokenType.NUMBER, acc.toString())
    case Some(c) => c match {
      case c if c.isDigit => buffer.next(); getNumber(acc += c)
      case '.' if !acc.contains('.') => buffer.next(); getNumber(acc += c)
      case '.' if acc.contains('.') => abort("Multiple points in number")
      case c if acc.toString().takeRight(1) == "." => abort("Lack of number after point")
      case _ => Token(TokenType.NUMBER, acc.toString())
    }
  }

  // Return keyword or ident token
  def getKeywordOrIdent(acc: StringBuilder = StringBuilder("")): Token = buffer.headOption match {
    case None =>
      val result = acc.toString()
      try {
        Token(TokenType.valueOf(result), result)
      } catch {
        case _ => Token(TokenType.IDENT, result)
      }
    case Some(c) => c match {
      case c if c.isLetterOrDigit => buffer.next(); getKeywordOrIdent(acc += c)
      case _ =>
        val result = acc.toString()
        try {
          Token(TokenType.valueOf(result), result)
        } catch {
          case _ => Token(TokenType.IDENT, result)
        }
    }
  }

  // Return equation operator token
  def getEqOperator(acc: StringBuilder = StringBuilder("")): Token = buffer.headOption match {
    case None =>
      val result = acc.toString()
      if (result == "") Token(TokenType.EOF)
      else if (result == "!") abort("Expected !=, got !")
      else Token(TokenType.fromValue(result), result)
    case Some(c) => c match {
      case c if acc.toString() == "" =>
        buffer.next()
        getEqOperator(acc += c)
      case '=' =>
        buffer.next()
        val result = (acc += c).toString()
        Token(TokenType.fromValue(result), result)
      case c if acc.toString() == "!" => abort("Expected !=, got !" + c)
      case _ => 
        val result = acc.toString()
        Token(TokenType.fromValue(result), result)
    }
  }

  // Return next token
  def getToken: Token = buffer.headOption match { // nextChar match {
    case None => Token(TokenType.EOF)
    case Some(c) => c match {
      case ' ' | '\t' => buffer.next(); getToken
      case '#' => buffer.next(); skipComment
      case '\n' => buffer.next(); Token(TokenType.NEWLINE, "\\n")
      case c if "+-*/".contains(c) => buffer.next(); Token(TokenType.fromValue(c.toString), c.toString)
      case c if "=><!".contains(c) => getEqOperator()
      case '\"' => buffer.next(); getString()
      case c if c.isDigit => getNumber()
      case c if c.isLetter => getKeywordOrIdent()
      case _ => abort("Unknown token: " + c) // unknown token
    }
  }
}

case class Token(tokType: TokenType, tokText: String = "")

enum TokenType {
  case EOF
  case NEWLINE
  case NUMBER
  case IDENT
  case STRING
  // Keywords
  case LABEL
  case GOTO
  case PRINT
  case INPUT
  case LET
  case IF
  case THEN
  case ENDIF
  case WHILE
  case REPEAT
  case ENDWHILE
  // Operators
  case EQ
  case PLUS
  case MINUS
  case ASTERISK
  case SLASH
  case EQEQ
  case NOTEQ
  case LT
  case LTEQ
  case GT
  case GTEQ
}

object TokenType {
  def fromValue(value: String): TokenType = value match {
    case "=" => TokenType.EQ
    case "+" => TokenType.PLUS
    case "-" => TokenType.MINUS
    case "*" => TokenType.ASTERISK
    case "/" => TokenType.SLASH
    case "==" => TokenType.EQEQ
    case "!=" => TokenType.NOTEQ
    case "<" => TokenType.LT
    case "<=" => TokenType.LTEQ
    case ">" => TokenType.GT
    case ">=" => TokenType.GTEQ
  }
}