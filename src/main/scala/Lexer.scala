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
  def getString(acc: String = ""): Token = buffer.headOption match {
    case None => abort("Unclosed string")
    case Some(c) => c match {
      case c if List('\r', '\n', '\t', '\\', '%').contains(c) => abort("Illegal character in string")
      case '\"' => buffer.next(); Token(TokenType.STRING, acc)
      case _ => buffer.next(); getString(acc + c)
    }
  }

  // Return number token
  def getNumber(acc: String = ""): Token = buffer.headOption match {
    case None => Token(TokenType.NUMBER, acc)
    case Some(c) => c match {
      case c if c.isDigit => buffer.next(); getNumber(acc + c)
      case '.' if !acc.contains('.') => buffer.next(); getNumber(acc + c)
      case '.' if acc.contains('.') => abort("Multiple points in number")
      case c if acc.takeRight(1) == "." => abort("Lack of number after point")
      case _ => Token(TokenType.NUMBER, acc)
    }
  }

  // Return keyword or ident token
  def getKeywordOrIdent(acc: String = ""): Token = buffer.headOption match {
    case None => try {
      Token(TokenType.valueOf(acc), acc)
    } catch {
      case _ => Token(TokenType.IDENT, acc)
    }
    case Some(c) => c match {
      case c if c.isLetterOrDigit => buffer.next(); getKeywordOrIdent(acc + c)
      case _ => try {
        Token(TokenType.valueOf(acc), acc)
      } catch {
        case _ => Token(TokenType.IDENT, acc)
      }
    }
  }

  // Return equation operator token
  def getEqOperator(acc: String = ""): Token = buffer.headOption match {
    case None =>
      if (acc == "") Token(TokenType.EOF)
      else if (acc == "!") abort("Expected !=, got !")
      else Token(TokenType.fromValue(acc), acc)
    case Some(c) => (c) match {
      case c if acc == "" => buffer.next(); getEqOperator(acc + c)
      case '=' => buffer.next(); Token(TokenType.fromValue(acc + c), acc + c)
      case c if acc == "!"  => abort("Expected !=, got !" + c)
      case _ => Token(TokenType.fromValue(acc), acc)
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