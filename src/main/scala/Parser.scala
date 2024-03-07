import TokenType._
import scala.collection.mutable.Set

class Parser(lexer: Lexer, emitter: Emitter) {

  val symbols: Set[Token] = Set() // Variables declared so far.
  val labelsDeclared: Set[Token] = Set() // Labels declared so far.
  val labelsGotoed: Set[Token] = Set() // Labels goto'ed so far.

  var curToken: Token = null
  var peekToken: Token = null

  // init
  nextToken
  nextToken


  // Return true if the current token matches
  def checkToken(tokType: TokenType): Boolean = tokType == curToken.tokType

  // Return true if the next token matches
  def checkPeek(tokType: TokenType): Boolean = tokType == peekToken.tokType

  // Try to match current token. If not, error. Advances the current token
  def matchToken(tokType: TokenType) =
    if (!checkToken(tokType)) abort("Expected " + tokType + ", got " + curToken.tokType)
    nextToken

  def matchToken(tokType1: TokenType, tokType2: TokenType) =
    if (!checkToken(tokType1) && !checkToken(tokType2)) abort(s"Expected $tokType1 or $tokType2, got $curToken.tokType")
    nextToken

  // Advances the current token
  def nextToken =
    curToken = peekToken
    peekToken = lexer.getToken

  // Return true if the current token is a comparison operator.
  def isComparisonOperator =
    checkToken(GT) || checkToken(GTEQ) || checkToken(LT) || checkToken(LTEQ) || checkToken(EQEQ) || checkToken(NOTEQ)

  // Print error message and exit
  def abort(message: String) =
    Console.err.println("Error. " + message)
    sys.exit(0)


  // Production rules

  // program ::= {statement}
  def program = {
    emitter.headerLine("#include <stdio.h>")
    emitter.headerLine("int main(void){")

    // Skip newlines at the start of input
    skipNewlines

    // Parse all the statements in the program
    parseStatements

    emitter.emitLine("return 0;")
    emitter.emitLine("}")

    // Check that each label referenced in a GOTO is declared.
    labelsGotoed.foreach { label =>
      if (!labelsGotoed.contains(label))
        abort("Attempting to GOTO to undeclared label: " + label)
    }

    def skipNewlines: Unit =
      if (checkToken(NEWLINE))
        nextToken
        skipNewlines

    def parseStatements: Unit =
      if (!checkToken(EOF))
        statement
        parseStatements
  }

  def statement: Unit = {
    curToken.tokType match {
      // "PRINT" (expression | string)
      case PRINT => {
        nextToken

        if (checkToken(STRING))
          // Print simple string
          emitter.emitLine(s"printf(\"${curToken.tokText}\\n\");")
          nextToken
        else
          // Expect an expression and print the result as a float
          emitter.emit("printf(\"%.2f\\n\", (float)(")
          expression
          emitter.emitLine("));")
      }

      // "IF" comparison "THEN" {statement} "ENDIF"
      case IF => {
        nextToken
        emitter.emit("if(")
        comparison

        matchToken(THEN)
        nl
        emitter.emitLine("){")

        statementsRec(ENDIF)
      }
      // "WHILE" comparison "REPEAT"  {statement} "ENDWHILE"
      case WHILE => {
        nextToken
        emitter.emit("while(")
        comparison

        matchToken(REPEAT)
        nl
        emitter.emitLine("){")

        statementsRec(ENDWHILE)
      }

      // "LABEL" ident
      case LABEL => {
        nextToken
        // Make sure this label doesn't already exist
        if (labelsDeclared.contains(curToken)) abort("Label already exists: " + curToken)
        labelsDeclared += curToken

        emitter.emitLine(curToken.tokText + ":")
        matchToken(IDENT)
      }

      // "GOTO" ident
      case GOTO => {
        nextToken
        labelsGotoed += curToken
        emitter.emitLine(s"goto ${curToken.tokText};")
        matchToken(IDENT)
      }

      // "LET" ident
      case LET => {
        nextToken

        // Check if ident exists in symbol table. If not, declare it.
        if (!symbols.contains(curToken))
          symbols += curToken
          emitter.headerLine(s"float ${curToken.tokText};")

        emitter.emit(s"${curToken.tokText} = ")
        matchToken(IDENT)
        matchToken(EQ)
        expression
        emitter.emitLine(";")
      }

      // "INPUT" ident
      case INPUT => {
        nextToken

        // If variable doesn't already exist, declare it.
        if (!symbols.contains(curToken))
          symbols += curToken
          emitter.headerLine(s"float ${curToken.tokText};")

        // Emit scanf and validate the input. If invalid, set the variable to 0 and clear the input
        emitter.emitLine(s"if(0 == scanf(\"%" + s"f\", &${curToken.tokText})) {")
        emitter.emitLine(s"${curToken.tokText} = 0;")
        emitter.emit("scanf(\"%")
        emitter.emitLine("*s\");")
        emitter.emitLine("}")
        matchToken(IDENT)
      }

      // This is not a valid statement. Error!
      case _ => abort("Invalid statement at " + curToken)
    }

    // Newline
    nl

    def statementsRec(tokType: TokenType): Unit = {
      if (!checkToken(tokType))
        statement
        statementsRec(tokType)
      else
        matchToken(tokType)
        emitter.emitLine("}")
    }
  }

  // comparison ::= expression (("==" | "!=" | ">" | ">=" | "<" | "<=") expression)+
  def comparison = {
    expression
    // Must be at least one comparison operator and another expression.
    if (isComparisonOperator)
      emitter.emit(curToken.tokText)
      nextToken
      expression
      comparisonRec
    else
      abort("Expected comparison operator at: " + curToken)

    def comparisonRec: Unit = {
      if (isComparisonOperator) {
        emitter.emit(curToken.tokText)
        nextToken
        expression
        comparisonRec
      }
    }
  }

  // expression ::= term {( "-" | "+" ) term}
  def expression = {
    term
    // Can have 0 or more +/- and expressions.
    expressionRec

    def expressionRec: Unit = {
      if (checkToken(PLUS) || checkToken(MINUS)) {
        emitter.emit(curToken.tokText)
        nextToken
        term
        expressionRec
      }
    }
  }

  // term ::= unary {( "/" | "*" ) unary}
  def term = {
    unary
    // Can have 0 or more +/- and expressions.
    termRec

    def termRec: Unit = {
      if (checkToken(ASTERISK) || checkToken(SLASH)) {
        emitter.emit(curToken.tokText)
        nextToken
        unary
        termRec
      }
    }
  }

  // unary ::= ["+" | "-"] primary
  def unary = {
    // Optional unary +/-
    if (checkToken(PLUS) || checkToken(MINUS))
      emitter.emit(curToken.tokText)
      nextToken
    primary
  }

  // primary ::= number | ident
  def primary = {
    if (checkToken(NUMBER))
      emitter.emit(curToken.tokText)
      nextToken
    else if (checkToken(IDENT))
      // Ensure the variable already exists.
      if (!symbols.contains(curToken)) abort("Referencing variable before assignment: " + curToken)
      emitter.emit(curToken.tokText)
      nextToken
    else abort("Unexpected token at " + curToken)
  }

  // nl ::= '\n\+
  def nl: Unit = {

    // Require at least one new line (or EOF)
    matchToken(NEWLINE, EOF)
    nlRec

    def nlRec: Unit =
      if (checkToken(NEWLINE))
        nextToken
        nlRec
  }
}
