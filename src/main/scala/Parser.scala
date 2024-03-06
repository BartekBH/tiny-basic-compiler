import TokenType._
import scala.collection.mutable.Set

class Parser(lexer: Lexer) {

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
    println("PROGRAM")

    // Skip newlines at the start of input
    skipNewlines

    // Parse all the statements in the program
    parseStatements

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
        println("STATEMENT-PRINT")
        nextToken

        if (checkToken(STRING))
          // Simple string
          nextToken
        else
          // Expect an expression
          expression
      }

      // "IF" comparison "THEN" {statement} "ENDIF"
      case IF => {
        print("STATEMENT-IF")
        nextToken
        comparison

        matchToken(THEN)
        nl

        statementsRec(ENDIF)
      }
      // "WHILE" comparison "REPEAT"  {statement} "ENDWHILE"
      case WHILE => {
        println("STATEMENT-WHILE")
        nextToken
        comparison

        matchToken(REPEAT)
        nl

        statementsRec(ENDWHILE)
      }

      // "LABEL" ident
      case LABEL => {
        println("STATEMENT-LABEL")
        nextToken
        if (labelsDeclared.contains(curToken)) abort("Label already exists: " + curToken)
        labelsDeclared += curToken
        matchToken(IDENT)
      }

      // "GOTO" ident
      case GOTO => {
        println("STATEMENT-GOTO")
        nextToken
        labelsGotoed += curToken
        matchToken(IDENT)
      }

      // "LET" ident
      case LET => {
        println("STATEMENT-LET")
        nextToken

        // Check if ident exists in symbol table. If not, declare it.
        if (!symbols.contains(curToken)) symbols += curToken

        matchToken(IDENT)
        matchToken(EQ)
        expression
      }

      // "INPUT" ident
      case INPUT => {
        println("STATEMENT-INPUT")
        nextToken

        // If variable doesn't already exist, declare it.
        if (!symbols.contains(curToken)) symbols += curToken

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
      else matchToken(tokType)
    }
  }

  // comparison ::= expression (("==" | "!=" | ">" | ">=" | "<" | "<=") expression)+
  def comparison = {
    println("COMPARISON")
    expression
    // Must be at least one comparison operator and another expression.
    if (isComparisonOperator)
      comparisonRec
    else
      abort("Expected comparison operator at: " + curToken)

    def comparisonRec: Unit = {
      if (isComparisonOperator) {
        nextToken
        expression
        comparisonRec
      }
    }
  }

  // expression ::= term {( "-" | "+" ) term}
  def expression = {
    println("EXPRESSION")
    term
    // Can have 0 or more +/- and expressions.
    expressionRec

    def expressionRec: Unit = {
      if (checkToken(PLUS) || checkToken(MINUS)) {
        nextToken
        term
        expressionRec
      }
    }
  }

  // term ::= unary {( "/" | "*" ) unary}
  def term = {
    println("TERM")
    unary
    // Can have 0 or more +/- and expressions.
    termRec

    def termRec: Unit = {
      if (checkToken(ASTERISK) || checkToken(SLASH)) {
        nextToken
        unary
        termRec
      }
    }
  }

  // unary ::= ["+" | "-"] primary
  def unary = {
    println("UNARY")
    // Optional unary +/-
    if (checkToken(PLUS) || checkToken(MINUS)) nextToken
    primary
  }

  // primary ::= number | ident
  def primary = {
    println(s"PRIMARY ($curToken)")

    if (checkToken(NUMBER)) nextToken
    else if (checkToken(IDENT))
      // Ensure the variable already exists.
      if (!symbols.contains(curToken)) abort("Referencing variable before assignment: " + curToken)
      nextToken
    else abort("Unexpected token at " + curToken)
  }

  // nl ::= '\n\+
  def nl: Unit = {
    println("NEWLINE")

    // Require at least one new line (or EOF)
    matchToken(NEWLINE, EOF)
    nlRec

    def nlRec: Unit =
      if (checkToken(NEWLINE))
        nextToken
        nlRec
  }
}
