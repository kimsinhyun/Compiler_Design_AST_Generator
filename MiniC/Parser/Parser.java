package MiniC.Parser;

import MiniC.Scanner.Scanner;
import MiniC.Scanner.Token;
import MiniC.Scanner.SourcePos;
import MiniC.Parser.SyntaxError;
import MiniC.ErrorReporter;
import MiniC.AstGen.*;


public class Parser {

  private Scanner scanner;
  private ErrorReporter errorReporter;
  private Token currentToken;
  private SourcePos previousTokenPosition;

  public Parser(Scanner lexer, ErrorReporter reporter) {
    scanner = lexer;
    errorReporter = reporter;
  }

  // accept() checks whether the current token matches tokenExpected.
  // If so, it fetches the next token.
  // If not, it reports a syntax error.
  void accept (int tokenExpected) throws SyntaxError {
    if (currentToken.kind == tokenExpected) {
      previousTokenPosition = currentToken.GetSourcePos();
      currentToken = scanner.scan();
    } else {
      syntaxError("\"%\" expected here", Token.spell(tokenExpected));
    }
  }

  // acceptIt() unconditionally accepts the current token
  // and fetches the next token from the scanner.
  void acceptIt() {
    previousTokenPosition = currentToken.GetSourcePos();
    currentToken = scanner.scan();
  }

  // start records the position of the start of a phrase.
  // This is defined to be the position of the first
  // character of the first token of the phrase.
  void start(SourcePos pos) {
    pos.StartCol = currentToken.GetSourcePos().StartCol;
    pos.StartLine = currentToken.GetSourcePos().StartLine;
  }

  // finish records the position of the end of a phrase.
  // This is defined to be the position of the last
  // character of the last token of the phrase.
  void finish(SourcePos pos) {
    pos.EndCol = previousTokenPosition.EndCol;
    pos.EndLine = previousTokenPosition.EndLine;
  }

  void syntaxError(String messageTemplate, String tokenQuoted) throws SyntaxError {
    SourcePos pos = currentToken.GetSourcePos();
    errorReporter.reportError(messageTemplate, tokenQuoted, pos);
    throw(new SyntaxError());
  }

  boolean isTypeSpecifier(int token) {
    if(token == Token.VOID ||
        token == Token.INT  ||
        token == Token.BOOL ||
        token == Token.FLOAT) {
      return true;
    } else {
      return false;
    }
  }

  boolean ifFirst_primary_expr(int token){
    if (token == Token.INTLITERAL ||
      token == Token.FLOATLITERAL  ||
      token == Token.STRINGLITERAL  ||
      token == Token.BOOLLITERAL ||
      token == Token.LEFTPAREN ||
      token == Token.ID) {
      return true;
    } else {
      return false;
    }
  }
  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseArrayIndexDecl (Type T):
  //
  // Take [INTLITERAL] and generate an ArrayType
  //
  ///////////////////////////////////////////////////////////////////////////////

  public ArrayType parseArrayIndexDecl(Type T) throws SyntaxError {
    IntLiteral L;
    IntExpr IE;
    accept(Token.LEFTBRACKET);
    SourcePos pos = currentToken.GetSourcePos();
    L = new IntLiteral(currentToken.GetLexeme(), pos);
    accept(Token.INTLITERAL);
    accept(Token.RIGHTBRACKET);
    IE = new IntExpr (L, pos);
    return new ArrayType (T, IE, previousTokenPosition);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // toplevel parse() routine:
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Program parse() {

    Program ProgramAST = null;

    previousTokenPosition = new SourcePos();
    previousTokenPosition.StartLine = 0;
    previousTokenPosition.StartCol = 0;
    previousTokenPosition.EndLine = 0;
    previousTokenPosition.EndCol = 0;

    currentToken = scanner.scan(); // get first token from scanner...

    try {
      ProgramAST = parseProgram();
      if (currentToken.kind != Token.EOF) {
        syntaxError("\"%\" not expected after end of program",
            currentToken.GetLexeme());
      }
    }
    catch (SyntaxError s) { return null; }
    return ProgramAST;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseProgram():
  //
  // program ::= ( (VOID|INT|BOOL|FLOAT) ID ( FunPart | VarPart ) )* ";"
  //
  ///////////////////////////////////////////////////////////////////////////////

  // parseProgDecls: recursive helper function to facilitate AST construction.
  public Decl parseProgDecls () throws SyntaxError {
    if (! isTypeSpecifier(currentToken.kind)) {
      return new EmptyDecl (previousTokenPosition);
    }
    SourcePos pos = new SourcePos();
    start(pos);
    Type T = parseTypeSpecifier();                  //isTypeSpecifiler
    ID Ident = parseID();                           //ID
    if(currentToken.kind == Token.LEFTPAREN) {      //FunPart
      Decl newD = parseFunPart(T, Ident, pos);
      return new DeclSequence (newD, parseProgDecls(), previousTokenPosition);
    } else {
      DeclSequence Vars = parseVarPart(T, Ident);
      if(Vars == null){                               //ex) int i;
        return new EmptyDecl(previousTokenPosition);
      }
      DeclSequence VarsTail = Vars.GetRightmostDeclSequenceNode();
      Decl RemainderDecls = parseProgDecls();
      VarsTail.SetRightSubtree (RemainderDecls);
      return Vars;
    }
  }

  public Program parseProgram() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    Decl D = parseProgDecls();
    finish(pos);
    Program P = new Program (D, pos);
    return P;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseFunPart():
  //
  // FunPart ::= ( "(" ParamsList? ")" CompoundStmt )
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseFunPart(Type T, ID Ident, SourcePos pos) throws SyntaxError {

    // We already know that the current token is "(".
    // Otherwise use accept() !
    acceptIt();
    Decl PDecl = parseParamsList(); // can also be empty...
    accept(Token.RIGHTPAREN);
    CompoundStmt CStmt = parseCompoundStmt();
    finish(pos);
    return new FunDecl (T, Ident, PDecl, CStmt, pos);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseParamsList():
  //
  // ParamsList ::= ParameterDecl ( "," ParameterDecl ) *
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseParamsList() throws SyntaxError {
    if (!isTypeSpecifier(currentToken.kind)) {
      return new EmptyFormalParamDecl(previousTokenPosition);         //if not Type specifier -> empty paramDecl
    }
    Decl Decl_1 = parseParameterDecl();
    Decl Decl_r = new EmptyFormalParamDecl(previousTokenPosition);
    if (currentToken.kind == Token.COMMA) {
      acceptIt();
      Decl_r = parseParamsList();
      if (Decl_r instanceof EmptyFormalParamDecl) {
        syntaxError("Declaration after comma expected", "");
      }
    }
    return new FormalParamDeclSequence (Decl_1, Decl_r, previousTokenPosition);
  } 


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseParameterDecl():
  //
  // ParameterDecl ::= (VOID|INT|BOOL|FLOAT) Declarator
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseParameterDecl() throws SyntaxError {
    Type T = null;
    Decl D = null;

    SourcePos pos = new SourcePos();
    start(pos);
    if (isTypeSpecifier(currentToken.kind)) {
      T = parseTypeSpecifier();
    } else {
      syntaxError("Type specifier instead of % expected",
          Token.spell(currentToken.kind));
    }
    D = parseDeclarator(T, pos);
    return D;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseDeclarator():
  //
  // Declarator ::= ID ( "[" INTLITERAL "]" )?
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseDeclarator(Type T, SourcePos pos) throws SyntaxError {
    ID Ident = parseID();
    if (currentToken.kind == Token.LEFTBRACKET) {
      ArrayType ArrT = parseArrayIndexDecl(T);
      finish(pos);
      return new FormalParamDecl (ArrT, Ident, pos);
    }
    finish(pos);
    return new FormalParamDecl (T, Ident, pos);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseVarPart():
  //
  // VarPart ::= ( "[" INTLITERAL "]" )?  ( "=" initializer ) ? ( "," init_decl)* ";"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public DeclSequence parseVarPart(Type T, ID Ident) throws SyntaxError {
    Type theType = T;
    Decl D;
    DeclSequence Seq = null;
    Expr E = new EmptyExpr(previousTokenPosition);
    if (currentToken.kind == Token.LEFTBRACKET) {
      theType = parseArrayIndexDecl(T);
    }
    if (currentToken.kind == Token.ASSIGN) {
      acceptIt();
      // You can use the following code after you have implemented
    // parseInitializer():
      E = parseInitializer();
    }
    D = new VarDecl (theType, Ident, E, previousTokenPosition);
    Seq = new DeclSequence(D, new EmptyDecl (previousTokenPosition),previousTokenPosition);
    // You can use the following code after implementatin of parseInitDecl():
    
    if (currentToken.kind == Token.COMMA) {
      acceptIt();
      Seq = new DeclSequence (D, parseInitDecl(T), previousTokenPosition);
    } 
    else {              //if not COMMA
      Seq = new DeclSequence (D, new EmptyDecl (previousTokenPosition),
      previousTokenPosition);
    }
    accept (Token.SEMICOLON);
    return Seq;
  }
  public Decl parseInitDecl(Type T) throws SyntaxError{
    Decl decl = null;
    Expr expr;
    accept(Token.ID);
    if(currentToken.kind == Token.LEFTBRACKET){
      accept(Token.LEFTBRACKET);
      accept(Token.INTLITERAL);
      accept(Token.RIGHTBRACKET);
    }
    if(currentToken.kind == Token.ASSIGN){
      accept(Token.ASSIGN);
      expr = parseInitializer();
    }
    return decl;
  }

  public Expr parseInitializer() throws SyntaxError{
    Expr expr;
    // if(currentToken.kind == Toekn.LEFTBRACE){
    //   accept(Token.LEFTBRACE);
    //   expr = parseExpr();
    //   while(currentToken.kind == Token.COMMA){
    //     accept(Token.COMMA);
    //     expr = parseExpr();
    //   }
    // }
    expr = parseExpr();
    return expr; 
  }

  public Expr parseExpr() throws SyntaxError{
    Expr andExpr;
    andExpr = parseAndExpr();
    return andExpr;
  }
  public Expr parseAndExpr() throws SyntaxError{
    Expr relation;
    relation = parseRelationalExpr();
    return relation;
  }
  public Expr parseRelationalExpr() throws SyntaxError{
    Expr addExpr;
    addExpr = praseAddExpr();
    return addExpr;
  }
  public Expr praseAddExpr() throws SyntaxError{
    Expr multExpr;
    multExpr = parseMultExpr();
    return multExpr;
  }
  public Expr parseMultExpr() throws SyntaxError{
    Expr unaryExpr;
    unaryExpr = parseUnaryExpr();
    return unaryExpr;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseUnaryExpr():
  //
  // UnaryExpr ::= ("+"|"-"|"!")* PrimaryExpr
  //
  ///////////////////////////////////////////////////////////////////////////////

    
    public Expr parseUnaryExpr() throws SyntaxError {
    if (currentToken.kind == Token.PLUS ||
        currentToken.kind == Token.MINUS ||
        currentToken.kind == Token.NOT) {
      Operator opAST = new Operator (currentToken.GetLexeme(),
          previousTokenPosition);
      acceptIt();
      return new UnaryExpr (opAST, parseUnaryExpr(), previousTokenPosition);
    }
    return parsePrimaryExpr();
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parsePrimaryExpr():
  //
  // PrimaryExpr ::= ID arglist?
  //              |  ID "[" expr "]"
  //              |  "(" expr ")"
  //              |  INTLITERAL | BOOLLITERAL | FLOATLITERAL | STRINGLITERAL
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parsePrimaryExpr() throws SyntaxError {
    Expr retExpr = null;
    SourcePos pos = currentToken.GetSourcePos();
    String tempLexeme;
    if(currentToken.kind == Token.INTLITERAL){
      tempLexeme = currentToken.GetLexeme();
      acceptIt();
      return new IntExpr(new IntLiteral(tempLexeme,pos),pos);
    // public IntExpr (IntLiteral astIL, SourcePos pos) {

    }
    else if(currentToken.kind == Token.BOOLLITERAL){
      tempLexeme = currentToken.GetLexeme();
      acceptIt();
      return new BoolExpr(new BoolLiteral(tempLexeme,pos),pos);
    }
    else if(currentToken.kind == Token.FLOATLITERAL){
      tempLexeme = currentToken.GetLexeme();
      acceptIt();
      return new FloatExpr(new FloatLiteral(tempLexeme,pos),pos);
    }
    else if(currentToken.kind == Token.STRINGLITERAL){
      tempLexeme = currentToken.GetLexeme();
      acceptIt();
      return new StringExpr(new StringLiteral(tempLexeme,pos),pos);
    }
    else if(currentToken.kind == Token.ID){
      System.out.println("llllllllllllllllll");
    }
    // your code goes here...




    return retExpr;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseCompoundStmt():
  //
  // CompoundStmt ::= "{" VariableDef* Stmt* "}"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseCompoundDecls () throws SyntaxError {
    if (!isTypeSpecifier(currentToken.kind)) {
      return new EmptyDecl (previousTokenPosition);
    }
    Type T = parseTypeSpecifier();
    ID Ident = parseID();
    DeclSequence Vars = parseVarPart(T, Ident);
    DeclSequence VarsTail = Vars.GetRightmostDeclSequenceNode();
    Decl RemainderDecls = parseCompoundDecls();
    VarsTail.SetRightSubtree (RemainderDecls);
    return Vars;       
  }
  
  public Stmt parseStmt() throws SyntaxError{
    SourcePos pos = new SourcePos();
    start(pos);
    if(currentToken.kind == Token.LEFTBRACE){
      CompoundStmt compoundStmt = parseCompoundStmt();
      return compoundStmt;
    }
    // else if (currentToken.kind == Token.IF){
    //   return parseIfStmt();
    // }
    // else if (currentToken.kind == Token.WHILE){
    //   return parseWhileStmt();
    // }
    // else if (currentToken.kind == Token.FOR){
    //   return parseForStmt();
    // }
    // else if (currentToken.kind == Token.RETURN){
    //   return parseReturnStmt();
    // }
    else if (currentToken.kind == Token.ID){
      ID ident = new ID(currentToken.GetLexeme(),pos);
      VarExpr varExpr = new VarExpr(ident,pos);
      accept(Token.ID);
      // if(currentToken.kind == Token.LEFTBRACKET){      //stmt ::= ID "[" expr "]" "=" expr ';'
      //   // Expr expr1;
      //   // Expr expr2;
      //   CallStmt callStmt1;
      //   CallStmt callStmt2;
      //   accept(Token.LEFTBRACKET);
      //   // expr1 = parseExpr();
      //   callStmt1 = new CallStmt(parseExpr(),pos);
      //   accept(Token.RIGHTBRACKET);
      //   accept(Token.ASSIGN);
      //   // expr2 = parseExpr();
      //   callStmt2 = new CallStmt(parseExpr(),pos);
      //   accept(Token.SEMICOLON);
      //   //--------------------------------------temp state!--------------------------------------
      //   return callStmt1;
      //   //--------------------------------------temp state!--------------------------------------
      // }
      if(currentToken.kind == Token.ASSIGN){
        AssignStmt assginStmt;
        accept(Token.ASSIGN);
        assginStmt = new AssignStmt(varExpr, parseExpr(),pos);
        return assginStmt;
      }
      else if( currentToken.kind == Token.LEFTPAREN){
        CallStmt callStmt = null;
        accept(Token.LEFTPAREN);
        if(ifFirst_primary_expr(currentToken.kind)){                                  //"(" (expr ("," expr)* )? ")"
          CallExpr callExpr;
          callExpr = new CallExpr(ident, parseExpr(), pos);
          callStmt = new CallStmt(callExpr, pos);
          // if(currentToken.kind == Token.COMMA){                                    //("," expr)*
          System.out.println("bbbbbbbbbbbbbbb");
            
          // }
        }
        else{
          CallExpr callExpr;
          callExpr = new CallExpr(ident, new EmptyExpr(previousTokenPosition), pos);
          callStmt = new CallStmt(callExpr, pos);
          accept(Token.RIGHTPAREN);
          return callStmt;
        }
        accept(Token.RIGHTPAREN);
        System.out.println("aaaaaaaaaaaaa");
        return callStmt;
      }
      return null;
    }
    return null;
  }

  public Stmt parseCompoundStmts () throws SyntaxError {
    if (! (currentToken.kind == Token.LEFTBRACE ||
          currentToken.kind == Token.IF ||
          currentToken.kind == Token.WHILE ||
          currentToken.kind == Token.FOR ||
          currentToken.kind == Token.RETURN ||
          currentToken.kind == Token.ID)
       ) {
      return new EmptyStmt(previousTokenPosition);
    }
    Stmt S = null;
    // You can use the following code after implementation of parseStmt():
    S = parseStmt();
    accept(Token.SEMICOLON);
    return new StmtSequence (S, parseCompoundStmts(), previousTokenPosition);
  }

  public CompoundStmt parseCompoundStmt() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    accept(Token.LEFTBRACE);
    Decl D = parseCompoundDecls();
    Stmt S = parseCompoundStmts();
    accept(Token.RIGHTBRACE);
    finish(pos);
    if ( (D.getClass() == EmptyDecl.class) &&
        (S.getClass() == EmptyStmt.class)) {
      return new EmptyCompoundStmt (previousTokenPosition);
    } else {
      return new CompoundStmt (D, S, pos);
    }
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseArgList():
  //
  // ArgList ::= "(" ( arg ( "," arg )* )? ")"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parseArgs() throws SyntaxError {
    if (currentToken.kind == Token.RIGHTPAREN) {
      return new  EmptyActualParam (previousTokenPosition);
    } 
    Expr Params = null;
    /*
     * You can use the following code after you have implemented parseExpr() aso.:
     *
     *
     Params = new ActualParam (parseExpr(), previousTokenPosition);
     if (currentToken.kind == Token.COMMA) {
     acceptIt();
     }
     */
    return new ActualParamSequence (Params, parseArgs(), previousTokenPosition);
  }

  public Expr parseArgList() throws SyntaxError {
    accept(Token.LEFTPAREN);
    Expr Params = parseArgs();
    accept(Token.RIGHTPAREN);
    return Params;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseID():
  //
  // ID (terminal)
  //
  ///////////////////////////////////////////////////////////////////////////////

  public ID parseID() throws SyntaxError {
    ID Ident = new ID(currentToken.GetLexeme(), currentToken.GetSourcePos());
    accept(Token.ID);
    return Ident;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseTypeSpecifier():
  //
  // VOID | INT | FLOAT | BOOL (all terminals)
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Type parseTypeSpecifier() throws SyntaxError {
    Type T = null;
    switch (currentToken.kind) {
      case Token.INT:
        T = new IntType(currentToken.GetSourcePos());
        break;
      case Token.FLOAT:
        T = new FloatType(currentToken.GetSourcePos());
        break;
      case Token.BOOL:
        T = new BoolType(currentToken.GetSourcePos());
        break;
      case Token.VOID:
        T = new VoidType(currentToken.GetSourcePos());
        break;
      default:
        syntaxError("Type specifier expected", "");
    }
    acceptIt();
    return T;
  }

}
