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
      System.out.println("lastlast444444444444444444444" + currentToken.GetLexeme());
      
      if (currentToken.kind != Token.EOF) {
        System.out.println("lastlast5555555555555555555" + currentToken.GetLexeme());
        syntaxError("\"%\" not expected after end of program",
            currentToken.GetLexeme());
      }
    }
    catch (SyntaxError s) { return null; }
      System.out.println("lastlast5555555555555555555" + currentToken.GetLexeme());
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
      System.out.println("lastlast333333333333333333" + currentToken.GetLexeme());
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
      System.out.println("firstfirst111111111111111111111");
      DeclSequence Vars = parseVarPart(T, Ident);
      System.out.println("lastlast111111111111111111111" + currentToken.GetLexeme());
      if(Vars == null){                               //ex) int i;
        return new EmptyDecl(previousTokenPosition);
      }
      DeclSequence VarsTail = Vars.GetRightmostDeclSequenceNode();
      System.out.println("lastlast22222222222222222" + currentToken.GetLexeme());
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
    System.out.println("||||||||||||||||||||||||||||| -> " + currentToken.GetLexeme());
    System.out.println("||||||||||||||||||||||||||||| line -> " + currentToken.GetSourcePos().StartLine);
    System.out.println("||||||||||||||||||||||||||||| col  -> " + currentToken.GetSourcePos().StartCol);
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
      System.out.println("bbbbbbbbbbbbbbbbbbbbbbbbbb last token -> " + currentToken.GetLexeme());
      accept(Token.COMMA);
      Decl decl1 = parseInitDecl(T);
      return new DeclSequence (decl1, parseVarPart(T,Ident), previousTokenPosition);
    } 
    // else {              //if not COMMA
    //   System.out.println("vvvvvvvvvvvvvvvvvvvvvvv last token -> " + currentToken.GetLexeme());
      accept(Token.SEMICOLON);
      return new DeclSequence (D, new EmptyDecl (previousTokenPosition),
      previousTokenPosition);
    // }
  }
  
  public Decl parseInitDecl(Type T) throws SyntaxError{
    Decl decl = null;
    Expr expr;
    System.out.println("^^^^^^^^^^^^^^^^^ " + currentToken.GetLexeme());
    accept(Token.ID);
    if(currentToken.kind == Token.LEFTBRACKET){
      accept(Token.LEFTBRACKET);
      accept(Token.INTLITERAL);
      accept(Token.RIGHTBRACKET);
    }
    else{
      decl = new EmptyDecl(previousTokenPosition);
    }
    if(currentToken.kind == Token.ASSIGN){
      accept(Token.ASSIGN);
      expr = parseInitializer();
    }
    return decl;
  }

  public Expr parseInitializer() throws SyntaxError{
    Expr expr = null;
    if(currentToken.kind == Token.LEFTBRACE){
      accept(Token.LEFTBRACE);
      expr = parseExpr();
      System.out.println("11111111111111111111111 --> " + currentToken.GetLexeme());
      if(currentToken.kind == Token.COMMA){
        // accept(Token.COMMA);
        System.out.println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");

        ExprSequence expr2 = parseExprs();
        // accept(Token.RIGHTBRACE);
        return expr2;
      }
      accept(Token.RIGHTBRACE);
      return expr;
    }
    else{
      expr = parseExpr();;
    }
    return expr; 
  }
  public ExprSequence parseExprs() throws SyntaxError{
    if(currentToken.kind == Token.COMMA){
      accept(Token.COMMA);
      System.out.println("2222222222222222222222222222222 --> " + currentToken.GetLexeme());
      return new ExprSequence(parseExpr(), parseExprs() ,previousTokenPosition);
    }
    System.out.println("44444444444444444444444444444444444 --> " + currentToken.GetLexeme());
    accept(Token.RIGHTBRACE);
    System.out.println("55555555555555555555555555555555555 --> " + currentToken.GetLexeme());
    return new ExprSequence(parseExpr(),new EmptyExpr(previousTokenPosition),previousTokenPosition);
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
    if(currentToken.kind == Token.EQ || currentToken.kind == Token.NOTEQ || currentToken.kind == Token.LESSEQ || 
    currentToken.kind == Token.LESS || currentToken.kind == Token.GREATER || currentToken.kind == Token.GREATEREQ){
      Operator opAst = new Operator(currentToken.GetLexeme(), previousTokenPosition);
      acceptIt();
      return new BinaryExpr(addExpr,opAst,praseAddExpr(),previousTokenPosition);
    }
    return addExpr;
  }
  public Expr praseAddExpr() throws SyntaxError{
    Expr multExpr;
    multExpr = parseMultExpr();
    if((currentToken.kind == Token.PLUS) || (currentToken.kind == Token.MINUS)){
      Operator opAST = new Operator (currentToken.GetLexeme(),previousTokenPosition);
      acceptIt();
      return new BinaryExpr(multExpr, opAST,praseAddExpr(),previousTokenPosition);
    }
    return multExpr;
  }
  public Expr parseMultExpr() throws SyntaxError{
    Expr unaryExpr;
    unaryExpr = parseUnaryExpr();
    if((currentToken.kind == Token.TIMES) || (currentToken.kind == Token.DIV)){
      Operator opAST = new Operator (currentToken.GetLexeme(),previousTokenPosition);
      acceptIt();
      return new BinaryExpr(unaryExpr, opAST,parseMultExpr(),previousTokenPosition);
    }
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
    System.out.println("%%%%%%%%%%%%%%%%%% -> " + currentToken.GetLexeme());
    if(currentToken.kind == Token.INTLITERAL){                        //primary-expr ::= INTLITERAL
      tempLexeme = currentToken.GetLexeme();
      System.out.println("######################## -> " + currentToken.GetLexeme());
      acceptIt();
      return new IntExpr(new IntLiteral(tempLexeme,pos),pos);
    // public IntExpr (IntLiteral astIL, SourcePos pos) {
    }
    else if(currentToken.kind == Token.BOOLLITERAL){                  //primary-expr ::= BOOLITERAL
      tempLexeme = currentToken.GetLexeme();
      acceptIt();
      return new BoolExpr(new BoolLiteral(tempLexeme,pos),pos);
    }
    else if(currentToken.kind == Token.FLOATLITERAL){                 //primary-expr ::= FLOATLITERAL
      tempLexeme = currentToken.GetLexeme();
      acceptIt();
      return new FloatExpr(new FloatLiteral(tempLexeme,pos),pos);
    }
    else if(currentToken.kind == Token.STRINGLITERAL){                //primary-expr ::= STRINGLITERAL
      tempLexeme = currentToken.GetLexeme();
      acceptIt();
      return new StringExpr(new StringLiteral(tempLexeme,pos),pos);
    }
    else if(currentToken.kind == Token.ID){                           ////primary-expr ::= ID ...........
      ID ident = new ID(currentToken.GetLexeme(),pos);
      System.out.println("********************* -> " + currentToken.GetLexeme());
      accept(Token.ID);
      if(currentToken.kind == Token.LEFTPAREN){              // primary-expr ::= ID (arglist)?
        // accept(Token.LEFTPAREN);
        Expr argList;
        argList = parseArgList();
        System.out.println("++++++++++++++++++> " + currentToken.GetLexeme());
        System.out.println("++++++++++++++++++ start line> " + currentToken.GetSourcePos().StartLine);
        System.out.println("++++++++++++++++++ start col> " + currentToken.GetSourcePos().StartCol);

        return new CallExpr(ident,argList,previousTokenPosition);
      }
      else if(currentToken.kind == Token.LEFTBRACKET){      //   primary-expr ::= ID "[" expr "]"
        accept(Token.LEFTBRACKET);
        Expr expr;
        expr = parseExpr();
        accept(Token.RIGHTBRACKET);
        return expr;
      }
      return new VarExpr(ident,pos);
    }
    else if(currentToken.kind == Token.LEFTPAREN){          // primary-expr ::= "(" expr ")"
      
      accept(Token.LEFTPAREN);
      Expr expr;
      expr = parseExpr();
      accept(Token.RIGHTPAREN);
      return expr;
    }
    // your code goes here...
    System.out.println("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx> " + currentToken.GetLexeme());
    return new EmptyExpr(previousTokenPosition);
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
    System.out.println(".....................  " + currentToken.GetLexeme());
    VarsTail.SetRightSubtree (RemainderDecls);
    return Vars;       
  }
  
  public Stmt parseStmt() throws SyntaxError{
    SourcePos pos = new SourcePos();
    start(pos);
    if(currentToken.kind == Token.LEFTBRACE){                                                           //stmt = CompoundStmt
      CompoundStmt compoundStmt = parseCompoundStmt();
      return compoundStmt;
    }
    else if(currentToken.kind == Token.IF){                                                             //stmt ::= if_stmt
      Stmt ifStmt = parseIFStmt();
      return ifStmt;
    }
    else if(currentToken.kind == Token.FOR){
      Stmt forStmt = parseForStmt();
      return forStmt;
    }
    else if(currentToken.kind == Token.WHILE){
      Stmt whileStmt = parseWhileStmt();
      return whileStmt;
    }
    
    else if (currentToken.kind == Token.ID){                                                            //stmt = ID .......
      ID ident = new ID(currentToken.GetLexeme(),pos);
      VarExpr varExpr = new VarExpr(ident,pos);
      // System.out.println("----------------------> " + currentToken.GetLexeme());
      accept(Token.ID);
      System.out.println("----------------------> " + currentToken.GetLexeme());
                                                                                                        
      if(currentToken.kind == Token.ASSIGN){                                                            //stmt ::= ID "=" expr ';'
        AssignStmt assginStmt;
        accept(Token.ASSIGN);
        System.out.println("lllllllllllllllllllllllllllllll> " + currentToken.GetLexeme());
        System.out.println("lllllllllllllllllllllllllllllll start line > " + currentToken.GetSourcePos().StartLine);
        System.out.println("lllllllllllllllllllllllllllllll start col > " + currentToken.GetSourcePos().StartCol);
        assginStmt = new AssignStmt(varExpr, parseExpr(),pos);
        System.out.println("ooooooooooooooooooooooo> " + currentToken.GetLexeme());
        accept(Token.SEMICOLON);
        return assginStmt;
      }
      else if( currentToken.kind == Token.LEFTPAREN){                                                   //stmt ::= ID arglist ";"
        Expr argList = parseArgList();
        CallExpr callExpr;
        CallStmt callStmt;
        // D.getClass() == EmptyDecl.class
        if(argList.getClass() == EmptyActualParam.class){

          callExpr = new CallExpr(ident, new EmptyActualParam(previousTokenPosition), pos);
        }
        else{
          callExpr = new CallExpr(ident, argList, pos);
        }
        callStmt = new CallStmt(callExpr, pos);
        accept(Token.SEMICOLON);
        return callStmt;
        
      }
      return new EmptyStmt(previousTokenPosition);
    }
    
    return new EmptyStmt(previousTokenPosition);
  }

  public Stmt parseIFStmt() throws SyntaxError{
    Expr expr = null;
    Stmt stmt;
    accept(Token.IF);
    accept(Token.LEFTPAREN);
    expr = parseExpr();
    accept(Token.RIGHTPAREN);
    System.out.println("dddddddddddddddddddddddddd");
    stmt = parseStmt();
    if(currentToken.kind == Token.ELSE){
      accept(Token.ELSE);
      Stmt stmt2 = parseStmt();
      return new IfStmt(expr,stmt,stmt2,previousTokenPosition);
    }
    return new IfStmt(expr,stmt,previousTokenPosition);
  }

  public Stmt parseForStmt() throws SyntaxError{
    Expr expr1 = null;
    Expr expr2 = null;
    Expr expr3 = null;
    Stmt stmt;
    accept(Token.FOR);
    accept(Token.LEFTPAREN);
    if(currentToken.kind == Token.ID){
      expr1 = parseAssign();
    }
    accept(Token.SEMICOLON);
    if(ifFirst_primary_expr(currentToken.kind)){
      expr2 = parseExpr();
    }
    accept(Token.SEMICOLON);
    if(currentToken.kind == Token.ID){
      expr3 = parseAssign();
    }
    accept(Token.RIGHTPAREN);
    stmt = parseStmt();
    if(expr1 == null){
      expr1 = new EmptyExpr(previousTokenPosition);
    }
    if(expr2 == null){
      expr2 = new EmptyExpr(previousTokenPosition);
    }
    if(expr3 == null){
      expr3 = new EmptyExpr(previousTokenPosition);
    }
    return new ForStmt(expr1,expr2,expr3,stmt,previousTokenPosition);
  }

  public WhileStmt parseWhileStmt() throws SyntaxError{
    accept(Token.WHILE);
    accept(Token.LEFTPAREN);
    Expr expr = parseExpr();
    accept(Token.RIGHTPAREN);
    Stmt stmt =parseStmt();
    return new WhileStmt(expr,stmt,previousTokenPosition); 
  } 

  public AssignExpr parseAssign() throws SyntaxError{
    ID ident = new ID(currentToken.GetLexeme(),previousTokenPosition);
    VarExpr var = new VarExpr(ident,previousTokenPosition);
    accept(Token.ID);
    accept(Token.ASSIGN);
    Expr expr = parseExpr();
    return new AssignExpr(var, expr, previousTokenPosition);
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

    // accept(Token.SEMICOLON);
    return new StmtSequence (S, parseCompoundStmts(), previousTokenPosition);
  }

  public CompoundStmt parseCompoundStmt() throws SyntaxError {
    System.out.println("}}}}}}}}}}}}}}}}}}}} -> " + currentToken.GetLexeme());
    System.out.println("}}}}}}}}}}}}}}}}}}}} line -> " + currentToken.GetSourcePos().StartLine);
    System.out.println("}}}}}}}}}}}}}}}}}}}} col  -> " + currentToken.GetSourcePos().StartCol);
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

  public Expr parseArgs() throws SyntaxError {          //args ::= expr ("," expr)*
    if (currentToken.kind == Token.RIGHTPAREN) {
    System.out.println("qqqqqqqqqqqqqqqqqqqqqqq=> " + currentToken.GetLexeme());
    System.out.println("qqqqqqqqqqqqqqqqqqqqqqq strat line => " + currentToken.GetSourcePos().StartLine);
    System.out.println("qqqqqqqqqqqqqqqqqqqqqqq strat col => " + currentToken.GetSourcePos().StartCol);
    return new  EmptyActualParam (previousTokenPosition);
    }
    Expr Params = null;
    /*
     * You can use the following code after you have implemented parseExpr() aso.:
     */
    Params = new ActualParam (parseExpr(), previousTokenPosition);
    if (currentToken.kind == Token.COMMA) {
      acceptIt();
    }
    return new ActualParamSequence(Params, parseArgs(), previousTokenPosition);
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
