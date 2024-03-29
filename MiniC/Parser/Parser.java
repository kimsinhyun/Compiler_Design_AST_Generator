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
      accept(Token.ASSIGN);
      // You can use the following code after you have implemented parseInitializer():
      E = parseInitializer();
    }
    D = new VarDecl (theType, Ident, E, previousTokenPosition);
    // You can use the following code after implementatin of parseInitDecl():
    
    if (currentToken.kind == Token.COMMA) {
      Seq = new DeclSequence (D, parseInitDecls(T), previousTokenPosition);
    } 
    else{
      Seq =  new DeclSequence (D, new EmptyDecl (previousTokenPosition),previousTokenPosition);
    }
    accept(Token.SEMICOLON);
    return Seq;
  }

  public Decl parseInitDecls(Type T) throws SyntaxError{
    if(currentToken.kind == Token.COMMA){
      accept(Token.COMMA);
      return new DeclSequence(parseInitDecl(T),parseInitDecls(T),previousTokenPosition);
    }
      return new EmptyDecl(previousTokenPosition);
  }
  
  public Decl parseInitDecl(Type T) throws SyntaxError{
    Decl decl = new EmptyDecl(previousTokenPosition);
    Expr expr;
    ID ident = parseID();
    if(currentToken.kind == Token.LEFTBRACKET){
      T = parseArrayIndexDecl(T);
    }
    if(currentToken.kind == Token.ASSIGN){
      accept(Token.ASSIGN);
      expr = parseInitializer();
      decl = new VarDecl(T,ident,expr,previousTokenPosition);
      return decl;
    }
    return  new VarDecl(T,ident,new EmptyExpr(previousTokenPosition),previousTokenPosition);
  }

  public Expr parseInitializer() throws SyntaxError{
    Expr expr = null;
    if(currentToken.kind == Token.LEFTBRACE){
      accept(Token.LEFTBRACE);
      expr = parseExpr();
        Expr expr2 = parseExprs();
        return new ExprSequence(expr,expr2,previousTokenPosition);
    }
    else{
      expr = parseExpr();;
    }
    return expr; 
  }
  public Expr parseExprs() throws SyntaxError{
    if(currentToken.kind == Token.COMMA){
      accept(Token.COMMA);
      return new ExprSequence(parseExpr(), parseExprs() ,previousTokenPosition);
    }
    accept(Token.RIGHTBRACE);
    return new EmptyExpr(previousTokenPosition);
  }

  public Expr parseExpr() throws SyntaxError{
    Expr andExpr;
    andExpr = parseAndExpr();
    if(currentToken.kind == Token.OR){
      Operator opAst = new Operator(currentToken.GetLexeme(),previousTokenPosition);
      accept(Token.OR);
      return new BinaryExpr(andExpr, opAst,parseExpr(),previousTokenPosition);
    }
    return andExpr;
  }
  public Expr parseAndExpr() throws SyntaxError{
    Expr relation;
    relation = parseRelationalExpr();
    if(currentToken.kind == Token.AND){
      Operator opAst = new Operator(currentToken.GetLexeme(),previousTokenPosition);
      accept(Token.AND);
      return new BinaryExpr(relation, opAst,parseAndExpr(),previousTokenPosition);
    }
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
    String tempLexeme;
    if(currentToken.kind == Token.INTLITERAL){                        //primary-expr ::= INTLITERAL
      tempLexeme = currentToken.GetLexeme();
      acceptIt();
      return new IntExpr(new IntLiteral(tempLexeme,previousTokenPosition),previousTokenPosition);
    }
    else if(currentToken.kind == Token.BOOLLITERAL){                  //primary-expr ::= BOOLITERAL
      tempLexeme = currentToken.GetLexeme();
      acceptIt();
      return new BoolExpr(new BoolLiteral(tempLexeme,previousTokenPosition),previousTokenPosition);
    }
    else if(currentToken.kind == Token.FLOATLITERAL){                 //primary-expr ::= FLOATLITERAL
      tempLexeme = currentToken.GetLexeme();
      acceptIt();
      return new FloatExpr(new FloatLiteral(tempLexeme,previousTokenPosition),previousTokenPosition);
    }
    else if(currentToken.kind == Token.STRINGLITERAL){                //primary-expr ::= STRINGLITERAL
      tempLexeme = currentToken.GetLexeme();
      acceptIt();
      return new StringExpr(new StringLiteral(tempLexeme,previousTokenPosition),previousTokenPosition);
    }
    else if(currentToken.kind == Token.ID){                           ////primary-expr ::= ID ...........
      ID ident = new ID(currentToken.GetLexeme(),previousTokenPosition);
      accept(Token.ID);
      if(currentToken.kind == Token.LEFTPAREN){              // primary-expr ::= ID (arglist)?
        // accept(Token.LEFTPAREN);
        Expr argList;
        argList = parseArgList();
        return new CallExpr(ident,argList,previousTokenPosition);
      }
      else if(currentToken.kind == Token.LEFTBRACKET){      //   primary-expr ::= ID "[" expr "]"
        accept(Token.LEFTBRACKET);
        Expr expr;
        expr = parseExpr();
        accept(Token.RIGHTBRACKET);
        return new ArrayExpr(new VarExpr(ident,previousTokenPosition), expr,previousTokenPosition);
      }
      return new VarExpr(ident,previousTokenPosition);
    }
    else if(currentToken.kind == Token.LEFTPAREN){          // primary-expr ::= "(" expr ")"
      
      accept(Token.LEFTPAREN);
      Expr expr;
      expr = parseExpr();
      accept(Token.RIGHTPAREN);
      return expr;
    }
    // your code goes here...
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
    VarsTail.SetRightSubtree (RemainderDecls);
    return Vars;       
  }
  
  public Stmt parseStmt() throws SyntaxError{
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
    else if (currentToken.kind == Token.RETURN){
      accept(Token.RETURN);
      Expr expr = new EmptyExpr(previousTokenPosition);
      if(ifFirst_primary_expr(currentToken.kind)){
        expr = parseExpr();
      }
      accept(Token.SEMICOLON);
      return new ReturnStmt(expr,previousTokenPosition);

    }
    
    else if (currentToken.kind == Token.ID){                                                            //stmt = ID .......
      ID ident = new ID(currentToken.GetLexeme(),previousTokenPosition);
      VarExpr varExpr = new VarExpr(ident,previousTokenPosition);
      accept(Token.ID);
                                                                                                        
      if(currentToken.kind == Token.ASSIGN){                                                            //stmt ::= ID "=" expr ';'
        AssignStmt assginStmt;
        accept(Token.ASSIGN);
        assginStmt = new AssignStmt(varExpr, parseExpr(),previousTokenPosition);
        accept(Token.SEMICOLON);
        return assginStmt;
      }
      else if( currentToken.kind == Token.LEFTPAREN){                                                   //stmt ::= ID arglist ";"
        Expr argList = parseArgList();
        CallExpr callExpr;
        CallStmt callStmt;
        // D.getClass() == EmptyDecl.class
        if(argList.getClass() == EmptyActualParam.class){

          callExpr = new CallExpr(ident, new EmptyActualParam(previousTokenPosition), previousTokenPosition);
        }
        else{
          callExpr = new CallExpr(ident, argList, previousTokenPosition);
        }
        callStmt = new CallStmt(callExpr, previousTokenPosition);
        accept(Token.SEMICOLON);
        return callStmt;
      }
      else if(currentToken.kind  == Token.LEFTBRACKET){
        accept(Token.LEFTBRACKET);
        Expr expr1 = parseExpr();
        accept(Token.RIGHTBRACKET);
        accept(Token.ASSIGN);
        Expr expr2 = parseExpr();
        accept(Token.SEMICOLON);
        
        // Expr expr1 = parseExpr();
        // accept(Token.ASSIGN);
        // Expr expr2 = parseExpr();
        return new AssignStmt(new ArrayExpr(new VarExpr(ident,previousTokenPosition), expr1, previousTokenPosition), expr2, previousTokenPosition);
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
