
#include "llvm/IR/Verifier.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include <cctype>
#include <cstdio>
#include <map>
#include <string>
#include <vector>

using namespace llvm;

// //////////////////////
// Parser
// ////////////////////

// The Lexer returns tokens [0-255] if it an unknown character, otherwise one
// of these for known things.
enum Token {
  tok_eof = 1,

  // commands
  tok_def = -2, tok_extern = -3,

  // primary
  tok_identifier = -4, tok_number = -5,
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_number

// gettok - Return the next token from standard input.
static int gettok() {
  static int LastChar = ' ';

  // Skip any whitespace.
  while (isspace(LastChar))
    LastChar = getchar();

  if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
    IdentifierStr = LastChar;
    while (isalnum((LastChar = getchar())))
      IdentifierStr += LastChar;
    
    if (IdentifierStr == "def") return tok_def;
    if (IdentifierStr == "extern") return tok_extern;
    return tok_identifier;
  }

  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
    std::string NumStr;

    do {
      NumStr += LastChar;
      LastChar = getchar();
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(NumStr.c_str(), 0);
    return tok_number;
  }

  if (LastChar == '#') {
    // Comment until the end of the line.
    do LastChar = getchar();
    while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

    if (LastChar != EOF)
      return gettok();
  }

  // Check for EOF. Don't eat the EOF.
  if (LastChar == EOF)
    return tok_eof;

  // Otherwise, just return the character as its ascii value
  int ThisChar = LastChar;
  LastChar = getchar();
  return ThisChar;
}

// /////////////////////
// Abstract Syntax Tree
// ////////////////////

// ExprAST - Base class for all expression nodes.
class ExprAST {
  public:
    virtual ~ExprAST() {}
    virtual Value *Codegen() = 0;
};

// NumberExprAST - Expression class for numberic literals "1.0"
class NumberExprAST : public ExprAST {
  double Val;
  public:
    NumberExprAST(double val) : Val(val) {}
    Value *Codegen();
};

// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string Name;
  public:
    VariableExprAST(const std::string &name) : Name(name) {}
    Value *Codegen();
};

// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  char Op;
  ExprAST *LHS, *RHS;
public:
  BinaryExprAST(char op, ExprAST *lhs, ExprAST *rhs)
    : Op(op), LHS(lhs), RHS(rhs) {}
  Value *Codegen();
};

// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<ExprAST*> Args;
public:
  CallExprAST(const std::string &callee, std::vector<ExprAST*> &args)
    : Callee(callee), Args(args) {}
  Value* Codegen();
};

// PrototypeAST - This class represents the "prototype" for a function,
// which captures its name, and argument names
class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;
public:
  PrototypeAST(const std::string &name, const std::vector<std::string> &args)
    : Name(name), Args(args) {}
  Function *Codegen();
};

// FunctionAST - This represents a function call itself.
class FunctionAST {
  PrototypeAST *Proto;
  ExprAST *Body;
public:
  FunctionAST(PrototypeAST *proto, ExprAST *body)
    : Proto(proto), Body(body) {}
  Function *Codegen();
};

// Error - Helper functions for error handling.
ExprAST *Error(const char *Str) { fprintf(stderr, "ErrorL %s\n", Str); return 0; }
PrototypeAST *ErrorP(const char *Str) { Error(Str); return 0; }
FunctionAST *ErrorF(const char *Str) { Error(Str); return 0; }

Value *ErrorV(const char *Str) { Error(Str); return 0; }

// ///////////////////
// Code Generation
// ///////////////////
static Module *TheModule;
static IRBuilder<> Builder(getGlobalContext());
static std::map<std::string, Value*> NamedValues;

Value *NumberExprAST::Codegen() {
  return ConstantFP::get(getGlobalContext(), APFloat(Val));
}

Value *VariableExprAST::Codegen() {
  // Look this variable up in the function.
  Value *V = NamedValues[Name];
  return V ? V : ErrorV("Unknown variable name");
}

Value *BinaryExprAST::Codegen() {
  Value *L = LHS->Codegen();
  Value *R = RHS->Codegen();
  if (L == 0 || R == 0) return 0;

  switch (Op) {
    case '+': return Builder.CreateFAdd(L, R, "addtmp");
    case '-': return Builder.CreateFSub(L, R, "subtmp");
    case '*': return Builder.CreateFMul(L, R, "multmp");
    case '<':
      L = Builder.CreateFCmpULT(L, R, "cmptmp");
      // Convert bool 0/1 to double 0.0 or 1.0
      return Builder.CreateUIToFP(L, Type::getDoubleTy(getGlobalContext()), "booltmp");
    default: return ErrorV("invalid binary operator");
  }
}

Value *CallExprAST::Codegen() {
  // Look up the name in the global module table.
  Function *CalleeF = TheModule->getFunction(Callee);
  if (CalleeF == 0)
    return ErrorV("Unknown function referenced.");

  // If argument mismatch error.
  if (CalleeF->arg_size() != Args.size())
    return ErrorV("Incorrect # arguments passed");

  std::vector<Value*> ArgsV;
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->Codegen());
    if (ArgsV.back() == 0) return 0;
  }

  return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

Function *PrototypeAST::Codegen() {
  // Make the function type: double(double, double) etc.
  std::vector<Type*> Doubles(Args.size(), Type::getDoubleTy(getGlobalContext()));
  FunctionType *FT = FunctionType::get(Type::getDoubleTy(getGlobalContext()), Doubles, false);

  Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule);

  // If F conflicted, there was already something named 'Name'
  // If body, don't allow redefinition or reextern
  if (F->getName() != Name) {
    // Delete the one we just made and get the existing
    F->eraseFromParent();
    F = TheModule->getFunction(Name);

    // If F already has a body, reject this
    if (!F->empty()) {
      ErrorF("redefinition of function");
      return 0;
    }

    // If F took a different number of args, reject
    if (F->arg_size() != Args.size()) {
      ErrorF("redefinition of function with different # args");
      return 0;
    }
  }

  // Set names for all arguments.
  unsigned Idx = 0;
  for (Function::arg_iterator AI = F->arg_begin(); Idx != Args.size();
       ++AI, ++Idx) {
    AI->setName(Args[Idx]);
    
    // Add arguments to variable symbol table.
    NamedValues[Args[Idx]] = AI;
  }

  return F;
}

Function *FunctionAST::Codegen() {
  NamedValues.clear();

  Function *TheFunction = Proto->Codegen();
  if (TheFunction == 0)
    return 0;

  // Create a new basic block to start insertion into.
  BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", TheFunction);
  Builder.SetInsertPoint(BB);

  if (Value *RetVal = Body->Codegen()) {
    // Finish off the function.
    Builder.CreateRet(RetVal);

    // Validate the generated code, checking for consistency.
    verifyFunction(*TheFunction);

    return TheFunction;
  }

  // Error reading body, remove function
  // FIXME: We can erase forward declarations here
  TheFunction->eraseFromParent();

  return 0;
}

// ///////////////////
// Parser Basics
// ///////////////////
// CurTok/getNextToken - Provides a simple token buffer.
static int CurTok;
static int getNextToken() {
  return CurTok = gettok();
}

static ExprAST *ParseExpression();

// numberexpr ::= number
static ExprAST *ParseNumberExpr() {
  ExprAST *Result = new NumberExprAST(NumVal);
  getNextToken(); // consume the number
  return Result;
}

// parenexpr ::= '(' expression ')'
static ExprAST *ParseParenExpr() {
  getNextToken(); // eat (
  ExprAST *V = ParseExpression();
  if (!V) return 0;

  if (CurTok != ')')
    return Error("expected ')'");
  getNextToken(); // eat )
  return V;
}

// identifierexpr
static ExprAST *ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  getNextToken(); // eat identifier

  if (CurTok != '(') // simple variable ref
    return new VariableExprAST(IdName);

  // Call
  getNextToken(); // eat (
  std::vector<ExprAST*> Args;
  if (CurTok != ')') {
    while (1) {
      ExprAST *Arg = ParseExpression();
      if (!Arg) return 0;
      Args.push_back(Arg);

      if (CurTok == ')') break;

      if (CurTok != ',')
        return Error("Expected ')' or ',' in argument list");
      getNextToken();
    }
  }

  // Eat the ')'
  getNextToken();

  return new CallExprAST(IdName, Args);
}

// primary
static ExprAST *ParsePrimary() {
  switch (CurTok) {
    default: return Error("unknown token when expecting an expression");
    case tok_identifier: return ParseIdentifierExpr();
    case tok_number: return ParseNumberExpr();
    case '(': return ParseParenExpr();
  }
}

// BinopPrecedence - Holds precedence for each binary operator
static std::map<char, int> BinopPrecedence;

// GetTokPrecedence - Gets the precedence of a binary operator token
static int GetTokPrecedence() {
  if (!isascii(CurTok))
    return -1;

  // Make sure it's a declared binop.
  int TokPrec = BinopPrecedence[CurTok];
  if (TokPrec <= 0) return -1;
  return TokPrec;
}

// binoprhs
static ExprAST *ParseBinOpRHS(int ExprPrec, ExprAST *LHS) {
  // If this is a binop, find its precedence.
  while (1) {
    int TokPrec = GetTokPrecedence();

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done
    if (TokPrec < ExprPrec)
      return LHS;

    // Okay, we know this is a binop
    int BinOp = CurTok;
    getNextToken(); // eat binop

    // Parse the primary expression after the binary operator.
    ExprAST *RHS = ParsePrimary();
    if (!RHS) return 0;

    // If BinOp binds less tightly with RHS than the operator after, let the
    // pending operator take RHS as its LHS.
    int NextPrec = GetTokPrecedence();
    if (TokPrec < NextPrec) {
      RHS = ParseBinOpRHS(TokPrec + 1, RHS);
      if (RHS == 0) return 0;
    }

    // Merge LHS / RHS
    LHS = new BinaryExprAST(BinOp, LHS, RHS);
  }

  return LHS;
}

// expression
static ExprAST *ParseExpression() {
  ExprAST *LHS = ParsePrimary();
  if (!LHS) return 0;

  return ParseBinOpRHS(0, LHS);
}

// prototype
static PrototypeAST *ParsePrototype() {
  if (CurTok != tok_identifier)
    return ErrorP("Expected function name in prototype");

  std::string FnName = IdentifierStr;
  getNextToken();

  if (CurTok != '(')
    return ErrorP("Expected '(' in prototype");

  // Read the list of argument names.
  std::vector<std::string> ArgNames;
  while (getNextToken() == tok_identifier)
    ArgNames.push_back(IdentifierStr);
  if (CurTok != ')')
    return ErrorP("Expected ')' in prototype");

  // success
  getNextToken(); // eat ')'

  return new PrototypeAST(FnName, ArgNames);
}

// definition
static FunctionAST *ParseDefinition() {
  getNextToken(); // eat def
  PrototypeAST *Proto = ParsePrototype();
  if (Proto == 0) return 0;

  if (ExprAST *E = ParseExpression())
    return new FunctionAST(Proto, E);
  return 0;
}

// external
static PrototypeAST *ParseExtern() {
  getNextToken(); // eat extern.
  return ParsePrototype();
}

// toplevelexpr
static FunctionAST *ParseTopLevelExpr() {
  if (ExprAST *E = ParseExpression()) {
    // Make an anon proto
    PrototypeAST *Proto = new PrototypeAST("", std::vector<std::string>());
    return new FunctionAST(Proto, E);
  }
  return 0;
}

// /////////////////////////
// Top-Level parsing
// ////////////////////////

static void HandleDefinition() {
  if (FunctionAST *F = ParseDefinition()) {
    fprintf(stderr, "Parsed a function definition.\n");
    if (Function *LF = F->Codegen()) {
      fprintf(stderr, "Read function definition:\n");
      LF->dump();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleExtern() {
  if (PrototypeAST *P = ParseExtern()) {
    fprintf(stderr, "Parsed an extern\n");
    if (Function *F = P->Codegen()) {
      fprintf(stderr, "Read extern:\n");
      F->dump();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (FunctionAST *F = ParseTopLevelExpr()) {
    fprintf(stderr, "Parsed a top-level expr\n");
    if (Function *LF = F->Codegen()) {
      fprintf(stderr, "Read top-level expression:\n");
      LF->dump();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

// top
static void MainLoop() {
  while (1) {
    fprintf(stderr, "ready> ");
    switch (CurTok) {
      case tok_eof: return;
      case ';': getNextToken(); break;
      case tok_def: HandleDefinition(); break;
      case tok_extern: HandleExtern(); break;
      default: HandleTopLevelExpression(); break;
    }
  }
}

// ////////////////
// Library functions that can be extern'd from user code.
// ////////////////
extern "C"
double putchard(double X) {
  putchar((char)X);
  return 0;
}

int main() {
  LLVMContext &Context = getGlobalContext();

  // Install standard binary operators
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40; // highest.

  // Prime the first token
  fprintf(stderr, "ready> ");
  getNextToken();

  // Make the module, which holds all the code.
  TheModule = new Module("my cool jit", Context);

  // Run the main loop
  MainLoop();

  // Print out all of the generated code.
  TheModule->dump();

  return 0;
}
