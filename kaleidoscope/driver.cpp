// driver.cpp

#include <algorithm>
#include <cstdio>
#include <map>
#include <memory>
#include <numeric>
#include <string>
#include <vector>

#include "KaleidoscopeJIT.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"

#include "llvm/IR/LegacyPassManager.h"
namespace legacy = llvm::legacy;

static constexpr const char* const ANON_EXPR_NAME = "__anon_expr";

// ----------------------------------------------------------------------------
// Lexer

enum Token {
  tok_eof = -1,

  // commands
  tok_def = -2,
  tok_extern = -3,

  // primary
  tok_identifier = -4,
  tok_number = -5,
};

static std::string IdentifierStr{};
static double NumVal{};

static auto gettok() -> int {
  static int LastChar = ' ';

  // skip whitespace
  while (isspace(LastChar)) {
    LastChar = getchar();
  }

  if (isalpha(LastChar)) {
    IdentifierStr = LastChar;
    while (isalpha(LastChar = getchar())) {
      IdentifierStr += LastChar;
    }

    if (IdentifierStr == "def") {
      return tok_def;
    } else if (IdentifierStr == "extern") {
      return tok_extern;
    } else {
      return tok_identifier;
    }
  }

  if (isdigit(LastChar) || LastChar == '.') {
    std::string NumStr{};
    do {
      NumStr += LastChar;
      LastChar = getchar();
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(NumStr.c_str(), 0);
    return Token::tok_number;
  }

  if (LastChar == '#') {
    // comment until the end of the line
    do {
      LastChar = getchar();
    } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

    if (LastChar != EOF) {
      return gettok();
    }
  }

  if (LastChar == EOF) {
    return Token::tok_eof;
  }

  auto const ThisChar = LastChar;
  LastChar = getchar();
  return ThisChar;
}

// ----------------------------------------------------------------------------
// AST

class ExprAST {
public:
  virtual ~ExprAST(){};
  virtual auto codegen() -> llvm::Value* = 0;
};

class NumberExprAST : public ExprAST {
  double Val;

public:
  NumberExprAST(double val)
      : Val{val} {}

  virtual auto codegen() -> llvm::Value* override;
};

class VariableExprAST : public ExprAST {
  std::string Name;

public:
  VariableExprAST(std::string const& name)
      : Name{name} {}

  virtual auto codegen() -> llvm::Value* override;
};

class BinaryExprAST : public ExprAST {
  char Op;
  std::unique_ptr<ExprAST> Lhs;
  std::unique_ptr<ExprAST> Rhs;

public:
  BinaryExprAST(char op, std::unique_ptr<ExprAST> lhs,
                std::unique_ptr<ExprAST> rhs)
      : Op{op}
      , Lhs{std::move(lhs)}
      , Rhs{std::move(rhs)} {}

  virtual auto codegen() -> llvm::Value* override;
};

class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;

public:
  CallExprAST(std::string const& callee,
              std::vector<std::unique_ptr<ExprAST>> args)
      : Callee{callee}
      , Args{std::move(args)} {}

  virtual auto codegen() -> llvm::Value* override;
};

class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;

public:
  PrototypeAST(std::string const& name, std::vector<std::string> args)
      : Name{name}
      , Args{std::move(args)} {}

  auto getName() const noexcept -> std::string const& { return Name; }

  auto codegen() -> llvm::Function*;
};

// BUG: the signature of the function is not validated against the prototype
class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> proto,
              std::unique_ptr<ExprAST> body)
      : Proto{std::move(proto)}
      , Body{std::move(body)} {}

  auto codegen() -> llvm::Function*;
};

// ----------------------------------------------------------------------------
// Parser

static int CurTok{};

// get the next token from the lexer
static auto getNextToken() -> int { return CurTok = gettok(); }

static auto LogError(char const* str) -> std::unique_ptr<ExprAST> {
  fprintf(stderr, "LogError: %s\n", str);
  return nullptr;
}

static auto LogErrorP(char const* str) -> std::unique_ptr<PrototypeAST> {
  LogError(str);
  return nullptr;
}

static auto ParseNumberExpr() -> std::unique_ptr<ExprAST> {
  auto result = std::make_unique<NumberExprAST>(NumVal);
  getNextToken();
  return result;
}

// forward declare
static auto ParseExpression() -> std::unique_ptr<ExprAST>;

static auto ParseParenExpr() -> std::unique_ptr<ExprAST> {
  getNextToken();  // eat (
  auto v = ParseExpression();
  if (!v) {
    return nullptr;
  }

  if (CurTok != ')') {
    return LogError("expected ')'");
  }
  getNextToken();  // eat )
  return v;
}

// IdentifierExpr
//  ::= identifier
//  ::= identifier '(' expression* ')'
static auto ParseIdentifierExpr() -> std::unique_ptr<ExprAST> {
  std::string id_name = IdentifierStr;
  getNextToken();

  if (CurTok != '(') {
    return std::make_unique<VariableExprAST>(id_name);
  }

  getNextToken();
  std::vector<std::unique_ptr<ExprAST>> args{};
  if (CurTok != ')') {
    for (;;) {
      if (auto arg = ParseExpression()) {
        args.push_back(std::move(arg));
      } else {
        return nullptr;
      }

      if (CurTok == ')') {
        break;
      }

      if (CurTok != ',') {
        return LogError("expected ')' or ',' in argument list");
      }

      getNextToken();
    }
  }

  getNextToken();
  return std::make_unique<CallExprAST>(id_name, std::move(args));
}

// PrimaryExpr
//  ::= IdentifierExpr
//  ::= NumberExpr
//  ::= ParenExpr
static auto ParsePrimary() -> std::unique_ptr<ExprAST> {
  switch (CurTok) {
  case Token::tok_identifier:
    return ParseIdentifierExpr();
  case Token::tok_number:
    return ParseNumberExpr();
  case '(':
    return ParseParenExpr();
  default:
    return LogError("unknown token when expecting expression");
  }
}

static std::map<char, int> BinopPrecedence{};

static auto GetTokPrecedence() -> int {
  if (!isascii(CurTok)) {
    return -1;
  }

  auto prec = BinopPrecedence.find(CurTok);
  if (BinopPrecedence.end() == prec) {
    return -1;
  }
  return (*prec).second;
}

static auto ParseBinOpRHS(int expr_prec, std::unique_ptr<ExprAST> lhs)
    -> std::unique_ptr<ExprAST> {
  for (;;) {
    auto const token_prec = GetTokPrecedence();
    if (token_prec < expr_prec) {
      return lhs;
    }

    auto const binop = CurTok;
    getNextToken();

    auto rhs = ParsePrimary();
    if (!rhs) {
      return nullptr;
    }

    auto const next_prec = GetTokPrecedence();
    if (token_prec < next_prec) {
      rhs = ParseBinOpRHS(token_prec + 1, std::move(rhs));
      if (!rhs) {
        return nullptr;
      }
    }

    lhs =
        std::make_unique<BinaryExprAST>(binop, std::move(lhs), std::move(rhs));
  }
}

static auto ParseExpression() -> std::unique_ptr<ExprAST> {
  auto lhs = ParsePrimary();
  if (!lhs) {
    return nullptr;
  }

  return ParseBinOpRHS(0, std::move(lhs));
}

static auto ParsePrototype() -> std::unique_ptr<PrototypeAST> {
  if (CurTok != tok_identifier) {
    return LogErrorP("expected function name in prototype");
  }

  std::string fn_name = IdentifierStr;
  getNextToken();

  if (CurTok != '(') {
    return LogErrorP("expected '(' in prototype");
  }

  std::vector<std::string> arg_names{};
  while (getNextToken() == tok_identifier) {
    arg_names.push_back(IdentifierStr);
  }

  if (CurTok != ')') {
    return LogErrorP("expected ')' in prototype");
  }

  getNextToken();

  return std::make_unique<PrototypeAST>(fn_name, std::move(arg_names));
}

static auto ParseDefinition() -> std::unique_ptr<FunctionAST> {
  getNextToken();
  auto proto = ParsePrototype();
  if (!proto) {
    return nullptr;
  }

  if (auto e = ParseExpression()) {
    return std::make_unique<FunctionAST>(std::move(proto), std::move(e));
  }
  return nullptr;
}

static auto ParseExtern() -> std::unique_ptr<PrototypeAST> {
  getNextToken();
  return ParsePrototype();
}

static auto ParseTopLevelExpression() -> std::unique_ptr<FunctionAST> {
  if (auto e = ParseExpression()) {
    // make a dummy prototype for the top-level expression with a known symbol
    auto proto = std::make_unique<PrototypeAST>(ANON_EXPR_NAME,
                                                std::vector<std::string>{});
    return std::make_unique<FunctionAST>(std::move(proto), std::move(e));
  }
  return nullptr;
}

// ----------------------------------------------------------------------------
// Code Generation

static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::Module> TheModule;
static std::unique_ptr<llvm::IRBuilder<>> Builder;

// function pass manager
static std::unique_ptr<legacy::FunctionPassManager> TheFPM;

// named values local to a function body
static std::map<std::string, llvm::Value*> NamedValues{};

// module function prototypes
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos{};

static auto LogErrorV(std::string const& msg) -> llvm::Value* {
  fprintf(stderr, "%s\n", msg.c_str());
  return nullptr;
}

auto getFunction(std::string const& name) -> llvm::Function* {
  if (auto* f = TheModule->getFunction(name)) {
    return f;
  }

  auto fi = FunctionProtos.find(name);
  if (fi != FunctionProtos.end()) {
    return fi->second->codegen();
  }

  return nullptr;
}

auto NumberExprAST::codegen() -> llvm::Value* {
  return llvm::ConstantFP::get(*TheContext, llvm::APFloat(Val));
}

auto VariableExprAST::codegen() -> llvm::Value* {
  auto v = NamedValues.find(Name);
  if (NamedValues.end() == v) {
    LogErrorV("unknown variable name");
  }
  return (*v).second;
}

auto BinaryExprAST::codegen() -> llvm::Value* {
  auto* l = Lhs->codegen();
  auto* r = Rhs->codegen();
  if (!l || !r) {
    return nullptr;
  }

  switch (Op) {
  case '+':
    return Builder->CreateFAdd(l, r, "addtmp");
  case '-':
    return Builder->CreateFSub(l, r, "subtmp");
  case '*':
    return Builder->CreateFMul(l, r, "multmp");
  case '<':
    l = Builder->CreateFCmpULT(l, r, "cmptmp");
    return Builder->CreateUIToFP(l, llvm::Type::getDoubleTy(*TheContext),
                                 "booltmp");
  default:
    return LogErrorV("invalid binary operation");
  }
}

auto CallExprAST::codegen() -> llvm::Value* {
  auto* callee_f = getFunction(Callee);
  if (!callee_f) {
    return LogErrorV("invalid function referenced");
  }

  if (callee_f->arg_size() != Args.size()) {
    return LogErrorV("incorrect number of arguments passed to function call");
  }

  std::vector<llvm::Value*> args{};
  std::transform(Args.cbegin(), Args.cend(), std::back_inserter(args),
                 [](auto const& expr) { return expr->codegen(); });

  // ensure that all of the codegen succeeded
  // NOTE: slightly less efficient to do this in separate steps
  // because we prohibit the ability to drop out of transform early
  // NOTE: a transform algorithm supporting early return would
  // be cool, perhaps something like a std::transform_while()
  auto const all_ok =
      std::none_of(args.cbegin(), args.cend(),
                   [](llvm::Value const* v) { return nullptr == v; });
  if (!all_ok) {
    return nullptr;
  }

  return Builder->CreateCall(callee_f, args, "calltmp");
}

auto PrototypeAST::codegen() -> llvm::Function* {
  // make the function type
  std::vector<llvm::Type*> doubles(Args.size(),
                                   llvm::Type::getDoubleTy(*TheContext));
  auto* func_type = llvm::FunctionType::get(
      llvm::Type::getDoubleTy(*TheContext), doubles, false);
  auto* func = llvm::Function::Create(
      func_type, llvm::Function::ExternalLinkage, Name, TheModule.get());
  std::for_each(func->args().begin(), func->args().end(),
                [&, i = 0U](auto& arg) mutable { arg.setName(Args[i++]); });
  return func;
}

auto FunctionAST::codegen() -> llvm::Function* {
  // transfer ownership of the prototype to the FunctionProtos map;
  // keep a local reference around for use below
  auto& p = *Proto;
  FunctionProtos[Proto->getName()] = std::move(Proto);
  auto* func = getFunction(p.getName());
  if (!func) {
    return nullptr;
  }

  // create a new basic block for the body of the function
  auto* bb = llvm::BasicBlock::Create(*TheContext, "entry", func);
  Builder->SetInsertPoint(bb);

  // set up the named variables for this function body
  NamedValues.clear();
  for (auto& arg : func->args()) {
    // NOTE: gross... temporary string construction
    NamedValues[std::string(arg.getName())] = &arg;
  }

  if (auto* retval = Body->codegen()) {
    // finish off the function code generation
    Builder->CreateRet(retval);

    // verify the correctness of the generated code
    llvm::verifyFunction(*func);

    // optimize the generated IR
    TheFPM->run(*func);
    return func;
  }

  // error reading the function body; bail
  func->eraseFromParent();
  return nullptr;
}

// ----------------------------------------------------------------------------
// Top-Level Handlers

// helper for bailing on failure (overloads operator())
static llvm::ExitOnError ExitOnErr{};

// JIT compilation context
using JITContext = llvm::orc::KaleidoscopeJIT;
static std::unique_ptr<JITContext> TheJIT;

static auto InitializeModuleAndPassManager() -> void {
  TheContext = std::make_unique<llvm::LLVMContext>();
  Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);

  TheModule = std::make_unique<llvm::Module>("Dope JIT", *TheContext);
  TheModule->setDataLayout(TheJIT->getDataLayout());

  // instantiate the manager for function-level optimizations
  TheFPM = std::make_unique<legacy::FunctionPassManager>(TheModule.get());

  // simple "peephole" optimizations and bit-twiddling optimizations
  TheFPM->add(llvm::createInstructionCombiningPass());
  // reassociate expressions
  TheFPM->add(llvm::createReassociatePass());
  // eliminate common subexpressions
  TheFPM->add(llvm::createGVNPass());
  // simplify control flow graph
  TheFPM->add(llvm::createCFGSimplificationPass());

  TheFPM->doInitialization();
}

static auto HandleDefinition() -> void {
  if (auto ast = ParseDefinition()) {
    if (auto* ir = ast->codegen()) {
      fprintf(stderr, "Read a function definition:\n");
      ir->print(llvm::errs());
      fprintf(stderr, "\n");

      ExitOnErr(TheJIT->addModule(llvm::orc::ThreadSafeModule{
          std::move(TheModule), std::move(TheContext)}));
      InitializeModuleAndPassManager();
    }
  } else {
    getNextToken();
  }
}

static auto HandleExtern() -> void {
  if (auto ast = ParseExtern()) {
    if (auto* ir = ast->codegen()) {
      fprintf(stderr, "Read an extern:\n");
      ir->print(llvm::errs());
      fprintf(stderr, "\n");

      FunctionProtos[ast->getName()] = std::move(ast);
    }
  } else {
    getNextToken();
  }
}

static auto HandleTopLevelExpression() -> void {
  using call_f = double (*)();

  if (auto ast = ParseTopLevelExpression()) {
    if (ast->codegen()) {
      // create a ResourceTracker instance to tracked JIT'd memory
      // allocated to our anonymous expression, so we can free it later
      auto tracker = TheJIT->getMainJITDylib().createResourceTracker();

      auto tsm = llvm::orc::ThreadSafeModule{std::move(TheModule),
                                             std::move(TheContext)};
      ExitOnErr(TheJIT->addModule(std::move(tsm), tracker));
      InitializeModuleAndPassManager();

      // search the JIT-ed code for the __anon_expr symbol
      auto expr_sym = ExitOnErr(TheJIT->lookup(ANON_EXPR_NAME));

      // get the symbol's address and cast it to the correct type;
      // this allows us to call it as a native function
      auto fp = reinterpret_cast<call_f>((intptr_t)expr_sym.getAddress());
      fprintf(stderr, "evaluated: %f\n", fp());

      // delete the anonymous expression module from the JIT
      ExitOnErr(tracker->remove());
    }
  } else {
    getNextToken();
  }
}

auto prompt() -> void { fprintf(stderr, "ready> "); }

static auto MainLoop() -> void {
  for (;;) {
    prompt();
    switch (CurTok) {
    case tok_eof:
      return;
    case ';':
      getNextToken();
      break;
    case tok_def:
      HandleDefinition();
      break;
    case tok_extern:
      HandleExtern();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}

// ----------------------------------------------------------------------------
// "Library" Functions (can be called from REPL)

extern "C" auto id(double const x) -> double { return x; }

// ----------------------------------------------------------------------------
// Driver Entry

auto main() -> int {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40;

  prompt();
  getNextToken();

  TheJIT = ExitOnErr(JITContext::Create());
  InitializeModuleAndPassManager();

  MainLoop();

  return EXIT_SUCCESS;
}