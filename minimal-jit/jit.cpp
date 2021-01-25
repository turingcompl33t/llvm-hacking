// jit.cpp
// A minimal example of JIT compilation with LLVM ORC APIs.

#include <cstdio>
#include <cstdlib>

#include <llvm/Support/Error.h>

#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

// signature for the function we codegen
using add_f = int (*)(int, int);

auto exit_on_error(llvm::Error e) {
  static llvm::ExitOnError err{};
  return err(std::move(e));
}

template <typename T> auto exit_on_error(llvm::Expected<T>&& e) {
  static llvm::ExitOnError err{};
  return err(std::move(e));
}

auto codegen(llvm::LLVMContext& ctx, llvm::Module& mod,
             llvm::IRBuilder<>& builder) -> llvm::Function* {
  // construct an array to represent our function arguments
  std::array<llvm::Type*, 2> args{llvm::Type::getInt32Ty(ctx),
                                  llvm::Type::getInt32Ty(ctx)};
  // construct the function type: int(int, int)
  auto* func_type =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx), args, false);
  // construct the function itself
  auto* function = llvm::Function::Create(
      func_type, llvm::Function::ExternalLinkage, "Add", mod);

  auto* arg_a = function->getArg(0);
  auto* arg_b = function->getArg(1);
  // name the arguments (optional)
  arg_a->setName("a");
  arg_b->setName("b");

  // create a BasicBlock for the body of the function
  auto* bb = llvm::BasicBlock::Create(ctx, "", function);
  builder.SetInsertPoint(bb);

  // fill the basic block
  auto* sum = builder.CreateAdd(arg_a, arg_b, "");
  builder.CreateRet(sum);

  // verify the generated function
  llvm::verifyFunction(*function);

  return function;
}

auto main() -> int {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  // construct the LLVM context object
  auto context = std::make_unique<llvm::LLVMContext>();

  // construct the JIT builder (only way to create instance of LLJIT)
  auto jit_builder = llvm::orc::LLJITBuilder{};

  // attempt to construct the JIT instance
  // NOTE: there must be a more idiomatic way to work with Expected<T>
  auto jit_exp = jit_builder.create();
  if (!jit_exp) {
    return EXIT_FAILURE;
  }
  auto& jit = jit_exp.get();

  // construct the LLVM module object (NOTE: module is now a reserved word...)
  auto module_ = std::make_unique<llvm::Module>("TheModule", *context);
  module_->setDataLayout(jit->getDataLayout());

  // check what the data layout is (optional):
  // auto& layout = jit->getDataLayout();
  // fprintf(stdout, "%s\n", layout.getStringRepresentation().c_str());
  //
  // => e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128
  // e    - little endian
  // m:e  - ELF mangling
  // p27X:Y:Z
  //  - 27X address space size
  //  - Y bit pointer
  //  - Z ABI
  // i64:64      - alignment for 64 bit integer is 64
  // f80:128     - alignment for 80 bit float is 128
  // n8:16:32:64 - set of native integer type widths
  // S128        - natural stack alignment is 128 bits

  // construct the LLVM IR builder
  auto builder = llvm::IRBuilder{*context};

  // codegen some IR
  // NOTE: we don't need the returned Function* here
  auto* function = codegen(*context, *module_, builder);
  (void)function;

  // create a resource tracker
  auto tracker = jit->getMainJITDylib().createResourceTracker();

  // construct a ThreadSafeModule instance;
  // add the instance to the JIT instance
  auto tsm =
      llvm::orc::ThreadSafeModule{std::move(module_), std::move(context)};
  exit_on_error(jit->addIRModule(tracker, std::move(tsm)));

  // lookup the symbol for our function in the JIT
  auto symbol = exit_on_error(jit->lookup("Add"));
  // reinterpret the symbol as a native function call
  auto add = reinterpret_cast<add_f>((intptr_t)symbol.getAddress());

  // invoke the native function
  auto const result = add(2, 3);
  fprintf(stdout, "Result: %d\n", result);

  // cleanup and exit
  exit_on_error(tracker->remove());
  return EXIT_SUCCESS;
}