# Makefile

CXX = clang++
CXXFLAGS = -g -O3 -Wall -Werror -fno-exceptions -std=c++14

# LLVM 12 installed in /usr/local
LLVM_CXX_FLAGS = -I/usr/local/include -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS
LLVM_LD_FLAGS = -L/usr/local/lib -lLLVM -rdynamic

run: driver
	LD_LIBRARY_PATH=/usr/local/lib ./driver

driver:
	$(CXX) $(CXXFLAGS) $(LLVM_CXX_FLAGS) $(LLVM_LD_FLAGS) -o driver driver.cpp

clean:
	rm -f *.o
	rm -f driver

.PHONY: clean