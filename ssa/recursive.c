// recursive.c
// Simple recursive definition of factorial.

#include <stdio.h>

int factorial(int val) {
  if (0 == val) {
    return 1;
  }
  return val * factorial(val - 1);
}

int main() {
  return factorial(5);
}
