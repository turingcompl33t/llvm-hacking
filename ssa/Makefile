# Makefile

# assumes clang is available on path
CC     = clang
CFLAGS = -Wall -Werror

recursive: recursive-c
	./recursive; echo $$?

recursive-c:
	$(CC) $(CFLAGS) -o recursive recursive.c

recursive-ir:

clean:
	rm -f *.o
	rm -f recursive

.PHONY: clean