LDFLAGS=-lgc
#CFLAGS=-Wextra -pedantic -Wall -g -std=c11 -save-temps
CFLAGS=-pedantic -Wall -g -std=c11 -save-temps
#CFLAGS=-O3
CC=gcc
#CFLAGS=-Weverything -Wsign-compare -pedantic -Wall -g -std=c11 -save-temps
#CC=clang

scheme: scheme.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

clean:
	rm -f scheme

prototypes:
	@ctags -f - scheme.c | grep -P '\tf\t|f$$' | sed -e 's#.*/\^##' -e 's#\$$/.*##' -e 's# {#;#'

