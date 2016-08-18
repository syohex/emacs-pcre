EMACS_ROOT ?= ../..
EMACS ?= emacs
PCRE_VERSION ?= 5.2

CC      = gcc
LD      = gcc
CPPFLAGS = -I$(EMACS_ROOT)/src $(shell pkg-config --cflags libpcre)
CFLAGS = -std=gnu99 -ggdb3 -Wall -fPIC $(CPPFLAGS)
PCRE_LIBS = $(shell pkg-config --libs libpcre)

.PHONY : test

all: pcre-core.so

pcre-core.so: pcre-core.o
	$(LD) -shared $(LDFLAGS) -o $@ $^ $(PCRE_LIBS)

pcre-core.o: pcre-core.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	-rm -f pcre-core.so pcre-core.o

test:
	$(EMACS) -Q -batch -L . $(LOADPATH) \
		-l test/test.el \
		-f ert-run-tests-batch-and-exit
