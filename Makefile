.PHONY: help

help:
	@printf "Usage:\n\n  make <target>\n\nwhere <target> is one of:\n\n\
	  lib         create 'out/lib/liblx.a'\n\
	  tool        create 'out/lxtool'\n\
	  test        run internal tests\n\
	  test-conv   run sample conversion tests\n\
	  clean       remove unnecessary files\n\
	  help        show help (this screen)\n"

# lib: create the library.
.PHONY: lib
lib: out/lib/liblx.a

# tool: create the tool.
.PHONY: tool
tool: out/lxtool.exe

# test: run tests.
.PHONY: test
test: out/lxtool.exe
	@printf "Running tests.\n"
	out/lxtool.exe test
	@$(PRINT-STATUS)

# clean: remove unnecessary files
.PHONY: clean
clean:
	@printf "Removing unnecessary files\n"
	rm -f *.stackdump
	@$(PRINT-STATUS)
# rm -f   do not report an error if a file does not exist.

# Use gcc.
CC := gcc
LD := gcc

# The project consists of a library and a tool that run tests and samples.
# The library is built from all files except 'lxtool'. The tool is built from
# the library and 'lxtool' object.

allsrc := $(basename $(wildcard src/*.c))
libsrc := $(filter-out src/lxtool,$(allsrc))
exesrc := $(filter     src/lxtool,$(allsrc))
libobj := $(patsubst src/%,out/obj/%.o,$(libsrc))
exeobj := $(patsubst src/%,out/obj/%.o,$(exesrc))

$(libobj) $(exeobj) : CFLAGS += -Wall

out/obj/%.o : src/%.c src/lx.h
	@$(MAKE-PARENT-DIR)
	@printf "Compile C: $(filter %.c,$^) -> $@\n"
	$(CC) $(CFLAGS) -c -o $@ $(filter %.c,$^)
	@$(PRINT-STATUS)

out/lib/liblx.a : $(libobj)
	@$(MAKE-PARENT-DIR)
	@printf "Make 'liblx': $@\n"
	$(AR) crs $@ $(filter %.o,$^)
	@$(PRINT-STATUS)

out/lxtool.exe : $(exeobj) out/lib/liblx.a
	@$(MAKE-PARENT-DIR)
	@printf "Make 'lxtool': $@\n"
	$(LD) $(LFLAGS) $(filter %.o,$^) -Lout/lib -llx -o $@
	@$(PRINT-STATUS)

# Make sure the parent directory for the target exists.
MAKE-PARENT-DIR = \
    if [ ! -d "$(@D)" ] ; then mkdir -p "$(@D)" ; fi
# mkdir -p <path>: create intermediate directories

PRINT-STATUS = \
    if [ $$? -eq 0 ] ; \
      then printf "$(G)OK$(Z)\n" ; \
      else printf "$(R)Error$(Z)\n" ; fi

# Color codes.

R := \e[31;1m
G := \e[32;1m
Z := \e[0m

# Generate encoding repertoire.

out/rep/s-%.txt: out/lxtool.exe
	@$(MAKE-PARENT-DIR)
	@printf "Generate code repertoire for $*.\n"
	out/lxtool.exe enc-print $* > $@
	@$(PRINT-STATUS)

# Test sample encoding conversion
.PHONY: test-conv
test-conv: \
  out/rep/c-utf8-utf16be.txt \
  out/rep/c-utf8-utf16le.txt \
  out/rep/c-utf16be-utf8.txt \
  out/rep/c-utf16be-utf16le.txt \
  out/rep/c-utf16le-utf8.txt \
  out/rep/c-utf16le-utf16be.txt
	rm out/rep/*.txt

out/rep/c-utf8-%.txt: out/rep/s-%.txt out/rep/s-utf8.txt
	@printf "Test conversion: utf8 -> $*.\n"
	out/lxtool.exe enc-conv utf8 $* < $(filter %/s-utf8.txt,$^) > $@
	diff $< $@
	@$(PRINT-STATUS)

out/rep/c-utf16be-%.txt: out/rep/s-%.txt out/rep/s-utf16be.txt
	@printf "Test conversion: utf16be -> $*.\n"
	out/lxtool.exe enc-conv utf16be $* < $(filter %/s-utf16be.txt,$^) > $@
	diff $< $@
	@$(PRINT-STATUS)

out/rep/c-utf16le-%.txt: out/rep/s-%.txt out/rep/s-utf16le.txt
	@printf "Test conversion: utf16le -> $*.\n"
	out/lxtool.exe enc-conv utf16le $* < $(filter %/s-utf16le.txt,$^) > $@
	diff $< $@
	@$(PRINT-STATUS)
