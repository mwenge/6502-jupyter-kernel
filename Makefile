# Create the Make rules for nesemu1
#
CXX=g++
CXXFLAGS=-g -Wall -Wno-misleading-indentation -Wno-implicit-fallthrough -Wno-unused-result -W -pedantic -Ofast -std=c++0x
CXXFLAGS += `pkg-config sdl --libs --cflags`

little_6502_emu: little_6502_emu.o
	$(CXX) -o "$@" "$<" $(CXXFLAGS)

little_6502_emu.o: little_6502_emu.cc
	$(CXX) -c -o "$@" "$<" $(CXXFLAGS)

.PHONY: install
install: little_6502_emu
	install -d /usr/local/bin/
	install little_6502_emu /usr/local/bin/

