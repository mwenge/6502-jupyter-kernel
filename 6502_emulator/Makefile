# Create the Make rules for nesemu1
#
CXX=g++
CXXFLAGS=-g -Wall -Wno-misleading-indentation -Wno-implicit-fallthrough -Wno-unused-result -W -pedantic -Ofast -std=c++0x
CXXFLAGS += `pkg-config sdl --libs --cflags`

6502_kernel: 6502_kernel.o
	$(CXX) -o "$@" "$<" $(CXXFLAGS)

6502_kernel.o: 6502_kernel.cc
	$(CXX) -c -o "$@" "$<" $(CXXFLAGS)



