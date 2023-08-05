# Create the Make rules for nesemu1
#
CXX=g++
CXXFLAGS=-Wall -W -pedantic -Ofast -std=c++0x
CXXFLAGS += `pkg-config sdl --libs --cflags`

nesemu1: nesemu1.o
	$(CXX) -o "$@" "$<" $(CXXFLAGS)

nesemu1.o: nesemu1.cc
	$(CXX) -c -o "$@" "$<" $(CXXFLAGS)

6502_kernel: 6502_kernel.o
	$(CXX) -o "$@" "$<" $(CXXFLAGS)

6502_kernel.o: 6502_kernel.cc
	$(CXX) -c -o "$@" "$<" $(CXXFLAGS)



