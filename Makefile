
all:
	clang++ -g -O3 -fno-rtti compiler.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core mcjit native` -I./../llvm-stuff/build/include/c++/v1/ -o compiler -Wno-move