# Eric Laursen, 25 October 2015, CS 441-001 Term Project
# Makefile for Color - a K-colorability on N-region graph solution implementing
# Y. Takefuji, et al., solution from Neural Network Parallel Computing,
# chapter 3

OBJS = color.o map.o

.adb.o:
	gcc -c $<

.SUFFIXES: .adb .o

color:	$(OBJS)
	gnatbind -xf color.ali
	gnatlink color.ali

clean:
	rm *.o *.ali color

tidy:
	rm *~
