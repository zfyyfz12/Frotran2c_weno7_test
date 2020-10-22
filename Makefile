all : subc.c subf.f90 main.f90
	gcc -c subc.c
	gfortran -c subf.f90
	gfortran -c main.f90
	gfortran -pg -o main.out subc.o subf.o main.o
    
clean:
	rm -f *.o *.out