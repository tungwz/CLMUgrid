# CLMUgrid
gfortran -o mkglobal -fopenmp -g CLMUgrid.F90 -I$Inc -L$Lib -lnetcdf -lnetcdff

25/10/2021: fix bug of assigning urban porperties (Line 655)
