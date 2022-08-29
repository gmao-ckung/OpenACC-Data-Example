programName = OACC_DATA_TEST

FC = gfortran
#OPT = -O3 -cpp -Mflushz -Mfunc32 -Kieee #NVIDIA compiler options
#OPT = -O3 -cpp -Mfunc32 -Kieee -acc=gpu -gpu=flushz -Minfo=acc #NVIDIA compiler options
OPT = -cpp -O3 -ffree-line-length-none -fopenacc -foffload=nvptx-none -DGNU_OACC# Gfortran compiler options
OBJ = example2.o

%.o : %.f90
	$(FC) -c -o $@ $^ $(OPT) $(INC)

$(programName) : $(OBJ)
	$(FC) -o $@ $^ $(OPT) $(INC) $(LIB)

clean :
	rm -rf *.o *.mod $(programName) *~ *.out output_data s_*
