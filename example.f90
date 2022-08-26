module oACC_module
    implicit none

    real, dimension(:,:), allocatable, target :: A1

     type :: oACC_module_type
          real, dimension(:,:), allocatable :: A1
     end type

    !$acc declare create(A1)

     contains

        subroutine sub1(A1_dtype)
        ! ******************************************************************
        ! *** Note : Passing the type oACC_module_type into sub1 creates ***
        ! ***        an internal compiler error with gfortran when using ***
        ! ***        openACC offloading.  It compiles and runs fine when ***
        ! ***        compiled with nvfortran.                            ***
        ! ******************************************************************
        ! subroutine sub1(typeExample)
        
            implicit none
        
            integer :: I, J
            real, dimension(:,:) :: A1_dtype
            ! type(oACC_module_type) :: typeExample
        
        ! !$acc kernels present(A1)
        !     A1 = A1+1
        ! !$acc end kernels
        
        !$acc parallel loop collapse(2) present(A1, A1_dType)
        !!$acc parallel loop collapse(2) present(A1, typeExample%A1)
            do J = 1,3
                do I = 1,3
                    A1(I,J) = A1(I,J) + 1
                    A1_dType(I,J) = A1_dType(I,J) - 1
                    ! typeExample%A1(I,J) = typeExample%A1(I,J) - 1
                enddo
            enddo
        !$acc end parallel loop
        
        end subroutine

end module

program oacc_data_example

    use openacc
    use oACC_module

    implicit none

    integer :: I, J
    real, dimension(:,:), pointer :: A1_ptr
    type(oACC_module_type), target :: typeExample

    !********************************************************************************
    !*** With nvfortran, tbe CPU allocate statement implicitly allocates arrays   ***
    !*** in GPU memory if array is specified in the module with declare create".  ***
    !*** HOWEVER, with gfortran, GPU data specified with "declare create" needs   ***
    !*** to be speified in "call acc_copyin" in order to be allocated on the GPU  ***
    !********************************************************************************

    allocate(A1(3,3))
    allocate(typeExample%A1(3,3))

#ifdef GNU_OACC
    print*,'*** Gfortran adjustment : Copying A1 to GPU ***'
    call acc_copyin(A1)
    !***********************************************************************
    !*** Using "!$acc enter data" creates a crash during execution with  ***
    !*** gfortran, though I think it should work.                        ***
    !***********************************************************************
    !!$acc enter data copyin(A1)

    call acc_copyin(typeExample)
    call acc_copyin(typeExample%A1)
    !*******************************************************************************
    !*** I think that "!$acc enter data" should work with derived data types in  ***
    !*** gfortran, but values written to typeExample%A1 are not valid            ***
    !*******************************************************************************
    !!$acc enter data create(typeExample)
    !!$acc enter data create(typeExample%A1)
#else
    !$acc enter data create(typeExample)
    !$acc enter data create(typeExample%A1)
#endif

    ! ***********************************************************************************************
    ! *** Note on !$acc kernels : When using "!$acc kernels" with gfortran, the code needs to be  ***
    ! *** compiled with either -O0 or -O1 to generate valid results.  Using -O2 or -O3 seems to   ***
    ! *** create a race condition.                                                                ***
    ! ***********************************************************************************************


    !****************************************************************
    !*** Initializing A1 and typeExample%A1 values in CPU memory. ***
    !****************************************************************
    
    A1 = 0.0
    typeExample%A1 = -1.0

    !********************************************************************
    !*** Computing/Setting values of A1 and typeExample%A1 on the GPU ***
    !********************************************************************

    print*,'*** Computing on GPU ***'

! !$acc kernels present(A1)
!     A1 = 7.0
! !$acc end kernels

!$acc parallel loop collapse(2) present(A1, typeExample%A1)
    do J = 1,3
        do I = 1,3
            A1(I,J) = 7.0
            typeExample%A1(I,J) = -7.0
        enddo
    enddo
!$acc end parallel loop

    print*, '*** Done computing on GPU ***'
    print*, '*** Printing Values of A1 and typeExample%A1 in CPU memory ***'
    print*, 'sum(A1) = ', sum(A1)
    print*, 'sum(typeExample%A1) = ', sum(typeExample%A1)
    print*, '*** Copying values of A1 and typeExample%A1 from GPU memory to CPU memory ***'

!$acc update host(A1)
!$acc update host(typeExample%A1)

    print*, '*** Printing Values of A1 and typeExample%A1 in CPU memory based on copy from GPU memory ***'

    print*, 'sum(A1) = ', sum(A1)
    print*, 'sum(typeExample%A1) = ', sum(typeExample%A1)
    
    if(sum(A1).eq.63.0) then
        print*,'*** Test for A1 passed! ***'
    else
        print*,'*** Test for A1 failed! ***'
    endif

    if(sum(typeExample%A1).eq.-63.0) then
        print*,'*** Test for typeExample%A1 passed! ***'
    else
        print*,'*** Test for typeExample%A1 failed! ***'
    endif

    print*, '*************************************************************************'
    print*, '*** Calling sub1 subroutine ***'

    call sub1(typeExample%A1)
    ! call sub1(typeExample)

    print*, '*** Printing Values of A1 and typeExample%A1 in CPU memory ***'
    print*, 'sum(A1) = ', sum(A1)
    print*, 'sum(typeExample%A1) = ', sum(typeExample%A1)
    print*, '*** Copying values of A1 and typeExample%A1 in GPU memory to CPU memory ***'

!$acc update host(A1, typeExample%A1)

    print*, '*** Printing Values of A1 and typeExample%A1 in CPU memory based on copy from GPU memory ***'
    print*, 'sum(A1) = ', sum(A1)
    print*, 'sum(typeExample%A1) = ', sum(typeExample%A1)

    if(sum(A1).eq.72.0) then
        print*,'*** Test for A1 passed! ***'
    else
        print*,'*** Test for A1 failed! ***'
    endif

    if(sum(typeExample%A1).eq.-72.0) then
        print*,'*** Test for typeExample%A1 passed! ***'
    else
        print*,'*** Test for typeExample%A1 failed! ***'
    endif
    print*,'**************************************************************************'

    print*,'*** Going into OpenACC Data region ***'

!$acc data present(A1, typeExample%A1)
    print*, '*** Computing/Setting values of A1 and typeExample%A1 on GPU ***'

! !$acc kernels
!     A1 = A1+2
! !$acc end kernels

!$acc parallel loop collapse(2)
    do J = 1,3
        do I = 1,3
            A1(I,J) = A1(I,J) + 2.0
            typeExample%A1(I,J) = typeExample%A1(I,J) - 2.0
        enddo
    enddo
!$acc end parallel loop
!$acc end data

    print*,'*** Exiting OpenACC Data region ***'
    print*,'*** Printing Values of A1 and typeExample%A1 in CPU memory ***'
    print*, 'sum(A1) = ', sum(A1)
    print*, 'sum(typeExample%A1) = ', sum(typeExample%A1)
    print*, '*** Copying values of A1 and typeExample%A1 in GPU memory to CPU memory ***'

!$acc update host(A1, typeExample%A1)

    print*, '*** Printing Values of A1 and typeExample%A1 in CPU memory based on copy from GPU memory ***'
    print*, 'sum(A1) = ', sum(A1)
    print*, 'sum(typeExample%A1) = ', sum(typeExample%A1)
    if(sum(A1).eq.90.0) then
        print*,'*** Test for A1 passed! ***'
    else
        print*,'*** Test for A1 failed! ***'
    endif

    if(sum(typeExample%A1).eq.-90.0) then
        print*,'*** Test for typeExample%A1 passed! ***'
    else
        print*,'*** Test for typeExample%A1 failed! ***'
    endif
    print*,'**************************************************************************'

    print*, '*** Computing/Setting values of A1 and typeExample%A1 on GPU via pointer ***'

    A1_ptr => A1

! !$acc kernels present(A1_ptr)
!     A1_ptr = A1_ptr-1   
! !$acc end kernels

!$acc parallel loop collapse(2) present(A1_ptr)
    do J = 1,3
        do I = 1,3
            A1_ptr(I,J) = A1_ptr(I,J) - 1
        enddo
    enddo
!$acc end parallel loop

    A1_ptr => typeExample%A1

!$acc parallel loop collapse(2) present(A1_ptr)
    do J = 1,3
        do I = 1,3
            A1_ptr(I,J) = A1_ptr(I,J) + 1
        enddo
    enddo
!$acc end parallel loop

    print*, '***Printing Values of A1 and typeExample%A1 in CPU memory***'
    print*, 'sum(A1) = ', sum(A1)
    print*, 'sum(typeExample%A1) = ', sum(typeExample%A1)
!$acc update host(A1, typeExample%A1)

    print*, '***Printing Values of A1 and typeExample%A1 in CPU memory based on copy from GPU memory***'
    print*, 'sum(A1) = ', sum(A1)
    print*, 'sum(typeExample%A1) = ', sum(typeExample%A1)
    if(sum(A1).eq.81.0) then
        print*,'*** Test for A1 passed! ***'
    else
        print*,'*** Test for A1 failed! ***'
    endif

    if(sum(typeExample%A1).eq.-81.0) then
        print*,'*** Test for typeExample%A1 passed! ***'
    else
        print*,'*** Test for typeExample%A1 failed! ***'
    endif
    print*,'**************************************************************************'

end program

