module oACC_module
    implicit none

    real, dimension(:,:), allocatable, target :: A1
    real, dimension(:,:), pointer             :: A1_ptr

    !  type :: oACC_module_type
    !       real, dimension(:,:), allocatable :: A1_inType
    !  end type

    !$acc declare create(A1)

end module

program oacc_data_example

    use openacc
    use oACC_module

    implicit none

    integer :: I, J

    ! type(oACC_module_type) typeExample

    !********************************************************************************
    !*** With nvfortran, tbe CPU allocate statement implicitly allocates arrays   ***
    !*** in GPU memory if array is specified in "declare create".                 ***
    !*** HOWEVER, with gfortran, GPU data specified with "declare create" needs   ***
    !*** to be speified in "call acc_copyin" in order to be allocated on the GPU  ***
    !********************************************************************************

    allocate(A1(3,3))
    ! allocate(typeExample%A1_inType(3,3))

#ifdef GNU_OACC
    print*,'*** Gfortran adjustment : Copying A1 to GPU ***'
    call acc_copyin(A1)
    !***********************************************************************
    !*** Using "!$acc enter data" creates a crash during execution with  ***
    !*** gfortran, though I think it should work.                        ***
    !***********************************************************************
    !!$acc enter data copyin(A1)
#endif

    ! ***********************************************************************************************
    ! *** Note on !$acc kernels : When using "!$acc kernels" with gfortran, the code needs to be  ***
    ! ***        compileed with either -O0 or -O1 to generate valid results.  Using -O2 or -O3    ***
    ! ***        seems to create a race condition.                                                ***
    ! ***********************************************************************************************


    !***************************************************************************
    !*** Initializing A1 on CPU memory with zeros                            ***
    !*** Note : This does not initialize the arrays with zeros in GPU memory ***
    !***************************************************************************
    A1 = 0.0

    !*************************************************
    !*** Computing/Setting values of A1 on the GPU ***
    !*************************************************

    print*,'*** Computing on GPU ***'
! !$acc kernels present(A1)
!     A1 = 7.0
! !$acc end kernels

!$acc parallel loop collapse(2) present(A1)
    do J = 1,3
        do I = 1,3
            A1(I,J) = 7.0
        enddo
    enddo
!$acc end parallel loop

    print*, '*** Done computing on GPU ***'
    print*, '*** Printing Values of A1 in CPU memory ***'
    print*, 'sum(A1) = ', sum(A1)
    print*, '*** Copying values of A1 in GPU memory to CPU memory ***'

!$acc update host(A1)

    print*, '*** Printing Values of A1 in CPU memory based on copy from GPU memory ***'

    print*, 'sum(A1) = ', sum(A1)
    
    if(sum(A1).eq.63.0) then
        print*,'*** Test passed! ***'
    else
        print*,'*** Test failed! ***'
    endif

    print*, '*************************************************************************'
    print*, '*** Calling sub1 subroutine ***'

    call sub1

    print*, '*** Printing Values of A1 in CPU memory ***'
    print*, 'sum(A1) = ', sum(A1)
    print*, '*** Copying values of A1 in GPU memory to CPU memory ***'

!$acc update host(A1)

    print*, '*** Printing Values of A1 in CPU memory based on copy from GPU memory ***'
    print*, 'sum(A1) = ', sum(A1)
    if(sum(A1).eq.72.0) then
        print*,'*** Test passed! ***'
    else
        print*,'*** Test failed! ***'
    endif
    print*,'**************************************************************************'

    print*,'*** Going into OpenACC Data region ***'

!$acc data present(A1)
    print*, '*** Computing/Setting values of A1 on GPU ***'

! !$acc kernels
!     A1 = A1+2
! !$acc end kernels

!$acc parallel loop collapse(2)
    do J = 1,3
        do I = 1,3
            A1(I,J) = A1(I,J) + 2
        enddo
    enddo
!$acc end parallel loop
!$acc end data

    print*,'*** Exiting OpenACC Data region ***'
    print*,'*** Printing Values of A1 in CPU memory ***'
    print*, 'sum(A1) = ', sum(A1)
    print*, '*** Copying values of of A1 in GPU memory to CPU memory ***'

!$acc update host(A1)

    print*, '*** Printing Values of A1 in CPU memory based on copy from GPU memory ***'
    print*, 'sum(A1) = ', sum(A1)
    if(sum(A1).eq.90.0) then
        print*,'*** Test passed! ***'
    else
        print*,'*** Test failed! ***'
    endif
    print*,'**************************************************************************'

    A1_ptr => A1
    print*, '*** Computing/Setting values of A1 on GPU via pointer ***'

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

    print*, '***Printing Values of A1 in CPU memory***'
    print*, 'sum(A1) = ', sum(A1)

!$acc update host(A1)

    print*, '***Printing Values of A1 and A2 in CPU memory based on copy from GPU memory***'
    print*, 'sum(A1) = ', sum(A1)
    if(sum(A1).eq.81.0) then
        print*,'*** Test passed! ***'
    else
        print*,'*** Test failed! ***'
    endif
    print*,'**************************************************************************'

end program

subroutine sub1
    use oACC_module

    implicit none

    integer :: I, J

! !$acc kernels present(A1)
!     A1 = A1+1
! !$acc end kernels

!$acc parallel loop collapse(2) present(A1)
    do J = 1,3
        do I = 1,3
            A1(I,J) = A1(I,J) + 1
        enddo
    enddo
!$acc end parallel loop

end subroutine