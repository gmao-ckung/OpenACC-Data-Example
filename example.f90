module oACC_module
    implicit none

    real, dimension(:,:), allocatable, target :: A1
    real, dimension(:,:), pointer             :: A1_ptr

    !$acc declare create(A1)

end module

program oacc_data_example

    use openacc
    use oACC_module

    implicit none

    integer :: I, J

    print*,'***CPU allocate statement implicitly allocates arrays in GPU memory***'
    print*,'***This happens due to the !$acc declare create statement within the module***'
    print*,'***NOTE : Implicit allocation happens with nvfortran, not gfortran!!!***'
    print*,'***       GPU Data with declare create on gfortran needs "call acc_copyin" to be allocated'
    print*,'*****'
    allocate(A1(3,3))

#ifdef GNU_OACC
    call acc_copyin(A1)
!!$acc enter data copyin(A1)
#endif

    print*, '***Initializing A1 and A2 on CPU memory with zeros***'
    print*, '***Note : This does not initialize the arrays with zeros in GPU memory***'
    print*,'*****'
    A1 = 0.0

    print*, '***Computing/Setting values of A1 and A2 on GPU and storing on GPU memory***'

!$acc kernels present(A1)
    A1 = 7.0
!$acc end kernels

    print*, '***Done computing on GPU***'
    print*, '***Printing Values of A1 in CPU memory***'
    print*, 'sum(A1) = ', sum(A1)
    print*, '***Copying values of of A1 in GPU memory to CPU memory***'

!$acc update host(A1)

    print*, '***Printing Values of A1 in CPU memory based on copy from GPU memory***'
    print*, 'sum(A1) = ', sum(A1)
    print*,'*****'
   print*, '***Calling sub1 subroutine***'

   call sub1

   print*, '***Printing Values of A1 in CPU memory***'
   print*, 'sum(A1) = ', sum(A1)
   print*, '***Copying values of of A1 in GPU memory to CPU memory***'

!$acc update host(A1)

   print*, '***Printing Values of A1 in CPU memory based on copy from GPU memory***'
   print*, 'sum(A1) = ', sum(A1)
   print*,'*****'
     print*,'***Going into OpenACC Data region***'

!$acc data present(A1)
     print*, '***Computing/Setting values of A1 on GPU and storing on GPU memory***'

!$acc kernels
     A1 = A1+1
!$acc end kernels

!$acc parallel loop collapse(2)
   do J = 1,3
     do I = 1,3
         A1(I,J) = A1(I,J) + 2
      enddo
   enddo
!$acc end parallel loop
!$acc end data

     print*,'***Exiting OpenACC Data region***'
     print*, '***Printing Values of A1 in CPU memory***'
     print*, 'sum(A1) = ', sum(A1)
     print*, '***Copying values of of A1 in GPU memory to CPU memory***'

!$acc update host(A1)

     print*, '***Printing Values of A1 in CPU memory based on copy from GPU memory***'
     print*, 'sum(A1) = ', sum(A1)
     A1_ptr => A1
     print*, '***Computing/Setting values of A1 on GPU via pointer and storing on GPU memory***'

!$acc kernels present(A1_ptr)
     A1_ptr = A1_ptr-1   
!$acc end kernels

     print*, '***Printing Values of A1 in CPU memory***'
     print*, 'sum(A1) = ', sum(A1)

!$acc update host(A1)

     print*, '***Printing Values of A1 and A2 in CPU memory based on copy from GPU memory***'
     print*, 'sum(A1) = ', sum(A1)

end program

subroutine sub1
   use oACC_module

   implicit none

!$acc kernels present(A1)
     A1 = A1+1
!$acc end kernels

end subroutine