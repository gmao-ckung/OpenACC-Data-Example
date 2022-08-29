module ESMF_AnalogMod

    implicit none

    private

    public :: ESMF_Field
    public :: alloc_field

    type :: ESMF_Field
        real, pointer :: f_array(:)
    end type

contains

    subroutine alloc_field(obj, value1)
        type(ESMF_Field), intent(inout) :: obj
        real,             intent(in)    :: value1

        allocate(obj%f_array(10))

        obj%f_array = value1

        !$acc enter data copyin(obj) 
        !$acc enter data copyin(obj%f_array)

    end subroutine

end module ESMF_AnalogMod

module KernelMod

    implicit none
    private

    public :: kernel

contains

    subroutine kernel(array)

        real, intent(inout) :: array(:)
        integer :: ii

!$acc parallel loop present(array)
        do ii = 1,10
            array(ii) = array(ii)+array(ii) - 1.0
        enddo
!$acc end parallel loop

    end subroutine kernel

end module KernelMod

module ComponentMod

    use ESMF_AnalogMod
    use KernelMod
    implicit none
    private

    public run_example

contains

    subroutine run_example(this)
        type(ESMF_Field), intent(inout) :: this

        real, pointer :: ptr(:)

        ptr => this%f_array

        call kernel(ptr) 
    end subroutine run_example

end module ComponentMod

program main
    
    use ComponentMod
    use ESMF_AnalogMod
    implicit none
    
    type(ESMF_Field) :: f

    call alloc_field(f, 7.0)

!$acc update host(f%f_array)

    print*, "f%f_array = ", f%f_array

    call run_example(f)

!$acc update host(f%f_array)

    print*, "f%f_array = ", f%f_array

end program