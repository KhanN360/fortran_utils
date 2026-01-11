module utils
    ! Contains some useful fortran functions and subroutines
    implicit none
    
    ! Gas constants
    real(8), parameter :: GAMMA = 1.4d0
    real(8), parameter :: GAMMA_RATIO = (GAMMA + 1)/(GAMMA - 1)
    real(8), parameter :: RU = 8314.0d0
    real(8), parameter :: MW_AIR = 28.96d0 

    ! Math constants
    real(8), parameter :: PI = 3.141592
    
    contains

    subroutine write_array(x, filename)
        ! Write a 1D array x into a file
        implicit none
        real(8), intent(in) :: x(:)
        character(*), intent(in) :: filename
        integer :: i, unit

        open(newunit=unit, file=filename, status='replace', action='write')
        do i = 1, size(x)
            write(unit,'(ES16.8)') x(i)
        end do
        close(unit)
    end subroutine write_array

    subroutine write_solution_1d(x, u, filename)
        ! Write a 1D solution with the x location and u solution
        implicit none
        real(8), intent(in) :: x(:), u(:)
        character(*), intent(in) :: filename
        integer :: i, unit

        open(newunit=unit, file=filename, status='replace', action='write')
        do i = 1, size(x)
            write(unit,'(2ES16.8)') x(i), u(i)
        end do
        close(unit)
    end subroutine write_solution_1d

    subroutine print_vec(x, name)
        ! Print a vector similar to numpy
        implicit none
        real(8), intent(in) :: x(:)
        character(*), intent(in) :: name
        integer :: i

        write(*,'(A)', advance='no') trim(name)//" = [ "
        do i = 1, size(x)
            write(*,'(F10.4)', advance='no') x(i)
            if (i < size(x)) write(*,'(A)', advance='no') " "
        end do
        write(*,'(A)') " ]"
    end subroutine print_vec

    real(8) function Lnorms(x, order)
        ! Compute the vector norms
        ! TODO: Add in L1 and Linf norms
        implicit none
        real(8), dimension(:), intent(in) :: x
        integer, intent(in) :: order

        if ( order == 2 ) then
            Lnorms = sqrt(dot_product(x, x))
        end if

    end function Lnorms

    function linspace(a, b, n) result(x)
        ! A replica of linspace in LAPACK or numpy linspace
        implicit none
        integer, intent(in) :: n
        real(8), intent(in) :: a, b
        real(8), allocatable :: x(:)
        integer :: i

        allocate(x(n))

        if (n == 1) then
            x(1) = a
        else
            do i = 1, n
                x(i) = a + (b - a) * real(i - 1, 8) / real(n - 1, 8)
            end do
        end if
    end function linspace

end module utils