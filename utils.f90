module utils
    ! Contains some useful fortran functions and subroutines
    implicit none
    
    ! Gas constants
    real(8), parameter :: GAMMA = 1.4d0
    real(8), parameter :: GAMMA_RATIO = (GAMMA + 1)/(GAMMA - 1)
    real(8), parameter :: RU = 8314.0d0
    real(8), parameter :: MW_AIR = 28.96d0 

    ! Math constants
    real(8), parameter :: PI = acos(-1.0d0)
    
    contains

    ! ================================
    ! Array Printing
    ! ================================
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

    subroutine print_mat(A)
        ! Print out a 2d array similar to numpy
        real(8), dimension(:, :), intent(in) :: A
        integer :: i

        write(*,'(A)', advance='no') '[ '
        do i = 1, size(A,1)
            if (i > 1) write(*,'(A)', advance='no') '  '
            write(*,'(*(F8.3,1X))', advance='no') A(i,:)
            if (i < size(A,1)) then
                write(*,*) ''
            else
                write(*,'(A)') ']'
            end if
        end do
    end subroutine

    ! ================================
    ! Write out arrays
    ! ================================
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

    subroutine write_array_2d(A, filename)
        ! Write out a 2D array
        implicit none
        character(len=*), intent(in) :: filename
        real(8), intent(in)          :: A(:,:)

        integer :: i, j, nx, ny, unit

        nx = size(A, 1)
        ny = size(A, 2)

        open(newunit=unit, file=filename, status='replace', action='write')

        do j = 1, ny
            write(unit, *) (A(i, j), i = 1, nx)
        end do

        close(unit)
    end subroutine write_array_2d

    ! ================================
    ! Misc
    ! ================================
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