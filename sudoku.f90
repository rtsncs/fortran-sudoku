! Program szuka rozwiazania dla podanej na wejscie planszy sudoku.
! Plansze wprowadza sie od lewej do prawej, od gory do dolu, liczby musza byc
! od siebie oddzielone, 0 oznacza pusta komorke.

! Program rekurencyjnie wstawia kolejne liczby do kolejnych wolnych komorek
! do momentu znalezienia rozwiazania, lub w przypadku jego braku do sprawdzenia
! wszystkich kombinacji.

! Przykladowe plansze
! 3 0 0 9 6 7 0 0 1
! 0 4 0 3 0 2 0 8 0
! 0 2 0 0 0 0 0 7 0
! 0 7 0 0 0 0 0 9 0
! 0 0 0 8 7 3 0 0 0
! 5 0 0 0 1 0 0 0 3
! 0 0 4 7 0 5 1 0 0
! 9 0 5 0 0 0 2 0 7
! 8 0 0 6 2 1 0 0 4
!
! 0 5 0 6 0 0 1 0 0
! 0 7 0 0 0 0 5 0 8
! 0 0 0 3 0 0 0 0 0
! 3 0 0 4 0 0 0 6 0
! 0 0 0 0 0 1 0 0 0
! 0 0 0 0 0 0 0 2 0
! 6 0 0 0 0 0 0 4 0
! 0 0 0 0 5 0 7 0 0
! 2 0 0 0 0 0 0 0 0
!
! 0 0 0 7 0 4 0 0 5
! 0 2 0 0 1 0 0 7 0
! 0 0 0 0 8 0 0 0 2
! 0 9 0 0 0 6 2 5 0
! 6 0 0 0 7 0 0 0 8
! 0 5 3 2 0 0 0 1 0
! 4 0 0 0 9 0 0 0 0
! 0 3 0 0 6 0 0 9 0
! 2 0 0 4 0 7 0 0 0 

program sudoku
    implicit none
    integer, dimension(9, 9) :: board
    logical :: solved
    print *, 'Wprowadz plansze:'
    read (*,*) board

    print *, 'Szukam rozwiazania planszy:'
    print '("|", I0, "|", I0, "|", I0, "|", I0, "|", I0, "|", I0, "|", I0, "|", I0, "|", I0, "|")', board

    if (maxval(board) > 9 .or. minval(board) < 0) then
        print *, 'Nieprawidlowa plansza.'
    endif

    call solve(board, 0, 0, solved)

    if (solved .eqv. .true.) then 
        print *, 'Znaleziono rozwiazanie:'
        print '("|", I0, "|", I0, "|", I0, "|", I0, "|", I0, "|", I0, "|", I0, "|", I0, "|", I0, "|")', board
    else
        print *, 'Nie znaleziono rozwiazania.'
    endif

contains
    ! Funkcja sprawdzajaca czy plansza jest poprawna
    ! (brak powtarzajacych sie liczb w kazdej kolumnie, wierszu i podkwadracie 3x3)
    function check_valid(board) result (valid)
        implicit none
        integer :: board(9, 9)
        integer :: i, j, k
        logical :: valid

        valid = .true.
        ! Sprawdzanie kolumn i wierszy
        do i = 1, 9
            do k = 1, 9
                if (count(board(:,i) == k) > 1 .or. count(board(i,:) == k) > 1) then
                    valid = .false.
                    return
                endif
            end do
        end do
        ! Sprawdzanie podkwadratow
        do i = 1, 8, 3
            do j = 1, 8, 3
                do k = 1, 9
                    if (count(board(j:j+2,i:i+2) == k) > 1) then
                        valid = .false.
                        return
                    endif
                end do
            end do
        end do
    end function check_valid

    ! Procedura szukajaca rozwiazania
    recursive subroutine solve(board, i, j, solved)
        implicit none
        integer, intent(inout) :: board(9, 9)
        integer, intent(in) :: i, j
        logical, intent(inout) :: solved
        integer :: ni, nj, k

        solved = .false.
        ! Jesli algorytm doszedl do konca planszy to znaczy ze znaleziono rozwiazanie
        if (j == 10 .and. i == 9) then
            solved = .true.
            return
        endif

        ! nj i ni oznaczaja kolumne i wiersz do sprawdzenia w kolejnym wywolaniu procedury
        nj = j + 1
        ni = i
        if (j == 10) then
            nj = 1
            ni = i + 1
        endif

        ! Pomijamy komorki ktore juz maja jakas liczbe
        if (board(j, i) > 0) then
            call solve(board, ni, nj, solved)
        else
            k = 1
            ! Probujemy kolejne liczby az do znalezienia rozwiazania
            do while (k < 10 .and. (.not. solved))
                board(j, i) = k
                if (check_valid(board)) then
                    call solve(board, ni, nj, solved)
                endif
                ! Jesli nie znaleziono rozwiazania to usuwamy sprawdzana liczbe
                if (.not. solved) then
                    board(j, i) = 0
                    k = k + 1
                endif
            end do
        endif

    end subroutine solve
end program sudoku
