program sudoku

    implicit none
    integer, dimension(9, 9) :: board
    logical :: solved
    !board = reshape([&
    !        3, 0, 0, 9, 6, 7, 0, 0, 1, &
    !        0, 4, 0, 3, 0, 2, 0, 8, 0, &
    !        0, 2, 0, 0, 0, 0, 0, 7, 0, &
    !        0, 7, 0, 0, 0, 0, 0, 9, 0, &
    !        0, 0, 0, 8, 7, 3, 0, 0, 0, &
    !        5, 0, 0, 0, 1, 0, 0, 0, 3, &
    !        0, 0, 4, 7, 0, 5, 1, 0, 0, &
    !        9, 0, 5, 0, 0, 0, 2, 0, 7, &
    !        8, 0, 0, 6, 2, 1, 0, 0, 4  &
    !    ], shape(board), order=[1, 2])
    board = reshape([&
            0, 0, 0, 7, 0, 4, 0, 0, 5, &
            0, 2, 0, 0, 1, 0, 0, 7, 0, &
            0, 0, 0, 0, 8, 0, 0, 0, 2, &
            0, 9, 0, 0, 0, 6, 2, 5, 0, &
            6, 0, 0, 0, 7, 0, 0, 0, 8, &
            0, 5, 3, 2, 0, 0, 0, 1, 0, &
            4, 0, 0, 0, 9, 0, 0, 0, 0, &
            0, 3, 0, 0, 6, 0, 0, 9, 0, &
            2, 0, 0, 4, 0, 7, 0, 0, 0  &
        ], shape(board), order=[1, 2])
    call solve(board, 0, 0, solved)
    !print *, solved
    print "(I0, 1x, I0, 1x, I0, 1x, I0, 1x, I0, 1x, I0, 1x, I0, 1x, I0, 1x, I0)", board


contains
    function check_valid(board) result (valid)
        integer :: board(9, 9)
        integer :: i, j, k
        logical :: valid

        valid = .true.
        do i = 1, 9
            do k = 1, 9
                if (count(board(:,i) == k) > 1) then
                    valid = .false.
                endif
            end do
        end do
        do j = 1, 9
            do k = 1, 9
                if (count(board(j,:) == k) > 1) then
                    valid = .false.
                endif
            end do
        end do
        do i = 1, 8, 3
            do j = 1, 8, 3
                do k = 1, 9
                    if (count(board(j:j+2,i:i+2) == k) > 1) then
                        valid = .false.
                    endif
                end do
            end do
        end do
    end function check_valid

    recursive subroutine solve(board, i, j, solved)
        integer, intent(inout) :: board(9, 9)
        integer, intent(in) :: i, j
        logical, intent(inout) :: solved
        integer :: ni, nj, k
        k = 1

        solved = .false.
        if (j == 10 .and. i == 9) then
            solved = .true.
        else
            nj = j + 1
            ni = i
            if (j == 10) then
                nj = 1
                ni = i + 1
            endif

            if (board(j, i) > 0) then
                call solve(board, ni, nj, solved)
            else
                do while (k < 10 .and. (.not. solved))
                    board(j, i) = k
                    if (check_valid(board)) then
                        call solve(board, ni, nj, solved)
                    endif
                    if (.not. solved) then
                        board(j, i) = 0
                        k = k + 1
                    endif
                end do
            endif
        endif

    end subroutine solve
end program sudoku

