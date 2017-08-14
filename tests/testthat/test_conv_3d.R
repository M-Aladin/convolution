library(testthat)


test_that("3x3x2 by 4x4x2 arrays give expected result", {
    A <- array(1:18, dim = c(3,3,2))
    B <- array(1:32, dim = c(4,4,2))

    result <- conv3d(A, B)
    # load(file = "padreplicate_3x32x32.rda")
    expected <- array(simplify2array(read.table(header = FALSE, sep = ",",
                                          file = "conv3d-332x442.txt")), dim=c(6,6,3))
    # print(sample)
    expect_equal(result, expected)
})


test_that("2x4x2 by 4x8x2 arrays give expected result", {
    C <- array(1:16, dim = c(2, 4, 2))
    D <- array(1:64, dim = c(4, 8, 2))

    result <- conv3d(C, D)
    # print(result)
    expected <- array(simplify2array(read.table(header = FALSE,
                                                sep = ",",
                                                file = "conv3d-2x4x2-4x8x2.txt")),
                                                dim=c(5,11,3))
    # print(sample)
    expect_equal(result, expected)
})


test_that("11x5x2 by 7x5x3 arrays give expected result", {
    E = array(1:110, dim = c(11, 5, 2))
    F = array(1:105, dim = c(7, 5, 3))

    result <- conv3d(E, F)
    # print(result)
    expected <- array(simplify2array(read.table(header = FALSE,
                                                sep = ",",
                                                file = "conv3d-17x9x4.txt")),
                                                dim=c(17, 9, 4))
    # print(expected)
    expect_equal(result, expected, tolerance = 0.0001)
})

test_that("11x5x2 by 7x5x3 arrays give expected result", {
    D <- array(1:64, dim = c(4, 8, 2))
    F = array(1:105, dim = c(7, 5, 3))

    result <- conv3d(D, F)
    # print(result)
    expected <- array(simplify2array(read.table(header = FALSE,
                                                sep = ",",
                                                file = "conv3d-10x12x4.txt")),
                                                dim=c(10, 12, 4))
    # print(expected)
    expect_equal(result, expected, tolerance = 0.0001)
})



test_that("11x5x2 by 7x5x3 arrays give expected result", {
    G = array(1:393216, dim = c(512, 256, 3))
    H = array(1:40960, dim = c(64, 128, 5))

    result <- conv3d(G, H)
    # print(result)
    expected <- array(simplify2array(read.table(header = FALSE,
                                                sep = ",",
                                                file = "conv3d-575x383x7.txt")),
                                                dim=c(575, 383, 7))
    # print(expected)
    expect_equal(result, expected, tolerance = 0.0001)
})
