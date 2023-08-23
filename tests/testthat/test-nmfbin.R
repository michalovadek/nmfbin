
# data --------------------------------------------------------------------

# small matrix
mat_small <- matrix(c(0,0,0,1,1,1,0,0), nrow = 2)

# rect matrix
mat_rect <- matrix(sample(c(0, 1), 1000, replace=TRUE), ncol = 100)

# bigger matrix
mat_big <- matrix(sample(c(0,1), 100000, replace = TRUE), ncol = 10000)

# tests -------------------------------------------------------------------

## MUR, random init ##

# small k values
testthat::test_that(
  "small k, mur", {
    expect_gte(
      nmfbin(mat_small, k = 2, optimizer = "mur", init = "random")$final_loss,
      0L
    )
    expect_gte(
      nmfbin(mat_rect, k = 1, optimizer = "mur", init = "random")$final_loss,
      0L
    )
    expect_gte(
      nmfbin(mat_big, k = 3, optimizer = "mur", init = "random")$final_loss,
      0L
    )
  })

# medium k values
testthat::test_that(
  "slightly higher k, mur", {
    expect_error(
      nmfbin(mat_small, k = 10, optimizer = "mur", init = "random")$final_loss
    )
    expect_gte(
      nmfbin(mat_rect, k = 11, optimizer = "mur", init = "random")$final_loss,
      0L
    )
    expect_gte(
      nmfbin(mat_big, k = 12, optimizer = "mur", init = "random")$final_loss,
      0L
    )
  })

# high k values
testthat::test_that(
  "slightly higher k, mur", {
    expect_error(
      nmfbin(mat_small, k = 51, optimizer = "mur", init = "random")$final_loss
    )
    expect_gte(
      nmfbin(mat_rect, k = 52, optimizer = "mur", init = "random")$final_loss,
      0L
    )
    expect_gte(
      nmfbin(mat_big, k = 53, optimizer = "mur", init = "random")$final_loss,
      0L
    )
  })

