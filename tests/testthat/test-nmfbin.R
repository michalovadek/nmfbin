
# data --------------------------------------------------------------------

# small matrix
mat_small <- matrix(c(0,0,0,1,1,1,0,0), nrow = 2)

# rect matrix
mat_rect <- matrix(sample(c(0, 1), 1000, replace=TRUE), ncol = 100)

# bigger matrix
mat_big <- matrix(sample(c(0,1), 100000, replace = TRUE), ncol = 10000)

# tests -------------------------------------------------------------------

## KL divergence, random init

# small k values
testthat::test_that(
  "small k, KL divergence", {
    expect_gte(
      nmfbin(mat_small, k = 2, init_method = "random")$final_divergence,
      0L
    )
    expect_gte(
      nmfbin(mat_rect, k = 1, init_method = "random")$final_divergence,
      0L
    )
    expect_gte(
      nmfbin(mat_big, k = 3, init_method = "random")$final_divergence,
      0L
    )
})

# medium k values
testthat::test_that(
  "slightly higher k, KL divergence", {
    expect_error(
      nmfbin(mat_small, k = 10, init_method = "random")$final_divergence
    )
    expect_gte(
      nmfbin(mat_rect, k = 11, init_method = "random")$final_divergence,
      0L
    )
    expect_gte(
      nmfbin(mat_big, k = 12, init_method = "random")$final_divergence,
      0L
    )
})

# high k values
testthat::test_that(
  "slightly higher k, KL divergence", {
    expect_error(
      nmfbin(mat_small, k = 51, init_method = "random")$final_divergence
    )
    expect_gte(
      nmfbin(mat_rect, k = 52, init_method = "random")$final_divergence,
      0L
    )
    expect_gte(
      nmfbin(mat_big, k = 53, init_method = "random")$final_divergence,
      0L
    )
})


## cross entropy, random init

# small k values
testthat::test_that(
  "small k, crossentropy", {
    expect_gte(
      nmfbin(mat_small, k = 2, divergence_type = "crossentropy", init_method = "random")$final_divergence,
      0L
    )
    expect_gte(
      nmfbin(mat_rect, k = 1, divergence_type = "crossentropy", init_method = "random")$final_divergence,
      0L
    )
    expect_gte(
      nmfbin(mat_big, k = 3, divergence_type = "crossentropy", init_method = "random")$final_divergence,
      0L
    )
  })

# medium k values
testthat::test_that(
  "slightly higher k, crossentropy", {
    expect_error(
      nmfbin(mat_small, k = 10, divergence_type = "crossentropy", init_method = "random")$final_divergence
    )
    expect_gte(
      nmfbin(mat_rect, k = 11, divergence_type = "crossentropy", init_method = "random")$final_divergence,
      0L
    )
    expect_gte(
      nmfbin(mat_big, k = 12, divergence_type = "crossentropy", init_method = "random")$final_divergence,
      0L
    )
  })

# high k values
testthat::test_that(
  "slightly higher k, crossentropy", {
    expect_error(
      nmfbin(mat_small, k = 51, divergence_type = "crossentropy", init_method = "random")$final_divergence
    )
    expect_gte(
      nmfbin(mat_rect, k = 52, divergence_type = "crossentropy", init_method = "random")$final_divergence,
      0L
    )
    expect_gte(
      nmfbin(mat_big, k = 53, divergence_type = "crossentropy", init_method = "random")$final_divergence,
      0L
    )
  })

