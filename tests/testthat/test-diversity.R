test_that("index_brillouin works", {
  n = 8L
  v = seq_len(n)
  m = matrix(v, nrow = n, ncol = 1L)
  dim(m) = c(x = n, y = 1L)
  ras = stars::st_as_stars(m)
  d = index_brillouin(ras) |>
    expect_type("double") |>
    expect_length(1) |>
    expect_gte(0)

  expect_identical(index_brillouin(ras + 42L), d)
})

test_that("summarize_counts works", {
  n = 8L
  v = seq_len(n)
  expect_identical(summarize_counts(v), summarize_counts(c(0L, v, 0L)))
})
