context("Grid layout")

rast <- as.raster(matrix(runif(100), ncol = 10))
grobs <- list(grid::rectGrob(), grid::rectGrob(), grid::rectGrob())

test_that("labels are included when present", {
  out <- build_image(rast, fixed = FALSE, label = "HI", fontsize = 12)
  expect_is(out$grobs[[1]], "text")
})

test_that("labels are skipped when absent", {
  out <- build_image(rast, fixed = FALSE)
  expect_is(out$grobs[[1]], "null")
})

test_that("empty cells are filled with nullGrobs", {
  out <- build_image_table(grobs, nrow = 2, ncol = 2)
  expect_is(out$grobs[[4]], "null")
})

test_that("Empty columns are removed from layout", {
  out <- build_image_table(grobs, ncol = 4)
  expect_equal(ncol(out), 3)
})

test_that("Empty rows are removed from layout", {
  out <- build_image_table(grobs, nrow = 4)
  expect_equal(nrow(out), 3)
})
