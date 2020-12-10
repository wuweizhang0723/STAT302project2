test_that("my_lm returns a table", {
  expect_is(my_lm(mpg ~ hp*wt, mtcars), "table")
})
