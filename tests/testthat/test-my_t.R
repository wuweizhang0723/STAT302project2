test_that("my_t.test returns a list", {
  expect_is(my_t.test(c(1,2,5,7), "two.sided", 0), "list")
  expect_is(my_t.test(c(0,1,2,5,7,-2), "greater", 2), "list")
  expect_is(my_t.test(c(0,1,4,5,7,-2), "less", 2), "list")
})

test_that("alternative input should be a valid string", {
  expect_error(my_t.test(c(1,2,5,7), "a string", 0))
})
