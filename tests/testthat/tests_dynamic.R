
context("dynamic_program")

set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(bfk <- dynamic_program(x = knapsack_objects[1:8,], w = 3500))
  expect_named(bfk, c("value", "elements"))
})


test_that("Function rejects errounous input.", {
  expect_error(dynamic_program("hej", 3500))
  expect_error(dynamic_program(x = knapsack_objects[1:8,], w = -3500))
})

test_that("Function return correct results.", {
  bfk <- dynamic_program(x = knapsack_objects[1:8,], w = 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))

  bfk <- dynamic_program(x = knapsack_objects[1:12,], w = 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))

  bfk <- dynamic_program(x = knapsack_objects[1:8,], w = 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))

  bfk <- dynamic_program(x = knapsack_objects[1:12,], w = 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))

})
