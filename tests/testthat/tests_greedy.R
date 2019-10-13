context("greedy_knapsack")

set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(gk <- greedy_knapsack(x = knapsack_objects[1:8,], w = 3500))
  expect_named(gk, c("value", "elements"))
})

test_that("functions rejects errounous input.", {
  expect_error(greedy_knapsack("hej", 3500))
  expect_error(greedy_knapsack(x = knapsack_objects[1:8,], w = -3500))
})

test_that("Function return correct results.", {
  gk <- greedy_knapsack(x = knapsack_objects[1:8,], w = 3500)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))

  gk <- greedy_knapsack(x = knapsack_objects[1:12,], w = 3500)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))

  gk <- greedy_knapsack(x = knapsack_objects[1:8,], w = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))

  gk <- greedy_knapsack(x = knapsack_objects[1:12,], w = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))

  st <- system.time(gk <- greedy_knapsack(x = knapsack_objects[1:16,], w = 2000))
  expect_true(as.numeric(st)[2] <= 0.01)

  gk <- greedy_knapsack(x = knapsack_objects[1:800,], w = 3500)
  expect_equal(round(gk$value), 192647)

  gk <- greedy_knapsack(x = knapsack_objects[1:1200,], w = 3500)
  expect_equal(round(gk$value), 270290)
})
