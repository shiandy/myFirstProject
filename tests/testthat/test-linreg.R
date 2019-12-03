test_that("beta on average is correct", {
  alpha_true <- 1
  beta_true <- 0.5
  beta_sim <- linreg_sim(1000, 100, alpha_true, beta_true)
  expect_equal(mean(beta_sim), beta_true, tolerance = 0.01)
})

test_that("get_lm_components is returning correctly", {
  x <- rnorm(100)
  y <- 0.2 + 0.1 * x + rnorm(100)
  mod <- lm(y ~ x)
  beta <- get_lm_components(mod)
  expect_identical(names(beta), "x")
})
