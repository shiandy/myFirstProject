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

test_that("fitting multiple lm's is same as fitting one big lm", {
  x <- rnorm(100)
  y1 <- 0.2 + 0.1 * x + rnorm(100)
  y2 <- 0.2 + 0.1 * x + rnorm(100)
  mod1 <- lm(y1 ~ x)
  mod2 <- lm(y2 ~ x)
  y_big <- cbind(y1, y2)
  bigmod <- lm(y_big ~ x)

  beta1 <- coef(mod1)[2]
  beta2 <- coef(mod2)[2]
  beta_all <- coef(bigmod)[2, ]
  expect_equal(beta_all, c(beta1, beta2))
})
