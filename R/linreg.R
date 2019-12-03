get_lm_components <- function(lm_obj) {
  return(coef(lm_obj)[2])
}

linreg_sim <- function(nsims, nsamp, alpha_true, beta_true,
                       err_func = rnorm) {
  x <- rnorm(nsamp)
  beta <- c()
  for (i in 1:nsims) {
    errors <- err_func(nsamp)
    y <- alpha_true + beta_true * x + errors
    mod <- lm(y ~ x)
    beta_cur <- get_lm_components(mod)
    beta <- c(beta, beta_cur)
  }
  return(beta)
}
