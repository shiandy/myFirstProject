get_lm_components <- function(lm_obj) {
  return(coef(lm_obj)[2])
}

#' Run linear regression simulation.
#'
#' Add documentation here.
#'
#' @param nsims
#' @param nsamp
#' @param alpha_true
#' @param beta_true
#' @param err_func
#'
#' @return
#' @export
#'
#' @examples
linreg_sim <- function(nsims, nsamp, alpha_true, beta_true,
                       err_func = rnorm) {
  x <- rnorm(nsamp)
  beta <- c()
  for (i in 1:nsims) {
    errors <- err_func(nsamp)
    y <- alpha_true + beta_true * x + errors
    mod <- lm(y ~ x)
    beta_cur <- get_lm_components(mod)
    beta[i] <- beta_cur
  }
  return(beta)
}

linreg_sim_new <- function(nsims, nsamp, alpha_true, beta_true,
                           err_func = rnorm) {
  x <- rnorm(nsamp)
  errors <- matrix(err_func(nsims * nsamp), nrow = nsamp, ncol = nsims)
  y <- alpha_true + beta_true * x + errors
  mod <- lm(y ~ x)
  beta <- coef(mod)[2, ]
  return(beta)
}
