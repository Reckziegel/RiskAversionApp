optimum_portfolio <- function(sigma, mu, .wmin = 0, .wmax = 1, .lambda) {

  assertthat::assert_that(is.numeric(.wmin))
  assertthat::assert_that(is.numeric(.wmax))

  num_assets <- ncol(sigma)

  Aeq  <- matrix(1, 1, num_assets)
  beq  <- 1

  A <- rbind(-diag(num_assets), diag(num_assets))
  b <- c(-if (length(.wmax) == 1L) rep(.wmax, num_assets) else .wmax, if (length(.wmin) == 1L) rep(.wmin, num_assets) else .wmin)

  Amat <- rbind(Aeq, A)
  bvec <- c(beq, b)

  # MinVol_Weights
  weights <- matrix(quadprog::solve.QP(Dmat = .lambda * 2 * sigma, dvec = -mu, Amat = t(Amat), bvec = bvec, meq = length(beq))$solution)
  return  <- t(weights) %*% mu
  volatility <- sqrt(t(weights) %*% sigma %*% weights)

  list(weights = weights, Retorno = return, Volatilidade = volatility)

}
