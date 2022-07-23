efficient_frontier <- function(num_portf, sigma, mu, .wmin = 0, .wmax = 1) {

  assertthat::assert_that(is.numeric(.wmin))
  assertthat::assert_that(is.numeric(.wmax))

  num_assets <- ncol(sigma)

  # determine return of minimum-risk portfolio
  first_degree  <- matrix(0, num_assets, 1)
  second_degree <- sigma

  Aeq  <- matrix(1, 1, num_assets)
  beq  <- 1

  A <- rbind(-diag(num_assets), diag(num_assets))
  b <- c(-if (length(.wmax) == 1L) rep(.wmax, num_assets) else .wmax, if (length(.wmin) == 1L) rep(.wmin, num_assets) else .wmin)

  Amat <- rbind(Aeq, A)
  bvec <- c(beq, b)

  # MinVol_Weights
  minvol_weights <- matrix(quadprog::solve.QP(Dmat = 2 * second_degree, dvec = -first_degree, Amat = t(Amat), bvec = bvec, meq = length(beq))$solution)
  minvol_return  <- t(minvol_weights) %*% mu

  # Determine return of maximum-return portfolio
  maxret_return <- max(mu)
  maxret_index  <- which(mu == max(mu))
  # Slice efficient frontier in NumPortf equally thick horizontal sectors in the upper branch only
  step          <- (maxret_return - minvol_return) / (num_portf - 1)
  target_returns <- seq(c(minvol_return), maxret_return, c(step))

  # Compute the NumPortf compositions and risk-return coordinates of the optimal allocations relative to each slice initialization
  composition   <- matrix(NA_real_, num_portf, num_assets)
  volatility    <- matrix(NA_real_, num_portf, 1)
  expectedvalue <- matrix(NA_real_, num_portf, 1)

  # start with min vol portfolio
  composition[1, ] <- t(minvol_weights)
  volatility[1]    <- sqrt(t(minvol_weights) %*% sigma %*% minvol_weights)
  expectedvalue[1] <- t(minvol_weights) %*% mu

  for (i in 2:(num_portf - 1)) {
    # determine least risky portfolio for given expected return
    AEq <- rbind(matrix(1, 1, num_assets), t(mu))
    bEq <- rbind(1, target_returns[i])
    Amat <- rbind(AEq, A)
    bvec <- c(bEq, b)

    weights <- t(quadprog::solve.QP(Dmat = 2 * second_degree, dvec = -first_degree, Amat = t(Amat), bvec = bvec, meq = length(bEq))$solution)

    composition[i, ] <- weights
    volatility[i]    <- sqrt(weights %*% sigma %*% t(weights))
    expectedvalue[i] <- weights %*% mu

  }

  # add max ret portfolio
  weights                          <- matrix(0, 1, num_assets)
  weights[maxret_index]            <- 1
  composition[nrow(composition), ] <- weights
  volatility[length(volatility)]   <- sqrt(weights %*% sigma %*% t(weights))
  expectedvalue[length(expectedvalue)] <- weights %*% mu

  if (!is.null(colnames(sigma))) {
    colnames(composition) <- colnames(sigma)
  } else {
    colnames(composition) <- make_tidy_names(sigma)
  }

  out <- list(Retorno      = tibble::tibble(Retorno = as.double(expectedvalue)),
              Volatilidade = tibble::tibble(Volatilidade = as.double(volatility)),
              weights = tibble::as_tibble(composition)
  )

  vctrs::new_list_of(x = out, ptype = double(), class = "efficient_frontier")

}

#' @importFrom vctrs obj_print_header
#' @export
obj_print_header.efficient_frontier <- function(x, ...) {
  cat(crayon::cyan("# Efficient Frontier"))
}

#' @importFrom vctrs obj_print_data
#' @export
obj_print_data.efficient_frontier <- function(x, ...) {
  cat("\n")
  cat("Retorno      : <<", crayon::silver("tbl"), NROW(x$Retorno), "x", NCOL(x$Retorno),">>")
  cat("\n")
  cat("Volatilidade : <<", crayon::silver("tbl"), NROW(x$Volatilidade), "x", NCOL(x$Volatilidade),">>")
  cat("\n")
  cat("weights      : <<", crayon::silver("tbl"), NROW(x$weights), "x", NCOL(x$weights),">>")
  cat("\n")
}
