plot_efficient_frontier <- function(.efficient_frontier, .opt) {

  point_mu <- tibble::tibble(.efficient_frontier$Volatilidade * sqrt(252), ((1 + .efficient_frontier$Retorno) ^ 252) - 1) |>
    dplyr::mutate(.sd = Volatilidade < c(.opt$Volatilidade) * sqrt(252)) |>
    dplyr::filter(.sd == FALSE) |>
    dplyr::slice(1) |>
    dplyr::pull(Retorno)
  ra <- tibble::tibble(Retorno = point_mu, Volatilidade = .opt$Volatilidade * sqrt(252))


  x <- diff(log(EuStockMarkets))
  assets_mu <- colMeans(x)
  assets_sd <- apply(x, 2, sd)
  assets <- tibble::tibble(
    Retorno = (1 + assets_mu) ^ 252 - 1,
    Volatilidade = assets_sd * sqrt(252),
    Ativo = colnames(x))

  #.colors <- viridisLite::cividis(4)
  .colvid <- viridisLite::viridis(1)

  frontier <- tibble::tibble(.efficient_frontier$Volatilidade * sqrt(252), ((1 + .efficient_frontier$Retorno) ^ 252) - 1)
  frontier |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Volatilidade, y = .data$Retorno)) +
    ggplot2::geom_line(show.legend = FALSE, size = 2, color = "grey") +
    ggplot2::geom_point(ggplot2::aes(x = Volatilidade, y = Retorno), data = ra, size = 5, color = .colvid) +
    ggplot2::geom_point(ggplot2::aes(x = Volatilidade, y = Retorno, color = Ativo), data = assets, size = 5, show.legend = FALSE) +
    ggplot2::scale_color_viridis_d(option = "cividis") +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_x_continuous(labels = scales::percent_format()) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank()) +
    ggplot2::labs(x = "Retorno Anualizado", y = "Volatilidade Anualizada")

}

