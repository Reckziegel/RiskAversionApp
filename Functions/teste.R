plot_teste <- function(.efficient_frontier, .opt) {

  ef1 <- tibble::tibble(.efficient_frontier$sigma, .efficient_frontier$mu) |>
    dplyr::mutate(ra = 1)
  ef2 <- tibble::tibble(rep(.opt$sigma, NROW(.efficient_frontier$sigma)), .) |>
    dplyr::mutate(ra = 2)

  frontier <- dplyr::bind_rows(ef1, ef2) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$sigma, y = .data$mu, color = as.factor(ra))) +
    ggplot2::geom_line() +
    #ggplot2::geom_point(ggplot2::aes(x = .opt$volatility, y = .opt$return), color = "red", size = 2) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_x_continuous(labels = scales::percent_format()) +
    ggplot2::labs(x = NULL)

  frontier


}
