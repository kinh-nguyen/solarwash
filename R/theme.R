#' Paper-and-ink theme
#'
#' Solarized base3 paper background, base01 ink for text, and faint base2
#' grid lines. Pair it with the watercolour scales or [geom_watercolor()].
#'
#' @param base_size Base font size.
#' @param base_family Base font family. A serif such as `"serif"` or
#'   `"EB Garamond"` suits the hand-painted look.
#' @param grid Draw grid lines? Maps usually look better without them.
#' @export
theme_wc <- function(base_size = 11, base_family = "", grid = FALSE) {
  base2 <- "#eee8d5"; base1 <- "#93a1a1"; base02 <- "#073642"
  t <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = wc_paper, colour = NA),
      panel.background = ggplot2::element_rect(fill = wc_paper, colour = NA),
      legend.background = ggplot2::element_rect(fill = wc_paper, colour = NA),
      legend.key = ggplot2::element_rect(fill = wc_paper, colour = NA),
      text = ggplot2::element_text(colour = wc_ink),
      plot.title = ggplot2::element_text(colour = base02, size = ggplot2::rel(1.35)),
      plot.subtitle = ggplot2::element_text(colour = base1),
      plot.caption = ggplot2::element_text(colour = base1),
      axis.text = ggplot2::element_text(colour = base1),
      strip.text = ggplot2::element_text(colour = wc_ink, hjust = 0),
      panel.grid = ggplot2::element_line(colour = base2, linewidth = 0.3),
      plot.title.position = "plot"
    )
  if (!grid) t <- t + ggplot2::theme(panel.grid = ggplot2::element_blank())
  t
}
