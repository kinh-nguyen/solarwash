#' Solarized watercolour scales for ggplot2
#'
#' * `_c`: continuous gradient.
#' * `_b`: binned, i.e. classed washes. This is usually the best fit for the
#'   watercolour look.
#' * `_d`: discrete or ordinal factors.
#'
#' @inheritParams wc_colors
#' @param midpoint For diverging use (`"sienna_prussian"`), the data value that
#'   should sit on bare paper, e.g. `0` for anomalies. `NULL` maps the
#'   midpoint of the data range to paper.
#' @param paper Sequential palettes only: start the gradient at bare paper?
#'   The default `FALSE` starts at a light wash so the lowest values stay
#'   visible on a paper background. `TRUE` suits filled rasters where low values
#'   should melt into the paper.
#' @param ... Passed on to the underlying ggplot2 scale (`name`, `limits`,
#'   `breaks`, `n.breaks`, `labels`, `guide`, ...).
#' @examples
#' library(ggplot2)
#' v <- data.frame(x = rep(seq_len(nrow(volcano)), ncol(volcano)),
#'                 y = rep(seq_len(ncol(volcano)), each = nrow(volcano)),
#'                 z = as.vector(volcano))
#' ggplot(v, aes(x, y, fill = z)) + geom_raster() +
#'   scale_fill_wc_b("sap_prussian", n.breaks = 7) + theme_wc()
#' @name scale_wc
NULL

wc_rescaler <- function(midpoint) {
  if (is.null(midpoint)) return(scales::rescale)
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to = to, from = from, mid = midpoint)
  }
}

#' @rdname scale_wc
#' @export
scale_fill_wc_c <- function(palette = "wc_sap_prussian", reverse = FALSE,
                            midpoint = NULL, paper = FALSE, ...) {
  ggplot2::scale_fill_gradientn(colours = wc_ramp_trimmed(palette, reverse, paper),
                                rescaler = wc_rescaler(midpoint),
                                na.value = "transparent", ...)
}

#' @rdname scale_wc
#' @export
scale_colour_wc_c <- function(palette = "wc_sap_prussian", reverse = FALSE,
                              midpoint = NULL, paper = FALSE, ...) {
  ggplot2::scale_colour_gradientn(colours = wc_ramp_trimmed(palette, reverse, paper),
                                  rescaler = wc_rescaler(midpoint),
                                  na.value = "transparent", ...)
}

#' @rdname scale_wc
#' @export
scale_fill_wc_b <- function(palette = "wc_sap_prussian", reverse = FALSE,
                            midpoint = NULL, paper = FALSE, ...) {
  ggplot2::scale_fill_stepsn(colours = wc_ramp_trimmed(palette, reverse, paper),
                             rescaler = wc_rescaler(midpoint),
                             na.value = "transparent", ...)
}

#' @rdname scale_wc
#' @export
scale_colour_wc_b <- function(palette = "wc_sap_prussian", reverse = FALSE,
                              midpoint = NULL, paper = FALSE, ...) {
  ggplot2::scale_colour_stepsn(colours = wc_ramp_trimmed(palette, reverse, paper),
                               rescaler = wc_rescaler(midpoint),
                               na.value = "transparent", ...)
}

wc_discrete_scale <- function(aesthetics, palette, reverse, ...) {
  args <- list(aesthetics = aesthetics, palette = wc_pal(palette, reverse), ...)
  if (utils::packageVersion("ggplot2") < "3.5.0") args$scale_name <- "wc_d"
  do.call(ggplot2::discrete_scale, args)
}

#' @rdname scale_wc
#' @export
scale_fill_wc_d <- function(palette = "wc_sap_prussian", reverse = FALSE, ...) {
  wc_discrete_scale("fill", palette, reverse, ...)
}

#' @rdname scale_wc
#' @export
scale_colour_wc_d <- function(palette = "wc_sap_prussian", reverse = FALSE, ...) {
  wc_discrete_scale("colour", palette, reverse, ...)
}

#' @rdname scale_wc
#' @export
scale_color_wc_c <- scale_colour_wc_c
#' @rdname scale_wc
#' @export
scale_color_wc_b <- scale_colour_wc_b
#' @rdname scale_wc
#' @export
scale_color_wc_d <- scale_colour_wc_d
