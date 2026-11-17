#' Pick colours from a Solarized watercolour palette
#'
#' @param palette One of `names(wc_palettes)`: `"wc_sap_prussian"`,
#'   `"wc_ochre_indigo"`, `"wc_rose_plum"` or `"wc_sienna_prussian"` (diverging).
#'   The `wc_` prefix may be omitted.
#' @param n Number of colours. `NULL` returns the full 64-stop ramp.
#' @param reverse Reverse the palette direction.
#' @param paper For sequential palettes, include the bare-paper colour as the
#'   first class? Default `FALSE`, so the lightest class stays visible against a
#'   paper background.
#' @return A character vector of hex colours.
#' @examples
#' wc_colors("sap_prussian", 7)
#' wc_colors("sienna_prussian", 5)
#' @export
wc_colors <- function(palette = "wc_sap_prussian", n = NULL, reverse = FALSE,
                      paper = FALSE) {
  palette <- wc_match_palette(palette)
  pal <- wc_palettes[[palette]]
  if (is.null(n)) return(if (reverse) rev(pal) else pal)
  stopifnot(n >= 1)
  if (wc_is_diverging(palette)) {
    x <- (seq_len(n) - 0.5) / n          # odd n -> paper in the middle
  } else if (paper) {
    x <- if (n == 1) 1 else seq(0, 1, length.out = n)
  } else {
    x <- seq(1 / (n + 1), 1, length.out = n)
  }
  ramp <- grDevices::colorRamp(pal, space = "Lab", interpolate = "linear")
  m <- ramp(pmin(pmax(x, 0), 1))
  out <- grDevices::rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255)
  if (reverse) rev(out) else out
}

#' @rdname wc_colors
#' @export
wc_colours <- wc_colors

#' Palette generator for discrete scales
#'
#' @inheritParams wc_colors
#' @return A function of `n` returning `n` hex colours.
#' @export
wc_pal <- function(palette = "wc_sap_prussian", reverse = FALSE, paper = FALSE) {
  force(palette); force(reverse); force(paper)
  function(n) wc_colors(palette, n, reverse = reverse, paper = paper)
}

wc_match_palette <- function(palette) {
  if (!grepl("^wc_", palette)) palette <- paste0("wc_", palette)
  if (!palette %in% names(wc_palettes)) {
    stop("Unknown palette '", palette, "'. Choose one of: ",
         paste(names(wc_palettes), collapse = ", "), call. = FALSE)
  }
  palette
}

wc_is_diverging <- function(palette) wc_match_palette(palette) == "wc_sienna_prussian"

# continuous ramp without the bare-paper end (sequential only), used by
# binned scales so the lowest bin is a visible wash
wc_ramp_trimmed <- function(palette, reverse, paper = FALSE) {
  pal <- wc_palettes[[wc_match_palette(palette)]]
  if (!wc_is_diverging(palette) && !paper) pal <- pal[-(1:5)]
  if (reverse) rev(pal) else pal
}

#' Solarized reference colours
#'
#' `wc_paper` is Solarized base3, the "paper" every palette starts from;
#' `wc_ink` is base01, used for text, outlines and contour lines.
#' @export
wc_paper <- "#fdf6e3"

#' @rdname wc_paper
#' @export
wc_ink <- "#586e75"
