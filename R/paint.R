# ---- watercolour rendering ---------------------------------------------------
# Physically motivated: every wash is converted to an absorbance relative to the
# paper (Beer-Lambert), the absorbance is modulated (pooled rims, granulation,
# blooms), then re-applied to the paper. Mixing therefore stays subtractive.

wc_s2l <- function(x) ifelse(x <= 0.04045, x / 12.92, ((x + 0.055) / 1.055)^2.4)
wc_l2s <- function(x) {
  x <- pmin(pmax(x, 0), 1)
  ifelse(x <= 0.0031308, 12.92 * x, 1.055 * x^(1 / 2.4) - 0.055)
}

# separable gaussian blur of a matrix with reflected edges
wc_blur <- function(m, sigma) {
  if (sigma < 0.3) return(m)
  r <- max(1L, ceiling(3 * sigma))
  k <- stats::dnorm(-r:r, sd = sigma); k <- k / sum(k)
  blur_cols <- function(a) {               # blur along rows index (down each column)
    n <- nrow(a); r2 <- min(r, n - 1L)
    idx <- c((r2 + 1):2, seq_len(n), (n - 1):(n - r2))
    idx <- idx[idx >= 1 & idx <= n]
    pad <- a[idx, , drop = FALSE]
    lead <- r2
    f <- stats::filter(pad, k[(r - r2 + 1):(r + r2 + 1)] / sum(k[(r - r2 + 1):(r + r2 + 1)]),
                       sides = 2)
    f <- as.matrix(f)[lead + seq_len(n), , drop = FALSE]
    f[is.na(f)] <- a[is.na(f)]
    f
  }
  t(blur_cols(t(blur_cols(m))))
}

wc_noise <- function(nr, nc, sigma) {
  if (sigma > 6) {                         # big blobs: blur a coarse grid, upsample
    f <- sigma / 3
    small <- wc_noise(max(2L, ceiling(nr / f)), max(2L, ceiling(nc / f)), 3)
    out <- wc_resize(small, nr, nc)
  } else {
    out <- wc_blur(matrix(stats::rnorm(nr * nc), nr, nc), sigma)
  }
  (out - mean(out)) / (stats::sd(as.vector(out)) + 1e-9)
}

# bilinear resize (NA-propagating)
wc_resize <- function(m, nr, nc) {
  r <- seq(1, nrow(m), length.out = nr); c <- seq(1, ncol(m), length.out = nc)
  r0 <- pmin(floor(r), nrow(m) - 1L); c0 <- pmin(floor(c), ncol(m) - 1L)
  if (nrow(m) == 1L) { r0 <- rep(1L, nr) }
  if (ncol(m) == 1L) { c0 <- rep(1L, nc) }
  fr <- r - r0; fc <- c - c0
  r1 <- pmin(r0 + 1L, nrow(m)); c1 <- pmin(c0 + 1L, ncol(m))
  m[r0, c0] * outer(1 - fr, 1 - fc) + m[r1, c0] * outer(fr, 1 - fc) +
    m[r0, c1] * outer(1 - fr, fc) + m[r1, c1] * outer(fr, fc)
}

wc_with_seed <- function(seed, code) {
  if (is.null(seed)) return(code)
  if (exists(".Random.seed", envir = globalenv())) {
    old <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old, envir = globalenv()))
  } else {
    on.exit(rm(".Random.seed", envir = globalenv()))
  }
  set.seed(seed)
  code
}

#' Paint a matrix as watercolour washes
#'
#' The engine behind [geom_watercolor()]. Use it directly for base graphics,
#' e.g. `plot(as.raster(wc_paint_matrix(volcano)$col))`.
#'
#' @param z Numeric matrix. Row 1 is the top of the image; `NA` cells are bare paper.
#' @inheritParams geom_watercolor
#' @return A list with `col` (a matrix of hex colours) and `band` (an integer
#'   matrix of class indices), plus `breaks`.
#' @export
wc_paint_matrix <- function(z, palette = "wc_sap_prussian", levels = 7,
                            reverse = FALSE, limits = NULL, midpoint = NULL,
                            seed = 1, wobble = 0.035, granulation = 0.18,
                            edge_pooling = 0.45, bloom = 0.12, paper_grain = 0.025) {
  wc_with_seed(seed, {
    nr <- nrow(z); nc <- ncol(z)
    s <- max(nr, nc) / 400                       # effect sizes scale with image size
    lim <- if (is.null(limits)) range(z, na.rm = TRUE) else limits
    if (!is.null(midpoint)) {
      h <- max(abs(lim - midpoint)); lim <- midpoint + c(-h, h)
    }
    breaks <- seq(lim[1], lim[2], length.out = levels + 1)
    zw <- z + wobble * diff(lim) * wc_noise(nr, nc, 6 * s)
    band <- matrix(findInterval(zw, breaks[-c(1, levels + 1)]) + 1L, nr, nc)
    band[is.na(z)] <- NA

    cols <- wc_colors(palette, levels, reverse = reverse)
    paper <- wc_s2l(grDevices::col2rgb(wc_paper)[, 1] / 255)
    wash <- wc_s2l(t(grDevices::col2rgb(cols)) / 255)            # levels x 3
    A_cls <- -log(pmax(sweep(wash, 2, paper, "/"), 1e-4))
    A_cls <- pmax(A_cls, 0)

    # pooled rims: inside each band, 1 at its edge falling to 0 in the interior
    rim <- matrix(0, nr, nc)
    for (k in seq_len(levels)) {
      ind <- (band == k) & !is.na(band)
      if (!any(ind)) next
      b <- wc_blur(ind * 1, 2.5 * s + 0.6)
      rim[ind] <- pmin(1, 2 * (1 - b[ind]))
    }
    f <- (1 + edge_pooling * rim) *
      (1 + bloom * wc_noise(nr, nc, 40 * s)) *
      (1 + granulation * (0.6 * wc_noise(nr, nc, 0.7) + 0.4 * wc_noise(nr, nc, 2.5 * s)))
    f <- pmax(f, 0.3)
    grain <- 1 - paper_grain * abs(wc_noise(nr, nc, 1.2))

    bi <- band; bi[is.na(bi)] <- 1L
    out <- vector("list", 3)
    for (ch in 1:3) {
      A <- matrix(A_cls[bi, ch], nr, nc)
      A <- wc_blur(A, 0.8 * s) * f                 # soft bled edges, then texture
      v <- paper[ch] * grain * exp(-A)
      v[is.na(band)] <- paper[ch] * grain[is.na(band)]
      out[[ch]] <- wc_l2s(v)
    }
    col <- matrix(grDevices::rgb(out[[1]], out[[2]], out[[3]]), nr, nc)
    col[is.na(z)] <- NA
    list(col = col, band = band, breaks = breaks)
  })
}

# ---- ggplot2 layer -----------------------------------------------------------

StatWatercolor <- ggplot2::ggproto("StatWatercolor", ggplot2::Stat,
  required_aes = c("x", "y", "z"),
  default_aes = ggplot2::aes(fill = ggplot2::after_stat(band)),
  compute_panel = function(data, scales, palette = "wc_sap_prussian", levels = 7,
                           reverse = FALSE, limits = NULL, midpoint = NULL,
                           seed = 1, resolution = 500, wobble = 0.035,
                           granulation = 0.18, edge_pooling = 0.45, bloom = 0.12,
                           paper_grain = 0.025, digits = 3) {
    xs <- sort(unique(data$x)); ys <- sort(unique(data$y))
    z <- matrix(NA_real_, length(ys), length(xs))
    z[cbind(match(data$y, ys), match(data$x, xs))] <- data$z
    # upsample coarse grids so brush texture has room to show
    f <- max(1, ceiling(resolution / max(length(xs), length(ys))))
    if (f > 1) {
      nr <- (length(ys) - 1) * f + 1; nc <- (length(xs) - 1) * f + 1
      z <- wc_resize(z, nr, nc)
      ys <- seq(min(ys), max(ys), length.out = nr)
      xs <- seq(min(xs), max(xs), length.out = nc)
    }
    zt <- z[nrow(z):1, , drop = FALSE]                    # row 1 = top
    p <- wc_paint_matrix(zt, palette, levels, reverse, limits, midpoint, seed,
                         wobble, granulation, edge_pooling, bloom, paper_grain)
    cmat <- p$col[nrow(z):1, , drop = FALSE]
    band <- p$band[nrow(z):1, , drop = FALSE]
    br <- signif(p$breaks, digits)
    labs <- paste(br[-length(br)], "to", br[-1])
    keep <- !is.na(cmat)
    data.frame(
      x = xs[col(z)[keep]], y = ys[row(z)[keep]],
      wc_col = cmat[keep],
      band = factor(labs[band[keep]], levels = labs),
      PANEL = data$PANEL[1], group = -1L
    )
  }
)

GeomWatercolor <- ggplot2::ggproto("GeomWatercolor", ggplot2::GeomRaster,
  draw_panel = function(self, data, panel_params, coord, interpolate = FALSE,
                        hjust = 0.5, vjust = 0.5) {
    data$fill <- data$wc_col            # paint with the textured colours;
    data$alpha <- NA                    # the mapped `fill` only drives the legend
    ggplot2::GeomRaster$draw_panel(data, panel_params, coord,
                                   interpolate = interpolate, hjust = hjust, vjust = vjust)
  },
  draw_key = ggplot2::draw_key_rect
)

#' Watercolour washes for gridded data
#'
#' Paints a regular grid (the same input `geom_raster()` takes) as classed
#' watercolour washes on Solarized paper. The washes have pigment granulation,
#' darker rims where each wash dries, soft blooms and slightly irregular
#' boundaries. A matching legend scale is added automatically.
#'
#' @param mapping Aesthetics: needs `x`, `y` and `z` (the value to paint).
#' @param data Data frame on a regular grid.
#' @param palette Palette name, see [wc_palettes].
#' @param levels Number of washes (classes). 5 to 8 reads best; use about 24
#'   with `edge_pooling = 0.15` for a near-continuous wash.
#' @param reverse Reverse the palette.
#' @param limits Value range to classify (default: data range).
#' @param midpoint For the diverging palette, the value that maps to bare paper.
#' @param seed Random seed for the texture (`NULL` = not reproducible).
#' @param resolution Coarse grids are upsampled to about this many cells
#'   along the long side so the texture has room to show.
#' @param wobble Irregularity of band boundaries (fraction of value range).
#' @param granulation Pigment grain strength.
#' @param edge_pooling Darkening at the rim of each wash.
#' @param bloom Large soft unevenness within washes.
#' @param paper_grain Paper texture strength.
#' @param legend_title Title for the fill legend.
#' @param add_scale Add the matching discrete fill scale for the legend?
#' @param na.rm,show.legend,inherit.aes,... Standard layer arguments.
#' @export
#' @examples
#' library(ggplot2)
#' v <- data.frame(x = rep(seq_len(nrow(volcano)), ncol(volcano)),
#'                 y = rep(seq_len(ncol(volcano)), each = nrow(volcano)),
#'                 z = as.vector(volcano))
#' ggplot(v, aes(x, y, z = z)) +
#'   geom_watercolor(palette = "sap_prussian", levels = 7) +
#'   coord_fixed() + theme_wc()
geom_watercolor <- function(mapping = NULL, data = NULL,
                            palette = "wc_sap_prussian", levels = 7,
                            reverse = FALSE, limits = NULL, midpoint = NULL,
                            seed = 1, resolution = 500, wobble = 0.035,
                            granulation = 0.18, edge_pooling = 0.45, bloom = 0.12,
                            paper_grain = 0.025, legend_title = ggplot2::waiver(),
                            add_scale = TRUE, na.rm = TRUE, show.legend = NA,
                            inherit.aes = TRUE, ...) {
  palette <- wc_match_palette(palette)
  lyr <- ggplot2::layer(
    stat = StatWatercolor, geom = GeomWatercolor, data = data, mapping = mapping,
    position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(palette = palette, levels = levels, reverse = reverse,
                  limits = limits, midpoint = midpoint, seed = seed,
                  resolution = resolution, wobble = wobble,
                  granulation = granulation, edge_pooling = edge_pooling,
                  bloom = bloom, paper_grain = paper_grain, na.rm = na.rm, ...)
  )
  if (!add_scale) return(lyr)
  list(lyr, ggplot2::scale_fill_manual(
    name = legend_title, values = wc_colors(palette, levels, reverse = reverse),
    drop = FALSE, na.translate = FALSE,
    breaks = function(lims) {                     # many washes -> label ~7 of them
      if (length(lims) <= 10) return(lims)
      lims[unique(round(seq(1, length(lims), length.out = 7)))]
    },
    guide = ggplot2::guide_legend(reverse = TRUE, ncol = 1,
                                  override.aes = list(colour = NA))))
}

#' @rdname geom_watercolor
#' @export
stat_watercolor <- function(mapping = NULL, data = NULL, ...) {
  geom_watercolor(mapping = mapping, data = data, ...)
}
