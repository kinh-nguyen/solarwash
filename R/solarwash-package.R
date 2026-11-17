#' solarwash: Solarized watercolour palettes for ggplot2
#'
#' Solarized accent colours are treated as watercolour pigments glazed onto
#' Solarized base3 paper. Darker values come from adding pigment layers
#' (subtractive, Beer-Lambert mixing) rather than from blending on screen, and
#' lightness falls evenly from paper to the deepest wash, so the colours read in
#' order and stay monotonic in grayscale and under colour-vision-deficiency
#' simulation.
#'
#' Palettes:
#' * `wc_sap_prussian`: paper, gamboge, sap green, viridian, prussian. For elevation and vegetation.
#' * `wc_ochre_indigo`: paper, ochre, burnt sienna, madder rose, violet, indigo. For density and counts.
#' * `wc_rose_plum`: paper, peach, madder, plum, deep violet. For intensity.
#' * `wc_sienna_prussian`: prussian blue, paper, burnt sienna. Diverging, for anomalies.
#'
#' @keywords internal
"_PACKAGE"

utils::globalVariables(c("band"))
