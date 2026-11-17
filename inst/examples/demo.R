library(ggplot2); library(solarwash)
v <- data.frame(x = rep(seq_len(nrow(volcano)), ncol(volcano)),
                y = rep(seq_len(ncol(volcano)), each = nrow(volcano)),
                z = as.vector(volcano))
print(wc_colors("sap_prussian", 7)); print(wc_colors("sienna_prussian", 5)); print(wc_colors("rose_plum", 4, reverse = TRUE))
t0 <- Sys.time()
p1 <- ggplot(v, aes(x, y, z = z)) +
  geom_watercolor(palette = "sap_prussian", levels = 7, legend_title = "Elevation (m)") +
  geom_contour(aes(z = z), colour = wc_ink, linewidth = 0.15, alpha = .5, bins = 14) +
  coord_fixed(expand = FALSE) + theme_wc(base_family = "serif") +
  labs(title = "Maunga Whau", subtitle = "geom_watercolor(palette = \"sap_prussian\", levels = 7)", x = NULL, y = NULL) +
  theme(axis.text = element_blank())
ggsave("demo_geom_watercolor.png", p1, width = 7, height = 5.2, dpi = 130)
cat("render secs:", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "\n")
v$anom <- v$z - mean(v$z)
p2 <- ggplot(v, aes(x, y, fill = anom)) + geom_raster() +
  scale_fill_wc_b("sienna_prussian", midpoint = 0, n.breaks = 8, name = "Anomaly") +
  coord_fixed(expand = FALSE) + theme_wc() + labs(title = "scale_fill_wc_b(\"sienna_prussian\", midpoint = 0)")
ggsave("demo_scale_b.png", p2, width = 7, height = 5, dpi = 110)
p3 <- ggplot(mpg, aes(displ, hwy, colour = cty)) + geom_point(size = 2.5) +
  scale_colour_wc_c("ochre_indigo") + theme_wc(grid = TRUE) + labs(title = "scale_colour_wc_c(\"ochre_indigo\")")
ggsave("demo_scale_c.png", p3, width = 7, height = 4.5, dpi = 110)
p4 <- ggplot(mpg, aes(class, fill = drv)) + geom_bar() + scale_fill_wc_d("rose_plum") + theme_wc() + labs(title = "scale_fill_wc_d(\"rose_plum\")")
ggsave("demo_scale_d.png", p4, width = 7, height = 4.5, dpi = 110)
