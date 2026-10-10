# Static versions of the app plot, for slides.
# Run from the project folder:  Rscript scripts/make_figures.R

library(ggplot2)
library(dplyr)
library(ggrepel)

load("data/ds16_Nobel_Laureates_and_Chocolate.rda")
df <- as.data.frame(ds16_Nobel_Laureates_and_Chocolate)
dir.create("figures", showWarnings = FALSE)

make_plot <- function(d, title) {
  r <- cor.test(d$CHOCOLATE, d$NOBEL)$estimate
  d <- d %>% mutate(is_outlier = ifelse(COUNTRY == "Sweden", "Outlier", "Normal"))
  ggplot(d, aes(CHOCOLATE, NOBEL)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                color = "#95A5A6", linetype = "dashed", linewidth = 1) +
    geom_point(aes(color = is_outlier, size = is_outlier), alpha = 0.85) +
    geom_text_repel(aes(label = COUNTRY), size = 4, max.overlaps = Inf,
                    box.padding = 0.35, min.segment.length = 0.2) +
    scale_color_manual(values = c("Normal" = "#2C3E50", "Outlier" = "#E74C3C")) +
    scale_size_manual(values = c("Normal" = 3.5, "Outlier" = 6)) +
    labs(title = title,
         subtitle = paste0("Pearson r = ", sprintf("%.3f", r), ", n = ", nrow(d), " countries"),
         x = "Chocolate consumption (kg per person per year)",
         y = "Nobel laureates per 10 million population") +
    theme_minimal(base_size = 15) +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))
}

ggsave("figures/scatter_all_countries.png",
       make_plot(df, "Chocolate consumption vs Nobel laureates"),
       width = 9, height = 6, dpi = 200, bg = "white")
ggsave("figures/scatter_without_sweden.png",
       make_plot(filter(df, COUNTRY != "Sweden"), "Same data without Sweden"),
       width = 9, height = 6, dpi = 200, bg = "white")
cat("Saved figures/scatter_all_countries.png and figures/scatter_without_sweden.png\n")
