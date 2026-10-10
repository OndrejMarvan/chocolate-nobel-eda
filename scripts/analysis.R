# Reproducible checks behind the presentation numbers.
# Run from the project folder:  source("scripts/analysis.R")   (or Rscript scripts/analysis.R)

load("data/ds16_Nobel_Laureates_and_Chocolate.rda")
df <- as.data.frame(ds16_Nobel_Laureates_and_Chocolate)

# ---- 1. Correlations: Pearson, Spearman, Kendall -----------------------------
cor_row <- function(d, label) {
  p <- cor.test(d$CHOCOLATE, d$NOBEL, method = "pearson")
  s <- cor.test(d$CHOCOLATE, d$NOBEL, method = "spearman", exact = FALSE)
  k <- cor.test(d$CHOCOLATE, d$NOBEL, method = "kendall",  exact = FALSE)
  data.frame(sample = label, n = nrow(d),
             pearson_r = round(unname(p$estimate), 3),
             spearman_rho = round(unname(s$estimate), 3),
             kendall_tau = round(unname(k$estimate), 3),
             pearson_p = signif(p$p.value, 2))
}

cat("\n=== 1. Correlations ===\n")
print(rbind(
  cor_row(df, "All countries"),
  cor_row(subset(df, COUNTRY != "Sweden"), "Without Sweden"),
  cor_row(subset(df, !COUNTRY %in% c("Sweden", "Switzerland")), "Without Sweden and Switzerland")
), row.names = FALSE)
cat("Paper (Messerli 2012): r = 0.791 (all), r = 0.862 (without Sweden)\n")

# ---- 2. Regression line --------------------------------------------------------
fit <- lm(NOBEL ~ CHOCOLATE, data = df)
cat("\n=== 2. Regression ===\n")
cat("Slope:", round(coef(fit)[["CHOCOLATE"]], 3), "laureates per 10M per kg\n")
cat("kg of chocolate for +1 laureate:", round(1 / coef(fit)[["CHOCOLATE"]], 2), "(paper: about 0.4)\n")
cat("Sweden predicted:", round(predict(fit, data.frame(CHOCOLATE = 6.4)), 1),
    "| observed:", df$NOBEL[df$COUNTRY == "Sweden"], "(paper: about 14 vs 32)\n")

# ---- 3. Outlier checks ---------------------------------------------------------
cat("\n=== 3. Outliers ===\n")
q <- quantile(df$NOBEL, c(0.25, 0.75)); iqr <- q[2] - q[1]
lo <- q[1] - 1.5 * iqr; hi <- q[2] + 1.5 * iqr
cat("IQR rule on NOBEL: fences", round(lo, 1), "to", round(hi, 1), "-> outliers:",
    paste(df$COUNTRY[df$NOBEL < lo | df$NOBEL > hi], collapse = ", "), "\n")

z <- as.numeric(scale(df$NOBEL))
cat("Z-score > 2 on NOBEL:", paste(df$COUNTRY[abs(z) > 2], collapse = ", "), "\n")

zres <- as.numeric(scale(resid(fit)))
cat("Z-score of regression residuals, largest:\n")
print(head(data.frame(country = df$COUNTRY, z_resid = round(zres, 2))[order(-abs(zres)), ], 3), row.names = FALSE)

cook <- cooks.distance(fit)
cat("Cook's distance, largest:\n")
print(head(data.frame(country = df$COUNTRY, cooks_d = round(cook, 2))[order(-cook), ], 3), row.names = FALSE)

# ---- 4. Normality --------------------------------------------------------------
cat("\n=== 4. Shapiro-Wilk normality test (p-value) ===\n")
cat("CHOCOLATE:", round(shapiro.test(df$CHOCOLATE)$p.value, 3),
    "| NOBEL:", round(shapiro.test(df$NOBEL)$p.value, 3), "\n")

# ---- 5. Rough confounder check (INTERNET = crude proxy for development) -------
cat("\n=== 5. Partial correlation controlling for INTERNET (rough proxy) ===\n")
n   <- nrow(df)
rxy <- cor(df$CHOCOLATE, df$NOBEL)
rxz <- cor(df$CHOCOLATE, df$INTERNET)
ryz <- cor(df$NOBEL, df$INTERNET)
pc  <- (rxy - rxz * ryz) / sqrt((1 - rxz^2) * (1 - ryz^2))
t   <- pc * sqrt((n - 3) / (1 - pc^2))
cat("cor(chocolate, internet) =", round(rxz, 2), "| cor(nobel, internet) =", round(ryz, 2), "\n")
cat("partial r =", round(pc, 3), "| p =", round(2 * pt(abs(t), n - 3, lower.tail = FALSE), 3), "\n")
cat("This is one crude proxy and n = 23, so it does not prove or rule out confounding.\n")
