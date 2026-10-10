# Chocolate consumption and Nobel laureates

Presentation project for **Statistics and Exploratory Data Analysis** (WNE UW, winter semester 2026/27, group 1).

We present the paper below and add our own check in R. The main message is that correlation is not causation.

> Messerli FH. Chocolate consumption, cognitive function, and Nobel laureates.
> N Engl J Med 2012;367(16):1562-1564. doi:10.1056/NEJMon1211064

The paper itself is not in this repo (NEJM copyright). Please get it from the link above.

## Folder content

```
app.R                  Shiny app: scatter plot, Sweden on/off, Pearson / Spearman / Kendall
data/                  Course dataset (.rda used by the code, .csv is a copy to read easily)
scripts/analysis.R     Numbers behind the slides (correlations, outliers, partial correlation)
scripts/make_figures.R Static scatter plots for the slides
figures/               Output of make_figures.R
```

## How to run

1. Open `chocolate-nobel-eda.Rproj` in RStudio.
2. Install the packages once:
   `install.packages(c("shiny", "ggplot2", "dplyr", "plotly", "ggrepel"))`
3. Start the app: `shiny::runApp()`
4. Print the numbers: `source("scripts/analysis.R")`
5. Make the figures: `source("scripts/make_figures.R")`

Note: do not put other scripts in a folder called `R/`. Shiny runs every file in that folder when the app starts.

## Results (course dataset)

| Sample | n | Pearson r | Spearman rho | Kendall tau |
|---|---|---|---|---|
| All countries | 23 | 0.801 | 0.902 | 0.743 |
| Without Sweden | 22 | 0.875 | 0.934 | 0.788 |
| Without Sweden and Switzerland | 21 | 0.849 | 0.924 | 0.766 |

- The regression slope is about 2.5 laureates per 10 million people for each extra kg, so about 0.4 kg per extra laureate. This matches the paper.
- Sweden: predicted about 13, observed 32. The IQR rule on the laureate counts does not flag it. The z-score of the regression residuals does (z = 3.2).
- Rough confounder check: the partial correlation controlling for INTERNET (a crude proxy for development) is 0.63 (p = 0.002). It is lower than 0.80 but does not disappear. n = 23, so treat it as a rough idea only.

## Notes on the data

- Columns: `COUNTRY`, `CHOCOLATE` (kg per person per year), `NOBEL` (laureates per 10 million people), `POPULATION`, `INTERNET`. The units of the last two are not documented. They look like millions and percent of the population.
- The paper reports r = 0.791 (all countries) and r = 0.862 (without Sweden). Our dataset gives 0.801 and 0.875. I could not find the reason in the files. Say this on the slides.
- The paper says 22 countries in the Methods and 23 in the Results. The dataset has 23.
- The course data may not be meant for public sharing. Keep the repo private unless you checked.
