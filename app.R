# Chocolate consumption vs Nobel laureates - interactive companion app
# Course: Statistics and Exploratory Data Analysis (WNE UW)
# Paper:  Messerli FH (2012), N Engl J Med 367(16):1562-1564
#
# Run from the project folder:  shiny::runApp()

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# 1. Data (path is relative to the project folder, so it works on any computer)
load("data/ds16_Nobel_Laureates_and_Chocolate.rda")
df <- ds16_Nobel_Laureates_and_Chocolate

# 2. UI layout
ui <- fluidPage(
  titlePanel("Chocolate Consumption and Nobel Laureates"),

  sidebarLayout(
    sidebarPanel(
      checkboxInput("exclude_sweden", "Exclude outlier (Sweden)", value = FALSE),
      checkboxInput("show_labels", "Show country names", value = FALSE),

      hr(),
      h4("Correlation results"),
      verbatimTextOutput("correlation_stats")
    ),

    mainPanel(
      plotlyOutput("scatter_plot", height = "550px")
    )
  )
)

# 3. Server logic
server <- function(input, output, session) {

  # Reactive filter: listens to the checkbox
  filtered_data <- reactive({
    if (input$exclude_sweden) {
      df %>% filter(COUNTRY != "Sweden")
    } else {
      df
    }
  })

  output$scatter_plot <- renderPlotly({
    plot_data <- filtered_data() %>%
      mutate(is_outlier = ifelse(COUNTRY == "Sweden", "Outlier", "Normal"))

    p <- ggplot(plot_data, aes(x = CHOCOLATE, y = NOBEL)) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                  color = "#95A5A6", linetype = "dashed", linewidth = 1) +
      geom_point(
        aes(color = is_outlier, size = is_outlier,
            text = paste0("<b>", COUNTRY, "</b>",
                          "<br>Chocolate: ", CHOCOLATE, " kg/capita",
                          "<br>Nobels: ", NOBEL, " per 10M")),
        alpha = 0.8
      ) +
      scale_color_manual(values = c("Normal" = "#2C3E50", "Outlier" = "#E74C3C")) +
      scale_size_manual(values = c("Normal" = 4, "Outlier" = 7)) +
      labs(
        title = "Chocolate Consumption vs. Nobel Laureates",
        x = "Chocolate Consumption (kg/capita/year)",
        y = "Nobel Laureates (per 10 million population)"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold")
      )

    if (input$show_labels) {
      p <- p + geom_text(aes(label = COUNTRY), vjust = -1.2, size = 3.5)
    }

    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(size = 14)))
  })

  # Correlation statistics for the currently shown data
  output$correlation_stats <- renderPrint({
    current_df <- filtered_data()

    pearson  <- cor.test(current_df$CHOCOLATE, current_df$NOBEL, method = "pearson")
    spearman <- cor.test(current_df$CHOCOLATE, current_df$NOBEL, method = "spearman", exact = FALSE)
    kendall  <- cor.test(current_df$CHOCOLATE, current_df$NOBEL, method = "kendall",  exact = FALSE)
    slope    <- coef(lm(NOBEL ~ CHOCOLATE, data = current_df))[["CHOCOLATE"]]

    cat("n =", nrow(current_df), "countries\n\n")

    cat("--- Pearson (linear) ---\n")
    cat("r =", round(pearson$estimate, 3), "| p-value =",
        format.pval(pearson$p.value, eps = 0.0001, digits = 3, scientific = FALSE), "\n\n")

    cat("--- Spearman (rank) ---\n")
    cat("rho =", round(spearman$estimate, 3), "| p-value =",
        format.pval(spearman$p.value, eps = 0.0001, digits = 3, scientific = FALSE), "\n\n")

    cat("--- Kendall (tau) ---\n")
    cat("tau =", round(kendall$estimate, 3), "| p-value =",
        format.pval(kendall$p.value, eps = 0.0001, digits = 3, scientific = FALSE), "\n\n")

    cat("--- Regression line ---\n")
    cat("slope =", round(slope, 2), "laureates per 10M for each extra kg\n")
  })
}

shinyApp(ui = ui, server = server)
