library(shiny)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(plotly)

# 1. Dataset
load("~/Desktop/Statistics and Exploratory Data Analysis Shinny App/ds16_Nobel_Laureates_and_Chocolate.rda")
df <- ds16_Nobel_Laureates_and_Chocolate

# 2. UI Layout
ui <- fluidPage(
  titlePanel("Chocolate Consumption and Nobel Laureates"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput("exclude_sweden", "Exclude Outlier (Sweden)", value = FALSE),
      
      hr(),
      h4("Correlation Results"),
      verbatimTextOutput("correlation_stats")
    ),
    
    mainPanel(
      plotlyOutput("scatter_plot")
    )
  )
)

# 3. Server Logic
server <- function(input, output) {
  
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
    
    p <- ggplot(plot_data, aes(
      x = CHOCOLATE,
      y = NOBEL,
      text = paste("<b>", COUNTRY, "</b>",
                   "<br>Chocolate:", CHOCOLATE, "kg/capita",
                   "<br>Nobels:", NOBEL, "per 10M")
      )) +
      geom_smooth(method = "lm", se = FALSE, color = "#95A5A6", linetype = "dashed", linewidth = 1) +
      geom_point(aes(color = is_outlier, size = is_outlier), alpha = 0.8) +
      scale_color_manual(values = c("Normal" = "#2C3E50", "Outlier" = "#E74C3C")) +
      scale_size_manual(values = c("Normal" = 4, "Outlier" = 7)) +
      labs(
        title = "Chocolate Consumption vs. Nobel Laureates",
        subtitle = "",
        x = "Chocolate Consumption (kg/capita/year)",
        y = "Nobel Laureates (per 10 million population)"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold")
      )
      
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(size = 14)))
  })
  
  # Render the correlation statistics
  output$correlation_stats <- renderPrint({
    current_df <- filtered_data()
    
    pearson <- cor.test(current_df$CHOCOLATE, current_df$NOBEL, method = "pearson")
    spearman <- cor.test(current_df$CHOCOLATE, current_df$NOBEL, method = "spearman", exact = FALSE)
    kendall <- cor.test(current_df$CHOCOLATE, current_df$NOBEL, method = "kendall", exact = FALSE)
   
    cat("--- Pearson (Linear) ---\n")
    cat("r =", round(pearson$estimate, 3), "| p-value =", format.pval(pearson$p.value, eps = 0.0001, digits = 3, scientific = FALSE), "\n\n")
    
    cat("--- Spearman (Rank) ---\n")
    cat("rho =", round(spearman$estimate, 3), "| p-value =", format.pval(spearman$p.value, eps = 0.0001, digits = 3, scientific = FALSE), "\n\n")
    
    cat("--- kendall(Tau) ---\n")
    cat("tau =", round(kendall$estimate, 3), "| p-value =", format.pval(kendall$p.value, eps = 0.0001, digits = 3, scientific = FALSE), "\n")
  })
}

shinyApp(ui = ui, server = server)
