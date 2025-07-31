# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

# --- Data Processing Module ---
# Read and clean data
tryCatch({
  data <- read.csv("D:/Downloads/survey_data.csv")
  # Validate required columns
  required_cols <- c("Gender", "EducationLevel", "Age", "SatisfactionScore", "Purchases")
  if (!all(required_cols %in% names(data))) {
    stop("CSV file must contain columns: ", paste(required_cols, collapse = ", "))
  }
  # Clean data
  data <- data %>%
    na.omit() %>%
    mutate(Gender = as.factor(Gender),
           EducationLevel = as.factor(EducationLevel),
           Age = as.numeric(Age),
           SatisfactionScore = as.numeric(SatisfactionScore),
           Purchases = as.numeric(Purchases))
}, error = function(e) {
  stop("Error reading CSV file: ", e$message)
})

# --- Shiny App (Visualization, Statistical Analysis, and Export Modules) ---
# UI
ui <- fluidPage(
  titlePanel("Interactive Survey Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Select Plot Type:", 
                  choices = c("Pie Chart", "Histogram")),
      conditionalPanel(
        condition = "input.plot_type == 'Pie Chart'",
        selectInput("pie_var", "Select Variable for Pie Chart:", 
                    choices = c("Gender", "EducationLevel"))
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Histogram'",
        selectInput("hist_var", "Select Variable for Histogram:", 
                    choices = c("Age", "SatisfactionScore", "Purchases")),
        sliderInput("binwidth", "Histogram Bin Width:", 
                    min = 1, max = 10, value = 5, step = 1)
      ),
      selectInput("filter_var", "Filter by Gender (Optional):", 
                  choices = c("All", levels(data$Gender))),
      selectInput("filter_edu", "Filter by Education Level (Optional):", 
                  choices = c("All", levels(data$EducationLevel))),
      downloadButton("download_plot", "Download Plot as PNG"),
      downloadButton("download_data", "Download Filtered Data as CSV"),
      downloadButton("download_summary", "Download Summary as CSV")
    ),
    mainPanel(
      plotOutput("survey_plot"),
      h3("Statistical Insights"),
      tableOutput("descriptive_stats"),
      textOutput("correlation_info"),
      tableOutput("grouped_stats")
    )
  )
)

# Server
server <- function(input, output) {
  # Reactive data filtering
  filtered_data <- reactive({
    df <- data
    if (input$filter_var != "All") {
      df <- df %>% filter(Gender == input$filter_var)
    }
    if (input$filter_edu != "All") {
      df <- df %>% filter(EducationLevel == input$filter_edu)
    }
    df
  })
  
  # Render plot (Visualization Module)
  output$survey_plot <- renderPlot({
    df <- filtered_data()
    
    if (input$plot_type == "Pie Chart") {
      var <- input$pie_var
      pie_data <- df %>%
        count(!!sym(var)) %>%
        mutate(percentage = n / sum(n) * 100,
               label = paste0(!!sym(var), ": ", round(percentage, 1), "%"))
      
      ggplot(pie_data, aes(x = "", y = percentage, fill = !!sym(var))) +
        geom_col() +
        coord_polar(theta = "y") +
        geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
        labs(title = paste(var, "Distribution")) +
        theme_void()
    } else {
      var <- input$hist_var
      ggplot(df, aes(x = !!sym(var))) +
        geom_histogram(binwidth = input$binwidth, fill = "steelblue", color = "black") +
        labs(title = paste(var, "Distribution"), 
             x = var, y = "Count")
    }
  })
  
  # Statistical Analysis Module
  # Descriptive statistics
  output$descriptive_stats <- renderTable({
    df <- filtered_data()
    df %>%
      summarise(
        Mean_Age = mean(Age, na.rm = TRUE),
        Median_Age = median(Age, na.rm = TRUE),
        SD_Age = sd(Age, na.rm = TRUE),
        Mean_Satisfaction = mean(SatisfactionScore, na.rm = TRUE),
        Median_Satisfaction = median(SatisfactionScore, na.rm = TRUE),
        SD_Satisfaction = sd(SatisfactionScore, na.rm = TRUE),
        Mean_Purchases = mean(Purchases, na.rm = TRUE),
        Median_Purchases = median(Purchases, na.rm = TRUE),
        SD_Purchases = sd(Purchases, na.rm = TRUE)
      )
  })
  
  # Correlation insights
  output$correlation_info <- renderText({
    df <- filtered_data()
    corr <- cor(df$SatisfactionScore, df$Purchases, use = "complete.obs")
    paste("Correlation between Satisfaction Score and Purchases:", round(corr, 2))
  })
  
  # Grouped metrics
  output$grouped_stats <- renderTable({
    df <- filtered_data()
    df %>%
      group_by(Gender, EducationLevel) %>%
      summarise(
        Mean_Satisfaction = mean(SatisfactionScore, na.rm = TRUE),
        Mean_Purchases = mean(Purchases, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # Export Module
  # Download plot as PNG
  output$download_plot <- downloadHandler(
    filename = function() {
      paste(input$plot_type, "_", 
            ifelse(input$plot_type == "Pie Chart", input$pie_var, input$hist_var), 
            "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      df <- filtered_data()
      if (input$plot_type == "Pie Chart") {
        var <- input$pie_var
        pie_data <- df %>%
          count(!!sym(var)) %>%
          mutate(percentage = n / sum(n) * 100,
                 label = paste0(!!sym(var), ": ", round(percentage, 1), "%"))
        plot <- ggplot(pie_data, aes(x = "", y = percentage, fill = !!sym(var))) +
          geom_col() +
          coord_polar(theta = "y") +
          geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
          labs(title = paste(var, "Distribution")) +
          theme_void()
      } else {
        var <- input$hist_var
        plot <- ggplot(df, aes(x = !!sym(var))) +
          geom_histogram(binwidth = input$binwidth, fill = "steelblue", color = "black") +
          labs(title = paste(var, "Distribution"), 
               x = var, y = "Count")
      }
      ggsave(file, plot = plot, device = "png", width = 6, height = 4)
    }
  )
  
  # Download filtered data as CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Download summary as CSV
  output$download_summary <- downloadHandler(
    filename = function() {
      paste("summary_stats_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df <- filtered_data()
      summary_stats <- df %>%
        summarise(
          Mean_Age = mean(Age, na.rm = TRUE),
          Median_Age = median(Age, na.rm = TRUE),
          SD_Age = sd(Age, na.rm = TRUE),
          Mean_Satisfaction = mean(SatisfactionScore, na.rm = TRUE),
          Median_Satisfaction = median(SatisfactionScore, na.rm = TRUE),
          SD_Satisfaction = sd(SatisfactionScore, na.rm = TRUE),
          Mean_Purchases = mean(Purchases, na.rm = TRUE),
          Median_Purchases = median(Purchases, na.rm = TRUE),
          SD_Purchases = sd(Purchases, na.rm = TRUE)
        )
      write.csv(summary_stats, file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
