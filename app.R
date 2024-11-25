library(shiny)
library(ggplot2)
library(dplyr)

# Load your dataset
# Assuming your dataset is called 'data' and contains the columns: 
# season, episode, dialogue_density, avg_length, sentiment_variance, unique_words, question_ratio, exclamation_ratio



bob_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-19/episode_metrics.csv')

# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr) # For pivot_longer

# UI
ui <- fluidPage(
  titlePanel("Bob's Burgers Metrics Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Select Season:", choices = unique(bob_data$season)),
      selectInput("episode", "Select Episode:", choices = NULL),
      selectInput("x_metric", "Select X Metric:", 
                  choices = c("dialogue_density", "avg_length", "sentiment_variance", "unique_words")),
      selectInput("y_metric", "Select Y Metric:", 
                  choices = c("dialogue_density", "avg_length", "sentiment_variance", "unique_words"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Line Plot", plotOutput("line_plot")),
        tabPanel("Scatter Plot", plotOutput("scatter_plot"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Update episode choices based on selected season
  observeEvent(input$season, {
    episodes <- bob_data %>%
      filter(season == input$season) %>%
      pull(episode)
    updateSelectInput(session, "episode", choices = episodes)
  })
  
  # Filter data for the selected season and episode
  filtered_data <- reactive({
    bob_data %>%
      filter(season == input$season, episode == input$episode)
  })
  
  # Render Line Plot
  output$line_plot <- renderPlot({
    # Data for the whole season
    season_data <- bob_data %>%
      filter(season == input$season)
    
    # Reshape data for plotting
    metrics <- season_data %>%
      select(episode, dialogue_density, avg_length, sentiment_variance, unique_words) %>%
      pivot_longer(cols = -episode, names_to = "Metric", values_to = "Value")
    
    ggplot(metrics, aes(x = episode, y = Value, color = Metric)) +
      geom_line(size = 1.2) +
      geom_point(data = metrics %>% filter(episode == input$episode), size = 4, color = "red") +
      labs(
        title = paste("Metrics Across Season", input$season),
        subtitle = paste("Highlighted: Episode", input$episode),
        x = "Episode",
        y = "Metric Value"
      ) +
      theme_minimal() +
      theme(
        legend.title = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14)
      )
  })
  
  # Render Scatter Plot
  output$scatter_plot <- renderPlot({
    # Data for the whole season
    season_data <- bob_data %>%
      filter(season == input$season)
    
    ggplot(season_data, aes_string(x = input$x_metric, y = input$y_metric)) +
      geom_point(size = 3, alpha = 0.7, color = "blue") +
      geom_point(data = filtered_data(), aes_string(x = input$x_metric, y = input$y_metric), 
                 size = 5, color = "red") +
      labs(
        title = paste("Relationship Between", input$x_metric, "and", input$y_metric),
        subtitle = paste("Highlighted: Episode", input$episode),
        x = input$x_metric,
        y = input$y_metric
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14)
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
