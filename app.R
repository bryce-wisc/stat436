
library(shiny)
library(dplyr)
library(tidyverse)
college_football_data <- read_csv("/srv/connect/apps/stat436/homework2/homework2/cfb22.csv")
selected_stats_1 <- c("Team", "Win-Loss", "Off Rank", "Def Rank")
selected_stats_2 <- c("Team","Off TDs", "Off Yards per Game", "3rd Percent")
ui <- fluidPage(
  titlePanel("College Football Statistics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team1", "Select Team 1", choices = unique(college_football_data$Team)),
      selectInput("team2", "Select Team 2", choices = unique(college_football_data$Team))
    ),
    mainPanel(
      h2("General Statistics"),
      tableOutput("general"),
      h2("Advanced Statistics"),
      plotOutput("plot1", height = "600px", width = "1200px"),
#      plotOutput("plot2"),
    )
  )
)

# Define server
server <- function(input, output) {
  output$general <- renderTable({
    general_stats <- college_football_data %>%
      filter(Team %in% c(input$team1, input$team2)) %>%
      select(selected_stats_1)
    general_stats
  })
  output$plot1 <- renderPlot({
    teams_data <- college_football_data |>
      filter(Team %in% c(input$team1, input$team2)) |>
      select(selected_stats_2)
    teams_data_long <- teams_data %>%
      pivot_longer(cols = -Team, names_to = "Statistic", values_to = "Value")
    ggplot(teams_data_long, aes(x = Team, y = Value, fill = Team)) +
      geom_col(position = "dodge", color = "black") +
      labs(title = "Statistics Comparison by Team", x = "Team", y = "Value") +
    #  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap(~ Statistic, scales = "free_y", ncol = 2)
  })
#  output$plot1 <- renderPlot({
 #   ggplot(teams_data) +
 #   geom_histogram(aes(input$team1, input$team2))})
 # output$plot2 <- renderPlot(histogram(teams_data(), input$team2))
  
}

# Run the application
shinyApp(ui = ui, server = server)
