library(shiny)
library(dplyr)
library(tidyverse)
college_football_data <- read_csv("https://raw.githubusercontent.com/bryce-wisc/stat436/main/cfb22.csv")
selected_stats_1 <- c("Team", "Win-Loss", "Off Rank", "Def Rank")
selected_stats_2 <- c("Off Rank","Team", "Off Plays", "Off Yards", "Off Yards/Play", "Off TDs", "Off Yards per Game", "Def Rank", "Def Plays", "Yards Allowed", "Yards/Play Allowed", "Off TDs Allowed", "Total TDs Allowed", "Yards Per Game Allowed", "3rd Down Rank", "3rd Attempts", "3rd Conversions", "3rd Percent", "3rd Down Def Rank", "Opp 3rd Conversion", "Opp 3rd Attempt", "Opponent 3rd Percent", "4th Down Rank", "4th Attempts", "4th Conversions", "4th Percent", "4rd Down Def Rank", "Opp 4th Conversion", "Opp 4th Attempt", "Opponent 4th Percent", "Penalty Rank", "Penalties", "Penalty Yards", "Penalty Yards Per Game", "First Down Def Rank", "Opp First Down Runs", "Opp First Down Passes", "Opp First Down Penalties", "Opp First Downs", "First Down Rank", "First Down Runs", "First Down Passes", "First Down Penalties", "First Downs", "Kickoff Return Def Rank", "Opp Kickoff Returns", "Kickoff Touchbacks", "Opponent Kickoff Return Yards", "Opp Kickoff Return Touchdowns Allowed", "Avg Yards per Kickoff Return Allowed", "Kickoff Return Rank", "Kickoffs Returned", "Kickoff Return Yards", "Kickoff Return Touchdowns", "Avg Yard per Kickoff Return", "Passing Off Rank", "Pass Attempts", "Pass Completions", "Interceptions Thrown_x", "Pass Yards", "Pass Yards/Attempt", "Yards/Completion", "Pass Touchdowns", "Pass Yards Per Game", "Pass Def Rank", "Opp Completions Allowed", "Opp Pass Attempts", "Opp Pass Yds Allowed", "Opp Pass TDs Allowed", "Yards/Attempt Allowed", "Yards/Completion Allowed", "Pass Yards Per Game Allowed", "Punt Return Def Rank", "Opp Punt Returns", "Opp Net Punt Return Yards", "Opp Punt Return Touchdowns Allowed", "Avg Yards Allowed per Punt Return", "Punt Return Rank", "Punt Returns", "Net Punt Return Yards", "Punt Return Touchdowns", "Avg Yards Per Punt Return", "Redzone Def Rank", "Opp Redzone Attempts", "Opp Redzone Rush TD Allowed", "Opp Redzone Pass Touchdowns Allowed", "Opp Redzone Field Goals Made", "Opp Redzone Scores", "Redzone Points Allowed", "Redzone Off Rank", "Redzone Attempts", "Redzone Rush TD", "Redzone Pass TD", "Redzone Field Goals Made", "Redzone Scores", "Redzone Points", "Rushing Def Rank", "Opp Rush Attempts", "Opp Rush Yards Alloweed", "Yds/Rush Allowed", "Opp Rush Touchdowns Allowed", "Rush Yards Per Game Allowed", "Rushing Off Rank", "Rush Attempts", "Rush Yds", "Yards/Rush", "Rushing TD", "Rushing Yards per Game", "Scoring Def Rank", "Touchdowns Allowed", "Opponent Extra Points", "2 Point Conversions Allowed", "Opp Deflected Extra Points", "Opp Field Goals Made", "Opp Safety", "Points Allowed", "Avg Points per Game Allowed", "Scoring Off Rank", "Touchdowns", "PAT", "2 Point Conversions", "Defensive Points", "Field Goals", "Safety", "Total Points", "Points Per Game", "Sack Rank", "Sacks", "Sack Yards", "Average Sacks per Game", "Tackle for Loss Rank", "Solo Tackle For Loss", "Assist Tackle For Loss", "Tackle for Loss Yards", "Total Tackle For Loss", "Tackle For Loss Per Game", "Time of Possession Rank", "Time of Possession", "Average Time of Possession per Game", "Turnover Rank", "Fumbles Recovered", "Opponents Intercepted", "Turnovers Gain", "Fumbles Lost", "Interceptions Thrown_y", "Turnovers Lost", "Turnover Margin", "Avg Turnover Margin per Game")
ui <- fluidPage(
  titlePanel("College Football Statistics 2022"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team1", "Select Team 1", choices = unique(college_football_data$Team)),
      selectInput("team2", "Select Team 2", choices = unique(college_football_data$Team)),
      selectInput("statistic", "Select Statistic:", choices = selected_stats_2)
    ),
    mainPanel(
      h2("General Statistics"),
      tableOutput("general"),
      h2("Advanced Statistics"),
      plotOutput("plot1", height = "600px", width = "1200px"),
    )
  )
)

# Define server
server <- function(input, output) {
  output$general <- renderTable({
    general_stats <- college_football_data |>
      filter(Team %in% c(input$team1, input$team2)) |>
      select(selected_stats_1)
    general_stats
  })
  output$plot1 <- renderPlot({
    teams_data <- college_football_data |>
      filter(Team %in% c(input$team1, input$team2)) 
    ggplot(teams_data, aes(x = Team, y = .data[[input$statistic]], fill = Team)) +
      geom_col(position = "dodge", color = "black") +
      labs(title = "Statistics Comparison by Team", x = "Team", y = "Value") +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 22, face = "bold"),
            strip.text = element_text(size = 12, face = "bold"),
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            legend.text = element_text(size=12),
            legend.title = element_text(size =12)) 
  })
  
}

shinyApp(ui = ui, server = server)
