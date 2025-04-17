library(tidyverse)
library(ggplot2)
library(dplyr)
library(shiny)         # For interactivity

# Load the data
student_data <- read.csv("C:/Users/varun/Desktop/archive/StudentsPerformance.csv")

# Convert column names if needed
colnames(student_data) <- tolower(gsub(" ", "_", colnames(student_data)))

# Average scores by gender
avg_scores <- student_data %>%
  group_by(gender) %>%
  summarise(
    math = mean(math.score),
    reading = mean(reading.score),
    writing = mean(writing.score)
  )

# Bar chart: Average scores by gender
ggplot(avg_scores, aes(x = gender)) +
  geom_bar(aes(y = math), stat = "identity", fill = "skyblue", position = position_dodge()) +
  geom_bar(aes(y = reading), stat = "identity", fill = "orange", position = position_dodge(width = 0.9)) +
  geom_bar(aes(y = writing), stat = "identity", fill = "green", position = position_dodge(width = 0.9)) +
  labs(title = "Average Scores by Gender", y = "Average Score") +
  theme_minimal()


library(shiny)

ui <- fluidPage(
  titlePanel("Student Performance Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Select Gender:", choices = unique(student_data$gender), selected = "female")
    ),
    mainPanel(
      plotOutput("scorePlot")
    )m
  )
)

server <- function(input, output) {
  output$scorePlot <- renderPlot({
    data_filtered <- student_data %>% filter(gender == input$gender)
    
    ggplot(data_filtered, aes(x = math.score, y = writing.score)) +
      geom_point() +
      labs(title = paste("Math vs Writing Score:", input$gender),
           x = "Math Score", y = "Writing Score")
  })
}

shinyApp(ui = ui, server = server)
