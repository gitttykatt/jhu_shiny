library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for app that draws a visualization of Iris dataset ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Iris dataset visualization"),

  p("For JHU Coursera Assignment, July 20, 2021"),
  br(), 
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choices ----
      
      radioButtons(inputId = "sepalorpetal", 
                   label = "Select Sepal or Petal:", 
                   choices = list("Sepal" = "sepal", 
                                  "Petal" = "petal"), 
                   selected = "sepal"
                   ), 
      
      checkboxGroupInput(inputId = "species",
                  label = "Select Species:",
                  choices = c("Setosa" = "setosa", 
                              "Versicolor" = "versicolor", 
                              "Virginica" = "virginica"), 
                  selected = "setosa"
                  ), 
      
      br(), 
      p("More information on Iris dataset can be found on:"), 
      a("https://en.wikipedia.org/wiki/Iris_flower_data_set")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output ----
      plotOutput(outputId = "plot")
      
    )
  )
)

# Define server logic required to draw a plot ----
server <- function(input, output) {

  output$plot <- renderPlot({
    
    data <- iris
     
    if (input$sepalorpetal == "sepal") {
      data <- data %>% select(x = Sepal.Width, y = Sepal.Length, Species)
    }
    else if (input$sepalorpetal == "petal") {
      data <- data %>% select(x = Petal.Width, y = Petal.Length, Species)
    }
    
    validate(
      need(input$species != "", "Please select at least 1 Specie")
    )
    
    data %>%
      filter(Species == input$species) %>%
      ggplot(aes(x, y, col=Species)) + 
      geom_point() + 
      labs(x = "Width", y = "Length")
  }

  )
}

shinyApp(ui = ui, server = server)