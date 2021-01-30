# Load the plotly package

library(plotly)
library(gapminder)
library(tidyverse)
library(colourpicker)

ui <- fluidPage(
  h1("Gapminder"),
  sliderInput(inputId = "life", label = "Life expectancy",
              min = 0, max = 120,
              value = c(30, 50)),
  selectInput("continent", "Continent",
              choices = c("All", levels(gapminder$continent))),
  tableOutput("table")
)

server <- function(input, output) {
  output$table <- renderTable({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    # Don't subset the data if "All" continent are chosen
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    data
  })
}

shinyApp(ui, server)