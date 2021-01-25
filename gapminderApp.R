library(shiny)
library(dplyr)
library(ggplot2)
library(gapminder)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Life Expectation vs. GDP Per Capita"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Select Year",
                        min = 1952,
                        max = 2007,
                        value = 1992, step = 5),
            selectInput("continent",
                        "Select Continent",
                        choices = unique(gapminder$continent))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Plot",
                    plotOutput("plot")),
                tabPanel(
                    "Table",
                    DT::DTOutput("table")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$plot <- renderPlot({
        data <- gapminder %>%
            filter(year == input$year) %>%
            filter(continent == input$continent)
        #print(data)
        ggplot(data, aes(x = gdpPercap, y = lifeExp)) +
            geom_point()
    })
    output$table <- DT::renderDT({
        gapminder %>%
            filter(year == input$year) %>%
            filter(continent == input$continent)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
