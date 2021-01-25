library(shiny)

bmi_help_text <- "Body Mass Index is a simple calculation using a person's height and weight. The formula is BMI = kg/m2 where kg is a person's weight in kilograms and m2 is their height in metres squared. A BMI of 25.0 or more is overweight, while the healthy range is 18.5 to 24.9."

# user interface side
ui <- fluidPage(
    titlePanel('BMI Calculator'),
    sidebarLayout(
        sidebarPanel(
            textInput('name', 'Enter your name'),
            numericInput('height', 'Enter height (in)', 60, 24, 96, step = 1),
            numericInput('weight', 'Enter weight (lbs)', 150, 50, 1000),
            actionButton("show_height_cm", "Show height in cm"),
            actionButton("show_weight_kg", "Show weight in kg"),
            actionButton("show_bmi", "Show BMI"),
            actionButton("show_help", "Help")
        ),
        mainPanel(
            textOutput("height_cm"),
            textOutput("weight_kg"),
            textOutput("bmi"),
            textOutput("bmi_status")
        )
    )
)

# server function
server <- function(input, output, session) {
    observeEvent(input$show_help, {
        showModal(modalDialog(bmi_help_text))
    })
    # reactive functions allow the functions to be in cache and make the app faster
    rval_height_cm <- eventReactive(input$show_height_cm, {
        height_cm <- input$height * 2.54
    })
    rval_weight_kg <- eventReactive(input$show_weight_kg, {
        weight_kg <- input$weight/2.20462
    })
    
    rval_bmi <- eventReactive(input$show_bmi, {
        (input$weight/2.20462)/((input$height * 0.0254)^2)
    })
    
    rval_bmi_status <- reactive({
        cut(rval_bmi(), 
            breaks = c(0, 18.5, 24.9, 29.9, 40),
            labels = c('underweight', 'healthy', 'overweight', 'obese')
        )
    })
    
    # output values
    output$height_cm <- renderText({
        height_cm <- rval_height_cm()
        paste("You are", height_cm, "cm tall")
    })
    output$weight_kg <- renderText({
        weight_kg <- rval_weight_kg()
        paste("You are", round(weight_kg,1), "kg heavy")
    })
    
    output$bmi <- renderText({
        bmi <- rval_bmi()
        paste("Your BMI is", round(bmi, 1))
    })
    output$bmi_status <- renderText({
        bmi_status <- rval_bmi_status()
        paste("You are", bmi_status)
    })
    observe({
        showNotification(
            paste('You have entered the height', input$height)
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
