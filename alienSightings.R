# Alien Sightings Dashboard

library(shiny)
library(tidyverse)

# data comes from this site https://data.world/timothyrenner/ufo-sightings

ufo_data <- read.csv("nuforc_reports.csv") %>%
  filter(state != "") %>%
  mutate(date_time = as.Date(date_time)) %>%
  mutate(duration = str_replace(duration, "-"," to ")) %>%
  mutate(trueTime = str_detect(duration, "[Ss]econd|[Mm]inute|[Hh]our")) %>%
  filter(trueTime == TRUE) %>%
  separate(duration, c("value", "unit"), extra ="merge", fill = "right") %>%
  mutate(value = as.numeric(str_extract(value, "[0-9]{1,2}"))) %>%
  mutate(value = ifelse(is.na(value),0,value)) %>%
  mutate(addition = case_when(grepl("second", unit) ~ 1,
                              grepl("minute", unit) ~ 60,
                              grepl("hour", unit) ~ 3600,
                              TRUE ~ 1)) %>%
  mutate(duration_sec = value * addition)

ui <- fluidPage(
  titlePanel("UFO Sightings"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", 
                  "Choose a state or country", 
                  unique(ufo_data$state)),
      dateRangeInput("dates", 
                     label = "Choose a date range", 
                     start = "1960-01-01",
                     end = "2020-01-01")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Number Sighted",
                 plotOutput("shapes")),
        tabPanel("Duration Table",
                 tableOutput("duration_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  output$shapes <- renderPlot({
    ufo_data %>%
      filter(state == input$state,
             date_time >= input$dates[1],
             date_time <= input$dates[2]) %>%
      ggplot(aes(shape)) +
      geom_bar() +
      labs(x = "Shape", y = "# Sighted") +
      theme(axis.text.x = element_text(angle = 90, vjust = 1))
  })
  output$duration_table <- renderTable({
    ufo_data %>%
      filter(
        state == input$state,
        date_time >= input$dates[1],
        date_time <= input$dates[2]
      ) %>%
      group_by(shape) %>%
      summarize(
        nb_sighted = n(),
        avg_duration = mean(duration_sec),
        median_duration = median(duration_sec),
        min_duration = min(duration_sec),
        max_duration = max(duration_sec)
      )
  })
}

shinyApp(ui = ui, server = server)
