library(shiny)
library(shinythemes)

ui <- fluidPage(
  navbarPage("Rental Property", theme = shinytheme("slate"),
    tabPanel("Map", icon = icon("globe-americas")),
    tabPanel("Data", icon = icon("table")),
    tabPanel("About", icon = icon("info-circle"))
  )
)

server <- function(input, output, session) {

}

shinyApp(ui = ui, server = server)
