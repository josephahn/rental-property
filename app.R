library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(leaflet)

# import data
sales <- read.csv("data/sales.csv", check.names = FALSE)
rents <- read.csv("data/rents.csv", check.names = FALSE)

# clean data
sales_clean <- sales %>%
  filter(RegionName != "United States") %>%
  separate(RegionName, c("City", "State"), sep = ", ") %>%
  select(City, State, "2014-01":"2020-03")

rents_clean <- rents %>%
  filter(RegionName != "United States") %>%
  separate(RegionName, c("City", "State"), sep = ", ") %>%
  select(City, State, "2014-01":"2020-03")

ui <- fluidPage(
  navbarPage("Rental Property", theme = shinytheme("slate"),
    tabPanel("Map", icon = icon("globe-americas"),
      leafletOutput("mymap")
    ),
    tabPanel("Data", icon = icon("table")),
    tabPanel("About", icon = icon("info-circle"))
  )
)

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat=39.8283, lng=-98.5795, zoom=2)
  })
}

shinyApp(ui = ui, server = server)
