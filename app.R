library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(leaflet)
library(zoo)

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

# variables
first_date <- as.Date(as.yearmon("2014-01", "%Y-%m"))
last_date <- as.Date(as.yearmon("2020-03", "%Y-%m"))

# shiny ui

ui <- fluidPage(
  includeCSS("styles.css"),
  navbarPage("Rental Property", theme = shinytheme("slate"), collapsible = TRUE,
    tabPanel("Map", icon = icon("globe-americas"),
      div(class = "map-container",
        leafletOutput("mymap", width = "100%", height = "100%"),
        absolutePanel(top = 62, left = 54, width = 250, fixed=TRUE, draggable = TRUE, height = "auto",
          sliderInput("date",
            label = NULL,
            min = first_date,
            max = last_date,
            value = last_date,
            timeFormat="%b %Y",
            animate = animationOptions(interval = 30, loop = FALSE))
        )
      )
    ),
    tabPanel("Data", icon = icon("table")),
    tabPanel("About", icon = icon("info-circle"))
  )
)

# shiny server

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat = 39.8283, lng = -98.5795, zoom = 2)
  })
}

# autoreload for devlepment
options(shiny.autoreload = TRUE)
shinyApp(ui = ui, server = server)
