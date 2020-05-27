library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(leaflet)
library(zoo)

# import data
sales <- read.csv("data/sales.csv", check.names = FALSE)
rents <- read.csv("data/rents.csv", check.names = FALSE)
# https://simplemaps.com/data/us-cities
cities <- read.csv("data/simplemaps_uscities.csv")

# helper functions
first_city <- function(city) {
  unlist(strsplit(city, "-"))[1]
}

# clean data
sales_clean <- sales %>%
  filter(RegionName != "United States") %>%
  separate(RegionName, c("city", "state_id"), sep = ", ", remove = FALSE) %>%
  mutate(city = sapply(city, first_city)) %>%
  merge(cities, by = c("city", "state_id")) %>%
  select(region_name = RegionName, city, state_id, state_name, lat, lng, "2014-01":"2020-03")

rents_clean <- rents %>%
  filter(RegionName != "United States") %>%
  separate(RegionName, c("city", "state_id"), sep = ", ", remove = FALSE) %>%
  mutate(city = sapply(city, first_city)) %>%
  merge(cities, by = c("city", "state_id")) %>%
  select(region_name = RegionName, city, state_id, state_name, lat, lng, "2014-01":"2020-03")

rents_matching <- filter(rents_clean, rents_clean$region_name %in% sales_clean$region_name)
sales_matching <- filter(sales_clean, sales_clean$region_name %in% rents_clean$region_name)
ratio <- cbind(
  select(rents_matching, city, state_id, state_name, lat, lng),
  select(rents_matching, matches("\\d{4}-\\d{2}")) / select(sales_matching, matches("\\d{4}-\\d{2}"))
)

# variables
first_date <- as.Date(as.yearmon("2014-01", "%Y-%m"))
last_date <- as.Date(as.yearmon("2020-03", "%Y-%m"))
states <- sort(unique(union(sales_clean$state_name, rents_clean$state_name)))

# shiny ui

ui <- fluidPage(
  includeCSS("styles.css"),
  navbarPage(
    "Rental Property",
    theme = shinytheme("slate"),
    collapsible = TRUE,
    tabPanel(
      "Map",
      icon = icon("globe-americas"),
      div(
        class = "map-container",
        leafletOutput("mymap", width = "100%", height = "100%"),
        absolutePanel(
          id = "controls",
          bottom = 12,
          left = 12,
          width = 340,
          height = 300,
          fixed=TRUE,
          draggable = TRUE,
          sliderInput(
            inputId = "date",
            label = NULL,
            min = first_date,
            max = last_date,
            value = last_date,
            timeFormat="%b %Y",
            animate = animationOptions(interval = 30, loop = FALSE)
          ),
          radioButtons(
            inputId = "type",
            label = NULL,
            choices = c("Rent / Sale", "Sale", "Rent"),
            selected = "Rent / Sale"
          ),
          selectInput(
            inputId = "state",
            label = NULL,
            choices = c("All states", states),
          )
        )
      )
    ),
    tabPanel("Data", icon = icon("table")),
    tabPanel("About", icon = icon("info-circle"))
  )
)

# shiny server

server <- function(input, output, session) {
  get_map_data <- function(date, type, state) {
    ym_date <- str_sub(as.character(date), 1, 7)
    is_all_states <- state == "All states"

    if (type == "Rent / Sale") {
      data <- ratio %>%
        select(city, state_id, state_name, lat, lng, all_of(ym_date))
    } else if (type == "Sale") {
      data <- sales_clean %>%
        select(city, state_id, state_name, lat, lng, all_of(ym_date))
    } else if (type == "Rent") {
      data <- rents_clean %>%
        select(city, state_id, state_name, lat, lng, all_of(ym_date))
    }

    if (!is_all_states) {
      data <- filter(data, state_name == state)
    }

    data
  }

  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat = 39.8283, lng = -98.5795, zoom = 2)
  })

  # observe multiple events
  # https://stackoverflow.com/questions/41960953/how-to-listen-for-more-than-one-event-expression-within-a-shiny-observeevent
  toListen <- reactive({
    list(input$date, input$type, input$state)
  })

  observeEvent(toListen(), {
    leafletProxy("mymap") %>%
    clearMarkers() %>%
    clearShapes() %>%
    addCircleMarkers(
      data = get_map_data(input$date, input$type, input$state),
      label = lapply(
        sprintf(
          ifelse(input$type == "Rent / Sale", "%s: %f<br/>%s, %s", "%s: %d<br/>%s, %s"),
          input$type,
          get_map_data(input$date, input$type, input$state)[, str_sub(as.character(input$date), 1, 7)],
          get_map_data(input$date, input$type, input$state)$city,
          get_map_data(input$date, input$type, input$state)$state_id),
        htmltools::HTML),
      lat = ~lat,
      lng = ~lng)
  })
}

# autoreload for devlepment
options(shiny.autoreload = TRUE)
shinyApp(ui = ui, server = server)
