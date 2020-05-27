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
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(
          id = "controls",
          top = 62,
          right = 12,
          width = 336,
          height = "auto",
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
          ),
          htmlOutput(outputId = "top")
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

  output$map <- renderLeaflet({
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
    date <- str_sub(as.character(input$date), 1, 7)
    map_data <- na.omit(get_map_data(input$date, input$type, input$state))
    binpal <- colorBin("Reds", map_data[, date], bins = 7)
    
    result <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes()
    
    if (nrow(map_data) > 0) {
      result %>%
        addCircleMarkers(
          data = map_data,
          label = lapply(
            sprintf(
              ifelse(input$type == "Rent / Sale", "%s: %f<br/>%s, %s", "%s: %d<br/>%s, %s"),
              input$type,
              map_data[, date],
              map_data$city,
              map_data$state_id),
            htmltools::HTML),
          lat = ~lat,
          lng = ~lng,
          fillOpacity = 0.7,
          color = ~binpal(map_data[, date]),
          stroke = FALSE)
    }
  })

  output$top <- renderUI({
    data <- na.omit(get_map_data(input$date, input$type, input$state))
    num <- nrow(data)
    top_num <- ifelse(num > 10, 10, num)
    title <- paste("Top ", top_num, ":")
    
    if (top_num == 0) {
      return(HTML(""))
    }
    
    date <- str_sub(as.character(input$date), 1, 7)
    top_data <- top_n(data[order(data[date], decreasing = TRUE), ], top_num)
    
    top_list <- c()
    for (row in 1:nrow(top_data)) {
      location <- paste0(top_data[row, "city"], ", ", top_data[row, "state_id"])
      value <- paste0(top_data[row, date])
      list_item <- paste0(location, ": ", value, "<br/>")
      top_list <- c(top_list, list_item)
    }

    HTML(paste(title, "<br/>", paste(top_list, collapse = "\n")))
  })
}

# autoreload for devlepment
options(shiny.autoreload = TRUE)
shinyApp(ui = ui, server = server)
