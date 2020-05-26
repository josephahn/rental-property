library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)

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
    tabPanel("Map", icon = icon("globe-americas")),
    tabPanel("Data", icon = icon("table")),
    tabPanel("About", icon = icon("info-circle"))
  )
)

server <- function(input, output, session) {

}

shinyApp(ui = ui, server = server)
