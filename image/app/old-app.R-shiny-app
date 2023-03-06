#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(shinydashboard)
library(tidyverse) 
library(magrittr)
library(utf8)
library(shiny)
library(DT)
library(lubridate)
library(hrbrthemes)
# load the data
load("data/archigos.rda")
# create the set of countries
Country.Select <- Archigos %$% table(idacr) %>% data.frame() %>% mutate(Country = idacr) %>% select(Country)
# plot for durations
Plot.Durations <- function(data, state) {
  data %>% ggplot(., aes(x=tenureY)) + geom_histogram() + theme_ipsum_rc() + labs(x="Durations", title=paste0("Durations: ",state))
}
# plot for chronology
Plot.Chronology <- function(data, state) {
  data %>% arrange(eindate) %>% 
    ggplot(., aes(x=fct_reorder(leader, eindate), color=leader)) + geom_errorbar(aes(ymin=eindate, ymax=eoutdate)) + coord_flip() + labs(x="", title=paste0("Leader Chronology: ",state)) + theme_ipsum_rc() + scale_color_viridis_d(option = "E") + guides(color=FALSE)
}

header <- dashboardHeader(title = "Archigos")
sidebar <-  dashboardSidebar(
  sidebarMenu(selectInput(inputId = "Country", label="Country:", choices = Country.Select$Country, selected="AFG"))
)
body <- dashboardBody(
  tabsetPanel(
    tabItem(tabName = "dashb1",
            title="Chronology",
            # Boxes need to be put in a row (or column)
            fluidRow(box(plotOutput("plotDur"), width=12))
    ),
    tabItem(tabName = "dashb2",
            title="Durations",
            fluidRow(box(plotOutput("plotChr"), width=12))
    )),
  fluidRow(DTOutput("plotDT"))
)
ui <- dashboardPage(skin = "purple", header, sidebar, body)

server <- function(input, output) {
  dataset <- reactive({
    Archigos %>% 
      filter(idacr==input$Country) %>% 
      arrange(desc(eoutdate))
  })
  output$plotDT <- renderDT({  dataset()}, options = list(scrollX = TRUE) 
  )
  output$plotDur <- renderPlot({
    Plot.Chronology(dataset(), input$country)
  })
  output$plotChr <- renderPlot({
    Plot.Durations(dataset(), input$country)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
