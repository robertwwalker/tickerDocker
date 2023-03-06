library(shiny)
library(tidyverse)
library(tidyquant)
library(plotly)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Tickers and Beta"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      textInput(inputId = "ticker",
                label = "Ticker",
                value = "F"),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      plotlyOutput("main_plot", height="500px")
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  # Create caption ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display
  #
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes
  output$caption <- renderText({
    input$ticker
  })
  
  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  
  library(tidyquant)
  output$summary <- renderPrint({
    DatG <- tq_get("^GSPC", from="2017-10-01", to="2022-11-01") %>%
      tq_transmute(., mutate_fun = monthlyReturn)
    names(DatG) <- c("date","SandP500")
    Data <- tq_get(c(input$ticker), from="2017-10-01", to="2022-11-01") %>%
      tq_transmute(., mutate_fun = monthlyReturn) %>% left_join(., DatG) %>% filter(date > as.Date("2017-11-01"))
    res <- lm(monthly.returns~SandP500, data=Data)
    summary(res)
  })
  output$main_plot <- renderPlotly({
    DatG <- tq_get("^GSPC", from="2017-10-01", to="2022-11-01") %>%
      tq_transmute(., mutate_fun = monthlyReturn)
    names(DatG) <- c("date","SandP500")
    Data <- tq_get(c(input$ticker), from="2017-10-01", to="2022-11-01") %>%
      tq_transmute(., mutate_fun = monthlyReturn) %>% left_join(., DatG) %>% filter(date > as.Date("2017-11-01"))
    df <- data.frame(tit=paste0("CAPM: ",input$ticker, sep=""))
    p1 <- ggplot(Data,
           aes(x=SandP500, y=monthly.returns)) + geom_point() + geom_smooth(method="lm") + labs(title=df$tit, x="S&P 500 Monthly Returns", y=input$ticker)
    ggplotly(p1)
  })
}

# Create Shiny app ----
shinyApp(ui, server)