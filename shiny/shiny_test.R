library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for miles per gallon application
bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  #--------------------------------------------
    sidebarLayout(
    sidebarPanel(sliderInput("priceInput", "Price", min = 0, max = 1000,
                               value = c(5, 200), # defaults shwon
                             pre = "$"),
                 radioButtons("typeInput",  # item?
                              "Product type",
                              choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                              selected = "SPIRITS"),
                 # checkboxInput("choice A", label = "A"),
                 # selectInput("countryInput", "Country",
                 #             choices = c("CANADA", "FRANCE", "ITALY")),
                 uiOutput("countryOutput")
                 
                 ),
    mainPanel(plotOutput("coolplot"),
              br(), br(),
              tableOutput("results"))
  )
)
 

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CHINA")
  }) 
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })
  
  output$results <- renderTable({
    filtered()
  })
}
# 
shinyApp(ui = ui, server = server)
# 
 


