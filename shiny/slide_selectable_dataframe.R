# data summary

# if (!require("devtools")) {
#   install.packages("devtools")
# }
# devtools::install_github("joachim-gassen/ExPanDaR")
# library(ExPanDaR)
# ExPanD()
library(shiny)
library(plotly)
# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Reactivity"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      textInput(inputId = "caption",
                label = "Caption:",
                value = "Data Summary"),
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("rock", "pressure", "cars")),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),

      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  output$caption <- renderText({
    input$caption
  })
  
 
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
 
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  output$plot <- renderPlot({
    plot_ly(
      type = "scatter",
      x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
      y = c(28.8, 28.5, 37, 56.8, 69.7, 79.7, 78.5, 77.8, 74.1, 62.6, 45.3, 39.9),
      mode = "markers+lines") %>%
        layout(xaxis = list(
          dtick = 0.75,  # interval
          tick0 = 0.5,  # start point
          tickmode = "linear"
        ))
      
    
  })
}

# Create Shiny app ----
shinyApp(ui, server)


