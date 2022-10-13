#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
interfaz <- fluidPage(
  
  # Application title
  titlePanel("titulo de la grafica  Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs",
                  "Numbero de observaciones:",
                  min = 10,
                  max = 2000,
                  value = 100)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("grafiquita")
    )
  )
)

# Define server logic required to draw a histogram
source("~/Documents/GitHub/Macroeconometria/2022-2/Funciones/ACFfun0.R")
back <- function(input, output) {
  output$grafiquita <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- rnorm(n=input$obs)
    barritas <- floor(3/50*input$obs)+10
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = barritas, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = interfaz, server = back)


