#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggthemes)
library(gridExtra)
# Define UI for application that draws a histogram
interfaz <- fluidPage(
  
  # Application title
  titlePanel("titulo de la grafica  Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("datos",
                  "cantidad de datos:",
                  min = 10,
                  max = 1000,
                  value = 511),
      sliderInput("constante",
                  "Valor de la media:",
                  min = 10,
                  max = 100,
                  value = 50),      
      sliderInput("phi1",
                  "Valor de phi1:",
                   min = -1,
                   max = 1,
                   value = -0.5 ,step = 0.01),
      sliderInput("phi2",
                  "Valor de phi2:",
                  min = -1,
                  max = 1,
                  value = 0.0325 ,step = 0.01),
      sliderInput("phi3",
                  "Valor de phi3:",
                  min = -1,
                  max = 1,
                  value = 0.0382 ,step = 0.01),
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
    set.seed(123)
    a <- rnorm(n=input$datos)
    N <- length(a)
    phi1 <- input$phi1
    phi2 <- input$phi2     #-0.01
    phi3 <- input$phi3           #0.01
    m <- input$constante/(1-phi1-phi2-phi3)
    Z <- matrix(data = NA,nrow = N,ncol = 1)
    Z[1] <-  0  # 0
    Z[2] <-  -2  #1
    Z[3] <-  3  #2
    for(i in 4:N){
      Z[i] <- phi1*(Z[i-1]-m)+phi2*(Z[i-2]-m) +phi3*(Z[i-3]-m) +a[i]#SE ELIMINA CONTSTANTE
    }
    Z1<- Z[-c(1:3)]
    y <- c(1:length(Z1))
    
    Z1 <- as.data.frame(cbind(y,Z1))
    
    # draw the histogram with the specified number of bins
    #plot(Z1,type='l')

    
    
    source("~/Documents/GitHub/Macroeconometria/2022-2/Funciones/ACFfun0.R")
    ACF <- ACFfun0(Z,N/4)
    quantil <- qnorm(0.975,mean=0,sd=1)
    quantil <- round(quantil,2) 
    lim_inf <- -quantil*sqrt(1/N)
    lim_sup <- quantil*sqrt(1/N)
    

seriep <-     ggplot(Z1) +
      geom_line(aes(y=Z1,x=y), colour= "blue",size=1) +
      ggtitle("                     Serie Proceso Generador AR3") +
      labs(x="Periodo de Tiempo",y="Values" )+
      theme_economist()+theme(axis.text = element_text(angle=0))
    #plot(ACF,type = 'h',ylim = c(-0.60, 1))
    
    
    y1 <- c(1:length(ACF))
    ACF1 <- as.data.frame(cbind(y1,ACF))
acfp <-     ggplot(ACF1) +
      geom_col(aes(y=ACF,x=y1), colour= "blue",size=0.4) +
      geom_hline(aes(yintercept=lim_sup),linetype="dashed",color="black")+
      geom_hline(aes(yintercept=lim_inf),linetype="dashed",color="black")+
      ggtitle("                     ACF Proceso Generador AR3") +
      labs(x="Periodo de Tiempo",y="Values" )+
      theme_economist()+theme(axis.text = element_text(angle=0))




grid.arrange(seriep,acfp)

  })
}

# Run the application 
shinyApp(ui = interfaz, server = back)



