library('shiny')
library('dplyr')
library('data.table')
library('RCurl')
library('ggplot2')
library('lattice')
library('zoo')
library('lubridate')

DF <- read.csv('DATA_v2.csv', header = TRUE, sep = ',')
DF <- data.table(DF)

shinyServer(function(input, output){
  DT <- reactive({
    DT <- DF[between(Year, input$year.range[1], input$year.range[2]) & Commodity.Code == input$sp.to.plot & Trade.Flow == input$trade.to.plot, ]
    DT <- data.table(DT)
  })
  output$sp.ggplot <- renderPlot({
    gp <- ggplot(data = DT(), aes(x = Netweight..kg., y = Trade.Value..US..))
    gp <- gp + geom_point() + geom_smooth(method = 'lm')
    gp
  })
})
