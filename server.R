#server.R
library(shiny)
library(ggplot2)
shinyServer(function(input, output) {
        output$map <- renderPlot({
                rus <- readRDS("rus.Rds")
                ggplot(data = rus, aes(x = long, y = lat, group = group)) + 
                        geom_polygon(aes(fill = eval(parse(text=input$text))), color = "white") + 
                        coord_map(projection = 'azequidist') +
                        guides(fill=FALSE)# +
                  #      geom_text(aes(x = x, y = y, label = substr(HASC_1,4,5)), colour = "white", size = 3, fontface = 2)
        })})