#ui.R
library(shiny)
library(ggplot2)
shinyUI(fluidPage(titlePanel("Basic widgets"),
                  fluidRow(column(3, textInput("text", label = h3("Text input"), value = "Enter text..."))),
                  titlePanel("censusVis"),
                  mainPanel(plotOutput("map"))))