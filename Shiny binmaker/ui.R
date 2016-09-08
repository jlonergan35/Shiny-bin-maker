library(shiny)
library(rhandsontable)
library(lubridate)
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(zoo)
library(xts)
library(xlsx)

ui <- fluidPage(
        titlePanel("TMY 3 Bin Generator"),
        
        sidebarLayout(
                sidebarPanel(
                        numericInput(inputId = "station",
                                     label = "Station ID",
                                     value = 690150),
                        htmlOutput("mySite"),
                        numericInput(inputId = "bin_size",
                                     label = "Bin Size",
                                     value = 5),
                        numericInput(inputId = "n",
                                     label = "number of scheduled periods",
                                     value = 1),
                        numericInput(inputId = "cp",
                                     label = "degree day change  point",
                                     value = 65),
                        #add link to meta data here (use a() function?)
                        actionButton(inputId = "clicks",
                                     label = "Click to Run"),
                        downloadButton('downloadData', 'Download bin Data'),
                        downloadButton('downloadData2', 'Download DD Data'),
                        downloadButton('downloadData3', 'Download Daily DD Data')
                ),    
                
                mainPanel(  
                        textOutput(outputId = "inst", container = div),
                        rHandsontableOutput('hot'),
                        tableOutput('table'),
                        tableOutput("table2"),
                        plotOutput(outputId = "data"),
                        plotOutput(outputId = "DDdata"),
                        plotOutput(outputId = "DailyDDdata")
                )
        ))