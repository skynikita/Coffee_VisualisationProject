library(shiny)
library(BH)
require(markdown)
require(data.table)
library(dplyr)
library(DT)
require(devtools)
library(rCharts)
library(plotly)
library(shinythemes)
library(leaflet)
library(RColorBrewer)

shinyUI(
  navbarPage(theme = shinytheme("flatly"),"Coffee In The World", 
             # multi-page user-interface that includes a navigation bar.
             
             tabPanel(p(icon("database"),"Explore the Data"),
                      sidebarPanel(
                        sliderInput("timeline", 
                                    "Timeline:", 
                                    min = 2000,
                                    max = 2018,
                                    value = c(2008, 2018)),
                        selectInput("Attribute","Please choose your Attribute",
                                    c("Production","Domestic_Consumption","Exports","Imports")
                                    
                        ),
                        #format = "####"),
                        actionButton(inputId = "clearAllTop", 
                                     label = "Clear selection", 
                                     icon = icon("square-o")),
                        actionButton(inputId = "selectAllTop",
                                     label = "Select all",
                                     icon = icon("check-square-o")),
                        uiOutput("countriesControl"), # the id
                        actionButton(inputId = "clearAllBottom",
                                     label = "Clear selection",
                                     icon = icon("square-o")),
                        actionButton(inputId = "selectAllBottom",
                                     label = "Select all",
                                     icon = icon("check-square-o"))
                      ),
                      mainPanel(
                        tabsetPanel(
                          # Data 
                          tabPanel(p(icon("table"), "Dataset"),
                                   dataTableOutput(outputId="dTable")
                          ), # end of "Dataset" tab panel
                          
                          #Map
                          tabPanel(p(icon("globe"), "Map"),
                                   mainPanel(
                                     #h4("World map"),
                                     #plotlyOutput("worldMap"),
                                     #tags$style(type = "text/css", "html, body "),
                                     tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                     leafletOutput("myMap",width=500,height = 800),
                                     absolutePanel(top = 10, right = 10,
                                                   sliderInput("range", "Total", min=0, max=900000,
                                                               value = c(10000, 550000), step = 500
                                                   )
                                                   
                                                   
                                     )
                                     
                                   )         
                          ),# end of "Map" tab panel
                          
                          #Coffee by Countries
                          tabPanel(p(icon("bar-chart"), "Coffee by Countries"),
                                   h4('Number of Coffee by Countries', align = "center"),
                                   h5('Please hover over each bar to see more details.',
                                      align ="center"),
                                   plotlyOutput("countriesByYear")
                                   
                          ), # end of "Coffee by Countries" tab panel
      
                          
                          # Coffee by Year 
                          tabPanel(p(icon("line-chart"), "Coffee by Year"),
                                   # h4('Production vs Consumption', align = "center"),
                                  # h5('Please hover over each bubble to see details.', 
                                    #  align ="center"),
                                  # plotlyOutput("coffee_comparison")
                                  h4('Number of Coffee by Year', align = "center"),
                                  # h5('Please hover over each point to see more details.', 
                                  #  align ="center")
                                  plotlyOutput("amountByYear"),
                                  h5('Please hover over each point to see more details.', 
                                     align ="center")
                          ), # end of "Coffee by Year" tab panel
                          
                  
                          # Coffee Comparison 
                          tabPanel(p(icon("asterisk"), "Coffee Comparison"),
                                   h4('Coffee Comparison', align = "center"),
                                   #h5('Please hover over each bubble to see details.', 
                                   #  align ="center"),
                                   plotlyOutput("coffee_comparison"),
                                   h5('Please hover over each bubble to see details.', 
                                      align ="center")
                          ) # end of "Coffee Comparison" tab panel
                          
                          
                          
                        )
                        
                      )     
             ), # end of "Explore Dataset" tab panel
             
             
             tabPanel(p(icon("book"),"About"),
                      mainPanel(
                        includeMarkdown("about.md")
                      )
             ) # end of "About" tab panel
             
             
             )  
  )
