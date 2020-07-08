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

# Load data processing file
source("data_processing.R")
Country_Name <- sort(unique(data$Country_Name))

# Shiny server
shinyServer(
  function(input, output, session) {
   
    
    
    # Initialize reactive values
    values <- reactiveValues()
    values$Country_Name <- Country_Name

    
    observe({
        if(input$selectAllTop > 0) {
            updateCheckboxGroupInput(session=session, inputId="Country_Name", 
                                     choices=Country_Name, selected=Country_Name)
            values$Country_Name <- Country_Name
        }
    })
    observe({
        if(input$selectAllBottom > 0) {
            updateCheckboxGroupInput(session=session, inputId="Country_Name", 
                                     choices=Country_Name, selected=Country_Name)
            values$Country_Name <- Country_Name
        }
    })

    
    observe({
        if(input$clearAllTop > 0) {
            updateCheckboxGroupInput(session=session, inputId="Country_Name", 
                                     choices=Country_Name, selected=NULL)
            values$Country_Name <- c()
        }
    })
    observe({
        if(input$clearAllBottom > 0) {
            updateCheckboxGroupInput(session=session, inputId="Country_Name", 
                                     choices=Country_Name, selected=NULL)
            values$Country_Name <- c()
        }
    })
    
    #print(values$Country_Name)
    
    
    # Create event type checkbox
    output$countriesControl <- renderUI({
        checkboxGroupInput('Country_Name1', 'Country_Name:', 
                           Country_Name, selected = values$Country_Name)
    })
    
    # Prepare dataset
    
    dataTable = reactive({
      data_table(data, input$timeline[1], 
             input$timeline[2], input$Attribute, input$Country_Name1)
      })
    
    year_dataTable <- reactive({
      total_each_year(data, input$timeline[1], 
                    input$timeline[2], input$Attribute, input$Country_Name1)
    })
    
    country_dataTable = reactive({
      dt = total_each_country(data, input$timeline[1], 
                      input$timeline[2], input$Attribute, input$Country_Name1)
      })
    
    # Render data table
    output$dTable <- renderDataTable({
      #country_dataTable()
      dataTable()
    } 
    )
    output$amountByYear <- renderPlotly({
      plot_total_each_year1(year_dataTable(), input$Attribute)
    })
    
    output$coffee_comparison <- renderPlotly({
      plot_total_each_country(country_dataTable(), input$Attribute)
    })
    
    output$countriesByYear <- renderPlotly({
      plot_bar_chart(country_dataTable(), input$Attribute)
    })
    

    filteredData <- reactive({
      if (input$Attribute == 'Production') {
        country_dataTable()[country_dataTable()$Production >= input$range[1] &
                              country_dataTable()$Production <= input$range[2], ]
      } else if (input$Attribute == 'Domestic_Consumption') {
        country_dataTable()[country_dataTable()$Domestic_Consumption >= input$range[1] &
                              country_dataTable()$Domestic_Consumption <= input$range[2], ]
      } else if (input$Attribute == 'Imports') {
        country_dataTable()[country_dataTable()$Imports >= input$range[1] &
                              country_dataTable()$Imports <= input$range[2], ]
      } else if (input$Attribute == 'Exports') {
        country_dataTable()[country_dataTable()$Exports >= input$range[1] &
                              country_dataTable()$Exports <= input$range[2], ]
      }
    })
    
    colorpal <- reactive({
      if (input$Attribute=='Production'){
        colorNumeric("RdYlBu", country_dataTable()$Production)}
      else if (input$Attribute=='Domestic_Consumption'){
        colorNumeric("RdYlBu", country_dataTable()$Domestic_Consumption)
      }else if (input$Attribute=='Imports'){
        colorNumeric("RdYlBu", country_dataTable()$Imports)
      }else if (input$Attribute=='Exports'){
        colorNumeric("RdYlBu", country_dataTable()$Exports)
      }
    })
    
    output$myMap <- renderLeaflet({
      # Use leaflet() here, and only include aspects of the map that
      # entire map is being torn down and recreated).
      
      if (input$Attribute == 'Production') {
        pal = colorNumeric("RdYlBu", filteredData()$Production)
        p = leaflet(filteredData()) %>% addTiles() %>%
          fitBounds( ~ -102,
                     ~ -41,
                     ~ 174,
                     ~ 62) %>%
          addCircles(
            radius = ~ (sqrt(sqrt(Production))) * 50000,
            weight = 1,
            color = "#777777",
            fillColor = ~ pal(Production),
            fillOpacity = 0.7,
            label = paste("Country:", filteredData()$Country_Name,";",
                         "Production:", filteredData()$Production)
            
          )
      } else if (input$Attribute == 'Domestic_Consumption') {
        pal = colorNumeric("RdYlBu", filteredData()$Domestic_Consumption)
        p = leaflet(filteredData()) %>% addTiles() %>%
          fitBounds( ~ -102,
                     ~ -41,
                     ~ 174,
                     ~ 62) %>%
          addCircles(
            radius = ~ (sqrt(sqrt(Domestic_Consumption))) * 50000,
            weight = 1,
            color = "#777777",
            fillColor = ~ pal(Domestic_Consumption),
            fillOpacity = 0.7,
            label = paste("Country:", filteredData()$Country_Name,";",
                          "Consumption:", filteredData()$Domestic_Consumption)
            
          )
      } else if (input$Attribute == 'Imports') {
        pal = colorNumeric("RdYlBu", filteredData()$Imports)
        p = leaflet(filteredData()) %>% addTiles() %>%
          fitBounds( ~ -102,
                     ~ -41,
                     ~ 174,
                     ~ 62) %>%
          addCircles(
            radius = ~ (sqrt(sqrt(Imports))) * 50000,
            weight = 1,
            color = "#777777",
            fillColor = ~ pal(Imports),
            fillOpacity = 0.7,
            label = paste("Country:", filteredData()$Country_Name,";",
                          "Imports:", filteredData()$Imports)
            
          )
      } else if (input$Attribute == 'Exports') {
        pal = colorNumeric("RdYlBu", filteredData()$Exports)
        p = leaflet(filteredData()) %>% addTiles() %>%
          fitBounds( ~ -102,
                     ~ -41,
                     ~ 174,
                     ~ 62) %>%
          addCircles(
            radius = ~ (sqrt(sqrt(Exports))) * 50000,
            weight = 1,
            color = "#777777",
            fillColor = ~ pal(Exports),
            fillOpacity = 0.7,
            label = paste("Country:", filteredData()$Country_Name,";",
                          "Exports:", filteredData()$Exports)
            
          )
      }
      
      p
    })
    
    observe({
      if (input$Attribute=='Production'){
        pal = colorNumeric("RdYlBu", filteredData()$Production)
        leafletProxy("myMap", data = filteredData()) %>%
          clearShapes() %>%
          addCircles(radius = ~(sqrt(sqrt(Production)))*50000, weight = 1, color = "#777777",
                     fillColor = ~pal(Production), fillOpacity = 0.7, label = paste("Country:", filteredData()$Country_Name,";",
                                                                                    "Production:", filteredData()$Production)
          )} else if (input$Attribute=='Domestic_Consumption'){
            pal = colorNumeric("RdYlBu", filteredData()$Domestic_Consumption)
            leafletProxy("myMap", data = filteredData()) %>%
              clearShapes() %>%
              addCircles(radius = ~(sqrt(sqrt(Domestic_Consumption)))*50000, weight = 1, color = "#777777",
                         fillColor = ~pal(Domestic_Consumption), fillOpacity = 0.7, label = paste("Country:", filteredData()$Country_Name,";",
                                                                                                  "Consumption:", filteredData()$Domestic_Consumption)
              )
          }else if (input$Attribute=='Imports'){
            pal = colorNumeric("RdYlBu", filteredData()$Imports)
            leafletProxy("myMap", data = filteredData()) %>%
              clearShapes() %>%
              addCircles(radius = ~(sqrt(sqrt(Imports)))*50000, weight = 1, color = "#777777",
                         fillColor = ~pal(Imports), fillOpacity = 0.7, label = paste("Country:", filteredData()$Country_Name,";",
                                                                                     "Imports:", filteredData()$Imports)
              )
          }else if (input$Attribute=='Exports'){
            pal = colorNumeric("RdYlBu", filteredData()$Exports)
            leafletProxy("myMap", data = filteredData()) %>%
              clearShapes() %>%
              addCircles(radius = ~(sqrt(sqrt(Exports)))*50000, weight = 1, color = "#777777",
                         fillColor = ~pal(Exports), fillOpacity = 0.7, label = paste("Country:", filteredData()$Country_Name,";",
                                                                                     "Exports:", filteredData()$Exports)
              )
          }
    })
    
    
    
    
    
  } # end of function(input, output)
)