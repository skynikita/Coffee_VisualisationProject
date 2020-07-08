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

# Read data
data= read.csv("psd_coffee 3.csv")
Country_Name_d <- sort(unique(data$Country_Name))

search_by_year = function(df, min, max){
  result = df[which((df$Market_Year<=max) & (df$Market_Year>=min)),]
  return(result)
}

search_by_attribute_country= function(df, attribute="all"){
  if (attribute == 'Production' || attribute == 'Domestic_Consumption'){
    result= subset(df, select = c('Country_Name', 'Production', 'Domestic_Consumption'))
  } else if (attribute == 'all'){
    result= subset(df, select = c('Country_Name', 'Production', 'Domestic_Consumption', 'Imports', 'Exports'))
  } else {
    result= subset(df, select = c('Country_Name', 'Imports', 'Exports'))
  }
  return(result)
}

search_by_attribute_year= function(df, attribute){
  result= subset(df, select = c('Market_Year', attribute))
  return(result)
}

group_by_country = function(df, attribute){
    df %>% 
      group_by(Country_Name) %>% 
      summarise(Production = sum(Production),
                Domestic_Consumption = sum(Domestic_Consumption),
                Exports = sum(Exports),
                Imports = sum(Imports),
                Arabica_Production = sum(Arabica_Production),
                Bean_Exports = sum(Bean_Exports),
                Bean_Imports = sum(Bean_Imports),
                Other_Production = sum(Other_Production),
                Roast_Ground_Exports = sum(Roast_Ground_Exports),
                Roast_Ground_Imports = sum(Roast_Ground_Imports),
                Robusta_Production = sum(Robusta_Production),
                Rst_Ground_Dom.Consum = sum(Rst_Ground_Dom.Consum),
                Soluble_Dom.Consum = sum(Soluble_Dom.Consum),
                Soluble_Exports = sum(Soluble_Exports),
                Country_Code = Country_Code[1],
                Latitude = mean(Latitude),
                Longitude = mean(Longitude)
                )
}

group_by_year = function(df, attribute){
  df %>% 
    group_by(Market_Year) %>% 
    summarise(Production = sum(Production),
              Domestic_Consumption = sum(Domestic_Consumption),
              Exports = sum(Exports),
              Imports = sum(Imports),
              Arabica_Production = sum(Arabica_Production),
              Bean_Exports = sum(Bean_Exports),
              Bean_Imports = sum(Bean_Imports),
              Other_Production = sum(Other_Production),
              Roast_Ground_Exports = sum(Roast_Ground_Exports),
              Roast_Ground_Imports = sum(Roast_Ground_Imports),
              Robusta_Production = sum(Robusta_Production),
              Rst_Ground_Dom.Consum = sum(Rst_Ground_Dom.Consum),
              Soluble_Dom.Consum = sum(Soluble_Dom.Consum),
              Soluble_Exports = sum(Soluble_Exports))
}

filter_by_country = function(df, country_list){
  result = df[df$Country_Name%in% country_list, ]
  return(result)
}

total_each_year = function (df, min_year, max_year, attribute, country_list){
  a = search_by_year(df, min_year, max_year)
  b = filter_by_country(a, country_list)
  c = group_by_year(b,attribute)
  #d = search_by_attribute_year(c, attribute)
  return(c)
}

total_each_country = function (df, min_year, max_year, attribute, country_list){
  a = search_by_year(df, min_year, max_year)
  b = filter_by_country(a, country_list)
  c = group_by_country(b, attribute)
  #d = search_by_attribute_country(c, attribute)
  return(c)
}

data_table = function(df, min_year, max_year, attribute, country_list){
  a = search_by_year(df, min_year, max_year)
  b = filter_by_country(a, country_list)
  c = group_by_country(b, attribute)
  c = search_by_attribute_country(c)
  return(c)
}



plot_map = function(dt, attribute){
  # light grey boundaries
  l <- list(color = toRGB("grey"), width = 0.5)
  
  # specify map projection/options
  g <- list(
    showframe = TRUE,
    showcoastlines = TRUE,
    projection = list(type = 'Mercator')
  )
  
  p <- plot_geo(dt) %>%
    colorbar(title = 'Production', tickprefix = '') %>%
    layout(
      title = 'Global Coffee Production',
      geo = g
    ) %>%
    add_trace(
      z = ~Production, color = ~Production, colors = 'Blues',
      text = ~Country_Name, locations = ~Country_Code, marker = list(line = l)
    )
  p
}

plot_bar_chart = function(dt, attribute){
  if(attribute=='Production'){
    dt=dt[order(-dt$Production),][1:25,]
    print(dt)
    p = plot_ly(dt, type = 'bar', orientation = 'h') %>% 
      add_trace(y = ~Country_Name, x = ~Arabica_Production, name = 'Arabica Production') %>% 
      add_trace(y = ~Country_Name, x = ~Robusta_Production, name = 'Robusta Production') %>% 
      add_trace(y = ~Country_Name, x = ~Other_Production, name = 'Other Production')%>%
      layout(xaxis = list(title = 'Production'), barmode = 'stack', autosize = F, yaxis = list(automargin = TRUE))
  } else if(attribute=='Domestic_Consumption'){
    dt=dt[order(-dt$Domestic_Consumption),][1:25,]
    p = plot_ly(dt, type = 'bar', orientation = 'h') %>% 
      add_trace(y = ~Country_Name, x = ~Rst_Ground_Dom.Consum, name = 'Roast & Ground') %>% 
      add_trace(y = ~Country_Name, x = ~Soluble_Dom.Consum, name = 'Soluble') %>% 
      layout(xaxis = list(title = 'Consumption'), barmode = 'stack', autosize = F, yaxis = list(automargin = TRUE))
  }else if(attribute=='Imports'){
    dt=dt[order(-dt$Imports),][1:25,]
    p = plot_ly(dt, type = 'bar', orientation = 'h') %>% 
      add_trace(y = ~Country_Name, x = ~Bean_Imports, name = 'Bean') %>% 
      add_trace(y = ~Country_Name, x = ~Roast_Ground_Imports, name = 'Roast & Ground') %>% 
      layout(xaxis = list(title = 'Imports'), barmode = 'stack', autosize = F, yaxis = list(automargin = TRUE))
  }else if(attribute=='Exports'){
    dt=dt[order(-dt$Exports),][1:25,]
    p = plot_ly(dt, type = 'bar', orientation = 'h') %>% 
      add_trace(y = ~Country_Name, x = ~Bean_Exports, name = 'Bean') %>% 
      add_trace(y = ~Country_Name, x = ~Roast_Ground_Exports, name = 'Roast & Ground') %>% 
      layout(xaxis = list(title = 'Imports'), barmode = 'stack', autosize = F, yaxis = list(automargin = TRUE))
  }
  
  p
}

plot_total_each_year1 <- function(dt, attribute){
  if(attribute=='Production'){
    p = plot_ly(dt, fill = 'tozeroy', line = list(color = '#00000000')) %>% 
      add_trace(x = ~Market_Year, y = ~Production, name = 'Total Production', 
                type = 'scatter', mode = 'lines'
                #,  fillcolor = 'green'
                ) %>% 
      add_trace(x = ~Market_Year, y = ~Arabica_Production, name = 'Arabica Production', 
                type = 'scatter', mode = 'lines',  fill = 'tozeroy'
                #, fillcolor = 'blue'
                ) %>% 
      add_trace(x = ~Market_Year, y = ~Robusta_Production, name = 'Robusta Production', 
                type = 'scatter', mode = 'lines',  fill = 'tozeroy'
                #, fillcolor = 'orange'
                ) %>% 
      add_trace(x = ~Market_Year, y = ~Other_Production, name = 'Other Production', 
                type = 'scatter', mode = 'lines',  fill = 'tozeroy'
                #, fillcolor = 'red'
                )
    } else if(attribute=='Domestic_Consumption'){
      p = plot_ly(dt, fill = 'tozeroy', line = list(color = '#00000000')) %>% 
        add_trace(x = ~Market_Year, y = ~Domestic_Consumption, name = 'Total Consumption', 
                  type = 'scatter', mode = 'lines'
                  #,  fillcolor = 'green'
                  ) %>% 
        add_trace(x = ~Market_Year, y = ~Rst_Ground_Dom.Consum, name = 'Roast & Ground', 
                  type = 'scatter', mode = 'lines',  fill = 'tozeroy') %>% 
        add_trace(x = ~Market_Year, y = ~Soluble_Dom.Consum, name = 'Soluble', 
                  type = 'scatter', mode = 'lines',  fill = 'tozeroy')
    }else if(attribute=='Imports'){
      p = plot_ly(dt, fill = 'tozeroy', line = list(color = '#00000000')) %>% 
        add_trace(x = ~Market_Year, y = ~Imports, name = 'Total Imports', 
                  type = 'scatter', mode = 'lines') %>% 
        add_trace(x = ~Market_Year, y = ~Bean_Imports, name = 'Bean Imports', 
                  type = 'scatter', mode = 'lines',  fill = 'tozeroy') %>% 
        add_trace(x = ~Market_Year, y = ~Roast_Ground_Imports, name = 'Roast & Ground', 
                  type = 'scatter', mode = 'lines',  fill = 'tozeroy')
    }else if(attribute=='Exports'){
      p = plot_ly(dt, fill = 'tozeroy', line = list(color = '#00000000')) %>% 
        add_trace(x = ~Market_Year, y = ~Exports, name = 'Total Exports', 
                  type = 'scatter', mode = 'lines') %>% 
        add_trace(x = ~Market_Year, y = ~Bean_Exports, name = 'Bean Exports', 
                  type = 'scatter', mode = 'lines',  fill = 'tozeroy') %>% 
        add_trace(x = ~Market_Year, y = ~Roast_Ground_Exports, name = 'Roast & Ground', 
                  type = 'scatter', mode = 'lines',  fill = 'tozeroy')
    }

    p
}

plot_total_each_country = function(dt, attribute){
  if (attribute == 'Production' || attribute == 'Domestic_Consumption'){
    gap = c(abs(dt$Domestic_Consumption -dt$Production)/(dt$Domestic_Consumption + dt$Production))*20
    size = c(sqrt(sqrt(abs(dt$Domestic_Consumption -dt$Production))))
    dt$Gap = gap
    dt$Size = size
    p <- plot_ly(dt, x = ~Production, y = ~Domestic_Consumption, text = ~Country_Name, color = ~Gap, type = 'scatter', mode = 'markers',
               marker = list(size = ~Size)) %>%
    layout(title = 'Production vs Consumption',
           xaxis = list(showgrid = FALSE),
           yaxis = list(showgrid = FALSE))
  p
  }
  else{
    
    
    gap = c(abs(dt$Imports -dt$Exports)/(dt$Imports + dt$Exports))*20
    size = c(sqrt(sqrt(abs(dt$Imports -dt$Exports))))
    dt$Gap = gap
    dt$Size = size
    p <- plot_ly(dt, x = ~Imports, y = ~Exports, text = ~Country_Name, color = ~Gap, type = 'scatter', mode = 'markers',
                 marker = list(size = ~Size)) %>%
      layout(title = 'Exports vs Imports',
             xaxis = list(showgrid = FALSE),
             yaxis = list(showgrid = FALSE))
    p  
  }

}


