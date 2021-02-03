#load libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(DT)
library(timevis)
library(jsonlite)
library(leaflet)
library(stringi)

#Build using R 4.0.0

#Load nlp model outputs
events <- read.csv("./model_output2.csv")

ui <- fluidPage(
  # Application title
  titlePanel("Key News Extraction"),
  br(), 
  div('Data was filtered to days where article volume was higher than 3 standard
      deviations from the mean over all article volume.'),
  br(), 
  #Timeline
  fluidRow(div(timevisOutput("timeline"), style = 'heigh: 300px')),
  
  box(width = 6, title = "Top Events",
      style = "border: .7px solid LightGray; margin: 0px; padding: 20px;",
      fluidRow(
        #Best news dataframe
        div(dataTableOutput("news_df"), style = "font-size: 90%; width: 100%;")
      )
  ),
  box(width = 6,
      title = "Top Locations",
      style = "border: .7px solid LightGray; margin: 0px; padding: 20px;",
      fluidRow(
        #leaflet map
        leafletOutput("locationsPlot")
      )
  ),
  br()
  

)

server <- function(input, output, session) {
  output$timeline <- renderTimevis({
    #Summarize events per day
    event_days <- events %>%
      group_by(day) %>%
      summarise(count = n())
    
    #Create timevis dataframe
    to_disp <- event_days %>%
      mutate(id = day,
             content = paste0('View ', count, ' event(s) from ', day),
             start = day,
             end = NA)
    
    timevis(to_disp , options = list(
      # zoomin =  60 * 60 * 24, #one day 
      #Limit the range of the visualization
      min = as.character(min(as.Date(events$day))-100),
      max = as.character(max(as.Date(events$day))+100))) %>%
      #Select the 1st event as the deafault selection
      setSelection(as.character(min(as.Date(events$day))))
  })
  
  #Reactive news df to display based on timeline selection
  output$news_df <- renderDataTable({
    req(input$timeline_selected)
    daily <- events %>%
      mutate(time = as.character(as.POSIXct(time,format="%Y-%m-%dT%H:%M:%OS"))) %>% 
      filter(day == input$timeline_selected) %>%
      select(Time = time, Event = sent)
    
    
    daily
    
  }, options = list(searching = F, paging = F, info = F), rownames= FALSE)
  
  
  #Gather all location's longitude and latitude basesd on selection
  loc_df <- reactive({

    #Filter to selection
    to_map <- events %>%
      filter(day == input$timeline_selected) 

    lon <- c(NA)
    lat <- c(NA)

    #For every row
    for(i in 1:nrow(to_map)) {

      str_i <- to_map[i,]$locations_dec

      #Split jsons 
      split_locs = strsplit(str_i, '},')

      #loop through list of jsons
      for (j in split_locs[[1]]) {

        #If it's not the last one (which was not split) , close the json bracket
        if (stri_sub(j, -1) != '}') {
          loc <- paste0(j, '}')
        } else {
          loc <- j
        }

        #Clean up R export replacing NaN with NA
        clean_loc = gsub("NaN", '\"NA\"', loc)

        #Read json
        parsed <- jsonlite::fromJSON(clean_loc)
        
        #Extract longitude and latitudes
        lon <- c(lon, parsed$longitude)
        lat <- c(lat, parsed$latitude)


      }

    }
  
    #Create output dataframe
    final <- data.frame(lat, lon) %>%
      mutate(lat = as.numeric(lat),
             lon = as.numeric(lon))

    #Remove locations that did not have longitude or latitude
    final <- final[complete.cases(final), ]

  })

  #Render leaflet map
  output$locationsPlot <- renderLeaflet({
    req(loc_df())

    map <- leaflet() %>%
      addTiles() %>%
      addMarkers(lng = loc_df()$lon, lat = loc_df()$lat)


  })

  
}

shinyApp(ui = ui, server = server)