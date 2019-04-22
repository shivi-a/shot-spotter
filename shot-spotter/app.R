
# Load in neccessary dependencies

library(shiny)
library(tidyverse)
library(leaflet)
library(readr)

# Read in Camden shotspotter data from rds file created from R script in the
# repo - this way data is processed separately, and then simply displayed in the
# Shiny App

shot <- read_rds("shot_file.rds")

# Define UI for application that has an interactive Leaflet map of Camden
# displaying gunshot data - with a play button that can be pressed to
# automatically cycle through the month, creating an animation

ui <- fluidPage(
   
   # Application title
  
   titlePanel("Gunshots in Camden City, NJ", "Data"),
   
   # Add line break for Shiny App aesthetics
   
   br(),
     
   # Sidebar with input options for year, month, and type of gunshot 
   
   sidebarLayout(
      sidebarPanel(
        
        # Radio button selector for year so that viewers can choose one year at
        # a time. 2018 is not allowed as an option because that data is very
        # small and looks to be an artifact of the dataset - has an unusual
        # uniform distribution
        
        radioButtons("Year", 
                     "Year:", 
                     c(2013, 2014, 2015, 2016, 2017),
                     selected = 2013),
        
        # Slider input to choose the month to view so that viewers can toggle
        # between the month of that particular year. The data starts at 05/2013,
        # thus the default month value is 05 since the default year is 2013.
        # Animation is toggled on such that a button can be pressed for the
        # months to be automatically cycled through
        
        sliderInput("Month",
                     "Month:",
                     min = 1,
                     max = 12,
                     value = 5,
                     animate = TRUE),
        
        # Include note about the limitations of ShotSpotter data and a line beak
        # for aesthetic purposes
        
        p("Note: ShotSpotter data available beginning 05/2013"),
        
        br(),
        
        # Check box selector for gun shot type so that viewers can choose one
        # year at a type of gunshot fire (e.g. perhaps only the distribution of
        # multiple ghunshots over time are of interest). The checkbox input type
        # was chosen however, so that multiple types of gunsot fire can be
        # visualized and constrasted
        
        checkboxGroupInput("Type",
                           "Type:",
                           unique(shot$type), 
                           selected = unique(shot$type))
      ),
      
      # Show a plot of the generated Leaflet map. In this instance, the use of
      # the map overlay provided by Leaflet seemed far superior to the shapefile
      # approach, which may recreate the boundaries of Camden City, but lacks
      # context for the overlaid points to have any meaning
      
      mainPanel(
        
        # Add a note thanking the Justice Tech Lab (and provide a link to their
        # site!)
        
        h6("Thanks to data from the ", a("ShotSpotter Project", href="http://justicetechlab.org/shotspotter-data/"), " by the Justice Tech Lab, we can visualize gunshot data in Camden, NJ"),
        
        # Include the Leaflet map output
        
        leafletOutput("mapPlot")
      )
   )
)

# Define server logic required to draw a Leaflet plot

server <- function(input, output) {
  
   output$mapPlot <- renderLeaflet({
     
     # If no gunshot type is selected (length of input is zero), then display
     # the blank map of Camden with the same viewing parameters
     
     # Or, if the year 2013 is selected and a month smaller than 05 is selected,
     # then also display a blank map of Camden -- Shotspotter data for this city
     # only begins after 05/2013, and adding this condition as well as the
     # alternative outcome (a blank map) - prevents the app from throwing up an
     # ugly error
     
     if (length(input$Type) == 0 | (input$Year == 2013 & input$Month < 5)) {
     
       # Create leaflet map with a minimum zoom of 13, focused on Camden, but
       # draggable within a small range around Camden
       
       leaflet(options = leafletOptions(minZoom = 13, dragging = TRUE))  %>% 
         addProviderTiles("Esri.WorldStreetMap")  %>% 
         
         # Used Google Maps to place a pin and determine ideal coordinates of
         # Camden City around which to center the map
         
         setView(lng = -75.119621, lat = 39.931364, zoom = 13) %>% 
         
         # Fix the bounds to which the leaflet map may be dragged, such that it is
         # limited to the Camden City area
         
         setMaxBounds(lng1 = -75.119621 + .075, 
                      lat1 = 39.931364 + .05, 
                      lng2 = -75.119621 - .075, 
                      lat2 = 39.931364 - .05)
       
     }
     
     else {
     
     # Subset the shot data based upon the user's selections so that it informs
     # the rendering of the map
       
     shot_select <- shot %>% filter(year == input$Year, month == input$Month, type %in% input$Type)
     
     # Create a custom color palette to assign different colors to the different
     # factor levels for gunshot type so that they will be distinguishable on
     # the map
     
     pal <- colorFactor(palette = c("red", "blue", "green"), 
                        levels = c("Multiple Gunshots", "Single Gunshot", "Gunshot or Firecracker"))
     
     # Create leaflet map with a minimum zoom of 13, focused on Camden, but
     # draggable within a small range around Camden
     
     leaflet(options = leafletOptions(minZoom = 13, dragging = TRUE))  %>% 
       addProviderTiles("Esri.WorldStreetMap")  %>% 
       
       # Used Google Maps to place a pin and determine ideal coordinates of
       # Camden City around which to center the map
       
       setView(lng = -75.119621, lat = 39.931364, zoom = 13) %>% 
       
       # Add markers using the data just from the subset of the shot data that
       # corresponds to the user's selection on the sidepanel
       
       addCircleMarkers(
         data = shot_select, 
         
         # Color the points by the type of gunshot to provide another layer of
         # information and allow viewers to assess the "severity" of the
         # gunshots being recorded at a given time and place
         
         color = ~pal(shot_select$type), 
         
         # Use the longitude and latitude from the Camden, NJ shotspotter
         # dataset to inform the marker positons
         
         lng = shot_select$longitude, 
         lat = shot_select$latitude, 
         
         # Make the radius of the points small - size 1 - so that multiple
         # points nearby can be more clearly visualized
         
         radius = 1, 
         label = shot_select$date) %>% 
       
       # Fix the bounds to which the leaflet map may be dragged, such that it is
       # limited to the Camden City area
       
       setMaxBounds(lng1 = -75.119621 + .075, 
                    lat1 = 39.931364 + .05, 
                    lng2 = -75.119621 - .075, 
                    lat2 = 39.931364 - .05)
     }
   })
}

# Run the application 

shinyApp(ui = ui, server = server)

