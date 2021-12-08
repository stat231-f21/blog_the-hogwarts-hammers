
library(shiny)
library(readr)
library(leaflet)
library(dplyr)
library(rgdal)
library(RColorBrewer)

# Read in data
suicideData <- read_csv('suicideRatesWideYears.csv')
leafletData <- readOGR(
    dsn = paste0(getwd(), "/data/LeafletData"),
    layer = "TM_WORLD_BORDERS_SIMPL-0.3",
    verbose = FALSE
)
leafletSuicideData <- merge(leafletData, suicideData, by = "ISO3")
# Create bins by suicide rate range
mybins <- c(0,1,2,5,7,10,12, 15, 20, 25, 30, 50, 100)
# Assign colors to these bins
myPalette <- colorBin( 
    palette=c("#333399",
              "#555599", 
              "#777799",
              "#999999",
              "#ffcccc",
              "#ffaaaa",
              "#ff9999", 
              "#ff7777", 
              "#cc5555",
              "#aa0000",
              "#550000",
              "#330000"),
    domain = leafletSuicideData$Rate2017,
    na.color = "transparent",
    bins = mybins
)

# Define UI for application that draws a map
ui <- fluidPage(

    # Application title
    titlePanel("World Suicide Rates"),

    # Sidebar with a slider input for the year
    fluidRow(
      wellPanel(
            sliderInput("year",
                        "Year:",
                        min = 1990,
                        max = 2017,
                        value = 2017,
                        sep = "",
                        # Add a button to animate through the years, changing
                        # years every 750ms in a loop
                        animate = animationOptions(interval = 750, loop = TRUE))
      )
        ),
    fluidRow(
        # Show a map of suicide rates
        mainPanel(
           leafletOutput("map", height = 600, width = 800)
        )
    )
)





# Define server logic required to draw maps of suicide rates
server <- function(input, output) {
    output$map <- renderLeaflet({
        # Draw the base map and legend
        leaflet(leafletSuicideData) %>%
            addTiles() %>%
            setView(lat = 30, lng = 0, zoom = 1.5) %>%
            addLegend(pal = myPalette, 
                      values = leafletSuicideData$Rate2017, 
                      opacity = .7, 
                      title = "Suicide Death Rate<br/>Per 100k", 
                      position = "bottomleft")
    })
    
    # Add the actual data to the map in observe. Doing it this way
    # gets rid of the flickering that would happen if we redrew
    # the whole map every time data changed
    observe({
        # Select the right column based on the selected year        
        selectedData = switch(as.character(input$year),
         "1990" = leafletSuicideData$Rate1990,
         "1991" = leafletSuicideData$Rate1991,
         "1992" = leafletSuicideData$Rate1992,
         "1993" = leafletSuicideData$Rate1993,
         "1994" = leafletSuicideData$Rate1994,
         "1995" = leafletSuicideData$Rate1995,
         "1996" = leafletSuicideData$Rate1996,
         "1997" = leafletSuicideData$Rate1997,
         "1998" = leafletSuicideData$Rate1998,
         "1999" = leafletSuicideData$Rate1999,
         "2000" = leafletSuicideData$Rate2000,
         "2001" = leafletSuicideData$Rate2001,
         "2002" = leafletSuicideData$Rate2002,
         "2003" = leafletSuicideData$Rate2003,
         "2004" = leafletSuicideData$Rate2004,
         "2005" = leafletSuicideData$Rate2005,
         "2006" = leafletSuicideData$Rate2006,
         "2007" = leafletSuicideData$Rate2007,
         "2008" = leafletSuicideData$Rate2008,
         "2009" = leafletSuicideData$Rate2009,
         "2010" = leafletSuicideData$Rate2010,
         "2011" = leafletSuicideData$Rate2011,
         "2012" = leafletSuicideData$Rate2012,
         "2013" = leafletSuicideData$Rate2013,
         "2014" = leafletSuicideData$Rate2014,
         "2015" = leafletSuicideData$Rate2015,
         "2016" = leafletSuicideData$Rate2016,
         "2017" = leafletSuicideData$Rate2017
         )

        # Create the text for the popup that shows the country name and suicide rate
        hoverText <- 
            paste("Country: ", leafletSuicideData@data$NAME, "<br/>",
                  "Suicide Rate (", input$year, "): ", selectedData, "<br/>",
                  sep = "") %>%
          lapply(htmltools::HTML)
            
        # Use the leaflet proxy to add the data to the map
        leafletProxy('map', data = leafletSuicideData) %>%
            # Clear the existing circles from the map first
            clearGroup("circles") %>%
            # Add circles colored and sized by the suicide rate for each country
            addCircles(
                lat = leafletSuicideData$LAT, lng = leafletSuicideData$LON,
                # Create circle size
                radius = selectedData * 10000,
                # Fill circle and define opacity
                opacity = 1,
                fillOpacity = .7,
                weight = 1,
                group = "circles",
                color = myPalette(selectedData),
                fillColor = myPalette(selectedData),
                # Create hover text
                label = hoverText,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
