#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

suicideData <- read_csv('fullSuicideData.csv')
leafletData <- readOGR(
    dsn = paste0(getwd(), "/data/LeafletData"),
    layer = "TM_WORLD_BORDERS_SIMPL-0.3",
    verbose = FALSE
)
suicideData2016 <- suicideData %>%
    filter(Year == "2016")
leafletSuicideData <- merge(leafletData, suicideData2016, by = "ISO3") 
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Year:",
                        min = 1990,
                        max = 2017,
                        value = 2017,
                        sep = "")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("map")
        )
    )
)

mybins <- c(0,1,2,5,7,10,12, 15, 20, 25, 30, Inf)
mypalette <- colorBin( 
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
              "#550000"),
    domain = leafletSuicideData2016@data$SuicideDeathRate,
    na.color = "transparent",
    bins = mybins
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    leafletSuicideData <- reactive(
        merge(leafletData, filter(suicideData, Year == input$year), by = "ISO3")
    )
    output$map <- renderLeaflet({
        # Prepare the text for tooltips:
        hoverText <- paste(
            "Country: ", leafletSuicideData()@data$NAME, "<br/>",
            "Total MH Beds: ", leafletSuicideData()@data$TOTAL_BEDS, "<br/>",
            "Suicide Rate: ", round(leafletSuicideData()@data$SuicideDeathRate, 2), "<br/>",
            "Ratio of male to female suicide: ", round(leafletSuicideData()@data$MaleFemaleSuicideRatio, 2),
            sep = ""
        ) %>%
            lapply(htmltools::HTML)
        
        # Final Map
        leaflet(leafletSuicideData()) %>%
            addTiles() %>%
            setView(lat = 10, lng = 0, zoom = 2) %>%
            addPolygons(
                fillColor = ~ mypalette(SuicideDeathRate),
                stroke = TRUE,
                fillOpacity = 1,
                color = "white",
                weight = 1,
                label = hoverText,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto"
                )
            ) %>%
            addLegend(pal = mypalette, values = ~SuicideDeathRate, opacity = 1, title = "Suicide Death Rate", position = "bottomleft")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
