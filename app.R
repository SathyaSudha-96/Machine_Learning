
library(shiny)
library(ggplot2)
library(dplyr)
library(maps)
library(ggmap)

world <- map_data("world")
countries <- unique(world$region)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("My First Application on World Map and Corona Data"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("var", 
                        label = "Choose a country to display",
                        choices = countries),
            plotOutput("distPlot"),
        ),
        
        mainPanel(
            textOutput("text"),
            plotOutput("worldmap"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        world<-map_data("world") %>% filter(region==input$var)
        ggplot(world)+
            geom_polygon(aes(x=long,y=lat,group=group))+
            geom_point(aes(x=long,y=lat),col="red")+labs(x="Longiude",y="Latitutde")
    })
    output$text <- renderText({
        paste("Are you able to locate",input$var,"in the World Map????")
    })
    output$worldmap <- renderPlot({
        ggplot(world)+
            geom_polygon(aes(x=long,y=lat,group=group))+
            geom_point(aes(x=long,y=lat),col="#000099")+labs(x="Longiude",y="Latitutde")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
