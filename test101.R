
library(shiny)
library(ggplot2)
library(dplyr)
library(maps)
library(ggmap)
library(readxl)

world <- map_data("world")
countries <- unique(world$region)


covid <- read_excel("covid_19_data.xlsx")
corona_country <- unique(covid$`Country/Region`)
corona_country



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
            selectInput("vari", 
                        label = "Choose a country to display",
                        choices = corona_country),
            textOutput("recovery"),
            textOutput("death"),
            
        ),
        
        mainPanel(
            textOutput("text"),
            plotOutput("worldmap"),
            plotOutput("CoronaBar")
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
    output$CoronaBar <- renderPlot({
        filtered <- filter(covid,covid$`Country/Region`==input$vari)
        filtered <- filtered[1:50,]
        qplot(x=`Last Update`,y=Confirmed,data=filtered,geom="boxplot",xlab="Date",ylab="Number of Confirmed Cases",main="Cases recorded  as per date")+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    output$recovery <- renderText({
        filtered <- filter(covid,covid$`Country/Region`==input$vari)
        r_rate = (sum(filtered$Recovered) / sum(filtered$Confirmed)) * 100
        paste("Recovery Rate in",input$vari,"is",r_rate)
    })
    output$death <- renderText({
        filtered <- filter(covid,covid$`Country/Region`==input$vari)
        d_rate = (sum(filtered$Deaths) / sum(filtered$Confirmed)) * 100
        paste("Death Rate in",input$vari,"is",d_rate)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
