# BMI 706 Visualization Project
# Eliana Marostica, Sunny Mahesh, Maria Nakhoul
# Created: May 9, 2019
# 
# Shiny app to display visualizations on Motor Vehicle-related Deaths in the United States
# 
#

library(shiny)
library(tidyverse)
library(plotly)
library(crosstalk)
library(shinythemes)
library(viridis)

load("data/deathrate.Rdata")


ui <- fluidPage(

    theme = shinytheme("sandstone"),
    
    #Title
    titlePanel(h1("Motor Vehicle-Related Deaths in the United States")),
    h3("Eliana Marostica, Maria Nakhoul, and Sunny Mahesh"),
    HTML('<p> Final Project for <a href="https://dbmi.hms.harvard.edu/education/courses/bmi-706">BMI 706</a> in
         <a href="https://dbmi.hms.harvard.edu/">The Blavatnik Institute Department of Biomedical Informatics</a></p>'),
    
    navbarPage("",
               
               tabPanel("Vis1",
                        value = "B", 
                        h4("Summary Statistics - Counts"),
                        sidebarPanel(
                            #radio buttons for age group
                            radioButtons("mapyear", "Year", choices=c("2012","2014")),
                            selectInput("mapstrat", "By Age or Gender", choices=unique(ocdr$Var))
                        ),
                        mainPanel(
                            plotlyOutput("newdatamap")
                            )
                        ),
               
               tabPanel("Vis2", value = "A", 
                        h4("Table"))
               ),
    
    #Signature
    h6(HTML('<p><i>Last Updated: May 9, 2019</i> | <a href="https://github.com/emarosti/bmi706-visualization-project">GitHub Repository</a>'))
)








# Define server logic required to draw a histogram
server <- function(input, output) {

    output$newdatamap <- renderPlotly({
        
        # give state boundaries a white border
        l <- list(color = toRGB("white"), width = 2)
        # specify some map projection/options
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = TRUE,
            lakecolor = toRGB('white')
        )

        ocdr %>%
            filter(Year == "2012", Var == "All Ages", !is.na(code)) %>%
            plot_geo(., locationmode = 'USA-states') %>%
            add_trace(
                z = ~Death_Rate, color=~Death_Rate, text = ~Location, locations = ~code,
                colors = viridis(n=5,direction=-1)
            ) %>%
            colorbar(title = "Death Rate per 100,000") %>%
            layout(
                title = paste('Motor Vehicle Related Deaths', "All Ages",sep=", "),
                geo = g
            )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
