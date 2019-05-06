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
library(ggthemes)

load("data/deathrate.Rdata")


ui <- fluidPage(

    theme = shinytheme("sandstone"),
    
    #Title
    titlePanel("Motor Vehicle-Related Deaths in the United States"),
    h3("Eliana Marostica, Maria Nakhoul, and Sunny Mahesh"),
    HTML('<p> Final Project for <a href="https://dbmi.hms.harvard.edu/education/courses/bmi-706">BMI 706</a> in
         <a href="https://dbmi.hms.harvard.edu/">The Blavatnik Institute Department of Biomedical Informatics</a></p>'),
    
    navbarPage("",
               
               tabPanel("By Age and Gender",
                        value = "B", 
                        h3("Death Rates by Age and Gender in the United States, 2012 & 2014"),
                        sidebarPanel(
                            #radio buttons for age group
                            radioButtons("mapyear", "Year", choices=c("2012","2014")),
                            selectizeInput("mapstrat", "By Age or Gender", choices=unique(ocdr$Var))
                        ),
                        mainPanel(
                            plotlyOutput("boxplot"),
                            verbatimTextOutput("boxclick")
                            ),
                        fluidRow(column(plotlyOutput("olddatamap"), width=12))
                        ),
               
               tabPanel("Vis2", value = "A", 
                        h4("Table"))
               ),
    
    #Signature
    h6(HTML('<p><i>Last Updated: May 9, 2019</i> | <a href="https://github.com/emarosti/bmi706-visualization-project">GitHub Repository</a>'))
)








# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$boxplot <- renderPlotly({
        p <- ocdr_tidy %>%
            unite(col="Label", Age, Gender, Year, sep=", ",remove=F) %>%
            #group_by(Year) %>%
            ggplot(aes(x=Label,y=Death_Rate)) +
            geom_boxplot(aes(fill=Year)) +
            theme_few() +
            theme(axis.title.x = element_text(),
                  axis.title.y = element_text(),
                  axis.text.x = element_text(angle=-45,size=8,hjust=1)) +
            scale_fill_viridis_d()
        ggplotly(p)
        
    })
    
    output$boxclick <- renderPrint({
        labels <- ocdr_tidy %>%
            unite(col="Label", Age, Gender, Year, sep=", ",remove=F) %>%
            select(Label) %>%
            distinct() %>%
            arrange(Label)
        
        d <- event_data("plotly_click")
        if (is.null(d)) {
            return(paste("Use the dropdown menu to stratify by age or gender, or select directly on the boxplot"))
        }
        else{
            #return(input$mapyear)
            updateRadioButtons(session,"mapyear",label="Year", choices=c("2012","2014"), selected=ifelse(sum(d$curveNumber) == 0,"2012","2014"))
            updateSelectizeInput(session,"mapstrat", selected= ifelse(unique(d$x) == 1 | unique(d$x) == 2, "Age 0-20",
                                                                   ifelse(unique(d$x) == 3 | unique(d$x) == 4, "Age 21-34",
                                                                          ifelse(unique(d$x) == 5 | unique(d$x) == 6, "Age 35-54",
                                                                                 ifelse(unique(d$x) == 7 | unique(d$x) == 8, "Age 55+",
                                                                                        ifelse(unique(d$x) == 9 | unique(d$x) == 10, "All Ages",
                                                                                               ifelse(unique(d$x) == 11 | unique(d$x) == 12,"Female", "Male"))))))) 
            return(paste("Use the dropdown menu to stratify by age or gender, or select directly on the boxplot"))
        }
        
    })
    

    output$olddatamap <- renderPlotly({
        
        # give state boundaries a white border
        l <- list(color = toRGB("white"), width = 2)
        # specify some map projection/options
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = FALSE
        )
        
        sd <- SharedData$new(ocdr, key=~State)
    
        
        p1 <- sd %>%
            plot_geo(.,locationmode = 'USA-states') %>%
            hide_legend() %>%
            filter(Year == input$mapyear, Var == input$mapstrat, !is.na(code)) %>%
            add_trace(
                z = ~Death_Rate, color=~Death_Rate, text = ~Location, locations = ~code,
                colors = viridis(n=5,direction=-1)
            ) %>%
            hide_colorbar() %>%
            layout(
                title = paste('Motor Vehicle Related Deaths', "All Ages",sep=", "),
                geo = g
            ) %>%
            highlight(selected = attrs_selected(opacity = 0.5))
        
        p2 <- sd %>%
            plot_ly() %>%
            filter(Year == input$mapyear, Var == input$mapstrat) %>%
            group_by(State) %>%
            arrange(Death_Rate) %>%
            hide_legend() %>%
            add_bars(x=~State, y=~Death_Rate, marker = list(color = "grey"), name = "Death Rates") %>%
            layout(xaxis= list(categoryorder = "array",
                               categoryarray = ~State),
                   barmode='overlay',
                   font=list(size=8))
        
        sub2 <- subplot(p1, p2, nrows=1) %>%
            layout(title="Motor Vehicle Related Deaths, Stratified")
        
        sub2
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
