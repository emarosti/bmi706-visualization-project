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
                        h4("Summary Statistics - Counts")),
               
               tabPanel("Vis2", value = "A", 
                        h4("Table"))
               ),
    
    #Signature
    h6(HTML('<p><i>Last Updated: May 9, 2019</i> | <a href="https://github.com/emarosti/bmi706-visualization-project">GitHub Repository</a>'))
)








# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
