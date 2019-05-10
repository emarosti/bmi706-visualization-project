# BMI 706 Visualization Project
# Eliana Marostica, Sunny Mahesh, Maria Nakhoul
# May 9, 2019
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

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = FALSE
)

ui <- fluidPage(
  
  theme = shinytheme("sandstone"),
  
  #Title
  titlePanel("Motor Vehicle-Related Deaths in the United States"),
  h3("Maria Nakhoul, Eliana Marostica, and Sunny Mahesh"),
  HTML('<p> Final Project for <a href="https://dbmi.hms.harvard.edu/education/courses/bmi-706">BMI 706</a> in
       <a href="https://dbmi.hms.harvard.edu/">The Blavatnik Institute Department of Biomedical Informatics</a></p>'),
  
  navbarPage("",
             
             tabPanel("By Age and Gender",
                      value = "B", 
                      h3("Death Rates by Age and Gender in the tates, 2012 & 2014"),
                      sidebarPanel(
                        #radio buttons for the year
                        radioButtons("mapyear", "Year", choices=c("2012","2014")),
                        #input selection for age and gender
                        selectizeInput("mapstrat", "By Age or Gender", choices=unique(ocdr$Var))
                      ),
                      mainPanel(
                        plotlyOutput("boxplot"),
                        verbatimTextOutput("boxclick")
                      ),
                      fluidRow(column(plotlyOutput("olddatamap"), width=12))
             ),
             tabPanel("Comparisons", value = "C", 
                      h3("Comparisons Between Death rates in Different Years to a Reference Year"),
                      sidebarPanel(
                        selectizeInput("year_strat2", "Pick Reference Year", choices=unique(deaths_car_crashes$Year))
                      ), mainPanel(
                        h4("Small Multiples"),plotlyOutput("plot"), 
                        verbatimTextOutput("info")
                       # 
                      ), fluidRow(column(align="left",plotlyOutput("subplot"), width=12))
             ),
             tabPanel("Differences", value = "B",  
                        h3("Differences Between Death rates in Different Years to a Reference Year"),
                        sidebarPanel(
                          selectizeInput("year_strat", "Pick Reference Year", choices=unique(deaths_car_crashes$Year))
                        ),
                        mainPanel(
                        h4("Total Deaths per Year"),plotlyOutput("line_plot")
                        ),
                        fluidRow(column(plotlyOutput("diff_plot"), width=12))
                        
                        
             ),
             tabPanel("Correlation", value = "D",  
                        h3("Looking at Correlations of Seatbelt Use and Death Rates"),
                        sidebarPanel(
                          selectizeInput("year_strat3", "Pick Reference Year", choices=unique(dui$Year))
                        ),
                        mainPanel(
                          h4("Total Deaths from 2009-2017 from Driving Under the Influence"),plotlyOutput("graph")
                        ),
                        fluidRow(column(plotlyOutput("corr"), width=12))
                        
                        
             )
  ),
  
  #Signature
  h6(HTML('<p><i>Last Updated: May 9, 2019</i> | <a href="https://github.com/emarosti/bmi706-visualization-project">GitHub Repository</a>'))
  )


# Define server logic
server <- function(input, output, session) {
  
  #boxplot by age and gender
  output$boxplot <- renderPlotly({
    #first tab visualization
    p <- ocdr_tidy %>%
      unite(col="Label", Age, Gender, Year, sep=", ",remove=F) %>%
      #group_by(Year) %>%
      ggplot(aes(x=Label,y=Death_Rate)) +
      geom_boxplot(aes(fill=Year)) 
    d <- event_data("plotly_click",source = "boxplot")
    if(length(d)){
      Ages<-c("Age 0-20","Age 0-20","Age 21-34","Age 21-34","Age 35-54","Age 35-54","Age 55+","Age 55+","All Ages","All Ages","Female","Female","Male", "Male")
      years<-rep(c("2012","2014"),7)
      index=unique(d$x)
      if(index<11){
      data_boxplot<-ocdr_tidy%>%filter(Age==Ages[index],Year==years[index])%>%unite(col="Label", Age, Gender, Year, sep=", ",remove=F)}
      else{data_boxplot<-ocdr_tidy%>%filter(Gender==Ages[index],Year==years[index])%>%unite(col="Label", Age, Gender, Year, sep=", ",remove=F)}
      p<-p+geom_boxplot(data=data_boxplot,aes(col="pink"))
    }
      p<-p+theme_few() +
      theme(axis.title.x = element_text(),
            axis.title.y = element_text(),
            axis.text.x = element_text(angle=-45,size=8,hjust=1)) +
      scale_fill_viridis_d()
    gg<-ggplotly(p,source = "boxplot")
    
  })

  #boxplot click --> updates shiny selection
  output$boxclick <- renderPrint({
    #first tab visualization
    labels <- ocdr_tidy %>%
      unite(col="Label", Age, Gender, Year, sep=", ",remove=F) %>%
      dplyr::select(Label) %>%
      distinct() %>%
      arrange(Label)
    
    d <- event_data("plotly_click",source = "boxplot")
    if (is.null(d)) {
      return(paste("Use the dropdown menu to stratify by age or gender, or select directly on the boxplot"))
    }
    else{
      
      
      Ages<-c("Age 0-20","Age 0-20","Age 21-34","Age 21-34","Age 35-54","Age 35-54","Age 55+","Age 55+","All Ages","All Ages","Female","Female","Male", "Male")
      index=unique(d$x)

      updateRadioButtons(session,inputId = "mapyear",label="Year", choices=c("2012","2014"), selected=ifelse(sum(d$curveNumber) == 0,"2012","2014"))
      updateSelectizeInput(session,inputId = "mapstrat", selected=Ages[index] ) 
      y<-ifelse(sum(d$curveNumber) == 0,"2012","2014")
      print(paste(c(Ages[index],y)))
    }
    
  })
  
  #choropleth map for death rate of a given year, age/gender
  output$olddatamap <- renderPlotly({
    #first tab visualization
    d<-event_data("plotly_click",source="barplot")
   
    ss<-d$pointNumber+1
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = FALSE
    )#
    
    dat<-ocdr%>%filter(Year == input$mapyear, Var == input$mapstrat)%>%arrange(Death_Rate)
    hello<-dat[ss,]
    bar<-dat$State[ss]
    h<-SharedData$new(hello)
    sd<-highlight_key(dat)
    

      p1<-plot_geo(sd,locationmode = 'USA-states',source="map") %>%
      hide_legend() %>%
      filter(Year == input$mapyear, Var == input$mapstrat, !is.na(code)) %>%
      add_trace(
        z = ~Death_Rate, color=~Death_Rate, text = ~Location, locations = ~code,
        colors =viridis(n=5,direction=-1)
      ) %>%
        add_trace(data =hello,z = ~Death_Rate, locations = ~code,
                  color='red') %>%
     hide_colorbar() %>%
      layout(
        title = paste('Motor Vehicle Related Deaths', "All Ages",sep=","),
        geo = g
      )
      p2 <- ocdr %>%filter(Year == input$mapyear, Var == input$mapstrat) %>%
        group_by(State) %>%
        arrange(Death_Rate) %>%
        plot_ly(x=~State, y=~Death_Rate,type="bar",name = "Death Rates",marker = list(color = ifelse(dat$State==bar,"red","grey")),source = "barplot") %>%
        hide_legend() %>%
        layout(xaxis= list(categoryorder = "array",
                           categoryarray = ~State),
               barmode='overlay',
               font=list(size=8))
      
      sub2 <- subplot(p1, p2, nrows=1) %>%
        layout(title="Motor Vehicle Related Deaths, Stratified")
      
      sub2
      })
  
  
  
  
  output$plot<-renderPlotly({
    #the small multiples
    #second tab visualization
    event.data <- event_data("plotly_click")
    make_one_map <- function(data) {
      plot_geo(data, locationmode = 'USA-states') %>%
        add_trace(
          z = ~Deaths_per_100000_Population, locations = ~Code,
          color = ~Deaths_per_100000_Population, colors = viridis(n = 5,direction=-1),showscale = FALSE
        ) %>%
        layout(
          title = 'Deaths in US rate by 100,000',
          geo = g,
          showlegend = FALSE
        )
    }
    
    p<-deaths_car_crashes %>%
      group_by(Year) %>%
      do(map = make_one_map(.)) %>%
      subplot(nrows = 5) %>%
      layout(
        showlegend = FALSE,
        title = 'Deaths Across USA from 2005 till 2017',
        hovermode =T
      )  
    
   p
    
  })
  
  
  output$subplot<-renderPlotly({  
    #this is the reference states, chosen multiple and the barplot from hover
    #d is the event_data plotly_click
    #d2 is the event_data plotly_hover
    #q is the chosen small multiple
    #q2 is the reference
    #z is the barplots
   
    d <- event_data("plotly_click")
    if (length(d)) {
      year=rev(c("2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005"))
      y=year[d$curveNumber+1]
      q<-deaths_car_crashes%>%filter(Year==y)%>%plot_geo(., locationmode = 'USA-states') %>%
        add_trace(
          z = ~Deaths_per_100000_Population, locations = ~Code,
          color = ~Deaths_per_100000_Population, colors = viridis(n = 5,direction=-1),showscale = TRUE
        ) %>%
        layout(
          title = 'Deaths in US rate by 100,000',
          geo = g,
          showlegend = FALSE
        )
      ggplotly(q)
      
      q2<-deaths_car_crashes%>%filter(Year==input$year_strat2)%>%plot_geo(., locationmode = 'USA-states') %>%
        add_trace(
          z = ~Deaths_per_100000_Population, locations = ~Code,
          color = ~Deaths_per_100000_Population, colors = viridis(n = 5,direction=-1),showscale = F
        ) %>%
        layout(
          title = 'Deaths in US rate by 100,000',
          geo = g,
          showlegend = FALSE
        )
      ggplotly(q2)
      
      d2 <- event_data("plotly_hover")
      if (length(d2)) {
        year=rev(c("2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005"))
        y=year[d2$curveNumber+1]
        state=d2$pointNumber+1
        q1<-road_users_deaths%>%filter(Year==y)
        q1=q1[state,]%>%dplyr::select(Car_Occupant_Death_Number,Pickup_and_SUV_Occupant_Death_Number,Large_Truck_Occupant_Death_Number,Pedestrians_Occupant_Death_Number,Bicyclists_Occupant_Death_Number)
        q1=as.numeric(q1)
        k<-plot_ly(x=c("Car_Occupant_Death_Number","Pickup_and_SUV_Occupant_Death_Number","Large_Truck_Occupant_Death_Number","Pedestrians_Occupant_Death_Number","Bicyclists_Occupant_Death_Number"),y=q1,type="bar")
        z<-ggplotly(k)
      } else { z<-NULL}
      subplot(q2,q,z,nrows=1)%>%layout(title="Reference, Comparison, Boxplot")
      
      }else{q<-NULL}
  })
  
 
  output$info <- renderPrint({
    
    d <- event_data("plotly_click")
    if (!length(d)) {
      return(paste("Pick a small multiple to enlarge"))
    }
    else{
      #return(input$mapyear)
      year=rev(c("2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005"))
      y=year[d$curveNumber+1]
      print(paste(y))
    }
    
  })
 
   output$line_plot<-renderPlotly({ 
     #third tab visualization
     #p5 is the total deaths per year
      special_point<-line_graph%>%filter(Year==input$year_strat)
      p5 <- ggplot(data=line_graph,aes(x=Year, y=Deaths,group=1))+geom_line()+geom_point()+geom_point(data=special_point,col="red" )
      ggplotly(p5)
    
  })
  
  output$diff_plot<-renderPlotly({
    #third tab visualization
    #q is the reference
    #p is the small multiples of differences
    
    event.data <- event_data("plotly_click")
    make_one_map <- function(data) {
      plot_geo(data, locationmode = 'USA-states') %>%
        add_trace(
          z = ~difference, locations = ~Code,
          color = ~difference,colorscale="diverge_hsv",colors = colorspace::diverge_hsv(n=30),showscale = FALSE
        ) %>%
        layout(
          title = 'Difference in Deaths in US by Year to a Reference Year',
          geo = g,
          showlegend = FALSE,
          hovermode =T
        )
    }
    #gives you differences between death rates in all the years
    #year - reference
    not_year_data<-deaths_car_crashes%>%group_by(Year) %>%filter(Year!=input$year_strat)
    death_difference<-c()
    ddd<-deaths_car_crashes%>%filter(Year==input$year_strat)%>%dplyr::select(`Deaths_per_100000_Population`)
    for(i in 1:length(unique(deaths_car_crashes$Year))){
      if(i==1){
      arr<-deaths_car_crashes$Deaths_per_100000_Population[c(i:(i+49))]
      if(unique(deaths_car_crashes$Year)[i]==input$year_strat){
        death_difference<-c(death_difference,ddd)
      }else{
        death_difference<-c(death_difference,arr-ddd)
      }}else{
        arr<-deaths_car_crashes$Deaths_per_100000_Population[c(i:(i+49))]
        if(unique(deaths_car_crashes$Year)[i]==input$year_strat){
          death_difference<-c(death_difference,ddd)
        }else{
          death_difference<-c(death_difference,arr-ddd)
        }
      }
    }
    death_difference<-unlist(death_difference)
    names(death_difference)<-NULL
    deaths_car_crashes<-deaths_car_crashes%>%mutate(difference=death_difference)
    
    p<-deaths_car_crashes%>%
      group_by(Year) %>%
      do(map =  make_one_map(.)) %>%
      subplot(nrows = 5) %>%
      layout(
        showlegend = FALSE,
        hovermode =T
      )
    
    
    
    q<-deaths_car_crashes%>%filter(Year==input$year_strat)%>%plot_geo(., locationmode = 'USA-states') %>%
      add_trace(
        z = ~Deaths_per_100000_Population, locations = ~Code,
        color = ~Deaths_per_100000_Population, colorscale="diverse_hsv",showscale = FALSE
      ) %>%
      layout(
        title = 'Deaths in US rate by 100,000',
        geo = g,
        showlegend = FALSE
      )

    
  subplot(q,p,nrows=1) 
    
  })
  
  
  output$graph<-renderPlotly({
    #fourthtab visualization
    #p9 is total deaths from driving under the influence from 2009-2017
    special_point<-US_total_DUI%>%filter(Year==input$year_strat3)
    p9 <- ggplot(data=US_total_DUI,aes(x=Year, y=Total,group=1))+geom_line()+geom_point()+geom_point(data=special_point,col="red" )
    ggplotly(p9)

  })
  
output$corr<-renderPlotly({
  #fourthtab visualization
  #p3 is the correlation scatter plot of deaths and percentage of seatbelt use
  #p0 is the barplot of deaths from dui
  d<-event_data("plotly_click")
  ss<-d$pointNumber+1
  dui_now<-dui %>%filter(Year == input$year_strat3) %>%
    arrange(Total) 
  bar<-dui_now$State[ss]
  
  if (length(d)) {
     
      p0 <-dui_now%>%plot_ly(x=~State, y=~Total,type="bar",name = "Deaths from Driving Under the Influence",marker = list(color = ifelse(dui_now$State==bar,"red","grey"))) %>%
    hide_legend() %>%
    layout(xaxis= list(categoryorder = "array",
                       categoryarray = ~State),
          
           barmode='overlay',
         font=list(size=8))
  special_point<-seatbelt_with_deaths%>%filter(Year==input$year_strat,State==bar)
  p3<-seatbelt_with_deaths%>%filter(Year==input$year_strat3)%>%ggplot(.,aes(y=Deaths,x=as.integer(Percentage_of_observed_seatbelt_use)))+geom_point()+geom_point(data=special_point,col="red" )+geom_smooth(method=lm,se = F)+xlab("Percentage of Observed Seatbelt Use")
  p3<-ggplotly(p3)
  

  
  }else{
    p0 <- dui %>%filter(Year == input$year_strat3) %>%
      arrange(Total) %>%
      plot_ly(x=~State, y=~Total,type="bar",name = "Deaths from Driving Under the Influence",marker = list(color ="grey")) %>%
      hide_legend() %>%
      layout(xaxis= list(categoryorder = "array",
                         categoryarray = ~State),
            
             barmode='overlay',
             font=list(size=8))
    
    p3<-seatbelt_with_deaths%>%filter(Year==input$year_strat3)%>%ggplot(.,aes(y=Deaths,x=as.integer(Percentage_of_observed_seatbelt_use)))+geom_point()+geom_smooth(method=lm,se = F)+xlab("Percentage of Observed Seatbelt Use")
    p3<-ggplotly(p3)
    
    }
  subplot(p3,p0)%>%layout(title="Correlation Between Percentage of Seat Belt Use and Number of Deaths / Deaths from Driving Under the Influence")
  
}
)  
}

# Run the application 
shinyApp(ui = ui, server = server)