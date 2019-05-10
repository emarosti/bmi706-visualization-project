# Combine and Tidy Data Files
# Eliana Marostica, Maria Nakhoul, Sunny Mahesh
# May 4, 2019

#Might need to install this package 
#install.packages("rio")

library(tidyverse)
library(rio)


## CDC DEATH RATES DATA
#change this
ocdr_orig <- read_csv("data/Motor_Vehicle_Occupant_Death_Rate__by_Age_and_Gender__2012___2014__All_States.csv")
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
codes <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv") %>%
  dplyr::select(code, state)
ocdr <- ocdr_orig %>%
  mutate(code = codes$code[match(ocdr_orig$State,codes$state)]) %>%
  gather(`All Ages, 2012`, `All Ages, 2014`, `Age 0-20, 2012`,`Age 0-20, 2014`,`Age 21-34, 2012`,`Age 21-34, 2014`,
         `Age 35-54, 2012`,`Age 35-54, 2014`,`Age 55+, 2012`,`Age 55+, 2014`, `Male, 2012`,`Male, 2014`,
         `Female, 2012`,`Female, 2014`, key="Var and Year", value="Death_Rate") %>%
  separate(`Var and Year`, into = c("Var", "Year"), sep=", ") %>%
  filter(!is.na(State))

us_ind <- which(ocdr$State=="United States")
ocdr <- ocdr[-us_ind,]

ocdr_gender <- ocdr_orig %>%
  mutate(code = codes$code[match(ocdr_orig$State,codes$state)]) %>%
  filter(!is.na(State)) %>%
  gather(`Male, 2012`,`Male, 2014`, `Female, 2012`,`Female, 2014`, key="GenderYr",value="Death_Rate") %>%
  separate(GenderYr, into = c("Gender", "Year"), sep=", ") %>%
  mutate(Age = "All Ages") %>%
  dplyr::select(State,Location,code,Age,Gender,Year,Death_Rate) 

ocdr_tidy <- ocdr_orig %>%
  mutate(code = codes$code[match(ocdr_orig$State,codes$state)]) %>%
  filter(!is.na(State)) %>%
  gather(`All Ages, 2012`, `All Ages, 2014`, `Age 0-20, 2012`,`Age 0-20, 2014`,`Age 21-34, 2012`,`Age 21-34, 2014`,
         `Age 35-54, 2012`,`Age 35-54, 2014`,`Age 55+, 2012`,`Age 55+, 2014`, key="AgeYr",value="Death_Rate") %>%
  separate(AgeYr, into = c("Age", "Year"), sep=", ") %>%
  mutate(Gender = "All Genders") %>%
  dplyr::select(State,Location,code,Age,Year,Gender,Death_Rate) %>%
  rbind(ocdr_gender)

##---

## Insurance Institute for Highway Safety Highway Loss Data Institute DATA

fatal_car_crashes<-import_list("data/fatal car_crash.xlsx",setclass = "tbl",rbind = TRUE)
deaths_by_road_users<-import_list("data/Deaths by road users.xlsx",setclass="tbl",rbind=T)
indicies=c()

for( i in 1:13){
  indicies[i]=52*i
}
deaths_car_crashes=fatal_car_crashes[-indicies,]

line_graph<-fatal_car_crashes[indicies,]
colnames(line_graph)=c("State","Population","Deaths","Deaths_per_100000_Population","Year")
line_graph$Year=c("2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005")

na_indicies=which(is.na(deaths_by_road_users$State))
deaths_by_road_users=deaths_by_road_users[-na_indicies,]
road_users_deaths=deaths_by_road_users[-indicies,]

year=c(rep(2017,51),rep(2016,51),rep(2015,51),rep(2014,51),rep(2013,51),rep(2012,51),rep(2011,51),rep(2010,51),rep(2009,51),rep(2008,51),rep(2007,51),rep(2006,51),rep(2005,51))

deaths_car_crashes=deaths_car_crashes[,1:4]
deaths_car_crashes$Year=year


seatbelt<-import_list("data/percent_of_seatbelt_use.xlsx",setclass = "tbl",rbind = TRUE)
seatbelt
year=c(rep(2017,51),rep(2016,51),rep(2015,51),rep(2014,51),rep(2013,51),rep(2012,51),rep(2011,51),rep(2010,51),rep(2009,51),rep(2008,51),rep(2007,51),rep(2006,51),rep(2005,51))
year2=c(rep(2017,51),rep(2016,51),rep(2015,51),rep(2014,51),rep(2013,51),rep(2012,51),rep(2011,51),rep(2010,51),rep(2009,51))
seatbelt$`_file`=year2
colnames(seatbelt)=c("State","Percentage_of_observed_seatbelt_use","Year")

deaths<-deaths_car_crashes%>%filter(Year!="2008",Year!="2007",Year!="2006",Year!="2005")%>%dplyr::select(`Deaths`,`Year`,`State`)

seatbelt_with_deaths<-inner_join(seatbelt,deaths,by=c("Year","State"))

dui<-import_list("data/DUI.xlsx",setclass = "tbl",rbind = TRUE)

US_total_DUI<-dui[indicies,]
dui<-dui[-indicies,]
dui$`_file`<-year2
US_total_DUI<-US_total_DUI%>%filter(!is.na(State))
US_total_DUI$`_file`<-c("2017","2016","2015","2014","2013","2012","2011","2010","2009")
colnames(US_total_DUI)<-c("State","Total","Year")
colnames(dui)<-c("State","Total","Year")

state_code<-ocdr_tidy%>%dplyr::select(`State`,`code`)%>%unique()

codes<-c(as.character(df$code[1:8]),"DC",as.character(df$code[9:50]))
road_users_deaths$code=codes
deaths_car_crashes$code=codes
deaths_car_crashes$hover <- with(deaths_car_crashes, paste(State, '<br>', "Population",Population, "<br>","Deaths", Deaths, "<br>",
                                                           "Year", Year,"<br>", "Deaths per 100,000 population", `Deaths per 100,000 population`))

road_users_deaths_column_names=c("State","Car_Occupant_Death_Number","Car_Occupant_Death_Percent","Pickup_and_SUV_Occupant_Death_Number","Pickup_and_SUV_Occupant_Death_Percent","Large_Truck_Occupant_Death_Number","Large_Truck_Occupant_Death_Percent","Motorcyclists_Occupant_Death_Number","Motorcyclists_Occupant_Death_Percent","Pedestrians_Occupant_Death_Number","Pedestrians_Occupant_Death_Percent","Bicyclists_Occupant_Death_Number","Bicyclists_Occupant_Death_Percent","Total_Occupant_Death_Number","Total_Occupant_Death_Percent","Year","Code")
colnames(road_users_deaths)=road_users_deaths_column_names
road_users_deaths$Year=year
colnames(deaths_car_crashes)=c("State","Population","Deaths","Deaths_per_100000_Population","Year","Code","Hover")

##--- 


save(ocdr,ocdr_gender,ocdr_orig,ocdr_tidy,state_code,road_users_deaths,deaths_car_crashes,seatbelt_with_deaths,line_graph,dui,US_total_DUI, file="data/deathrate.Rdata")
