# Combine and Tidy Data Files
# Eliana Marostica
# May 4, 2019

library(tidyverse)

iddr <- read_csv("data/Impaired_Driving_Death_Rate__by_Age_and_Gender__2012___2014__All_States.csv")
ocdr_orig <- read_csv("data/Motor_Vehicle_Occupant_Death_Rate__by_Age_and_Gender__2012___2014__All_States.csv")
sebt <- read_csv("data/Percentage_of_Drivers_and_Front_Seat_Passengers_Wearing_Seat_Belts__2012___2014__All_States.csv")
codes <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv") %>%
  select(code, state)

ocdr <- ocdr_orig %>%
  mutate(code = codes$code[match(ocdr_orig$State,codes$state)]) %>%
  gather(`All Ages, 2012`, `All Ages, 2014`, `Age 0-20, 2012`,`Age 0-20, 2014`,`Age 21-34, 2012`,`Age 21-34, 2014`,
         `Age 35-54, 2012`,`Age 35-54, 2014`,`Age 55+, 2012`,`Age 55+, 2014`, `Male, 2012`,`Male, 2014`,
         `Female, 2012`,`Female, 2014`, key="Var and Year", value="Death_Rate") %>%
  separate(`Var and Year`, into = c("Var", "Year"), sep=", ") %>%
  filter(!is.na(State))

ocdr_gender <- ocdr_orig %>%
  mutate(code = codes$code[match(ocdr_orig$State,codes$state)]) %>%
  filter(!is.na(State)) %>%
  gather(`Male, 2012`,`Male, 2014`, `Female, 2012`,`Female, 2014`, key="GenderYr",value="Death_Rate") %>%
  separate(GenderYr, into = c("Gender", "Year"), sep=", ") %>%
  mutate(Age = "All Ages") %>%
  select(State,Location,code,Age,Gender,Year,Death_Rate) 


ocdr_tidy <- ocdr_orig %>%
  mutate(code = codes$code[match(ocdr_orig$State,codes$state)]) %>%
  filter(!is.na(State)) %>%
  gather(`All Ages, 2012`, `All Ages, 2014`, `Age 0-20, 2012`,`Age 0-20, 2014`,`Age 21-34, 2012`,`Age 21-34, 2014`,
         `Age 35-54, 2012`,`Age 35-54, 2014`,`Age 55+, 2012`,`Age 55+, 2014`, key="AgeYr",value="Death_Rate") %>%
  separate(AgeYr, into = c("Age", "Year"), sep=", ") %>%
  mutate(Gender = "All Genders") %>%
  select(State,Location,code,Age,Year,Gender,Death_Rate) %>%
  rbind(ocdr_gender)

save(ocdr, ocdr_gender, ocdr_tidy,file="data/deathrate.Rdata")





