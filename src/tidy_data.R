# Combine and Tidy Data Files
# Eliana Marostica
# May 4, 2019

iddr <- read_csv("data/Impaired_Driving_Death_Rate__by_Age_and_Gender__2012___2014__All_States.csv")
ocdr <- read_csv("data/Motor_Vehicle_Occupant_Death_Rate__by_Age_and_Gender__2012___2014__All_States.csv")
sebt <- read_csv("data/Percentage_of_Drivers_and_Front_Seat_Passengers_Wearing_Seat_Belts__2012___2014__All_States.csv")
codes <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv") %>%
  select(code, state)

ocdr <- ocdr %>%
  mutate(code = codes$code[match(ocdr$State,codes$state)]) %>%
  gather(`All Ages, 2012`, `All Ages, 2014`, `Age 0-20, 2012`,`Age 0-20, 2014`,`Age 21-34, 2012`,`Age 21-34, 2014`,
         `Age 35-54, 2012`,`Age 35-54, 2014`,`Age 55+, 2012`,`Age 55+, 2014`, `Male, 2012`,`Male, 2014`,
         `Female, 2012`,`Female, 2014`, key="Var and Year", value="Death_Rate") %>%
  separate(`Var and Year`, into = c("Var", "Year"), sep=", ") %>%
  filter(!is.na(State))

save(ocdr,iddr, sebt, file="data/deathrate.Rdata")
