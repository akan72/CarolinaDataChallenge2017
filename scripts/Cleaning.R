library(tidyverse)
library(lubridate)
library(stringr)
library(plyr)

# data <- read_csv("data/NYPD_Complaint_Data_Historic_short.csv")
#https://data.world/data-society/nyc-crime-datacrimes <- data

# Rename cols
names <- c("complaintNum", "complaintDate", "complaintTime", "unknownDate", 
           "unknownTime", "reportedDate", "offenseCode", "offenseDescription", "internalOffenseCode", 
           "internalOffenseDescrption", "completion", "offenseLevel", "jurisdiction", "borough", "precinct", 
           "premise", "premiseDescrption", "parkName", "housingDevelopment", "x", "y", "lat", "long")

colnames(crimes) <- names

# Convert dates into DateTime objects 
crimes$complaintDate <- as.Date(crimes$complaintDate, "%m/%d/%Y")
crimes$unknownDate <- as.Date(crimes$unknownDate, "%m/%d/%Y")
crimes$reportedDate <- as.Date(crimes$reportedDate, "%m/%d/%Y")

# Create new feature describing month in which crime was committed
crimes$Season <- month(as.POSIXlt(crimes$complaintDate, format="%Y/%m/%d"))

crimes$Season[crimes$Season <= 2 | crimes$Season == 12] = "Winter"
crimes$Season[crimes$Season <= 5 & crimes$Season >= 3] = "Spring"
crimes$Season[crimes$Season <= 8 & crimes$Season >= 6] = "Summer"
crimes$Season[crimes$Season <= 11 & crimes$Season >= 9] = "Fall"


# Create new feature describing the number of days between the day a crime was committed and the day that it was reported to police
crimes <- mutate(crimes, reportingDelay = difftime(crimes$reportedDate, crimes$complaintDate, units = "days"))

# Find crimes that were reported late (not on the day they were committed)
crimes %>% 
    group_by(borough) %>%
    summarise(
        count = n()
    )

# Filter crimes by borough 
manhattan <- filter(crimes, borough == "MANHATTAN")
bronx <- filter(crimes, borough == "BRONX")
queens <- filter(crimes, borough == "QUEENS")
brooklyn <- filter(crimes, borough == "BROOKLYN")
staten <- filter(crimes, borough == "STATEN ISLAND")

# Number of felonies committed in Manhattan
filter(manhattan, offenseLevel == "FELONY") %>%
    nrow

# Output car theft and housing development crime .csv files 
carTheft <- crimes %>%
    filter(str_detect(offenseDescription, 'GRAND LARCENY OF MOTOR VEHICLE'))

# write.csv(carTheft, file = "cartheft.csv", sep = ",")

housingDev <- crimes %>% 
    filter(is.na(housingDevelopment)== FALSE)
# write.csv(housingDev, file = "HousingDevelopments.csv", sep = ",")
