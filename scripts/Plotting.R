library(tidyverse)

# Plot different levels of offense, fill by completion
ggplot(crimes, aes(offenseLevel)) + 
    geom_bar(aes(fill = completion)) +
    labs(x = "Level of Offense", y = "Count", title = "Level of Offense by Completion")

# number of felonies attempted is 37456
crimes %>%
    filter(offenseLevel == "FELONY", completion == "ATTEMPTED") %>%
    nrow()

# number of felonies completed is 869596
crimes %>%
    filter(offenseLevel == "FELONY", completion == "COMPLETED") %>%
    nrow()
### percentage attempted out of total is 4.1%

# number of misdemeanors attempted 14272
crimes %>%
    filter(offenseLevel == "MISDEMEANOR", completion == "ATTEMPTED") %>%
    nrow()
# number of misdemeanors completed is 1701129
crimes %>%
    filter(offenseLevel == "MISDEMEANOR", completion == "COMPLETED") %>%
    nrow()

### percentage out of total is .83% 


# number of violations attempted 985
crimes %>%
    filter(offenseLevel == "VIOLATION", completion == "ATTEMPTED") %>%
    nrow()
# number of violations completed is 356070
crimes %>%
    filter(offenseLevel == "VIOLATION", completion == "COMPLETED") %>%
    nrow()

### percentage out of total is .27%


# Analysis of reportingDelay 
longCrimes <- filter(crimes, reportingDelay < 400 & reportingDelay > 0)

ggplot(longCrimes, aes(x = reportingDelay)) + 
    geom_bar(aes(fill = offenseLevel)) +
    labs(x = "Reporting Delay", y = "Count", title = "Reporting Delay Over Time")

# However, at one year exactly, the number of crimes reported drastically increase
subtractLong <- filter(crimes, reportingDelay < 380 & reportingDelay > 350)
ggplot(subtractLong, aes(x = reportingDelay)) + 
    geom_bar(aes(fill = offenseLevel)) +
    labs(x = "Reporting Delay", y = "Count", title = "Reporting Delay Over Time")

## Number of crimes reported at exactly one year 

crimes %>%
    filter(reportingDelay == 364) %>%
    nrow()

crimes %>%
    filter(reportingDelay == 365) %>% 
    nrow()

crimes %>%
    filter(reportingDelay == 366) %>%
    nrow()


# Percentages of assaults in Housing developments 

filter(crimes, is.na(housingDevelopment) == FALSE,
       str_detect(offenseDescription, 'ASSAULT 3')) %>%
    nrow()

filter(crimes, is.na(housingDevelopment) == FALSE) %>% 
    nrow()

filter(crimes, is.na(housingDevelopment) == TRUE,
       str_detect(offenseDescription, 'ASSAULT 3')) %>%
    nrow()

filter(crimes, is.na(housingDevelopment) == TRUE) %>%
    nrow()

filter(crimes, reportingDelay != 0) %>% 
    nrow()
