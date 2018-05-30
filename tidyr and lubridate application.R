# Installing packages for readr and loading the library

install.packages("readr")
library("readr")

# Installing packages for dplyr and loading the library

install.packages("dplyr")
library("dplyr")

# Installing packages for tidyr and loading the library

install.packages("tidyr")
library("tidyr")

# Installing packages for lubridate and loading the library

install.packages("lubridate")
library("lubridate")

# The Process Mining data is imported into the tibble d from a CSV file 

ProcessMining <- read.csv("F:/Assignments/Programming for Data Analytics/process_mining.csv")
ProcessMining

# The process mining data is separated into individual columns to handle the data 

SeparatedData <- ProcessMining %>% separate(CaseID.EventID.TimeStamp.Activity.Resource.Costs,
                                            into=c("CaseID","EventID","TimeStamp","Activity","Resource","Cost"),sep = ";")
SeparatedData

# Calculating the duration for each activity and removing the activity for which the duration could not be calculated 
# as it is the first data in the group(splited based on case ID)

SeparatedData <- mutate(SeparatedData,
                        TimeDifference = difftime(dmy_hm(SeparatedData$TimeStamp),dmy_hm(lag(SeparatedData$TimeStamp)),
                        units = "days"))

# Summarizing the data to get the average, standard deviation, maximum, minimum duration for each activity

act <- SeparatedData %>% group_by(Activity) %>% summarise(AverageDuration=as.numeric(mean(TimeDifference)),
                                                          SD=as.numeric(sd(TimeDifference)),
                                                          MaxTime=as.numeric(max(TimeDifference)),
                                                          MinTime=as.numeric(min(TimeDifference))) %>% 
                                                          arrange(desc(AverageDuration)) %>% 
                                                          na.omit(SeparatedData)
act
