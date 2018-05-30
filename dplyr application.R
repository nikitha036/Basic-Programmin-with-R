# Using dplyr and readr functionalities application

# Installing packages for readr and loading the library

install.packages("readr")
library("readr")

# Installing packages for dplyr and loading the library

install.packages("dplyr")
library("dplyr")

# Installing packages for ggplot and loading the library

install.packages("ggplot2")
library("ggplot2")

# The data is imported into the tibble d from a CSV file 

d <- read_csv("F:/Assignments/Programming for Data Analytics/ExamDataNarrow.csv")
d

# Results are placed in an overall band

d <- mutate(d,MarksBand = 
         case_when(Grade >= 70 ~ "H1", Grade >= 60 & Grade < 70 ~ "H2.1", 
                   Grade >= 50 & Grade < 60 ~ "H2.2", Grade >= 40 & Grade < 50 ~ "Pass", 
                   Grade >=35 & Grade < 40 ~ "Comp", Grade < 35 ~ "Fail"))
d

# Graph is plotted to visualize the subject details

ggplot(data = d) + geom_point(mapping = aes(x=StudentID,y=Grade, colour = MarksBand)) + facet_wrap(~Subject)

# Summarizing & displaying the student details

StudentDetails <- d %>% group_by(StudentID) %>%
        summarize(Average = mean(Grade,na.rm=T), Range = (max(Grade)-min(Grade)), 
                  NumH1.1 = sum(MarksBand == "H1"), NumH2.1 = sum(MarksBand == "H2.1"), 
                  NumH2.2 = sum(MarksBand == "H2.2"), NumPass = sum(MarksBand == "Pass"),
                  NumComp = sum(MarksBand == "Comp"), NumFail = sum(MarksBand == "Fail")) %>%
  arrange(desc(Average))

StudentDetails

# Summarizing & displaying the subject details

SubjectDetails <- d %>% group_by(Subject) %>%
  summarize(Average = mean(Grade,na.rm=T), Range = (max(Grade)-min(Grade)), 
            NumH1.1 = sum(MarksBand == "H1"), NumH2.1 = sum(MarksBand == "H2.1"), 
            NumH2.2 = sum(MarksBand == "H2.2"), NumPass = sum(MarksBand == "Pass"),
            NumComp = sum(MarksBand == "Comp"), NumFail = sum(MarksBand == "Fail")) %>%
  arrange(desc(Average))

SubjectDetails