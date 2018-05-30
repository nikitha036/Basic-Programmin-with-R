# Installing packages for readr and loading the library

install.packages("readtext")
library("readtext")

# Installing packages for dplyr and loading the library

install.packages("dplyr")
library("dplyr")

# Installing packages for ggplot and loading the library

install.packages("ggplot2")
library("ggplot2")

# Installing packages for stringr and loading the library

install.packages("stringr")
library("stringr")

# Installing packages for tidyr and loading the library

install.packages("tidyr")
library("tidyr")

# Reading the lines from the test file

f <- readLines("F:/Assignments/Programming for Data Analytics/J Joyce.txt")
f

# Gives the total number of lines
length(f)

# Function to remove the empty lines

remove_blank_lines <- function(x)
  {
   x[x==""] <- NA
   x <- na.omit(x)
   x
  # return(x)
}

# Gives the total number of lines after removing the empty lines

length(remove_blank_lines(f))
f <- remove_blank_lines(f)

# Function extracts the words separately, converts them into lower case, removes all the speacial character mentioned

prepare_line <- function(x)
{
  x <- unlist(str_extract_all(str_to_lower(x), "[^ ]+"))
  x <- str_replace_all(x,str_c(c("[?.:',]" ,"(--)","said"),collapse = "|"),"")
  remove_blank_lines(x)
}

f[3]
prepare_line(f[3])

# Above function is processed for every line and the individual terms are taken and stored in "terms"

terms <- unlist(lapply(f,prepare_line))
str(terms)

terms[1:20]

# A tibble is formed with the list of all the words taken above and renaming it as given("Words")

tibble_term <- as_tibble(terms)
tibble_term <- rename(tibble_term,Words = value)

# Three additional columns viz., Pattern, WLength(word length), WFrequency(Word frequency) is added
# Unique words are grouped and sorted by Word Frequency in desending order

tibble_term <- tibble_term %>% mutate(Pattern = str_c("^",Words,"$"),WLength = str_length(Words)) %>% 
     group_by(Words) %>% mutate( WFrequency = sum(str_detect(Words, pattern = Pattern ))) %>%  
     distinct(Words, Pattern, WLength, WFrequency) %>% arrange(desc(WFrequency))

tibble_term

# A new tibble with Word lengths greater than 3 is filtered out

summ <- tibble_term %>% filter(WLength > 3) %>% select(Words,WLength,WFrequency)
summ

# A graph is plotted with Word Length Vs Word Frequency taken from the tibble "tibble_term"

ggplot(data = tibble_term) + geom_point(position = "jitter",mapping = aes(x = WLength,y = WFrequency)) + 
  xlab(" Word Length") + ylab("Word Frequency")
