# Installing packages for readr and loading the library

install.packages("readr")
library("readr")

# Installing packages for dplyr and loading the library

install.packages("dplyr")
library("dplyr")

# Installing packages for ggplot and loading the library

install.packages("ggplot2")
library("ggplot2")

# The wave data is imported into the tibble d from a CSV file 

waveTable <- read_csv("Wave.csv")
waveTable

# The weather data is imported into the tibble d from a CSV file 

weatherTable <- read_csv("Weather.csv")
weatherTable

# Finding the average of Height by grouping the tibble based on Date and Hour

WaveHeight <- group_by(waveTable,Date,Hour) %>% summarise(avrMin = mean(Havg_m))
WaveHeight

# Finding the average of Speed by grouping the tibble based on Date and Hour 

WeatherSpeed <- group_by(weatherTable,Date,Hour) %>% summarise(avrSpeed = mean(`Wind Speed (m/s)`))
WeatherSpeed

# Joining the above two tibbles to plot them on a graph

tablemergerd <- inner_join(WaveHeight,WeatherSpeed)
tablemergerd

# The newly joined tibbled has been plotted to explore the relationship b/w wind speed and wave height

ggplot(data=tablemergerd,mapping = aes(x=avrSpeed,y=avrMin)) +
  geom_point()+
  geom_smooth(method='lm') +
  xlab("Wind Speed (average per hour) m/s") + ylab("Average Wave Height (per hour)m")