library(ggplot2)
library(lubridate)
library(tidyr)

g <- ggplot(data = testing, aes(x = `MONTH`, y = `CRIME NUM`)) +
  geom_bar(stat = "identity")
g


set.seed(49)
dat1 = data.frame(date = seq(as.Date("2015-01-15"), as.Date("2015-12-15"), "1 month"),
                  value = cumsum(rnorm(12)))
dat1$date = as.yearmon(dat1$date)

dat2 = data.frame(date = seq(as.Date("2016-01-15"), as.Date("2016-12-15"), "1 month"),
                  value = cumsum(rnorm(12)))
dat2$date = as.yearmon(dat2$date) 

ggplot(crimeData$`DATE`, aes(month(`DATE`, label=TRUE, abbr=TRUE), 
                             value, group=factor(year(date)), colour=factor(year(date)))) +
  geom_line() +
  geom_point() +
  labs(x="Month", colour="Year") +
  theme_classic()


qplot(crimeData$CRIME, xlab = "Crime", main = "Crimes in Chicago") + 
  scale_y_continuous("Number of crimes")

testing <- crimeData %>% filter(CRIME %in% input$crime_Selector)

tbl_df(crimeData)
