# theme por plot
theme_1 <-
  theme(
    legend.direction = 'vertical',
    #legend.position="right",
    legend.key.width = unit(0.5, "cm"),  # symbol size
    legend.key.height = unit(0.3, "cm"), # symbol size
    legend.text = element_text(size = 7, color = "white"),
    legend.background =  element_rect(colour = "grey20", fill = "#1e1e1e"),
    legend.title = element_text(size = 7, color = "white",angle = 90),
    legend.key = element_blank(),
    strip.background =element_rect(fill="grey20"),
    strip.text = element_text(colour = 'white'),
    axis.text.y = element_text(size = 6, color = "white"),
    axis.text.x = element_text(angle = 90, vjust= 0.5,
                               size = 6, color = "white"),
    axis.line = element_line(color = "grey20", linewidth = .01),
    axis.title = element_text(size = 6, color = "white"),
    plot.title = element_text(size = 6, color = "white"),
    panel.grid.major.x = element_line(colour = "grey20",linewidth=0.01),
    panel.grid.minor.x = element_line(colour = "grey20",linewidth=0.01),
    panel.grid.major.y = element_line(colour = "grey20",linewidth=0.01),
    panel.grid.minor.y = element_line(colour = "grey20",linewidth=0.01),
    #panel.grid = element_line(colour = "white",size=2),
    #panel.border = element_rect(fill = NA, colour = "#1e1e1e", size = 2),
    panel.background = element_rect(colour = "grey20", fill = "#1e1e1e"),
    plot.background = element_rect(colour = "grey20", fill = "#1e1e1e")
  )

# build calendar ----
f_calendar <- function(ical, fcal){
  require(lubridate)
  require(zoo)
  require(tidyverse)
  #require(tidyverse)
  ical <- tibble(data = seq.Date(as.Date(ical, '%d-%m-%Y'),
                                     as.Date(fcal, '%d-%m-%Y'), by = '1 day')) %>%
    mutate(
      #finding the day no. of the week
      weekday = lubridate::wday(data, week_start = getOption("lubridate.week.start", 1)),
      #converting the day no. to factor
      weekdayf = factor(weekday,levels=rev(1:7),
                        labels=rev(c("Seg","Ter","Qua","Qui","Sex","Sab","Dom")),
                        ordered=TRUE),
      # finding the month
      monthf = factor(month(data),levels=as.character(1:12),
                      labels=c("Jan","Fev","Mar","Abr","Mai","Jun","Jul",
                               "Ago","Set","Out","Nov","Dez"),
                      ordered=TRUE),
      #finding the year and the month from the date. Eg: Sep 2020
      yearmonth = factor(as.yearmon(data)),
      #finding the week of the year for each date
      week = as.numeric(format(data,"%W")),
      day = strftime(data, format="%Y-%m-%d", tz = 'UTC')
    )

  return(ical)
}
