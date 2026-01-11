# Packages ----
library(bioRad)
library(getRad)
library(rhdf5)
library(tidyverse)
library(lubridate)
library(zoo)
library(RColorBrewer)
library(patchwork)

# Download Vertical Profiles ----
bioRad::download_vpfiles(
  date_min = '2025-02-14',
  date_max = '2025-02-16',
  radars = c("ptlis"),
  directory = here::here('data-raw'),
  overwrite = T
)

# List h5f files ----
gzf <- list.files(here::here('data-raw'), pattern = '.gz', recursive = T, full.names = T)
tail(gzf)

# Read vertical profiles ----
csv_file <- lapply(gzf, read_csv)

v#pts_file <- read_vpts(csv_file, radar = "KBGM", wavelength = "S")
vpts_file <- read_vpts(csv_file)

bioRad::is.vpts(vpts_file)

# Bind VP into time series  vpts ----
vpf %>%
  bioRad::regularize_vpts() %>%
  plot()

vpts0 <- bind_into_vpts(vpf)

summary(vpts0$data) # data type = list and not numeric

ip_vpts0 <- integrate_profile(vpts0)

plot(ip_vpts0, quantity = "mtr")


ip_vpts1 <-
  integrate_profile(
  regularize_vpts(vpts_list),
  alt_min = 0,
  alt_max = 2000,
  alpha = NA,
  interval_max = Inf
)
#
# plot(ip_vpts1, quantity = "mtr",
#      night_shade = T)

tsReg1 <- regularize_vpts(vpts0)
tsReg1$attributes$how

plot(tsReg1) # ok

plot(
  vpts_list,
  quantity = "dens",
  xlab = NA,
  ylab = "Altitude (m)",
  ylim = c(0, 1000),
)

# Calendar Dates ----
ical = '15-12-2025 00:00:00'
fcal = '30-12-2025 23:59:59'

calendar <- f_calendar(ical, fcal)

bin_cal <- tidyr::crossing(filter(select(calendar, day)),
                           alt = vpts_list$height)

dens <-
  as_tibble(vpts_list$data$dens) %>%
  mutate(alt = vpts_list$height) %>%
  pivot_longer(cols = starts_with("V"), values_to = 'dens') %>%
  mutate(date = rep(vpts_list$datetime, 25),
         day = strftime(date, format="%Y-%m-%d", tz = 'UTC')) %>%
  filter(!is.na(dens)) #%>%

dens %>% filter(day == '2025-12-15') %>% print(n = 100)

dens_cal <-
  bin_cal %>%
  left_join(dens, by = c('day' = 'day', 'alt' = 'alt')) %>%
  filter(alt < 2000) %>%
  mutate(cld = cut(dens, breaks = c(-Inf, 0, 1,2,3,4,5,6,7,8,+Inf),
                   labels = c('0', '1','2','3','4',
                              '5','6','7','8','>8')))
# BirdTAM Scale
BirdTAM <- c('0' = '#FFFFFF', '1' = '#E5FFE5', '2' = '#CCFFCC',
             '3'= '#B2FFB2', '4'='#99FF99',
             '5' = '#99FF99', '6' = '#FFFF00', '7' = '#FF7F00',
             '8' = '#FF0000', '>8' = '#333333')

pw1 <-
  dens_cal %>% filter(alt > 0 & !is.na(alt)) %>%
  #ggplot(aes(x = day, y = alt, z = sdens)) +
  #stat_summary_2d(fun = sum, bins = 8) +
  ggplot() +
  #ggplot(aes(x = day, y = alt, fill = sdens)) +
  geom_tile(aes(x = as.Date(day), y = alt/1000, fill = cld)) +
  scale_fill_manual('BirdTAM', values = BirdTAM, drop = FALSE, na.value = "grey80") +
  scale_y_continuous(breaks = seq(0, 2, by = .2)) +
  scale_x_date(date_breaks = "4 days", date_labels = "%b %d") +
  theme_1 +
  guides(fill = guide_legend(reverse=T, title.position = "right")) +
  labs(title = expression('BirdTAM height profile' ~ birds / km^-3)
       , x = NULL, y = 'height (km)')
pw1

# weather radar MTR
wr_mtr <-
  ip_vpts1 %>%
  select(datetime, mtr) %>%
  mutate(hm =  as.numeric(strftime(datetime, format="%H", tz = 'UTC')),
         day = strftime(datetime, format="%Y-%m-%d", tz = 'UTC')) %>%
  filter(!is.na(mtr)) %>%
  group_by(day, hm) %>%
  summarise(mtr_h = sum(mtr, na.rm = T), .groups = 'drop') %>%
  group_by(day) %>%
  summarise(mtr = mean(mtr_h, na.rm = T),
            se = sd(mtr_h)/sqrt(n()), .groups = 'drop')

wr_mtr <- calendar %>% left_join(wr_mtr, by = c('day' = 'day'))
wr_mtr %>% print(n=100)

pw2 <-
  ggplot(aes(x = data, y = mtr), data = wr_mtr) +
  geom_bar(fill = "#00AFBB", stat = 'identity') +
  geom_errorbar(aes(ymin=mtr, ymax=mtr+se), width=0, colour = "#21908CFF") +
  #geom_point(colour = "#00AFBB", size = 2.5) +
  #scale_y_continuous(breaks = seq(0, 1000, by = 200)) +
  scale_x_date(date_breaks = "4 days", date_labels = "%b %d") +
  theme(
    legend.position="bottom",
    legend.key.width = unit(0.5, "cm"),  # symbol size
    legend.key.height = unit(0.3, "cm"), # symbol size
    legend.text = element_text(size = 7, color = "white"),
    legend.background =  element_rect(colour = "grey20", fill = "#1e1e1e"),
    legend.title = element_text(size = 7, color = "white"),
    legend.key = element_blank(),
    strip.background =element_rect(fill="grey20"),
    strip.text = element_text(colour = 'white'),
    axis.text.y = element_text(size = 6, color = "white"),
    axis.text.x = element_text(angle = 90, vjust= 0.5,
                               size = 6, color = "white"),
    axis.line = element_line(color = "grey20", size = .01),
    axis.title = element_text(size = 6, color = "white"),
    plot.title = element_text(size = 6, color = "white"),
    panel.grid.major.x = element_line(colour = "grey20",size=0.01),
    panel.grid.minor.x = element_line(colour = "grey20",size=0.01),
    panel.grid.major.y = element_line(colour = "grey20",size=0.01),
    panel.grid.minor.y = element_line(colour = "grey20",size=0.01),
    #panel.grid = element_line(colour = "white",size=2),
    #panel.border = element_rect(fill = NA, colour = "#1e1e1e", size = 2),
    panel.background = element_rect(colour = "grey20", fill = "#1e1e1e"),
    plot.background = element_rect(colour = "grey20", fill = "#1e1e1e")
  ) +
  labs(title = 'MTR',
       y = expression(atop('MTR '~ birds~km^-1~ h^-1, '(mean + se)'))
       #expression(atop('TPR' ~ h^-1, '(mean + se)'))
       , x = NULL)
pw2

pw1/pw2

ggsave(
  filename = here::here('outputs', 'birdTam_mtr.png'),
  plot = pw1/pw2,
  dpi = 300,
  units = 'cm',
  height = 12,
  width = 12
)

# Load radar data ----
# Source aux data & functions ----
source('D:/Dropbox/programacao/GtruthRadar/R/aux_func.R')

list_tbl <- read_rds('D:/Dropbox/programacao/GtruthRadar/data/lsummarioglobaljul2020_jan2021.R')

penram <- bind_rows(list_tbl$tpr) %>%
  filter(minfunc > 30 & dia > '2020-11-30') %>%
  ggplot(aes(dia, tpr)) +
  # stat_summary(fun.data = 'mean_cl_normal', na.rm = T, size = 1, fatten = 1,
  #              geom = "pointrange", colour = "#00AFBB",
  #              fun.args = list(mult = 1)) +
  stat_summary(fun = mean, na.rm = T, geom = "bar", fill = "#00AFBB") +
  stat_summary(fun.data = mean_cl_normal, na.rm = T, geom = "errorbar",
               fun.args = list(mult = 1), color = "#00AFBB", size = .2 ) +
  scale_y_continuous(expand = c(0.01,0)) +
  scale_x_date(date_breaks = '5 days', date_labels = "%b %d") +
  labs(y = expression(atop('TPR' ~ h^-1, '(# unique track_id / hour)'))
       ,x = NULL , title = 'Dialy TPR for VSR') +
  theme_tpr
pw1/pw2/penram

ggsave(
  filename = here::here('outputs/teste_mtr_weatherradar_merlin.png'),
  plot = pw1/pw2/penram,
  dpi = 400,
  units = 'cm',
  height = 20,
  width = 16
)

