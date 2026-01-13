# Download, Process and Plot Vertical Profile data from Aloft Repo
# 2026-01-11

# https://aloftdata.github.io/getRad/
# https://aloftdata.github.io/getRad/articles/supported_sources.html#vertical-profile-time-series-vpts-data
# https://www.nature.com/articles/s41597-025-04641-5
# https://github.com/aloftdata/data-repository/wiki

# Packages ----
#install.packages('pacman')
pacman::p_load(
  'bioRad',
  'getRad',
  'tidyverse',
  'lubridate',
  'zoo',
  'RColorBrewer',
  'patchwork')

# Source functions and Defs
source(here::here('R', 'aux_func.R'))

# Calendar Dates ----
ical = '2025-07-01 00:00:00'
fcal = '2026-01-01 23:59:59'

# Get vertical profile for lis radar
vpts_list <-
  get_vpts(
  radar = c("ptlis"),
  datetime = lubridate::interval(
    lubridate::as_datetime(ical),
    lubridate::as_datetime(fcal)
  ),
  source = "baltrad"
)

vpts_list$attributes
vpts_list$attributes$how

ni <- names(vpts_list$radar)
#plot(regularize_vpts(vpts_list), main = ni)

plot(
  regularize_vpts(vpts_list),
  quantity = "dens",
  xlab = NA,
  ylab = "Altitude (m)",
  ylim = c(0, 2000),
)

plot(
  vpts_list,
  quantity = "dens",
  xlab = NA,
  ylab = "Altitude (m)",
  ylim = c(0, 2000),
)

ip_vpts1 <-
  integrate_profile(
    regularize_vpts(vpts_list),
    alt_min = 0,
    alt_max = 2000,
    alpha = NA,
    interval_max = Inf
  )

# Calendar Dates ----
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

#dens %>% filter(day == '2025-12-15') %>% print(n = 100)

dens_cal <-
  bin_cal %>%
  left_join(dens, by = c('day' = 'day', 'alt' = 'alt')) %>%
  filter(alt < 2000) %>%
  mutate(cld = cut(dens, breaks = c(-Inf, 0, 1,2,3,4,5,6,7,8,+Inf),
                   labels = c('0', '1','2','3','4',
                              '5','6','7','8','>8')))
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
#pw1

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
  theme_1 +
  labs(title = 'MTR',
       y = expression(atop('MTR '~ birds~km^-1~ h^-1, '(mean + se)'))
       #expression(atop('TPR' ~ h^-1, '(mean + se)'))
       , x = NULL)
#pw2

pw1/pw2

ggsave(
  filename = here::here('outputs', 'birdTam_mtr_dec2025.png'),
  plot = pw1/pw2,
  dpi = 300,
  units = 'cm',
  height = 12,
  width = 12
)
