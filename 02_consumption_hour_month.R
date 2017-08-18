library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

parse_data <- function(year){
  
  if(year ==  2016) {
    
    url = 'http://data.gov.ua/sites/default/files/media//2782/11.08.2017/dg_year_2016_16.csv'
    
  } else if (year == 2017) {
    
    url = 'http://data.gov.ua/sites/default/files/media//2782/11.08.2017/dg_year_15.csv'
    
  }
  
  df <- read.csv(url, stringsAsFactors = F)[,c(1,8)]
  names(df) <- c('datetime', 'consumption')
  
  df <- df %>% 
    mutate(datetime = as.POSIXct(strptime(datetime, format = '%d.%m.%Y %H:%M:%S'), tz = 'UTC+02'),
           month = month(datetime, label = T, abbr = F),
           day = mday(datetime),
           yday = yday(datetime),
           wday = wday(datetime, label = T, abbr = F),
           wday = factor(wday, ordered = T, 
                         levels = c('Monday', 'Tuesday', 'Wednesday', 
                                    'Thursday', 'Friday', 'Saturday', 'Sunday')),
           hour = hour(datetime)) %>% 
    filter(consumption != 0)
  
  df$season <- case_when(
    
    df$month %in% c('January', 'February', 'December') ~ 'winter',
    df$month %in% c('March', 'April', 'May') ~ 'spring',
    df$month %in% c('June', 'July', 'August') ~ 'summer',
    df$month %in% c('September', 'October', 'November') ~ 'autumn'
    
  )
  
  df$season <- factor(df$season, ordered = T, 
                      levels = c('winter', 'spring', 'summer', 'autumn'))
  
  return(df)
  
}

df <- parse_data(year = 2016)

df <- df %>% 
  group_by(season, month, hour) %>% 
  summarise(consumption = mean(consumption))%>% 
  ungroup() %>% 
  mutate(month = forcats::fct_recode(month, 
                                     січень = 'January', лютий = 'February',
                                     березень = 'March', квітень = 'April',
                                     травень = 'May', червень = 'June',
                                     липень = 'July', серпень = 'August',
                                     вересень = 'September', жовтень = 'October',
                                     листопад = 'November', грудень = 'December'))
  
months <- df %>% 
  filter(hour == 23) %>% 
  group_by(month) %>% 
  mutate(consumption = mean(consumption, na.rm = T)) %>% 
  select(month, hour, season, consumption) %>% 
  unique.data.frame()

# annotations <- data.frame(
#   
#   hour = c(12, 12),
#   consumption = c(18200, 20500),
#   label = c('у літні місяці споживання електроенергії впродовж робочого дня, навпаки, збільшується',
#             'взимку, навесні та восени впродовж робочого дня споживання електроенергії зменшується'),
#   season = c('summer', 'spring'),
#   stringsAsFactors = T
#   
# )


png('consumption_by_month&hour.png', width = 1000, height = 900)

ggplot(df, aes(x = hour, y = consumption, group = month, color = month))+
  geom_path(size = 1, alpha = 0.75)+
  geom_point(data = months, size = 3.5)+
  geom_text(data = months, 
            aes(x = 24.5, label = month, family = 'Ubuntu Condensed'), size = 6, fontface = 'plain', hjust = 0)+
  # geom_label(data = annotations, 
  #           aes(x = hour, y = consumption, label = stringr::str_wrap(label, 20), 
  #               family = 'Ubuntu Condensed'), fill = '#EFF2F4',
  #           label.size = 0, label.padding = unit(0.05, 'lines'),
  #           size = 5, inherit.aes = F)+
  scale_x_continuous(breaks = c(0, 10, 18, 24),
                     limits = c(0, 36), minor_breaks = NULL)+
  scale_y_continuous(breaks = seq(12000, 20000, 2000), 
                     labels = unit_format(unit = "", scale = 1e-3, digits = 1))+
  scale_color_manual(values = c('грудень' = '#8e0152', 'січень' = '#c51b7d', 'лютий' = '#de77ae',
                                'березень' = '#8e0152', 'квітень' = '#c51b7d', 'травень' = '#de77ae',
                                'липень' = '#8e0152', 'червень' = '#c51b7d', 'серпень' = '#de77ae',
                                'листопад' = '#8e0152', 'жовтень' = '#c51b7d', 'вересень' = '#de77ae'))+
  facet_wrap(~season, ncol = 4)+
  labs(title = 'Споживання електроенергії впродовж дня', 
       subtitle = stringr::str_wrap('Лінії на графіку позначають середньомісячний обсяг споживання електроенергії (ГВт⋅год) в Україні у 2016 році в розрізі годин доби та місяців', 108),
       caption = 'Дані: НЕК Укренерго, 2016 рік | Візуалізація: Textura.in.ua')+
  theme_minimal()+
  theme(text = element_text(family = 'Ubuntu Condensed', face = 'plain', color = '#3A3F4A'),
        legend.position = 'none',
        axis.title = element_blank(),
        axis.text = element_text(size = 18),
        panel.grid.major = element_line(linetype = 'dotted', size = 0.35, color = '#5D646F'),
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        panel.spacing.x = unit(2, 'lines'),
        plot.title = element_text(face = 'bold', size = 44, margin = margin(b = 20, t = 10)),
        plot.subtitle = element_text(size = 20, face = 'plain', margin = margin(b = 20)),
        plot.caption = element_text(size = 16, margin = margin(b = 20, t = 40), color = '#5D646F'),
        plot.background = element_rect(fill = '#EFF2F4'),
        plot.margin = unit(c(2, 2, 2, 2), 'cm'))

dev.off()