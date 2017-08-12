library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(stringr)

parse_data <- function(year){
  
  if(year ==  2016) {
    
    url = 'http://data.gov.ua/sites/default/files/media//2782/11.08.2017/dg_year_2016_16.csv'
    
  } else if (year == 2017) {
    
    url = 'http://data.gov.ua/sites/default/files/media//2782/11.08.2017/dg_year_15.csv'
    
  }
  
  df <- read.csv(url, stringsAsFactors = F)[,c(1,8)]
  names(df) <- c('datetime', 'consumption')
  
  df <- df %>% 
    mutate(datetime = as.POSIXct(strptime(datetime, format = '%d.%m.%Y %H:%M:%S', tz = 'UTC+2')),
           month = month(datetime, label = T, abbr = F),
           day = mday(datetime),
           yday = yday(datetime),
           wday = wday(datetime, label = T, abbr = F),
           wday = factor(wday, ordered = T, 
                         levels = c('Monday', 'Tuesday', 'Wednesday', '
                                    Thursday', 'Friday', 'Saturday', 'Sunday')),
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

consumption <- parse_data(year = 2016)

consumption <- consumption %>% 
  mutate(date = as.Date(datetime)) %>% 
  group_by(date) %>% 
  summarise(consumption = sum(consumption)) %>% 
  arrange(date)

temp <- read.csv('data/1043337.csv', stringsAsFactors = F)
temp <- temp %>% 
  mutate(DATE = as.Date(DATE)) %>%
  group_by(DATE) %>% 
  summarise(tavg = mean(TAVG, na.rm = T)) %>% 
  arrange(DATE) %>% 
  cbind.data.frame(consumption) %>% 
  filter(date < as.Date('2016-12-31'))

annotations <- data.frame(
  
  x = c(-10.25, -13.25, 16.25, 20),
  y = c(363439, 442786, 306250, 450000),
  label = c('1 січня', '2 січня', '1 і 2 травня', 
            str_wrap('Зі зростанням температури споживання електроенергії зменшується. Однак у літні місяці — особливо у липні — зростання температури результує у збільшенні споживання електроенергії внаслідок використання кондиціонерів та вентиляторів', 32))
  
)

png('weather_vs_consumption.png', width = 1000, height = 800)

ggplot(temp, aes(x = tavg, y = consumption))+
  geom_point(color = '#ce1256', alpha = 0.7, size = 4)+
  geom_smooth(se = F, color = '#88419d', size = 2)+
  geom_label(data = annotations, 
            aes(x = x, y = y, label = label, family = 'Ubuntu Condensed'),
            fill = '#EFF2F4', color = '#3A3F4A', 
            label.size = 0, label.padding = unit(0.05, 'lines'),
            size = 6, inherit.aes = F)+
  scale_x_continuous(breaks = seq(-15, 25, 5), expand = c(0.01, 0), limits = c(-18, 27))+
  scale_y_continuous(breaks = seq(300000, 500000, 50000), 
                     labels = unit_format(unit = "", scale = 1e-3, digits = 1))+
  labs(title = 'Погода та споживання електроенергії', 
       subtitle = str_wrap('Кожна точка на графіку - один день у 2016 році. Позиція точки визначається середньою температурою в цей день (вісь Х, градуси Цельсія) та обсягом споживання електроенергії (вісь Y, тисячі МВт)', 110),
       caption = 'Дані: НЕК Укренерго, 2016 рік | Візуалізація: Textura.in.ua')+
  theme_minimal()+
  theme(text = element_text(family = 'Ubuntu Condensed', face = 'plain', color = '#3A3F4A'),
        legend.position = 'none',
        axis.title = element_blank(),
        axis.text = element_text(size = 18),
        panel.grid.major = element_line(linetype = 'dotted', size = 0.35, color = '#5D646F'),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = 'bold', size = 44, margin = margin(b = 20, t = 10)),
        plot.subtitle = element_text(size = 19, face = 'plain'),
        plot.caption = element_text(size = 16, margin = margin(b = 20, t = 30), color = '#5D646F'),
        plot.background = element_rect(fill = '#EFF2F4'),
        plot.margin = unit(c(2, 2, 2, 2), 'cm'))

dev.off()