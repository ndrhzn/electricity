library(ggplot2)
library(scales)
library(stringr)
source('00_parse_data.R')

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
       subtitle = str_wrap('Кожна точка на графіку — один день у 2016 році. Позиція точки визначається середньою температурою в цей день (вісь Х, градуси Цельсія) та обсягом споживання електроенергії (вісь Y, ГВт⋅год)', 110),
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