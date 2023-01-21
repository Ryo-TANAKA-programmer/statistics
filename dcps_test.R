###
setwd("~/Grad School/SIS_AU/R/Working Directory")
library(tidyverse)
library(ggplot2)
dcps <- read_csv('DCPS_data.csv')

dcps %>% 
filter(race == 'White') %>% 
  summarise(mean = mean(grad_rate),
            sd = sd(grad_rate))

white_dcps <- dcps %>% 
  filter(race == 'White')
asian_dcps <- dcps %>% 
  filter(race == 'Asian')
black_dcps <- dcps %>% 
  filter(race == 'Black')
hispa_dcps <- dcps %>% 
  filter(race == 'Hispanic')

new_dcps <- dcps %>% 
  filter(race %in% c('Asian','White','Black','Hispanic'))

new_dcps %>%
  group_by(year) %>%
  ggplot() +
  geom_line(aes(x = year,
                y = grad_rate),
            lwd = 1.2,
            color = 'darkgreen') +
  theme_minimal() +
  labs(title = 'DC Public School Graduation Rate')

graph_grad_rate <- new_dcps %>%
  group_by(year) %>%
  ggplot(aes(x = year,
             y = grad_rate)) +
  geom_point(aes(x = year,
                y = grad_rate,
            color = race)) +
  geom_line(aes(color = race),
            size = 0.8) +
  labs(title = 'DC Public School Graduation Rate',
       caption = 'data:https://dcps.dc.gov/service/school-data',
       x = 'Year',
       y = 'Graduation Rate') +
  theme_minimal()
ggsave('graph_grad_rate.png')
