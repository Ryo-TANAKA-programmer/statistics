###
setwd("~/Grad School/SIS_AU/R/Working Directory")
library(tidyverse)
###

metro <- read.csv('metro.csv')

metro %>% 
  summarise(mean = mean(time,na.rm = T),
            sd = sd(time,na.rm = T),
            max = max(time),
            min = min(time))

ggplot(data = metro,
       aes(x = time)) +
  geom_density(color = 'black',
                 fill = 'green',
               alpha = 0.5) +
  theme_classic() +
  labs(title = 'WMATA Bus Waiting Time',
       x = 'Waiting Time (minute)',
       y = ' ',
       caption = 'data: Ryo.Tanaka') +
  annotate('text',
           x = 14,
           y = 0.08,
       label = 'Mean of the waiting time: 6.56 min
Standard deviation: 4.91 min',
       col = 'red',
       size = 5)

ggsave('wmata.png')

