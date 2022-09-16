library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(readr)
library(lubridate)
library(ggplot2movies)
library(extrafont)
loadfonts(device = "win")

letra <- "Tahoma"
base <- movies %>%
  select(-c(r1:r10)) %>%
  gather(genre, value, Action:Short) %>%
  mutate(budget = budget/1000000) %>%
  filter(value == 1, !is.na(budget), genre != "Short", length >= 60)


ggplot(base) +
  geom_point(aes(length, rating, size = budget, color = factor(genre)),
             alpha = .5) +
  scale_color_manual(values = c(
    "#78FF7D",
    "#85FFED",
    "#766CFF",
    "#FF4A97",
    "#FF8D5E",
    "#FFDC2A")
  ) +
  scale_y_continuous(breaks = trans_breaks(identity, identity, n = 10)) +
  scale_x_continuous(breaks = trans_breaks(identity, identity, n = 10)) +
  theme(
    panel.grid.major.y  = element_line(colour = "#66656a", size = .1, 
                                       linetype = 'dotted'),
    panel.grid.minor.y  = element_blank(),
    panel.grid.major.x = element_line(colour = "#66656a", size = .1, 
                                      linetype = 'dotted'),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = '#29282e', colour = "#29282e"),
    plot.background = element_rect(fill = "#29282e"),
    axis.line = element_line(colour = 'white'), 
    axis.ticks = element_line(colour = 'white'),
    plot.title = element_text(hjust = .5, family = letra),
    plot.caption = element_text(hjust =0, family = letra),
    axis.text = element_text(colour = "#929197", size = 10, family = letra),
    axis.title = element_text(colour = "#bebdc6", size = 13, family = letra),
    legend.background = element_rect(fill = '#29282e'),
    legend.box.background = element_rect(fill = '#29282e', colour = '#29282e'),
    legend.key = element_rect(fill = '#29282e', color = '#29282e'),
    legend.title = element_text(size = 15, 
                                color = '#bebdc6',
                                hjust = .5, 
                                family = letra),
    legend.text = element_text(size = 11, 
                               color = '#bebdc6'),
    title = element_text(size = 16, colour = "#bebdc6", family = letra),
    plot.subtitle = element_text(size = 11, colour = "#bebdc6", hjust = .5, family = letra)
    
  ) +
  labs(x = "Duración (minutos)", y = "Calificación", 
       title = "Base de películas IMDB",
       subtitle = "1913 - 2005",
       color = "Género",
       size = "Presupuesto (MDD)") + 
  guides(size=guide_legend(override.aes=list(colour="#bebdc6")),
         color = guide_legend(order = 1, override.aes=list(size = 4))) +
  scale_size_continuous(range = c(1, 5))

ggsave("C:/Users/marco/OneDrive/Documentos/.R/30daysgraphs/3/g1.png", width = 13, height = 8)

# Máximas
base[which(base$budget==max(base$budget)), ]
base[which(base$length==max(base$length)), ] 
base[which(base$rating==max(base$rating)), ]

# Mínimas

base[which(base$budget==min(base$budget)), ]
base[which(base$rating==min(base$rating)), ]

# Deflactado?

# Promedio por género
mean(base[base$genre=="Action",]$rating)
mean(base[base$genre=="Animation",]$rating)
mean(base[base$genre=="Comedy",]$rating)
mean(base[base$genre=="Documentary",]$rating)
mean(base[base$genre=="Drama",]$rating)
mean(base[base$genre=="Romance",]$rating)
