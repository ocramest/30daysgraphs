#https://www.iucnredlist.org/

library(dplyr)
library(ggplot2)
library(tidyr)
library(extrafont)
loadfonts(device = "win", quiet = TRUE)

letra <- "Tahoma"

base <- read.csv("C:/Users/marco.esteban.DOMINIOIIEG/Downloads/2020_1_RL_Stats_Table_5.csv",
                 stringsAsFactors = F) %>% tbl_df()

names(base)[length(base)-3] <- "Other inverts"
names(base)[length(base)-1] <- "Fungi & Protists"

species <- base %>%
  filter(Continent != "Antarctica") %>%
  mutate(Continent = as.factor(Continent),
         Country = as.factor(Country)) %>%
  select(-c(Country, Total))

for(i in 2:10){
  species[,i] <- as.numeric(unlist(species[,i]))
}

species[is.na(species)] <- 0


thrspcs <- species %>%
  gather(Class, Value, Mammals:`Fungi & Protists`) %>%
  group_by(Continent, Class) %>%
  summarise(Total = sum(Value)/1000)

#thrspcs <- thrspcs[10:63,]

g1 <- ggplot(thrspcs) +
  geom_bar(aes(Class, Total, fill = Class),
           stat = "identity", width = .5, color = "black", alpha = .7) +
  facet_grid(.~Continent) +
  coord_flip() +
  scale_fill_manual(values = c("#c99746",
                               "#d31719",
                               "#6b9c7d",
                               "#8f9394",
                               "#aa2d20",
                               "#16101e",
                               "#fac856",
                               "#ba8e8e",
                               "#3d89a7",
                               "#6b9c7d")
                  ) +
  theme(axis.text = element_text(size = 13, color = 'black', family = letra),
        axis.title = element_text(size = 13, color = 'black', face = 'bold', family = letra),
        panel.grid.major.y  = element_line(colour = "black", size = .01,
                                           linetype = "dotted"),
        panel.grid.minor.y  = element_line(colour = "black", size = .01,
                                           linetype = "dotted"),
        panel.grid.major.x = element_line(colour = "black", size = .01,
                                          linetype = "dotted"),
        panel.grid.minor.x = element_line(colour = "black", size = .01,
                                          linetype = "dotted"),
        panel.background = element_rect(fill = '#e3e3e3'),
        legend.key = element_rect(fill = '#e3e3e3', color = '#e3e3e3'),
        legend.background = element_rect(fill = '#e3e3e3'),
        legend.title = element_text(size = 15, 
                                    color = 'black',
                                    face = 'bold',
                                    hjust = .5),
        legend.text = element_text(size = 11, 
                                   color = 'black'),
        plot.background = element_rect(fill = "#e3e3e3"),
        axis.line = element_line(colour = 'black'), 
        axis.ticks = element_line(colour = 'black'),
        legend.position = "none",
        plot.title = element_text(hjust = .5, size = 16, family = letra),
        strip.text.x = element_text(
          size = 13, color = "ghostwhite", family = letra),
        strip.background = element_rect(
          color="black", fill="#335144", size=1.5, linetype="solid"
        ),
        panel.border = element_rect(color = "black", fill = NA, size = .7),
        plot.caption = element_text(size = 10, face = "italic")
  ) +
  labs(y = "Threatened species (thousands)", x = "",
       title = "Threatened species by class and continent",
       caption = "Source: IUCN red list (https://www.iucnredlist.org/)")


g2 <- ggplot(thrspcs) +
  geom_bar(aes(Class, Total, fill = Class),
           stat = "identity", width = .5, color = "black", alpha = .7) +
  facet_grid(.~Continent, scales = "free") +
  coord_flip() +
  scale_fill_manual(values = c("#c99746",
                               "#d31719",
                               "#6b9c7d",
                               "#8f9394",
                               "#aa2d20",
                               "#16101e",
                               "#fac856",
                               "#ba8e8e",
                               "#3d89a7",
                               "#6b9c7d")
  ) +
  theme(axis.text = element_text(size = 13, color = 'black', family = letra),
        axis.title = element_text(size = 13, color = 'black', face = 'bold', family = letra),
        panel.grid.major.y  = element_line(colour = "black", size = .01,
                                           linetype = "dotted"),
        panel.grid.minor.y  = element_line(colour = "black", size = .01,
                                           linetype = "dotted"),
        panel.grid.major.x = element_line(colour = "black", size = .01,
                                          linetype = "dotted"),
        panel.grid.minor.x = element_line(colour = "black", size = .01,
                                          linetype = "dotted"),
        panel.background = element_rect(fill = '#e3e3e3'),
        legend.key = element_rect(fill = '#e3e3e3', color = '#e3e3e3'),
        legend.background = element_rect(fill = '#e3e3e3'),
        legend.title = element_text(size = 15, 
                                    color = 'black',
                                    face = 'bold',
                                    hjust = .5),
        legend.text = element_text(size = 11, 
                                   color = 'black'),
        plot.background = element_rect(fill = "#e3e3e3"),
        axis.line = element_line(colour = 'black'), 
        axis.ticks = element_line(colour = 'black'),
        legend.position = "none",
        plot.title = element_text(hjust = .5, size = 16, family = letra),
        strip.text.x = element_text(
          size = 13, color = "ghostwhite", family = letra),
        strip.background = element_rect(
          color="black", fill="#335144", size=1.5, linetype="solid"
        ),
        panel.border = element_rect(color = "black", fill = NA, size = .7),
        plot.caption = element_text(size = 10, face = "italic")
  ) +
  labs(y = "Threatened species (thousands)", x = "",
       title = "Threatened species by class and continent",
       caption = "Source: IUCN red list (https://www.iucnredlist.org/)")



ggsave("C:/Users/marco.esteban.DOMINIOIIEG/Desktop/g1.png", g1, width = 12, height = 8)
ggsave("C:/Users/marco.esteban.DOMINIOIIEG/Desktop/g2.png", g2, width = 12, height = 8)






