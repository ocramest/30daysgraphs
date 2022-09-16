#https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html

library(dplyr)
library(ggplot2)
library(tidyr)
library(foreign)
library(ggridges)
library(scales)
library(extrafont)
loadfonts(device = "win", quiet = TRUE)

letra <- "Tahoma"

c1 <- "#010101"
c2 <- "#f4f4f4"

defun <- read.dbf("C:/Users/marco/OneDrive/Documentos/.R/30daysgraphs/7/Mortalidad/DEFUN18.DBF", as.is = T) %>%
  tbl_df()

ent <- read.csv("C:/Users/marco/OneDrive/Documentos/.R/30daysgraphs/7/Mortalidad/entidades.csv",
                stringsAsFactors = F) %>% tbl_df() %>%
  mutate(ENT_OCURR = substr(CODIGO, 3, 4))

bd1 <- defun %>%
  select(ENT_OCURR, ANIO_OCUR, ANIO_NACIM, PRESUNTO, EDO_CIVIL) %>%
  filter(PRESUNTO == 3, ANIO_OCUR != 9999, ANIO_NACIM != 9999,
         EDO_CIVIL != 8, EDO_CIVIL != 9) %>%
  mutate(EDAD = ANIO_OCUR-ANIO_NACIM,
         EDO_CIVIL = case_when(EDO_CIVIL==1 ~ "Soltero",
                   EDO_CIVIL==2 ~ "Divorciado",
                   EDO_CIVIL==3 ~ "Viudo",
                   EDO_CIVIL==4 ~ "Uni?n libre",
                   EDO_CIVIL==5 ~ "Casado",
                   EDO_CIVIL==6 ~ "Separado",
                   TRUE ~ "Otro")
  )

c3 <- "#2e2c2b"

ggplot(bd1, aes(EDAD, EDO_CIVIL, fill = .5-abs(.5-stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      color = c2, size = .01) +
  scale_fill_viridis_c(name = "Valor p", direction = -1, alpha = .8) +
  scale_x_continuous(breaks = trans_breaks(identity, identity, n = 7))+
  labs(x = "Edad", y = "", 
       title = "Distribuci?n de la edad al momento del suicidio por estado civil",
       caption = "Fuente: microdatos de mortalidad, INEGI") +
  theme(axis.text = element_text(size = 10, color = c2, family = letra),
        axis.title = element_text(size = 13, color = c2, family = letra),
        panel.grid.major.y  = element_line(colour = c2, size = .01,
                                           linetype = "dotted"),
        panel.grid.minor.y  = element_blank(),
        panel.grid.major.x = element_line(colour = c2, size = .01,
                                          linetype = "dotted"),
        panel.grid.minor.x =element_line(colour = c2, size = .01,
                                         linetype = "dotted"),
        panel.background = element_rect(fill = c3),
        legend.key = element_rect(fill = c3, color = c3),
        legend.background = element_rect(fill = c3),
        legend.title = element_text(size = 15, 
                                    color = c2,
                                    hjust = .5, family = letra),
        legend.text = element_text(size = 11, 
                                   color = c2, family = letra),
        plot.background = element_rect(fill = c3),
        axis.line = element_line(colour = c2), 
        axis.ticks = element_line(colour = c2),
        plot.title = element_text(size = 15, 
                                  color = c2,
                                  hjust = .5, family = letra,
                                  margin = margin(2, 0, 10, 2)),
        plot.caption = element_text(size = 9, 
                                    color = c2,
                                    face = "italic")
  )
  
# ggsave("C:/Users/marco/OneDrive/Documentos/.R/30daysgraphs/7/g2.png",
#        height = 10, width = 10)



bd2 <- defun %>%
  inner_join(ent, by = "ENT_OCURR") %>%
  select(ENT_OCURR, ESTADO, ANIO_OCUR, ANIO_NACIM, PRESUNTO, EDO_CIVIL) %>%
  filter(PRESUNTO == 3, ANIO_OCUR != 9999, ANIO_NACIM != 9999,
         ENT_OCURR!=99) %>%
  mutate(EDAD = ANIO_OCUR-ANIO_NACIM
  )


ggplot(bd2) +
  geom_density_ridges(aes(EDAD, ESTADO),
                      fill = c1, color = c2, size = .65) +
  labs(x = "Edad", y = "", 
       title = "Distribuci?n de la edad al momento del suicidio en M?xico",
       caption = "Fuente: microdatos de mortalidad, INEGI") +
  scale_x_continuous(breaks = trans_breaks(identity, identity, n = 7))+
  theme(axis.text = element_text(size = 10, color = c2, family = letra),
        axis.title = element_text(size = 13, color = c2,, family = letra),
        panel.grid.major.y  = element_line(colour = c2, size = .65,
                                           linetype = "solid"),
        panel.grid.minor.y  = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = c1),
        legend.key = element_rect(fill = c1, color = c1),
        legend.background = element_rect(fill = c1),
        legend.title = element_text(size = 15, 
                                    color = c2,
                                    hjust = .5,
                                    family = letra),
        legend.text = element_text(size = 11, 
                                   color = c2, family = letra),
        plot.background = element_rect(fill = c1),
        axis.line = element_line(colour = c2), 
        axis.ticks = element_line(colour = c2),
        plot.title = element_text(size = 15, 
                                  color = c2,
                                  hjust = .3, family = letra,
                                  margin=margin(0,0,30,0)),
        plot.caption = element_text(size = 9, 
                                    color = c2,
                                    face = "italic")
  )

# ggsave("C:/Users/marco/OneDrive/Documentos/.R/30daysgraphs/7/g1.png",
#        height = 10, width = 8.3)
