library(ggplot2)
library(dplyr)
library(scales)
library(extrafont)
loadfonts(device = "win")

edv <- read.csv("C:/Users/marco/OneDrive/Documentos/.R/30daysgraphs/13/edv.csv",
                stringsAsFactors = F) %>%
  tbl_df()
edv_h <- read.csv("C:/Users/marco/OneDrive/Documentos/.R/30daysgraphs/13/edv2.csv",
                  stringsAsFactors = F) %>%
  tbl_df()

names(edv_h) <- c("Año", "masculino", "inf_m", "sup_m")
names(edv) <- c("Año", "femenino", "inf_f", "sup_f")

ev <- inner_join(edv, edv_h, by = "Año")

letra <- "Tahoma"

g1 <- ggplot(ev) +
  geom_line(aes(x = Año, y = femenino),
            color = "white", alpha = .4, size = .4) +
  geom_point(aes(x = Año, y = femenino, color = "Femenino"),
             alpha = .4) +
  geom_ribbon(aes(x = Año, ymin=inf_f, ymax=sup_f),
              fill = "#4bb46d", alpha=0.08) +
  geom_line(aes(x = Año, y = masculino),
            color = "white", alpha = .4, size = .4) +
  geom_point(aes(x = Año, y = masculino, color = "Masculino"),
            alpha = .4) +
  geom_ribbon(aes(x = Año, ymin=inf_m, ymax=sup_m),
              fill = "#843dcf", alpha=0.08) +
  scale_colour_manual(values = c("#4bb46d", "#843dcf")) +
  scale_y_continuous(breaks = trans_breaks(identity, identity, n = 10)) +
  scale_x_continuous(breaks = trans_breaks(identity, identity, n = 10)) +
theme(
      panel.grid.major.y  = element_line(colour = "#66656a", size = .1,
                                         linetype = 'dotted'),
      panel.grid.minor.y  = element_blank(),
      panel.grid.major.x = element_line(colour = "#66656a", size = .05,
                                        linetype = 'dotted'),
      panel.grid.minor.x = element_line(colour = "#66656a", size = .05,
                                        linetype = 'dotted'),
      panel.background = element_rect(fill = '#29282e', colour = "#29282e"),
      plot.background = element_rect(fill = "#29282e"),
      axis.line = element_line(colour = 'white'),
      axis.ticks = element_line(colour = 'white'),
      plot.title = element_text(hjust = .5, family = letra),
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
      plot.subtitle = element_text(size = 11, colour = "#bebdc6", hjust = .5, family = letra),
      plot.caption = element_text(size = 8.5, colour = "#bebdc6")

) +
labs(x = "Año",
     title = "Esperanza de vida a edad 65 en México",
     subtitle = "1950-2015 y estimaciones 2016-2050",
     caption = "Datos de CONAPO y estimaciones propias (Lee-Carter)",
     y = "Esperanza de vida",
     color = "Sexo")

ggsave("C:/Users/marco/OneDrive/Documentos/.R/30daysgraphs/13/g1.png", g1, width = 10, height = 8)

