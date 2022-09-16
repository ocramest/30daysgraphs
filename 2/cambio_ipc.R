library(ggplot2)
library(dplyr)
library(scales)
library(extrafont)
loadfonts(device = "win")

ipc <- read.csv("C:/Users/marco/OneDrive/Documentos/.R/30daysgraphs/2/^MXX.csv",
                stringsAsFactors = F) %>%
  tbl_df()

cierre <- as.numeric(ipc$Adj.Close)
cierre <- cierre[!is.na(cierre)]
cambio <- cierre[-1]/cierre[-length(cierre)]-1#diff(log(cierre))

letra <- "Tahoma"
source("C:/Users/marco/OneDrive/Documentos/.R/30daysgraphs/2/ocramest.R")
g1 <- ggplot() +
  geom_line(aes(x = seq_along(cambio), y = cambio),
            color = "white", alpha = .3, size = .2) +
  geom_hline(aes(yintercept = quantile(cambio, .99), color = "Q99 muestra"),
             linetype = "dashed", size = .5, alpha = .65) +
  geom_hline(aes(yintercept = qnorm(.99, mean(cambio), sd(cambio)), color = "Q99 normal"),
             linetype = "dashed", size = .5, alpha = .65) +
  geom_hline(aes(yintercept = quantile(cambio, .01), color = "Q01 muestra"),
             linetype = "dashed", size = .5, alpha = .65) +
  geom_hline(aes(yintercept = qnorm(.01, mean(cambio), sd(cambio)), color = "Q01 normal"),
             linetype = "dashed", size = .5, alpha = .65) +
  scale_y_continuous(breaks = trans_breaks(identity, identity, n = 11)) +
  # theme(
  #       panel.grid.major.y  = element_line(colour = "#66656a", size = .1, 
  #                                          linetype = 'dotted'),
  #       panel.grid.minor.y  = element_blank(),
  #       panel.grid.major.x = element_blank(),
  #       panel.grid.minor.x = element_blank(),
  #       panel.background = element_rect(fill = '#29282e', colour = "#29282e"),
  #       plot.background = element_rect(fill = "#29282e"),
  #       axis.line = element_line(colour = 'white'), 
  #       axis.ticks = element_line(colour = 'white'),
  #       plot.title = element_text(hjust = .5, family = letra),
  #       plot.caption = element_text(hjust =0, family = letra),
  #       axis.text = element_text(colour = "#929197", size = 10, family = letra),
  #       axis.title = element_text(colour = "#bebdc6", size = 13, family = letra),
  #       legend.background = element_rect(fill = '#29282e'),
  #       legend.box.background = element_rect(fill = '#29282e', colour = '#29282e'),
  #       legend.key = element_rect(fill = '#29282e', color = '#29282e'),
  #       legend.title = element_text(size = 15, 
  #                                   color = '#bebdc6',
  #                                   hjust = .5, 
  #                                   family = letra),
  #       legend.text = element_text(size = 11, 
  #                                  color = '#bebdc6'),
  #       title = element_text(size = 16, colour = "#bebdc6", family = letra),
  #       plot.subtitle = element_text(size = 11, colour = "#bebdc6", hjust = .5, family = letra)
  #       
  # ) +
  labs(x = "Tiempo (días)", y = "Cambio", 
       title = "Cambio porcentual diario del IPC",
       subtitle = " 08/11/1991 - 12/05/2020",
       color = "Cuantiles")

ggsave("C:/Users/marco/OneDrive/Documentos/.R/30daysgraphs/2/g1.png", g1, width = 10, height = 8)

normi <- rnorm(length(cambio), mean(cambio), sd(cambio))

qqplot(normi, cambio)
abline(0, 1)

g2 <- ggplot() +
  geom_line(aes(x = seq(-4, 4), y = seq(-4, 4)),
            color = "#bebdc6", linetype = "dashed") +
  stat_qq(aes(sample = (cambio-mean(cambio))/sd(cambio)),
          color = "#e5941e", alpha = .4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  scale_x_continuous(breaks = trans_breaks(identity, identity, n = 10)) +
  scale_y_continuous(breaks = trans_breaks(identity, identity, n = 10)) +
  theme(
    panel.grid.major.y  = element_line(colour = "#66656a", size = .1, 
                                       linetype = 'dotted'),
    panel.grid.minor.y  = element_line(colour = "#66656a", size = .1, 
                                       linetype = 'dotted'),
    panel.grid.major.x = element_line(colour = "#66656a", size = .1, 
                                      linetype = 'dotted'),
    panel.grid.minor.x = element_line(colour = "#66656a", size = .1, 
                                      linetype = 'dotted'),
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
  labs(x = "Normal estándar teórica", y = "Muestra estandarizada", 
       title = "QQ-Plot para el cambio diario del IPC")

ggsave("C:/Users/marco/OneDrive/Documentos/.R/30daysgraphs/2/g2.png", g2, width = 10, height = 8) 

  


datos <- data.frame(
  n=seq_along(cambio),
  muestra = cambio[order(cambio)],
  teorico = normi[order(normi)]
)

g3 <- ggplot(datos) +
  geom_density(aes(muestra, fill = "Muestral"), color = "#66656a", alpha = .7) +
  geom_density(aes(teorico, fill = "Teórica (normal)"), color = "#66656a", alpha = .6) +
  scale_fill_manual(values = c("#304392", "#f08f33")) +
  scale_x_continuous(breaks = trans_breaks(identity, identity, n = 10)) +
  scale_y_continuous(breaks = trans_breaks(identity, identity, n = 10)) +
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
  labs(x = "Cambio", y = "Densidad", 
       title = "Densidad kernel del cambio del IPC y distribución normal",
       fill = "Densidad")

ggsave("C:/Users/marco/OneDrive/Documentos/.R/30daysgraphs/2/g3.png", g3, width = 10, height = 8) 

