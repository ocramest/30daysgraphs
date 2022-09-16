library(dplyr)
library(ggplot2)
library(scales)

base <- read.csv("C:/Users/marco/Downloads/SSNMX_catalogo_19000101_20200512.csv", stringsAsFactors = F) %>%
  tbl_df()

sismos <- as.numeric(base$Magnitud)
sismos <- sismos[!is.na(sismos)]
g1 <- ggplot() +
  geom_histogram(aes(sismos),
                          bins = 50,
                          color = "black",
                          fill = "#292c83",
                          alpha = .6) +
  scale_y_continuous(breaks = trans_breaks(identity, identity, n = 10)) +
  scale_x_continuous(breaks = trans_breaks(identity, identity, n = 10)) +
  ggtitle("Número de terremotos registrados en México desde 1900 por magnitud") +
  theme(panel.grid.major.y  = element_line(colour = "grey", size = .3, 
                                           linetype = 'dashed'),
        panel.grid.minor.y  = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = "white"),
        axis.line = element_line(colour = 'black'), 
        axis.ticks = element_line(colour = 'black'),
        plot.title = element_text(hjust = .5),
        plot.caption = element_text(hjust =0),
        axis.text.x = element_text(angle = 0)
        ) +
  labs(x = "Magnitud del sismo", y = "Frecuencia")
 
quantile(sismos, .95)
quantile(sismos, .999)

cabrones <- sismos[sismos>quantile(sismos,.95)]

dpareto <- function(x, alpha, theta){
  return(
    alpha*theta^alpha/(x^(alpha+1))
  )
}

rpareto <- function(u, alpha, theta){
  
  return(
    theta*(1-u)^(-1/alpha)
  )
  
}


mle <- optim(par = 2, datos = cabrones, method = "Brent", lower = 0,upper = 100,
             fn = function(par, datos){
               -sum(log(dpareto(x = datos, alpha = par, theta = min(cabrones))))}
)
mle$par  

sim <- rpareto(runif(length(cabrones)),
               alpha = mle$par,
               theta = min(cabrones)) 


g2 <- ggplot() +
  geom_histogram(aes(cabrones),
                 bins = 30,
                 color = "black",
                 fill = "#3a805a",
                 alpha = .9) +
  scale_y_continuous(breaks = trans_breaks(identity, identity, n = 10)) +
  ggtitle("Frecuencia de terremotos registrados en México con magnitud mayor a 4.3")+
 theme(panel.grid.major.y  = element_line(colour = "grey", size = .3, 
                                           linetype = 'dashed'),
        panel.grid.minor.y  = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = "white"),
        axis.line = element_line(colour = 'black'), 
        axis.ticks = element_line(colour = 'black'),
        plot.title = element_text(hjust = .5),
        plot.caption = element_text(hjust =0),
        axis.text.x = element_text(angle = 0)
  ) +
  labs(x = "Magnitud del sismo", y = "Frecuencia") +
  geom_histogram(aes(sim),
                 bins = 30,
                 color = "black",
                 fill = "#ffe869",
                 alpha = .4) +
  xlim(min(cabrones)*.9, max(sim))+
  scale_x_continuous(breaks = trans_breaks(identity, identity, n = 10))

max(sim)

ggsave("C:/Users/marco/OneDrive/Documentos/.R/30daysgraphs/g1.png", g1)


