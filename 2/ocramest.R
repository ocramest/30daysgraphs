
theme_ocramest <- theme(
  panel.grid.major.y  = element_line(colour = "#66656a", size = .1, 
                                     linetype = 'dotted'),
  panel.grid.minor.y  = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.background = element_rect(fill = '#29282e', colour = "#29282e"),
  plot.background = element_rect(fill = "#29282e"),
  axis.line = element_line(colour = 'white'), 
  axis.ticks = element_line(colour = 'white'),
  plot.title = element_text(hjust = .5),
  plot.caption = element_text(hjust =0),
  axis.text = element_text(colour = "#929197", size = 10),
  axis.title = element_text(colour = "#bebdc6", size = 13),
  legend.background = element_rect(fill = '#29282e'),
  legend.box.background = element_rect(fill = '#29282e', colour = '#29282e'),
  legend.key = element_rect(fill = '#29282e', color = '#29282e'),
  legend.title = element_text(size = 15, 
                              color = '#bebdc6',
                              hjust = .5),
  legend.text = element_text(size = 11, 
                             color = '#bebdc6'),
  title = element_text(size = 16, colour = "#bebdc6"),
  plot.subtitle = element_text(size = 11, colour = "#bebdc6", hjust = .5)
  
)