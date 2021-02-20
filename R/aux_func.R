# theme por plot
theme_1 <-
  theme(
    legend.direction = 'vertical',
    #legend.position="right",
    legend.key.width = unit(0.5, "cm"),  # symbol size
    legend.key.height = unit(0.3, "cm"), # symbol size
    legend.text = element_text(size = 7, color = "white"),
    legend.background =  element_rect(colour = "grey20", fill = "#1e1e1e"),
    legend.title = element_text(size = 7, color = "white",angle = 90),
    legend.key = element_blank(),
    strip.background =element_rect(fill="grey20"),
    strip.text = element_text(colour = 'white'),
    axis.text.y = element_text(size = 6, color = "white"),
    axis.text.x = element_text(angle = 90, vjust= 0.5,
                               size = 6, color = "white"),
    axis.line = element_line(color = "grey20", size = .01),
    axis.title = element_text(size = 6, color = "white"),
    plot.title = element_text(size = 6, color = "white"),
    panel.grid.major.x = element_line(colour = "grey20",size=0.01),
    panel.grid.minor.x = element_line(colour = "grey20",size=0.01),
    panel.grid.major.y = element_line(colour = "grey20",size=0.01),
    panel.grid.minor.y = element_line(colour = "grey20",size=0.01),
    #panel.grid = element_line(colour = "white",size=2),
    #panel.border = element_rect(fill = NA, colour = "#1e1e1e", size = 2),
    panel.background = element_rect(colour = "grey20", fill = "#1e1e1e"),
    plot.background = element_rect(colour = "grey20", fill = "#1e1e1e")
  ) 
