######################################
### CAN ADD TO R PROFILE           ###
######################################
# 
# setHook(packageEvent("ggplot2", "onLoad"), 
#         function(...) ggplot2::theme_set(ggplot2::theme_minimal()))

library(ggplot2)
library(wesanderson)

thesis_theme <- theme_minimal() + 
  # theme(text = element_text(size = 11), + 
  theme(text = element_text(size = 10, family = 'EB Garamond'), 
        plot.title = element_text(hjust = 0.5, size = 12, face = 'bold'), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(face = "italic", hjust = 0), 
        axis.text.y = element_text(margin = margin(0, 0, 0, 8)),
        axis.text.x = element_text(margin = margin(0, 0, 8, 0)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = 'darkgrey', 
                                          linetype = 'dotted', size = 0.3),
        panel.border= element_blank(),
        axis.line = element_line(color = "black", size = 0.25)
        # legend.title = element_blank()
        )

# t_colors <- c('#F2E204', '#F2CA04', '#726316', '#D8BEBE', '#0C0C0C')
t_colors <- c('#D83A84', '#0A4272', '#78BFAB', '#F2AE30', '#F24E29') # Adobe Nunheads Festival
# t_colors <-  c('#F24B59', '#1344A5', '#F2D026', '#F26141', '#F2F2F2')