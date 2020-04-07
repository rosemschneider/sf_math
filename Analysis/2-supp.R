#supplementary material for sf-math paper

# SETUP ----
source("0-clean.R") # data cleaning script, produces cleaned data for study 1 and 2
# Load cleaned data - 2 dfs
rm(list = ls())
load("../Data/sf_math_data_cleaned.RData")
load("../Data/highest_count_full.RData")

## load libraries
library(tidyverse)
library(magrittr)
library(langcog)
library(lme4)
library(stringr)    
library(RColorBrewer)
library(ggthemes)
library(broom.mixed)
library(ggpubr)
library(patchwork)

'%!in%' <- function(x,y)!('%in%'(x,y))

#custom palette
productivity.pal <- c("#00b8e6", "#666666")

# 1. Highest Count ----

# ... descriptives ----
initial_final <- all.data %>%
  filter(!is.na(Productive))%>%
  distinct(SID, IHC, FHC, Productive)%>%
  mutate(IHC = as.numeric(IHC), 
         FHC = as.numeric(FHC))

#overall
initial_final %>%
  summarise(n = n(),
            mean_IHC = mean(IHC), 
            sd_IHC = sd(IHC), 
            median_IHC = median(IHC), 
            mean_FHC = mean(FHC), 
            sd_FHC = sd(FHC), 
            median_FHC = median(FHC))

#by counting resilience
initial_final %>%
  group_by(Productive)%>%
  summarise(n = n(),
            mean_IHC = mean(IHC), 
            sd_IHC = sd(IHC), 
            median_IHC = median(IHC), 
            mean_FHC = mean(FHC), 
            sd_FHC = sd(FHC), 
            median_FHC = median(FHC))

# How many children could count past their IHC? 
initial_final %>% 
  filter(IHC <120)%>%
  mutate(delta = FHC-IHC, 
         delta.group = ifelse(delta == 0, "no progress", "progress"))%>%
  group_by(delta.group)%>%
  summarise(n = n(), 
            mean = mean(delta), 
            sd = sd(delta), 
            median = median(delta))

# ...visualization - density/scatterplot ----

library(ggstance)
library(ggjoy)
library(cowplot)
#main plot
pmain <- ggplot(initial_final, aes(x = IHC, y = FHC, color = Productive, shape = Productive)) + 
  geom_point(size = 3, alpha = .8, position = position_jitter()) + 
  # geom_jitter() + 
  scale_color_manual(values = productivity.pal) + 
  coord_fixed() + 
  theme_bw(base_size = 15) +
  scale_x_continuous(breaks = seq(0, 120, 10)) + 
  scale_y_continuous(breaks = seq(0, 120, 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12),
        legend.position = c(.75, 0.2), 
        legend.text = element_text(size = 10), 
        legend.title = element_blank(), 
        axis.text = element_text(size = 11)) + 
  labs(x = "Initial Highest Count", y = "Final Highest Count") 

#density
xdens <- axis_canvas(pmain, axis = "x") + 
  geom_density(data = initial_final, aes(x = IHC, y = ..density.., fill = Productive), 
               alpha=.4, adjust = .5) + 
  scale_fill_manual(values = productivity.pal) 

ydens <- axis_canvas(pmain, axis = "y", coord_flip = TRUE) + 
  geom_density(data = initial_final, aes(x = FHC, y = ..density.., fill = Productive), 
               alpha=.4, adjust = .5) +
  coord_flip() +
  scale_fill_manual(values = productivity.pal) 

#put it all together
p5 <- insert_xaxis_grob(pmain, xdens, grid::unit(.2, "null"), position = "top")
count_dens <- insert_yaxis_grob(p5, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(count_dens)
ggsave("Figures/hc_density_scatter.png", width = 5, height =5)
# ggdraw(p6)
# png(filename = "hk_density.png")
# ggdraw(p6)
# dev.off()