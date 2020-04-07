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


# 2. Relationship between Counting Resilience and general task performance
#model df 
model.df <- all.data %>%
  filter(Task == "SF" | 
           Task == "WCN" | 
           Task == "MF")%>%
  mutate(age.c  = as.vector(scale(Age, center = TRUE, scale = TRUE)), 
         Task_item = as.numeric(as.character(Task_item)), 
         count_range = ifelse(Task_item <= IHC, "Within", "Outside"), 
         Productive = factor(Productive, levels = c("Non-resilient", "Resilient")))

#main effects with contrasts
task.contrasts <- rbind(c(1, -0.5, -0.5),     # MF vs. (SF + NN) / 2
                        c(0, 1, -1))                # SF vs. NN
cMat <- MASS::ginv(task.contrasts)

prod.base.contrast <- glmer(Correct ~ Productive + Task + count_range + age.c + (1|SID), 
                              data = model.df, 
                              family = "binomial",
                              contrasts = list(Task = cMat))
car::Anova(prod.base.contrast)
summary(prod.base.contrast)

#interaction with contrasts
prod.int.contrast <- glmer(Correct ~ Productive*Task + count_range + age.c + (1|SID), 
                            data = model.df, 
                            family = "binomial",
                            contrasts = list(Task = cMat))
anova(prod.base.contrast, prod.int.contrast, test = 'lrt')
car::Anova(prod.int.contrast)
summary(prod.int.contrast)



# ...visualization ----
all.data %>%
  filter(Task == "SF" | 
           Task == "WCN" | 
           Task == "MF")%>%
  mutate(Task = factor(Task, levels = c("SF", "WCN", "MF"), 
                       labels = c("Unit Task", "Next Number", "Math Facts")))%>%
  group_by(SID, Productive, Task)%>%
  multi_boot_standard("Correct", na.rm = TRUE) %>%
  ggplot(aes(x = Task, y = mean, fill=Productive)) +
  stat_summary(fun.y = mean, position = position_dodge(width = .95),
               geom="bar", alpha = .5, colour = "black") +
  # geom_violin(alpha = .5) +
  geom_point(aes(x = Task, y = mean, colour = Productive),
             position=position_jitterdodge(jitter.width = .18, jitter.height = .035, 
                                           dodge.width = .95),
             size=1.5,
             show.legend=FALSE, 
             inherit.aes = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", geom="linerange", 
               position = position_dodge(width=0.95), width = 0.2, size = 1)+
  ylab("Mean task performance") + 
  xlab('Task') + 
  theme_bw(base_size = 11) + 
  theme(legend.position = "bottom") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        legend.position = "top", 
        legend.title = element_blank()) +
  ylim(0, 1.0) +
  scale_fill_manual(values = productivity.pal ) +
  scale_colour_manual(values = productivity.pal , guide = "none")
ggsave("Figures/productivity_3_tasks.png", width = 6, height = 5)


