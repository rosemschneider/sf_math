#SF-math data 

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

## hello world! This is a change!

# leftover data manipulations
##mean sf, mf, wcn, and indefinite
means <- all.data %>%
  filter(Task != "GiveN")%>%
  group_by(SID, Task)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))%>%
  pivot_wider(names_from = Task, 
              values_from = mean)%>%
  dplyr::rename('sf.mean' = "SF", 
                'mf.mean'= 'MF', 
                'nn.mean' = 'WCN', 
                'indef.mean' = 'Indefinite')

#add this to all.data 
all.data <- left_join(all.data, means, by = "SID")

#centering and scaling various things
all.data %<>%
  mutate(SID = factor(SID), 
         age.c = as.vector(scale(Age, center = TRUE, scale = TRUE)), 
         highest_contig.c = as.vector(scale(highest_contig, center = TRUE, scale = TRUE)), 
         ihc.c = as.vector(scale(IHC, center = TRUE, scale = TRUE)), 
         fhc.c = as.vector(scale(FHC, center = TRUE, scale = TRUE)), 
         mean.mf.c = as.vector(scale(mf.mean, center = TRUE, scale = TRUE)))

# classify MF, SF, and WCN as within or outside count range
all.data %<>%
  mutate(count_range = ifelse((Task == "SF" | Task == "WCN" | Task == "MF") & 
                                as.numeric(as.character(Task_item)) <= IHC, "Within", 
                              ifelse((Task == "SF" | Task == "WCN" | Task == "MF") &
                                       as.numeric(as.character(Task_item)) > IHC, "Outside", NA))) %>%
  mutate(count_range = factor(count_range, levels = c("Within", "Outside")))

#global theme set
theme_set(theme_bw() + theme(text = element_text(size=9), 
                             axis.title=element_text(size=8),
                             strip.text = element_text(margin=margin(2,0,2,0)), 
                             panel.grid = element_blank()))

#palette set
three.tasks.pal <- c("#173BAB", "#5CA7D8", "#DE8141")
two.tasks.pal <- c("#173BAB", "#DE8141")
productivity.pal <- c("#00b8e6", "#666666")

# Demographic info ----
all.data %>%
  distinct(SID, Age)%>%
  summarise_at('Age', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~median(., na.rm=T),
                    ~min(., na.rm=T),
                    ~max(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(n, mean, sd, median, min, max)

#sex info
all.data %>%
  distinct(SID, Sex)%>%
  group_by(Sex)%>%
  summarise(n = n())

# Highest count ----
#prompt information
## did any child use all 12 prompts? 
highest_counts %>%
  group_by(SID)%>%
  summarise(n = n())%>%
  group_by()%>%
  summarise(max = max(n), 
            min = min(n),
            median = median(n), 
            sd = sd(n)) #no, max is 10

#how many children did not count to max? 
highest_counts %>% 
  filter(IHC < 120)%>%
  distinct(SID)%>%
  summarise(n = n())

## prompt descriptives for children who did not count to 120
highest_counts %>%
  filter(IHC < 120)%>%
  group_by(SID)%>%
  summarise(n = n())%>%
  group_by()%>%
  summarise(max = max(n), 
            min = min(n),
            median = median(n), 
            sd = sd(n), 
            mean = mean(n)) 

# Descriptives of all three tasks ----
all.data %>%
  group_by(Task)%>%
  summarise(mean = mean(Correct, na.rm = TRUE), 
            sd = sd(Correct, na.rm = TRUE))

  # Models ----
model.df <- all.data %>%
  filter(Task == "SF")%>%
  mutate(starting_num.c = as.vector(scale(as.numeric(as.character(Task_item)), center = TRUE, scale = TRUE)), 
         Productive = factor(Productive, levels = c("Non-resilient", "Resilient")))

# ...Productivity model select ----
base.model <- glmer(Correct ~ count_range + age.c + (1|SID) + (1|starting_num.c), 
                    data = model.df, family = "binomial")
ihc.model <- glmer(Correct ~ ihc.c + count_range + age.c + (1|SID) + (1|starting_num.c), 
                   data = model.df, family = "binomial")
fhc.model <- glmer(Correct ~ fhc.c + count_range + age.c + (1|SID) + (1|starting_num.c), 
                   data = model.df, family = "binomial")
hcnn.model <- glmer(Correct ~ highest_contig.c + count_range + age.c + (1|SID) + (1|starting_num.c), 
                    data = model.df, family = "binomial")
resilient.model <- glmer(Correct ~ Productive + count_range + age.c + (1|SID) + (1|starting_num.c), 
                         data = model.df, family = "binomial")

## Model comparison process
### Base v. IHC
anova(base.model, ihc.model, test = 'LRT') #AIC = 2471.3
### Base v. FHC
anova(base.model, fhc.model, test = 'LRT') #AIC = 2458.7
### Base v. HCNN
anova(base.model, hcnn.model, test = 'LRT') # AIC = 2428.9
### Base v. Resilience
anova(base.model, resilient.model, test = 'LRT') # AIC = 2482.9

## All productivity predictors significantly predict Unit perf
### summaries for reporting
#base
summary(base.model)
#ihc
summary(ihc.model)
#fhc
summary(fhc.model)
#resilience
summary(resilient.model)
#hcnn
summary(hcnn.model)

## Large model building - start with HCNN, then add FHC, then IHC, then Resilience
large.model.base <- hcnn.model
large.plus.fhc <- glmer(Correct ~ fhc.c + highest_contig.c + count_range + age.c + (1|SID) + 
                          (1|starting_num.c), data = model.df, family = "binomial")
#test
anova(large.model.base, large.plus.fhc, test = 'LRT') #sig, p = .001, retain

#add IHC
large.plus.ihc <- glmer(Correct ~ ihc.c + fhc.c + highest_contig.c + count_range + age.c + 
                          (1|SID) + (1|starting_num.c), 
                          data = model.df, family = "binomial")
#test
anova(large.plus.fhc, large.plus.ihc, test = 'LRT') #ns, p = .23

#add resilience
large.plus.resilience <- glmer(Correct ~ Productive + fhc.c + highest_contig.c + count_range + age.c + 
                          (1|SID) + (1|starting_num.c), 
                        data = model.df, family = "binomial", 
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e4)))
#test
anova(large.plus.fhc, large.plus.resilience, test = 'LRT') #ns, p = .13

## FINAL PRODUCTIVITY MODEL: HCNN + FHC
final.prod.model <- large.plus.fhc
#main effects
car::Anova(final.prod.model)
tidy(final.prod.model, conf.int=T) %>% #coefficients, cis, and p values
  mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))

# Math facts model ----
mf.model <- glmer(Correct ~ mean.mf.c + count_range + age.c + (1|SID) + (1|starting_num.c), 
                  data = model.df, family = "binomial")
# compare
anova(base.model, mf.model, test = 'LRT') #significant

summary(mf.model)

# Final model building (Productivity + math facts) ----
large.plus.mf <- glmer(Correct ~ mean.mf.c + fhc.c + highest_contig.c + count_range + 
                         age.c + (1|SID) + (1|starting_num.c), 
                       data = model.df, family = "binomial")
#compare
anova(final.prod.model, large.plus.mf, test = 'lrt') #significant

## FINAL MODEL: MF + FHC + HCNN
final.model <- large.plus.mf
car::Anova(final.model)
summary(final.model)

# ...visualization of productivity parameter estimates ----
base.model.output <- tidy(base.model, conf.int=T) %>% #coefficients, cis, and p values
  mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))%>%
  mutate(Model = "Base")
ihc.model.output <- tidy(ihc.model, conf.int=T) %>% #coefficients, cis, and p values
  mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))%>%
  mutate(Model = "Initial Highest Count")
fhc.model.output <- tidy(fhc.model, conf.int=T) %>% #coefficients, cis, and p values
  mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))%>%
  mutate(Model = "Final Highest Count")
prod.model.output <- tidy(resilient.model, conf.int=T) %>% #coefficients, cis, and p values
  mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))%>%
  mutate(Model = "Counting Resilience")
hcnn.model.output <- tidy(hcnn.model, conf.int=T) %>% #coefficients, cis, and p values
  mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))%>%
  mutate(Model = "Highest Next Number")
math.model.output <- tidy(mf.model, conf.int=T) %>% #coefficients, cis, and p values
  mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))%>%
  mutate(Model = "Math Facts")

#bind together
all.model.output <- bind_rows(base.model.output,
                                   ihc.model.output, 
                                   fhc.model.output, 
                                   prod.model.output, 
                                   hcnn.model.output, 
                                   math.model.output)

all.model.output %<>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value, Model)%>%
  filter(term != "sd__(Intercept)")%>%
  mutate(term = ifelse(term == "(Intercept)", "Intercept", 
                       ifelse((term == "ihc.c" | term == "fhc.c" |
                                term == "highest_contig.c" | term == "ProductiveResilient" |
                                 term == "mean.mf.c"), "Productivity/Math Facts", 
                              ifelse(term == "count_rangeOutside", "Outside count range", "Age"))),
         term = factor(term, levels = c("Age", "Outside count range", 
                                        "Productivity/Math Facts", "Intercept")),
         Model = factor(Model, levels = c("Base",
                                          "Initial Highest Count", 
                                          "Final Highest Count", 
                                          "Counting Resilience", 
                                          "Highest Next Number", 
                                          "Math Facts"), 
                        labels = c("a) Base",
                                   "b) Initial Highest Count", 
                                   "c) Final Highest Count", 
                                   "d) Counting Resilience", 
                                   "e) Highest Next Number", 
                                   "f) Math Facts")),
         p.val.rounded = round(p.value, 3), 
         p.stars = ifelse(p.val.rounded < .001, "***", 
                          ifelse((p.val.rounded >= .001 & p.val.rounded < .01), "**", 
                                 ifelse((p.val.rounded >= .01 & p.val.rounded < .05), "*", ""))), 
         p.val.rounded = ifelse(p.val.rounded == 0, "<.001", p.val.rounded))

all.model.output %>%
  ggplot(aes(x = estimate, y = term, color = "#062a9e")) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", size = .25) +
  geom_point(size = 1) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height =0, 
                 size = .5) +
  geom_text(label = all.model.output$p.stars, 
            nudge_y = .15, 
            size = 3) +
  theme_bw(base_size = 8) +
  theme(legend.position = "none", 
        panel.grid.minor.x = element_blank()) +
  facet_wrap(~Model, ncol = 2) + 
  labs(x = "Parameter estimate", 
       y = "") + 
  scale_color_manual(values = "#062a9e")
ggsave('Figures/individ_parameter_estimates.png', units = "in", width = 3.5, height = 3.5)

# ...visualizing final large model ----
large.model.output <- tidy(large.plus.mf, conf.int=T) %>% #coefficients, cis, and p values
  mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))

large.model.output %<>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value)%>%
  filter(term != "sd__(Intercept)")%>%
  mutate(term = ifelse(term == "(Intercept)", "Intercept", 
                       ifelse(term == "fhc.c", "Final Highest Count", 
                              ifelse(term == "highest_contig.c", "Highest Next Number",
                                     ifelse(term == "mean.mf.c", "Mean Math Facts",
                              ifelse(term == "count_rangeOutside", "Outside count range", "Age"))))),
         term = factor(term, levels = c("Age", "Outside count range", 
                                        "Final Highest Count", "Highest Next Number", 
                                        "Mean Math Facts", "Intercept")),
         p.val.rounded = round(p.value, 3), 
         p.stars = ifelse(p.val.rounded < .001, "***", 
                          ifelse((p.val.rounded >= .001 & p.val.rounded < .01), "**", 
                                 ifelse((p.val.rounded >= .01 & p.val.rounded < .05), "*", ""))), 
         p.val.rounded = ifelse(p.val.rounded == 0, "<.001", p.val.rounded))

large.model.output %>%
  ggplot(aes(x = estimate, y = term, color = "#062a9e")) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 2) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height =0, 
                 size = .5) +
  geom_text(label = large.model.output$p.stars, 
            nudge_y = .2, 
            size = 4.5) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none", 
        panel.grid.minor.x = element_blank()) +
  labs(x = "Parameter estimate", 
       y = "") + 
  scale_color_manual(values = "#062a9e")
ggsave('Figures/large_parameter_estimates.png', width = 4.5, height = 2.5)


# ...visualization of NN/MF and Unit performance
hcnn.unit <- all.data %>%
  mutate(highest_contig = factor(highest_contig, levels = c("0", "1", 
                                                            "7", "24", "26", "30", "62", 
                                                            "71", "83", "95")))%>%
  mutate(highest_contig.num = as.numeric(highest_contig))%>%
  distinct(SID, sf.mean, highest_contig, highest_contig.num)%>%
  ggplot(aes(x = highest_contig.num, y = sf.mean, color = "#3AAADD")) + 
  geom_count(stat = "sum", 
             show.legend = FALSE) + 
  geom_smooth(aes(fill = "#3AAADD"), method = 'lm', alpha = .3, 
              show.legend = FALSE) + 
  scale_color_manual(values = "#3AAADD") + 
  scale_fill_manual(values = "#3AAADD") + 
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  labs(x = 'Highest Contiguous Next Number', 
       y = "Mean Unit Task performance") + 
  scale_x_continuous(breaks = (1:length(as.numeric(levels(factor(all.data$highest_contig, levels = c("0", "1", 
                                                                                                     "7", "24", "26", "30", "62", 
                                                                                                     "71", "83", "95")))))), 
                     labels = as.character(levels(factor(all.data$highest_contig, levels = c("0", "1", 
                                                                                             "7", "24", "26", "30", "62", "71", "83", "95")))))
# ggsave("Figures/unit_by_hcnn.png", width = 4, height = 3.25)
                      
## math facts
mf.unit <- all.data %>%
  distinct(SID, sf.mean, mf.mean)%>%
  ggplot(aes(x = mf.mean, y = sf.mean, colour = "#DE8141")) + 
  geom_count(stat = "sum", show.legend = FALSE) + 
  geom_smooth(aes(fill = "#DE8141"), 
              method = 'lm', alpha = .3,  
              show.legend = FALSE) + 
  scale_color_manual(values = "#DE8141") + 
  scale_fill_manual(values = "#DE8141") +
  labs(x = "Mean Math Facts performance", 
       y = "") + 
  theme_bw(base_size = 14) + 
  theme(panel.grid = element_blank())
# ggsave("Figures/unit_by_mf.png", width = 4, height = 3.25)

hcnn.unit +mf.unit
ggsave("Figures/hcnn_mf_unit.png", units = "in", width = 8, height = 3.25)

                                                                                                                                                                 

# Is SF generalized from Math Facts? ----
##Testing whether SF is generalized from Math Facts
#If children are acquiring SF from MF, we should expect that these children 
#should also be at ceiling in Math Facts. Here, I am testing whether there is a difference in 
#accuracy across the Unit, NN, and Math Facts tasks

#add quartiles for unit task
all.data %<>%
  mutate(sf.quartile = cut(sf.mean, 
                           breaks=quantile(sf.mean, na.rm=TRUE), 
                           include.lowest=TRUE))

all.tasks.model <- all.data %>%
  filter(Task == "SF" | 
           Task == "WCN" |
           Task == "MF")%>%
  mutate(Task = factor(Task, levels = c("MF", "SF", "WCN")))

# ... Test whether there are significant differences in accuracy by task  for top quartile 
##make appropriate contrasts
task.contrasts <- rbind(c(1, -0.5, -0.5),     # MF vs. (SF + NN) / 2
                        c(0, 1, -1))                # SF vs. NN
cMat <- MASS::ginv(task.contrasts)

no.planned.contrasts <- glmer(Correct ~ Task + count_range + age.c + (1|SID), 
                           data = subset(all.tasks.model, sf.quartile == "(0.875,1]"), 
                           family = "binomial",
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=2e4)))
summary(no.planned.contrasts)

#starting number is not going to be informative here, I think because everyone is at ceiling in the Unit Task and WCN
planned.contrasts <- glmer(Correct ~ Task + count_range + age.c + (1|SID), 
                           data = subset(all.tasks.model, sf.quartile == "(0.875,1]"), 
                           family = "binomial",
                           contrasts = list(Task = cMat), 
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=2e4)))
car::Anova(planned.contrasts)
tidy(planned.contrasts, conf.int=T) %>% #coefficients, cis, and p values
  mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))

# ...repeat this analysis for all children
#starting number is not going to be informative here, I think because everyone is at ceiling in the Unit Task and WCN
planned.contrasts.all <- glmer(Correct ~ Task + count_range + age.c + (1|SID), 
                           data = all.tasks.model, 
                           family = "binomial",
                           contrasts = list(Task = cMat), 
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=2e4)))
car::Anova(planned.contrasts.all)
tidy(planned.contrasts.all, conf.int=T) %>% #coefficients, cis, and p values
  mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))


# ...visualize - all three tasks by SF quartile ----
all.data %>%
  filter(Task == "SF" |
           Task == "WCN" |
           Task == "MF")%>%
  mutate(Task = factor(Task, levels = c("SF", "WCN", "MF"), 
                       labels = c("Unit Task", "Next Number", "Math Facts")), 
         sf.quartile = factor(sf.quartile, 
                              labels = c("25% — 44%", "44% — 63%", "63% — 88%", "88% — 100%")))%>%
  group_by(SID, Task, sf.quartile)%>%
  multi_boot_standard("Correct", na.rm = TRUE) %>%
  ggplot(aes(x = Task, y = mean, fill=Task)) +
  stat_summary(fun.y = mean, position = position_dodge(width = .95),
               geom="bar", alpha = .5, colour = "black") +
  # geom_violin(alpha = .5) +
  geom_point(aes(x = Task, y = mean, colour = Task),
             position=position_jitter(width = .18, height = .035),
             size=1.5,
             show.legend=FALSE, 
             inherit.aes = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", geom="linerange", 
               position = position_dodge(width=0.95), width = 0.2, size = 1)+
  ylab("Mean task performance") + 
  xlab('Unit Task performance quartiles') + 
  theme_bw(base_size = 14) + 
  theme(legend.position = "bottom") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        legend.position = "top", 
        legend.title = element_blank()) +
  ylim(0, 1.0) +
  scale_fill_manual(values = three.tasks.pal ) +
  scale_colour_manual(values = three.tasks.pal , guide = "none") + 
  facet_wrap(~sf.quartile, strip.position = "bottom", ncol = 4)
ggsave("Figures/all_tasks_quartile.png", units = "in", width = 8, height = 3.5)


# Math facts and Unit Task comparison ----
#Is there a difference between by-item performance on math facts and Unit? 
##This analysis is testing for an interaction between item magnitude and task
#Need to create "magnitude match" because we didn't always test the same items

#Items that overlap: 20, 32, 57, 93
#Magnitude match pairs (Unit in parens): 5 (6), 64 (62), 86 (84)

mf.unit.comparison <- all.data %>%
  filter(Task == "MF" | Task == "SF")%>%
  mutate(Task_item = as.numeric(as.character(Task_item)))%>%
  filter(Task_item == 5 |Task_item == 6 |Task_item == 20 |Task_item == 32 | Task_item == 57 | Task_item == 64 
         |Task_item == 62 | Task_item == 86 | Task_item == 84 |Task_item == 93)%>%
  mutate(Magnitude_match = ifelse((Task_item == 5 | Task_item == 6), 5.5, 
                                  ifelse(Task_item == 20, 20, 
                                         ifelse(Task_item == 32, 32, 
                                                ifelse(Task_item == 57, 57, 
                                                       ifelse((Task_item == 64 | Task_item == 62), 63, 
                                                              ifelse((Task_item == 86 | Task_item == 84), 85, 93)))))))%>%
  mutate(magnitude.match.c = as.vector(scale(Magnitude_match, center = TRUE, scale = TRUE)))

# ...is there an interaction between item magnitude and task for matched items? ----
mf.unit.comparison %<>%
  mutate(Task = factor(Task, levels = c("SF", "MF")))

#save this for future graphing
save(mf.unit.comparison, file = "mf_unit_comparison.RData")

#magnitude match
comparison.sf.mf.mag <- glmer(Correct ~ magnitude.match.c+Task + count_range + age.c + (1|SID) , 
                        data =  mf.unit.comparison, 
                        family = "binomial", 
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e4)))

#add interaction with magnitude match
comparison.sf.mf.int <- glmer(Correct ~ magnitude.match.c*Task + count_range + age.c + (1|SID), 
                        data =  mf.unit.comparison, 
                        family = "binomial", 
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e4)))

#compare
car::Anova(comparison.sf.mf.int)
# anova(comparison.sf.mf.mag,comparison.sf.mf.int, test = 'lrt')
summary(comparison.sf.mf.int)

## ...visualize - magnitude comparison between MF and Unit
mf.unit.comparison %>%
  dplyr::mutate(Task = factor(Task, levels = c("SF", "MF"), labels = c("Unit Task", "Math Facts")), 
         Magnitude_match = factor(Magnitude_match, levels = c("5.5", "20", 
                                                              "32", "57", "63", "85", "93"), 
                                  labels = c("5", "20", 
                                                      "32", "57", "63", "85", "93")))%>%
  group_by(Task, Magnitude_match)%>%
  multi_boot_standard("Correct", na.rm = TRUE) %>%
  ggplot(aes(x = Magnitude_match, y = mean, colour = Task, group = Task, shape = Task)) +
  geom_point(size = .8) + 
  geom_line(size = .25) +
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper), 
                 size = .25) +
  theme_bw(base_size = 4) +
  # facet_grid(~trial.type, scale = "free_x") +
  scale_colour_manual(values = two.tasks.pal) +
  theme(legend.key.size = unit(.35, "line"),
        legend.position = c(.8, .88),
        legend.title = element_blank(),
        legend.margin = unit(-0.6,"cm"),
        panel.grid = element_blank()) +
  labs(x = "Magnitude of number queried", y = "Mean performance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(limits = c(0,1))
ggsave("Figures/magnitude_overall_comp.png", units = "in", width = 1.2, height = 1)

# ...does success on Unit translate into success on Math Facts?
##This analysis is restricted to kids who have at least minimal familiarity with the "MF" template - kids who succeed on 5+1
##Testing whether, if they get an item correct on Unit, they also get it correct on MF
##This is analysis is restricted to items that appear on both tasks: 20, 32, 57, 93
  
#first, filter down to only kids who get 5+1 correct
addition.understanders <- mf.unit.comparison %>%
  filter(Task_item == 5)%>%
  mutate(addition.understander = ifelse((Task_item == 5 & Correct == 1), 1, 0))%>%
  dplyr::select(SID, addition.understander)

#add to larger df
mf.unit.correct.comparison <- left_join(mf.unit.comparison, addition.understanders, by = "SID")

mf.unit.correct.comparison %>%
  filter(Task == "MF", 
         Task_item == 5)%>%
  distinct(SID, addition.understander)%>%
  group_by(addition.understander)%>%
  summarise(n = n())

#Now we need to indicate if they got an item correct or incorrect on Unit
mf.unit.correct.comparison %<>%
  dplyr::select(SID, Task, Task_item, addition.understander, Age, age.c, count_range, Correct)%>%
  filter(Task_item == 20 | Task_item == 32 | Task_item == 57 | Task_item == 93)%>%
  group_by(SID, Task_item, addition.understander, age.c, count_range)%>%
  pivot_wider(names_from = Task, 
              values_from = Correct)%>%
  ungroup()

#descriptives; does performance on math facts improve if corresponding item is correct on Unit?
mf.unit.correct.comparison %>%
  filter(addition.understander == 1)%>%
  group_by(SF)%>%
  summarise(mean = mean(MF, na.rm = TRUE), 
            sd = sd(MF, na.rm = TRUE))

#t-test for accuracy
mf.unit.comparison.ms <- mf.unit.correct.comparison %>%
  filter(addition.understander == 1)%>%
  group_by(SID, SF)%>%
  summarise(mean = mean(MF, na.rm = TRUE))

t.test(subset(mf.unit.comparison.ms, SF == 0)$mean, 
       subset(mf.unit.comparison.ms, SF == 1)$mean, var.equal = TRUE)

#save this data for graphing
save(mf.unit.correct.comparison, file = "mf_unit_correct_comparison.RData")

##this will go in graphs, but we'll have it here for now
mf.unit.correct.comparison %>%
  filter(addition.understander == 1)%>%
  mutate(Task_item = factor(Task_item)) %>%
  mutate(MF = factor(MF, levels = c(0,1), 
                             labels = c("Math Facts Incorrect", "Math Facts Correct")), 
         SF = factor(SF, levels = c(0, 1),
                     labels = c("Unit Task Incorrect", "Unit Task Correct")))%>%
  group_by(Task_item, SF, MF)%>%
  summarise(n = n()) %>%
  group_by(Task_item)%>%
  mutate(total.n = sum(n), 
         prop = n/total.n)%>%
  ggplot(aes(x = Task_item, y = prop, fill = SF, group = SF)) +
  geom_bar(stat = "identity", color = "black", size = .25) +
  facet_grid(~MF) + 
  labs(y = 'Prop. Math Facts correct/incorrect', x = 'Number queried', 
       fill = "Unit Task correct/incorrect") + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw(base_size = 6) + 
  theme(panel.grid = element_blank(), 
        legend.position = "top", 
        legend.title = element_blank(), 
        legend.key.size = unit(.35, "line"))
ggsave("Figures/mf_unit_comparison.png", units = "in", width = 2.75, height = 1.9)

## ... analysis: testing whether, for addition knowers, they are more accurate on math facts for items that they succeeded on in Unit Task ---- 

#now make a model predicting a correct response on MF from Unit task
addition.understander.comparison <- glmer(MF ~ SF + count_range + age.c + (1|SID), 
                                          data = mf.unit.correct.comparison, family = "binomial")
car::Anova(addition.understander.comparison)
summary(addition.understander.comparison)

# Indefinite next number task ----
## make an indefinite next number dataset

indefinite.df <- all.data %>%
  filter(Task == "Indefinite")%>%
  droplevels()

#descriptives
indefinite.df %>%
  distinct(SID, Age)%>%
  summarise(n = n(), 
            mean_age = mean(Age), 
            sd_age = sd(Age))

# ...overal performance ----
indefinite.df %>%
  summarise(mean = mean(Correct, na.rm = TRUE), 
            sd = sd(Correct, na.rm = TRUE))

# ...is performance predicted by mean Unit Task performance for the largest items (81, 84, 93, 95)
restricted_unit <- all.data %>%
  filter(Task_item == 81 | 
           Task_item == 84 | 
           Task_item == 93 | 
           Task_item == 95, 
         Task == "SF") %>%
  group_by(SID)%>%
  summarise(mean.unit.restricted = mean(Correct, na.rm = TRUE))

#left join to indefinite
indefinite.df <- left_join(indefinite.df, restricted_unit, by = "SID")

indefinite.df %<>%
  mutate(restricted.unit.c = as.vector(scale(mean.unit.restricted, center = TRUE, scale = TRUE)))

# now make a model predicting Indefinite NN performance from mean Unit performance for these items 
indef.from.unit <- glmer(Correct ~ restricted.unit.c + age.c + (1|SID) + (1|Task_item), 
                         data = indefinite.df, family = "binomial")
car::Anova(indef.from.unit)
summary(indef.from.unit)

#group to see how accuracy differs by restricted Unit Task accuracy 
indefinite.df %>%
  group_by(mean.unit.restricted)%>%
  summarise(mean = mean(Correct, na.rm =TRUE), 
            n = n())%>%
  mutate(n.children = n/4)

# ... does mean indefinite performance predict unit task performance? ----
##go back to model.df data frame
indef.model.df <- model.df %>%
  filter(SID %in% indefinite.df$SID)%>%
  mutate(indef.mean.c = as.vector(scale(indef.mean, center = TRUE, scale = TRUE)))

unit.from.indef.base <- glmer(Correct ~ count_range + age.c + (1|SID) + 
                                (1|starting_num.c), data = indef.model.df, family = 'binomial')
unit.from.indef <- glmer(Correct ~ indef.mean.c + count_range + age.c + (1|SID) + 
                           (1|starting_num.c), data = indef.model.df, family = 'binomial')
anova(unit.from.indef.base, unit.from.indef, test = 'lrt')
summary(unit.from.indef)

# ...finally, test whether indef explains anything above and beyond large model 
final.large <- glmer(Correct ~ mean.mf.c + fhc.c + highest_contig.c + count_range + age.c + (1|SID) + 
                       (1|starting_num.c), data = indef.model.df, family = 'binomial')
large.plus.indef <- glmer(Correct ~ indef.mean.c + mean.mf.c + fhc.c + highest_contig.c + count_range + age.c + (1|SID) + 
                            (1|starting_num.c), data = indef.model.df, family = 'binomial')
anova(final.large, large.plus.indef, test = 'lrt')
summary(large.plus.indef)

# ... visualization of large indef model output ----
large.indef.model.output <- tidy(large.plus.indef, conf.int=T) %>% #coefficients, cis, and p values
  mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))

large.indef.model.output %<>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value)%>%
  filter(term != "sd__(Intercept)")%>%
  mutate(term = ifelse(term == "(Intercept)", "Intercept", 
                       ifelse(term == "fhc.c", "Final Highest Count", 
                              ifelse(term == "highest_contig.c", "Highest Next Number",
                                     ifelse(term == "mean.mf.c", "Mean Math Facts",
                                            ifelse(term == "indef.mean.c", "Mean Indefinite Number",
                                              ifelse(term == "count_rangeOutside", "Outside count range", "Age")))))),
         term = factor(term, levels = c("Age", "Outside count range", 
                                        "Final Highest Count", "Highest Next Number", 
                                        "Mean Math Facts", "Mean Indefinite Number", "Intercept")),
         p.val.rounded = round(p.value, 3), 
         p.stars = ifelse(p.val.rounded < .001, "***", 
                          ifelse((p.val.rounded >= .001 & p.val.rounded < .01), "**", 
                                 ifelse((p.val.rounded >= .01 & p.val.rounded < .05), "*", ""))), 
         p.val.rounded = ifelse(p.val.rounded == 0, "<.001", p.val.rounded))

large.indef.model.output %>%
  ggplot(aes(x = estimate, y = term, color = "#062a9e")) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 1) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height =0, 
                 size = .5) +
  geom_text(label = large.indef.model.output$p.stars, 
            nudge_y = .15, 
            size = 3) +
  theme_bw(base_size = 6) +
  theme(legend.position = "none", 
        panel.grid.minor.x = element_blank()) +
  labs(x = "Parameter estimate", 
       y = "") + 
  scale_color_manual(values = "#062a9e")
ggsave('Figures/large_indef_parameter_estimates.png', units = "in", width = 2.5, height = 1.35)


# ... visualize - relationship between Unit and Indefinite ----

all.data %>%
  filter(!is.na(indef.mean), 
         Task == "SF")%>%
  mutate(indef.group = ifelse(indef.mean <= .50, "0%-50%", "50%-100%"))%>%
  # mutate(indef.mean = factor(indef.mean, labels = c("0%", "25%", "50%", "75%", "100%")))%>%
  group_by(SID, indef.group)%>%
  summarise(mean = mean(Correct, na.rm = TRUE)) %>%
  ggplot(aes(x = indef.group, y = mean, color = '#2a8db8')) + 
  geom_violin(aes(fill = '#2a8db8'), alpha = .2, show.legend = FALSE) +
  geom_point(show.legend = FALSE, position = position_jitter(width = .05), 
             alpha = .6) + 
  stat_summary(fun.data = "mean_cl_boot", geom="linerange", 
               position = position_dodge(width=0.9), width = 0.2, 
               size = 1, color = "black") +
  scale_color_manual(values = "#2a8db8") + 
  scale_fill_manual(values = '#2a8db8') + 
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = 'Mean Indefinite Next Number Performance', 
       y = "Mean Unit Task performance") 
ggsave("Figures/indef_unit.png", width = 5, height = 3.5)

# Follow-up: What is the average distance between IHC and HCNN? 
all.data %>% 
  filter(IHC <= 95)%>%
  distinct(SID, IHC, highest_contig)%>%
  mutate(delta.hc = IHC - highest_contig)%>%
  summarise(mean = mean(delta.hc), 
            sd = sd(delta.hc), 
            min = min(delta.hc), 
            max = max(delta.hc), 
            median = median(delta.hc))

##Follow up: Is there an effect of alternative order in Unit task?
greater.first.trials <- as.vector(c("2", "4", "6", "8", 
                                    "10", "12", "14", "16", "18"))

unit.data <- all.data %>%
  filter(Task == "SF", 
         Trial_number != "Training")%>%
  mutate(alt.order = ifelse(Trial_number %in% greater.first.trials, "greater_first", 
                            "lesser_first"), 
         Trial_number = as.numeric(as.character(Trial_number)))

test <- unit.data %>%
  group_by(SID, alt.order)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))

t.test(subset(test, alt.order == "greater_first")$mean, 
       subset(test, alt.order == "lesser_first")$mean, var.equal = TRUE)

summary(glmer(Correct ~ alt.order + as.numeric(as.character(Task_item)) + (1|SID), 
              family= "binomial", 
              data = unit.data))

