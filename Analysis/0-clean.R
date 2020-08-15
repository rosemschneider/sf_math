##SF-math data cleaning and processing
##Rose M. Schneider

##Takes as input raw trial and counting data for SF-Math project, cleans, and returns processed data with productive/nonproductive classification
##performs data validation

#### Load libraries ####
rm(list = ls())
library(tidyverse)
library(magrittr)
library(tidylog)
library(stringr)

#filtering function
'%!in%' <- function(x,y)!('%in%'(x,y))

# Load trial data ----
data.raw <- read.csv('../Data/sf_math_data.csv') %>%
  filter(SID != "?", 
         Exclude_analysis_reason != "ALREADY PARTICIPATED ON 4/20/18")%>% #hard-code, one participant with unrecorded SID
  mutate(Age = round(as.numeric(as.character(Age)), 2))%>%
  dplyr::select(-Response_single, -Response_double)%>% #remove double coding
  dplyr::rename(Response = Response_final)%>%#rename for code
  mutate(Exclude_trial = ifelse(Exclude_trial == "#NAME?", 0, as.numeric(as.character(Exclude_trial))))#anything that is not 1 is not excluded, and should be 0

## ...exclusions ----
## how many kids pre-exclusion?
total.pre.excl <- data.raw %>%
  distinct(SID, Age) ##n =230

## why are kids excluded? 
## Note: There is one kid who has more than 20% of data missing overall (checked below in code, so this is actually 38)
## Note: 5 kids excluded manually for less than 50% data 
data.raw %>%
  filter(Exclude_analysis == 1)%>%
  distinct(SID, Exclude_analysis_reason)%>%
  group_by(Exclude_analysis_reason)%>%
  summarise(n = n())%>%
  mutate(excl.type = ifelse(Exclude_analysis_reason == "NOT CP KNOWER", "NOT CP", "OTHER"))%>%
  group_by(excl.type)%>%
  mutate(total.n = sum(n))# n = 48 non-cp, n = 37 other exclusions

## Exclude these kids
data.raw %<>%
  filter(Exclude_analysis != 1)

##We are excluding kids because they don't have enough data below
## Task exclusions 
## check when kids are missing more than 20% of data within a task
task.check <- data.raw %>%
  filter(Task != "GiveN", 
         Trial_number != "Training") %>%
  mutate(data.missing = ifelse(is.na(Response), "MISSING", "PRESENT"))%>%
  group_by(SID, Task, data.missing)%>%
  summarise(n = n())%>%
  pivot_wider(names_from = data.missing, 
              values_from = n)%>%
  mutate(MISSING = ifelse(is.na(MISSING), 0, MISSING), 
         PRESENT = ifelse(is.na(PRESENT), 0, PRESENT),
         completed = PRESENT - MISSING,
         total.possible.n = ifelse(Task == "SF", 16, 
                                   ifelse((Task == "WCN" | Task == "MF"), 8, 4)), 
         prop = completed/total.possible.n, 
         exclude_task_check = ifelse(prop < .8, "EXCLUDE", "INCLUDE"))%>%
  dplyr::select(SID, Task, exclude_task_check)

#add this to data
data.raw <- left_join(data.raw, task.check, by = c("SID", "Task"))

## Now check to see whether kids are missing more than 20% of data overall
overall.check <- data.raw %>%
  filter(Task != "GiveN", 
         Task != "Indefinite",
         Trial_number != "Training") %>%
  mutate(data.missing = ifelse(is.na(Response), "MISSING", "PRESENT"))%>%
  group_by(SID, data.missing)%>%
  summarise(n = n())%>%
  pivot_wider(names_from = data.missing, 
              values_from = n)%>%
  mutate(MISSING = ifelse(is.na(MISSING), 0, MISSING), 
         PRESENT = ifelse(is.na(PRESENT), 0, PRESENT), 
         completed = PRESENT - MISSING,
         total.possible.n = 32, 
         prop = completed/total.possible.n, 
         exclude_overall_check = ifelse(prop < .8, "EXCLUDE", "INCLUDE"))%>%
  dplyr::select(SID, exclude_overall_check) 

overall.check %>%
  filter(exclude_overall_check == "EXCLUDE")%>%
  distinct(SID) #one kid who needs to be excluded for less than 20% of data

#add this to data
data.raw <- left_join(data.raw, overall.check, by = c("SID")) %>%
            mutate(Exclude_analysis_reason = ifelse(exclude_overall_check == "EXCLUDE", 
                                 'MORE THAN 20% OF DATA MISSING', 
                                 as.character(Exclude_analysis_reason)))

# how many tasks excluded? 
data.raw %>%
  filter(exclude_task_check == "EXCLUDE")%>%
  distinct(SID, Task)%>%
  group_by(Task)%>%
  summarise(n = n())

# now exclude
data.raw %<>%
  filter(exclude_task_check != "EXCLUDE", 
         exclude_overall_check != "EXCLUDE")

## Response exclusions (NAs)
data.raw %<>%
  filter(!is.na(Correct))

# Now global check - does anyone have less than 20% of data overall? 
global.check <- data.raw %>%
  filter(Task != "GiveN", 
         Task != "Indefinite", 
         Trial_number!= "Training")%>%
  group_by(SID, Task)%>%
  summarise(n = n())%>%
  group_by(SID)%>%
  mutate(total.n = sum(n), 
         total.possible.n = 16+8+8, 
         prop = total.n/total.possible.n, 
         global.exclude = ifelse(prop < .8, "EXCLUDE", "INCLUDE"))%>%
  distinct(SID, global.exclude) 

global.check %>%
  filter(global.exclude == "EXCLUDE")%>%
  distinct(SID) #n = 8 kids missing more than 20% of data post task exclusions

# add to data 
data.raw <- left_join(data.raw, global.check, by = "SID")

data.raw %<>%
  filter(global.exclude != "EXCLUDE")%>%
  dplyr::select(-global.exclude)

#how many kids?
data.raw %>%
  distinct(SID, Age)

# Highest contiguous Next Number ----

##Highest contiguous NN = highest number successfully generated,
#provided that previous numbers were correct. Children who failed training given HCNN of 0 (N = 2). 

#Get kids who failed NN for highest contiguous
failed.nn <- data.raw %>%
  filter(Task == "WCN", 
         Correct == 0, 
         Trial_number == "Training")

failed.nn.sids <- unique(as.vector(failed.nn$SID))

#get unique ids
unique.nn <- data.raw %>%
  filter(Task == "WCN")%>%
  distinct(SID)

unique.nn <- as.vector(unique.nn$SID)
nextnums <- as.vector(c(7, 26, 30, 62, 83, 95, 71, 24))

#this is a function that pulls out the largest number for which a participant had a correct consecutive
get_contiguous <- function(){
  contig <- data.frame()
  for (sub in unique.nn) {
    tmp <- data.raw %>%
      filter(Task == "WCN",
             SID == sub, 
             Correct == 0)%>%
      mutate(Task_item= as.integer(as.character(Task_item)))%>%
      mutate(Task_item = sort(as.integer(as.character(Task_item))))
    if (length(tmp$SID) == 0) {
      highest_contig = 95
      sub_contig <- data.frame(sub, highest_contig) 
      sub_contig %<>%
        mutate(sub = as.character(sub),
               highest_contig = as.character(highest_contig))
      contig <- bind_rows(contig, sub_contig)
    } else if (sub %in% failed.nn.sids) {
      highest_contig = 0
      sub_contig <- data.frame(sub, highest_contig) 
      sub_contig %<>%
        mutate(sub = as.character(sub),
               highest_contig = as.character(highest_contig))
      contig <- bind_rows(contig, sub_contig)
    } else if (length(tmp$Task_item) > 0 & min(as.integer(as.character(tmp$Task_item))) == 7) {
      highest_contig = 1
      sub_contig <- data.frame(sub, highest_contig)
      sub_contig %<>%
        mutate(sub = as.character(sub),
               highest_contig = as.character(highest_contig))
      contig <- bind_rows(contig, sub_contig)
    } else {
      min.nn <- min(as.integer(as.character(tmp$Task_item)))
      prev_correct <- nextnums[nextnums < min.nn]
      highest_contig <- max(prev_correct)
      
      sub_contig <- data.frame(sub,
                               highest_contig) 
      sub_contig %<>%
        mutate(sub = as.character(sub),
               highest_contig = as.character(highest_contig))
      contig <- bind_rows(contig, sub_contig)
    }
  }
  contig %<>%
    mutate(highest_contig = as.character(highest_contig))
  return(contig)
}

#run this for each SID in lookup
highest_contiguous_nn <- get_contiguous()%>%
  dplyr::rename(SID = sub)

#add this to df 
data.raw <- left_join(data.raw, highest_contiguous_nn, by = "SID")

data.raw %<>%
  mutate(highest_contig = ifelse(is.na(highest_contig), NA, as.numeric(as.character(highest_contig))))

# Highest next number (HNN) ----

##Find the highest next number answered correctly for each participant.

#Create a lookup table with the highest NN correctly answered
lookup <- data.raw %>%
  filter(Task == "WCN")%>%
  filter(Correct == 1)%>%
  group_by(SID)%>%
  summarise(highest_next_num = max(as.integer(as.character(Task_item))))

## add to data
data.raw <- left_join(data.raw, lookup, by = "SID")

# Highest count ----
hc.df <- read.csv('../Data/sf_math_hc.csv', na.strings = c("NA", "NaN", "NA ", "", " "))%>%
  dplyr::select(-IHC_single, - FHC_single, -Special_count, -Notes, -RMS.note)%>%
  filter(Exclude_trial != 1)%>%
  dplyr::rename(FHC = FHC_final, 
                IHC = IHC_final)%>%
  filter(!is.na(FHC), 
         !is.na(IHC))%>%
  mutate(IHC = ifelse(as.integer(as.character(IHC)) > 120, 120, as.integer(as.character(IHC))), 
         FHC = ifelse(as.integer(as.character(FHC)) > 120, 120, as.integer(as.character(FHC)))) #add cap to IHC and FHC

##Resilient/Non-Resilient classification##
#Children are classified as Productive if they are able to count at least 2 decades 
#past an error without making more than 3 errors within those 2 decades. 

#remove anyone not in trial data
unique_SIDs <- as.vector(unique(hc.df$SID))
unique_trial_SIDS <- as.vector(unique(data.raw$SID))

hc.df %<>%
  filter(SID %in% unique_trial_SIDS)

#Make a dataframe to run the productive function on
hc <- hc.df %>% 
  dplyr::select(SID, Last_successful, IHC, FHC) %>% 
  mutate_at(c('Last_successful','IHC','FHC'),
            function(col) as.integer(str_replace_all(col,'\\D',''))) %>% 
  mutate(Last_successful = ifelse(is.na(Last_successful), 120, Last_successful))%>%
  mutate(SID = as.character(SID))



#check ns
unique_SIDs <- as.vector(unique(hc.df$SID)) #145
unique_trial_SIDS <- as.vector(unique(data.raw$SID)) #145

#### Resilience classification ####

is.productive = function(subject){
  # takes as input the data for a single subject
  # RULES:
  # - counts to 120 unaided = productive
  # - after making first error, counts >= 20 higher, with no more than 3 errors on way
  if(subject$IHC[1] >= 120){
    # if they get to 120 on first try, = productive
    return("Productive")
  } else if(subject$FHC[1] == 120 & nrow(subject) < 4) {
    return("Productive")
  } else if(subject$FHC[1] < 120 & nrow(subject) == 1 
            & subject$FHC[1] == subject$IHC[1]) {
    return("Nonproductive")
  } else if((subject$FHC[1] - subject$IHC[1]) >= 20){
    # if their final is >= 20 larger than their intial...
    if(nrow(subject) < 4){
      # and they've made 3 or fewer total errors, = productive
      return("Productive")
    } 
    else {
      for(i in 1:nrow(subject)){ # start at row 2
        # check if they ever made it >= 20 counts & <= 3 errors after an error
        runLength = 0 # they just made an error, so no post-error successes yet
        numErrors = 0 # first row was an error if it's not finalCount == 120
        prev = subject$Last_successful[i]
        for (j in i+1:nrow(subject)){ # from current row until end...
          numErrors = numErrors + 1 # new row means new error
          runLength = runLength + (subject$Last_successful[j] - prev)
          # ^ add difference between current count and last count to run length
          prev = subject$Last_successful[j] # update last count
          if(runLength >= 20 & numErrors < 4){
            # if at any point the productivity conditions are met...
            return("Productive") # = productive
          }
        }
      }
      # productivity conditions were never met (because we got to this point) so...
      return("Nonproductive") # != productive
    }
  } else {
    # highest is not >= 20 greater than initial
    return("Nonproductive")
  }
}

#run resilience classification for all participants 
class_prod <- function(vector) {
  temp_data <- data.frame()
  for (i in vector) {
    prod.class <- data.frame(i, is.productive(subset(hc, SID == i)))
    # print(i) # for debugging
    names(prod.class) <- c("SID", "productive")
    prod.class %<>%
      mutate(SID = as.character(SID), 
             productive = as.character(productive))
    temp_data <- bind_rows(temp_data, prod.class)
  }
  return(temp_data)
}
# 

#get productive classification for every participant
productive <- class_prod(unique_SIDs)%>%
  dplyr::rename(Productive = productive)

#make a hc data frame for prompt and descriptive info for analysis

highest_counts <- left_join(hc, productive, by = "SID")

# save and export

save(highest_counts, file = '../Data/highest_count_full.RData')

#remove last-successful from hc so you can add IHC and FHC to data.raw
hc %<>%
  dplyr::select(-Last_successful)%>%
  distinct(SID, IHC, FHC)

#add to data
data.raw <- left_join(data.raw, hc, by = "SID")

#now add productive and rename
data.raw <- left_join(data.raw, productive, by = "SID")

data.raw %<>%
  mutate(Productive = factor(Productive, levels = c("Productive", "Nonproductive"), 
                             labels = c("Resilient", "Non-resilient")))

## Filter out training trials
data.raw %<>%
  filter(Trial_number != "Training")

# Save data ----
all.data <- data.raw

save(all.data, file="../Data/sf_math_data_cleaned.RData")

write.csv(all.data, file="../Data/sf_math_data_cleaned.csv")


