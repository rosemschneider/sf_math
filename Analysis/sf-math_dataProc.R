##SF-math data cleaning and processing
##Rose M. Schneider
##8/19/19

##Takes as input raw trial and counting data for SF-Math project, cleans, and returns processed data with productive/nonproductive classification

#### Load libraries ####
rm(list = ls())
project_root <- here::here() 
library(tidyverse)
library(magrittr)
library(langcog)
library(lme4)
library(stringr)
library(RColorBrewer)
library(ggthemes)

#set file name
data_name <- "sf_math_data.csv"
hc_name <- "sf_math_hc.csv"

####  Load data ####
###Trial data
data.raw <- read.csv(fs::path(project_root, "Data", data_name))%>%
  filter(SID != "CopyPasteMe", #filter out template data from coding sheet
         SID != "?")%>% #hard-code, one participant with unrecorded SID
  droplevels()%>% 
  mutate(Age = as.numeric(as.character(Age)),
         Correct = as.integer(as.character(Correct)))%>%
  mutate(Age = round(Age, 2))%>%
  dplyr::select(-Response_single, -Response_double)%>% #remove double coding
  dplyr::rename(Response = Response_final)%>%#rename for code
  mutate(Exclude_trial = ifelse(Exclude_trial == "#NAME?", 0, as.numeric(as.character(Exclude_trial))))

#### Load Highest count data ####
hc.df <- read.csv(fs::path(project_root, "Data", hc_name))%>%
  dplyr::select(-IHC_single, - FHC_single, -Special_count, -Notes, -RMS.note)%>%
  filter(Exclude_trial != 1)%>%
  dplyr::rename(FHC = FHC_final, 
                IHC = IHC_final)%>%
  filter(!is.na(FHC), 
         !is.na(IHC))%>%
  mutate(IHC = ifelse(as.integer(as.character(IHC)) > 120, 120, as.integer(as.character(IHC))), 
         FHC = ifelse(as.integer(as.character(FHC)) > 120, 120, as.integer(as.character(FHC)))) #add cap to IHC and FHC

#### Data manipulations #####

##Resilient/Non-Resilient classification##
#Children are classified as Productive if they are able to count at least 2 decades 
#past an error without making more than 3 errors within those 2 decades. 

#Make a dataframe to run the productive function on
hc <- hc.df %>% 
  dplyr::select(SID, Last_successful, IHC, FHC) %>% 
  mutate_at(c('Last_successful','IHC','FHC'),
            function(col) as.integer(str_replace_all(col,'\\D',''))) %>% 
  mutate(Last_successful = ifelse(is.na(Last_successful), 120, Last_successful))%>%
  mutate(SID = as.character(SID))

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

#make function to run for all participants
unique_SIDs <- as.vector(unique(hc.df$SID))

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

#remove last-successful from hc so you can add IHC and FHC to data.raw
hc %<>%
  dplyr::select(-Last_successful)

#add productive classifications
productive <- full_join(productive, hc, by = "SID")%>%
  distinct(SID, IHC, FHC, Productive)

#full join with raw data
data.raw <- full_join(data.raw, productive, by = "SID") 

#made SID and Productive factors again #MSaPFA
data.raw %<>%
  mutate(SID = factor(SID), 
         Productive = factor(Productive))

#### Highest contiguous Next Number ####

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
data.raw <- full_join(data.raw, highest_contiguous_nn, by = "SID")


#### Highest next number (HNN) ####

##Find the highest next number answered correctly for each participant.

#Create a lookup table with the highest NN correctly answered
lookup <- data.raw %>%
  filter(Task == "WCN")%>%
  filter(Correct == 1)%>%
  group_by(SID)%>%
  summarise(max = max(as.integer(as.character(Task_item))))

#get participants who failed training trial & add to vector
no.corr.nn <- data.raw %>%
  filter(Task == "WCN")%>%
  group_by(SID)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))%>%
  filter(mean == 0)

no.corr.nn.sids <- as.vector(unique(no.corr.nn$SID))

#Function that adds the highest NN to a participant's row in the SF dataframe
add_highest_num <- function(df) {
  tmp <- data.raw
  for (row in 1:nrow(tmp)) {
    sub = as.character(tmp[row, "SID"])
    exclude = as.character(tmp[row, "Exclude_analysis"])
    if (sub %in% no.corr.nn.sids) {
      highest_num = 0
      tmp[row, "highest_num"] = highest_num
    } else if (exclude == "1") {
      highest_num = NA
    } else {
      highest_num = subset(lookup, SID == sub)$max
      tmp[row, "highest_num"] = highest_num
    }
  }
  return(tmp)
}

#run this function on SF dataframe
data.raw <- add_highest_num(data.raw)
output_path <- fs::path(project_root, "Data")

write_csv(data.raw,path=paste0(output_path,"/","sf-math_data_processed.csv"))


