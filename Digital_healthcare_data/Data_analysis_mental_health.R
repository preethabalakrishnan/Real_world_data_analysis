# Name: Data analysis Menta1 Health
# Description: This is a code to cleaning dummy data for studying the effectiveness of a mental health App
# Author:Preetha
#Date: 29.09.2023


##### Step 1: Install the necessary Package to start analysis#####
#Install dplyr to start basic analysis and learn more about the data
install.packages("dplyr")
library(dplyr)

##### Step 2: CLEANING: Check unique values in each column to ensure if the data is complete and how to clean/filter data #####

#Step2.1: Read the csv and create a backup version
raw_data <- read.csv("dummy_patient_reported_data.csv") # the input dataset
raw_data_bk <- raw_data # a back up to ensure or compare changes that could alter data in the csv


#Step2.2: Check the number of unique participants in the csv
uni_par<- length(unique(raw_data$id)) # data from 100 participants available

#Step 2.3: Look at the different values for time. It will help in cleaning 
time_values <- unique(raw_data$timepoint) #T0 to T6 values available. questions asked every 7 days

#Step 2.4: Change name-timepoint_3 to T3 -useful for cleaning and check if the name change worked
raw_data_T3 <- raw_data %>%
  mutate(., timepoint=ifelse(timepoint=="timepoint_3","T3",timepoint))

# Sanity check for the if else statement
SC_1 <- unique(raw_data_T3$timepoint) # all values of timepoint_3 seems to have changed to T3 and a random check for other values wrt is ok

#Step 2.5: Check the diff forms in which Gender is specified
uni_gender <- unique(raw_data_T3$gender)

#Step 2.6:Gender is written both in English and German. Keep it consistent and change everything to English
raw_T3_gen <- raw_data_T3 %>%
  mutate(., gender=ifelse(gender=="weiblich","female",gender))%>%
  mutate(., gender=ifelse(gender=="m","male",gender))

# Sanity check for the name change in the gender column
SC_2 <- raw_T3_gen %>%
  group_by(., gender) %>%
  summarise(., gender_count = length(unique(id)))# 50 males and 50 females in the changes set

SC_3 <- raw_data_T3 %>%
  group_by(., gender) %>%
  summarize(., gender_count = length(unique(id))) # the total no of females and males are 50 each in the original set

#Step 2.7: Check the diff values in confidence_handleAnxiety
con_anx <- data.frame(unique(raw_data_T3$confidence_handleAnxiety))# values from 1 to 12 and NA . 

#Step 2.8: How many participants have an NA in any time point?
con_anx_na <- raw_T3_gen %>% 
  filter(., is.na(confidence_handleAnxiety)) %>% 
  subset(., select = c(id))%>%
  filter(., !duplicated(.)) # 79 participants have an NA at one time point or the other.We cannot exclude 79 participants from analysis. We can either impute data if all time points need to be included  or find another alternative to analyse data using baseline and T6

# Step 2.9 : Check if the NA's have a pattern based on time points
con_anx_na_tp <- raw_T3_gen %>% 
  filter(., is.na(confidence_handleAnxiety)) %>% 
  subset(., select = c(id, confidence_handleAnxiety, timepoint)) %>% 
  group_by(., timepoint) %>%
  summarise(., total_par = length(unique(id))) # At T0 there is no NA, which is good. T6-32 participants and T5-27 participants

#Step 2.10: Get the IDs of participants with NA at T0 or T6 in the confidence column as we are planning to use this time points in our study
con_anx_na_T6 <- raw_T3_gen %>% 
  filter(., timepoint == 'T6') %>%
  filter(., is.na(confidence_handleAnxiety)) %>%
  subset(., select = c(id))%>%
  filter(., !duplicated(.)) # these participants should be excluded from the original data set for the confidence parameter

#Step 2.11.1: Check the diff values in worry about panic attack at T0 and T6 
wry_pan <- data.frame(unique(raw_data_T3$worry_aboutPanicAttack))# values from 2 to 13 and NA. For sure,1 would be an option but none have chosen it

#Step 2.11.2: As per the developer, the input values of 0-10 are offered as options in COnfidence and worry ques, so any value above 10 can be converted to 10 as the participant meant to rate it as high as possible
raw_T3_gen_grt_10 <- raw_T3_gen %>%
  mutate(., confidence_handleAnxiety=ifelse(confidence_handleAnxiety > 10, 10, confidence_handleAnxiety)) %>%
  mutate(., worry_aboutPanicAttack=ifelse(worry_aboutPanicAttack > 10, 10, worry_aboutPanicAttack))# This df has to be used for further analysis
  
#Step 2.12: Check how many IDs have an NA for this question at diff timepoints
wry_pan_na_tp <- raw_T3_gen %>% 
  filter(., is.na(worry_aboutPanicAttack)) %>%
  subset(., select = c(id, worry_aboutPanicAttack, timepoint))%>%
  group_by(., timepoint) %>%
  summarise(., total_par = length(unique(id)))# 22 participants had an NA at T0

# Step 2.13: Get the set of unique participants who had an NA for worry at T0 or T6
wry_pan_na_T0_T6 <- raw_T3_gen %>%
  filter(., timepoint == 'T6' | timepoint == 'T0') %>%
  filter(., is.na(worry_aboutPanicAttack)) %>%
  subset(., select = c(id))  %>%
  filter(., !duplicated(.)) # 38 entries must be removed from the original dataset and the rest can be used for analysis
  
#Step 2.14: Check the diff values in no of panic attacks
no_pan_atk <- data.frame(unique(raw_data_T3$n_PanicAttacks))# values from 0 to 7 and -999. -999 seems like a back-end value added while collecting data.Mostly, it may be an empty or unanswered value

#Step 2.15: Check how many ID#s have a  -999 for this question based on timepoints
no_pan_atk_999_tp <- raw_T3_gen %>% 
  filter(., n_PanicAttacks== -999) %>%
  subset(., select = c(id, n_PanicAttacks, timepoint)) %>%
  group_by(., timepoint) %>%
  summarise(., total_par = length(unique(id))) # 22 participants had a NA at T0,  25 had -999 at T6

#Step 2.16: Get the IDs of participants with -999 for no of panic attacks at T0 and T6
no_pan_atk_T0_T6 <- raw_T3_gen %>%
  filter(., timepoint == 'T6' | timepoint == 'T0') %>%
  filter(., n_PanicAttacks== '-999') %>%
  subset(., select = c(id)) %>%
  filter(., !duplicated(.)) # 38 participants have to be removed from the original data set for analysis

### Step 2.17: Exclude the participants with an NA/-999 from the original data
#Method1: Exclude all patients from all end
# Step 2.17.1 If we want to include only those patients who have answered or given a score at T0 and T6 for all end points, this is the df that needs to be used for statistical analysis
final_data_all <- raw_T3_gen_grt_10 %>%
  filter(., timepoint == 'T6' | timepoint == 'T0') %>%
  filter(., !(id%in%con_anx_na_T6$id)) %>%
  filter(., !(id%in%wry_pan_na_T0_T6$id)) %>%
  filter(., !(id%in%no_pan_atk_T0_T6$id))# Only 20 patients can be included in the study if NA's and -999 from T0 and T6 are removed from all 3 endpoints. The sample size is too small to start a study or analysis

# Method2:Exclude patients with NA specific to each question
#Step 2.17.2 Remove NA from confidence ques. Ths is the df that will be used for statistical analysis
con_anx_cln <- raw_T3_gen_grt_10 %>%
  subset(., select = -c(worry_aboutPanicAttack, n_PanicAttacks, X))%>%
  filter(., timepoint == 'T6' | timepoint == 'T0') %>%
  filter(., !(id%in%con_anx_na_T6$id)) # From a total of 100, 32 were excluded so, 68 participants should be remaining with 2 entries per participant
  
# Step 2.17.3 Remove NA from worry ques. This is the df for further analysis
wry_pan_atk_cln <- raw_T3_gen_grt_10 %>%
  subset(., select = -c(confidence_handleAnxiety, n_PanicAttacks, X))%>%
  filter(., timepoint == 'T6' | timepoint == 'T0') %>%
  filter(., !(id%in%wry_pan_na_T0_T6$id)) # From a total of 100, 38 were excluded. Remaining 64 will be a part of the analysis

# Step 2.17.4 Remove -999 from no of panic attacks ques. This is the df for further analysis
no_pan_atk_cln <- raw_T3_gen_grt_10 %>%
  subset(., select = -c(confidence_handleAnxiety, worry_aboutPanicAttack, X))%>%
  filter(., timepoint == 'T6' | timepoint == 'T0') %>%
  filter(., !(id%in%no_pan_atk_T0_T6$id)) # From a total of 100, 38 were excluded. Remaining 64 will be a part of the analysis

###### Step 3: Data preparation and transformation for statistical analysis #######
#Step 3.1:  Modify the df of each question such that T0 and T6 are two separate columns
#Install the maditr library to use the dcast function 
#install.packages('maditr')
library('maditr')

# Dcast is used here to change the data format such that T0 and T6 become separate columns for each participant
con_anx_cln_cast <- dcast(con_anx_cln, id + age + gender ~ timepoint, value.var = "confidence_handleAnxiety")
wry_pan_atk_cln_cast <- dcast(wry_pan_atk_cln, id + age + gender ~ timepoint, value.var = "worry_aboutPanicAttack") 
no_pan_atk_cln_cast <- dcast(no_pan_atk_cln, id + age + gender ~ timepoint, value.var = "n_PanicAttacks") 


###### Step 4: Statistical analysis #####################
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("PairedData")
#install.packages(" rstatix") 
#install.packages('coin')
#install.packages("plyr")
library(ggplot2)
#library(vctrs)
#library(tidyverse)
#library(stats)

#library(coin)

#Step 4.1: Descriptive statistics- Baseline characteristics
base_stats <-  function(x= clean_cast_df_input, y= clean_df_input) {
  
  #Step 1: Calculate the mean age
  mean_age <- mean(x$age)
  
  #Step 2: Calculate the variance and standard deviation of T0 and T6
  mean_T0 <- mean(x$T0)
  var_T0 <- var(x$T0)
  std_T0 <- sqrt(var_T0)
  mean_T6 <- mean(x$T6)
  var_T6 <- var(x$T6)
  std_T6 <- sqrt(var_T6)
  
  # Step 3: Group the data based on Gender and calcúlate the respective mean at T0 and T6
  gender_diff_median <- x %>%
    group_by(., gender) %>%
    summarise(., total_no = length(unique(id)),T0_median = median(T0), T6_median = median(T6))
  
  # Step 4: Calculate the mean scores based on age and plot the graph to check if there are any diff
  colnames(y)[5] <- "value"
  gender_diff_mean <- y %>%
    group_by(., gender, timepoint) %>%
    summarise(., total_no = length(unique(id)),mean_score = mean(value))
  
  #Step 5: Plot graph with mean values
  bar_graph <- ggplot(data = gender_diff_mean, 
         aes(x = gender, 
             y = mean_score , 
             fill = timepoint )) + 
    geom_bar(stat = "identity", 
             position = position_dodge(.9))
  
  
  # Put the results in a list:
  results_list <- list(gender_diff_median, gender_diff_mean, "mean_age" = mean_age, "mean_T0" = mean_T0, "mean_T6" = mean_T6, "var_T0" =  var_T0, "std_T0" = std_T0, "std_T6" = std_T6,  "var_T6" =  var_T6 )
  
  print(bar_graph)
  return(results_list)
}


#Step 4.2: Call the function for each question
con_anx_stat_res <- base_stats( x = con_anx_cln_cast, y = con_anx_cln)
wry_anx_stat_res <- base_stats( x = wry_pan_atk_cln_cast, y = wry_pan_atk_cln)
no_pan_atk_stat_res <- base_stats( x = no_pan_atk_cln_cast,  y = no_pan_atk_cln)

#Step 4.3: Visualize the T0 and T6 values in a histogram
hist(con_anx_cln_cast$age, xlab = "Age" )
hist(con_anx_cln_cast$T0)
hist(con_anx_cln_cast$T6)

##*************** Step 4.4: Statistical Analysis ****************************************************###

#Step 4.4.1: Add the necessary libraries
library(plyr) # For ddply function
library(ggsignif)# For geom_signif
library(ggprism) # To obtain prism like plot settings 
library('rstatix') # FOr wilcoxon effect size
#install.packages("RColorBrewer")
install.packages("ggprism")
library(RColorBrewer) # for changing the color of the graphs
install.packages('base') # for concatenating characters
library(base)

#Step 4.4.2: Create a function that calculates the differences between T0 and T6 and plots a graph for each reported parameter
  stat_analysis <-  function(x= clean_cast_df_input, y= clean_df_input, z = title_of_the_plot, a = y_axis_title) {
  
  #Step 1: Check if the data is normally distributed
  #WE already know that the data is not normally distributed. This is just to be sure if the data is normally distributed
  nor_T0 <- shapiro.test(x$T0)
  nor_T6 <- shapiro.test(x$T6)
  
  #Step 2: Statistical analysis using Wilcoxon's signal rank test 
  wilcox_result <- wilcox.test(x$T0, x$T6, paired = TRUE, alternative = "two.sided")
  
  # Step 3: Check the effect size
  # Change the column name of the last column in the clean df input
  colnames(y)[5] <- "value"
  effect_size_test <- y %>% 
    wilcox_effsize(value ~ timepoint, paired = TRUE, alternative = "two.sided")
  
  # Step 4: Calculate the median values for T0 and T6
  med_score <- ddply(y, .(timepoint), summarise, score = median(value))
  
  #Step 5: Plot the graph
  box_plot_grp <- ggplot(y,aes(x = timepoint, y = value, fill = timepoint)) + 
    geom_boxplot(width=0.4, linewidth = 0.7, fatten = 1.4) +
    geom_signif(comparisons = list(c("T0", "T6")), map_signif_level = TRUE, size = 0.7) +
    scale_x_discrete(labels=c("Baseline","After 6 weeks")) +
    ggtitle(as.character(z)) +
    labs(x="Group", y = as.character(a)) +
    scale_fill_brewer(palette="Oranges") +
    geom_text(aes(label = paste("p =", format(wilcox_result$p.value, digits = 3))), x = 1.5, y = 9.5 , hjust = 0.5, vjust = -1, size = 3) +
    geom_text(data = med_score, aes(x = timepoint, y = score, label = score), size = 3, vjust = -0.5) +
    theme_prism() +
    theme(legend.position="none") +
    theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10), plot.title = element_text(size = 11, hjust = 0.5))

  plot_pdf <- ggsave(box_plot_grp, width = 4, height = 4, file = paste(z, "pdf", sep = "."))
  
  # Step 6: Put the results in a list:
  results_list <- list("nor_T0" = nor_T0, "nor_T6" = nor_T6, wilcox_result, effect_size_test)
  
  print(box_plot_grp)
  return(results_list)
  
}


#Step 4.4.2: Call the function
con_anx_stat_diff <- stat_analysis( x = con_anx_cln_cast, y = con_anx_cln, z = 'Confidence to handle anxiety', a = 'Median score')
wry_pan_stat_diff <- stat_analysis( x = wry_pan_atk_cln_cast, y = wry_pan_atk_cln, z = 'Worry about panic attacks', a = 'Median score')
no_pan_atk_stat_diff <- stat_analysis( x = no_pan_atk_cln_cast, y = no_pan_atk_cln, z = 'No of panic attacks', a = 'Value')# #V = 353, p-value = 0.1892, it shows that the number of panic attacks remained the same


#************** Use only as back-up- other ideas ************************
###Step 2.17: Check if the same users have NA or -999 across questions.T5 seems to be a better end timepoint as we do not lose many participants
#Step2.17.1: Filter the T5 values for confidence with NA. T0 had no NA's so its not needed
con_anx_na_T5 <- raw_T3_gen %>%
  filter(., timepoint == 'T5') %>%
  filter(., is.na(confidence_handleAnxiety))  %>% # At T5, there are 27 users who have an NA. If these are the same across all questions, we could probably leave them out from analysis
  subset(., select = c(id))# These IDs need to be excluded from the original dataset
         
#Step2.17.2: Filter the T0 values for worry and check how many users have an NA in both confidence and worry at T6
wry_pan_T0_T5 <- raw_T3_gen %>%
  filter(., timepoint == 'T0' | timepoint == 'T5') %>%
  filter(., is.na(worry_aboutPanicAttack)) %>%
  subset(., select = c(id)) %>%
  filter(., !duplicated(.)) # 22 unique participants in worry about Panic attack , who have NA

#Step2.17.3: Filter the T0 values for no of panic attacks
no_pan_atk_T0_T5 <- raw_T3_gen %>%
  filter(., timepoint == 'T0' | timepoint == 'T5') %>%
  filter(., n_PanicAttacks == -999) %>%
  subset(., select = c(id)) %>%
  filter(., !duplicated(.))# 22 participants have an NA for this question

#Step 2.17.4: Check the total ID's which have an NA or -999 in any of these questions 
cln_raw_t3_gen_T5 <- raw_T3_gen %>%
  filter(., timepoint == 'T0' | timepoint == 'T5') %>%
  filter(., !(id%in%no_pan_atk_T0_T5$id))%>%
  filter(., !(id%in%wry_pan_T0_T5$id))%>%
  filter(., !(id%in%con_anx_na_T5$id)) # A total of 45 participants/patients are a part of the analysis
