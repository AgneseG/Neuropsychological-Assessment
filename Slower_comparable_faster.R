### Note: this script follows the previous "time_between_correctwords.Rmd"

library(dplyr)
library(openxlsx)

###################################################################################################################
### Stage 1
###################################################################################################################

time_between_words_stage1 <- read.table("time_between_words_stage1.csv", header = T, sep=",")
time_between_words_stage1 <- time_between_words_stage1[,-1]

healthy_controls_stage1 <- time_between_words_stage1 %>% filter(Group=="healthy_controls") %>% as.data.frame()

time_between_words_stage1_HC_mean <- healthy_controls_stage1 %>% select(-patientId, -Group) %>% summarise_all(funs(mean(.,na.rm=TRUE)))
time_between_words_stage1_HC_mean  # Mean of the time between correct words at each word said (i=1,...,12)

time_between_words_stage1_HC_sd <- healthy_controls_stage1 %>% select(-patientId, -Group) %>% summarise_all(funs(sd(.,na.rm=TRUE)))
time_between_words_stage1_HC_sd    # SD of the time between correct words at each word said (i=1,...,12)


left_CI_stage1 <- right_CI_stage1 <- rep(NA, 12)

for (i in 1:12){
  error <- qnorm(0.95)*time_between_words_stage1_HC_sd[i]/sqrt(nrow(healthy_controls_stage1))
  left_CI_stage1[i] <-  round(unlist(time_between_words_stage1_HC_mean[i] - error))
  right_CI_stage1[i] <- round(unlist(time_between_words_stage1_HC_mean[i] + error))
}

# Checking whether the patient, for each correct response, took more, less, or comparable time to give the answer, compared to healthy controls
###############################################################################################################################################

patients_1 <- time_between_words_stage1 %>% filter(Group!="healthy_controls") %>% as.data.frame()

time_between_words_patients_1 <- patients_1[,-c(1,14)] # Excluding group and patient indication

below_within_above1 <- matrix(NA, nrow=nrow(patients_1), ncol=12)

## faster

each_word_comparison_1 <- matrix(NA, nrow=nrow(time_between_words_patients_1), ncol=12)

for (i in 1:nrow(time_between_words_patients_1)){
  each_word_comparison_1[i,] <- as.matrix(time_between_words_patients_1[i,]) < as.vector(left_CI_stage1) # faster
}


for (i in 1:12){
  for (j in 1:nrow(patients_1)){
    if(!is.na(each_word_comparison_1[j,i])){
      if (each_word_comparison_1[j,i]==TRUE){
        below_within_above1[j,i] <- "faster"
      }
    }
    else NULL
  }
}

## slower

each_word_comparison_1 <- matrix(NA, nrow=nrow(time_between_words_patients_1), ncol=12)

for (i in 1:nrow(time_between_words_patients_1)){
  each_word_comparison_1[i,] <- as.matrix(time_between_words_patients_1[i,]) > as.vector(right_CI_stage1) # slower
}


for (i in 1:12){
  for (j in 1:nrow(patients_1)){
    if(!is.na(each_word_comparison_1[j,i])){
      if (each_word_comparison_1[j,i]==TRUE){
        below_within_above1[j,i] <- "slower"
      }
    }
    else NULL
  }
}

## comparable

each_word_comparison_1 <- matrix(NA, nrow=nrow(time_between_words_patients_1), ncol=12)

for (i in 1:nrow(time_between_words_patients_1)){
  each_word_comparison_1[i,] <- as.matrix(time_between_words_patients_1[i,]) <= as.vector(right_CI_stage1) & as.matrix(time_between_words_patients_1[i,]) >= as.vector(left_CI_stage1) # comparable
}


for (i in 1:12){
  for (j in 1:nrow(patients_1)){
    if(!is.na(each_word_comparison_1[j,i])){
      if (each_word_comparison_1[j,i]==TRUE){
        below_within_above1[j,i] <- "comparable"
      }
    }
    else NULL
  }
}

# Summarising number of responses that were slower, comparable or faster in a single dataframe
##################################################################################################################

sum_comparability_1 <- matrix(NA, nrow=nrow(patients_1), ncol=3)
colnames(sum_comparability_1) <- c("slower", "comparable", "faster")

for ( i in 1:nrow(below_within_above1)){
  sum_comparability_1[i,1] = length(below_within_above1[which(below_within_above1[i,]=="slower")])
  sum_comparability_1[i,2] = length(below_within_above1[which(below_within_above1[i,]=="comparable")])
  sum_comparability_1[i,3] = length(below_within_above1[which(below_within_above1[i,]=="faster")])
}

sum_comparability_1

# Sumamrising results on this trial
###################################################################################################################

RAVLT_totalcorrectwords_1 <- rowSums(!is.na(below_within_above1)) # Total number of correct responses
RAVLT_slower_1 <- sum_comparability_1[,1]
RAVLT_comparable_1 <- sum_comparability_1[,2]
RAVLT_faster_1 <- sum_comparability_1[,3]
RAVLT_score_1 <- round(RAVLT_slower_1/RAVLT_totalcorrectwords_1, 2) # How many times the patient was slower? --> Indication of the patient's impairment
RAVLT_score_percentage_1 <- RAVLT_score_1*100 

# Standard Error of the time between correct responses, both on the total responses and on the first-second half of them:
# Did the performance of the patient change over time? 

RAVLT_SE_total_1 <- round(apply(time_between_words_patients_1, 1, sd, na.rm=TRUE)/sqrt(RAVLT_totalcorrectwords_1), 2)

firstpart1 <- trunc(RAVLT_totalcorrectwords_1/2) # Defining first part 
secondpart1 <- RAVLT_totalcorrectwords_1 - firstpart1 # Defining second part 

RAVLT_SE_1stpart_1 <- RAVLT_SE_2ndpart_1 <- rep(NA, length(firstpart1)) # Computing the respective SEs

       for (i in 1:nrow(time_between_words_patients_1)) {
  RAVLT_SE_1stpart_1[i] <-   round(sd(time_between_words_patients_1[i,1:firstpart1[i]], na.rm=TRUE)/sqrt(firstpart1[i]),2)
}

       for (i in 1:nrow(time_between_words_patients_1)) {
  RAVLT_SE_2ndpart_1[i] <-  round(sd(time_between_words_patients_1[i,(firstpart1[i]+1):RAVLT_totalcorrectwords_1[i]], na.rm=TRUE)/sqrt(secondpart1[i]),2)
  }

# Summarising results of this trial in a dataframe

final_excel <- cbind(as.character(patients_1[,1]), RAVLT_totalcorrectwords_1,                            # Patient information and number of correct responses on this trial 
                     RAVLT_slower_1, RAVLT_comparable_1, RAVLT_faster_1, RAVLT_score_1, RAVLT_score_percentage_1,  # How many times the patient was slower, comparable to Healthy Controls, or Faster? 
                     RAVLT_SE_total_1, RAVLT_SE_1stpart_1, RAVLT_SE_2ndpart_1)                                     # Defining SE of the mean time between correct words, and comparing it in the first and second half ot the trial.

###################################################################################################################
### Stage 2 
###################################################################################################################

time_between_words_stage2 <- read.table("time_between_words_stage2.csv", header = T, sep=",")
time_between_words_stage2 <- time_between_words_stage2[,-1]

healthy_controls_stage2 <- time_between_words_stage2 %>% filter(Group=="healthy_controls") %>% as.data.frame()

time_between_words_stage2_HC_mean <- healthy_controls_stage2 %>% select(-patientId, -Group) %>% summarise_all(funs(mean(.,na.rm=TRUE)))
time_between_words_stage2_HC_mean  # Mean of the time between correct words at each word said (i=1,...,12)

time_between_words_stage2_HC_sd <- healthy_controls_stage2 %>% select(-patientId, -Group) %>% summarise_all(funs(sd(.,na.rm=TRUE)))
time_between_words_stage2_HC_sd    # SD of the time between correct words at each word said (i=1,...,12)


left_CI_stage2 <- right_CI_stage2 <- rep(NA, 14)

for (i in 1:14){
  error <- qnorm(0.95)*time_between_words_stage2_HC_sd[i]/sqrt(nrow(healthy_controls_stage2))
  left_CI_stage2[i] <-  round(unlist(time_between_words_stage2_HC_mean[i] - error))
  right_CI_stage2[i] <- round(unlist(time_between_words_stage2_HC_mean[i] + error))
}

# Checking whether the patient, for each correct response, took more, less, or comparable time to give the answer, compared to healthy controls
###############################################################################################################################################

patients_2 <- time_between_words_stage2 %>% filter(Group!="healthy_controls") %>% as.data.frame()

time_between_words_patients_2 <- patients_2[,-c(1,16)] # Excluding group and patient indication

below_within_above2 <- matrix(NA, nrow=nrow(patients_2), ncol=14)

## faster

each_word_comparison_2 <- matrix(NA, nrow=nrow(time_between_words_patients_2), ncol=14)

for (i in 1:nrow(time_between_words_patients_2)){
  each_word_comparison_2[i,] <- as.matrix(time_between_words_patients_2[i,]) < as.vector(left_CI_stage2) # faster
}


for (i in 1:14){
  for (j in 1:nrow(patients_2)){
    if(!is.na(each_word_comparison_2[j,i])){
      if (each_word_comparison_2[j,i]==TRUE){
        below_within_above2[j,i] <- "faster"
      }
    }
    else NULL
  }
}

## slower

each_word_comparison_2 <- matrix(NA, nrow=nrow(time_between_words_patients_2), ncol=14)

for (i in 1:nrow(time_between_words_patients_2)){
  each_word_comparison_2[i,] <- as.matrix(time_between_words_patients_2[i,]) > as.vector(right_CI_stage2) # slower
}


for (i in 1:14){
  for (j in 1:nrow(patients_2)){
    if(!is.na(each_word_comparison_2[j,i])){
      if (each_word_comparison_2[j,i]==TRUE){
        below_within_above2[j,i] <- "slower"
      }
    }
    else NULL
  }
}

## comparable

each_word_comparison_2 <- matrix(NA, nrow=nrow(time_between_words_patients_2), ncol=14)

for (i in 1:nrow(time_between_words_patients_2)){
  each_word_comparison_2[i,] <- as.matrix(time_between_words_patients_2[i,]) <= as.vector(right_CI_stage2) & as.matrix(time_between_words_patients_2[i,]) >= as.vector(left_CI_stage2) # comparable
}


for (i in 1:14){
  for (j in 1:nrow(patients_2)){
    if(!is.na(each_word_comparison_2[j,i])){
      if (each_word_comparison_2[j,i]==TRUE){
        below_within_above2[j,i] <- "comparable"
      }
    }
    else NULL
  }
}

# Summarising number of responses that were slower, comparable or faster in a single dataframe
##################################################################################################################

sum_comparability_2 <- matrix(NA, nrow=nrow(patients_2), ncol=3)
colnames(sum_comparability_2) <- c("slower", "comparable", "faster")

for ( i in 1:nrow(below_within_above2)){
  sum_comparability_2[i,1] = length(below_within_above2[which(below_within_above2[i,]=="slower")])
  sum_comparability_2[i,2] = length(below_within_above2[which(below_within_above2[i,]=="comparable")])
  sum_comparability_2[i,3] = length(below_within_above2[which(below_within_above2[i,]=="faster")])
}

sum_comparability_2

# Sumamrising results on this trial
###################################################################################################################

RAVLT_totalcorrectwords_2 <- rowSums(!is.na(below_within_above2)) # Total number of correct responses
RAVLT_slower_2 <- sum_comparability_2[,1]
RAVLT_comparable_2 <- sum_comparability_2[,2]
RAVLT_faster_2 <- sum_comparability_2[,3]
RAVLT_score_2 <- round(RAVLT_slower_2/RAVLT_totalcorrectwords_2, 2) # How many times the patient was slower? --> Indication of the patient's impairment
RAVLT_score_percentage_2 <- RAVLT_score_2*100 

# Standard Error of the time between correct responses, both on the total responses and on the first-second half of them:
# Did the performance of the patient change over time? 

RAVLT_SE_total_2 <- round(apply(time_between_words_patients_2, 1, sd, na.rm=TRUE)/sqrt(RAVLT_totalcorrectwords_2), 2)

firstpart2 <- trunc(RAVLT_totalcorrectwords_2/2) # Defining first part 
secondpart2 <- RAVLT_totalcorrectwords_2 - firstpart2 # Defining second part 

RAVLT_SE_1stpart_2 <- RAVLT_SE_2ndpart_2 <- rep(NA, length(firstpart2)) # Computing the respective SEs

for (i in 1:nrow(time_between_words_patients_2)) {
  RAVLT_SE_1stpart_2[i] <-   round(sd(time_between_words_patients_2[i,1:firstpart2[i]], na.rm=TRUE)/sqrt(firstpart2[i]),2)
}

for (i in 1:nrow(time_between_words_patients_2)) {
  RAVLT_SE_2ndpart_2[i] <-  round(sd(time_between_words_patients_2[i,(firstpart2[i]+1):RAVLT_totalcorrectwords_2[i]], na.rm=TRUE)/sqrt(secondpart2[i]),2)
}

# Summarising results of this trial in a dataframe

final_excel <- cbind(final_excel, RAVLT_totalcorrectwords_2,                            # Patient information and number of correct responses on this trial 
                     RAVLT_slower_2, RAVLT_comparable_2, RAVLT_faster_2, RAVLT_score_2, RAVLT_score_percentage_2,  # How many times the patient was slower, comparable to Healthy Controls, or Faster? 
                     RAVLT_SE_total_2, RAVLT_SE_1stpart_2, RAVLT_SE_2ndpart_2)                                     # Defining SE of the mean time between correct words, and comparing it in the first and second half ot the trial.


###################################################################################################################
### Stage 3
###################################################################################################################

time_between_words_stage3 <- read.table("time_between_words_stage3.csv", header = T, sep=",")
time_between_words_stage3 <- time_between_words_stage3[,-1]

healthy_controls_stage3 <- time_between_words_stage3 %>% filter(Group=="healthy_controls") %>% as.data.frame()

time_between_words_stage3_HC_mean <- healthy_controls_stage3 %>% select(-patientId, -Group) %>% summarise_all(funs(mean(.,na.rm=TRUE)))
time_between_words_stage3_HC_mean  # Mean of the time between correct words at each word said (i=1,...,12)

time_between_words_stage3_HC_sd <- healthy_controls_stage3 %>% select(-patientId, -Group) %>% summarise_all(funs(sd(.,na.rm=TRUE)))
time_between_words_stage3_HC_sd    # SD of the time between correct words at each word said (i=1,...,12)


left_CI_stage3 <- right_CI_stage3 <- rep(NA, 15)

for (i in 1:15){
  error <- qnorm(0.95)*time_between_words_stage3_HC_sd[i]/sqrt(nrow(healthy_controls_stage3))
  left_CI_stage3[i] <-  round(unlist(time_between_words_stage3_HC_mean[i] - error))
  right_CI_stage3[i] <- round(unlist(time_between_words_stage3_HC_mean[i] + error))
}

# Checking whether the patient, for each correct response, took more, less, or comparable time to give the answer, compared to healthy controls
###############################################################################################################################################

patients_3 <- time_between_words_stage3 %>% filter(Group!="healthy_controls") %>% as.data.frame()

time_between_words_patients_3 <- patients_3[,-c(1,17)] # Excluding group and patient indication

below_within_above3 <- matrix(NA, nrow=nrow(patients_3), ncol=15)

## faster

each_word_comparison_3 <- matrix(NA, nrow=nrow(time_between_words_patients_3), ncol=15)

for (i in 1:nrow(time_between_words_patients_3)){
  each_word_comparison_3[i,] <- as.matrix(time_between_words_patients_3[i,]) < as.vector(left_CI_stage3) # faster
}


for (i in 1:15){
  for (j in 1:nrow(patients_3)){
    if(!is.na(each_word_comparison_3[j,i])){
      if (each_word_comparison_3[j,i]==TRUE){
        below_within_above3[j,i] <- "faster"
      }
    }
    else NULL
  }
}

## slower

each_word_comparison_3 <- matrix(NA, nrow=nrow(time_between_words_patients_3), ncol=15)

for (i in 1:nrow(time_between_words_patients_3)){
  each_word_comparison_3[i,] <- as.matrix(time_between_words_patients_3[i,]) > as.vector(right_CI_stage3) # slower
}


for (i in 1:15){
  for (j in 1:nrow(patients_3)){
    if(!is.na(each_word_comparison_3[j,i])){
      if (each_word_comparison_3[j,i]==TRUE){
        below_within_above3[j,i] <- "slower"
      }
    }
    else NULL
  }
}

## comparable

each_word_comparison_3 <- matrix(NA, nrow=nrow(time_between_words_patients_3), ncol=15)

for (i in 1:nrow(time_between_words_patients_3)){
  each_word_comparison_3[i,] <- as.matrix(time_between_words_patients_3[i,]) <= as.vector(right_CI_stage3) & as.matrix(time_between_words_patients_3[i,]) >= as.vector(left_CI_stage3) # comparable
}


for (i in 1:15){
  for (j in 1:nrow(patients_3)){
    if(!is.na(each_word_comparison_3[j,i])){
      if (each_word_comparison_3[j,i]==TRUE){
        below_within_above3[j,i] <- "comparable"
      }
    }
    else NULL
  }
}

# Summarising number of responses that were slower, comparable or faster in a single dataframe
##################################################################################################################

sum_comparability_3 <- matrix(NA, nrow=nrow(patients_3), ncol=3)
colnames(sum_comparability_3) <- c("slower", "comparable", "faster")

for ( i in 1:nrow(below_within_above3)){
  sum_comparability_3[i,1] = length(below_within_above3[which(below_within_above3[i,]=="slower")])
  sum_comparability_3[i,2] = length(below_within_above3[which(below_within_above3[i,]=="comparable")])
  sum_comparability_3[i,3] = length(below_within_above3[which(below_within_above3[i,]=="faster")])
}

sum_comparability_3

# Sumamrising results on this trial
###################################################################################################################

RAVLT_totalcorrectwords_3 <- rowSums(!is.na(below_within_above3)) # Total number of correct responses
RAVLT_slower_3 <- sum_comparability_3[,1]
RAVLT_comparable_3 <- sum_comparability_3[,2]
RAVLT_faster_3 <- sum_comparability_3[,3]
RAVLT_score_3 <- round(RAVLT_slower_3/RAVLT_totalcorrectwords_3, 2) # How many times the patient was slower? --> Indication of the patient's impairment
RAVLT_score_percentage_3 <- RAVLT_score_3*100 

# Standard Error of the time between correct responses, both on the total responses and on the first-second half of them:
# Did the performance of the patient change over time? 

RAVLT_SE_total_3 <- round(apply(time_between_words_patients_3, 1, sd, na.rm=TRUE)/sqrt(RAVLT_totalcorrectwords_3), 2)

firstpart3 <- trunc(RAVLT_totalcorrectwords_3/2) # Defining first part 
secondpart3 <- RAVLT_totalcorrectwords_3 - firstpart3 # Defining second part 

RAVLT_SE_1stpart_3 <- RAVLT_SE_2ndpart_3 <- rep(NA, length(firstpart3)) # Computing the respective SEs

for (i in 1:nrow(time_between_words_patients_3)) {
  RAVLT_SE_1stpart_3[i] <-   round(sd(time_between_words_patients_3[i,1:firstpart3[i]], na.rm=TRUE)/sqrt(firstpart3[i]),2)
}

for (i in 1:nrow(time_between_words_patients_3)) {
  RAVLT_SE_2ndpart_3[i] <-  round(sd(time_between_words_patients_3[i,(firstpart3[i]+1):RAVLT_totalcorrectwords_3[i]], na.rm=TRUE)/sqrt(secondpart3[i]),2)
}

# Summarising results of this trial in a dataframe

final_excel <- cbind(final_excel, RAVLT_totalcorrectwords_3,                            # Patient information and number of correct responses on this trial 
                     RAVLT_slower_3, RAVLT_comparable_3, RAVLT_faster_3, RAVLT_score_3, RAVLT_score_percentage_3,  # How many times the patient was slower, comparable to Healthy Controls, or Faster? 
                     RAVLT_SE_total_3, RAVLT_SE_1stpart_3, RAVLT_SE_2ndpart_3)                                     # Defining SE of the mean time between correct words, and comparing it in the first and second half ot the trial.


###################################################################################################################
### Stage 4
###################################################################################################################

time_between_words_stage4 <- read.table("time_between_words_stage4.csv", header = T, sep=",")
time_between_words_stage4 <- time_between_words_stage4[,-1]

healthy_controls_stage4 <- time_between_words_stage4 %>% filter(Group=="healthy_controls") %>% as.data.frame()

time_between_words_stage4_HC_mean <- healthy_controls_stage4 %>% select(-patientId, -Group) %>% summarise_all(funs(mean(.,na.rm=TRUE)))
time_between_words_stage4_HC_mean  # Mean of the time between correct words at each word said (i=1,...,12)

time_between_words_stage4_HC_sd <- healthy_controls_stage4 %>% select(-patientId, -Group) %>% summarise_all(funs(sd(.,na.rm=TRUE)))
time_between_words_stage4_HC_sd    # SD of the time between correct words at each word said (i=1,...,12)


left_CI_stage4 <- right_CI_stage4 <- rep(NA, 15)

for (i in 1:15){
  error <- qnorm(0.95)*time_between_words_stage4_HC_sd[i]/sqrt(nrow(healthy_controls_stage4))
  left_CI_stage4[i] <-  round(unlist(time_between_words_stage4_HC_mean[i] - error))
  right_CI_stage4[i] <- round(unlist(time_between_words_stage4_HC_mean[i] + error))
}

# Checking whether the patient, for each correct response, took more, less, or comparable time to give the answer, compared to healthy controls
###############################################################################################################################################

patients_4 <- time_between_words_stage4 %>% filter(Group!="healthy_controls") %>% as.data.frame()

time_between_words_patients_4 <- patients_4[,-c(1,17)] # Excluding group and patient indication

below_within_above4 <- matrix(NA, nrow=nrow(patients_4), ncol=15)

## faster

each_word_comparison_4 <- matrix(NA, nrow=nrow(time_between_words_patients_4), ncol=15)

for (i in 1:nrow(time_between_words_patients_4)){
  each_word_comparison_4[i,] <- as.matrix(time_between_words_patients_4[i,]) < as.vector(left_CI_stage4) # faster
}


for (i in 1:15){
  for (j in 1:nrow(patients_4)){
    if(!is.na(each_word_comparison_4[j,i])){
      if (each_word_comparison_4[j,i]==TRUE){
        below_within_above4[j,i] <- "faster"
      }
    }
    else NULL
  }
}

## slower

each_word_comparison_4 <- matrix(NA, nrow=nrow(time_between_words_patients_4), ncol=15)

for (i in 1:nrow(time_between_words_patients_4)){
  each_word_comparison_4[i,] <- as.matrix(time_between_words_patients_4[i,]) > as.vector(right_CI_stage4) # slower
}


for (i in 1:15){
  for (j in 1:nrow(patients_4)){
    if(!is.na(each_word_comparison_4[j,i])){
      if (each_word_comparison_4[j,i]==TRUE){
        below_within_above4[j,i] <- "slower"
      }
    }
    else NULL
  }
}

## comparable

each_word_comparison_4 <- matrix(NA, nrow=nrow(time_between_words_patients_4), ncol=15)

for (i in 1:nrow(time_between_words_patients_4)){
  each_word_comparison_4[i,] <- as.matrix(time_between_words_patients_4[i,]) <= as.vector(right_CI_stage4) & as.matrix(time_between_words_patients_4[i,]) >= as.vector(left_CI_stage4) # comparable
}


for (i in 1:15){
  for (j in 1:nrow(patients_4)){
    if(!is.na(each_word_comparison_4[j,i])){
      if (each_word_comparison_4[j,i]==TRUE){
        below_within_above4[j,i] <- "comparable"
      }
    }
    else NULL
  }
}

# Summarising number of responses that were slower, comparable or faster in a single dataframe
##################################################################################################################

sum_comparability_4 <- matrix(NA, nrow=nrow(patients_4), ncol=3)
colnames(sum_comparability_4) <- c("slower", "comparable", "faster")

for ( i in 1:nrow(below_within_above4)){
  sum_comparability_4[i,1] = length(below_within_above4[which(below_within_above4[i,]=="slower")])
  sum_comparability_4[i,2] = length(below_within_above4[which(below_within_above4[i,]=="comparable")])
  sum_comparability_4[i,3] = length(below_within_above4[which(below_within_above4[i,]=="faster")])
}

sum_comparability_4

# Sumamrising results on this trial
###################################################################################################################

RAVLT_totalcorrectwords_4 <- rowSums(!is.na(below_within_above4)) # Total number of correct responses
RAVLT_slower_4 <- sum_comparability_4[,1]
RAVLT_comparable_4 <- sum_comparability_4[,2]
RAVLT_faster_4 <- sum_comparability_4[,3]
RAVLT_score_4 <- round(RAVLT_slower_4/RAVLT_totalcorrectwords_4, 2) # How many times the patient was slower? --> Indication of the patient's impairment
RAVLT_score_percentage_4 <- RAVLT_score_4*100 

# Standard Error of the time between correct responses, both on the total responses and on the first-second half of them:
# Did the performance of the patient change over time? 

RAVLT_SE_total_4 <- round(apply(time_between_words_patients_4, 1, sd, na.rm=TRUE)/sqrt(RAVLT_totalcorrectwords_4), 2)

firstpart4 <- trunc(RAVLT_totalcorrectwords_4/2) # Defining first part 
secondpart4 <- RAVLT_totalcorrectwords_4 - firstpart4 # Defining second part 

RAVLT_SE_1stpart_4 <- RAVLT_SE_2ndpart_4 <- rep(NA, length(firstpart4)) # Computing the respective SEs

for (i in 1:nrow(time_between_words_patients_4)) {
  RAVLT_SE_1stpart_4[i] <-   round(sd(time_between_words_patients_4[i,1:firstpart4[i]], na.rm=TRUE)/sqrt(firstpart4[i]),2)
}

for (i in 1:nrow(time_between_words_patients_4)) {
  RAVLT_SE_2ndpart_4[i] <-  round(sd(time_between_words_patients_4[i,(firstpart4[i]+1):RAVLT_totalcorrectwords_4[i]], na.rm=TRUE)/sqrt(secondpart4[i]),2)
}

# Summarising results of this trial in a dataframe

final_excel <- cbind(final_excel, RAVLT_totalcorrectwords_4,                            # Patient information and number of correct responses on this trial 
                     RAVLT_slower_4, RAVLT_comparable_4, RAVLT_faster_4, RAVLT_score_4, RAVLT_score_percentage_4,  # How many times the patient was slower, comparable to Healthy Controls, or Faster? 
                     RAVLT_SE_total_4, RAVLT_SE_1stpart_4, RAVLT_SE_2ndpart_4)                                     # Defining SE of the mean time between correct words, and comparing it in the first and second half ot the trial.


###################################################################################################################
### Stage 5
###################################################################################################################

time_between_words_stage5 <- read.table("time_between_words_stage5.csv", header = T, sep=",")
time_between_words_stage5 <- time_between_words_stage5[,-1]

healthy_controls_stage5 <- time_between_words_stage5 %>% filter(Group=="healthy_controls") %>% as.data.frame()

time_between_words_stage5_HC_mean <- healthy_controls_stage5 %>% select(-patientId, -Group) %>% summarise_all(funs(mean(.,na.rm=TRUE)))
time_between_words_stage5_HC_mean  # Mean of the time between correct words at each word said (i=1,...,12)

time_between_words_stage5_HC_sd <- healthy_controls_stage5 %>% select(-patientId, -Group) %>% summarise_all(funs(sd(.,na.rm=TRUE)))
time_between_words_stage5_HC_sd    # SD of the time between correct words at each word said (i=1,...,12)


left_CI_stage5 <- right_CI_stage5 <- rep(NA, 15)

for (i in 1:15){
  error <- qnorm(0.95)*time_between_words_stage5_HC_sd[i]/sqrt(nrow(healthy_controls_stage5))
  left_CI_stage5[i] <-  round(unlist(time_between_words_stage5_HC_mean[i] - error))
  right_CI_stage5[i] <- round(unlist(time_between_words_stage5_HC_mean[i] + error))
}

# Checking whether the patient, for each correct response, took more, less, or comparable time to give the answer, compared to healthy controls
###############################################################################################################################################

patients_5 <- time_between_words_stage5 %>% filter(Group!="healthy_controls") %>% as.data.frame()

time_between_words_patients_5 <- patients_5[,-c(1,17)] # Excluding group and patient indication

below_within_above5 <- matrix(NA, nrow=nrow(patients_5), ncol=15)

## faster

each_word_comparison_5 <- matrix(NA, nrow=nrow(time_between_words_patients_5), ncol=15)

for (i in 1:nrow(time_between_words_patients_5)){
  each_word_comparison_5[i,] <- as.matrix(time_between_words_patients_5[i,]) < as.vector(left_CI_stage5) # faster
}


for (i in 1:15){
  for (j in 1:nrow(patients_5)){
    if(!is.na(each_word_comparison_5[j,i])){
      if (each_word_comparison_5[j,i]==TRUE){
        below_within_above5[j,i] <- "faster"
      }
    }
    else NULL
  }
}

## slower

each_word_comparison_5 <- matrix(NA, nrow=nrow(time_between_words_patients_5), ncol=15)

for (i in 1:nrow(time_between_words_patients_5)){
  each_word_comparison_5[i,] <- as.matrix(time_between_words_patients_5[i,]) > as.vector(right_CI_stage5) # slower
}


for (i in 1:15){
  for (j in 1:nrow(patients_5)){
    if(!is.na(each_word_comparison_5[j,i])){
      if (each_word_comparison_5[j,i]==TRUE){
        below_within_above5[j,i] <- "slower"
      }
    }
    else NULL
  }
}

## comparable

each_word_comparison_5 <- matrix(NA, nrow=nrow(time_between_words_patients_5), ncol=15)

for (i in 1:nrow(time_between_words_patients_5)){
  each_word_comparison_5[i,] <- as.matrix(time_between_words_patients_5[i,]) <= as.vector(right_CI_stage5) & as.matrix(time_between_words_patients_5[i,]) >= as.vector(left_CI_stage5) # comparable
}


for (i in 1:15){
  for (j in 1:nrow(patients_5)){
    if(!is.na(each_word_comparison_5[j,i])){
      if (each_word_comparison_5[j,i]==TRUE){
        below_within_above5[j,i] <- "comparable"
      }
    }
    else NULL
  }
}

# Summarising number of responses that were slower, comparable or faster in a single dataframe
##################################################################################################################

sum_comparability_5 <- matrix(NA, nrow=nrow(patients_5), ncol=3)
colnames(sum_comparability_5) <- c("slower", "comparable", "faster")

for ( i in 1:nrow(below_within_above5)){
  sum_comparability_5[i,1] = length(below_within_above5[which(below_within_above5[i,]=="slower")])
  sum_comparability_5[i,2] = length(below_within_above5[which(below_within_above5[i,]=="comparable")])
  sum_comparability_5[i,3] = length(below_within_above5[which(below_within_above5[i,]=="faster")])
}

sum_comparability_5

# Sumamrising results on this trial
###################################################################################################################

RAVLT_totalcorrectwords_5 <- rowSums(!is.na(below_within_above5)) # Total number of correct responses
RAVLT_slower_5 <- sum_comparability_5[,1]
RAVLT_comparable_5 <- sum_comparability_5[,2]
RAVLT_faster_5 <- sum_comparability_5[,3]
RAVLT_score_5 <- round(RAVLT_slower_5/RAVLT_totalcorrectwords_5, 2) # How many times the patient was slower? --> Indication of the patient's impairment
RAVLT_score_percentage_5 <- RAVLT_score_5*100 

# Standard Error of the time between correct responses, both on the total responses and on the first-second half of them:
# Did the performance of the patient change over time? 

RAVLT_SE_total_5 <- round(apply(time_between_words_patients_5, 1, sd, na.rm=TRUE)/sqrt(RAVLT_totalcorrectwords_5), 2)

firstpart5 <- trunc(RAVLT_totalcorrectwords_5/2) # Defining first part 
secondpart5 <- RAVLT_totalcorrectwords_5 - firstpart5 # Defining second part 

RAVLT_SE_1stpart_5 <- RAVLT_SE_2ndpart_5 <- rep(NA, length(firstpart5)) # Computing the respective SEs

for (i in 1:nrow(time_between_words_patients_5)) {
  RAVLT_SE_1stpart_5[i] <-   round(sd(time_between_words_patients_5[i,1:firstpart5[i]], na.rm=TRUE)/sqrt(firstpart5[i]),2)
}

for (i in 1:nrow(time_between_words_patients_5)) {
  RAVLT_SE_2ndpart_5[i] <-  round(sd(time_between_words_patients_5[i,(firstpart5[i]+1):RAVLT_totalcorrectwords_5[i]], na.rm=TRUE)/sqrt(secondpart5[i]),2)
}

# Summarising results of this trial in a dataframe

final_excel <- cbind(final_excel, RAVLT_totalcorrectwords_5,                            # Patient information and number of correct responses on this trial 
                     RAVLT_slower_5, RAVLT_comparable_5, RAVLT_faster_5, RAVLT_score_5, RAVLT_score_percentage_5,  # How many times the patient was slower, comparable to Healthy Controls, or Faster? 
                     RAVLT_SE_total_5, RAVLT_SE_1stpart_5, RAVLT_SE_2ndpart_5)                                     # Defining SE of the mean time between correct words, and comparing it in the first and second half ot the trial.

###################################################################################################################
### Adding total score
###################################################################################################################

RAVLT_total_correct <- as.numeric(RAVLT_totalcorrectwords_1) + as.numeric(RAVLT_totalcorrectwords_2) + as.numeric(RAVLT_totalcorrectwords_3) + as.numeric(RAVLT_totalcorrectwords_4) + as.numeric(RAVLT_totalcorrectwords_5)
RAVLT_total_slower <-  RAVLT_slower_1 + RAVLT_slower_2 + RAVLT_slower_3 + RAVLT_slower_4 + RAVLT_slower_5
RAVLT_total_score <- round(RAVLT_total_slower / RAVLT_total_correct, 2)
RAVLT_total_score_percentage <- RAVLT_total_score*100

final_excel <- cbind(final_excel, RAVLT_total_correct, RAVLT_total_slower, RAVLT_total_score, RAVLT_total_score_percentage)

write.xlsx(final_excel, "final_RAVLT.xlsx", keepNA=T)
