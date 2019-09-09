library(rlist)

######################################
#### Group 1 
######################################

setwd("~/Desktop/Neurophycological Assessment/UMCU stroke study 1")

files_group_1 <- list.files(pattern=".*testType_8_", all.files = TRUE, recursive = TRUE)

files_group_1 <- files_group_1[-48] 

n_patients <- length(files_group_1)
l <- vector("list", n_patients)


for (k in 1:n_patients){#n_patients){
  
  patient_test8 <- rjson::fromJSON(file = files_group_1[k]) # reading file
  
  resultData <- patient_test8$resultData                     # resultData extraction
  patient_test8_resultData <- rjson::fromJSON(resultData)
  
  results_per_stage <-  list.cbind(patient_test8_resultData$result$stages) 
  
  first_stage <- results_per_stage[7,][[1]]
  
  x_2 <- matrix(NA, nrow=100, ncol=5)
  
  for (i in 1:length(first_stage)){
    if (ncol(cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion))==5){
    x_2[i,] <- cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion) # make graph with correct or not = col 
    }
    
  }
  
  x_2 <- x_2[complete.cases(x_2),]
  
  one_patient <- data.frame(t(as.vector(t(x_2))))

    l[[k]] <- cbind(patient_test8$patientId, unlist(results_per_stage[2,1]), one_patient)
    
} # maybe most cited words 

words_first_stage <- do.call(rbind.fill, l)

dim(words_first_stage)

#######################################################################################################################################

######################################
#### Group 2 
######################################

setwd("~/Desktop/Neurophycological Assessment/UMCU stroke study 2")

files_group_2 <- list.files(pattern=".*testType_8_", all.files = TRUE, recursive = TRUE)

files_group_2 <- files_group_2[-20]

n_patients <- length(files_group_2)
l_2 <- vector("list", n_patients)


for (k in 1:n_patients){#n_patients){
  
  patient_test8 <- rjson::fromJSON(file = files_group_2[k]) # reading file
  
  resultData <- patient_test8$resultData                     # resultData extraction
  patient_test8_resultData <- rjson::fromJSON(resultData)
  
  results_per_stage <-  list.cbind(patient_test8_resultData$result$stages) 
  
  first_stage <- results_per_stage[7,][[1]]
  
  x_2 <- matrix(NA, nrow=100, ncol=5)
  
  for (i in 1:length(first_stage)){
    if (ncol(cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion))==5){
      x_2[i,] <- cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion) # make graph with correct or not = col 
    }
    
  }
  
  x_2 <- x_2[complete.cases(x_2),]
  
  one_patient <- data.frame(t(as.vector(t(x_2))))
  
  l_2[[k]] <- cbind(patient_test8$patientId, unlist(results_per_stage[2,1]), one_patient)
  
} # maybe most cited words 

words_first_stage_2 <- do.call(rbind.fill, l_2)

dim(words_first_stage_2)

words_first_stage_2[,1] <- paste(words_first_stage_2[,1], "2", sep="-")

words_first_stage <- rbind.fill(words_first_stage, words_first_stage_2)

dim(words_first_stage) # 177x72, correct 

##########################################################################

setwd("~/Desktop/Neurophycological Assessment/Results")

patiens <- read.csv("patients_group.csv")

colnames(words_first_stage)[[1]] <- "patientId"

words_first_stage <- merge(words_first_stage, patiens, by="patientId")

dim(words_first_stage)

setwd("~/Desktop/Neurophycological Assessment/Results/RAVLT")

write.csv(words_first_stage, file = "words_first_stage.csv")

read.csv("words_first_stage.csv")

##########################################################################

##### Stage 2 


######################################
#### Group 1 
######################################

setwd("~/Desktop/Neurophycological Assessment/UMCU stroke study 1")

files_group_1 <- list.files(pattern=".*testType_8_", all.files = TRUE, recursive = TRUE)

files_group_1 <- files_group_1[-48] 

n_patients <- length(files_group_1)
l <- vector("list", n_patients)


for (k in 1:n_patients){#n_patients){
  
  patient_test8 <- rjson::fromJSON(file = files_group_1[k]) # reading file
  
  resultData <- patient_test8$resultData                     # resultData extraction
  patient_test8_resultData <- rjson::fromJSON(resultData)
  
  results_per_stage <-  list.cbind(patient_test8_resultData$result$stages) 
  
  first_stage <- results_per_stage[7,][[2]]
  
  x_2 <- matrix(NA, nrow=100, ncol=5)
  
  for (i in 1:length(first_stage)){
    if (ncol(cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion))==5){
      x_2[i,] <- cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion) # make graph with correct or not = col 
    }
    
  }
  
  x_2 <- x_2[complete.cases(x_2),]
  
  one_patient <- data.frame(t(as.vector(t(x_2))))
  
  l[[k]] <- cbind(patient_test8$patientId, unlist(results_per_stage[2,2]), one_patient)
  
} # maybe most cited words 

words_first_stage <- do.call(rbind.fill, l)

dim(words_first_stage)

#######################################################################################################################################

######################################
#### Group 2 
######################################

setwd("~/Desktop/Neurophycological Assessment/UMCU stroke study 2")

files_group_2 <- list.files(pattern=".*testType_8_", all.files = TRUE, recursive = TRUE)

files_group_2 <- files_group_2[-20]

n_patients <- length(files_group_2)
l_2 <- vector("list", n_patients)


for (k in 1:n_patients){#n_patients){
  
  patient_test8 <- rjson::fromJSON(file = files_group_2[k]) # reading file
  
  resultData <- patient_test8$resultData                     # resultData extraction
  patient_test8_resultData <- rjson::fromJSON(resultData)
  
  results_per_stage <-  list.cbind(patient_test8_resultData$result$stages) 
  
  first_stage <- results_per_stage[7,][[2]]
  
  x_2 <- matrix(NA, nrow=100, ncol=5)
  
  for (i in 1:length(first_stage)){
    if (ncol(cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion))==5){
      x_2[i,] <- cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion) # make graph with correct or not = col 
    }
    
  }
  
  x_2 <- x_2[complete.cases(x_2),]
  
  one_patient <- data.frame(t(as.vector(t(x_2))))
  
  l_2[[k]] <- cbind(patient_test8$patientId, unlist(results_per_stage[2,2]), one_patient)
  
} # maybe most cited words 

words_first_stage_2 <- do.call(rbind.fill, l_2)

dim(words_first_stage_2)

words_first_stage_2[,1] <- paste(words_first_stage_2[,1], "2", sep="-")

words_first_stage <- rbind.fill(words_first_stage, words_first_stage_2)

dim(words_first_stage) # 177x71, correct 

##########################################################################

setwd("~/Desktop/Neurophycological Assessment/Results")

patiens <- read.csv("patients_group.csv")

colnames(words_first_stage)[[1]] <- "patientId"

words_first_stage <- merge(words_first_stage, patiens, by="patientId")

dim(words_first_stage)

setwd("~/Desktop/Neurophycological Assessment/Results/RAVLT")

write.csv(words_first_stage, file = "words_second_stage.csv")

##########################################################################

### Third

######################################
#### Group 1 
######################################

setwd("~/Desktop/Neurophycological Assessment/UMCU stroke study 1")

files_group_1 <- list.files(pattern=".*testType_8_", all.files = TRUE, recursive = TRUE)

files_group_1 <- files_group_1[-48] 

n_patients <- length(files_group_1)
l <- vector("list", n_patients)


for (k in 1:n_patients){#n_patients){
  
  patient_test8 <- rjson::fromJSON(file = files_group_1[k]) # reading file
  
  resultData <- patient_test8$resultData                     # resultData extraction
  patient_test8_resultData <- rjson::fromJSON(resultData)
  
  results_per_stage <-  list.cbind(patient_test8_resultData$result$stages) 
  
  first_stage <- results_per_stage[7,][[3]]
  
  x_2 <- matrix(NA, nrow=100, ncol=5)
  
  for (i in 1:length(first_stage)){
    if (ncol(cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion))==5){
      x_2[i,] <- cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion) # make graph with correct or not = col 
    }
    
  }
  
  x_2 <- x_2[complete.cases(x_2),]
  
  one_patient <- data.frame(t(as.vector(t(x_2))))
  
  l[[k]] <- cbind(patient_test8$patientId, unlist(results_per_stage[2,3]), one_patient)
  
} # maybe most cited words 

words_first_stage <- do.call(rbind.fill, l)

dim(words_first_stage)

#######################################################################################################################################

######################################
#### Group 2 
######################################

setwd("~/Desktop/Neurophycological Assessment/UMCU stroke study 2")

files_group_2 <- list.files(pattern=".*testType_8_", all.files = TRUE, recursive = TRUE)

files_group_2 <- files_group_2[-20]

n_patients <- length(files_group_2)
l_2 <- vector("list", n_patients)


for (k in 1:n_patients){#n_patients){
  
  patient_test8 <- rjson::fromJSON(file = files_group_2[k]) # reading file
  
  resultData <- patient_test8$resultData                     # resultData extraction
  patient_test8_resultData <- rjson::fromJSON(resultData)
  
  results_per_stage <-  list.cbind(patient_test8_resultData$result$stages) 
  
  first_stage <- results_per_stage[7,][[3]]
  
  x_2 <- matrix(NA, nrow=100, ncol=5)
  
  for (i in 1:length(first_stage)){
    if (ncol(cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion))==5){
      x_2[i,] <- cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion) # make graph with correct or not = col 
    }
    
  }
  
  x_2 <- x_2[complete.cases(x_2),]
  
  one_patient <- data.frame(t(as.vector(t(x_2))))
  
  l_2[[k]] <- cbind(patient_test8$patientId, unlist(results_per_stage[2,3]),  one_patient)
  
} # maybe most cited words 

words_first_stage_2 <- do.call(rbind.fill, l_2)

dim(words_first_stage_2)

words_first_stage_2[,1] <- paste(words_first_stage_2[,1], "2", sep="-")

words_first_stage <- rbind.fill(words_first_stage, words_first_stage_2)

dim(words_first_stage) # 177x71, correct 

##########################################################################

setwd("~/Desktop/Neurophycological Assessment/Results")

patiens <- read.csv("patients_group.csv")

colnames(words_first_stage)[[1]] <- "patientId"

words_first_stage <- merge(words_first_stage, patiens, by="patientId")

dim(words_first_stage)

setwd("~/Desktop/Neurophycological Assessment/Results/RAVLT")

write.csv(words_first_stage, file = "words_third_stage.csv")

##########################################################################

### Fourth stage

######################################
#### Group 1 
######################################

setwd("~/Desktop/Neurophycological Assessment/UMCU stroke study 1")

files_group_1 <- list.files(pattern=".*testType_8_", all.files = TRUE, recursive = TRUE)

files_group_1 <- files_group_1[-48] 

n_patients <- length(files_group_1)
l <- vector("list", n_patients)


for (k in 1:n_patients){#n_patients){
  
  patient_test8 <- rjson::fromJSON(file = files_group_1[k]) # reading file
  
  resultData <- patient_test8$resultData                     # resultData extraction
  patient_test8_resultData <- rjson::fromJSON(resultData)
  
  results_per_stage <-  list.cbind(patient_test8_resultData$result$stages) 
  
  first_stage <- results_per_stage[7,][[4]]
  
  x_2 <- matrix(NA, nrow=100, ncol=5)
  
  for (i in 1:length(first_stage)){
    if (ncol(cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion))==5){
      x_2[i,] <- cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion) # make graph with correct or not = col 
    }
    
  }
  
  x_2 <- x_2[complete.cases(x_2),]
  
  one_patient <- data.frame(t(as.vector(t(x_2))))
  
  l[[k]] <- cbind(patient_test8$patientId, unlist(results_per_stage[2,4]), one_patient)
  
} # maybe most cited words 

words_first_stage <- do.call(rbind.fill, l)

dim(words_first_stage)

#######################################################################################################################################

######################################
#### Group 2 
######################################

setwd("~/Desktop/Neurophycological Assessment/UMCU stroke study 2")

files_group_2 <- list.files(pattern=".*testType_8_", all.files = TRUE, recursive = TRUE)

files_group_2 <- files_group_2[-20]

n_patients <- length(files_group_2)
l_2 <- vector("list", n_patients)


for (k in 1:n_patients){#n_patients){
  
  patient_test8 <- rjson::fromJSON(file = files_group_2[k]) # reading file
  
  resultData <- patient_test8$resultData                     # resultData extraction
  patient_test8_resultData <- rjson::fromJSON(resultData)
  
  results_per_stage <-  list.cbind(patient_test8_resultData$result$stages) 
  
  first_stage <- results_per_stage[7,][[4]]
  
  x_2 <- matrix(NA, nrow=100, ncol=5)
  
  for (i in 1:length(first_stage)){
    if (ncol(cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion))==5){
      x_2[i,] <- cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion) # make graph with correct or not = col 
    }
    
  }
  
  x_2 <- x_2[complete.cases(x_2),]
  
  one_patient <- data.frame(t(as.vector(t(x_2))))
  
  l_2[[k]] <- cbind(patient_test8$patientId, unlist(results_per_stage[2,4]), one_patient)
  
} # maybe most cited words 

words_first_stage_2 <- do.call(rbind.fill, l_2)

dim(words_first_stage_2)

words_first_stage_2[,1] <- paste(words_first_stage_2[,1], "2", sep="-")

words_first_stage <- rbind.fill(words_first_stage, words_first_stage_2)

dim(words_first_stage) # 177x71, correct 

##########################################################################

setwd("~/Desktop/Neurophycological Assessment/Results")

patiens <- read.csv("patients_group.csv")

colnames(words_first_stage)[[1]] <- "patientId"

words_first_stage <- merge(words_first_stage, patiens, by="patientId")

dim(words_first_stage)

setwd("~/Desktop/Neurophycological Assessment/Results/RAVLT")

write.csv(words_first_stage, file = "words_fourth_stage.csv")

##########################################################################
# Fifth
######################################
#### Group 1 
######################################

setwd("~/Desktop/Neurophycological Assessment/UMCU stroke study 1")

files_group_1 <- list.files(pattern=".*testType_8_", all.files = TRUE, recursive = TRUE)

files_group_1 <- files_group_1[-48] 
files_group_1 <- files_group_1[-52] 

n_patients <- length(files_group_1)
l <- vector("list", n_patients)


for (k in 1:n_patients){#n_patients){
  
  patient_test8 <- rjson::fromJSON(file = files_group_1[k]) # reading file
  
  resultData <- patient_test8$resultData                     # resultData extraction
  patient_test8_resultData <- rjson::fromJSON(resultData)
  
  results_per_stage <-  list.cbind(patient_test8_resultData$result$stages) 
  
  first_stage <- results_per_stage[7,][[5]]
  
  x_2 <- matrix(NA, nrow=100, ncol=5)
  
  for (i in 1:length(first_stage)){
    if (ncol(cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion))==5){
      x_2[i,] <- cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion) # make graph with correct or not = col 
    }
    
  }
  
  x_2 <- x_2[complete.cases(x_2),]
  
  one_patient <- data.frame(t(as.vector(t(x_2))))
  
  l[[k]] <- cbind(patient_test8$patientId, unlist(results_per_stage[2,5]), one_patient)
  
} # maybe most cited words 

words_first_stage <- do.call(rbind.fill, l)

dim(words_first_stage)

#######################################################################################################################################

######################################
#### Group 2 
######################################

setwd("~/Desktop/Neurophycological Assessment/UMCU stroke study 2")

files_group_2 <- list.files(pattern=".*testType_8_", all.files = TRUE, recursive = TRUE)

files_group_2 <- files_group_2[-20]

n_patients <- length(files_group_2)
l_2 <- vector("list", n_patients)


for (k in 1:n_patients){#n_patients){
  
  patient_test8 <- rjson::fromJSON(file = files_group_2[k]) # reading file
  
  resultData <- patient_test8$resultData                     # resultData extraction
  patient_test8_resultData <- rjson::fromJSON(resultData)
  
  results_per_stage <-  list.cbind(patient_test8_resultData$result$stages) 
  
  first_stage <- results_per_stage[7,][[5]]
  
  x_2 <- matrix(NA, nrow=100, ncol=5)
  
  for (i in 1:length(first_stage)){
    if (ncol(cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion))==5){
      x_2[i,] <- cbind(first_stage[[i]]$text, first_stage[[i]]$correct, first_stage[[i]]$timeStamp, first_stage[[i]]$repetition, first_stage[[i]]$intrusion) # make graph with correct or not = col 
    }
    
  }
  
  x_2 <- x_2[complete.cases(x_2),]
  
  one_patient <- data.frame(t(as.vector(t(x_2))))
  
  l_2[[k]] <- cbind(patient_test8$patientId, unlist(results_per_stage[2,5]), one_patient)
  
} # maybe most cited words 

words_first_stage_2 <- do.call(rbind.fill, l_2)

dim(words_first_stage_2)

words_first_stage_2[,1] <- paste(words_first_stage_2[,1], "2", sep="-")

words_first_stage <- rbind.fill(words_first_stage, words_first_stage_2)

dim(words_first_stage) # 177x71, correct 

##########################################################################

setwd("~/Desktop/Neurophycological Assessment/Results")

patiens <- read.csv("patients_group.csv")

colnames(words_first_stage)[[1]] <- "patientId"

words_first_stage <- merge(words_first_stage, patiens, by="patientId")

dim(words_first_stage)

setwd("~/Desktop/Neurophycological Assessment/Results/RAVLT")

write.csv(words_first_stage, file = "words_fifth_stage.csv")

##########################################################################