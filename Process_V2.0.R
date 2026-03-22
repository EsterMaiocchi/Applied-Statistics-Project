dev.off()
rm(list = ls()) 
cat("\014")      


#setwd() to the Dati folder
setwd("C:/Users/morra/OneDrive - Politecnico di Milano/Applied statistics - Shared folder/Dati")
getwd()

#Note: the first 3 blocks of code can be executed without thinking the output are just 3 datasets



if (!require(R.matlab)) install.packages("R.matlab", dependencies = TRUE)
if (!require(car)) install.packages("car", dependencies = TRUE)
if (!require(mvtnorm)) install.packages("mvtnorm", dependencies = TRUE)
if (!require(ggbiplot)) install.packages("ggbiplot", dependencies = TRUE)
if (!require(RColorBrewer)) install.packages("RColorBrewer", dependencies = TRUE)
if (!require(MVN)) install.packages("MVN", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(rrcov)) install.packages("rrcov", dependencies = TRUE)
if (!require(vegan)) install.packages("vegan", dependencies = TRUE)

library(ggbiplot)
library(R.matlab)
library(vegan)
library(car)
library(mvtnorm)
library(RColorBrewer)
library(MVN)
library(rrcov)
library(dplyr)
library(openxlsx)
library(plm)
library(Rcpp)
library(MVN)
library(car)
library(whitestrap)
library(tseries)
library(RColorBrewer)
library(StatDA)


# Data Preprocess 1 ---------  


#Function to normalize vector length to 300
#Another idea to fill the gaps could be: add to the last measured value alpha times the sample standard deviation, where alpha is in [-1, 1] *  

normalize_length <- function(vec, target_length = 300) {
  len <- length(vec)
  
  if (len > target_length) {
    return(vec[1:target_length])  # Truncate if too long
  } else if (len < target_length) {
    sd_val <- sd(vec)  # Calculate standard deviation once
      if(sd_val==0){
        sd_val=0.001
      }
    additional_values <- rnorm(target_length - len, mean = tail(vec,1), sd = sd_val)
    vec <- c(vec, additional_values)  # Append new values
  }
  
  return(vec)
}


#Notes:
#To optimize the code it's probably enough to just not create a new matrix each time but write on the prev one
#e.g. instead of S2_A, S3_A... just use one name and keep replacing the content each time

#The key element of this process is the function "normalize_length"
#We are supposed to stay in each point for 5 minutes and we measure each second 
#Hence we are supposed to have 300 measurement for each point for each subject, which is not the case sometimes we have 305, sometimes 295 
#(not too big of a difference)
#If we have more than 300 measurements we can just cut the tail to get back to the "experiment condition" without too much trouble
#If we have less than 300 measurements the idea is to generate new measures until we get to 300, with the idea being "in the last 5 second of 
#the experiment nothing has changed"
#This shouldn't have much impact since we are in the worst case filling just the last 10 seconds (10 is a made up number, not much though)
#Another idea to fill the gaps could be: add to the last measured value alpha times the sample standard deviation, where alpha is in [-1, 1] *




##S1
# !!Special case for S1 since we don't have the full PM25!!
variables <- c("S1CO2", "S1P", "S1PM1", "S1PM10", "S1PM10", "S1RH", "S1T", "S1VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S01/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}



S1_A <- t(rbind("S1","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S1_B <- t(rbind("S1","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S1_C <- t(rbind("S1","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S1_D <- t(rbind("S1","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S1_E <- t(rbind("S1","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S1_F <- t(rbind("S1","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S1_G <- t(rbind("S1","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S1_static <- rbind(S1_A, S1_B, S1_C, S1_D, S1_E, S1_F, S1_G)
colnames(S1_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")  


##S2
variables <- c("S2CO2", "S2P", "S2PM1", "S2PM10", "S2PM25", "S2RH", "S2T", "S2VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S02/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S2_A <- t(rbind("S2","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S2_B <- t(rbind("S2","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S2_C <- t(rbind("S2","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S2_D <- t(rbind("S2","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S2_E <- t(rbind("S2","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S2_F <- t(rbind("S2","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S2_G <- t(rbind("S2","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S2_static <- rbind(S2_A, S2_B, S2_C, S2_D, S2_E, S2_F, S2_G)
colnames(S2_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")   


##S3
variables <- c("S3CO2", "S3P", "S3PM1", "S3PM10", "S3PM25", "S3RH", "S3T", "S3VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S03/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S3_A <- t(rbind("S3","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S3_B <- t(rbind("S3","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S3_C <- t(rbind("S3","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S3_D <- t(rbind("S3","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S3_E <- t(rbind("S3","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S3_F <- t(rbind("S3","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S3_G <- t(rbind("S3","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S3_static <- rbind(S3_A, S3_B, S3_C, S3_D, S3_E, S3_F, S3_G)
colnames(S3_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")  



##S4
variables <- c("S4CO2", "S4P", "S4PM1", "S4PM10", "S4PM25", "S4RH", "S4T", "S4VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S04/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S4_A <- t(rbind("S4","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S4_B <- t(rbind("S4","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S4_C <- t(rbind("S4","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S4_D <- t(rbind("S4","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S4_E <- t(rbind("S4","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S4_F <- t(rbind("S4","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S4_G <- t(rbind("S4","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S4_static <- rbind(S4_A, S4_B, S4_C, S4_D, S4_E, S4_F, S4_G)
colnames(S4_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")    
  


##S5
variables <- c("S5CO2", "S5P", "S5PM1", "S5PM10", "S5PM25", "S5RH", "S5T", "S5VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S05/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S5_A <- t(rbind("S5","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S5_B <- t(rbind("S5","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S5_C <- t(rbind("S5","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S5_D <- t(rbind("S5","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S5_E <- t(rbind("S5","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S5_F <- t(rbind("S5","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S5_G <- t(rbind("S5","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S5_static <- rbind(S5_A, S5_B, S5_C, S5_D, S5_E, S5_F, S5_G)
colnames(S5_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")   



##S6
variables <- c("S6CO2", "S6P", "S6PM1", "S6PM10", "S6PM25", "S6RH", "S6T", "S6VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S06/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S6_A <- t(rbind("S6","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S6_B <- t(rbind("S6","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S6_C <- t(rbind("S6","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S6_D <- t(rbind("S6","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S6_E <- t(rbind("S6","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S6_F <- t(rbind("S6","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S6_G <- t(rbind("S6","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S6_static <- rbind(S6_A, S6_B, S6_C, S6_D, S6_E, S6_F, S6_G)
colnames(S6_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")   



##S7
variables <- c("S7CO2", "S7P", "S7PM1", "S7PM10", "S7PM25", "S7RH", "S7T", "S7VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S07/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S7_A <- t(rbind("S7","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S7_B <- t(rbind("S7","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S7_C <- t(rbind("S7","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S7_D <- t(rbind("S7","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S7_E <- t(rbind("S7","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S7_F <- t(rbind("S7","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S7_G <- t(rbind("S7","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S7_static <- rbind(S7_A, S7_B, S7_C, S7_D, S7_E, S7_F, S7_G)
colnames(S7_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")  




##S8
variables <- c("S8CO2", "S8P", "S8PM1", "S8PM10", "S8PM25", "S8RH", "S8T", "S8VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S08/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S8_A <- t(rbind("S8","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S8_B <- t(rbind("S8","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S8_C <- t(rbind("S8","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S8_D <- t(rbind("S8","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S8_E <- t(rbind("S8","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S8_F <- t(rbind("S8","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S8_G <- t(rbind("S8","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S8_static <- rbind(S8_A, S8_B, S8_C, S8_D, S8_E, S8_F, S8_G)
colnames(S8_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")



##S9
variables <- c("S9CO2", "S9P", "S9PM1", "S9PM10", "S9PM25", "S9RH", "S9T", "S9VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S09/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S9_A <- t(rbind("S9","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S9_B <- t(rbind("S9","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S9_C <- t(rbind("S9","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S9_D <- t(rbind("S9","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S9_E <- t(rbind("S9","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S9_F <- t(rbind("S9","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S9_G <- t(rbind("S9","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S9_static <- rbind(S9_A, S9_B, S9_C, S9_D, S9_E, S9_F, S9_G)
colnames(S9_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")



##S10
variables <- c("S10CO2", "S10P", "S10PM1", "S10PM10", "S10PM25", "S10RH", "S10T", "S10VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S10/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S10_A <- t(rbind("S10","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S10_B <- t(rbind("S10","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S10_C <- t(rbind("S10","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S10_D <- t(rbind("S10","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S10_E <- t(rbind("S10","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S10_F <- t(rbind("S10","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S10_G <- t(rbind("S10","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S10_static <- rbind(S10_A, S10_B, S10_C, S10_D, S10_E, S10_F, S10_G)
colnames(S10_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")


##S11
variables <- c("S11CO2", "S11P", "S11PM1", "S11PM10", "S11PM25", "S11RH", "S11T", "S11VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S11/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S11_A <- t(rbind("S11","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S11_B <- t(rbind("S11","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S11_C <- t(rbind("S11","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S11_D <- t(rbind("S11","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S11_E <- t(rbind("S11","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S11_F <- t(rbind("S11","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S11_G <- t(rbind("S11","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S11_static <- rbind(S11_A, S11_B, S11_C, S11_D, S11_E, S11_F, S11_G)
colnames(S11_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")



##S12
variables <- c("S12CO2", "S12P", "S12PM1", "S12PM10", "S12PM25", "S12RH", "S12T", "S12VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S12/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S12_A <- t(rbind("S12","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S12_B <- t(rbind("S12","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S12_C <- t(rbind("S12","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S12_D <- t(rbind("S12","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S12_E <- t(rbind("S12","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S12_F <- t(rbind("S12","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S12_G <- t(rbind("S12","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S12_static <- rbind(S12_A, S12_B, S12_C, S12_D, S12_E, S12_F, S12_G)
colnames(S12_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")



##S13
variables <- c("S13CO2", "S13P", "S13PM1", "S13PM10", "S13PM25", "S13RH", "S13T", "S13VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S13/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S13_A <- t(rbind("S13","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S13_B <- t(rbind("S13","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S13_C <- t(rbind("S13","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S13_D <- t(rbind("S13","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S13_E <- t(rbind("S13","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S13_F <- t(rbind("S13","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S13_G <- t(rbind("S13","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S13_static <- rbind(S13_A, S13_B, S13_C, S13_D, S13_E, S13_F, S13_G)
colnames(S13_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")


##S14
variables <- c("S14CO2", "S14P", "S14PM1", "S14PM10", "S14PM25", "S14RH", "S14T", "S14VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S14/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S14_A <- t(rbind("S14","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S14_B <- t(rbind("S14","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S14_C <- t(rbind("S14","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S14_D <- t(rbind("S14","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S14_E <- t(rbind("S14","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S14_F <- t(rbind("S14","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S14_G <- t(rbind("S14","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S14_static <- rbind(S14_A, S14_B, S14_C, S14_D, S14_E, S14_F, S14_G)
colnames(S14_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")



##S15
variables <- c("S15CO2", "S15P", "S15PM1", "S15PM10", "S15PM25", "S15RH", "S15T", "S15VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S15/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S15_A <- t(rbind("S15","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S15_B <- t(rbind("S15","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S15_C <- t(rbind("S15","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S15_D <- t(rbind("S15","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S15_E <- t(rbind("S15","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S15_F <- t(rbind("S15","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S15_G <- t(rbind("S15","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S15_static <- rbind(S15_A, S15_B, S15_C, S15_D, S15_E, S15_F, S15_G)
colnames(S15_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")



##S16
variables <- c("S16CO2", "S16P", "S16PM1", "S16PM10", "S16PM25", "S16RH", "S16T", "S16VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S16/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S16_A <- t(rbind("S16","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S16_B <- t(rbind("S16","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S16_C <- t(rbind("S16","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S16_D <- t(rbind("S16","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S16_E <- t(rbind("S16","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S16_F <- t(rbind("S16","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S16_G <- t(rbind("S16","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S16_static <- rbind(S16_A, S16_B, S16_C, S16_D, S16_E, S16_F, S16_G)
colnames(S16_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")



##S17
variables <- c("S17CO2", "S17P", "S17PM1", "S17PM10", "S17PM25", "S17RH", "S17T", "S17VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S17/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S17_A <- t(rbind("S17","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S17_B <- t(rbind("S17","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S17_C <- t(rbind("S17","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S17_D <- t(rbind("S17","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S17_E <- t(rbind("S17","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S17_F <- t(rbind("S17","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S17_G <- t(rbind("S17","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S17_static <- rbind(S17_A, S17_B, S17_C, S17_D, S17_E, S17_F, S17_G)
colnames(S17_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")



##S18
variables <- c("S18CO2", "S18P", "S18PM1", "S18PM10", "S18PM25", "S18RH", "S18T", "S18VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S18/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S18_A <- t(rbind("S18","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S18_B <- t(rbind("S18","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S18_C <- t(rbind("S18","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S18_D <- t(rbind("S18","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S18_E <- t(rbind("S18","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S18_F <- t(rbind("S18","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S18_G <- t(rbind("S18","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S18_static <- rbind(S18_A, S18_B, S18_C, S18_D, S18_E, S18_F, S18_G)
colnames(S18_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")


##S19
variables <- c("S19CO2", "S19P", "S19PM1", "S19PM10", "S19PM25", "S19RH", "S19T", "S19VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S19/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S19_A <- t(rbind("S19","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S19_B <- t(rbind("S19","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S19_C <- t(rbind("S19","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S19_D <- t(rbind("S19","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S19_E <- t(rbind("S19","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S19_F <- t(rbind("S19","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S19_G <- t(rbind("S19","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S19_static <- rbind(S19_A, S19_B, S19_C, S19_D, S19_E, S19_F, S19_G)
colnames(S19_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")



##S20
variables <- c("S20CO2", "S20P", "S20PM1", "S20PM10", "S20PM25", "S20RH", "S20T", "S20VOC")
data_list <- lapply(variables, function(var) readMat(paste0("S20/", var, ".mat")))

for (i in seq_along(variables)) {
  data_list[[i]]$A <- normalize_length(data_list[[i]]$A)
  data_list[[i]]$B <- normalize_length(data_list[[i]]$B)
  data_list[[i]]$C <- normalize_length(data_list[[i]]$C)
  data_list[[i]]$D <- normalize_length(data_list[[i]]$D)
  data_list[[i]]$E <- normalize_length(data_list[[i]]$E)
  data_list[[i]]$F <- normalize_length(data_list[[i]]$F)
  data_list[[i]]$G <- normalize_length(data_list[[i]]$G)
}

S20_A <- t(rbind("S20","A", data_list[[1]]$A, data_list[[2]]$A, data_list[[3]]$A, data_list[[4]]$A, data_list[[5]]$A, data_list[[6]]$A, data_list[[7]]$A, data_list[[8]]$A))
S20_B <- t(rbind("S20","B", data_list[[1]]$B, data_list[[2]]$B, data_list[[3]]$B, data_list[[4]]$B, data_list[[5]]$B, data_list[[6]]$B, data_list[[7]]$B, data_list[[8]]$B))
S20_C <- t(rbind("S20","C", data_list[[1]]$C, data_list[[2]]$C, data_list[[3]]$C, data_list[[4]]$C, data_list[[5]]$C, data_list[[6]]$C, data_list[[7]]$C, data_list[[8]]$C))
S20_D <- t(rbind("S20","D", data_list[[1]]$D, data_list[[2]]$D, data_list[[3]]$D, data_list[[4]]$D, data_list[[5]]$D, data_list[[6]]$D, data_list[[7]]$D, data_list[[8]]$D))
S20_E <- t(rbind("S20","E", data_list[[1]]$E, data_list[[2]]$E, data_list[[3]]$E, data_list[[4]]$E, data_list[[5]]$E, data_list[[6]]$E, data_list[[7]]$E, data_list[[8]]$E))
S20_F <- t(rbind("S20","F", data_list[[1]]$F, data_list[[2]]$F, data_list[[3]]$F, data_list[[4]]$F, data_list[[5]]$F, data_list[[6]]$F, data_list[[7]]$F, data_list[[8]]$F))
S20_G <- t(rbind("S20","G", data_list[[1]]$G, data_list[[2]]$G, data_list[[3]]$G, data_list[[4]]$G, data_list[[5]]$G, data_list[[6]]$G, data_list[[7]]$G, data_list[[8]]$G))
S20_static <- rbind(S20_A, S20_B, S20_C, S20_D, S20_E, S20_F, S20_G)
colnames(S20_static) <- c("subject_ID","place_ID","C02", "P", "PM1", "PM10", "PM25", "RH", "T", "VOC")




# Dataset Creation --------------------
dataset_static <- rbind(S1_static,S2_static,S3_static,S4_static,S5_static,
                        S6_static,S7_static,S8_static,S9_static,S10_static,
                        S11_static,S12_static,S13_static,S14_static,S15_static,
                        S16_static,S17_static,S18_static,S19_static,S20_static)
write.table(dataset_static, file = "dataset_static.txt")
rm(list = ls())
data = read.table("dataset_static.txt")
data$MorA = ifelse(data$subject_ID %in% c("S1", "S2", "S4", "S8", "S10", "S12", "S14", "S17"), "M","A")
data$Outdoor = ifelse(data$place_ID == "A", "F","T")

data$subject_ID=as.factor(data$subject_ID)
data$place_ID=as.factor(data$place_ID)
data$MorA=as.factor(data$MorA)
data$Outdoor=as.factor(data$Outdoor)
str(data)

# Dataset Preprocess 2 -----
#Idea: the rows are not independent since the measurements are time-dependent
#Let's try a block bootstrapping

interation_subject <- unique(data$subject_ID)
interation_place <- unique(data$place_ID)

set.seed(762)

#Rows shuffling (fixed place&subject)
data_shuffled <- data
for (i in interation_subject) {
  for (j in interation_place) {
    
    condition <- data_shuffled$subject_ID == i & data_shuffled$place_ID == j
    
    if (sum(condition) > 0) {
      n <- sum(condition)  
      
      sampled_rows <- data_shuffled[condition, ][sample(1:n, size = n, replace = TRUE), ]
      
      data_shuffled[condition, ] <- sampled_rows
      
      sampled_rows = NULL
    }
    
  }
}




# Undersampling (fixed place&subject)
n_sample=30

data_undersampled <- data.frame()
for (i in interation_subject) {
  
  for (j in interation_place) {
    
    condition <- data$subject_ID == i & data$place_ID == j
    
    if (sum(condition) > 0) {
      group_rows <- data[condition, ]
      
      sampled_rows <- group_rows[sample(1:300, size = n_sample, replace = FALSE), ]
      
      data_undersampled <- rbind(data_undersampled, sampled_rows)
      
      group_rows <- NULL
      sampled_rows <- NULL
    }
  }
}



# Remove S8: see Data Acception for further details
data_shuffled <- subset(data_shuffled, subject_ID != "S8")

data_undersampled <- subset(data_undersampled, subject_ID != "S8")

# Dataset Outdoor creation ----
data_undersampled_outdoor <- subset(data_undersampled, data_undersampled$Outdoor == "T")

data_undersampled_outdoor_scaled <- data_undersampled_outdoor
data_undersampled_outdoor_scaled$C02 <- scale(data_undersampled_outdoor$C02)
data_undersampled_outdoor_scaled$P <- scale(data_undersampled_outdoor$P)
data_undersampled_outdoor_scaled$PM1 <- scale(data_undersampled_outdoor$PM1)
data_undersampled_outdoor_scaled$PM10 <- scale(data_undersampled_outdoor$PM10)
data_undersampled_outdoor_scaled$PM25 <- scale(data_undersampled_outdoor$PM25)
data_undersampled_outdoor_scaled$RH <- scale(data_undersampled_outdoor$RH)
data_undersampled_outdoor_scaled$T <- scale(data_undersampled_outdoor$T)
data_undersampled_outdoor_scaled$VOC <- scale(data_undersampled_outdoor$VOC)

rm(list = ls()[!grepl("data", ls())])

#Data Acception ----

#Idea: define measure acceptance intervals for each variable
#If the measure is messed up (e.g. CO2 at 3k ppm) the whole row can be dismissed as "measure error"
#Note that this has nothing to do with outlier detection, we just want to keep the right measure
#For the first step I will define super large acceptance intervals, and see how many rows falls out 


#coppia mattina/pomeriggio: 2-3, 4-5, 8-9, 10-11, 12-13, 17-18

# PM25  
pm25_max=80
pm25_min=1
filtered_data <- subset(data, data$PM25 > pm25_max | data$PM25 < pm25_min)
#either the S8 was very lucky with air pollution or there is something wrong with his measures

# Pressure*
p_max = 104000
p_min = 96800
filtered_data <- subset(data, data$P > p_max | data$P < p_min)

CO2_max=5000
CO2_min=400
filtered_data <- subset(data, data$C02 > CO2_max | data$C02 < CO2_min)
# We are pretty happy in this case since all the measure with high CO2 are in point A

VOC_max=5000
VOC_min=0
filtered_data <- subset(data, data$VOC > VOC_max | data$VOC < VOC_min)


#Exploratory analysis ---- 

##Box plot agenti atmosferici senza orario----
data_sampled <- subset(data_sampled, subject_ID != "S8")
data_sampled = droplevels(data_sampled)
dati.atmosfera = data_sampled[, c("subject_ID", "place_ID", "P", "RH", "T", "MorA")]
colnames(dati.atmosfera) <- c("subject_ID", "place_ID", "P", "RH", "T", "orario")

x11()
par(mfrow = c(1,3))

colors = brewer.pal(7, "Set3")
boxplot(P ~ place_ID, data = dati.atmosfera, main = "Pressure by place", xlab = "Place", ylab = "Pressure [Pa]", col = colors)
boxplot(RH ~ place_ID, data = dati.atmosfera, main = "Humidity by place", xlab = "Place", ylab = "Humidity [%]", col = colors)
boxplot(T ~ place_ID, data = dati.atmosfera, main = "Temperature by place", xlab = "Place", ylab = "Temperature [°C]", col = colors)
#la pressione ha variabilità molto alta nel laboratorio (ipotesi: il dispositivo ha bisogno di un momento iniziale per calibrarsi)
#la temperatura è più alta nel laboratorio rispetto all'esterno (esperimento condotto a novembre, riscaldamento acceso)


### Boxplot agenti atmosferici con orario 
x11()
par(mfrow = c(1,3))
boxplot(P ~ interaction(orario, place_ID),  data = dati.atmosfera, main = 'Pressure by place and time', 
        col = c("lightpink", "lightblue"), xlab = "Place", ylab = "Pressure [Pa]")
legend("topright", legend = c("Morning", "Afternoon"), fill = c("lightpink", "lightblue"))
boxplot(RH ~ interaction(orario, place_ID),  data = dati.atmosfera, main = 'Humidity by place and time', 
        col = c("lightpink", "lightblue"), xlab = "Place", ylab = "Humidity [%]")
legend("topright", legend = c("Morning", "Afternoon"), fill = c("lightpink", "lightblue"))
boxplot(T ~ interaction(orario, place_ID),  data = dati.atmosfera, main = 'Temperature by place and time', 
        col = c("lightpink", "lightblue"), xlab = "Place", ylab = "Temperature [°C]")
legend("topright", legend = c("Morning", "Afternoon"), fill = c("lightpink", "lightblue"))
#pressione in media lievemente più alta nel pomeriggio in tutti i luoghi
#umidità in media notevolmente più alta la mattina in tutti i luoghi (tranne nel laboratorio in cui la differenza è minima)
#temperature in media più alte la mattina in tutti i luoghi (tranne nel luogo G in cui la differenza è impercettibile),
#la variabilità nelle misurazioni della temperatura è più grande nel pomeriggio per tutti i luoghi

###Temperatura 
Temp.A = dati.atmosfera[which(dati.atmosfera$place_ID == "A"), c("subject_ID", "T")]
summary = summary(Temp.A$T)
summary

Temp.esterna = dati.atmosfera[which(dati.atmosfera$place_ID != "A"), c("subject_ID", "T")]
summary = summary(Temp.esterna$T)
summary
#in generale le temperature rilevate sono eccessivamente alte. 
#a fine novembre in un ambiente chiuso solitamente ci sono circa 22°C, noi abbiamo una media di 27.07°C
#all'esterno le temperature (tra le 10 e le 16) sono intorno ai 10°C, noi abbiamo una media di 21.373°C

### Punto A : pressione nel laboratorio
Pressione.A = dati.atmosfera[which(dati.atmosfera$place_ID == "A"), c("subject_ID", "P")]
summary = summary(Pressione.A$P)
summary

# Disegno uno stripchart
# Quali soggetti hanno misurato una pressione inferiore al primo o superiore al terzo quartile

Q3 = quantile(Pressione.A$P, 0.75)
Q1 = quantile(Pressione.A$P, 0.25)
IQR_value = IQR(Pressione.A$P)

lim_sup <- Q3 + 1.5 * IQR_value # soglia oltre il quale ho outlier
lim_inf <- Q1 - 1.5 * IQR_value
P_riferimento <- 101300 #la pressione di riferimento in Pa, misurata a livello del mare

x11()
stripchart(P ~ subject_ID, data = Pressione.A, 
           vertical = TRUE, pch = 19, col = "blue", 
           xlab = "Subject", ylab = "Pressure [Pa]", 
           main = "Pressure by subject at place A")
abline(h = lim_sup, col = 'red')
abline(h = lim_inf, col = 'red')
abline(h = P_riferimento, col = "purple")
abline(h = Q1, col = 'lightgreen')
abline(h = Q3, col = 'lightgreen')
legend("topright", legend = c("Outlier threshold","Sea level pressure", "Q1&Q3"), fill = c("red", "purple", "lightgreen"), text.font = 2)

#problemi con tutti i soggetti -> c'è un "rumore" esterno che disturba le misurazioni, avvalorata l'ipotesi della calibrazione dello strumento.





##Box plot CO2 e VOC----

# Estrazione CO2 e VOC
dati.CO2.VOC = data_sampled[, c("subject_ID", "place_ID", "C02", "VOC", "MorA")]
colnames(dati.CO2.VOC) <- c("subject_ID", "place_ID", "C02", "VOC", "orario")


colors = brewer.pal(7, "Set3")
x11()
par(mfrow = c(1,2))
boxplot(C02 ~ place_ID, data = dati.CO2.VOC, main = "CO2 concentration by place", xlab = "Place", ylab = "Concentration [ppm]", col = colors)
boxplot(VOC ~ place_ID, data = dati.CO2.VOC, main = "VOC concentration by place", xlab = "Place", ylab = "Concentration [ppb]", col = colors)
# CO2 è molto più alta, variabile nel laboratorio (al chiuso) rispetto all'esterno e contiene anche outlier. Probabilmente dipende da
# il numero di persone presenti in quel momento, dal ricircolo dell'aria o da una fase di assestamento del dispositivo
# VOC nel punto G contiene picchi anomali

## Boxplot concentrazioni CO2 e VOC con orario
x11()
par(mfrow = c(1,2))
boxplot(C02 ~ interaction(orario, place_ID),  data = dati.CO2.VOC, main = 'CO2 concentration by place and time', 
        col = c("lightpink", "lightblue"), xlab = "Place", ylab = "Concentration [ppm]")
legend("topright", legend = c("Morning", "Afternoon"), fill = c("lightpink", "lightblue"))
boxplot(VOC ~ interaction(orario, place_ID),  data = dati.CO2.VOC, main = 'VOC concentration by place and time', 
        col = c("lightpink", "lightblue"), xlab = "Place", ylab = "Concentration [ppb]")
legend("topleft", legend = c("Morning", "Afternoon"), fill = c("lightpink", "lightblue"))


### Punto A : Laboratorio e CO2
# INFO: Il valore massimo raccomandato per gli interni è generalmente di 1.000/1.200 ppm e il valore limite per gli uffici o scuole è di 1.500 ppm.
CO2_A = dati.CO2.VOC[which(dati.CO2.VOC$place_ID == "A"), c("subject_ID", "C02")]
summary = summary(CO2_A$C02) # Il quartile Q3 è 1441
summary

# Disegno uno stripchart
# Per quali soggetti posso identificare degli outlier?
Q3 = quantile(CO2_A$C02, 0.75)
Q1 = quantile(CO2_A$C02, 0.25)
IQR_value = IQR(CO2_A$C02)

lim_sup <- Q3 + 1.5 * IQR_value # soglia oltre il quale ho outlier

x11()
stripchart(C02 ~ subject_ID, data = CO2_A, 
           vertical = TRUE, pch = 19, col = "blue", 
           xlab = "Subject", ylab = "Concentration [ppm]", 
           main = "CO2 concentration by subject at place A")
abline(h = lim_sup, col = 'red', lwd = 2)
abline(h = Q1, col = 'lightgreen', lwd = 2)
abline(h = Q3, col = 'lightgreen', lwd = 2)
legend("topright", legend = c("Outlier threshold", "Q1&Q3"), fill = c("red", "lightgreen"), text.font = 2)

# SOGGETTI CON CONCENTRAZIONE OLTRE IL LIMITE: S10, S14, S17, S18. 
# IN PARTICOLARE LA VARIANZA DI S14 e S18 E' MOLTO ALTA

### Punto G : low traffic street e VOC
# INFO: ogni composto volatile può avere un suo limite massimo consentito. Tale limite può variare da stato a stato
VOC_G = dati.CO2.VOC[which(dati.CO2.VOC$place_ID == "G"), c("subject_ID", "VOC")]
summary = summary(VOC_G$VOC)
summary

# Disegno uno stripchart
# Quali soggetti hanno misurato una concentrazione superiore al terzo quartile?
Q3 = quantile(VOC_G$VOC, 0.75)
Q1 = quantile(VOC_G$VOC, 0.25)
IQR_value = IQR(VOC_G$VOC)

lim_sup <- Q3 + 1.5 * IQR_value # soglia oltre il quale ho outlier

x11()
stripchart(VOC ~ subject_ID, data = VOC_G, 
           vertical = TRUE, pch = 19, col = "blue", 
           xlab = "Subject", ylab = "Concentration [ppb]", 
           main = "VOC concentration by subject at place G")
abline(h = lim_sup, col = 'red', lwd = 2)
abline(h = Q1, col = 'lightgreen', lwd = 2)
abline(h = Q3, col = 'lightgreen', lwd = 2)
legend("topright", legend = c("Outlier threshold", "Q1&Q3"), fill = c("red", "lightgreen"), text.font = 2)

# PROBLEMI PRINCIPALI CON IL SOGGETTO S17 E IN MISURA MINORE CON S8 e S3

##Box plot ----

# Estrazione PM1, PM2.5, PM10
dati.PM = data_sampled[, c("subject_ID", "place_ID", "PM1", "PM10", "PM25", "MorA")]
colnames(dati.PM) <- c("subject_ID", "place_ID", "PM1", "PM10", "PM25", "orario")

## Boxplot dei vari PM senza orario 
x11()
par(mfrow = c(1,3))
colors = brewer.pal(7, "Set3") 
boxplot(PM1 ~ place_ID, data = dati.PM, main = "PM1 by place", xlab = "Place", ylab = "Concentration [µg/m³]", col = colors)
boxplot(PM10 ~ place_ID, data = dati.PM, main = "PM10 by place", xlab = "Place", ylab = "Concentration [µg/m³]", col = colors)
boxplot(PM25 ~ place_ID, data = dati.PM, main = "PM25 by place", xlab = "Place", ylab = "Concentration [µg/m³]", col = colors)

#Ovviamente tutti i PM hanno il valore più basso in A1 (laboratorio).
#Rispetto agli altri luoghi, eccetto A1, i valori di PM sono più bassi in C1 (parco). 
#i valori più alti dei PM sono in G1 (low traffic street), anche se vi è un maggiore distacco tra G1 e gli altri luoghi per PM25
#mentre per PM1 e PM10 B1 si avvicina di molto a G1 
#i valori di PM1 di B1 sono un pochino più alti rispetto a quelli di PM10 e PM25: ha senso visto che PM1 deriva dagli altri due
#e in quella zona sono presenti le particelle più pericolose

#Come riportato nei commenti all'inizio, il massimo per PM10 è 45 µg/m³: IQR non supera di poco tale limite ma vi sono degli otulier che lo fanno
#il massimo per PM2.5 è 15 µg/m³: non viene superato solo in A1, per il resto è nettamente superato

## Boxplot agenti atmosferici con orario
x11()
par(mfrow = c(1,3))
boxplot(PM1 ~ interaction(orario, place_ID),  data = dati.PM, main = 'PM1 by place and time', 
        col = c("lightpink", "lightblue"), xlab = "Place", ylab = "Concentration [µg/m³]")
legend("topright", legend = c("Morning", "Afternoon"), fill = c("lightpink", "lightblue"))
boxplot(PM10 ~ interaction(orario, place_ID),  data = dati.PM, main = 'PM10 by place and time', 
        col = c("lightpink", "lightblue"), xlab = "Place", ylab = "Concentration [µg/m³]")
legend("topright", legend = c("Morning", "Afternoon"), fill = c("lightpink", "lightblue"))
boxplot(PM25 ~ interaction(orario, place_ID),  data = dati.PM, main = 'PM25 by place and time', 
        col = c("lightpink", "lightblue"), xlab = "Place", ylab = "Concentration [µg/m³]")
legend("topright", legend = c("Morning", "Afternoon"), fill = c("lightpink", "lightblue"))

#i minimi sono raggiunti nelle prime tappe, oltre a A1 dove ovvio che siano più bassi i valori
#A1:valori bassi in generale, anche se la mattina ha la media più bassa (ci puo' stare, in particolare se la stanza non è molto arieggiata)
#B1: mattino IQR più largo ma ha media molto più bassa al mattino (quantità di macchine che sono passate fino al mattino inferiore 
#rispetto a quelle fino al pomeriggio)
#C1: come B1 ma media del pomeriggio è ovviamente inferiore rispetto al pomeriggio di B1
#In generale valori più alti per Pm1 e Pm25 rispetto a PM10 (IQR più larghi)

###PM1 (il più pericoloso) e PM25 di cui sfora sempre il limite
#PM1

#B1(gas station)
PM1.B = dati.PM[which(dati.PM$place_ID == "B"), c("subject_ID", "PM1")]
summary = summary(PM1.B$PM1)
summary

#C1 dove ho max 
PM1.C = dati.PM[which(dati.PM$place_ID == "C"), c("subject_ID", "PM1")]
summary = summary(PM1.C$PM1)
summary

#PM25
#B1(gas station)
PM25.B = dati.PM[which(dati.PM$place_ID == "B"), c("subject_ID", "PM25")]
summary = summary(PM25.B$PM25)
summary

#C1 
PM25.C = dati.PM[which(dati.PM$place_ID == "C"), c("subject_ID", "PM25")]
summary = summary(PM25.C$PM25)
summary

#a volte ho minimi con valori negativi: leggendo su internet, alcuni sensori hanno una deriva o un offset che può portare a valori negativi.
#Se il sensore è molto sensibile e la concentrazione è vicina a zero, può generare letture sotto lo zero a causa di rumore di fondo.

# Disegno uno stripchart
# Quali soggetti hanno misurato un PM1 inferiore al primo o superiore al terzo quartile?

Q3 = quantile(PM1.B$P, 0.75)
Q1 = quantile(PM1.B$P, 0.25)
IQR_value = IQR(PM1.B$PM1)

lim_sup <- Q3 + 1.5 * IQR_value # soglia oltre il quale ho outlier
lim_inf <- Q1 - 1.5 * IQR_value
PM25_riferimento <- 15 #massimo per legge

x11()
stripchart(PM25 ~ subject_ID, data = PM25.B, 
           vertical = TRUE, pch = 19, col = "blue", 
           xlab = "Subject", ylab = "Concentration [µg/m³]", 
           main = "PM25 by subject at place B")
abline(h = lim_sup, col = 'red')
abline(h = lim_inf, col = 'red')
abline(h = PM25_riferimento, col = "purple")
abline(h = Q1, col = 'lightgreen')
abline(h = Q3, col = 'lightgreen')
legend("topleft", legend = c("Outlier threshold", "Legal limit", "Q1&Q3"), fill = c("red", "purple", "lightgreen"), text.font = 2)

#PCA ----

data_pca <- data_undersampled_outdoor_scaled[,-c(4,5,6)]

pc.pollutant_outdoor <- princomp(data_pca[, 3:6], scores = T)
pc.pollutant_outdoor
summary(pc.pollutant_outdoor)

##Graph to choose how many PC to keep 
#In this case we are choosing either 2 or 3
layout(matrix(c(2, 3, 1, 3), 2, byrow = TRUE))
# Variance explained by the principal components
plot(pc.pollutant_outdoor, las = 2, main = 'Principal Components', ylim = c(0, 7))
abline(h = 1, col = 'blue')

# Variances of original variables
barplot(sapply(data_pca[, 3:7], sd)^2, las = 2, main = 'Original Variables', ylim = c(0, 7),
        ylab = 'Variances')
# Plot contribution to the total variance by number of components
plot(cumsum(pc.pollutant_outdoor$sde^2) / sum(pc.pollutant_outdoor$sde^2), type = 'b', axes = FALSE, 
     xlab = 'Number of components', ylab = 'Contribution to the total variance', ylim = c(0, 1))
abline(h = 1, col = 'blue')
abline(h = 0.8, lty = 2, col = 'blue')
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(data_pca[, 3:7]), labels = 1:ncol(data_pca[, 3:7]), las = 2)


##Graph to interpret PCi
#PC1 --> PM
#PC2 --> Just a linear combination of the others

load.pollutant_outdoor <- pc.pollutant_outdoor$loadings
load.pollutant_outdoor

par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(load.pollutant_outdoor[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))



scores.pollutant_outdoor <- pc.pollutant_outdoor$scores


# Scatter plot of the scores
par(mfrow = c(1,1))
plot(scores.pollutant_outdoor[, 1:2])
abline(h=0, v=0, lty=2, col='grey')


colors = c(brewer.pal(11, "Set3"), brewer.pal(9, "Set1"))



x11()
ggbiplot(pc.pollutant_outdoor, 
         obs.scale = 1, var.scale = 1, 
         groups =data_pca$place_ID ,   
         ellipse = FALSE,        
         circle = FALSE) +       
  theme_minimal() +             
  scale_color_manual(values = colors) +  
  ggtitle("PCA Biplot of Outdoor Pollutants") +
  theme(legend.position = "bottom")
# Outlier Detection (column by column) ----
# We detect the outliers column by column. We also distinguish the different places, otherwise we
# would mix up indoors and outdoors data.
# By definition an outlier is outside the range [Q1-1.5*IQR, Q3+1.5*IQR]

# Save place_ID and pollutants strings to perform the nested loops
place_ID = levels(data_sampled$place_ID)
pollutants = colnames(data_sampled)[3:10]

for (j in pollutants) {
  for (i in place_ID) {
    print(paste("Estraggo i dati relativi a", j, "nel luogo", i, "..."))
    # Extract data related to place i and pollutant j
    sub_out <- subset(data_sampled, place_ID == i, select = c(j)) # It returns a dataframe. 'Subset' preserves the colnames
    
    # Compute the thresholds
    Q1 <- quantile(sub_out, 0.25, na.rm = TRUE)
    Q3 <- quantile(sub_out, 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    out_sup <- Q3 + 1.5 * IQR_val
    out_inf <- Q1 - 1.5 * IQR_val
    print(paste("Range accettazione: [", round(out_inf, 2), ";", round(out_sup,2), "]"))
    
    # Identify non-outliers
    non_outliers <- sub_out >= out_inf & sub_out <= out_sup  # Returns a vector with T/F values
    mean_wo_out <- mean(sub_out[non_outliers, j])  # Compute the mean of values marked as TRUE
    
    print(paste("Media con outlier: ", round(mean(sub_out[, j]), 2)))
    print(paste("Mean_wo_out: ", mean_wo_out))
    
    # Replace outliers with mean
    outlier_rows <- which(data_sampled$place_ID == i & 
                            (data_sampled[, j] > out_sup | data_sampled[, j] < out_inf))
    data_sampled[outlier_rows, j] <- mean_wo_out
    
    print(paste("Media senza outlier: ", round(mean(data_sampled[outlier_rows, j]), 2)))
    print(paste("Outlier individuati: ", length(outlier_rows)))
    cat("---------------------------------------------- \n")
    
  }
}
#Outlier Detection (multivariate) ----
data_outdoor_scaled_outliers <- data_outdoor_scaled_pca
mean_vec <- colMeans(data_outdoor_scaled_outliers[,3:8])
cov_matrix <- cov(data_outdoor_scaled_outliers[,3:8])


mahal_dist <- mahalanobis(data_outdoor_scaled_outliers[,3:8], center = mean_vec, cov = cov_matrix)
plot(density(mahal_dist))

df <- 20  # Numero di variabili utilizzate
x_vals <- seq(0, max(mahal_dist), length.out = 100)  # Definisce un range di valori
y_vals <- dchisq(x_vals, df)  # Calcola la densità della distribuzione chi-quadrato

# Aggiungi la distribuzione chi-quadrato al grafico
lines(x_vals, y_vals, col = "red", lwd = 2, lty = 2)

# Step 3: Set the threshold using chi-squared distribution
threshold <- qchisq(0.99, df = 5)

# Step 4: Identify outliers
data_outdoor_scaled_outliers$outlier <- mahal_dist > threshold


pc.pollutant_outdoor_outlier <- princomp(data_outdoor_scaled_outliers[, 3:8], scores = T)
scores.pollutant_outdoor <- pc.pollutant_outdoor_outlier$scores

x11()
ggbiplot(pc.pollutant_outdoor_outlier, 
         obs.scale = 1, var.scale = 1, 
         groups =data_outdoor_scaled_outliers$outlier , 
         ellipse = FALSE,        
         circle = FALSE) +       
  theme_minimal() +             
  ggtitle("PCA Biplot of Outdoor Pollutants") +
  theme(legend.position = "bottom")

# Anova su HR (gruppo: place/subject) ------------------------------------------------------
# Importazione dati
dati <- read.table("data_HR_FB.txt", header=TRUE)

str(dati)
summary(dati)

# Converto i fattori
dati$place_ID <- as.factor(dati$place_ID)
dati$subject_ID <- as.factor(dati$subject_ID)

# Boxplot HR per luogo
ggplot(dati, aes(x = place_ID, y = HR)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "HR per luogo", x = "Luogo", y = "Frequenza cardiaca (HR)")

# Boxplot HR per soggetto
ggplot(dati, aes(x = subject_ID, y = HR)) +
  geom_boxplot(fill = "lightgreen") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "HR per soggetto", x = "Soggetto", y = "Frequenza cardiaca (HR)")




#COMMENTO: Per luogo (place_ID): non si osservano differenze visibili tra le medie,
# non suggerendo possibile violazione dell’ipotesi di omoscedasticità.
#Per soggetto (subject_ID): le differenze sono marcate. Ci sono soggetti con HR sistematicamente più alti o bassi.


# One-way ANOVA----------------------------------------------------------------
#FATTORE: PLACE

#Verifico le assunzioni della one-way ANOVA per place
# Normalità
Ps <- sapply(levels(dati$place_ID), function(gr) shapiro.test(dati$HR[dati$place_ID==gr])$p)
Ps

# Omogeneità delle varianze per i place
bartlett.test(dati$HR, dati$place_ID)

#COMMENTI SU ASSUNZIONI:
#Normalità (Shapiro-Wilk test): non possiamo assumere normalità nei gruppi.

#Omogeneità delle varianze (Bartlett test)
#Per luogo: p-value = 0.0008: eteroschedasticità significativa.


# RIVERIFICA delle assunzioni su HR con Boxcox

#1) Normalità per gruppo
#Provo con Box-Cox 
library(MASS)
fit_bc <- lm(HR ~ place_ID, data = dati)

# Box-Cox transformation
boxcox(fit_bc, lambda = seq(-2, 2, 0.1))

lambda_opt <- -1
dati$HR_boxcox <- (dati$HR^lambda_opt - 1) / lambda_opt

# RIVERIFICA
Ps_bc <- sapply(levels(dati$place_ID), function(gr) shapiro.test(dati$HR_boxcox[dati$place_ID == gr])$p)
Ps_bc
# con alpha=0.01 accetto normalità

#2) Proviamo a vedere se ora è valida la seconda assunzione con i dati trasformati con Box-Cox
bartlett.test(dati$HR_boxcox, dati$place_ID)
# Assunzione accettata anche a livello alpha=0.05

# Procedo a fare il modello
fit_bc_aov <- aov(HR_boxcox ~ place_ID, data = dati)
summary(fit_bc_aov)
#COMMENTO SUI RISULTATI: Effetto significativo del luogo sulla frequenza cardiaca probabilmente per effetto del place A.



#FATTORE: SUBJECT
#Verifico le assunzioni della one-way ANOVA per subject
#Normalità
Ps <- sapply(levels(dati$subject_ID), function(gr) shapiro.test(dati$HR[dati$subject_ID==gr])$p)
Ps
#Per soggetto: la situazione è leggermente migliore ma la normalità non è rispettata ovunque.

#omogeneità delle varianze per i place
bartlett.test(dati$HR, dati$subject_ID)
#Per soggetto: p-value = 3.55e-16: forte violazione dell’ipotesi.

# RIVERIFICA delle assunzioni su HR con Boxcox

#1) Normalità per gruppo
#Provo con Box-Cox 
library(MASS)
fit_bc <- lm(HR ~ subject_ID, data = dati)

# Box-Cox transformation
boxcox(fit_bc, lambda = seq(-2, 2, 0.1))

lambda_opt <- -0.6
dati$HR_boxcox <- (dati$HR^lambda_opt - 1) / lambda_opt

# RIVERIFICA
Ps_bc <- sapply(levels(dati$subject_ID), function(gr) shapiro.test(dati$HR_boxcox[dati$subject_ID == gr])$p)
Ps_bc
#La normalità non è rispettata per tutti i soggetti con alpha= 0.01, anche dopo la trasformazione Box-Cox.
#15 su 19 soggetti passano il test, quindi la violazione è limitata.

#Proviamo a vedere se ora è valida la seconda assunzione con i dati trasformati con Box-Cox
bartlett.test(dati$HR_boxcox, dati$subject_ID)

# Non ancora valida la seconda assunzione, allora vado a vedere se le varianze 
# sono tutte dello stesso ordine di grandezza
# Calcolo le varianze per ciascun subject
var_subject <- sapply(levels(dati$subject_ID), function(id) {
  var(dati$HR_boxcox[dati$subject_ID == id])
})

# Visualizza le varianze
var_subject

# sono tutte dello stesso ordine di grandezza (tranne uno che è e-04 ma va bene comunque),
# quindi ritengo anche la seconda assunzione valida

# Modello:
fit_subject <- aov(HR ~ subject_ID, data = dati)
summary(fit_subject)

# COMMENTI SUI RISULTATI: Effetto altamente significativo del soggetto. E' plausibile poiché ogni soggetto
# ha una HR media fisiologicamente diversa.

# ----------------------------------------------------------------------------------- #

# Eliminando il place A, cosa succede?
dati_noA <- subset(dati, place_ID != "A")
dati_noA <- droplevels(dati_noA) #rimuovo il place A in quanto livello della variabile factor

#FATTORE: PLACE
#Verifico le assunzioni della one-way ANOVA per place

#Normalità
Ps <- sapply(levels(dati_noA$place_ID), function(gr) shapiro.test(dati_noA$HR[dati_noA$place_ID==gr])$p)
Ps


#omogeneità delle varianze per i place
bartlett.test(dati_noA$HR, dati_noA$place_ID)

#COMMENTI SU ASSUNZIONI:
#Normalità (Shapiro-Wilk test): non possiamo assumere normalità nei gruppi.

#Omogeneità delle varianze (Bartlett test) per luogo: p-value = 0.0977, assunzione valida!


# RIVERIFICA delle assunzioni su HR_boxcox

#1) Normalità per gruppo
#Provo con Box-Cox 
library(MASS)
fit_bc <- lm(HR ~ place_ID, data = dati_noA)

# Box-Cox transformation
boxcox(fit_bc, lambda = seq(-2, 2, 0.1))

lambda_opt <- -0.8
dati_noA$HR_boxcox <- (dati_noA$HR^lambda_opt - 1) / lambda_opt

# RIVERIFICA
Ps_bc <- sapply(levels(dati_noA$place_ID), function(gr) shapiro.test(dati_noA$HR_boxcox[dati_noA$place_ID == gr])$p)
Ps_bc
#	A livello alpha= 0.05, 4 gruppi su 6 (C, E, F, G) non rispettano la normalità: normalità non soddisfatta
# A livello alpha= 0.01, solo G viola la normalità: normalità accettabile

#2) Proviamo a vedere se ora è valida la seconda assunzione con i dati trasformati con Box-Cox

bartlett.test(dati_noA$HR_boxcox, dati_noA$place_ID)
#Assunzione accettata anche a livello alpha=0.05

#Procedo con il modello
fit_bc_aov <- aov(HR_boxcox ~ place_ID, data = dati_noA)
summary(fit_bc_aov)
#COMMENTO SUI RISULTATI: Effetto non molto significativo.



#FATTORE: SUBJECT
#Verifico le assunzioni della one-way ANOVA per subject
#Normalità
Ps <- sapply(levels(dati_noA$subject_ID), function(gr) shapiro.test(dati_noA$HR[dati_noA$subject_ID==gr])$p)
Ps
#Per soggetto: la situazione la normalità non è rispettata ovunque.

#omogeneità delle varianze per i soggetti
bartlett.test(dati_noA$HR, dati_noA$subject_ID)
#Per soggetto:forte violazione dell’ipotesi.

# RIVERIFICA delle assunzioni su HR_boxcox

#1) Normalità per gruppo
#Provo con Box-Cox 
library(MASS)
fit_bc <- lm(HR ~ subject_ID, data = dati_noA)

# Box-Cox transformation
boxcox(fit_bc, lambda = seq(-2, 2, 0.1))

lambda_opt <- -0.6
dati_noA$HR_boxcox <- (dati_noA$HR^lambda_opt - 1) / lambda_opt

# RIVERIFICA
Ps_bc <- sapply(levels(dati_noA$subject_ID), function(gr) shapiro.test(dati_noA$HR_boxcox[dati_noA$subject_ID == gr])$p)
Ps_bc

# A livello alpha = 0.01, la situazione è accettabile: 16 su 19 soggetti risultano normali.
# A livello alpha = 0.05, invece la normalità non è rispettata in 7 soggetti

#Proviamo a vedere se ora è valida la seconda assunzione con i dati trasformati con Box-Cox

bartlett.test(dati_noA$HR_boxcox, dati_noA$subject_ID)
#Non ancora valida la seconda assunzione, allora vado a vedere se le varianze 
#sono tutte dello stesso ordine di grandezza
# Calcola le varianze per ciascun subject
var_subject <- sapply(levels(dati_noA$subject_ID), function(id) {
  var(dati_noA$HR_boxcox[dati_noA$subject_ID == id])
})

# Visualizza le varianze
var_subject

#sono tutte dello stesso ordine di grandezza, quindi ritengo anche la seconda assunzione valida

#Modello:
fit_subject <- aov(HR ~ subject_ID, data = dati_noA)
summary(fit_subject)

# COMMENTI SUI RISULTATI: Effetto altamente significativo del soggetto. E' plausibile, poiché ogni soggetto
#ha una HR media fisiologicamente diversa.

# ------------------------------------------------------------------------------ #

# Two-ways ANOVA----------------------------------------------------------------

#Verifico assunzioni two-ways ANOVA
#Normalità dei residui

#Modello
fit_int <- aov(HR ~ place_ID * subject_ID, data = dati) #con interazione tra place e subject
summary(fit_int)

fit_add <- aov(HR ~ place_ID + subject_ID, data = dati) #senza interazione tra place e subject
summary(fit_add)

# Residui del modello additivo
res <- residuals(fit_add)

# Shapiro-Wilk test
shapiro.test(res)  #rifiuto normalità: può essere per gli outlier 
#perchè W si avvicina a 1, inoltre guarda anche qqplot-> ma ho poche osservazioni 
#quindi non è valido lo shapiro test

# QQ-plot
qqnorm(res)
qqline(res)

# Istogramma
hist(res, breaks = 20, col = "gray", main = "Residui ANOVA") 
#vicino a una normale, forse sono gli outlier il problema
library(tseries)
jarque.bera.test(res)

#Provo a eliminare il place A perchè quando dovremo vedere l'impatto dei pollutant su HR ci interessano i luoghi all'esterno, inoltre
#potrebbero essere le osservazioni del place A a influire poichè siamo all'interno e mediamente i valori di Hr sono più bassi
dati_noA <- subset(dati, place_ID != "A")

#rifaccio tutto con il nuovo dataset
fit_int <- aov(HR ~ place_ID * subject_ID, data = dati_noA) #con interazione tra place e subject
summary(fit_int)

fit_add <- aov(HR ~ place_ID + subject_ID, data = dati_noA) #senza interazione tra place e subject
summary(fit_add)

# Residui del modello additivo
res <- residuals(fit_add)

# Shapiro-Wilk test
shapiro.test(res)  #accetto normalità: vicino a 0.05 
#inoltre W si avvicina ancora di più a 1, inoltre guarda anche qqplot

jarque.bera.test(res)

# QQ-plot
qqnorm(res)
qqline(res)

# Istogramma
hist(res, breaks = 20, col = "gray", main = "Residui ANOVA senza place A") 
#vicino a una normale, forse sono gli outlier il problema


#MANOVA 1 ----

# Research question: nelle stazioni outdoor le concentranzioni di inquinanti sono diverse?

data_manova <- subset(data_shuffled, data_shuffled$Outdoor == "T")
data_manova <- data_manova[, 2:10] # Elimino "subject_ID", "MorA", "Outdoor"
data_manova <- data_manova[, -c(3,4,5,9)] # Elimino "P", "PM1", "PM10", "VOC" lasciando solo gli inquinanti più significativi per la nostra analisi

head(data_manova)

y <- data_manova[, -c(1)]
#H0: u1 = u2 = ... = u3
robust_manova <- Wilks.test(y, grouping = data_manova$place_ID)
?Wilks.test
robust_manova



data_matrix <- data_manova[, -1]  # assuming first column is 'place_ID'
gruppo <- data_manova$place_ID
dati <- data.frame(gruppo)

# Compute distance matrix
data_dist <- vegdist(data_matrix, method = "euclidean")


permanova_result <- adonis2(data_dist ~ gruppo, data = dati, permutations = 200)
permanova_result

#Si potrebbe usare PERMANOVA



