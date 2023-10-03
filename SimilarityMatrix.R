install.packages("fastDummies")
library(fastDummies)
install.packages('flexclust')
library('flexclust')
install.packages("stringdist")
library(stringdist)

# Read in Data 
block1 <- read_csv("sm_data blocking + mut data/block1.csv")
block2 <- read_csv("sm_data blocking + mut data/block2.csv")
mut_block1 <- read_csv("sm_data blocking + mut data/mut_block1.csv")
mut_block2 <- read_csv("sm_data blocking + mut data/mut_block2.csv")


# Use rdist function for continuous values 
block1_dist_Income <- dist2(block1$Income, mut_block1$Income)# return difference of income 
block1_dist_Age <- dist2(block1$Age, mut_block1$Age) # returns difference of age 

block2_dist_Income <- dist2(block2$Income, mut_block2$Income)# return difference of income 
block2_dist_Age <- dist2(block2$Age, mut_block2$Age) # returns difference of age 


# Categorical values 

# dummification to Gender: change all NA values to na first so matrix will not have NA 
block1[is.na(block1)] <- "na" 
mut_block1[is.na(mut_block1)] <- "na"
gender_dummy_b1 <- dummy_cols(block1$"Gender")
gender_dummy_mut_b1 <- dummy_cols(mut_block1$"Gender")
# return distance between two values -> 0: same gender 0.667: different gender
block1_dist_Gender <- dist2(gender_dummy_b1, gender_dummy_mut_b1, method="binary")

block2[is.na(block1)] <- "na" 
mut_block2[is.na(mut_block1)] <- "na"
gender_dummy_b2 <- dummy_cols(block2$"Gender")
gender_dummy_mut_b2 <- dummy_cols(mut_block2$"Gender")
# return distance between two values -> 0: same gender 0.667: different gender
block2_dist_Gender <- dist2(gender_dummy_b2, gender_dummy_mut_b2, method="binary")

# Levenshtein Distance: measure distance between First.Name and Hometown 
block1_dist_fir_name <- stringdistmatrix(block1$First.Name, mut_block1$First.Name, method = "lv")
block1_dist_hometown <- stringdistmatrix(block1$Hometown, mut_block1$Hometown, method = "lv")

block2_dist_fir_name <- stringdistmatrix(block2$First.Name, mut_block2$First.Name, method = "lv")
block2_dist_hometown <- stringdistmatrix(block2$Hometown, mut_block2$Hometown, method = "lv")

# measure distance between birthdays  
block1$birthday_format <- strptime(as.character(block1$Birthday), "%d-%m-%Y")
mut_block1$birthday_format <- strptime(as.character(block1$Birthday), "%d-%m-%Y")
block1_dist_bday <- matrix(nrow = length(block1)-1, ncol = length(block1)-1)

block2$birthday_format <- strptime(as.character(block2$Birthday), "%d-%m-%Y")
mut_block2$birthday_format <- strptime(as.character(block2$Birthday), "%d-%m-%Y")
block2_dist_bday <- matrix(nrow = length(block2)-1, ncol = length(block2)-1)

for(i in 1:length(block1)-1){
  for(j in 1:length(block1)-1){
    block1_dist_bday[i,j]<- abs(difftime(block1$birthday_format[i], 
                                          mut_block1$birthday_format[j], unit="days"))
  }
}

for(i in 1:10){
  for(j in 1:10){
    block1_dist_bday[i,j]<- abs(difftime(block2$birthday_format[i], 
                                         mut_block2$birthday_format[j], unit="days"))
  }
}


# use Levenshtein distance to measure the difference between two phone number 
#   (distance of 1 => 1 different value in the corresponding place) 
block1_dist_ph_number <- stringdistmatrix(block1$Phone.Number, mut_block1$Phone.Number, method = "lv")
block2_dist_ph_number <- stringdistmatrix(block2$Phone.Number, mut_block2$Phone.Number, method = "lv")














