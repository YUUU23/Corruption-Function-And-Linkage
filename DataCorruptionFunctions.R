library(truncnorm)
library(stringr)
library(stringi)


# Generate one random integer within range using probability of normal distribution 
#   with mean at (max+min)/2, and sd of 3 to cover most numbers 
#
# Param: 
#   min: range minimum 
#   max: range maximum  

rand_norm <- function(min=1, max) {
  rand <- floor(rtruncnorm(n=1, a=min, b=max+1, mean=((min+max)/2), sd=3))
  return(rand)
}


# corruption by mutating attribute value to randomize integer within desired range using normal distribution
#   based on input range and attribute value. Generates only 0 or positive numbers.  
#   
#   example : if attribute value is 35, and val is 5, returns value ranged [30,40]
#   could be used for age, when people send in fake age within a range 
#
# param: 
#   df : dataframe 
#   col : attribute (column)
#   idx : one value from column
#   range : establish the range of potential value for mutation 
#     where range is established as [attr_val-range, attr_val+range], should be positive
#   no_org : TRUE if original attr_val should be excluded from random number range, 
#             FALSE if it should be included (default TRUE)
# 
# return: 
#   copy of dataframe with mutated value  

corr_num_by_range <- function(df, col, idx, range) {
  # Get value of targeted mutate value 
  attr_val <- df[, col][idx] 
  
  # Check for if input range and target mutate value is valid 
  if(attr_val < 0 || range < 0) {
    stop("invalid range or attribute value") 
    return (NA) 
  } 
  
  # Set range 
  range_min <- attr_val - range 
  range_max <- attr_val + range 
  
  # Generate one random integer within range and is positive
  mutated_val <- rand_norm(range_min, range_max) 
  while(mutated_val < 0) {
    mutated_val <- rand_norm(range_min, range_max)
  }
  
  
  # Set generated value back to dataframe 
  df[, col][idx] <- mutated_val
  return(df)
}



# Corruption by turning not-missing value into missing value. 
#   If value is already missing, do nothing. 
#   Need to turn df NA-representation to NA 
# 
# param: 
#   df : data frame (expect all null representation to be NA)
#   col : attribute column 
#   idx : idx of value to NA  
# 
# return: 
#   copy of dataframe with mutated value  

corr_missing_val <- function(df, col, idx) {
  val_to_mut <- df[, col][idx]
  
  # only change to NULL if value is not NULL already 
  if(!is.na(val_to_mut)){
    val_to_mut <- NA
  } else {
    print("value is already NA")
  }
  
  df[, col][idx] <- val_to_mut
  return(df) 
}



# Change one random value in list of number (i.e. phone number, ssn) to random value in 0-9 
#   Checks for if value to be change is a integer 
# 
# param: 
#   df : data frame 
#   col : attribute column 
#   idx : idx of value to NA    
#
# return: 
#   copy of mutated df 

corr_zero_nine_number <- function(df, col, idx) {
  val_to_mut <- df[, col][idx]
  toString(val_to_mut)
  len <- nchar(val_to_mut)
  
  # Generate random index 
  repeat{
    idx_to_change <- floor(runif(1, min=1, max=len+1))
    
    # check that char at randomly generated index is a integer 
    if(grepl("^[[:digit:]]+$", substr(val_to_mut, idx_to_change, idx_to_change))) {
      break
    }
  }
  
  # Generate random integer 0-9
  repeat {
    val_to_change <- floor(runif(1, min=0, max=10))
    
    # check that char about to be change is not the same as the randomly generated value 
    if(val_to_change != substr(val_to_mut, idx_to_change, idx_to_change)){
      break 
    }
  }
  
  str_sub(val_to_mut, idx_to_change, idx_to_change) <- toString(val_to_change)
  df[, col][idx] <- val_to_mut
  return(df)
}



# Corruption by randomly choosing one character to delete
#   Use function rand_norm for normally distributed probability of randomly choosing character
#   (mimic higher probability that character in middle of string is deleted).
#
# param: 
#   df : data frame (expect all null representation to be NA)
#   col : attribute column 
#   idx : idx of value to NA
#
# return: 
#   copy of dataframe with mutated value  

corr_del_letter <- function(df, col, idx) {
  val_to_mut <- df[, col][idx]
  toString(val_to_mut)
  len <- nchar(val_to_mut)
  
  repeat{
    rmv_idx <- rand_norm(1, len)
    if(substr(val_to_mut, rmv_idx, rmv_idx) != " "){
      break
    }
  }
  str_sub(val_to_mut, rmv_idx, rmv_idx) <- ""
  
  df[, col][idx] <- val_to_mut 
  return(df)
}


# Corruption by randomly choosing one index to add character
#   Use rand_norm function for normally distributed probability of randomly choosing index 
#   (mimic higher probability that character in middle of string is deleted).
#
# param: 
#   df : data frame (expect all null representation to be NA)
#   col : attribute column 
#   idx : idx of value to NA
#
# return: 
#   copy of dataframe with mutated value  
corr_add_letter <- function(df, col, idx) {
  val_to_mut <- df[, col][idx]
  toString(val_to_mut)
  len <- nchar(val_to_mut)
  
  add_idx <- rand_norm(0, len+1)
  add_char <- stri_rand_strings(1, 1, pattern = "[a-z]")
  val_to_mut <- paste(c(substr(val_to_mut, 0, add_idx), substr(val_to_mut, add_idx+1,len+1)), collapse=add_char)
  
  df[, col][idx] <- val_to_mut 
  return(df)
}










