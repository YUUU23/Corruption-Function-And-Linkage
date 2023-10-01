library(dplyr)
source("DataCorruptionFunctions.R")
source("BlockingFunction.R")
source("CorruptionFunctionWithProbability.R")

# import small data set
sm_data_df <- read.csv("sm_sample_dataset.csv", header=TRUE)

# Function calls, generate 2 blocks from sm_data_df: 
mut_sm_data <- sm_data_df
mut_sm_data[mut_sm_data == 'na'] <- NA
block_two <- df_to_block_equal(mut_sm_data, 2)

# Corrupt Age 
mut_block_two <- block_corr_with_degree("Age", block_two, 2, 2)

# Corrupt with NA to gender 
mut_block_two <- block_corr_general(corr_missing_val, "Gender", mut_block_two, 2, 2)

# Corrupt phone number 
mut_block_two <- block_corr_general(corr_zero_nine_number,"Phone.Number", mut_block_two, 2, 2)

# Corrupt first name by adding letter 
mut_block_two <- block_corr_general(corr_add_letter,"First.Name", mut_block_two, 2, 2)

# Corrupt first name by deleting letter 
mut_block_two <- block_corr_general(corr_del_letter,"First.Name", mut_block_two, 2, 2)

