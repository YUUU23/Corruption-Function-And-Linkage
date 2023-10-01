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




# import medium data set 
md_data_df <- read.csv("md_data.csv", header=TRUE)

# Function calls, generate 5 blocks from sm_data_df: 
mut_md_data <- md_data_df
mut_md_data[mut_md_data == 'na'] <- NA
md_block_five <- df_to_block_equal(mut_md_data, 5)
mut_md_block_five <- md_block_five

# Corrupt Age 
mut_md_block_five <- block_corr_with_degree("Age", mut_md_block_five, 2, 5)

# Corrupt with NA to gender 
mut_md_block_five <- block_corr_general(corr_missing_val, "Gender", mut_md_block_five, 2, 5)

# Corrupt phone number 
mut_md_block_five <- block_corr_general(corr_zero_nine_number,"Phone.Number", mut_md_block_five, 2, 5)

# Corrupt first name by adding letter 
mut_md_block_five <- block_corr_general(corr_add_letter,"Name", mut_md_block_five, 2, 5)

# Corrupt first name by deleting letter 
mut_md_block_five <- block_corr_general(corr_del_letter,"Name", mut_md_block_five, 2, 5)



# Function to save data frames to file 
# idx <- 1
# for(block in mut_md_block_five){
#   file_name <- paste("mut_md_block",idx,".csv", sep="")
#   print(file_name)
#   write.csv(block, file=file_name, quote = FALSE)
#   idx <- idx+1
# }

