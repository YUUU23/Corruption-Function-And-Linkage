library(dplyr)


# divide data into n equaled-sized blocks where each block holds unique records
# 
# Param: 
#   df : dataframe to divide 
#   n : number of blocks 
# 
# Return: 
#   each block as its own df in a vector  
df_to_block_equal <- function(df, n) {
  # split - split df into groups based on their assigned block number 
  #   !! gives warning when block size is not multiple of n -> suppressed warning, expected behavior
  # rep - assign each record a number 1:n, this number is the block the record is in 
  #   times - amt of record in each block (assigned a block number) 
  # sample: randomly assign record to block number 
  
  size <- nrow(df)
  sample_func <- sample(rep(1:n,times=floor(size/n)))
  return (suppressWarnings(split(df, sample_func)))
}
