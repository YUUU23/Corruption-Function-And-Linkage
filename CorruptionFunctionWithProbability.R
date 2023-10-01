library(dplyr)
source("DataCorruptionFunctions.R")
source("BlockingFunction.R")

# HELPER FUNCTION --> 

# helper function to apply a corruption to a single block
#
# param: 
#   func: the corruption function to apply 
#   col: the attribute 
#   block: the block to corrupt 
#   prob: the probability to apply corruption to the block 
#
# return: 
#   the corrupted block (copy)
corr_single_block <- function(func, col, block, prob){
  idxs <- sample(1:length(block[, col]), floor(length(block[, col]) * prob))
  
  for (idx in idxs) {
    block <- func(block, col, idx)
  }
  
  return(block)
}


# decide the probability of applying corruption function x to each block 
#   using beta distribution 
# 
# Param: 
#   n : number of blocks 
#   alpha: alpha value 
#   beta: beta value 
# 
# Return: 
#   vector of integer denoting probability. idx i is applied to the nth block where i=n   
corr_fun_prob <- function(n, alpha, beta) {
  prob_vec <- round(rbeta(n, alpha, beta), digits=2)# Not sure about this 
  return (prob_vec)
}



# APPLYING CORRUPTION FUNCTIONS --> 

# applies a corruption function to each block with random generated probability using beta distribution  
#   uses corr_single_block function to apply corruption to each block 
#   function for general corruption functions that does not need to generate corruption by how much
# 
# Param: 
#   func: the corruption function 
#   col: the attribute to corrupt 
#   blocks: vector containing the blocks 
#   alpha: beta distribution value 
#   beta: beta distribution value 
# 
# Return: 
#   vector containing new mutated blocks with corruption function applied  

block_corr_general <- function(func, col, blocks, alpha, beta) {
  prob_vec <- corr_fun_prob(length(blocks), alpha, beta) 
  print(prob_vec)
  mut_blocks <- c()
  block_i <- 1
  for (block in blocks) {
    block <- data.frame(block)
    block <- corr_single_block(func, col, block, prob_vec[block_i])
    mut_blocks <- append(mut_blocks, list(block))
    block_i <- block_i + 1 
  }
  return(mut_blocks)
}



# for corr_num_by_range function, decide the range input (how much the corruption is) 
#   for one block. If the corr_fun_prob is 0 for block n_i, then idx i wouldn't take effect 
#   Generate random range value with uniform distribution 
# 
# Param: 
#   df : the block 
#   col : col in df, *to get range of data value 
# 
# Return: 
#   int denoting degree of corruption 
corr_num_range_degree <- function(df, col) {
  min <- min(df[,col])
  max <- max(df[,col])
  if (!is.numeric(min) || !is.numeric(max)) {
    print("values associated with this attribute is not a number")
  } else {
    # wouldn't make sense to generate a val for range from (min, max)  
    #   since this range can get really big -> EX: (min=18, max=94), we don't want +/- 86 for range
    max_range <- floor((min + max) / 10) 
    random_range <- floor(runif(1, min=0, max=max_range))
    return (random_range)
  }
  return(1)
}


block_corr_with_degree <- function(col, blocks, alpha, beta) {
  prob_vec <- corr_fun_prob(length(blocks), alpha, beta)
  print(prob_vec)
  mut_blocks <- c()
  block_i <- 1
  for (block in blocks) {
    block <- data.frame(block)
    idxs <- sample(1:length(block[, col]), floor(length(block[, col]) * prob_vec[block_i]))
    corr_deg <- corr_num_range_degree(block, col)
    print(corr_deg)
    for (idx in idxs) {
      block <- corr_num_by_range(block, col, idx, corr_deg)
    }
    mut_blocks <- append(mut_blocks, list(block))
    block_i <- block_i + 1 
  }
  return(mut_blocks)
}






