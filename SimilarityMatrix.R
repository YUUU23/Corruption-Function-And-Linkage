install.packages('fields')
library(fields)

# Read in Data 
block1 <- read_csv("sm_data blocking + mut data/block1.csv")
block2 <- read_csv("sm_data blocking + mut data/block2.csv")
mut_block1 <- read_csv("sm_data blocking + mut data/mut_block1.csv")
mut_block2 <- read_csv("sm_data blocking + mut data/mut_block2.csv")

block1_dist_Income <- rdist(block1$Income, mut_block1$Income)
block1_dist_Age <- rdist(block1$Age, mut_block1$Age)
