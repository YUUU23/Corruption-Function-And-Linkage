# Corruption-Function-And-Linkage

This repository contains corruption function with example datasets (original and corrupted) and corruption analysis with similarity matrices for UTRA Fall 2023 Project "Record Linkage with Differing Errors Across Blocks". 

**BlockingFunction.R**: stores functions for dividing dataset into blocks randomly 
- df_to_block_equal : split dataset randomly into n-equal-sized blocks 

**DataCorruptionFunctions.R** : stores functions used to corrupt one data-value 
- rand_norm : generates random int in desired range with normal distribution *helper func.* 
- corr_num_by_range: corrupt continous values by changing it to random integer within desired range generated by rand_norm 
- corr_missing_val: corrupt values by changing to NA; if value is already NA, do nothing. Require data-frame null representation to be NA 
- corr_zero_nine_number: corrupt by changing values [0-9] into random integer [0-9] (uniform distribution) 
- corr_del_letter: corrupt value stored as string by deleteing random letter in value (normal distribution) 
- corr_add_letter: corrupt value stored as string by appending random letter (uniform distribution) to random position in string (normal distribution) 

**CorruptionFunctionWithProbability.R** : functions to apply corruption function in DataCorruptionFunctions.R to data-frame columns with beta distribution probability; corruption can be applied to multiple blocks or single data-frame (use block size n=1 for all functions) 

- corr_single_block: corrupts a single block with given probability and function *helper func.*
- corr_fun_prob: generates vector of probability (desired beta distribution) denoting probability that corruption function is applied to each block
- block_corr_general: corrupting vector of block where the degree of corruption remains constant
- corr_num_range_degree: generate random degree of corruption (range) to be used in corr_num_by_range (uniform distribution) 
- block_corr_with_degree: corrupting vector of block where the degree of corruption varies block to block (i.e. corr_num_by_range), !! currently only used with corr_num_by_range function !! 

**CorruptData.R** : Series of corruption applied to sm_sample_data.csv and md_data.csv (generated randomly be GeCo)

- sm_sample_data.csv corruption:
    * divided into 2 blocks, uses beta distribution (2,2) to calculate probability that corruption is applied to each attribute (col) -> 
    * corrupt age: (range) **b1: 0.28, degree: 6 b2: 0.30 degree 6**
    * corrupt gender: (missing_val) **0.30 0.19**
    * corrupt phone number: (zero_nine_number) **0.35 0.40**
    * corrupt first name: (add_letter) **0.76 0.61**
    * corrupt first name: (del_letter) **0.41 0.26**

- md_sample_data.csv corruption:
    * divided into 5 blocks, uses beta distribution (2,5) to calculate probability that corruption is applied to each attribute (col), same corruption functions are applied accordingly as sm_sample_data.csv corruption -> 
    * corrupt age: **b1: 0.66(degree=5) b2: 0.21(degree=0) b3: 0.41(degree=5) b4: 0.30(degree=4) b5: 0.57(degree=10)**
    * corrupt gender: **0.20 0.06 0.31 0.27 0.31**
    * corrupt phone number: **0.12 0.21 0.33 0.38 0.47**
    * corrupt first name add_letter: **0.13 0.19 0.15 0.43 0.59**
    * corrupt first name del_letter: **0.29 0.27 0.16 0.40 0.61**

**sm_sample_dataset.csv**: sample dataset randomly generated from GeCo with 20 entries 

**md_data.csv**: sample dataset randomly generated from GeCo with 100 entries

**sm_data blocking + mut data**: sm_sample_dataset.csv corruption results -> folder stores each block, original and corrupted, as individual dataframes  

**md_data blocking + mut data**: md_data.csv corruption results -> folder stores each block, original and corrupted, as individual dataframes 


### After Corruption 
**SimilarityMatrix.R** 

