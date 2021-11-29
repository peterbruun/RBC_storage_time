args <- commandArgs(trailingOnly=TRUE)

library(tidyverse)

setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/")
# Load data
data <- read_tsv(args[1])
# data <- read_tsv("data/interim/data_mortality.tsv")
#data = read_tsv("data/processed/gformula_data_IPW.tsv")

# Set seed
seednum <- str_split(args[2], "bootstrap_",simplify = TRUE)[2]
seednum <- as.integer(str_remove(seednum,".tsv")) + 1000
set.seed(seednum)


sample_n_groups = function(grouped_df, size, replace = TRUE, weight=NULL) {
  grp_var <- grouped_df %>% 
    groups %>%
    unlist %>% 
    as.character
  random_grp <- grouped_df %>% 
    summarise() %>% 
    sample_n(size, replace, weight) %>% 
    mutate(unique_id = 1:NROW(.))
  grouped_df %>% 
    right_join(random_grp, by=grp_var) %>% 
    group_by_(grp_var) 
}

# Number of samples
n_samples = n_distinct(data$patient_cpr_enc)
data <- data %>% group_by(patient_cpr_enc) %>% sample_n_groups(n_samples)
#n_distinct(test$patient_cpr_enc)
#n_distinct(test$unique_id)

# Save bootstrapped data
write_tsv(data,args[2])