library(Hmisc)

# Assuming your dataframe is called 'df' containing the values of alpha diversity metrics and 
# Specify the variables of interest
microbiome_indices <- c('Chao', 'Shannon', 'Simpson')
biomarkers <- c('pep1', 'pep2', 'g17', 'zonulin', 'tbio', 'tendo', 'Thisto', 'final_leeds_score', 'final_score') #any other continous variable can go here 

# Confounders: Age and Gender
confounders <- c("age", "gender",'disease','comorb') #any other categorical *confounding" variable can go here 

# Initialize a list to store results
results <- list()

# Loop over all combinations of microbiome indices and biomarkers
for (micro in microbiome_indices) {
  for (bio in biomarkers) {
    # Residualize microbiome index by regressing on confounders
    micro_model <- lm(as.formula(paste(micro, "~", paste(confounders, collapse = "+"))), data = df, na.action = na.exclude)
    micro_residuals <- residuals(micro_model)
    
    # Residualize biomarker by regressing on confounders
    bio_model <- lm(as.formula(paste(bio, "~", paste(confounders, collapse = "+"))), data = df, na.action = na.exclude)
    bio_residuals <- residuals(bio_model)
    
    # Perform Kendall's correlation on the residuals
    test <- cor.test(micro_residuals, bio_residuals, method = "kendall") #spearman and pearson can also be used
    
    # Store the result (correlation and p-value)
    results[[paste(micro, bio, sep = "_")]] <- c(correlation = test$estimate, p_value = test$p.value)
  }
}

# Convert the list to a dataframe for easier viewing
result_df <- as.data.frame(do.call(rbind, results))
print(result_df)
