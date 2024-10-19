library(psych)   # for corr.test() instead of cor.test which doesn't support p value adjustment

# Confounders: age and gender
confounders <- c("age", "sex",'smokn','alcn','comorb','Disease')

# Specify the variables of interest
microbiome_indices <- c('Chao', 'Shannon', 'Simpson')
biomarkers <- c('pep1', 'pep2', 'g17', 'zonulin', 'tbio', 'tendo', 'Thisto', 'final_leeds_score')

# Initialize lists to store residuals
residuals_microbiome <- list()
residuals_biomarkers <- list()

# Residualize microbiome indices by regressing on confounders
for (micro in microbiome_indices) {
    micro_model <- lm(as.formula(paste(micro, "~", paste(confounders, collapse = "+"))), data = df, na.action = na.exclude)
    residuals_microbiome[[micro]] <- residuals(micro_model)
}

# Residualize biomarkers by regressing on confounders
for (bio in biomarkers) {
    bio_model <- lm(as.formula(paste(bio, "~", paste(confounders, collapse = "+"))), data = df, na.action = na.exclude)
    residuals_biomarkers[[bio]] <- residuals(bio_model)
}

# Convert residuals lists into data frames
residuals_microbiome_df <- as.data.frame(residuals_microbiome)
residuals_biomarkers_df <- as.data.frame(residuals_biomarkers)

# Compute Kendall correlation using corr.test with BH correction
corr_results <- corr.test(residuals_microbiome_df, residuals_biomarkers_df, method = "kendall", 
                          use = "pairwise.complete.obs", adjust = "BH")

# View the correlation values and adjusted p-values
correlation_values <- corr_results$r
adjusted_p_values <- corr_results$p

# Combine results into a single dataframe for easier viewing
result_df <- data.frame(
    Microbiome_Index = rep(rownames(correlation_values), times = ncol(correlation_values)),
    Biomarker = rep(colnames(correlation_values), each = nrow(correlation_values)),
    Correlation = as.vector(correlation_values),
    Adjusted_P_Value = as.vector(adjusted_p_values)
)

# View the result dataframe
print(result_df)
