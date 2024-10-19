result_df$pair <- rownames(result_df)

# Melt the correlation and p-value data for plotting
correlation_melt <- melt(dcast(result_df, Microbiome_Index ~ Biomarker, value.var = "Correlation"))
pvalue_melt <- melt(dcast(result_df, Microbiome_Index ~ Biomarker, value.var = "Adjusted_P_Value"))

# Create labels that combine correlation values and asterisks for p < 0.2
correlation_melt$label <- ifelse(pvalue_melt$value < 0.2, 
                                 paste0(round(correlation_melt$value, 2), " *"), 
                                 round(correlation_melt$value, 2))

# Plot the correlation heatmap with correlation values and asterisks
ggplot(correlation_melt, aes(x = variable, y = Microbiome_Index, fill = value)) +
    geom_tile() +
    geom_text(aes(label = label), color = "black", size = 2) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), name = "Kendall\nCorrelation") +
    theme_minimal() +
    labs(title = "Robust Kendall correlations (Alpha diversity vs Markers)"", 
         x = "Biomarker", y = "Microbiome Index") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
