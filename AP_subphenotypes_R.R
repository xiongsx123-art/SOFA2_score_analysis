# Clustering Analysis
library(openxlsx)
library(cluster)       
library(ggplot2)
library(clusterCrit)
library(mclust)       
library(tidyverse)     

# Read data from Excel file
data <- read.xlsx("data.xlsx")

# Data standardization
scaled_data <- scale(data)

# Remove rows containing NA values after standardization
scaled_data <- na.omit(scaled_data)

# Function to determine optimal number of clusters K using Elbow Method
k_elbow <- function(data, max_k = 15) {
  wss <- numeric(max_k)  # Within-cluster sum of squares
  
  for (k in 1:max_k) {
    km_result <- kmeans(data, centers = k, nstart = 25)
    wss[k] <- km_result$tot.withinss
  }
  
  elbow_data <- data.frame(k = 1:max_k, wss = wss)
  
  # Create Elbow Method plot
  p <- ggplot(elbow_data, aes(x = k, y = wss)) +
    geom_line() + 
    geom_point() + 
    theme_bw() + 
    labs(title = "Elbow Method Plot", 
         x = "Number of Clusters (k)", 
         y = "Within-Cluster Sum of Squares (WSS)")
  
  return(list(plot = p, wss = wss))
}

# Execute and display Elbow Method results
elbow_result <- k_elbow(scaled_data)
print(elbow_result$plot)

# Function to determine optimal number of clusters K using Silhouette Method
silhouette_method <- function(data, max_k = 15) {
  silhouette_width <- numeric(max_k - 1)
  
  for (k in 2:max_k) {
    km_result <- kmeans(data, centers = k, nstart = 25)
    sil <- silhouette(km_result$cluster, dist(data))
    silhouette_width[k - 1] <- mean(sil[, 3])
  }
  
  silhouette_data <- data.frame(k = 2:max_k, silhouette = silhouette_width)
  
  # Create Silhouette Coefficient plot
  p <- ggplot(silhouette_data, aes(x = k, y = silhouette)) +
    geom_line() + 
    geom_point() + 
    theme_bw() +
    labs(title = "Silhouette Method Plot", 
         x = "Number of Clusters (k)", 
         y = "Average Silhouette Coefficient")
  
  # Find optimal k value (k corresponding to maximum silhouette coefficient)
  optimal_k <- which.max(silhouette_width) + 1
  
  return(list(plot = p, 
              silhouette = silhouette_width, 
              optimal_k = optimal_k))
}

# Execute and display Silhouette Method results
silhouette_result <- silhouette_method(scaled_data)
print(silhouette_result$plot)

# Determine optimal number of clusters K using Gap Statistic
gap_statistic <- clusGap(scaled_data, 
                         FUNcluster = kmeans, 
                         K.max = 15,
                         B = 50)  # B = number of bootstrap samples
print(plot(gap_statistic, main = "Gap Statistic Plot"))
set.seed(123)

# Perform K-means clustering with selected K value
km_result <- kmeans(scaled_data, centers = final_k, nstart = 25)

# View clustering results
print("Cluster Assignment Results:")
print(km_result$cluster)

print("Cluster Centers:")
print(km_result$centers)

print("Total Within-Cluster Sum of Squares:")
print(km_result$tot.withinss)

print("Within-Cluster Sum of Squares for Each Cluster:")
print(km_result$withinss)

print("Total Sum of Squares:")
print(km_result$totss)

print("Between-Cluster Sum of Squares:")
print(km_result$betweenss)

# Visualize clustering results using PCA (dimensionality reduction)
pca_result <- prcomp(scaled_data, scale. = FALSE)
pca_df <- data.frame(pca_result$x[, 1:2])  
pca_df$Cluster <- factor(km_result$cluster)

# Create PCA plot with clusters
pca_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  stat_ellipse(type = "norm", alpha = 0.5) +  
  theme_minimal() +
  labs(title = "Clustering Results Visualization (PCA)",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  theme(legend.position = "right")

print(pca_plot)

# Calculate clustering validation metrics
print("Clustering Validation Metrics:")

# 1. Average Silhouette Coefficient
avg_silhouette <- mean(silhouette(km_result$cluster, dist(scaled_data))[, 3])
print(paste("Average Silhouette Coefficient:", round(avg_silhouette, 4)))

# 2. Dunn Index
dunn_index <- intCriteria(as.matrix(scaled_data), km_result$cluster, "Dunn")[[1]]
print(paste("Dunn Index:", round(dunn_index, 4)))

# 3. Davies-Bouldin Index
db_index <- intCriteria(as.matrix(scaled_data), km_result$cluster, "Davies_Bouldin")[[1]]
print(paste("Davies-Bouldin Index:", round(db_index, 4)))

# 4. Calinski-Harabasz Index
ch_index <- intCriteria(as.matrix(scaled_data), km_result$cluster, "Calinski_Harabasz")[[1]]
print(paste("Calinski-Harabasz Index:", round(ch_index, 4)))

# Other useful metrics
print("Cluster Sizes:")
print(table(km_result$cluster))

# Calculate between/within cluster sum of squares ratio
bw_ratio <- km_result$betweenss / km_result$tot.withinss
print(paste("Between/Within Cluster Sum of Squares Ratio:", round(bw_ratio, 4)))


# -----------------------------------------------------------------------------
# Gaussian Mixture Model (GMM) Clustering

df_gmm <- as.data.frame(scaled_data)

# Perform GMM clustering
set.seed(2024)
gmm <- Mclust(data = df_gmm, 
              G = 1:9) 

# GMM model summary
print(summary(gmm))

# Get clustering results (classification)
df_gmm$cluster <- as.factor(gmm$classification)

# Reuse previous pca_result to ensure comparable plots
pca_df_gmm <- data.frame(pca_result$x[, 1:2])
pca_df_gmm$Cluster <- df_gmm$cluster

# Plot GMM results
gmm_plot <- ggplot(pca_df_gmm, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  stat_ellipse(type = "norm", alpha = 0.5) + # Add confidence ellipse
  labs(title = "GMM Clustering Results (PCA Projection)", 
       x = "Principal Component 1", 
       y = "Principal Component 2",
       color = "Cluster") +
  theme_bw() +
  theme(legend.position = "right")

print(gmm_plot)

comparison_table <- table(KMeans = km_result$cluster, GMM = gmm$classification)
print("Comparison of K-means and GMM Cluster Assignments:")
print(comparison_table)


# boruta_ridgeplot
library(readxl) 
library(dplyr) 
library(tidyr) 
library(ggplot2) 
library(ggridges) 
library(scales) 
library(Boruta) 
library(reshape2) 
options(warn = -1) 

# 1) Read data
file_path <- "data.xlsx"

# 2) Check data structure
print("Data Structure:")
print(str(df))

# 3) Prepare data for Boruta analysis
# Separate features and target variable
x <- df %>% select(-Outcome)
y <- df$Outcome

# 4) Perform Boruta feature selection
print("Performing Boruta feature selection...")
set.seed(123)
boruta <- Boruta(x = x, y = y, pValue = 0.01, mcAdj = TRUE, maxRuns = 50)  # Reduce to 50 iterations

# 5) View Boruta results
print("Boruta Analysis Results:")
print(boruta)

# 6) Extract importance data
print("Extracting importance data...")
boruta.imp <- function(x) {
  imp <- reshape2::melt(x$ImpHistory, na.rm = TRUE)[, -1]
  colnames(imp) <- c("Variable", "Importance")
  imp <- imp[is.finite(imp$Importance), ]
  
  variableGrp <- data.frame(Variable = names(x$finalDecision), 
                            finalDecision = x$finalDecision)
  
  showGrp <- data.frame(Variable = c("shadowMax", "shadowMean", "shadowMin"), 
                        finalDecision = c("shadowMax", "shadowMean", "shadowMin"))
  
  variableGrp <- rbind(variableGrp, showGrp)
  boruta.variable.imp <- merge(imp, variableGrp, all.x = TRUE)
  
  sortedVariable <- boruta.variable.imp %>% 
    group_by(Variable) %>% 
    summarise(median = median(Importance)) %>% 
    arrange(median)
  
  sortedVariable <- as.vector(sortedVariable$Variable)
  boruta.variable.imp$Variable <- factor(boruta.variable.imp$Variable, levels = sortedVariable)
  
  invisible(boruta.variable.imp)
}

boruta.variable.imp <- boruta.imp(boruta)
print("Importance Data Preview:")
print(head(boruta.variable.imp))

# 7) Process data for ridge plot
boruta.variable.imp <- boruta.variable.imp %>%
  mutate(Decision = case_when(
    finalDecision %in% c("shadowMax", "shadowMean", "shadowMin") ~ "Shadow Variables",
    finalDecision == "Confirmed" ~ "Confirmed Important",
    finalDecision == "Rejected" ~ "Rejected (Not Important)",
    TRUE ~ finalDecision
  ))

# 8) Generate ridge plot
print("Generating ridge plot...")
p <- ggplot(boruta.variable.imp, aes(x = Importance, y = Variable, fill = Decision)) + 
  geom_density_ridges(
    alpha = 0.6,
    scale = 0.9,
    color = "black",
    size = 0.3,
    rel_min_height = 0.01,
    position = "identity"
  ) + 
  theme_bw() + 
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5, margin = margin(b = 10)),
    legend.position = "right",
    panel.grid = element_line(color = "gray", alpha = 0.15, linewidth = 0.3)
  ) + 
  labs(
    x = "Importance (Gain)",
    y = "Indicator",
    title = "Boruta Feature Importance Distribution (Ridge Plot)",
    fill = "Decision"
  ) +
  scale_fill_manual(
    values = c(
      "Confirmed Important" = "#4ECDC4",  # Cyan
      "Rejected (Not Important)" = "#E74C3C",    # Red
      "Shadow Variables" = "#95A5A6"       # Gray
    ),
    labels = c("Confirmed Important", "Rejected (Not Important)", "Shadow Variables")
  )

# 9) Save plot
ggsave("boruta_ridgeplot.png", p, width = 12, height = 8, dpi = 300, bg = "white")
cat("Saved: boruta_ridgeplot.png\n")

# 10) Extract and save important variables
print("Extracting important variables...")
boruta.finalVars <- data.frame(Item = getSelectedAttributes(boruta, withTentative = FALSE), 
                               Type = "Boruta")
print("Important Variables:")
print(boruta.finalVars)
write.csv(boruta.finalVars, "boruta_selected_variables.csv", row.names = FALSE)
cat("Saved: boruta_selected_variables.csv\n")

# 11) Display plot
print(p)

print("Analysis completed!")


## Multiple Group Propensity Score Matching
library(readxl) 
library(pm3) 
library(dplyr) 
library(tableone) 
options(warn = -1) 

# 1) Read data
file_path <- "Local_K-means.xlsx" 
df <- read_excel(file_path) 

# 2) View information for kmeams3 column
print("kmeams3 Group Information:") 
print(table(df$kmeams3)) 

# 3) Prepare data
# Select necessary variables
data <- df %>% 
  select(kmeams3, Sex, TG, TC, NEU, DBIL, LDH, Death) 

# Ensure grouping variable is factor type
data$kmeams3 <- as.factor(data$kmeams3) 

# Handle missing values (remove rows with NA)
data <- data %>% na.omit() 
print(paste("Number of rows after missing value handling:", nrow(data))) 

# 4) Perform three-group matching using pm3 function (match all three groups simultaneously)
print("Performing three-group matching using pm3 function...") 
set.seed(123) 

# Perform matching using pm3 (using original 3-level variable)
result <- pm3( 
  data = data, 
  x = "kmeams3",  
  y = "Death",     
  covs = c("Sex", "TG", "TC", "NEU", "DBIL"),  
  CALIP = 0.05  
) 

# View matching results
print("Three-group Matching Results:") 
print(result) 

# Extract matched data
matched_data <- result$mbc 
print(paste("Number of rows after matching:", nrow(matched_data))) 
print("Group distribution after matching:") 
print(table(matched_data$kmeams3)) 

# Extract matched data for each group
matched_group1 <- result$fMtchDf_1 
matched_group2 <- result$fMtchDf_2 
matched_group3 <- result$fMtchDf_3 

print(paste("Number of rows for Group 1 after matching:", nrow(matched_group1))) 
print(paste("Number of rows for Group 2 after matching:", nrow(matched_group2))) 
print(paste("Number of rows for Group 3 after matching:", nrow(matched_group3))) 

# 5) Calculate Standardized Mean Differences (SMD) for all pairwise combinations
print("\nCalculating SMD after matching:") 

# Group 1 vs Group 2 
data_1_vs_2 <- matched_data %>% filter(kmeams3 %in% c(1, 2)) 
print("SMD after matching - Group 1 vs Group 2:") 
after_match_1_vs_2 <- CreateTableOne(vars = c("Sex", "TG", "TC", "NEU", "DBIL"), 
                                     strata = "kmeams3", 
                                     data = data_1_vs_2, 
                                     test = FALSE) 
print(after_match_1_vs_2, smd = TRUE) 

# Group 1 vs Group 3 
data_1_vs_3 <- matched_data %>% filter(kmeams3 %in% c(1, 3)) 
print("\nSMD after matching - Group 1 vs Group 3:") 
after_match_1_vs_3 <- CreateTableOne(vars = c("Sex", "TG", "TC", "NEU", "DBIL"), 
                                     strata = "kmeams3", 
                                     data = data_1_vs_3, 
                                     test = FALSE) 
print(after_match_1_vs_3, smd = TRUE) 

# Group 2 vs Group 3 
data_2_vs_3 <- matched_data %>% filter(kmeams3 %in% c(2, 3)) 
print("\nSMD after matching - Group 2 vs Group 3:") 
after_match_2_vs_3 <- CreateTableOne(vars = c("Sex", "TG", "TC", "NEU", "DBIL"), 
                                     strata = "kmeams3", 
                                     data = data_2_vs_3, 
                                     test = FALSE) 
print(after_match_2_vs_3, smd = TRUE) 

# 6) Compare SMD before and after matching
print("\nComparing SMD before and after matching:") 

# SMD before matching
print("SMD before matching:") 
before_match <- CreateTableOne(vars = c("Sex", "TG", "TC", "NEU", "DBIL"), 
                               strata = "kmeams3", 
                               data = data, 
                               test = FALSE) 
print(before_match, smd = TRUE) 

# SMD after matching
print("\nSMD after matching:") 
after_match_all <- CreateTableOne(vars = c("Sex", "TG", "TC", "NEU", "DBIL"), 
                                  strata = "kmeams3", 
                                  data = matched_data, 
                                  test = FALSE) 
print(after_match_all, smd = TRUE) 

# 7) Save results
# Save complete matched data
write.csv(matched_data, "matched_data_pm3_all.csv", row.names = FALSE) 
cat("Saved: matched_data_pm3_all.csv\n") 

# Save matched data for each group
write.csv(matched_group1, "matched_data_pm3_group1.csv", row.names = FALSE) 
cat("Saved: matched_data_pm3_group1.csv\n") 

write.csv(matched_group2, "matched_data_pm3_group2.csv", row.names = FALSE) 
cat("Saved: matched_data_pm3_group2.csv\n") 

write.csv(matched_group3, "matched_data_pm3_group3.csv", row.names = FALSE) 
cat("Saved: matched_data_pm3_group3.csv\n") 

print("PM3 three-group matching analysis completed!")


## Two-group Propensity Score Matching
library(readxl) 
library(MatchIt) 
library(dplyr) 
library(tableone) 
library(ggplot2) 
library(cowplot) 
options(warn = -1) 

# 1) Read data
file_path <- "Local_K-means.xlsx" 
df <- read_excel(file_path) 

# 2) View information for LMWH column
print("LMWH Variable Information:") 
print(table(df$LMWH)) 
print("Unique values in LMWH:") 
print(unique(df$LMWH)) 

# 3) Prepare data
# Select necessary variables
data <- df %>% 
  select(LMWH, Sex, TG, TC, NEU, DBIL, LDH) 

# Ensure LMWH is factor variable
data$LMWH <- as.factor(data$LMWH) 

# Handle missing values (remove rows with NA)
data <- data %>% na.omit() 
print(paste("Number of rows after missing value handling:", nrow(data))) 
print("LMWH distribution after handling:") 
print(table(data$LMWH)) 

# 4) Perform propensity score matching using MatchIt
print("\nPerforming propensity score matching using MatchIt...") 
set.seed(123) 

# Perform matching
match_result <- matchit( 
  LMWH ~ Sex + TG + TC + NEU + DBIL, 
  data = data, 
  method = "nearest", 
  ratio = 1,          
  caliper = 0.05     
) 

# View matching results
print("Matching Results Summary:") 
print(summary(match_result)) 

# Extract matched data
matched_data <- match.data(match_result) 
print(paste("Number of rows after matching:", nrow(matched_data))) 
print("LMWH distribution after matching:") 
print(table(matched_data$LMWH)) 

# 5) Calculate SMD before and after matching
print("\nCalculating SMD before and after matching:") 

# SMD before matching
before_match <- CreateTableOne(vars = c("Sex", "TG", "TC", "NEU", "DBIL"), 
                               strata = "LMWH", 
                               data = data, 
                               test = FALSE) 
print("SMD before matching:") 
print(before_match, smd = TRUE) 

# SMD after matching
after_match <- CreateTableOne(vars = c("Sex", "TG", "TC", "NEU", "DBIL"), 
                              strata = "LMWH", 
                              data = matched_data, 
                              test = FALSE) 
print("\nSMD after matching:") 
print(after_match, smd = TRUE) 

# 6) Generate SMD visualization plot
print("\nGenerating SMD visualization plot...") 

# Manually extract SMD values from tableone output
smd_data <- data.frame( 
  variable = c("Sex", "TG", "TC", "NEU", "DBIL"), 
  before = c(0.163, 0.478, 0.404, 0.057, 0.176), 
  after = c(0.087, 0.079, 0.027, 0.065, 0.105) 
) 

print("\nExtracted SMD values:") 
print(smd_data) 

# Calculate SMD reduction values
smd_data$reduction <- smd_data$before - smd_data$after 

# Create SMD comparison plot
p1 <- ggplot(smd_data, aes(x = variable)) + 
  geom_point(aes(y = before, color = "Before Matching"), size = 4) + 
  geom_point(aes(y = after, color = "After Matching"), size = 4) + 
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = 0.25, linetype = "dotted", color = "orange") + 
  labs( 
    title = "LMWH SMD Comparison Before and After Matching", 
    x = "Covariate", 
    y = "Absolute SMD Value" 
  ) + 
  theme_bw() + 
  theme( 
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.title = element_blank(), 
    legend.position = "top" 
  ) + 
  scale_color_manual(values = c("Before Matching" = "blue", "After Matching" = "green")) 

# Create SMD reduction value plot
p2 <- ggplot(smd_data, aes(x = variable, y = reduction)) + 
  geom_bar(stat = "identity", fill = "purple") + 
  labs( 
    title = "LMWH SMD Reduction After Matching", 
    x = "Covariate", 
    y = "SMD Reduction Value" 
  ) + 
  theme_bw() + 
  theme( 
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
    axis.text.x = element_text(angle = 45, hjust = 1) 
  ) 

# Save individual plots
ggsave("smd_comparison_before_after_lmwh.png", p1, width = 10, height = 6, dpi = 300) 
ggsave("smd_reduction_lmwh.png", p2, width = 10, height = 6, dpi = 300) 

# Create combined plot
combined_plot <- plot_grid(p1, p2, ncol = 1, nrow = 2, align = "v") 
ggsave("smd_comparison_lmwh.png", combined_plot, width = 10, height = 12, dpi = 300) 

# 7) Save matched data
write.csv(matched_data, "matched_data_lmwh.csv", row.names = FALSE) 

print("\nLMWH propensity score matching analysis completed!") 
print("Generated files:") 
print("- matched_data_lmwh.csv: Matched data") 
print("- smd_comparison_lmwh.png: Combined SMD comparison plot") 
print("- smd_comparison_before_after_lmwh.png: SMD comparison before and after matching") 
print("- smd_reduction_lmwh.png: SMD reduction bar chart")


# Kaplan-Meier Survival Curve
library(tidyverse) 
library(readxl) 
library(survival) 
library(survminer) 

# Read dataset 
data <- read_excel("Local_K-means.xlsx") 

# Check column names 
print("Column names in dataset:") 
print(colnames(data)) 

# Check information for kmeams3 and Death variables 
print("\nkmeams3 Variable Information:") 
print(table(data$kmeams3)) 

# Prepare data 
print("\nChecking possible survival time columns:") 
survival_columns <- c("Totalhospitalizationdays", "Daysinintensivecare", "Day7", "Day14", "Day28", "Day56") 
for (col in survival_columns) { 
  if (col %in% colnames(data)) { 
    print(paste(col, "exists in dataset")) 
    print(paste("Minimum value:", min(data[[col]], na.rm = TRUE))) 
    print(paste("Maximum value:", max(data[[col]], na.rm = TRUE))) 
  } 
} 

# Select necessary variables
surv_data <- data %>% 
  select(kmeams3, Death, Totalhospitalizationdays) 

# Ensure kmeams3 is factor type 
surv_data$kmeams3 <- as.factor(surv_data$kmeams3) 

# Handle missing values (remove rows with NA) 
surv_data <- surv_data %>% na.omit() 

# Create survival object 
surv_object <- Surv(time = surv_data$Totalhospitalizationdays, event = surv_data$Death) 

# Fit survival model 
fit <- survfit(surv_object ~ kmeams3, data = surv_data) 

# Create survival curve plot 
surv_plot <- ggsurvplot( 
  fit, 
  data = surv_data, 
  pval = TRUE, 
  pval.coord = c(0, 0.15), 
  risk.table = TRUE, 
  risk.table.col = "strata", 
  xlab = "Time (days)", 
  ylab = "Survival Probability", 
  legend.title = "kmeams3 Group", 
  legend.labs = c("Group 1", "Group 2", "Group 3"), 
  surv.median.line = "hv", 
  ggtheme = theme_minimal(), 
  palette = c("#E41A1C", "#377EB8", "#4DAF4A"), 
  risk.table.height = 0.25, 
  risk.table.y.text.col = TRUE, 
  risk.table.title = "Number at risk: n (%)", 
  font.risk.table = c(10, "plain", "black"), 
  font.legend = c(10, "plain", "black"), 
  break.time.by = 30, 
  conf.int = TRUE, 
  legend = c(0.8, 0.8) 
) 

ggsave("km_curve_kmeams3.png", surv_plot$plot, width = 12, height = 8, dpi = 300) 

pdf("km_curve_kmeams3.pdf", width = 12, height = 8) 
print(surv_plot) 
dev.off() 

print("\nCreating survival curve based on 28-day mortality...") 
surv_data_28 <- data %>% 
  select(kmeams3, Death28, Day28) 

surv_data_28$kmeams3 <- as.factor(surv_data_28$kmeams3) 
surv_data_28 <- surv_data_28 %>% na.omit() 

surv_object_28 <- Surv(time = surv_data_28$Day28, event = surv_data_28$Death28) 
fit_28 <- survfit(surv_object_28 ~ kmeams3, data = surv_data_28) 

surv_plot_28 <- ggsurvplot( 
  fit_28, 
  data = surv_data_28, 
  pval = TRUE, 
  pval.coord = c(0, 0.15), 
  risk.table = TRUE, 
  risk.table.col = "strata", 
  xlab = "Time (days)", 
  ylab = "Survival Probability", 
  legend.title = "kmeams3 Group", 
  legend.labs = c("Group 1", "Group 2", "Group 3"), 
  surv.median.line = "hv", 
  ggtheme = theme_minimal(), 
  palette = c("#E41A1C", "#377EB8", "#4DAF4A"), 
  risk.table.height = 0.25, 
  risk.table.y.text.col = TRUE, 
  risk.table.title = "Number at risk: n (%)", 
  font.risk.table = c(10, "plain", "black"), 
  font.legend = c(10, "plain", "black"), 
  break.time.by = 7, 
  conf.int = TRUE, 
  legend = c(0.8, 0.8), 
  xlim = c(0, 28) 
) 

# Save 28-day survival curve 
ggsave("km_curve_kmeams3_28days.png", surv_plot_28$plot, width = 12, height = 8, dpi = 300) 

pdf("km_curve_kmeams3_28days.pdf", width = 12, height = 8) 
print(surv_plot_28) 
dev.off() 

print("\nKM curve analysis completed!") 
print("Generated files:") 
print("- km_curve_kmeams3.pdf: KM curve PDF file") 
print("- km_curve_kmeams3.png: KM curve PNG file")

## COX Proportional Hazards Regression
# Load necessary packages
library(tidyverse)
library(readxl)
library(survival)
library(survminer)

# Read dataset
data <- read_excel("Local_K-means.xlsx") 

# View column names of dataset
print("Column names in dataset:")
print(colnames(data))

# Check information for required variables
print("\nChecking information for required variables:")
variables <- c("Death", "Totalhospitalizationdays", "Sex", "Age", "WBC", "MBP")
for (var in variables) {
  if (var %in% colnames(data)) {
    print(paste(var, "exists in dataset"))
    print(paste("Number of missing values:", sum(is.na(data[[var]]))))
    print(paste("Unique values:", paste(head(unique(data[[var]]), 10), collapse=", ")))
  } else {
    print(paste(var, "does not exist in dataset"))
  }
}

# Prepare data
# Select required variables
cox_data <- data %>% 
  select(Death, Totalhospitalizationdays, Sex, Age, WBC, MBP, kmeams3)

# Ensure kmeams3 is factor variable
cox_data$kmeams3 <- as.factor(cox_data$kmeams3)

# Handle missing values (remove rows with NA)
cox_data <- cox_data %>% na.omit()
print(paste("\nNumber of rows after data processing:", nrow(cox_data)))
print("Death distribution after processing:")
print(table(cox_data$Death))
print("kmeams3 distribution after processing:")
print(table(cox_data$kmeams3))

# Fit multivariate Cox regression model
print("\nFitting multivariate Cox regression model...")
set.seed(123)

# Base model: adjust only clinical variables
model1 <- coxph(
  Surv(Totalhospitalizationdays, Death) ~ Sex + Age + WBC + MBP,
  data = cox_data
)

# Full model: add kmeams3 grouping
model2 <- coxph(
  Surv(Totalhospitalizationdays, Death) ~ Sex + Age + WBC + MBP + kmeams3,
  data = cox_data
)

# Output model results
print("\n=== Model 1: Adjusted only for clinical variables ===")
print(summary(model1))

print("\n=== Model 2: Added kmeams3 grouping ===")
print(summary(model2))

# Extract Hazard Ratio (HR) and confidence intervals
print("\n=== Hazard Ratio (HR) and 95% Confidence Intervals ===")
model2_results <- broom::tidy(model2, exponentiate = TRUE, conf.int = TRUE)
print(model2_results[, c("term", "estimate", "conf.low", "conf.high", "p.value")])

# Plot forest plot (using simplified method)
print("\nGenerating forest plot...")
# Using simplified version of ggforest function
tryCatch({
  forest_plot <- ggforest(
    model2,
    data = cox_data,
    main = "Cox Regression Analysis: Hazard Ratios"
  )
  ggsave("cox_forest_plot.png", forest_plot, width = 12, height = 8, dpi = 300)
  cat("Saved: cox_forest_plot.png\n")
}, error = function(e) {
  print("Failed to generate forest plot, skipping this step.")
  print(paste("Error message:", e$message))
})

# Generate survival curves to show survival in different kmeams3 groups
print("\nGenerating survival curves for different kmeams3 groups...")
surv_object <- Surv(time = cox_data$Totalhospitalizationdays, event = cox_data$Death)
fit_surv <- survfit(surv_object ~ kmeams3, data = cox_data)

surv_plot <- ggsurvplot(
  fit_surv,
  data = cox_data,
  pval = TRUE,
  pval.coord = c(0, 0.15),
  risk.table = TRUE,
  risk.table.col = "strata",
  xlab = "Time (days)",
  ylab = "Survival Probability",
  legend.title = "kmeams3 Group",
  legend.labs = c("Group 1", "Group 2", "Group 3"),
  surv.median.line = "hv",
  ggtheme = theme_minimal(),
  palette = c("#E41A1C", "#377EB8", "#4DAF4A"),
  risk.table.height = 0.25
)

# Save survival curves
pdf("cox_survival_curves.pdf", width = 12, height = 8)
print(surv_plot)
dev.off()

png("cox_survival_curves.png", width = 1200, height = 800, res = 100)
print(surv_plot)
dev.off()
cat("Saved: cox_survival_curves.pdf and cox_survival_curves.png\n")

# Model diagnostics
print("\n=== Model Diagnostics ===")
# Check proportional hazards assumption
print("\nChecking proportional hazards assumption:")
cox.zph_result <- cox.zph(model2)
print(cox.zph_result)

# Plot diagnostic plots
print("\nGenerating diagnostic plots...")
diagnostic_plot <- ggcoxzph(cox.zph_result)
ggsave("cox_diagnostic_plot.png", diagnostic_plot, width = 12, height = 8, dpi = 300)

# Calculate survival probabilities
print("\n=== Survival Probability Calculation ===")
# Create new dataframe for prediction
test_data <- data.frame(
  Sex = c(0, 1),
  Age = mean(cox_data$Age),
  WBC = mean(cox_data$WBC),
  MBP = mean(cox_data$MBP),
  kmeams3 = factor(c(1, 1))
)

# Predict survival probabilities
surv_pred <- survfit(model2, newdata = test_data)
surv_pred_df <- broom::tidy(surv_pred)
print(head(surv_pred_df))

print("\nCox regression analysis completed!")
print("Generated files:")
print("- cox_forest_plot.png: Cox regression forest plot")
print("- cox_diagnostic_plot.png: Proportional hazards assumption diagnostic plot")

## XGBoost Classification Model
library(tidymodels)
library(xgboost)
library(doParallel)
library(readxl)
library(tidyverse)

# Enable parallel computing to speed up nested cross-validation
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

# Read data (modify path according to actual situation)
df <- read_excel("/training_data.xlsx", sheet = "Sheet1")

# Correct variable types
for(i in c(12)){
  df[[i]] <- factor(df[[i]])
}

# 1. Data Splitting
#################################################################
set.seed(4321)
datasplit <- initial_split(winequality, prop = 0.70, strata = group)
traindata <- training(datasplit)
testdata <- testing(datasplit)
#################################################################
# 2. Build nested cross-validation structure on [training set]
#################################################################
set.seed(4321)
nested_folds <- nested_cv(traindata,
                          outside = vfold_cv(v = 5, strata = group),
                          inside = vfold_cv(v = 5, strata = group))

# 3. Define model and workflow
#################################################################
# Data preprocessing recipe
datarecipe <- recipe(group ~ ., data = traindata) %>%
  step_dummy(all_nominal_predictors()) %>%  
  step_nzv(all_predictors()) %>%             
  step_normalize(all_numeric_predictors())   

# Set up model
model_xgboost <- boost_tree(
  mode = "classification",
  engine = "xgboost",
  mtry = tune(),
  trees = 1000,
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune(), 
  stop_iter = 25
)

# Base workflow
wk_xgboost <- workflow() %>%
  add_model(model_xgboost) %>%
  add_recipe(datarecipe)

# Set hyperparameter grid
hpset_xgboost <- extract_parameter_set_dials(model_xgboost) %>%
  update(
    mtry = mtry(range = c(2, ncol(traindata) - 1)), 
    sample_size = sample_prop(range = c(0.8, 1))    
  )

set.seed(42)
hpgrid_xgboost <- grid_random(hpset_xgboost, size = 10) 
print(hpgrid_xgboost)

# 4. Execute nested cross-validation (evaluate performance of tuning strategy)
#################################################################

# Define tuning function
tune_over_folds <- function(split, inner_resamples, workflow, grid) {
  
  # A. Perform tuning on inner folds
  tune_res <- tune_grid(
    workflow,
    resamples = inner_resamples,
    grid = grid,
    metrics = metric_set(accuracy, roc_auc),
    control = control_grid(verbose = FALSE, save_pred = FALSE)
  )
  
  # B. Select best parameters
  best_params <- select_best(tune_res, metric = "accuracy")
  
  # C. Update workflow with best parameters
  final_wk <- finalize_workflow(workflow, best_params)
  
  # D. Final fitting and evaluation
  suppressWarnings(last_fit(final_wk, split, metrics = metric_set(accuracy, roc_auc)))
}

# Use map2 to iterate over each row, perform tuning and evaluation on each outer fold
nested_results <- nested_folds %>%
  mutate(results = purrr::map2(splits, inner_resamples,
                               tune_over_folds,
                               workflow = wk_xgboost,
                               grid = hpgrid_xgboost))

# View results
nested_metrics <- nested_results %>%
  mutate(metrics = map(results, collect_metrics)) %>%
  select(id, metrics) %>%
  unnest(metrics)

print("Nested cross-validation detailed results:")
print(nested_metrics)

# Calculate average performance
average_performance <- nested_metrics %>%
  group_by(.metric) %>%
  summarise(mean_estimate = mean(.estimate),
            sd_estimate = sd(.estimate))

print("Nested cross-validation average performance:")
print(average_performance)

# 5. Train final model
#################################################################

print("Starting to train final model...")

# Global tuning
set.seed(42)
final_folds <- vfold_cv(traindata, v = 5, strata = group) 

final_tune <- tune_grid(
  wk_xgboost,
  resamples = final_folds,
  grid = hpgrid_xgboost,
  metrics = metric_set(accuracy, roc_auc)
)

best_overall <- select_best(final_tune, metric = "accuracy")
print("Final selected best parameters:")
print(best_overall)

# Train on all training data using best parameters
final_workflow <- wk_xgboost %>%
  finalize_workflow(best_overall)

# Fit on training data to get directly predictable object
final_fit_obj <- fit(final_workflow, data = traindata)

# 6. Model Prediction & Calibration Curve
#################################################################

# Predict on training set
predtrain_xgb <- predict(final_fit_obj, new_data = traindata, type = "prob") %>%
  bind_cols(predict(final_fit_obj, new_data = traindata, type = "class")) %>%
  bind_cols(traindata %>% select(group)) %>%
  mutate(dataset = "train")

# Predict on test set
predtest_xgb <- predict(final_fit_obj, new_data = testdata, type = "prob") %>%
  bind_cols(predict(final_fit_obj, new_data = testdata, type = "class")) %>%
  bind_cols(testdata %>% select(group)) %>%
  mutate(dataset = "test")

# Check prediction results
print("Test set prediction results overview:")
head(predtest_xgb)

# ----------------------------------------------------------------
# Calculate Brier score (multiclass)
calc_multiclass_brier <- function(data, truth_col, prob_cols) {
  # 1. Create one-hot encoded true label matrix
  truth <- data[[truth_col]]
  levels <- levels(truth)
  n <- nrow(data)
  k <- length(levels)
  
  Y <- matrix(0, nrow = n, ncol = k)
  colnames(Y) <- levels
  for (i in 1:k) {
    Y[truth == levels[i], i] <- 1
  }
  
  # 2. Extract predicted probability matrix
  P <- data %>% select(all_of(prob_cols)) %>% as.matrix()
  
  # Simple check if column names match
  p_names <- colnames(P)
  p_clean_names <- str_remove(p_names, ".pred_")
  
  # Reorder P if column name order is inconsistent
  if (!all(p_clean_names == levels)) {
    # Try to match
    P <- P[, paste0(".pred_", levels)]
  }
  
  # 3. Calculate Brier score
  bs <- mean(rowSums((Y - P)^2))
  return(bs)
}

# Extract probability column names
prob_cols <- names(predtrain_xgb)[str_detect(names(predtrain_xgb), "^.pred_")]

# Training set Brier score
brier_train_val <- calc_multiclass_brier(predtrain_xgb, "group", prob_cols)
print(paste("Training set Brier score:", round(brier_train_val, 4)))

# Test set Brier score
# brier_test <- brier_class_prob(predtest_xgb, group, contains(".pred_"))
brier_test_val <- calc_multiclass_brier(predtest_xgb, "group", prob_cols)
print(paste("Test set Brier score:", round(brier_test_val, 4)))


# Plot calibration curve
cal_plot_data <- predtest_xgb %>%
  select(group, starts_with(".pred_")) %>%
  pivot_longer(cols = starts_with(".pred_"),
               names_to = "class_pred",
               values_to = "probability") %>%
  # Extract class name after .pred_
  mutate(class_label = str_remove(class_pred, ".pred_")) %>%
  # Create binary label: whether current row belongs to this class
  mutate(is_class = ifelse(group == class_label, "Event Occurred", "Non-Event")) %>%
  mutate(is_class = factor(is_class, levels = c("Event Occurred", "Non-Event")))

# Manual binning calculation
cal_curve_data <- cal_plot_data %>%
  group_by(class_label) %>%
  mutate(bin = cut(probability, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE, labels = FALSE)) %>% 
  group_by(class_label, bin) %>%
  summarise(
    mean_pred = mean(probability),
    obs_rate = mean(is_class == "Event Occurred"),
    n = n(),
    .groups = "drop"
  )

# Plotting
p_cal <- ggplot(cal_curve_data, aes(x = mean_pred, y = obs_rate)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(aes(color = class_label)) +
  geom_point(aes(color = class_label)) +
  facet_wrap(~class_label) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(title = "Calibration Curve (Test Set)",
       x = "Predicted Probability",
       y = "Actual Incidence Rate",
       color = "Class") +
  theme_minimal()

print(p_cal)

# Save plot
ggsave("calibration_curve_multiclass.pdf", plot = p_cal, width = 10, height = 8)
###ROC
# Load necessary packages
library(pROC)
library(ggplot2)
library(dplyr)

# Function to calculate per-class AUC confidence intervals
calculate_per_class_auc_ci <- function(pred_data, n_boot = 1000) {
  set.seed(123)
  classes <- levels(pred_data$group)
  
  result_list <- list()
  
  for(class in classes) {
    prob_col <- paste0(".pred_", class)
    if(prob_col %in% names(pred_data)) {
      true_labels <- ifelse(pred_data$group == class, 1, 0)
      pred_probs <- pred_data[[prob_col]]
      roc_obj <- roc(true_labels, pred_probs, quiet = TRUE)
      auc_original <- auc(roc_obj)
      auc_boot <- numeric(n_boot)
      
      for(b in 1:n_boot) {
        boot_idx <- sample(1:nrow(pred_data), replace = TRUE)
        boot_true <- true_labels[boot_idx]
        boot_pred <- pred_probs[boot_idx]
        if(length(unique(boot_true)) >= 2) {  
          tryCatch({
            boot_roc <- roc(boot_true, boot_pred, quiet = TRUE)
            auc_boot[b] <- auc(boot_roc)
          }, error = function(e) {
            auc_boot[b] <- NA
          })
        } else {
          auc_boot[b] <- NA
        }
      }
      
      auc_boot <- auc_boot[!is.na(auc_boot)]
      if(length(auc_boot) > 0) {
        auc_ci <- quantile(auc_boot, c(0.025, 0.975), na.rm = TRUE)
      } else {
        auc_ci <- c(NA, NA)
      }
      
      result_list[[class]] <- list(
        class = class,
        auc = auc_original,
        ci_lower = auc_ci[1],
        ci_upper = auc_ci[2]
      )
    }
  }
  
  return(result_list)
}

# Macro-average AUC confidence interval function
calculate_macro_auc_ci <- function(pred_data, n_boot = 1000) {
  set.seed(123)
  classes <- levels(pred_data$group)
  
  macro_auc_boot <- numeric(n_boot)
  
  for(b in 1:n_boot) {
    boot_idx <- sample(1:nrow(pred_data), replace = TRUE)
    boot_data <- pred_data[boot_idx, ]
    
    class_aucs <- numeric(length(classes))
    valid_classes <- 0
    
    for(i in seq_along(classes)) {
      class <- classes[i]
      prob_col <- paste0(".pred_", class)
      if(prob_col %in% names(boot_data)) {
        true_labels <- ifelse(boot_data$group == class, 1, 0)
        pred_probs <- boot_data[[prob_col]]
        
        if(length(unique(true_labels)) >= 2) {
          tryCatch({
            roc_obj <- roc(true_labels, pred_probs, quiet = TRUE)
            class_aucs[i] <- auc(roc_obj)
            valid_classes <- valid_classes + 1
          }, error = function(e) {
            class_aucs[i] <- NA
          })
        }
      }
    }
    
    valid_aucs <- class_aucs[!is.na(class_aucs)]
    if(length(valid_aucs) > 0) {
      macro_auc_boot[b] <- mean(valid_aucs, na.rm = TRUE)
    } else {
      macro_auc_boot[b] <- NA
    }
  }
  
  macro_auc_boot <- macro_auc_boot[!is.na(macro_auc_boot)]
  if(length(macro_auc_boot) > 0) {
    macro_ci <- quantile(macro_auc_boot, c(0.025, 0.975), na.rm = TRUE)
  } else {
    macro_ci <- c(NA, NA)
  }
  
  original_aucs <- numeric(length(classes))
  for(i in seq_along(classes)) {
    class <- classes[i]
    prob_col <- paste0(".pred_", class)
    if(prob_col %in% names(pred_data)) {
      roc_obj <- roc(ifelse(pred_data$group == class, 1, 0), 
                     pred_data[[prob_col]], quiet = TRUE)
      original_aucs[i] <- auc(roc_obj)
    }
  }
  macro_auc <- mean(original_aucs, na.rm = TRUE)
  
  return(list(
    auc = macro_auc,
    ci_lower = macro_ci[1],
    ci_upper = macro_ci[2]
  ))
}

# Micro-average AUC confidence interval function
calculate_micro_auc_ci <- function(pred_data, n_boot = 1000) {
  set.seed(123)
  
  classes <- levels(pred_data$group)
  all_probs <- c()
  all_labels <- c()
  
  for(class in classes) {
    prob_col <- paste0(".pred_", class)
    if(prob_col %in% names(pred_data)) {
      all_probs <- c(all_probs, pred_data[[prob_col]])
      all_labels <- c(all_labels, ifelse(pred_data$group == class, 1, 0))
    }
  }
  
  micro_roc <- roc(all_labels, all_probs, quiet = TRUE)
  micro_auc_original <- auc(micro_roc)
  micro_auc_boot <- numeric(n_boot)
  
  for(b in 1:n_boot) {
    boot_idx <- sample(1:nrow(pred_data), replace = TRUE)
    
    boot_probs <- c()
    boot_labels <- c()
    
    for(class in classes) {
      prob_col <- paste0(".pred_", class)
      if(prob_col %in% names(pred_data)) {
        boot_probs <- c(boot_probs, pred_data[[prob_col]][boot_idx])
        boot_labels <- c(boot_labels, ifelse(pred_data$group[boot_idx] == class, 1, 0))
      }
    }
    
    if(length(unique(boot_labels)) >= 2) {
      tryCatch({
        boot_roc <- roc(boot_labels, boot_probs, quiet = TRUE)
        micro_auc_boot[b] <- auc(boot_roc)
      }, error = function(e) {
        micro_auc_boot[b] <- NA
      })
    } else {
      micro_auc_boot[b] <- NA
    }
  }
  
  micro_auc_boot <- micro_auc_boot[!is.na(micro_auc_boot)]
  if(length(micro_auc_boot) > 0) {
    micro_ci <- quantile(micro_auc_boot, c(0.025, 0.975), na.rm = TRUE)
  } else {
    micro_ci <- c(NA, NA)
  }
  
  return(list(
    auc = micro_auc_original,
    ci_lower = micro_ci[1],
    ci_upper = micro_ci[2]
  ))
}

create_complete_roc_data <- function(pred_data) {
  cat("Generating corrected ROC data...\n")
  
  classes <- levels(pred_data$group)
  roc_data <- data.frame()
  auc_summary <- data.frame()
  
  # 1. Calculate ROC curves and AUC confidence intervals for each class
  cat("Calculating AUC and confidence intervals for each class...\n")
  per_class_aucs <- calculate_per_class_auc_ci(pred_data)
  
  for(result in per_class_aucs) {
    class <- result$class
    prob_col <- paste0(".pred_", class)
    
    if(prob_col %in% names(pred_data)) {
      roc_obj <- roc(ifelse(pred_data$group == class, 1, 0), 
                     pred_data[[prob_col]], quiet = TRUE)
      roc_coords <- coords(roc_obj, transpose = FALSE, ret = c("specificity", "sensitivity"))
      
      roc_df <- data.frame(
        specificity = roc_coords$specificity,
        sensitivity = roc_coords$sensitivity,
        class = class,
        type = "Per-Class"
      )
      roc_data <- rbind(roc_data, roc_df)
      
      # Save AUC information
      auc_summary <- rbind(auc_summary, data.frame(
        Class = class,
        AUC = result$auc,
        AUC_CI_lower = result$ci_lower,
        AUC_CI_upper = result$ci_upper,
        Type = "Per-Class",
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # 2. Calculate macro-average AUC
  cat("Calculating macro-average AUC and confidence intervals...\n")
  macro_result <- calculate_macro_auc_ci(pred_data)
  
  # Add macro-average ROC curve
  fpr_seq <- seq(0, 1, 0.01)
  macro_tpr <- numeric(length(fpr_seq))
  
  for(i in seq_along(fpr_seq)) {
    fpr <- fpr_seq[i]
    tpr_values <- numeric(length(classes))
    
    for(j in seq_along(classes)) {
      class <- classes[j]
      prob_col <- paste0(".pred_", class)
      if(prob_col %in% names(pred_data)) {
        roc_obj <- roc(ifelse(pred_data$group == class, 1, 0), 
                       pred_data[[prob_col]], quiet = TRUE)
        coords_df <- coords(roc_obj, transpose = FALSE, ret = c("specificity", "sensitivity"))
        current_fprs <- 1 - coords_df$specificity
        if(length(current_fprs) > 0) {
          closest_idx <- which.min(abs(current_fprs - fpr))
          if(length(closest_idx) > 0) {
            tpr_values[j] <- coords_df$sensitivity[closest_idx[1]]
          }
        }
      }
    }
    macro_tpr[i] <- mean(tpr_values, na.rm = TRUE)
  }
  
  macro_roc_df <- data.frame(
    specificity = 1 - fpr_seq,
    sensitivity = macro_tpr,
    class = "Macro Average",
    type = "Average",
    stringsAsFactors = FALSE
  )
  roc_data <- rbind(roc_data, macro_roc_df)
  
  auc_summary <- rbind(auc_summary, data.frame(
    Class = "Macro Average",
    AUC = macro_result$auc,
    AUC_CI_lower = macro_result$ci_lower,
    AUC_CI_upper = macro_result$ci_upper,
    Type = "Average",
    stringsAsFactors = FALSE
  ))
  
  # 3. Calculate micro-average AUC
  cat("Calculating micro-average AUC and confidence intervals...\n")
  micro_result <- calculate_micro_auc_ci(pred_data)
  
  # Micro-average ROC curve
  all_probs <- c()
  all_labels <- c()
  
  for(class in classes) {
    prob_col <- paste0(".pred_", class)
    if(prob_col %in% names(pred_data)) {
      all_probs <- c(all_probs, pred_data[[prob_col]])
      all_labels <- c(all_labels, ifelse(pred_data$group == class, 1, 0))
    }
  }
  
  if(length(all_probs) > 0 && length(all_labels) > 0) {
    micro_roc <- roc(all_labels, all_probs, quiet = TRUE)
    micro_coords <- coords(micro_roc, transpose = FALSE, ret = c("specificity", "sensitivity"))
    
    micro_roc_df <- data.frame(
      specificity = micro_coords$specificity,
      sensitivity = micro_coords$sensitivity,
      class = "Micro Average",
      type = "Average",
      stringsAsFactors = FALSE
    )
    roc_data <- rbind(roc_data, micro_roc_df)
    
    auc_summary <- rbind(auc_summary, data.frame(
      Class = "Micro Average",
      AUC = micro_result$auc,
      AUC_CI_lower = micro_result$ci_lower,
      AUC_CI_upper = micro_result$ci_upper,
      Type = "Average",
      stringsAsFactors = FALSE
    ))
  }
  
  return(list(roc_data = roc_data, auc_summary = auc_summary))
}

# Plot ROC curves
plot_enhanced_roc <- function(roc_data, title) {
  auc_labels <- roc_data$auc_summary %>%
    mutate(
      display_label = case_when(
        Type == "Per-Class" ~ sprintf("%s: %.3f (%.3f-%.3f)", Class, AUC, AUC_CI_lower, AUC_CI_upper),
        Type == "Average" ~ sprintf("%s: %.3f (%.3f-%.3f)", Class, AUC, AUC_CI_lower, AUC_CI_upper)
      )
    )
  
  # Set line types and colors
  n_classes <- sum(roc_data$auc_summary$Type == "Per-Class")
  line_types <- c(rep("solid", n_classes), "dashed", "dotted")
  color_palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", 
                     "#A65628", "#F781BF", "#999999", "#66C2A5")[1:nrow(roc_data$auc_summary)]
  
  # Plot ROC curves
  p <- ggplot(roc_data$roc_data, aes(x = 1 - specificity, y = sensitivity, 
                                     color = class, linetype = class)) +
    geom_line(linewidth = 1.2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
                color = "gray50", linewidth = 0.6) +
    labs(
      title = title,
      x = "1 - Specificity (False Positive Rate)",
      y = "Sensitivity (True Positive Rate)",
      color = "Class (AUC with 95% CI)",
      linetype = "Class (AUC with 95% CI)"
    ) +
    theme_bw() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      legend.key.width = unit(1.5, "cm")
    ) +
    scale_color_manual(values = color_palette,
                       labels = setNames(auc_labels$display_label, auc_labels$Class)) +
    scale_linetype_manual(values = line_types,
                          labels = setNames(auc_labels$display_label, auc_labels$Class)) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0.02)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0.02))
  
  return(p)
}