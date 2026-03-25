# 0. Load necessary R packages
# ------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(tableone)       
library(survival)      
library(survminer)      
library(rms)            
library(pROC)           
library(Boruta)         
library(ggridges)       
library(reshape2)       
library(lcmm)      
library(glmnet)      
library(JMbayes2)     
library(nlme)         
library(patchwork)     

# ------------------------------------------------------------------------------
# 1. Data loading and preprocessing
# ------------------------------------------------------------------------------
df_surv <- read_excel("data.xlsx")
df_long <- read_excel("long_data.xlsx")
df_external <- read_excel("mimic_iv_data.xlsx")

# ------------------------------------------------------------------------------
# 1.1 Comparison of continuous and categorical variables (Table 1)
# ------------------------------------------------------------------------------

exclude_vars <- c("id", "HospitalDays")
all_vars <- setdiff(colnames(df_surv), exclude_vars)

cat_vars <- sapply(df_surv[all_vars], function(x) {
  is.character(x) || is.factor(x) || length(unique(na.omit(x))) < 10
})
cat_vars <- names(cat_vars)[cat_vars]
cat_vars <- setdiff(cat_vars, "Mortality") # exclude outcome variable itself

cont_vars <- setdiff(all_vars, c(cat_vars, "Mortality"))

# Create baseline table
table1 <- CreateTableOne(vars = c(cont_vars, cat_vars), 
                         strata = "Mortality", 
                         data = df_surv, 
                         factorVars = cat_vars)

# Print baseline table
print(table1, 
      nonnormal = cont_vars, 
      exact = cat_vars, 
      smd = FALSE, 
      showAllLevels = TRUE,
      quote = FALSE)

# ------------------------------------------------------------------------------
# 1.2 Multivariate Cox proportional hazards regression model
# ------------------------------------------------------------------------------

surv_obj <- Surv(time = df_surv$HospitalDays, event = df_surv$Mortality)

# Model 1
cox_model1 <- coxph(surv_obj ~ CumSOFA, data = df_surv)

# Model 2
cox_model2 <- coxph(surv_obj ~ CumSOFA + Sex + Age + Weight + HeartRate, data = df_surv)

# Model 3
cox_model3 <- coxph(surv_obj ~ CumSOFA + Sex + Age + Weight + HeartRate + Albumin + Calcium + LDH + BUN, data = df_surv)

# Extract HR and 95% CI
model3_results <- broom::tidy(cox_model3, exponentiate = TRUE, conf.int = TRUE)
print(model3_results[, c("term", "estimate", "conf.low", "conf.high", "p.value")])

# Test proportional hazards assumption 
cox_zph_res <- cox.zph(cox_model3)
print(cox_zph_res)
ggcoxzph(cox_zph_res)

# ------------------------------------------------------------------------------
# 1.3 Restricted cubic spline (RCS) dose-response relationship

dd <- datadist(df_surv)
options(datadist = "dd")

# Adjusted covariates
fit_rcs <- cph(surv_obj ~ rcs(CumSOFA, 4) + Sex + Age + Weight + HeartRate + Albumin + Calcium + LDH + BUN, 
               data = df_surv, x = TRUE, y = TRUE)

# Predict and plot dose-response curve
pred_rcs <- Predict(fit_rcs, CumSOFA, fun = exp, ref.zero = TRUE) 
plot(pred_rcs, 
     xlab = "CumSOFA Score", 
     ylab = "Hazard Ratio (HR) for In-hospital Mortality", 
     main = "Dose-Response Relationship (Adjusted by Model 3)")

# ------------------------------------------------------------------------------
# 1.4 ROC curve analysis
# Calculate ROC and AUC 95% CI
roc_cumsofa <- roc(df_surv$Mortality, df_surv$CumSOFA, ci = TRUE, quiet = TRUE)
roc_sofa <- roc(df_surv$Mortality, df_surv$SOFA, ci = TRUE, quiet = TRUE)

# Find optimal cutoff (Youden index)
best_cutoff <- coords(roc_cumsofa, "best", ret = c("threshold", "specificity", "sensitivity"), best.method = "youden")
print(best_cutoff)

# DeLong test comparison
delong_test <- roc.test(roc_cumsofa, roc_sofa, method = "delong")
print(delong_test)

# Plot ROC curves
ggroc(list("CumSOFA" = roc_cumsofa, "Daily SOFA" = roc_sofa), legacy.axes = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  theme_minimal() +
  labs(title = "ROC Curves for In-hospital Mortality", x = "1 - Specificity", y = "Sensitivity")

# ------------------------------------------------------------------------------
# 1.5 Boruta feature selection algorithm
# ------------------------------------------------------------------------------

# Automatically use all potential predictors 
exclude_for_boruta <- c("id", "HospitalDays", "Mortality", "TrajectoryGroup")
boruta_vars <- setdiff(colnames(df_surv), exclude_for_boruta)

df_boruta <- df_surv %>% select(Mortality, all_of(boruta_vars)) %>% na.omit()

set.seed(123)
boruta_result <- Boruta(x = df_boruta %>% select(-Mortality), 
                        y = as.factor(df_boruta$Mortality), 
                        pValue = 0.01, mcAdj = TRUE, maxRuns = 100)

boruta.imp <- function(x) {
  imp <- reshape2::melt(x$ImpHistory, na.rm = TRUE)[, -1]
  colnames(imp) <- c("Variable", "Importance")
  imp <- imp[is.finite(imp$Importance), ]
  variableGrp <- data.frame(Variable = names(x$finalDecision), finalDecision = x$finalDecision)
  showGrp <- data.frame(Variable = c("shadowMax", "shadowMean", "shadowMin"), 
                        finalDecision = c("shadowMax", "shadowMean", "shadowMin"))
  variableGrp <- rbind(variableGrp, showGrp)
  boruta.variable.imp <- merge(imp, variableGrp, all.x = TRUE)
  sortedVariable <- boruta.variable.imp %>% group_by(Variable) %>% 
    summarise(median = median(Importance)) %>% arrange(median)
  boruta.variable.imp$Variable <- factor(boruta.variable.imp$Variable, levels = sortedVariable$Variable)
  return(boruta.variable.imp)
}

boruta_df <- boruta.imp(boruta_result) %>%
  mutate(Decision = case_when(
    finalDecision %in% c("shadowMax", "shadowMean", "shadowMin") ~ "Shadow Variables",
    finalDecision == "Confirmed" ~ "Confirmed Important",
    finalDecision == "Rejected" ~ "Rejected (Not Important)",
    TRUE ~ as.character(finalDecision)
  ))

p_boruta <- ggplot(boruta_df, aes(x = Importance, y = Variable, fill = Decision)) + 
  geom_density_ridges(alpha = 0.6, scale = 0.9, color = "black", size = 0.3) + 
  theme_bw() + 
  labs(title = "Boruta Feature Importance Distribution", x = "Importance (Gain)", y = "Variables") +
  scale_fill_manual(values = c("Confirmed Important" = "#4ECDC4", 
                               "Rejected (Not Important)" = "#E74C3C", 
                               "Shadow Variables" = "#95A5A6"))
print(p_boruta)



# ------------------------------------------------------------------------------
# 2.1 Latent class growth trajectory model (LCGM)
# ------------------------------------------------------------------------------

set.seed(123)
m1 <- hlme(SOFA2 ~ time, random = ~ time, subject = "id", ng = 1, data = df_long)
m2 <- hlme(SOFA2 ~ time, random = ~ time, subject = "id", ng = 2, data = df_long, mixture = ~ time, B = m1)
m3 <- hlme(SOFA2 ~ time, random = ~ time, subject = "id", ng = 3, data = df_long, mixture = ~ time, B = m1)
m4 <- hlme(SOFA2 ~ time, random = ~ time, subject = "id", ng = 4, data = df_long, mixture = ~ time, B = m1)
m5 <- hlme(SOFA2 ~ time, random = ~ time, subject = "id", ng = 5, data = df_long, mixture = ~ time, B = m1)
m6 <- hlme(SOFA2 ~ time, random = ~ time, subject = "id", ng = 6, data = df_long, mixture = ~ time, B = m1)

lcmm_summary <- summarytable(m1, m2, m3, m4, m5, m6)
print(lcmm_summary)

print(m4$pprob) 
print(postprob(m4)) 

best_lcgm <- m4

group_results <- data.frame(id = best_lcgm$pprob$id, 
                            TrajectoryGroup = as.factor(best_lcgm$pprob$class))
df_surv <- df_surv %>% left_join(group_results, by = "id")

plot(best_lcgm, which = "fit", var.time = "time", bty = "l", 
     ylab = "SOFA-2 Score", xlab = "Time (days)", main = "Latent Class Growth Trajectories")

# ------------------------------------------------------------------------------
# 2.2 Kaplan-Meier survival curves
#---------------------------------
surv_obj <- Surv(time = df_surv$HospitalDays, event = df_surv$Mortality)

fit_km_traj <- survfit(surv_obj ~ TrajectoryGroup, data = df_surv)
p_km_traj <- ggsurvplot(fit_km_traj, 
                        data = df_surv, 
                        pval = TRUE, 
                        risk.table = TRUE, 
                        xlab = "Time (days)", 
                        ylab = "Survival Probability",
                        title = "Kaplan-Meier Survival Curve by LCGM Trajectory Groups")
print(p_km_traj)

# ------------------------------------------------------------------------------
# 2.3 LASSO regression to select baseline covariates
# Automatically extract all available covariates for screening
exclude_for_lasso <- c("id", "HospitalDays", "Mortality", "TrajectoryGroup")
lasso_vars <- setdiff(colnames(df_surv), exclude_for_lasso)

# Prepare LASSO input data 
df_lasso <- df_surv %>% select(HospitalDays, Mortality, all_of(lasso_vars)) %>% na.omit()
x_lasso <- as.matrix(df_lasso %>% select(-HospitalDays, -Mortality))
y_lasso <- Surv(df_lasso$HospitalDays, df_lasso$Mortality)

cv_lasso <- cv.glmnet(x_lasso, y_lasso, family = "cox", alpha = 1)
plot(cv_lasso)

# Extract lambda.1se 
lasso_coefs <- coef(cv_lasso, s = "lambda.1se")
lasso_selected_vars <- rownames(lasso_coefs)[which(lasso_coefs != 0)]
print("LASSO Selected Variables (based on lambda.1se):")
print(lasso_selected_vars)

# ------------------------------------------------------------------------------
# 2.4 Data split: training set (70%) and internal validation set (30%)
# ------------------------------------------------------------------------------

set.seed(123)
train_idx <- sample(1:nrow(df_surv), size = round(0.7 * nrow(df_surv)))

df_train <- df_surv[train_idx, ]
df_internal <- df_surv[-train_idx, ]

df_long_train <- df_long %>% filter(id %in% df_train$id)
df_long_internal <- df_long %>% filter(id %in% df_internal$id)

# ------------------------------------------------------------------------------
# 2.5 Build joint dynamic prediction model (JMbayes2)

lme_fit <- lme(SOFA2 ~ ns(time, 2), 
               random = ~ ns(time, 2) | id, 
               data = df_long_train,
               control = lmeControl(opt = "optim"))

cox_fit <- coxph(Surv(HospitalDays, Mortality) ~ Age + LDH + BUN + Baseline_SOFA2, 
                 data = df_train, model = TRUE)

joint_fit <- jm(cox_fit, list(lme_fit), time_var = "time",
                functional_forms = list("SOFA2" = ~ value(SOFA2)),
                n_iter = 15000, n_burnin = 3000) # Adjust iterations for computational speed

print(summary(joint_fit))


# ------------------------------------------------------------------------------
# 2.6 Dynamic prediction performance

landmarks <- c(1, 2, 3, 5, 7)
horizons <- 90 

# 100 Bootstrap iterations
run_full_bootstrap_auc <- function(surv_data, long_data, landmarks, target_time, B = 100) {
  # Store all bootstrap results
  boot_results <- data.frame(Landmark = numeric(), AUC = numeric(), Boot_Iter = numeric())
  
  # Get list of unique IDs in training set for resampling
  unique_ids <- unique(surv_data$id)
  
  for (b in 1:B) {
    cat(sprintf("Running Bootstrap Iteration %d / %d...\n", b, B))
    
    # 1. Sample IDs with replacement
    boot_ids <- sample(unique_ids, replace = TRUE)
    
    # 2. Reconstruct survival and longitudinal datasets
    boot_surv_list <- lapply(1:length(boot_ids), function(i) {
      df <- surv_data[surv_data$id == boot_ids[i], ]
      df$id <- paste0("boot_", i) 
      return(df)
    })
    boot_surv <- do.call(rbind, boot_surv_list)
    
    boot_long_list <- lapply(1:length(boot_ids), function(i) {
      df <- long_data[long_data$id == boot_ids[i], ]
      df$id <- paste0("boot_", i)
      return(df)
    })
    boot_long <- do.call(rbind, boot_long_list)
    
    # 3. Refit submodels
    tryCatch({
      # Cox submodel
      boot_cox <- coxph(Surv(HospitalDays, Mortality) ~ Age + LDH + BUN + Baseline_SOFA2, 
                        data = boot_surv, model = TRUE)
      
      # LME submodel
      boot_lme <- lme(SOFA2 ~ ns(time, 2), 
                      random = ~ ns(time, 2) | id, 
                      data = boot_long,
                      control = lmeControl(opt = "optim"))
      
      # Joint model
      boot_jm <- jm(boot_cox, list(boot_lme), time_var = "time",
                    functional_forms = list("SOFA2" = ~ value(SOFA2)),
                    n_iter = 5000, n_burnin = 1000)
      
      # 4. Calculate AUC for each landmark
      for (lm in landmarks) {
        roc_res <- tvROC(boot_jm, newdata = boot_long, Tstart = lm, Thoriz = target_time, cores = 1)
        auc_val <- tvAUC(roc_res)$auc
        boot_results <- rbind(boot_results, data.frame(Landmark = lm, AUC = auc_val, Boot_Iter = b))
      }
      
    }, error = function(e) {
      cat("Bootstrap Iteration", b, "failed:", e$message, "\n")
    })
  }
  
  # Summarize bootstrap results
  summary_results <- boot_results %>%
    group_by(Landmark) %>%
    summarise(
      Mean_AUC = mean(AUC, na.rm = TRUE),
      CI_Lower = quantile(AUC, 0.025, na.rm = TRUE),
      CI_Upper = quantile(AUC, 0.975, na.rm = TRUE)
    )
  
  return(list(summary = summary_results, raw_data = boot_results))
}

# Run 100-resample bootstrap validation
print("Starting 100-resample Bootstrap Validation for Training Set...")
boot_auc_train <- run_full_bootstrap_auc(df_train, df_long_train, landmarks, horizons, B = 100)
print("Training Set Bootstrap AUC Summary:")
print(boot_auc_train$summary)

print("Starting 100-resample Bootstrap Validation for Internal Validation Set...")
boot_auc_internal <- run_full_bootstrap_auc(df_internal, df_long_internal, landmarks, horizons, B = 100)
print("Internal Validation Set Bootstrap AUC Summary:")
print(boot_auc_internal$summary)

# ------------------------------------------------------------------------------
# 2.7 Dynamic calibration curve (Binning Method)
# ------------------------------------------------------------------------------

pred_data_lm3 <- df_long_internal %>% filter(time <= 3)
surv_data_lm3 <- df_internal %>% filter(HospitalDays > 3)

valid_ids <- intersect(pred_data_lm3$id, surv_data_lm3$id)
pred_data_lm3 <- pred_data_lm3 %>% filter(id %in% valid_ids)
surv_data_lm3 <- surv_data_lm3 %>% filter(id %in% valid_ids)

# Predict risk probabilities
preds_internal <- predict(joint_fit, newdata = pred_data_lm3, process = "event", 
                          times = horizons, return_newdata = FALSE)

# Merge actual outcomes and perform binning calibration
if (!is.null(preds_internal)) {
  # Extract predictions at the target time
  idx_target <- which(preds_internal$times == horizons)
  pred_probs <- preds_internal$pred[idx_target]
  pred_ids <- preds_internal$id[idx_target]
  
  # Obtain actual survival status
  actual_outcome <- ifelse(surv_data_lm3$HospitalDays <= horizons & surv_data_lm3$Mortality == 1, 1, 0)
  names(actual_outcome) <- surv_data_lm3$id
  obs_probs <- actual_outcome[match(pred_ids, names(actual_outcome))]
  
  # Create calibration data frame
  cal_df <- data.frame(pred = pred_probs, obs = obs_probs) %>% na.omit()
  
  # Binning method
  cal_df$bin <- cut(cal_df$pred, breaks = quantile(cal_df$pred, probs = seq(0, 1, 0.2)), include.lowest = TRUE)
  cal_summary <- cal_df %>% group_by(bin) %>% 
    summarise(mean_pred = mean(pred), mean_obs = mean(obs), se = sqrt(mean_obs*(1-mean_obs)/n()))
  
  # Plot calibration curve
  p_cal <- ggplot(cal_summary, aes(x = mean_pred, y = mean_obs)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = mean_obs - 1.96*se, ymax = mean_obs + 1.96*se), width = 0.02) +
    geom_point(color = "blue", size = 3) +
    geom_line(color = "blue") +
    scale_x_continuous(limits = c(0, 1)) + scale_y_continuous(limits = c(0, 1)) +
    labs(title = "Calibration Curve (Binning Method)", x = "Predicted Probability", y = "Observed Probability") +
    theme_classic()
  print(p_cal)
}

# ------------------------------------------------------------------------------
# 2.8 Dynamic risk stratification (Risk Stratification & KM Curve)
# ------------------------------------------------------------------------------

if (exists("cal_df")) {
  roc_pred <- roc(cal_df$obs, cal_df$pred, quiet = TRUE)
  best_coords <- coords(roc_pred, "best", ret = "threshold", best.method = "youden")
  best_cutoff <- as.numeric(best_coords)
  
  print(paste("Optimal Cut-off based on Youden Index:", best_cutoff))
  
  cal_df$RiskGroup <- factor(ifelse(cal_df$pred > best_cutoff, "High Risk", "Low Risk"), 
                             levels = c("Low Risk", "High Risk"))
  cal_df$HospitalDays <- surv_data_lm3$HospitalDays[match(pred_ids, surv_data_lm3$id)]
  cal_df$Mortality <- surv_data_lm3$Mortality[match(pred_ids, surv_data_lm3$id)]
  
  fit_risk_km <- survfit(Surv(HospitalDays, Mortality) ~ RiskGroup, data = cal_df)
  p_risk_km <- ggsurvplot(fit_risk_km, data = cal_df, pval = TRUE, risk.table = TRUE,
                          title = "90-Day Mortality Risk Stratification (Predicted from Day 3)")
  print(p_risk_km)
}

# ------------------------------------------------------------------------------
# 2.9 Individualized dynamic risk visualization (Individualized Dynamic Mortality Risk Plots)
# ------------------------------------------------------------------------------

patient_id_example <- sample(df_long_train$id, 1) # randomly select a patient
df_patient <- df_long_train %>% filter(id == patient_id_example & time <= 7)

if (nrow(df_patient) > 0) {
  # Generate future prediction time grid
  t_grid <- seq(max(df_patient$time), horizons, length.out = 50)
  
  # Predict risk
  pred_patient_risk <- predict(joint_fit, newdata = df_patient, process = "event", 
                               times = t_grid, return_newdata = TRUE)
  
  # Plot individualized risk plot
  plot(pred_patient_risk, main = paste("Dynamic Risk Prediction for Patient", patient_id_example))
}