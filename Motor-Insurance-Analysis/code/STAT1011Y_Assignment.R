#Student ID:2519242
cat("Seed:", 519242, "\n")

#loading datasets
data1 <- read.csv("umu_motor_portfolio_2025.csv", header = TRUE)
data2 <- read.csv("umu_daily_claim_counts_2025.csv", header = TRUE)

cat("TOKEN-A:", sum(data1$policy_id * (data1$past_claims_count + 1)) %% 100000, "\n")
cat("TOKEN-B:", sum(data2$claim_count[data2$day %in% c(7,77,177)]) * 100 + sum(data2$claim_count == 0), "\n")

# PROBLEM 1: RISK PROFILING AND DIMENSION REDUCTION
# File: umu_motor_portfolio_2025.csv

cat("\n============================================================\n")
cat("START OF PROBLEM 1 ANALYSIS\n")
cat("============================================================\n")

View(data1)
str(data1)
names(data1)

#Installing Packages
#Load required libraries
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("FactoMineR")
library(FactoMineR)
install.packages("factoextra")
library(factoextra)
install.packages("corrplot")
library(corrplot)
install.packages("gridExtra")
library(gridExtra)
install.packages("psych")
library(psych)
install.packages("naniar")
library(naniar)
install.packages("dplyr")
library(dplyr)
install.packages("ggpubr")
library(ggpubr)
install.packages("nFactors")
library(nFactors)

# Display summary of data
summary(data1)
# Check for missing values
na.counts<-colSums(is.na(data1))
print(na.counts)

pca_vars <- c("age", "vehicle_age", "premium", "no_claims_discount","past_claims_count", "past_total_paid", "annual_mileage", "engine_power_kw")
cat("Variables selected for PCA:\n")
cat(paste(pca_vars, collapse = ", "), "\n\n")

# Create dataset for PCA
pca_data <- data1 %>%
  select(all_of(pca_vars))


# Impute missing values
pca_data_imputed <- pca_data
imputation_summary <- data.frame()

for (var in pca_vars) {
  missing_count <- sum(is.na(pca_data_imputed[[var]]))
  if (missing_count > 0) {
    median_val <- median(pca_data_imputed[[var]], na.rm = TRUE)
    pca_data_imputed[[var]][is.na(pca_data_imputed[[var]])] <- median_val
    imputation_summary <- rbind(imputation_summary,
                                data.frame(Variable = var,
                                           Missing_Count = missing_count,
                                           Imputation_Value = median_val,
                                           Imputation_Type = "Median"))
  }
}

if (nrow(imputation_summary) > 0) {
  cat("\nIMPUTATION SUMMARY:\n")
  print(imputation_summary)
} else {
  cat("\nNo missing values in selected variables.\n")
}

# Verify no missing values remain
cat("\nMissing values after imputation:", sum(is.na(pca_data_imputed)), "\n")


#Plot 1: Boxplots for visual reprensentation of outliers
par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))
boxplot(data1$age, main="Age", xlab="Years", col="lightblue", horizontal=TRUE)
boxplot(data1$vehicle_age, main="Vehicle Age", xlab="Years", col="lightgreen", horizontal=TRUE)
boxplot(data1$premium, main="Premium", xlab="MUR", col="lightpink", horizontal=TRUE)
boxplot(data1$no_claims_discount, main="NCD", xlab="%", col="yellow", horizontal=TRUE)
boxplot(data1$past_claims_count, main="Past Claims", xlab="Count", col="navajowhite", horizontal=TRUE)
boxplot(data1$past_total_paid, main="Past Total Paid", xlab="MUR", col="plum", horizontal=TRUE)
boxplot(data1$annual_mileage, main="Annual Mileage", xlab="km/year", col="blue", horizontal=TRUE)
boxplot(data1$claim_severity, main="Claim Severity", xlab="MUR/Claim", col="salmon", horizontal=TRUE)
boxplot(data1$engine_power_kw, main="Engine power", xlab="kW", col="firebrick", horizontal=TRUE)

# Standardize the data
pca_data_scaled <- scale(pca_data_imputed)
summary(pca_data_scaled)

#Plot 2: Correlation Heatmap
corr_matrix<- cor(pca_data_scaled)
par(mfrow = c(1, 1))
corrplot(corr_matrix, method = "color", type = "upper",tl.col = "black", tl.cex = 0.8, title = "Correlation Heatmap")

# Matrix of correlated data
round((corr_matrix),3)

#PRINCIPAL COMPONENT ANALYSIS
cat("\n\nPRINCIPAL COMPONENT ANALYSIS\n")

# Perform PCA
pca_result <- prcomp(pca_data_scaled, center = TRUE, scale. = TRUE)

# Summary of PCA
cat("PCA SUMMARY:\n")
print(summary(pca_result))

# Extract eigenvalues
eigenvalues <- pca_result$sdev^2
cat("\nEIGENVALUES:\n")
for (i in 1:length(eigenvalues)) {
  cat(sprintf("PC%d: %.3f\n", i, eigenvalues[i]))
}

#Plot 3: Scree Plot for PCA analysis
fviz_eig(pca_result,xlab="Principal Component",ylab="Percentage of explained variance",main="Scree Plot", addlabels = TRUE,ylim = c(0, 40), linecolor ="darkred") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


# Cumulative variance
cum_var <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
cat("\nCUMULATIVE VARIANCE EXPLAINED:\n")
for (i in 1:length(cum_var)) {
  cat(sprintf("PC1 to PC%d: %.1f%%\n", i, cum_var[i] * 100))
}

# Decide on number of components (Kaiser Criterion)
n_components <- 3
cat(sprintf("\nDECISION: Retaining %d principal components\n", n_components))
cat(sprintf("Variance explained by first %d PCs: %.1f%%\n", 
            n_components, cum_var[n_components] * 100))

#COMPONENT INTERPRETATION
cat("\n\nCOMPONENT INTERPRETATION\n")

# Loadings for first 3 components
loadings <- pca_result$rotation[, 1:n_components]
colnames(loadings) <- paste0("PC", 1:n_components)

cat("VARIABLE LOADINGS (First 3 Principal Components):\n")
print(round(loadings, 3))

#Plot 4: Contribution of variable for PC1
fviz_contrib(pca_result, choice = "var", axes = 1, top = 6)+
  theme_minimal() + # Use minimal theme
  theme(panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank(),, # Remove minor grid lines
        axis.line = element_line(color = "black"), # Add X and Y axis lines
        axis.ticks = element_line(color = "black"), # Add axis ticks
        axis.text = element_text(color = "black"), # Ensure axis text is visible
        axis.title = element_text(face = "bold")) # Make axis titles bold


# PLOT 5: PC1 vs PC2 biplot
biplot_1 <- fviz_pca_var(pca_result, 
                         axes = c(1, 2),
                         col.var = "contrib",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE,
                         title = "Variables - PCA (PC1 vs PC2)") +
  xlab("PC1: Claims & Discount (25.8%)") +
  ylab("PC2: Power & Usage (17.2%)") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
print(biplot_1)


# PLOT 6: PC1 vs PC3 biplot
biplot_2 <- fviz_pca_var(pca_result, 
                         axes = c(1, 3),
                         col.var = "contrib",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE,
                         title = "Variables - PCA (PC1 vs PC3)") +
  xlab("PC1: Claims & Discount (25.8%)") +
  ylab("PC3: Age & Value (12.3%)") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
print(biplot_2)

#ROBUSTNESS CHECK (EXCLUDING PREMIUM)
cat("\n\nROBUSTNESS CHECK (EXCLUDING PREMIUM)\n")

# Select variables excluding premium
pca_vars_no_premium <- setdiff(pca_vars, "premium")
cat("Variables for PCA without premium:\n")
cat(paste(pca_vars_no_premium, collapse = ", "), "\n\n")

pca_data_no_premium <- data1 %>%
  select(all_of(pca_vars_no_premium))

# Impute missing values
for (var in pca_vars_no_premium) {
  if (any(is.na(pca_data_no_premium[[var]]))) {
    median_val <- median(pca_data_no_premium[[var]], na.rm = TRUE)
    pca_data_no_premium[[var]][is.na(pca_data_no_premium[[var]])] <- median_val
  }
}

# Standardize
pca_data_no_premium_scaled <- scale(pca_data_no_premium)

# Perform PCA without premium
pca_result_no_premium <- prcomp(pca_data_no_premium_scaled, 
                                center = TRUE, scale. = TRUE)

cat("PCA SUMMARY (without premium):\n")
print(summary(pca_result_no_premium))


# Compare variance explained
cum_var_no_premium <- cumsum(pca_result_no_premium$sdev^2 / 
                               sum(pca_result_no_premium$sdev^2))
cat(sprintf("\nVARIANCE EXPLAINED COMPARISON (first 3 PCs):\n"))
cat(sprintf("With premium: %.1f%%\n", cum_var[3] * 100))
cat(sprintf("Without premium: %.1f%%\n", cum_var_no_premium[3] * 100))

cat("\nCONCLUSION: Component structure remains largely stable without premium.\n")
cat("PC1 still captures claims history vs. discount\n")
cat("PC2 still represents vehicle power and usage\n")
cat("PC3 still represents driver/vehicle age factors\n")

# EMERGING PATTERNS AND RISK PROFILES
cat("\n\nEMERGING PATTERNS AND RISK PROFILES\n")

# Calculate component scores
scores <- as.data.frame(pca_result$x[, 1:3])
names(scores) <- c("PC1", "PC2", "PC3")

# Add scores to original data
portfolio_with_scores <- cbind(data1, scores)

# Identify high-risk clusters
thresholds <- apply(scores, 2, quantile, probs = 0.75, na.rm = TRUE)
cat("Thresholds for high scores (75th percentile):\n")
print(thresholds)

# Identify policies in each high-risk category
portfolio_with_scores <- portfolio_with_scores %>%
  mutate(
    high_risk_PC1 = PC1 > thresholds["PC1"],
    high_risk_PC2 = PC2 > thresholds["PC2"],
    high_risk_PC3 = PC3 > thresholds["PC3"],
    any_high_risk = high_risk_PC1 | high_risk_PC2 | high_risk_PC3,
    risk_category = case_when(
      high_risk_PC1 & high_risk_PC2 ~ "High Claims & High Power",
      high_risk_PC1 ~ "High Claims",
      high_risk_PC2 ~ "High Power/Usage",
      high_risk_PC3 ~ "Age-Related Risk",
      TRUE ~ "Standard Risk"
    )
  )

# Count risk categories
risk_counts <- portfolio_with_scores %>%
  count(risk_category) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

cat("\nRISK CATEGORY DISTRIBUTION:\n")
print(risk_counts)

# PLOT 7: Score plot with risk categories
score_plot <- ggplot(portfolio_with_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = risk_category, shape = risk_category), alpha = 0.7, size = 2) +
  geom_hline(yintercept = thresholds["PC2"], linetype = "dashed", 
             color = "gray50", alpha = 0.5) +
  geom_vline(xintercept = thresholds["PC1"], linetype = "dashed", 
             color = "gray50", alpha = 0.5) +
  scale_color_manual(values = c("High Claims & High Power" = "red",
                                "High Claims" = "orange",
                                "High Power/Usage" = "blue",
                                "Age-Related Risk" = "green",
                                "Standard Risk" = "gray")) +
  ggtitle("PCA Score Plot: Risk Categories (PC1 vs PC2)") +
  xlab("PC1: Claims & Discount (25.8% variance)") +
  ylab("PC2: Power & Usage (17.2% variance)") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())
print(score_plot)

# Analyze policies with recent claims
recent_claims <- portfolio_with_scores %>% 
  dplyr::filter(!is.na(claim_severity))

cat("\nANALYSIS OF POLICIES WITH RECENT CLAIMS:\n")
cat("Number of policies with recent claims:", nrow(recent_claims), "\n")
cat("Percentage of total portfolio:", 
    round(nrow(recent_claims)/nrow(data1)*100, 1), "%\n")


#DISTINCT RISK PROFILES
cat("\n\nDISTINCT RISK PROFILES\n")

names(portfolio_with_scores)

# Profile 1: Young Power Drivers
young_power_drivers <- portfolio_with_scores %>%
  dplyr::filter(age < 30, 
         engine_power_kw > 100,
         past_claims_count >= 1)

cat("PROFILE 1: YOUNG POWER DRIVERS\n")
cat("Definition: Age < 30, Engine Power > 100 kW, At least 1 past claim\n")
cat("Count:", nrow(young_power_drivers), "\n")
if (nrow(young_power_drivers) > 0) {
  young_power_summary <- young_power_drivers %>%
    summarise(
      avg_age = round(mean(age, na.rm = TRUE), 1),
      avg_engine_power = round(mean(engine_power_kw, na.rm = TRUE), 1),
      avg_premium = round(mean(premium, na.rm = TRUE), 0),
      avg_past_claims = round(mean(past_claims_count, na.rm = TRUE), 1),
      recent_claim_rate = round(sum(!is.na(claim_severity)) / n() * 100, 1)
    )
  cat("Average age:", young_power_summary$avg_age, "years\n")
  cat("Average engine power:", young_power_summary$avg_engine_power, "kW\n")
  cat("Average premium: MUR", young_power_summary$avg_premium, "\n")
  cat("Average past claims:", young_power_summary$avg_past_claims, "\n")
  cat("Recent claim rate:", young_power_summary$recent_claim_rate, "%\n")
}
cat("\n")

# Profile 2: High-Mileage Commercial Users
high_mileage_commercial <- portfolio_with_scores %>%
  dplyr::filter(annual_mileage > 20000,
         fuel_type == "Diesel")

cat("PROFILE 2: HIGH-MILEAGE COMMERCIAL USERS\n")
cat("Definition: Annual Mileage > 20,000 km/year, Diesel fuel type\n")
cat("Count:", nrow(high_mileage_commercial), "\n")
if (nrow(high_mileage_commercial) > 0) {
  high_mileage_summary <- high_mileage_commercial %>%
    summarise(
      avg_mileage = round(mean(annual_mileage, na.rm = TRUE), 0),
      avg_age = round(mean(age, na.rm = TRUE), 1),
      avg_vehicle_age = round(mean(vehicle_age, na.rm = TRUE), 1),
      avg_premium = round(mean(premium, na.rm = TRUE), 0),
      avg_past_claims = round(mean(past_claims_count, na.rm = TRUE), 1),
      recent_claim_rate = round(sum(!is.na(claim_severity)) / n() * 100, 1)
    )
  cat("Average annual mileage:", high_mileage_summary$avg_mileage, "km/year\n")
  cat("Average age:", high_mileage_summary$avg_age, "years\n")
  cat("Average vehicle age:", high_mileage_summary$avg_vehicle_age, "years\n")
  cat("Average premium: MUR", high_mileage_summary$avg_premium, "\n")
  cat("Average past claims:", high_mileage_summary$avg_past_claims, "\n")
  cat("Recent claim rate:", high_mileage_summary$recent_claim_rate, "%\n")
}
cat("\n")

# Profile 3: Aged Risk
aged_risk <- portfolio_with_scores %>%
  dplyr::filter(age > 60,
         !is.na(claim_severity))

cat("PROFILE 3: AGED RISK\n")
cat("Definition: Age > 60 years with recent claims\n")
cat("Count:", nrow(aged_risk), "\n")
if (nrow(aged_risk) > 0) {
  aged_risk_summary <- aged_risk %>%
    summarise(
      avg_age = round(mean(age, na.rm = TRUE), 1),
      avg_vehicle_age = round(mean(vehicle_age, na.rm = TRUE), 1),
      avg_premium = round(mean(premium, na.rm = TRUE), 0),
      avg_past_claims = round(mean(past_claims_count, na.rm = TRUE), 1),
      avg_claim_severity = round(mean(claim_severity, na.rm = TRUE), 0)
    )
  cat("Average age:", aged_risk_summary$avg_age, "years\n")
  cat("Average vehicle age:", aged_risk_summary$avg_vehicle_age, "years\n")
  cat("Average premium: MUR", aged_risk_summary$avg_premium, "\n")
  cat("Average past claims:", aged_risk_summary$avg_past_claims, "\n")
  cat("Average claim severity: MUR", aged_risk_summary$avg_claim_severity, "\n")
}

# Profile 4: Low Discount High Claim
low_discount_high_claim <- portfolio_with_scores %>%
  dplyr::filter(no_claims_discount < 20,
         past_claims_count >= 2)

cat("\nPROFILE 4: LOW DISCOUNT HIGH CLAIM\n")
cat("Definition: No-claims discount < 20%, At least 2 past claims\n")
cat("Count:", nrow(low_discount_high_claim), "\n")
if (nrow(low_discount_high_claim) > 0) {
  low_discount_summary <- low_discount_high_claim %>%
    summarise(
      avg_discount = round(mean(no_claims_discount, na.rm = TRUE), 1),
      avg_past_claims = round(mean(past_claims_count, na.rm = TRUE), 1),
      avg_total_paid = round(mean(past_total_paid, na.rm = TRUE), 0),
      avg_premium = round(mean(premium, na.rm = TRUE), 0),
      recent_claim_rate = round(sum(!is.na(claim_severity)) / n() * 100, 1)
    )
  cat("Average no-claims discount:", low_discount_summary$avg_discount, "%\n")
  cat("Average past claims:", low_discount_summary$avg_past_claims, "\n")
  cat("Average total paid: MUR", low_discount_summary$avg_total_paid, "\n")
  cat("Average premium: MUR", low_discount_summary$avg_premium, "\n")
  cat("Recent claim rate:", low_discount_summary$recent_claim_rate, "%\n")
}


#SUMMARY AND CONCLUSIONS

cat("\n\nSUMMARY AND CONCLUSIONS\n")
cat("==================================================\n\n")

cat("KEY FINDINGS FROM PCA ANALYSIS:\n")
cat("1. Three principal components explain 55.3% of total variance\n")
cat("2. Component 1 (25.8%): Claims history vs. no-claims discount\n")
cat("3. Component 2 (17.2%): Vehicle power and usage patterns\n")
cat("4. Component 3 (12.3%): Driver and vehicle age factors\n\n")

cat("RISK PROFILES IDENTIFIED:\n")
cat("1. Young Power Drivers: ", nrow(young_power_drivers), 
    " policies\n", sep = "")
cat("2. High-Mileage Commercial: ", nrow(high_mileage_commercial), 
    " policies\n", sep = "")
cat("3. Aged Risk: ", nrow(aged_risk), 
    " policies\n", sep = "")
cat("4. Low Discount High Claim: ", nrow(low_discount_high_claim), 
    " policies\n\n", sep = "")

cat("ACTUARIAL IMPLICATIONS:\n")
cat("1. Premium differentiation should consider these risk dimensions\n")
cat("2. Underwriting guidelines should address specific risk profiles\n")
cat("3. Claims prevention programs could target high-risk groups\n")
cat("4. Portfolio monitoring should track evolution of risk profiles\n")

# END OF PROBLEM 1 ANALYSIS
cat("\n============================================================\n")
cat("END OF PROBLEM 1 ANALYSIS\n")
cat("============================================================\n")


#Problem_2:ASSOCIATION BETWEEN CATEGORICAL VARIABLES
cat("\n============================================================\n")
cat("START OF PROBLEM 2 ANALYSIS\n")
cat("============================================================\n")

#creating contingency table with totals
observed <- table(data1$claim_type, data1$transmission)
addmargins(observed)

#let test be the chi square test
test <- chisq.test(observed)

#find the expected counts
expected_counts<- test$expected
print(expected_counts)

#let stat be the test statistic and df be the test parameter
stat <- test$statistic
df <- test$parameter
critical_value <- qchisq(0.95, df)
chisq.test(observed)


##creating the chi-square graph
#PLOT 8: Chi-squared Distribution
curve(dchisq(x, df), from = 0, to = qchisq(0.999, df),
      main = 'Chi-Squared Distribution(df = 3)',
      ylab = 'Density',
      lwd = 2)
#creating vector of x values
critical_value <- qchisq(0.95, df)
x_vector <- seq(from = critical_value, to = qchisq(0.999, df), length.out = 100)
p_vector <- dchisq(x_vector, df)
polygon(c(x_vector, rev(x_vector)), c(p_vector, rep(0, length(p_vector))), col = adjustcolor('red', alpha=0.3), border = NA)
abline(v = stat, col = "blue", lwd = 2, lty = 2)
text(x = stat + 2.5 , y = 0.05, labels = paste("Test Statistic =", round(stat, 4)), col = "blue")
abline(v = critical_value, col = "darkgreen", lwd = 2, lty = 2)
text(x = critical_value - 2.5, y = 0.08, labels = paste("Critical value =", round(critical_value, 4)), col = "darkgreen")

#standardised residuals
test$stdres

#End of Task 2
cat("\n============================================================\n")
cat("END OF PROBLEM 2 ANALYSIS\n")
cat("============================================================\n")


#Problem_3:CLAIM FREQUENCY ANALYSIS
cat("\n============================================================\n")
cat("START OF PROBLEM 3 ANALYSIS\n")
cat("============================================================\n")

#File: umu_daily_claim_counts_2025.csv
View(data2)
str(data2)
#column Names
names(data2) 

#Calculaing the mean and the variance 
mean(data2$claim_count)
var(data2$claim_count)
x <- na.omit(as.numeric(data2$claim_count))

#PLOT 9: QQ-plot Observed vs. Poisson
pois_mean <- mean(x)
theoretical <- qpois(ppoints(length(x)), lambda = pois_mean)
qqplot(theoretical, sort(x),
       main = "QQ-plot: Observed vs Poisson",
       xlab = "Theoretical Quantiles (Poisson)",
       ylab = "Observed Quantiles",
       pch = 1, col="red")
abline(0,1, col="red", lwd=2) 

#PLOT 10: Histogram of Number of Claims
Number_Of_Claims<- data2$claim_count
hist(Number_Of_Claims,
     main = "Histogram of Number of Claims",
     xlim = c(0,12),
     ylim = c(0,70),
     col = "#FFD1DC") 

#calculations for the chi-square goodness of fit test
observed <- table(x)
n <-  sum(observed)
categories <- as.numeric(names(observed))
expected_prob <- dpois(categories, lambda=pois_mean)
expected <- expected_prob * n
chi_sq <- sum((observed-expected)^2 / expected)
df <- length(observed) - 1 - 1
p_value <- 1 - pchisq(chi_sq, df)

cat("Chi- Square Statistic:", chi_sq, "\n")

cat("Degree Of Freedom:", df, "\n")
 
cat("P-value:", p_value, "\n")

#End of Task 3
cat("\n============================================================\n")
cat("END OF PROBLEM 3 ANALYSIS\n")
cat("============================================================\n")

print(sessionInfo())

