#Dalton Christopher
##################################################
#
# purchases_example gives an example of a model evaluation
#   for three specifications by considering out-of-sample validation
#   on a dataset of purchases from credit-card applications.
#
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# You need to set the working directory to the location
# of your files.
# setwd("/path/to/your/folder")
# Find this path as follows:
# 1. Click on the "File" tab in the bottom right pane.
# 2. Browse to the folder on your computer that contains your R files.
# 3. Click the gear icon and choose the option "Set as Working Directory."
# 4. Copy the command from the Console in the bottom left pane.
# 5. Paste the command below:


 setwd()

# Now, RStudio should know where your files are.



# No libraries required.
# Otherwise would have a command like the following.
# library(name_of_R_package)


# The csv file used below must be in the working directory.
# If you an error message, make sure that the file is
# located in your working directory.
# Also make sure that the name has not changed.


##################################################
# Loading the Data
##################################################

applications <- read.csv('applications_full.csv')

# Inspect the contents.
summary(applications)
# Make sure there are no problems with the data.

credit_bureau <- read.csv('credit_bureau_full.csv')

# Inspect the contents.
summary(credit_bureau)
# Make sure there are no problems with the data.

demographic <- read.csv('demographic_full.csv')

# Inspect the contents.
summary(demographic)
# Make sure there are no problems with the data.



##################################################
# Join into a single dataset
##################################################

# Join the credit data first.
purchases_full <- merge(applications, credit_bureau,
                        by = c('ssn', 'year'))
# Join by SSN and year but not by (possibly stale) zip code. 

summary(purchases_full)


# Join the dempgraphic data by application zip code (not credit bureau).
purchases_full <- merge(purchases_full, demographic,
                        by.x = c('zip_code.x', 'year'),
                        by.y = c('zip_code', 'year'))

summary(purchases_full)



##################################################
# Separate into training and testing datasets
##################################################

# Select columns for regression (no longer need keys).
sel_cols <- c('purchases', 'credit_limit', 
              'fico', 'income', 'homeownership', 
              'num_late', 'past_def', 'num_bankruptcy', 
              'avg_income', 'density')


# Training sample (for estimation) is from year 2022.
sel_rows <- purchases_full[, 'year'] == 2022

purchases_2022 <- purchases_full[sel_rows, sel_cols]


summary(purchases_2022)


# Testing sample 1 (for validation) is from year 2023.
sel_rows <- purchases_full[, 'year'] == 2023

purchases_2023 <- purchases_full[sel_rows, sel_cols]


summary(purchases_2023)


# Testing sample 2 (for validation) is from year 2024.
sel_rows <- purchases_full[, 'year'] == 2024

purchases_2024 <- purchases_full[sel_rows, sel_cols]


summary(purchases_2024)




##################################################
# Estimate models on training data (2022 sample)
##################################################

# First model predicts purchases.
lm_purchases <- lm(data = purchases_2022, 
                   formula = purchases ~ 
                     fico + income + homeownership + 
                     num_late + past_def + num_bankruptcy +
                     avg_income + density)

summary(lm_purchases)


# Second model predicts utilization.
purchases_2022[, 'utilization'] <- 
  purchases_2022[, 'purchases']/purchases_2022[, 'credit_limit']

summary(purchases_2022[, 'utilization'])

lm_utilization <- lm(data = purchases_2022, 
                     formula = utilization ~ 
                       fico + income + homeownership + 
                       num_late + past_def + num_bankruptcy +
                       avg_income + density)

summary(lm_utilization)



# Third model predicts log-odds of utilization.
purchases_2022[, 'log_odds_util'] <- 
  log(purchases_2022[, 'utilization']/(1 - purchases_2022[, 'utilization']))

summary(purchases_2022[, 'log_odds_util'])

lm_log_odds_util <- lm(data = purchases_2022, 
                     formula = log_odds_util ~ 
                       fico + income + homeownership + 
                       num_late + past_def + num_bankruptcy +
                       avg_income + density)

summary(lm_log_odds_util)



##################################################
# Calculate MAE on predictions
##################################################

#--------------------------------------------------
# In-Sample predictions.
#--------------------------------------------------

# Calculate predictions of purchases.
purchases_2022[, 'pred_purch'] <- predict(lm_purchases, newdata = purchases_2022)

# Calculate errors.
purchases_2022[, 'error_purch'] <- purchases_2022[, 'pred_purch'] - purchases_2022[, 'purchases']


MAE_purch_2022 <- mean(abs(purchases_2022[, 'error_purch']))



# Calculate predictions of utilization.
purchases_2022[, 'pred_util'] <- predict(lm_utilization, newdata = purchases_2022)

# Calculate predictions of purchases.
purchases_2022[, 'pred_purch_util'] <- purchases_2022[, 'pred_util']*purchases_2022[, 'credit_limit'] 


# Calculate errors.
purchases_2022[, 'error_purch_util'] <- purchases_2022[, 'pred_purch_util'] - purchases_2022[, 'purchases']


MAE_util_2022 <- mean(abs(purchases_2022[, 'error_purch_util']))



# Calculate predictions of utilization.
purchases_2022[, 'pred_log_odds'] <- predict(lm_log_odds_util, newdata = purchases_2022)

# Calculate predictions of utilization.
purchases_2022[, 'pred_util_log_odds'] <- 
  exp(purchases_2022[, 'pred_log_odds']) / (1 + exp(purchases_2022[, 'pred_log_odds']))

# Calculate predictions of purchases.
purchases_2022[, 'pred_purch_log_odds'] <- purchases_2022[, 'pred_util_log_odds']*purchases_2022[, 'credit_limit'] 


# Calculate errors.
purchases_2022[, 'error_purch_log_odds'] <- purchases_2022[, 'pred_purch_log_odds'] - purchases_2022[, 'purchases']


MAE_log_odds_2022 <- mean(abs(purchases_2022[, 'error_purch_log_odds']))

# Summarize results
print(MAE_purch_2022)
print(MAE_util_2022)
print(MAE_log_odds_2022)


# That was on the sample we estimated.
# Try it on the other samples for some real tests.


#--------------------------------------------------
# Out-Of-Sample predictions with 2023.
#--------------------------------------------------
# Calculate predictions of purchases.
purchases_2023[, 'pred_purch'] <- predict(lm_purchases, newdata = purchases_2023)

# Calculate errors.
purchases_2023[, 'error_purch'] <- purchases_2023[, 'pred_purch'] - purchases_2023[, 'purchases']


MAE_purch_2023 <- mean(abs(purchases_2023[, 'error_purch']))



# Calculate predictions of utilization.
purchases_2023[, 'pred_util'] <- predict(lm_utilization, newdata = purchases_2023)

# Calculate predictions of purchases.
purchases_2023[, 'pred_purch_util'] <- purchases_2023[, 'pred_util']*purchases_2023[, 'credit_limit'] 


# Calculate errors.
purchases_2023[, 'error_purch_util'] <- purchases_2023[, 'pred_purch_util'] - purchases_2023[, 'purchases']


MAE_util_2023 <- mean(abs(purchases_2023[, 'error_purch_util']))



# Calculate predictions of utilization.
purchases_2023[, 'pred_log_odds'] <- predict(lm_log_odds_util, newdata = purchases_2023)

# Calculate predictions of utilization.
purchases_2023[, 'pred_util_log_odds'] <- 
  exp(purchases_2023[, 'pred_log_odds']) / (1 + exp(purchases_2023[, 'pred_log_odds']))

# Calculate predictions of purchases.
purchases_2023[, 'pred_purch_log_odds'] <- purchases_2023[, 'pred_util_log_odds']*purchases_2023[, 'credit_limit'] 


# Calculate errors.
purchases_2023[, 'error_purch_log_odds'] <- purchases_2023[, 'pred_purch_log_odds'] - purchases_2023[, 'purchases']


MAE_log_odds_2023 <- mean(abs(purchases_2023[, 'error_purch_log_odds']))

# Summarize results
print(MAE_purch_2023)
print(MAE_util_2023)
print(MAE_log_odds_2023)





#--------------------------------------------------
# Out-Of-Sample predictions with 2024.
#--------------------------------------------------
# Calculate predictions of purchases.
purchases_2024[, 'pred_purch'] <- predict(lm_purchases, newdata = purchases_2024)

# Calculate errors.
purchases_2024[, 'error_purch'] <- purchases_2024[, 'pred_purch'] - purchases_2024[, 'purchases']


MAE_purch_2024 <- mean(abs(purchases_2024[, 'error_purch']))



# Calculate predictions of utilization.
purchases_2024[, 'pred_util'] <- predict(lm_utilization, newdata = purchases_2024)

# Calculate predictions of purchases.
purchases_2024[, 'pred_purch_util'] <- purchases_2024[, 'pred_util']*purchases_2024[, 'credit_limit'] 


# Calculate errors.
purchases_2024[, 'error_purch_util'] <- purchases_2024[, 'pred_purch_util'] - purchases_2024[, 'purchases']


MAE_util_2024 <- mean(abs(purchases_2024[, 'error_purch_util']))



# Calculate predictions of utilization.
purchases_2024[, 'pred_log_odds'] <- predict(lm_log_odds_util, newdata = purchases_2024)

# Calculate predictions of utilization.
purchases_2024[, 'pred_util_log_odds'] <- 
  exp(purchases_2024[, 'pred_log_odds']) / (1 + exp(purchases_2024[, 'pred_log_odds']))

# Calculate predictions of purchases.
purchases_2024[, 'pred_purch_log_odds'] <- purchases_2024[, 'pred_util_log_odds']*purchases_2024[, 'credit_limit'] 


# Calculate errors.
purchases_2024[, 'error_purch_log_odds'] <- purchases_2024[, 'pred_purch_log_odds'] - purchases_2024[, 'purchases']


MAE_log_odds_2024 <- mean(abs(purchases_2024[, 'error_purch_log_odds']))

# Summarize results
print(MAE_purch_2024)
print(MAE_util_2024)
print(MAE_log_odds_2024)

# Create a matrix to store MAE results
mae_results <- matrix(c(
  MAE_purch_2022, MAE_util_2022, MAE_log_odds_2022,
  MAE_purch_2023, MAE_util_2023, MAE_log_odds_2023,
  MAE_purch_2024, MAE_util_2024, MAE_log_odds_2024
), 
nrow = 3, byrow = TRUE)

# Add row and column names for clarity
rownames(mae_results) <- c("2022", "2023", "2024")
colnames(mae_results) <- c("Purchases", "Utilization", "Log-Odds Utilization")

# Display the MAE results table
print(mae_results)



##################################################
# End
##################################################

