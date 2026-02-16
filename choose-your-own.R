# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(scales)
library(randomForest)
library(knitr)
library(kableExtra)


# DATA LOADING, CLEANING AND TRANSFORMATION -------------------------------

# Load credit card churn dataset:

options(timeout = 120)

bankChurners <- read.csv("BankChurners.csv")

# Drop unnecessary columns
bankChurners <- bankChurners[, 2:(ncol(bankChurners)-2)]

# Transform all character columns to factors and sort Income_Category
bankChurners <- bankChurners %>% mutate_if(is.character, as.factor)
bankChurners$Income_Category <- factor(
  bankChurners$Income_Category,
  levels = c(
    "Less than $40K",
    "$40K - $60K",
    "$60K - $80K",
    "$80K - $120K",
    "$120K +",
    "Unknown"
  )
)

# Configuring names to be compatible with r
levels(bankChurners$Attrition_Flag) <- make.names(levels(bankChurners$Attrition_Flag))

# Structure of bankChurners dataset
str(bankChurners)

# Dataframe with description of bankChurners variables
bankChurners_v <- data.frame(
  Variable = c(
    "Customer_Age", "Gender", "Dependent_count", "Education_Level",
    "Marital_Status", "Income_Category", "Card_Category", "Months_on_book",
    "Total_Relationship_Count", "Months_Inactive_12_mon", "Contacts_Count_12_mon",
    "Credit_Limit", "Total_Revolving_Bal", "Avg_Open_To_Buy",
    "Total_Amt_Chng_Q4_Q1", "Total_Trans_Amt", "Total_Trans_Ct",
    "Total_Ct_Chng_Q4_Q1", "Avg_Utilization_Ratio"
  ),
  Type = c(
    "Integer", "Factor", "Integer", "Factor", "Factor", "Factor", "Factor",
    "Integer", "Integer", "Integer", "Integer", "Numeric", "Integer", "Numeric",
    "Numeric", "Integer", "Integer", "Numeric", "Numeric"
  ),
  Description = c(
    "Customer age in years.",
    "Customer gender: 'F' for female and 'M' for male.",
    "Number of persons who are financially dependent on the customer.",
    "Customer education level: 'Unknown', 'Uneducated', 'High School', 'Graduate', 'Post-Graduate', 'College', and 'Doctorate'.",
    "Customer marital status: 'Unknown', 'Single', 'Married', and 'Divorced'.",
    "Customer annual income range: 'Less than $40K', '$40K - $60K', '$60K - $80K', '$80K - $120K', '$120K +', and 'Unknown'.",
    "Customer card type: 'Blue', 'Silver', 'Gold', and 'Platinum'.",
    "Length of relationship with the bank in months.",
    "Total number of different products held by the customer.",
    "Months without activity in the last year.",
    "Number of times the customer contacted the bank in the last year.",
    "Average credit limit over the last year.",
    "Average total revolving balance over the last year.",
    "Average available credit line in the last year (Credit_Limit - Total_Revolving_Bal).",
    "Change in transaction amount (Q4 over Q1).",
    "Total amount of transactions in the last year.",
    "Total number of transactions in the last year.",
    "Change in the number of transactions (Q4 over Q1).",
    "Average percentage of credit line usage (0 to 1)."
  )
)

# Table with description of bankChurners variables with established format
bankChurners_v %>%
  kbl(align = "ccl", longtable = TRUE, booktabs = TRUE) %>%
  kable_styling(
    latex_options = c("striped", "repeat_header", "hold_position"),
    full_width = FALSE,
    font_size = 10
  ) %>%
  row_spec(0, bold = TRUE, align = "c") %>%
  column_spec(1, width = "4.8cm", bold = TRUE, italic = TRUE) %>%
  column_spec(2, width = "1.4cm") %>%
  column_spec(3, width = "8.8cm")

# Convert character columns into columns with numbers 
bankChurners_num <- bankChurners %>%
  {data.frame(
    # Keep the target variable as a factor for classification
    Attrition_Flag = .$Attrition_Flag,
    predict(
      # dummyVars creates an array where each category is a new numeric column
      dummyVars(~ ., data = select(., -Attrition_Flag), fullRank = TRUE),
      newdata = .
    )
  )}

# Final hold-out test set will be 10% of bankChurners_num data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = bankChurners_num$Attrition_Flag, p = 0.1, list = FALSE)
total_train_set <- bankChurners_num[-test_index,]
final_holdout_test <- bankChurners_num[test_index,]

# Divide the rest into train_set (80%) and test_set (20%).
test_index <- createDataPartition(y = total_train_set$Attrition_Flag, p = 0.2, list = FALSE)
test_set <- total_train_set[test_index,]
train_set <- total_train_set[-test_index,]


# EXPLORATORY DATA ANALYSIS (EDA) -----------------------------------------

# Plot the proportion of customer attrition status with bars
bankChurners %>% ggplot(
  aes(Attrition_Flag, fill = Attrition_Flag)
) +
  geom_bar(aes(y = after_stat(count / sum(count)))) +
  scale_y_continuous(labels = percent) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_rect(
      colour = "black",
      fill = NA
    )
  ) +
  labs(
    title = "Distribution of Customer Attrition Status",
    x = "Attrition Flag",
    y = "Percentage of Total Customers"
  )

# Compare age distributions by customer gender and attrition status using a faceted histogram grid.
bankChurners %>%
  ggplot(aes(Customer_Age, fill = Attrition_Flag)) +
  geom_histogram(binwidth = 5, color = "white") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_rect(
      colour = "black",
      fill = NA
    )
  ) +
  facet_grid(
    Gender ~ Attrition_Flag,
    labeller = labeller(Gender = c(F = "Female", M = "Male"))
  ) +
  labs(
    title = "Age Distribution by Gender and Attrition Flag",
    x = "Customer Age",
    y = "Count of Customers"
  )

# Plot the proportion of customer attrition status across income categories using a stacked bar plot
bankChurners %>% ggplot(
  aes(Income_Category, fill = Attrition_Flag)
) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA
    )
  ) +
  labs(
    title = "Attrition Proportion Across Income Categories",
    x = "Income Category",
    y = "Percentage",
    fill = "Attrition Flag"
  )

# Plot the proportion of customer attrition status across months of inactivity using a stacked bar plot
bankChurners %>% ggplot(
  aes(as.factor(Months_Inactive_12_mon), fill = Attrition_Flag)
) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA
    )
  ) +
  labs(
    title = "Attrition Proportion by Months of Inactivity",
    x = "Months Inactive (12 months)",
    y = "Attrition Proportion",
    fill = "Attrition Flag"
  )

# Plot the proportion of customer attrition status across contact frequency using a stacked bar plot
bankChurners %>% ggplot(
  aes(as.factor(Contacts_Count_12_mon), fill = Attrition_Flag)
) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA
    )
  ) +
  labs(
    title = "Attrition Proportion by Number of Contacts",
    x = "Number of Contacts (12 months)",
    y = "Attrition Proportion",
    fill = "Attrition Flag"
  )

# Plot distribution of total transactions by customer attrition status using a density plot
bankChurners %>% ggplot(
  aes(Total_Trans_Ct, fill = Attrition_Flag)
) +
  geom_density(alpha = 0.2) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA
    )
  ) +
  labs(
    title = "Total Transaction Count Density by Attrition Flag",
    x = "Number of Transactions (12 Months)",
    y = "Density",
    fill = "Attrition Flag"
  )

# Plot the relationship between transaction number and amount using a scatter plot. 
bankChurners %>% ggplot(
  aes(Total_Trans_Ct, Total_Trans_Amt, color = Attrition_Flag)
) +
  geom_point(alpha = 0.2) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA
    )
  ) +
  labs(
    title = "Number of Transactions vs Amount of Transactions",
    x = "Number of Transactions (12 months)",
    y = "Amount of Transactions (12 months)",
    color = "Attrition Flag"
  )

# Plot distribution of average credit utilization ratio for each attrition status using a density plot.
bankChurners %>% ggplot(
  aes(Avg_Utilization_Ratio, fill = Attrition_Flag)
) +
  geom_density(alpha = 0.2) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA
    )
  ) +
  labs(
    title = "Average Credit Utilization Ratio Density by Attrition Flag",
    x = "Average Utilization Ratio",
    y = "Density",
    fill = "Attrition Flag"
  )

# COMPUTATIONAL NUANCES OF THE TRAIN FUNCTION -----------------------------

# Define the training control parameters
control <- trainControl(
  method="cv", # Cross-Validation
  number = 10, # Use 10 folds
  classProbs = TRUE, # Calculate class probabilities to enable ROC curve analysis
  summaryFunction = twoClassSummary # Use ROC, Sensitivity, and Specificity as evaluation metrics
)

# Create a tibble to store all the results of the different approaches
model_results <- tibble()

# MODEL APPROACHES --------------------------------------------------------

## MODEL 1: K-NEAREST NEIGHBORS -------------------------------------------

# Train a KNN model by testing 35 values of k between 3 and 71 using the
# 10-fold cross validation created previously through the ‘control’ variable and
# ensuring that the best k is selected based on ROC instead of Accuracy 
set.seed(1, sample.kind="Rounding")
fit_knn <- train(
  Attrition_Flag ~ .,
  method = "knn",
  data = train_set,
  tuneGrid = data.frame(k = seq(3, 71, 2)),
  trControl = control
)

# See the results and highlight the optimal k
ggplot(fit_knn, highlight = TRUE) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA
    )
  )

# Access the parameter that maximized the area under the ROC curve
fit_knn$bestTune

# Observe the results of predicting using the best performing model
cm_knn <- confusionMatrix(
  predict(fit_knn, test_set, type = "raw"),
  test_set$Attrition_Flag
)
cm_knn

# Create a dataframe in the format required for the twoClassSummary function
TCSdf <-data.frame(
  obs = test_set$Attrition_Flag,
  predict(fit_knn, test_set, type = "prob"),
  pred = predict(fit_knn, test_set, type = "raw")
)

# Display the ROC, sensitivity, and specificity metrics
results <- twoClassSummary(
  TCSdf,
  lev = levels(TCSdf$obs)
)
results

# Store KNN performance metrics into the tibble
model_results <- bind_rows(
  model_results,
  tibble(
    Method = "k-nearest neighbors",
    Accuracy = cm_knn$overall["Accuracy"],
    ROC = results["ROC"],
    Sensitivity = results["Sens"],
    Specificity = results["Spec"]
  )
)
model_results

## MODEL 2: BINOMIAL LOGISTIC REGRESSION (GLM) --------------------------------------

# Train a binomial logistic regression model using the 10-fold cross validation
# created previously through the ‘control’ variable and using ROC as the primary
# evaluation metric for comparison
set.seed(1, sample.kind="Rounding")
fit_glm <- train(
  Attrition_Flag ~ .,
  method = "glm",
  family = "binomial",
  data = train_set,
  trControl = control
)

# Observe the results of predicting using the fitted glm model
cm_glm <- confusionMatrix(
  predict(fit_glm, test_set, type = "raw"),
  test_set$Attrition_Flag
)
cm_glm

# Create a dataframe in the format required for the twoClassSummary function
TCSdf <-data.frame(
  obs = test_set$Attrition_Flag,
  predict(fit_glm, test_set, type = "prob"),
  pred = predict(fit_glm, test_set, type = "raw")
)

# Display the ROC, sensitivity, and specificity metrics
results <- twoClassSummary(
  TCSdf,
  lev = levels(TCSdf$obs)
)
results

# Store glm performance metrics into the tibble
model_results <- bind_rows(
  model_results,
  tibble(
    Method = "binomial logistic regression",
    Accuracy = cm_glm$overall["Accuracy"],
    ROC = results["ROC"],
    Sensitivity = results["Sens"],
    Specificity = results["Spec"]
  )
)
model_results

## MODEL 3: RANDOM FOREST -------------------------------------------------

# mtry and nodesize range to optimize random forest parameters
mtry <- data.frame(mtry = seq(2, 32, 6))
nodesize <- seq(5, 50, 5)

# Create a tibble to store all the results of the different parameters
parameters <- tibble()

# Iterate over each nodesize to evaluate performance with each mtry
sapply(nodesize, function(ns) {
  set.seed(1, sample.kind="Rounding")
  # Train a random forest model using the 10-fold cross validation
  # created previously through the ‘control’ variable
  rm <- train(
    Attrition_Flag ~ .,
    method = "rf",
    data = train_set,
    tuneGrid = mtry,
    trControl = control,
    nodesize = ns
  )$results
  # Store the results of ROC, Sensitivity, and Specificity.
  parameters <<- bind_rows(
    parameters,
    tibble(
      mtry = rm$mtry,
      nodesize = ns,
      ROC = rm$ROC,
      Sens = rm$Sens,
      Spec = rm$Spec
    )
  )
})

# mtry = 2 is removed due to its poor overall performance, and all metrics are
# plotted under all parameters entered.
parameters %>%
  filter(mtry > 1) %>% 
  pivot_longer(
    cols = c(ROC, Sens, Spec),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  ggplot(aes(nodesize, Value, color = as.factor(mtry))) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA
    )
  ) +
  labs(
    title = "Metrics by mtry and nodesize",
    x = "nodesize",
    y = "Metric",
    color = "mtry"
  )

# metrics with mtry = 14
parameters %>%
  filter(mtry == 14) %>% 
  pivot_longer(
    cols = c(ROC, Sens, Spec),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  ggplot(aes(nodesize, Value, color = as.factor(mtry))) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA
    )
  ) +
  labs(
    title = "Metrics by mtry = 14 and nodesize",
    x = "nodesize",
    y = "Metric",
    color = "mtry"
  )

# Based on the plot above, mtry = 14 and nodesize = 10 are selected.
set.seed(1, sample.kind="Rounding")
fit_rf <- train(
  Attrition_Flag ~ .,
  method = "rf",
  data = train_set,
  tuneGrid = data.frame(mtry = 14),
  trControl = control,
  nodesize = 10,
  importance = TRUE
)

# Observe the results of predicting using the random forest model
cm_rf <- confusionMatrix(
  predict(fit_rf, test_set, type = "raw"),
  test_set$Attrition_Flag
)
cm_rf

# Create a dataframe in the format required for the twoClassSummary function
TCSdf <-data.frame(
  obs = test_set$Attrition_Flag,
  predict(fit_rf, test_set, type = "prob"),
  pred = predict(fit_rf, test_set, type = "raw")
)

# Display the ROC, sensitivity, and specificity metrics
results <- twoClassSummary(
  TCSdf,
  lev = levels(TCSdf$obs)
)
results

# Store rf performance metrics into the tibble
model_results <- bind_rows(
  model_results,
  tibble(
    Method = "random forest",
    Accuracy = cm_rf$overall["Accuracy"],
    ROC = results["ROC"],
    Sensitivity = results["Sens"],
    Specificity = results["Spec"]
  )
)
model_results

# Results -----------------------------------------------------------------

# Observe the results of predicting using the random forest model in final_holdout_test
cm_final <- confusionMatrix(
  predict(fit_rf, final_holdout_test, type = "raw"),
  final_holdout_test$Attrition_Flag
)
cm_final

# Create a dataframe in the format required for the twoClassSummary function
TCSdf <-data.frame(
  obs = final_holdout_test$Attrition_Flag,
  predict(fit_rf, final_holdout_test, type = "prob"),
  pred = predict(fit_rf, final_holdout_test, type = "raw")
)

# Display the ROC, sensitivity, and specificity metrics
results <- twoClassSummary(
  TCSdf,
  lev = levels(TCSdf$obs)
)
results

# Store final rf performance metrics into the tibble
model_results <- bind_rows(
  model_results,
  tibble(
    Method = "random forest final holdout set",
    Accuracy = cm_final$overall["Accuracy"],
    ROC = results["ROC"],
    Sensitivity = results["Sens"],
    Specificity = results["Spec"]
  )
)
model_results

# See the importance of the variables in the model
varImpPlot(
  fit_rf$finalModel,
  main = "",
  cex = 0.8,
  n.var = 15
)
title("Variable Importance in Prediction", line = 0.5, font.main = 1)