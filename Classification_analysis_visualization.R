install.packages("ggplot2")
install.packages("infotheo", dependencies=TRUE)
install.packages("dplyr", dependencies=TRUE)
install.packages("caret")
install.packages("Information")
install.packages("pROC")
install.packages("tidyr") 

library(Information)
library(caret)
library(ggplot2)  # For visualization
library(infotheo)
library(dplyr)
library(pROC)
library(tidyr) 


# Reading CSV files
df1 <- read.csv("data/2014_Financial_Data.csv")
df2 <- read.csv("data/2015_Financial_Data.csv")
df3 <- read.csv("data/2016_Financial_Data.csv")
df4 <- read.csv("data/2017_Financial_Data.csv")
df5 <- read.csv("data/2018_Financial_Data.csv")

# Display the first few rows of the first dataset
head(df1)


# Check if all datasets have the same column names
all_columns <- list(
  colnames(df1),
  colnames(df2),
  colnames(df3),
  colnames(df4),
  colnames(df5)
)




# Check if all columns are identical
identical_columns <- all(sapply(all_columns, function(x) identical(x, colnames(df1))))

if (identical_columns) {
  print("All datasets have identical columns.")
} else {
  print("There are differences in column names across datasets.")
  # Print the differing columns for debugging
  lapply(all_columns, setdiff, colnames(df1))
}


# Rename the differing column in each dataframe
colnames(df1)[colnames(df1) == "X2015.PRICE.VAR...."] <- "PRICE_VAR"
colnames(df2)[colnames(df2) == "X2016.PRICE.VAR...."] <- "PRICE_VAR"
colnames(df3)[colnames(df3) == "X2017.PRICE.VAR...."] <- "PRICE_VAR"
colnames(df4)[colnames(df4) == "X2018.PRICE.VAR...."] <- "PRICE_VAR"
colnames(df5)[colnames(df5) == "X2019.PRICE.VAR...."] <- "PRICE_VAR"


# Check for proper renaming
grep("PRICE_VAR", colnames(df1), value = TRUE)

# Display the column names of the first dataset
colnames(df1)


# Get a statistical summary
summary(df1)

# Drop the first column from each dataframe
df1 <- df1[, -1]
df2 <- df2[, -1]
df3 <- df3[, -1]
df4 <- df4[, -1]
df5 <- df5[, -1]

# Check if the first column was removed
head(df1)

# Function to identify column types
get_column_types <- function(df) {
  sapply(df, function(col) {
    if (is.numeric(col)) {
      return("numeric")
    } else if (is.factor(col) || is.character(col)) {
      return("categorical")
    } else {
      return("other")
    }
  })
}

# Apply the function to your dataframe
column_types <- get_column_types(df1)

# Count the number of each type
type_counts <- table(column_types)

# Display the column types and their counts
print(type_counts)


# Adding the year column to each dataset, for future visualization purposes
df1$Year <- 2014
df2$Year <- 2015
df3$Year <- 2016
df4$Year <- 2017
df5$Year <- 2018



# Check for missing values in df1
sum(is.na(df1))  # Total number of missing values
sum(is.na(df2))
sum(is.na(df3))
sum(is.na(df4))
sum(is.na(df5))

# Check for missing values in each column
colSums(is.na(df1))


# Define the mode function
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]  # Find most frequent value
}

# Impute numeric and categorical columns for all dataframes
impute_missing <- function(df) {
  df <- data.frame(lapply(df, function(x) {
    if (is.numeric(x)) {
      x[is.na(x)] <- median(x, na.rm = TRUE)  # Impute numeric with median
    } else if (is.factor(x) || is.character(x)) {
      x[is.na(x)] <- get_mode(x[!is.na(x)])  # Impute categorical with mode
    }
    return(x)
  }))
  return(df)
}

# Apply the function to all dataframes
df1 <- impute_missing(df1)
df2 <- impute_missing(df2)
df3 <- impute_missing(df3)
df4 <- impute_missing(df4)
df5 <- impute_missing(df5)

# Verify that no missing values remain
sum(is.na(df1)) 

# Verify no missing values across all dataframes
sapply(list(df1, df2, df3, df4, df5), function(df) sum(is.na(df)))

# Function to replace outliers with median
replace_outliers_with_median <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  column[column < lower_bound | column > upper_bound] <- median(column, na.rm = TRUE)
  return(column)
}

# Apply to numerical columns for all dataframes
replace_outliers_in_dataframe <- function(df) {
  numerical_columns <- sapply(df, is.numeric)
  df[, numerical_columns] <- lapply(df[, numerical_columns], replace_outliers_with_median)
  return(df)
}

# Apply to each dataframe
df1_clean <- replace_outliers_in_dataframe(df1)
df2_clean <- replace_outliers_in_dataframe(df2)
df3_clean <- replace_outliers_in_dataframe(df3)
df4_clean <- replace_outliers_in_dataframe(df4)
df5_clean <- replace_outliers_in_dataframe(df5)

# Display the first 5 rows of the cleaned dataframe
head(df1_clean, 5)

# Check summary statistics for df1_clean
summary(df1_clean)

# Select specific numerical columns for boxplot
selected_columns <- c("Revenue", "EPS", "Net.Debt", "Market.Cap", "ROIC")


# Concatenate all cleaned dataframes
data <- rbind(df1_clean, df2_clean, df3_clean, df4_clean, df5_clean)

# Display structure of the concatenated dataframe
str(data)

# Summary statistics for numerical columns only
summary(data[, sapply(data, is.numeric)])

# Number of rows and columns
nrow(data)
ncol(data)

# Check for missing values in the entire dataframe
sum(is.na(data)) 

# Check for missing values per column
colSums(is.na(data))


# Sector-Class
sector_class_counts <- data %>%
  count(Sector, Class) %>%
  group_by(Sector) %>%
  mutate(Percentage = n / sum(n) * 100) %>%  # Calculate percentages
  ungroup() %>%
  complete(Sector, Class, fill = list(n = 0, Percentage = 0)) %>%  
  mutate(Class = factor(Class, levels = c(0, 1)))  

# Ensure proper ordering of Sectors based on total stock count
sector_totals <- sector_class_counts %>%
  group_by(Sector) %>%
  summarise(Total = sum(n))  # Sum stock counts per sector

sector_class_counts <- sector_class_counts %>%
  left_join(sector_totals, by = "Sector") %>%
  mutate(Sector = reorder(Sector, -Total))  # Reorder based on total stocks

# Visualization with percentages
ggplot(sector_class_counts, aes(x = Sector, y = Percentage, fill = factor(Class))) +
  geom_bar(stat = "identity", position = "fill", width = 0.9) +  # Stacked proportional bars
  
  # Add text inside bars showing percentages
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),  
            position = position_fill(vjust = 0.5),  
            size = 5, fontface = "bold", color = "white") +  
  
  # Labels & Titles
  labs(title = "Class Distribution Across Sectors",
       subtitle = "Proportion of Buy (1) vs. Not Buy (0) per sector",
       x = "Sector", y = "Proportion",
       fill = "Stock Classification") +
  
  # Define fill colors for bars
  scale_fill_manual(values = c("red", "green"), labels = c("0 - Not Buy", "1 - Buy")) +  
  
  # Set x-axis with proper sector names
  scale_x_discrete(name = "Sector") +  
  
  # Theme improvements
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),  # Rotate x-axis labels
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14),
    legend.title = element_text(face = "bold", size = 12),
    legend.position = "right"
  )





# Feature Selection for Classification - Class Variable

# Remove columns
data_classification <- data[, !(names(data) %in% c("Year", "PRICE_VAR"))]
print(data_classification)

# Calculate Information Value (IV)
library(Information)
iv_summary <- create_infotables(data = data_classification, y = "Class", bins = 10, parallel = FALSE)
print(iv_summary$Summary)

# Select Variables with 0.1 ≤ IV ≤ 0.5
selected_vars <- subset(iv_summary$Summary, IV >= 0.1 & IV <= 0.5)$Variable
final_model_data <- data_classification[, c(selected_vars, "Class")]

# Display Selected Variables and Summary
cat("\nSelected Variables (0.1 ≤ IV ≤ 0.5):\n")
print(selected_vars)
cat("\nFinal Dataset Summary:\n")
print(summary(final_model_data))

# Identify and Remove Highly Correlated Variables
corr_matrix <- cor(final_model_data[, -ncol(final_model_data)], use = "complete.obs")
highly_corr <- which(abs(corr_matrix) > 0.85, arr.ind = TRUE)
highly_corr <- highly_corr[highly_corr[, 1] != highly_corr[, 2], ]

correlated_vars <- data.frame(
  Feature1 = rownames(corr_matrix)[highly_corr[, 1]],
  Feature2 = colnames(corr_matrix)[highly_corr[, 2]],
  Correlation = corr_matrix[highly_corr]
)

cat("\nHighly Correlated Variable Pairs (|correlation| > 0.85):\n")
print(correlated_vars)

# Count occurrences of multicollinear variables
var_corr_counts <- table(c(correlated_vars$Feature1, correlated_vars$Feature2))
vars_to_remove <- names(var_corr_counts[var_corr_counts > 3])
filtered_data <- final_model_data[, !names(final_model_data) %in% vars_to_remove]

# Remove Least Predictive Variable from Remaining Correlated Pairs
remaining_corr_vars <- correlated_vars[!(correlated_vars$Feature1 %in% vars_to_remove | correlated_vars$Feature2 %in% vars_to_remove), ]
vars_to_remove_final <- unique(apply(remaining_corr_vars, 1, function(row) {
  f1 <- row["Feature1"]
  f2 <- row["Feature2"]
  ifelse(iv_summary$Summary$IV[iv_summary$Summary$Variable == f1] <
           iv_summary$Summary$IV[iv_summary$Summary$Variable == f2], f1, f2)
}))

cleaned_data <- filtered_data[, !names(filtered_data) %in% vars_to_remove_final]

# Ensure No More Multicollinear Pairs
final_corr_matrix <- cor(cleaned_data[, -ncol(cleaned_data)], use = "complete.obs")
final_highly_corr <- which(abs(final_corr_matrix) > 0.85, arr.ind = TRUE)
final_highly_corr <- final_highly_corr[final_highly_corr[, 1] != final_highly_corr[, 2], ]

final_correlated_vars <- data.frame(
  Feature1 = rownames(final_corr_matrix)[final_highly_corr[, 1]],
  Feature2 = colnames(final_corr_matrix)[final_highly_corr[, 2]],
  Correlation = final_corr_matrix[final_highly_corr]
)

# Output Removed Variables and Summary
cat("\nRemoved Variables (Correlation > 3 pairs):\n", vars_to_remove, "\n")
cat("\nRemoved Least Predictive Variables from Correlated Pairs:\n", vars_to_remove_final, "\n")
cat("\nFinal Dataset Summary:\n")
print(summary(cleaned_data))

# Ensure No More Multicollinearity Exists
if (nrow(final_correlated_vars) == 0) {
  cat("\nNo more multicollinear pairs detected.\n")
} else {
  cat("\nRemaining Multicollinear Pairs:\n")
  print(final_correlated_vars)
}



# Prepare the Data
set.seed(123)  # For reproducibility

# Ensure Class variable is a factor
cleaned_data$Class <- as.factor(cleaned_data$Class)

# Split the data into training and testing sets (80% train, 20% test)
train_indices <- sample(seq_len(nrow(cleaned_data)), size = 0.8 * nrow(cleaned_data))
train_data <- cleaned_data[train_indices, ]
test_data <- cleaned_data[-train_indices, ]

# Build the Logistic Regression Model
logistic_model <- glm(Class ~ ., data = train_data, family = binomial)

# Logistic Regression Model Summary
cat("\nLogistic Regression Model Summary:\n")
summary(logistic_model)

# Predictions and Evaluation
predicted_probs <- predict(logistic_model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)  # Threshold = 0.5
predicted_classes <- as.factor(predicted_classes)

# Confusion Matrix
cat("\nConfusion Matrix:\n")
confusion <- confusionMatrix(predicted_classes, test_data$Class)
print(confusion)

# Calculate and Plot AUC-ROC Curve
roc_curve <- roc(test_data$Class, predicted_probs)

cat("\nAUC Value:\n")
print(auc(roc_curve))

# Plot ROC Curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)



# Extract coefficients and determine variable importance
coeff_df <- data.frame(
  Feature = names(coef(logistic_model)),
  Coeff_Value = coef(logistic_model)
)

# Remove intercept from importance ranking
coeff_df <- coeff_df[coeff_df$Feature != "(Intercept)", ]

# Compute absolute coefficient values for sorting
coeff_df$Abs_Coeff <- abs(coeff_df$Coeff_Value)
coeff_df <- coeff_df[order(-coeff_df$Abs_Coeff), ]

# Display top 20 influential variables
cat("\nTop 20 Most Important Variables:\n")
print(head(coeff_df, 20))

# Ensure proper sorting before visualization
coeff_df$Abs_Coeff <- abs(coeff_df$Coeff_Value)
coeff_df <- coeff_df[order(-coeff_df$Abs_Coeff), ]


coeff_df <- coeff_df[order(-coeff_df$Abs_Coeff), ]
top_features <- coeff_df[1:10, ]

# Plot top 10 most important financial indicators
ggplot(top_features, aes(x = reorder(Feature, Abs_Coeff), y = Abs_Coeff, fill = Abs_Coeff)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Remove legend
  geom_text(aes(label = round(Abs_Coeff, 2)), 
            hjust = -0.1,  # Ensure all numbers are placed to the right
            color = "black",  # Ensure readability
            size = 5, fontface = "bold") +  # Increase text size
  coord_flip() +  
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  
  labs(
    title = "Top 10 Financial Indicators Impacting Stock Performance",
    x = "Financial Features",
    y = "Absolute Coefficient"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12, face = "bold"),  
    axis.text.x = element_text(size = 12), 
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )
