
# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)

# Manually choose the cleaned CSV file
file_path <- file.choose()

# Load the dataset
df <- read_csv(file_path, show_col_types = FALSE)

# Verify the dataset
cat("Dataset loaded successfully!\n")
cat("Number of rows and columns:\n")
print(dim(df))

cat("\nColumn names:\n")
print(names(df))

cat("\nMissing values in each column:\n")
print(colSums(is.na(df)))

cat("\nDuplicate rows:\n")
print(sum(duplicated(df)))

cat("\nAutomation risk score summary:\n")
print(summary(df$automation_risk_score))




# ============================================================
# DA2: AI AUTOMATION RISK PREDICTION
# STEP 2: DESCRIPTIVE STATISTICAL ANALYSIS
# ============================================================

# Check that the dataset is loaded
if (!exists("df") || !is.data.frame(df)) {
  stop("Dataset 'df' is not loaded. Run Step 1 first.")
}

# Select numeric columns only
numeric_df <- df[, vapply(df, is.numeric, logical(1)),
                 drop = FALSE]

if (ncol(numeric_df) == 0) {
  stop("No numeric columns were found in the dataset.")
}

# Calculate descriptive statistics
descriptive_stats <- data.frame(
  Variable = names(numeric_df),
  Mean = vapply(numeric_df, mean, numeric(1), na.rm = TRUE),
  Median = vapply(numeric_df, median, numeric(1), na.rm = TRUE),
  Standard_Deviation = vapply(
    numeric_df, sd, numeric(1), na.rm = TRUE
  ),
  Variance = vapply(numeric_df, var, numeric(1), na.rm = TRUE),
  Minimum = vapply(numeric_df, min, numeric(1), na.rm = TRUE),
  Maximum = vapply(numeric_df, max, numeric(1), na.rm = TRUE),
  row.names = NULL,
  check.names = FALSE
)

# Round numeric statistics only; keep variable names unchanged
numeric_columns <- vapply(descriptive_stats, is.numeric, logical(1))
descriptive_stats[numeric_columns] <-
  lapply(descriptive_stats[numeric_columns], round, digits = 3)

# Display results
cat("\nDESCRIPTIVE STATISTICS\n")
print(descriptive_stats, row.names = FALSE)

# Save results to a CSV file
write.csv(
  descriptive_stats,
  file = "descriptive_statistics.csv",
  row.names = FALSE
)

cat("\nAnalysis completed successfully.\n")
cat("Results saved as descriptive_statistics.csv\n")
cat("Saved in:", getwd(), "\n")

# Display descriptive statistics as a clean table
print(
  as.data.frame(descriptive_stats),
  row.names = FALSE,
  right = FALSE
)

# Open the saved CSV file in RStudio
View(descriptive_stats)




# ============================================================
# DA2 - STEP 3: CORRELATION ANALYSIS
# ============================================================

# Install packages if required
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}

library(ggplot2)
library(corrplot)

# Select meaningful numeric variables
cor_vars <- c(
  "posting_year",
  "ai_intensity_score",
  "salary_usd",
  "salary_change_vs_prev_year_percent",
  "automation_risk_score"
)

# Verify required columns exist
missing_cols <- setdiff(cor_vars, names(df))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

# Create numeric dataset
cor_data <- df[, cor_vars, drop = FALSE]

# Calculate Pearson correlation matrix
cor_matrix <- cor(
  cor_data,
  use = "complete.obs",
  method = "pearson"
)

# Display correlation values rounded to 3 decimals
cat("\nPEARSON CORRELATION MATRIX\n")
print(round(cor_matrix, 3))

# Save correlation matrix
write.csv(
  round(cor_matrix, 3),
  "correlation_matrix.csv"
)

# Generate correlation heatmap
png("correlation_heatmap.png", width = 1200, height = 1000,
    res = 150)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.7,
  tl.col = "black",
  tl.srt = 45,
  diag = FALSE,
  title = "Pearson Correlation Heatmap",
  mar = c(0, 0, 2, 0)
)

dev.off()

cat("\nCorrelation matrix saved as correlation_matrix.csv\n")
cat("Heatmap saved as correlation_heatmap.png\n")



# ============================================================
# DA2 - STEP 4: HYPOTHESIS TESTING
# Question: Does automation risk differ by AI mention status?
# ============================================================

# Verify required columns
required_cols <- c("ai_mentioned", "automation_risk_score")

if (!all(required_cols %in% names(df))) {
  stop("Required columns are missing from the dataset.")
}

# Prepare analysis data
test_data <- data.frame(
  ai_mentioned = tolower(trimws(as.character(df$ai_mentioned))),
  automation_risk_score = df$automation_risk_score
)

# Standardize common Boolean labels
test_data$ai_mentioned[
  test_data$ai_mentioned %in% c("true", "yes", "1")
] <- "yes"

test_data$ai_mentioned[
  test_data$ai_mentioned %in% c("false", "no", "0")
] <- "no"

# Keep valid observations only
test_data <- test_data[
  test_data$ai_mentioned %in% c("yes", "no") &
    !is.na(test_data$automation_risk_score),
]

test_data$ai_mentioned <- factor(
  test_data$ai_mentioned,
  levels = c("no", "yes"),
  labels = c("AI not mentioned", "AI mentioned")
)

# Check that both groups have observations
if (any(table(test_data$ai_mentioned) == 0)) {
  stop("Both AI mention groups must contain observations.")
}

# Calculate group summary statistics
group_summary <- do.call(
  rbind,
  lapply(split(test_data$automation_risk_score,
               test_data$ai_mentioned),
         function(x) {
           data.frame(
             Sample_Size = length(x),
             Mean = mean(x),
             SD = sd(x),
             Median = median(x)
           )
         })
)

group_summary$Group <- rownames(group_summary)
rownames(group_summary) <- NULL
group_summary <- group_summary[
  , c("Group", "Sample_Size", "Mean", "SD", "Median")
]

cat("\nGROUP SUMMARY\n")
print(group_summary, row.names = FALSE)

# Hypotheses:
# H0: The population mean automation risk is equal in both groups.
# H1: The population mean automation risk differs between groups.

welch_result <- t.test(
  automation_risk_score ~ ai_mentioned,
  data = test_data,
  alternative = "two.sided",
  var.equal = FALSE,
  conf.level = 0.95
)

cat("\nWELCH TWO-SAMPLE T-TEST\n")
print(welch_result)

# Save results
write.csv(
  group_summary,
  "hypothesis_group_summary.csv",
  row.names = FALSE
)

test_results <- data.frame(
  Test = "Welch two-sample t-test",
  t_statistic = unname(welch_result$statistic),
  degrees_of_freedom = unname(welch_result$parameter),
  p_value = welch_result$p.value,
  mean_difference = unname(diff(welch_result$estimate)),
  CI_lower = welch_result$conf.int[1],
  CI_upper = welch_result$conf.int[2]
)

write.csv(
  test_results,
  "hypothesis_test_results.csv",
  row.names = FALSE
)

# Boxplot
hypothesis_plot <- ggplot2::ggplot(
  test_data,
  ggplot2::aes(
    x = ai_mentioned,
    y = automation_risk_score,
    fill = ai_mentioned
  )
) +
  ggplot2::geom_boxplot(alpha = 0.75) +
  ggplot2::labs(
    title = "Automation Risk by AI Mention Status",
    x = "AI mention status",
    y = "Automation risk score"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none")

print(hypothesis_plot)

ggplot2::ggsave(
  "hypothesis_test_boxplot.png",
  plot = hypothesis_plot,
  width = 8,
  height = 5,
  dpi = 300
)

cat("\nHypothesis testing completed.\n")
cat("Group summary and test results saved as CSV files.\n")
cat("Boxplot saved as hypothesis_test_boxplot.png\n")


# Reload the cleaned dataset
library(readr)

file_path <- file.choose()

df <- read_csv(
  file_path,
  show_col_types = FALSE
)

# Verify that it loaded correctly
cat("Dataset loaded successfully!\n")
cat("Rows and columns:\n")
print(dim(df))

cat("\nDataset available:", exists("df"), "\n")




# ============================================================
# STEP 5.1: FEATURE INSPECTION
# ============================================================

# Confirm the dataset is available
stopifnot(exists("df"), is.data.frame(df))

# 1. Check AI mention and reskilling relationship
cat("\n1. AI MENTION VS RESKILLING REQUIRED\n")

print(table(
  AI_Mentioned = df$ai_mentioned,
  Reskilling_Required = df$reskilling_required,
  useNA = "ifany"
))

match_rate <- mean(
  as.character(df$ai_mentioned) ==
    as.character(df$reskilling_required),
  na.rm = TRUE
)

cat("\nProportion of matching values:\n")
print(match_rate)

# 2. Automation risk by AI mention status
cat("\n2. AUTOMATION RISK BY AI MENTION STATUS\n")

print(aggregate(
  automation_risk_score ~ ai_mentioned,
  data = df,
  FUN = function(x) {
    c(
      Count = length(x),
      Mean = mean(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE),
      Minimum = min(x, na.rm = TRUE),
      Maximum = max(x, na.rm = TRUE)
    )
  }
))

# 3. Displacement risk categories
cat("\n3. DISPLACEMENT RISK VS AUTOMATION RISK\n")

print(aggregate(
  automation_risk_score ~ ai_job_displacement_risk,
  data = df,
  FUN = function(x) {
    c(
      Count = length(x),
      Mean = mean(x, na.rm = TRUE),
      Minimum = min(x, na.rm = TRUE),
      Maximum = max(x, na.rm = TRUE)
    )
  }
))

# 4. Confirm identifier columns
cat("\n4. IDENTIFIER COLUMNS\n")
print(intersect(
  c("job_id", "company_name"),
  names(df)
))

cat("\nFeature inspection completed.\n")




# ============================================================
# DA2 - STEP 5.2: FEATURE ENGINEERING PREPARATION
# ============================================================

# Verify dataset
stopifnot(exists("df"), is.data.frame(df))
stopifnot("automation_risk_score" %in% names(df))

# 1. Inspect every column's data type
feature_info <- data.frame(
  Feature = names(df),
  Data_Type = vapply(
    df,
    function(x) class(x)[1],
    character(1)
  ),
  Unique_Values = vapply(
    df,
    function(x) length(unique(x[!is.na(x)])),
    integer(1)
  ),
  Missing_Values = vapply(
    df,
    function(x) sum(is.na(x)),
    integer(1)
  ),
  row.names = NULL
)

cat("\nFEATURE INFORMATION\n")
print(feature_info, row.names = FALSE)

# 2. Identify categorical columns
categorical_cols <- names(df)[
  vapply(df, function(x) is.character(x) ||
           is.factor(x) || is.logical(x), logical(1))
]

cat("\nCATEGORICAL / TEXT / LOGICAL COLUMNS\n")
print(categorical_cols)

# 3. Inspect the target variable
cat("\nTARGET VARIABLE SUMMARY\n")
print(summary(df$automation_risk_score))

# 4. Save feature information for documentation
write.csv(
  feature_info,
  "feature_inspection.csv",
  row.names = FALSE
)

cat("\nFeature inspection saved to feature_inspection.csv\n")



# ============================================================
# DA2 - STEP 5.3: INITIAL FEATURE SELECTION
# ============================================================

stopifnot(exists("df"), is.data.frame(df))
stopifnot("automation_risk_score" %in% names(df))

# Exclude identifiers and features requiring validation
excluded_features <- c(
  "job_id",
  "company_name",
  "ai_mentioned",
  "reskilling_required",
  "ai_job_displacement_risk"
)

# Retain the target and remaining candidate predictors
selected_columns <- setdiff(names(df), excluded_features)
model_data <- df[, selected_columns, drop = FALSE]

# Verify target and dataset
stopifnot("automation_risk_score" %in% names(model_data))
stopifnot(nrow(model_data) == nrow(df))

cat("\nINITIAL MODELING DATASET\n")
cat("Rows:", nrow(model_data), "\n")
cat("Columns:", ncol(model_data), "\n")

cat("\nSelected columns:\n")
print(names(model_data))

cat("\nExcluded columns:\n")
print(excluded_features)

# Save the initial feature set
write.csv(
  model_data,
  "initial_modeling_data.csv",
  row.names = FALSE
)

cat("\nInitial modeling dataset saved successfully.\n")


# ============================================================
# DA2 - STEP 5.4: FEATURE ENGINEERING
# ============================================================

# 1. Verify the initial modeling dataset
stopifnot(exists("model_data"))
stopifnot(is.data.frame(model_data))
stopifnot("automation_risk_score" %in% names(model_data))

# Create a separate copy; preserve original data
engineered_data <- model_data

# 2. Convert nominal categorical variables to factors
categorical_features <- c(
  "country",
  "region",
  "city",
  "company_size",
  "industry",
  "job_title",
  "seniority_level",
  "industry_ai_adoption_stage"
)

categorical_features <- intersect(
  categorical_features,
  names(engineered_data)
)

for (col in categorical_features) {
  engineered_data[[col]] <- as.factor(engineered_data[[col]])
}

# 3. Treat embedding cluster labels as categories
if ("job_description_embedding_cluster" %in%
    names(engineered_data)) {
  engineered_data$job_description_embedding_cluster <-
    as.factor(engineered_data$job_description_embedding_cluster)
}

# 4. Create text-length features from skill/keyword fields
text_features <- c("ai_keywords", "core_skills", "ai_skills")
text_features <- intersect(text_features, names(engineered_data))

for (col in text_features) {
  text_values <- as.character(engineered_data[[col]])
  text_values[is.na(text_values)] <- ""
  
  new_col <- paste0(col, "_char_length")
  engineered_data[[new_col]] <- nchar(text_values)
}

# 5. Confirm the target remains numeric
stopifnot(is.numeric(engineered_data$automation_risk_score))

# 6. Summarize engineered data
cat("\nENGINEERED DATASET DIMENSIONS\n")
print(dim(engineered_data))

cat("\nDATA TYPES AFTER ENGINEERING\n")
print(data.frame(
  Feature = names(engineered_data),
  Data_Type = vapply(
    engineered_data,
    function(x) class(x)[1],
    character(1)
  ),
  row.names = NULL
))

cat("\nTARGET SUMMARY\n")
print(summary(engineered_data$automation_risk_score))

# 7. Save engineered dataset
write.csv(
  engineered_data,
  "engineered_modeling_data.csv",
  row.names = FALSE
)

cat("\nFeature engineering completed.\n")
cat("Saved as engineered_modeling_data.csv\n")



# ============================================================
# DA2 - STEP 5.5: FINAL PREDICTOR PREPARATION
# ============================================================

# Verify that feature engineering has been completed
stopifnot(exists("engineered_data"))
stopifnot(is.data.frame(engineered_data))
stopifnot("automation_risk_score" %in% names(engineered_data))

# Target variable
target <- "automation_risk_score"

# Exclude raw text fields because their lengths were engineered
raw_text_fields <- intersect(
  c("ai_keywords", "core_skills", "ai_skills"),
  names(engineered_data)
)

# Create candidate predictor names
predictor_names <- setdiff(
  names(engineered_data),
  c(target, raw_text_fields)
)

# Ensure the target is not among the predictors
stopifnot(!target %in% predictor_names)

# Create a separate candidate dataset
model_candidates <- engineered_data[
  ,
  c(predictor_names, target),
  drop = FALSE
]

# Inspect candidate features
feature_selection_report <- data.frame(
  Feature = predictor_names,
  Data_Type = vapply(
    model_candidates[predictor_names],
    function(x) class(x)[1],
    character(1)
  ),
  Unique_Values = vapply(
    model_candidates[predictor_names],
    function(x) length(unique(x)),
    integer(1)
  ),
  row.names = NULL
)

cat("\nNUMBER OF CANDIDATE PREDICTORS\n")
print(length(predictor_names))

cat("\nCANDIDATE FEATURE LIST\n")
print(feature_selection_report, row.names = FALSE)

cat("\nTARGET VARIABLE\n")
print(target)

cat("\nCANDIDATE DATASET DIMENSIONS\n")
print(dim(model_candidates))

# Save the feature-selection report
write.csv(
  feature_selection_report,
  "candidate_feature_selection.csv",
  row.names = FALSE
)

write.csv(
  model_candidates,
  "model_candidates.csv",
  row.names = FALSE
)

cat("\nCandidate predictors and dataset saved successfully.\n")





# ============================================================
# DA2 - STEP 6: SQLITE DATABASE CONNECTIVITY
# ============================================================

library(DBI)
library(RSQLite)

stopifnot(exists("df"), is.data.frame(df))

# Create results folder
if (!dir.exists("results")) {
  dir.create("results", recursive = TRUE)
}

# Connect to the database
db_path <- file.path("results", "ai_job_market.db")
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)

# Store the cleaned dataset
DBI::dbWriteTable(
  conn = con,
  name = "ai_job_market",
  value = as.data.frame(df),
  overwrite = TRUE,
  row.names = FALSE
)

# Verify table creation
cat("\nDATABASE TABLES\n")
print(DBI::dbListTables(con))

# SQL query 1: Record count
count_result <- DBI::dbGetQuery(
  con,
  "SELECT COUNT(*) AS total_records
   FROM ai_job_market"
)

cat("\nRECORD COUNT\n")
print(count_result)

# SQL query 2: Sample records
sample_result <- DBI::dbGetQuery(
  con,
  "SELECT job_id, posting_year, industry,
          ai_intensity_score, automation_risk_score
   FROM ai_job_market
   LIMIT 10"
)

cat("\nSAMPLE RECORDS\n")
print(sample_result)

# SQL query 3: Average automation risk by industry
industry_result <- DBI::dbGetQuery(
  con,
  "SELECT industry,
          COUNT(*) AS job_postings,
          ROUND(AVG(automation_risk_score), 3)
            AS mean_automation_risk
   FROM ai_job_market
   GROUP BY industry
   ORDER BY mean_automation_risk DESC"
)

cat("\nINDUSTRY ANALYSIS\n")
print(industry_result)

# Save query results
write.csv(
  count_result,
  "results/database_record_count.csv",
  row.names = FALSE
)

write.csv(
  sample_result,
  "results/database_sample_records.csv",
  row.names = FALSE
)

write.csv(
  industry_result,
  "results/database_industry_analysis.csv",
  row.names = FALSE
)

# Close the connection
DBI::dbDisconnect(con)

cat("\nSTEP 6 COMPLETED SUCCESSFULLY\n")
cat("Database:", normalizePath(db_path), "\n")





# ============================================================
# DA2 - STEP 7: FINAL FEATURE VALIDATION AND DATA SPLITTING
# ============================================================

# Verify candidate dataset
stopifnot(exists("model_candidates"))
stopifnot(is.data.frame(model_candidates))
stopifnot("automation_risk_score" %in% names(model_candidates))

target <- "automation_risk_score"

# 1. Check target validity
stopifnot(is.numeric(model_candidates[[target]]))
stopifnot(all(is.finite(model_candidates[[target]])))

cat("\nTARGET VALIDATION\n")
cat("Target:", target, "\n")
cat("Observations:", nrow(model_candidates), "\n")
cat("Unique target values:",
    length(unique(model_candidates[[target]])), "\n")

# 2. Check numeric predictors for exact duplication
numeric_predictors <- names(model_candidates)[
  vapply(model_candidates, is.numeric, logical(1))
]
numeric_predictors <- setdiff(numeric_predictors, target)

exact_duplicates <- numeric_predictors[
  vapply(
    numeric_predictors,
    function(feature) {
      isTRUE(all.equal(
        model_candidates[[feature]],
        model_candidates[[target]],
        check.attributes = FALSE
      ))
    },
    logical(1)
  )
]

cat("\nNUMERIC PREDICTORS EXACTLY EQUAL TO TARGET\n")
if (length(exact_duplicates) == 0) {
  cat("None detected.\n")
} else {
  print(exact_duplicates)
}

# 3. Inspect numeric correlations with target
if (length(numeric_predictors) > 0) {
  correlations <- vapply(
    numeric_predictors,
    function(feature) {
      cor(
        model_candidates[[feature]],
        model_candidates[[target]],
        use = "complete.obs"
      )
    },
    numeric(1)
  )
  
  correlation_report <- data.frame(
    Feature = names(correlations),
    Correlation_With_Target = as.numeric(correlations),
    row.names = NULL
  )
  
  correlation_report <- correlation_report[
    order(abs(correlation_report$Correlation_With_Target),
          decreasing = TRUE),
  ]
  
  cat("\nNUMERIC CORRELATIONS WITH TARGET\n")
  print(correlation_report, row.names = FALSE)
  
  write.csv(
    correlation_report,
    "results/target_correlation_validation.csv",
    row.names = FALSE
  )
}

# 4. Inspect categorical feature cardinality
categorical_predictors <- names(model_candidates)[
  vapply(
    model_candidates,
    function(x) is.factor(x) || is.character(x),
    logical(1)
  )
]

if (length(categorical_predictors) > 0) {
  categorical_report <- data.frame(
    Feature = categorical_predictors,
    Unique_Values = vapply(
      model_candidates[categorical_predictors],
      function(x) length(unique(x)),
      integer(1)
    ),
    row.names = NULL
  )
  
  cat("\nCATEGORICAL PREDICTOR CHECK\n")
  print(categorical_report, row.names = FALSE)
  
  write.csv(
    categorical_report,
    "results/categorical_feature_validation.csv",
    row.names = FALSE
  )
}

# 5. Reproducible 80:20 train/test split
set.seed(123)

n <- nrow(model_candidates)
train_indices <- sample(
  seq_len(n),
  size = floor(0.80 * n)
)

train_data <- model_candidates[train_indices, , drop = FALSE]
test_data <- model_candidates[-train_indices, , drop = FALSE]

cat("\nTRAIN/TEST SPLIT\n")
cat("Training rows:", nrow(train_data), "\n")
cat("Testing rows:", nrow(test_data), "\n")

# 6. Save datasets and validation report
write.csv(
  train_data,
  "results/training_data.csv",
  row.names = FALSE
)

write.csv(
  test_data,
  "results/testing_data.csv",
  row.names = FALSE
)

cat("\nStep 7 completed.\n")
cat("Training and testing datasets saved in results/.\n")


# ============================================================
# STEP 7.1: TARGET LEAKAGE AND FEATURE REDUNDANCY CHECKS
# ============================================================

stopifnot(exists("df"), exists("engineered_data"))

# 1. Check whether the two text-length features are identical
cat("\n1. TEXT-LENGTH FEATURE REDUNDANCY\n")

text_pairs <- c(
  "ai_keywords_char_length",
  "ai_skills_char_length"
)

if (all(text_pairs %in% names(engineered_data))) {
  x <- engineered_data[[text_pairs[1]]]
  y <- engineered_data[[text_pairs[2]]]
  
  cat("Exactly identical:", identical(x, y), "\n")
  cat("Correlation:", cor(x, y), "\n")
  cat("Number of differing rows:", sum(x != y), "\n")
}

# 2. Examine AI intensity and automation risk
cat("\n2. AI INTENSITY VS AUTOMATION RISK\n")

print(summary(df$ai_intensity_score))
print(summary(df$automation_risk_score))

cat(
  "Pearson correlation:",
  cor(df$ai_intensity_score, df$automation_risk_score),
  "\n"
)

# Inspect mean risk across AI-intensity groups
intensity_groups <- cut(
  df$ai_intensity_score,
  breaks = 5,
  include.lowest = TRUE
)

intensity_summary <- aggregate(
  automation_risk_score ~ intensity_groups,
  data = df,
  FUN = function(x) {
    c(
      Count = length(x),
      Mean_Risk = mean(x),
      SD_Risk = sd(x)
    )
  }
)

cat("\nRISK BY AI-INTENSITY GROUP\n")
print(intensity_summary)

# 3. Confirm the AI mention/reskilling relationship
cat("\n3. AI MENTION VS RESKILLING\n")

print(table(
  AI_Mentioned = df$ai_mentioned,
  Reskilling_Required = df$reskilling_required
))

cat(
  "Proportion matching:",
  mean(df$ai_mentioned == df$reskilling_required),
  "\n"
)

# Save diagnostic output tables
write.csv(
  intensity_summary,
  "results/ai_intensity_risk_diagnostic.csv",
  row.names = FALSE
)

if (all(text_pairs %in% names(engineered_data))) {
  redundancy_result <- data.frame(
    Feature_1 = text_pairs[1],
    Feature_2 = text_pairs[2],
    Exactly_Identical = identical(x, y),
    Correlation = cor(x, y),
    Differing_Rows = sum(x != y)
  )
  
  write.csv(
    redundancy_result,
    "results/text_feature_redundancy.csv",
    row.names = FALSE
  )
}

cat("\nDiagnostic checks completed.\n")












# MODEL 1: MULTIPLE LINEAR REGRESSION

# 1. Select predictors for the first model
excluded_columns <- c(
  "ai_intensity_score",
  "ai_skills_char_length"
)

model1_data <- model_candidates[
  , !(names(model_candidates) %in% excluded_columns),
  drop = FALSE
]

# 2. Check the data
stopifnot("automation_risk_score" %in% names(model1_data))
stopifnot(!anyNA(model1_data))

cat("Dataset dimensions:", dim(model1_data), "\n")
cat("Target variable: automation_risk_score\n")

# 3. Create a reproducible 80:20 train-test split
set.seed(123)

train_index <- sample(
  seq_len(nrow(model1_data)),
  size = floor(0.80 * nrow(model1_data))
)

train_data <- model1_data[train_index, , drop = FALSE]
test_data  <- model1_data[-train_index, , drop = FALSE]

cat("Training records:", nrow(train_data), "\n")
cat("Testing records:", nrow(test_data), "\n")

# 4. Check for categorical levels present only in test data
factor_columns <- names(train_data)[
  vapply(train_data, is.factor, logical(1))
]

unseen_levels <- lapply(factor_columns, function(col) {
  setdiff(
    unique(as.character(test_data[[col]])),
    unique(as.character(train_data[[col]]))
  )
})

names(unseen_levels) <- factor_columns

unseen_levels <- unseen_levels[
  vapply(unseen_levels, length, integer(1)) > 0
]

if (length(unseen_levels) > 0) {
  print(unseen_levels)
  stop("Unseen test categories found. Resolve these before fitting.")
}

# 5. Train Multiple Linear Regression
model1 <- lm(
  automation_risk_score ~ .,
  data = train_data
)

cat("\nMODEL SUMMARY\n")
print(summary(model1))

# 6. Predict on the unseen test data
predictions <- predict(model1, newdata = test_data)
actual <- test_data$automation_risk_score

# 7. Calculate performance metrics
MAE <- mean(abs(actual - predictions))
RMSE <- sqrt(mean((actual - predictions)^2))

R_squared <- 1 -
  sum((actual - predictions)^2) /
  sum((actual - mean(actual))^2)

model1_metrics <- data.frame(
  Model = "Multiple Linear Regression",
  MAE = MAE,
  RMSE = RMSE,
  R_squared = R_squared
)

cat("\nTEST-SET PERFORMANCE\n")
print(model1_metrics)

# 8. Save the model and results
dir.create("results", showWarnings = FALSE)

write.csv(
  model1_metrics,
  "results/model1_metrics.csv",
  row.names = FALSE
)

write.csv(
  data.frame(
    Actual = actual,
    Predicted = as.numeric(predictions)
  ),
  "results/model1_predictions.csv",
  row.names = FALSE
)

saveRDS(
  model1,
  "results/model1_linear_regression.rds"
)

cat("\nModel, metrics, and predictions saved in results/.\n")


# MODEL 2: DECISION TREE REGRESSION

# 1. Check that the required package is available
if (!requireNamespace("rpart", quietly = TRUE)) {
  install.packages("rpart")
}

# 2. Train the Decision Tree model
set.seed(123)

model2 <- rpart::rpart(
  automation_risk_score ~ .,
  data = train_data,
  method = "anova",
  control = rpart::rpart.control(
    cp = 0.01,
    minsplit = 20,
    maxdepth = 10
  )
)

# 3. Display the model summary
print(model2)
printcp(model2)

# 4. Predict on the same test data
predictions2 <- predict(model2, newdata = test_data)
actual <- test_data$automation_risk_score

# 5. Calculate evaluation metrics
MAE2 <- mean(abs(actual - predictions2))
RMSE2 <- sqrt(mean((actual - predictions2)^2))

R_squared2 <- 1 -
  sum((actual - predictions2)^2) /
  sum((actual - mean(actual))^2)

model2_metrics <- data.frame(
  Model = "Decision Tree Regression",
  MAE = MAE2,
  RMSE = RMSE2,
  R_squared = R_squared2
)

# 6. Display test performance
cat("\nMODEL 2: TEST-SET PERFORMANCE\n")
print(model2_metrics)

# 7. Save results
dir.create("results", showWarnings = FALSE)

write.csv(
  model2_metrics,
  "results/model2_metrics.csv",
  row.names = FALSE
)

write.csv(
  data.frame(
    Actual = actual,
    Predicted = as.numeric(predictions2)
  ),
  "results/model2_predictions.csv",
  row.names = FALSE
)

saveRDS(
  model2,
  "results/model2_decision_tree.rds"
)

cat("\nModel 2 results saved in the results folder.\n")

rpart::printcp(model2)










# Predict on the test data
predictions2 <- predict(model2, newdata = test_data)
actual <- test_data$automation_risk_score

# Calculate evaluation metrics
MAE2 <- mean(abs(actual - predictions2))
RMSE2 <- sqrt(mean((actual - predictions2)^2))

R_squared2 <- 1 -
  sum((actual - predictions2)^2) /
  sum((actual - mean(actual))^2)

model2_metrics <- data.frame(
  Model = "Decision Tree Regression",
  MAE = MAE2,
  RMSE = RMSE2,
  R_squared = R_squared2
)

# Display results
cat("\nMODEL 2: TEST-SET PERFORMANCE\n")
print(model2_metrics)

# Save metrics and predictions
dir.create("results", showWarnings = FALSE)

write.csv(
  model2_metrics,
  "results/model2_metrics.csv",
  row.names = FALSE
)

write.csv(
  data.frame(
    Actual = actual,
    Predicted = as.numeric(predictions2)
  ),
  "results/model2_predictions.csv",
  row.names = FALSE
)

saveRDS(model2, "results/model2_decision_tree.rds")

cat("\nModel 2 results saved successfully.\n")



requireNamespace("randomForest", quietly = TRUE)
# MODEL 3: RANDOM FOREST REGRESSION

# 1. Install the package if needed
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}

# 2. Train the Random Forest model
set.seed(123)

model3 <- randomForest::randomForest(
  automation_risk_score ~ .,
  data = train_data,
  ntree = 300,
  mtry = max(1, floor(sqrt(ncol(train_data) - 1))),
  importance = TRUE
)

# 3. Display model information
print(model3)

# 4. Predict on the test data
predictions3 <- predict(model3, newdata = test_data)
actual <- test_data$automation_risk_score

# 5. Calculate evaluation metrics
MAE3 <- mean(abs(actual - predictions3))
RMSE3 <- sqrt(mean((actual - predictions3)^2))

R_squared3 <- 1 -
  sum((actual - predictions3)^2) /
  sum((actual - mean(actual))^2)

model3_metrics <- data.frame(
  Model = "Random Forest Regression",
  MAE = MAE3,
  RMSE = RMSE3,
  R_squared = R_squared3
)

# 6. Display results
cat("\nMODEL 3: TEST-SET PERFORMANCE\n")
print(model3_metrics)

# 7. Display predictor importance
cat("\nPREDICTOR IMPORTANCE\n")
print(randomForest::importance(model3))

# 8. Save results
dir.create("results", showWarnings = FALSE)

write.csv(
  model3_metrics,
  "results/model3_metrics.csv",
  row.names = FALSE
)

write.csv(
  data.frame(
    Actual = actual,
    Predicted = as.numeric(predictions3)
  ),
  "results/model3_predictions.csv",
  row.names = FALSE
)

write.csv(
  randomForest::importance(model3),
  "results/model3_feature_importance.csv"
)

saveRDS(model3, "results/model3_random_forest.rds")

cat("\nModel 3 results saved in the results folder.\n")




# INVESTIGATE KEYWORD LENGTH AND AUTOMATION RISK

# 1. Calculate keyword text length from the original column
keyword_length <- nchar(df$ai_keywords)

# 2. Check correlation with the target
cat("Correlation between keyword length and risk score:\n")

print(cor(
  keyword_length,
  df$automation_risk_score,
  use = "complete.obs"
))

# 3. Summarize risk scores by keyword length
keyword_risk_summary <- aggregate(
  automation_risk_score ~ keyword_length,
  data = data.frame(
    keyword_length = keyword_length,
    automation_risk_score = df$automation_risk_score
  ),
  FUN = mean
)

# Show the first 15 keyword lengths
print(head(keyword_risk_summary, 15))

# 4. Visualize the relationship
plot(
  keyword_length,
  df$automation_risk_score,
  xlab = "AI keyword text length (characters)",
  ylab = "Automation risk score",
  main = "Keyword Length vs Automation Risk",
  pch = 16
)

# 5. Save the summary
write.csv(
  keyword_risk_summary,
  "results/keyword_length_risk_summary.csv",
  row.names = FALSE
)


# Remove keyword length from the modelling dataset
model_candidates <- model_candidates[
  , !(names(model_candidates) %in% "ai_keywords_char_length"),
  drop = FALSE
]

# Verify removal
"ai_keywords_char_length" %in% names(model_candidates)



set.seed(123)

train_index <- sample(
  seq_len(nrow(model_candidates)),
  size = floor(0.80 * nrow(model_candidates))
)

train_data <- model_candidates[
  train_index, , drop = FALSE
]

test_data <- model_candidates[
  -train_index, , drop = FALSE
]

cat("Training records:", nrow(train_data), "\n")
cat("Testing records:", nrow(test_data), "\n")

write.csv(
  train_data,
  "results/training_data.csv",
  row.names = FALSE
)

write.csv(
  test_data,
  "results/testing_data.csv",
  row.names = FALSE
)



# Verify the updated modelling data
cat("Dataset dimensions:", dim(model_candidates), "\n")

cat(
  "Keyword length present:",
  "ai_keywords_char_length" %in% names(model_candidates),
  "\n"
)

cat(
  "Skills length present:",
  "ai_skills_char_length" %in% names(model_candidates),
  "\n"
)

cat("Training records:", nrow(train_data), "\n")
cat("Testing records:", nrow(test_data), "\n")


# Remove the duplicate skills-length feature
model_candidates <- model_candidates[
  , !(names(model_candidates) %in% "ai_skills_char_length"),
  drop = FALSE
]

# Recreate the train-test split
set.seed(123)

train_index <- sample(
  seq_len(nrow(model_candidates)),
  size = floor(0.80 * nrow(model_candidates))
)

train_data <- model_candidates[
  train_index, , drop = FALSE
]

test_data <- model_candidates[
  -train_index, , drop = FALSE
]

# Verify
cat("Dataset dimensions:", dim(model_candidates), "\n")
cat("Training records:", nrow(train_data), "\n")
cat("Testing records:", nrow(test_data), "\n")
cat(
  "Duplicate feature present:",
  "ai_skills_char_length" %in% names(model_candidates),
  "\n"
)

# Save the updated datasets
write.csv(
  train_data,
  "results/training_data.csv",
  row.names = FALSE
)

write.csv(
  test_data,
  "results/testing_data.csv",
  row.names = FALSE
)



# MODEL 4: RIDGE REGRESSION

# 1. Prepare predictors
ridge_data <- model_candidates[
  , !(names(model_candidates) %in%
        c("ai_keywords_char_length",
          "ai_skills_char_length")),
  drop = FALSE
]

# 2. Install required packages if missing
if (!requireNamespace("glmnet", quietly = TRUE)) {
  install.packages("glmnet")
}

if (!requireNamespace("Matrix", quietly = TRUE)) {
  install.packages("Matrix")
}

# 3. Create reproducible train-test split
set.seed(123)

train_index4 <- sample(
  seq_len(nrow(ridge_data)),
  size = floor(0.80 * nrow(ridge_data))
)

train4 <- ridge_data[train_index4, , drop = FALSE]
test4  <- ridge_data[-train_index4, , drop = FALSE]

# 4. Convert predictors into numeric model matrices
x_train <- model.matrix(
  automation_risk_score ~ .,
  data = train4
)[, -1, drop = FALSE]

x_test <- model.matrix(
  automation_risk_score ~ .,
  data = test4
)[, -1, drop = FALSE]

# Ensure train and test columns match
x_test <- x_test[
  , colnames(x_train), drop = FALSE
]

y_train <- train4$automation_risk_score
y_test <- test4$automation_risk_score

# 5. Train Ridge Regression
set.seed(123)

model4 <- glmnet::cv.glmnet(
  x = x_train,
  y = y_train,
  alpha = 0,
  nfolds = 5,
  standardize = TRUE
)

# 6. Predict test scores
predictions4 <- as.numeric(
  predict(model4, newx = x_test, s = "lambda.min")
)

# 7. Calculate evaluation metrics
MAE4 <- mean(abs(y_test - predictions4))
RMSE4 <- sqrt(mean((y_test - predictions4)^2))

R_squared4 <- 1 -
  sum((y_test - predictions4)^2) /
  sum((y_test - mean(y_test))^2)

model4_metrics <- data.frame(
  Model = "Ridge Regression",
  MAE = MAE4,
  RMSE = RMSE4,
  R_squared = R_squared4
)

cat("\nMODEL 4: TEST-SET PERFORMANCE\n")
print(model4_metrics)

# 8. Save results
dir.create("results", showWarnings = FALSE)

write.csv(
  model4_metrics,
  "results/model4_metrics.csv",
  row.names = FALSE
)

write.csv(
  data.frame(Actual = y_test, Predicted = predictions4),
  "results/model4_predictions.csv",
  row.names = FALSE
)

saveRDS(model4, "results/model4_ridge.rds")

cat("\nModel 4 results saved.\n")





# MODEL 5: LASSO REGRESSION

if (!requireNamespace("glmnet", quietly = TRUE)) {
  install.packages("glmnet")
}

# Use the same revised data and split as Model 4
set.seed(123)

model5 <- glmnet::cv.glmnet(
  x = x_train,
  y = y_train,
  alpha = 1,
  nfolds = 5,
  standardize = TRUE
)

# Predict on the test set
predictions5 <- as.numeric(
  predict(model5, newx = x_test, s = "lambda.min")
)

# Calculate metrics
MAE5 <- mean(abs(y_test - predictions5))
RMSE5 <- sqrt(mean((y_test - predictions5)^2))

R_squared5 <- 1 -
  sum((y_test - predictions5)^2) /
  sum((y_test - mean(y_test))^2)

model5_metrics <- data.frame(
  Model = "Lasso Regression",
  MAE = MAE5,
  RMSE = RMSE5,
  R_squared = R_squared5
)

cat("\nMODEL 5: TEST-SET PERFORMANCE\n")
print(model5_metrics)

# Save outputs
write.csv(
  model5_metrics,
  "results/model5_metrics.csv",
  row.names = FALSE
)

write.csv(
  data.frame(Actual = y_test, Predicted = predictions5),
  "results/model5_predictions.csv",
  row.names = FALSE
)

saveRDS(model5, "results/model5_lasso.rds")

cat("\nModel 5 results saved.\n")











# MODEL 6: K-NEAREST NEIGHBORS REGRESSION

# Install package if needed
if (!requireNamespace("FNN", quietly = TRUE)) {
  install.packages("FNN")
}

# 1. Scale predictors using training-set statistics only
train_center <- colMeans(x_train)
train_scale <- apply(x_train, 2, sd)

# Remove columns with zero or invalid standard deviation
valid_cols <- is.finite(train_scale) & train_scale > 0

x_train6 <- x_train[, valid_cols, drop = FALSE]
x_test6  <- x_test[, valid_cols, drop = FALSE]

train_center <- train_center[valid_cols]
train_scale  <- train_scale[valid_cols]

x_train6 <- scale(
  x_train6,
  center = train_center,
  scale = train_scale
)

x_test6 <- scale(
  x_test6,
  center = train_center,
  scale = train_scale
)

# 2. Train and predict using KNN regression
# Use the 5 nearest training records
set.seed(123)

predictions6 <- FNN::knn.reg(
  train = x_train6,
  test = x_test6,
  y = y_train,
  k = 5
)$pred

# 3. Calculate test-set metrics
MAE6 <- mean(abs(y_test - predictions6))
RMSE6 <- sqrt(mean((y_test - predictions6)^2))

R_squared6 <- 1 -
  sum((y_test - predictions6)^2) /
  sum((y_test - mean(y_test))^2)

model6_metrics <- data.frame(
  Model = "K-Nearest Neighbors Regression",
  MAE = MAE6,
  RMSE = RMSE6,
  R_squared = R_squared6
)

cat("\nMODEL 6: TEST-SET PERFORMANCE\n")
print(model6_metrics)

# 4. Save results
dir.create("results", showWarnings = FALSE)

write.csv(
  model6_metrics,
  "results/model6_metrics.csv",
  row.names = FALSE
)

write.csv(
  data.frame(
    Actual = y_test,
    Predicted = as.numeric(predictions6)
  ),
  "results/model6_predictions.csv",
  row.names = FALSE
)

cat("\nModel 6 metrics and predictions saved.\n")




# MODEL 7: SUPPORT VECTOR REGRESSION (SVR)

# 1. Install package if needed
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071")
}

# 2. Train SVR with an RBF kernel
set.seed(123)

model7 <- e1071::svm(
  x = x_train,
  y = y_train,
  type = "eps-regression",
  kernel = "radial",
  cost = 1,
  epsilon = 0.1,
  scale = TRUE
)

# 3. Predict on the test set
predictions7 <- as.numeric(
  predict(model7, newdata = x_test)
)

# 4. Calculate test metrics
MAE7 <- mean(abs(y_test - predictions7))
RMSE7 <- sqrt(mean((y_test - predictions7)^2))

R_squared7 <- 1 -
  sum((y_test - predictions7)^2) /
  sum((y_test - mean(y_test))^2)

model7_metrics <- data.frame(
  Model = "Support Vector Regression",
  MAE = MAE7,
  RMSE = RMSE7,
  R_squared = R_squared7
)

cat("\nMODEL 7: TEST-SET PERFORMANCE\n")
print(model7_metrics)

# 5. Save results
dir.create("results", showWarnings = FALSE)

write.csv(
  model7_metrics,
  "results/model7_metrics.csv",
  row.names = FALSE
)

write.csv(
  data.frame(
    Actual = y_test,
    Predicted = predictions7
  ),
  "results/model7_predictions.csv",
  row.names = FALSE
)

saveRDS(model7, "results/model7_svr.rds")

cat("\nModel 7 results saved.\n")












# FAIR COMPARISON: PREPARE COMMON DATASET

# 1. Remove the two redundant text-length features
fair_data <- model_candidates[
  , !(names(model_candidates) %in%
        c("ai_keywords_char_length",
          "ai_skills_char_length")),
  drop = FALSE
]

# 2. Verify the target and missing values
stopifnot("automation_risk_score" %in% names(fair_data))
stopifnot(!anyNA(fair_data))

# 3. Create one common 80:20 split
set.seed(123)

fair_index <- sample(
  seq_len(nrow(fair_data)),
  size = floor(0.80 * nrow(fair_data))
)

fair_train <- fair_data[fair_index, , drop = FALSE]
fair_test  <- fair_data[-fair_index, , drop = FALSE]

# 4. Check dimensions
cat("Full dataset:", dim(fair_data), "\n")
cat("Training records:", nrow(fair_train), "\n")
cat("Testing records:", nrow(fair_test), "\n")

# 5. Save the common split
dir.create("results", showWarnings = FALSE)

write.csv(
  fair_train,
  "results/fair_training_data.csv",
  row.names = FALSE
)

write.csv(
  fair_test,
  "results/fair_testing_data.csv",
  row.names = FALSE
)

cat("Common comparison split saved.\n")




# FAIR COMPARISON: MODELS 1-3

# Model 1: Multiple Linear Regression
fair_model1 <- lm(
  automation_risk_score ~ .,
  data = fair_train
)

fair_pred1 <- predict(fair_model1, newdata = fair_test)


# Model 2: Decision Tree Regression
if (!requireNamespace("rpart", quietly = TRUE)) {
  install.packages("rpart")
}

fair_model2 <- rpart::rpart(
  automation_risk_score ~ .,
  data = fair_train,
  method = "anova",
  control = rpart::rpart.control(
    cp = 0.01,
    minsplit = 20,
    maxdepth = 10
  )
)

fair_pred2 <- predict(fair_model2, newdata = fair_test)


# Model 3: Random Forest Regression
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}

set.seed(123)

fair_model3 <- randomForest::randomForest(
  automation_risk_score ~ .,
  data = fair_train,
  ntree = 300,
  mtry = max(1, floor(sqrt(ncol(fair_train) - 1))),
  importance = TRUE
)

fair_pred3 <- predict(fair_model3, newdata = fair_test)


# Calculate common test metrics
calculate_metrics <- function(actual, predicted, model_name) {
  data.frame(
    Model = model_name,
    MAE = mean(abs(actual - predicted)),
    RMSE = sqrt(mean((actual - predicted)^2)),
    R_squared = 1 -
      sum((actual - predicted)^2) /
      sum((actual - mean(actual))^2)
  )
}

actual <- fair_test$automation_risk_score

fair_metrics_1_3 <- rbind(
  calculate_metrics(
    actual, fair_pred1, "Multiple Linear Regression"
  ),
  calculate_metrics(
    actual, fair_pred2, "Decision Tree Regression"
  ),
  calculate_metrics(
    actual, fair_pred3, "Random Forest Regression"
  )
)

cat("\nFAIR COMPARISON: MODELS 1-3\n")
print(fair_metrics_1_3)

# Save results without overwriting original models
write.csv(
  fair_metrics_1_3,
  "results/fair_models1_3_metrics.csv",
  row.names = FALSE
)

saveRDS(
  fair_model1,
  "results/fair_model1_linear_regression.rds"
)

saveRDS(
  fair_model2,
  "results/fair_model2_decision_tree.rds"
)

saveRDS(
  fair_model3,
  "results/fair_model3_random_forest.rds"
)

cat("\nFair-comparison models 1-3 saved.\n")




# FAIR COMPARISON: RETRAIN MODELS 4-7

# 1. Create consistent numeric predictor matrices
# The same encoding is used for training and testing.
all_x <- model.matrix(
  automation_risk_score ~ .,
  data = fair_data
)[, -1, drop = FALSE]

x_train_fair <- all_x[fair_index, , drop = FALSE]
x_test_fair  <- all_x[-fair_index, , drop = FALSE]

y_train_fair <- fair_train$automation_risk_score
y_test_fair  <- fair_test$automation_risk_score

# Remove predictors with zero variance in training data
valid_cols <- apply(x_train_fair, 2, sd) > 0

x_train_fair <- x_train_fair[, valid_cols, drop = FALSE]
x_test_fair  <- x_test_fair[, valid_cols, drop = FALSE]

# 2. Model 4: Ridge Regression
set.seed(123)

fair_model4 <- glmnet::cv.glmnet(
  x_train_fair, y_train_fair,
  alpha = 0, nfolds = 5, standardize = TRUE
)

fair_pred4 <- as.numeric(predict(
  fair_model4, newx = x_test_fair, s = "lambda.min"
))


# 3. Model 5: Lasso Regression
set.seed(123)

fair_model5 <- glmnet::cv.glmnet(
  x_train_fair, y_train_fair,
  alpha = 1, nfolds = 5, standardize = TRUE
)

fair_pred5 <- as.numeric(predict(
  fair_model5, newx = x_test_fair, s = "lambda.min"
))


# 4. Model 6: KNN Regression
# Scale using training-set statistics only
train_center <- colMeans(x_train_fair)
train_scale <- apply(x_train_fair, 2, sd)

x_train_scaled <- scale(
  x_train_fair,
  center = train_center,
  scale = train_scale
)

x_test_scaled <- scale(
  x_test_fair,
  center = train_center,
  scale = train_scale
)

set.seed(123)

fair_pred6 <- FNN::knn.reg(
  train = x_train_scaled,
  test = x_test_scaled,
  y = y_train_fair,
  k = 5
)$pred


# 5. Model 7: Support Vector Regression
set.seed(123)

fair_model7 <- e1071::svm(
  x = x_train_fair,
  y = y_train_fair,
  type = "eps-regression",
  kernel = "radial",
  cost = 1,
  epsilon = 0.1,
  scale = TRUE
)

fair_pred7 <- as.numeric(predict(
  fair_model7, newdata = x_test_fair
))


# 6. Calculate identical test metrics for every model
calculate_fair_metrics <- function(actual, predicted, model_name) {
  data.frame(
    Model = model_name,
    MAE = mean(abs(actual - predicted)),
    RMSE = sqrt(mean((actual - predicted)^2)),
    R_squared = 1 -
      sum((actual - predicted)^2) /
      sum((actual - mean(actual))^2)
  )
}

fair_metrics_4_7 <- rbind(
  calculate_fair_metrics(
    y_test_fair, fair_pred4, "Ridge Regression"
  ),
  calculate_fair_metrics(
    y_test_fair, fair_pred5, "Lasso Regression"
  ),
  calculate_fair_metrics(
    y_test_fair, fair_pred6, "KNN Regression"
  ),
  calculate_fair_metrics(
    y_test_fair, fair_pred7, "Support Vector Regression"
  )
)

cat("\nFAIR COMPARISON: MODELS 4-7\n")
print(fair_metrics_4_7)

# 7. Save fair-comparison metrics and predictions
dir.create("results", showWarnings = FALSE)

write.csv(
  fair_metrics_4_7,
  "results/fair_models4_7_metrics.csv",
  row.names = FALSE
)

fair_predictions <- data.frame(
  Actual = y_test_fair,
  Ridge = fair_pred4,
  Lasso = fair_pred5,
  KNN = fair_pred6,
  SVR = fair_pred7
)

write.csv(
  fair_predictions,
  "results/fair_models4_7_predictions.csv",
  row.names = FALSE
)

saveRDS(fair_model4, "results/fair_model4_ridge.rds")
saveRDS(fair_model5, "results/fair_model5_lasso.rds")
saveRDS(fair_pred6, "results/fair_model6_knn_predictions.rds")
saveRDS(fair_model7, "results/fair_model7_svr.rds")

cat("\nFair-comparison Models 4-7 saved.\n")




cat(
  "Training rows match:",
  identical(rownames(fair_train), rownames(train4)),
  "\n"
)

cat(
  "Testing rows match:",
  identical(rownames(fair_test), rownames(test4)),
  "\n"
)




list.files("results", pattern = "fair_model[4-7].*\\.rds$")




# Verify saved fair-comparison predictions

fair_pred_check <- read.csv(
  "results/fair_models4_7_predictions.csv"
)

cat("Test records:", nrow(fair_pred_check), "\n")
cat("Expected test records:", nrow(fair_test), "\n")

cat(
  "Actual scores match common test set:",
  isTRUE(all.equal(
    fair_pred_check$Actual,
    fair_test$automation_risk_score
  )),
  "\n"
)

cat(
  "Ridge predictions saved:",
  file.exists("results/fair_model4_ridge.rds"),
  "\n"
)

cat(
  "Lasso predictions saved:",
  file.exists("results/fair_model5_lasso.rds"),
  "\n"
)

cat(
  "SVR model saved:",
  file.exists("results/fair_model7_svr.rds"),
  "\n"
)










# Recalculate metrics from saved predictions

fair_pred_check <- read.csv(
  "results/fair_models4_7_predictions.csv"
)

calculate_metrics <- function(actual, predicted, model_name) {
  data.frame(
    Model = model_name,
    MAE = mean(abs(actual - predicted)),
    RMSE = sqrt(mean((actual - predicted)^2)),
    R_squared = 1 -
      sum((actual - predicted)^2) /
      sum((actual - mean(actual))^2)
  )
}

verified_metrics_4_7 <- rbind(
  calculate_metrics(
    fair_pred_check$Actual,
    fair_pred_check$Ridge,
    "Ridge Regression"
  ),
  calculate_metrics(
    fair_pred_check$Actual,
    fair_pred_check$Lasso,
    "Lasso Regression"
  ),
  calculate_metrics(
    fair_pred_check$Actual,
    fair_pred_check$KNN,
    "KNN Regression"
  ),
  calculate_metrics(
    fair_pred_check$Actual,
    fair_pred_check$SVR,
    "Support Vector Regression"
  )
)

print(verified_metrics_4_7)

write.csv(
  verified_metrics_4_7,
  "results/verified_fair_metrics_4_7.csv",
  row.names = FALSE
)# Recalculate metrics from saved predictions

fair_pred_check <- read.csv(
  "results/fair_models4_7_predictions.csv"
)

calculate_metrics <- function(actual, predicted, model_name) {
  data.frame(
    Model = model_name,
    MAE = mean(abs(actual - predicted)),
    RMSE = sqrt(mean((actual - predicted)^2)),
    R_squared = 1 -
      sum((actual - predicted)^2) /
      sum((actual - mean(actual))^2)
  )
}

verified_metrics_4_7 <- rbind(
  calculate_metrics(
    fair_pred_check$Actual,
    fair_pred_check$Ridge,
    "Ridge Regression"
  ),
  calculate_metrics(
    fair_pred_check$Actual,
    fair_pred_check$Lasso,
    "Lasso Regression"
  ),
  calculate_metrics(
    fair_pred_check$Actual,
    fair_pred_check$KNN,
    "KNN Regression"
  ),
  calculate_metrics(
    fair_pred_check$Actual,
    fair_pred_check$SVR,
    "Support Vector Regression"
  )
)

print(verified_metrics_4_7)

write.csv(
  verified_metrics_4_7,
  "results/verified_fair_metrics_4_7.csv",
  row.names = FALSE
)












cat(
  "Keyword length present:",
  "ai_keywords_char_length" %in% names(fair_data),
  "\n"
)

cat(
  "Skills length present:",
  "ai_skills_char_length" %in% names(fair_data),
  "\n"
)

cat(
  "Predictor count:",
  ncol(fair_data) - 1,
  "\n"
)



























# FINAL MODEL COMPARISON

all_model_metrics <- rbind(
  fair_metrics_1_3,
  verified_metrics_4_7
)

# Sort by RMSE (lower is better)
all_model_metrics <- all_model_metrics[
  order(all_model_metrics$RMSE),
]

print(all_model_metrics, row.names = FALSE)

# Save final comparison table
write.csv(
  all_model_metrics,
  "results/all_seven_models_comparison.csv",
  row.names = FALSE
)

# Compare R-squared across all models
barplot(
  all_model_metrics$R_squared * 100,
  names.arg = all_model_metrics$Model,
  las = 2,
  cex.names = 0.7,
  ylab = "R-squared (%)",
  main = "Comparison of Seven Regression Models"
)

# Save the graph
png(
  "results/seven_model_r_squared_comparison.png",
  width = 1200,
  height = 800
)

barplot(
  all_model_metrics$R_squared * 100,
  names.arg = all_model_metrics$Model,
  las = 2,
  cex.names = 0.7,
  ylab = "R-squared (%)",
  main = "Comparison of Seven Regression Models"
)

dev.off()

cat("\nComparison table and graph saved.\n")



















# CHECK 1: Confirm the revised features are absent
cat("Keyword length present:",
    "ai_keywords_char_length" %in% names(fair_train), "\n")

cat("Skills length present:",
    "ai_skills_char_length" %in% names(fair_train), "\n")

# CHECK 2: Confirm all models should use the same test records
cat("Common test records:", nrow(fair_test), "\n")

# CHECK 3: Compare old and new metric values
cat("\nOriginal Model 4 metrics:\n")
print(model4_metrics)

cat("\nFair Model 4 metrics:\n")
print(fair_metrics_4_7[1, ])

cat("\nOriginal Model 5 metrics:\n")
print(model5_metrics)

cat("\nFair Model 5 metrics:\n")
print(fair_metrics_4_7[2, ])

# CHECK 4: Verify the fair model's predictor matrix
cat("\nFair predictor matrix dimensions:\n")
print(dim(x_train_fair))
print(dim(x_test_fair))

cat("\nVerification checks completed.\n")







# Check whether the fair models were trained on the revised data

cat(
  "Fair Ridge model exists:",
  exists("fair_model4"),
  "\n"
)

cat(
  "Fair Lasso model exists:",
  exists("fair_model5"),
  "\n"
)

cat(
  "Ridge lambda selected:",
  fair_model4$lambda.min,
  "\n"
)

cat(
  "Lasso lambda selected:",
  fair_model5$lambda.min,
  "\n"
)

# Check the revised training data
cat(
  "Redundant features removed:",
  !any(c(
    "ai_keywords_char_length",
    "ai_skills_char_length"
  ) %in% names(fair_train)),
  "\n"
)



















# RETRAIN FAIR RIDGE AND LASSO MODELS

set.seed(123)
fair_model4 <- glmnet::cv.glmnet(
  x = x_train_fair,
  y = y_train_fair,
  alpha = 0,
  nfolds = 5,
  standardize = TRUE
)

fair_pred4 <- as.numeric(predict(
  fair_model4,
  newx = x_test_fair,
  s = "lambda.min"
))

set.seed(123)
fair_model5 <- glmnet::cv.glmnet(
  x = x_train_fair,
  y = y_train_fair,
  alpha = 1,
  nfolds = 5,
  standardize = TRUE
)

fair_pred5 <- as.numeric(predict(
  fair_model5,
  newx = x_test_fair,
  s = "lambda.min"
))

# Recalculate metrics
fair_metrics_4_5 <- rbind(
  calculate_fair_metrics(
    y_test_fair, fair_pred4, "Ridge Regression"
  ),
  calculate_fair_metrics(
    y_test_fair, fair_pred5, "Lasso Regression"
  )
)

print(fair_metrics_4_5)

# Save corrected fair models
saveRDS(fair_model4, "results/fair_model4_ridge.rds")
saveRDS(fair_model5, "results/fair_model5_lasso.rds")

write.csv(
  data.frame(
    Actual = y_test_fair,
    Ridge = fair_pred4,
    Lasso = fair_pred5
  ),
  "results/fair_models4_5_predictions.csv",
  row.names = FALSE
)





# FAIR MODEL 6: KNN REGRESSION

set.seed(123)

fair_pred6 <- FNN::knn.reg(
  train = x_train_scaled,
  test = x_test_scaled,
  y = y_train_fair,
  k = 5
)$pred

# FAIR MODEL 7: SUPPORT VECTOR REGRESSION

set.seed(123)

fair_model7 <- e1071::svm(
  x = x_train_fair,
  y = y_train_fair,
  type = "eps-regression",
  kernel = "radial",
  cost = 1,
  epsilon = 0.1,
  scale = TRUE
)

fair_pred7 <- as.numeric(
  predict(fair_model7, newdata = x_test_fair)
)

# Calculate metrics
fair_metrics_6_7 <- rbind(
  calculate_fair_metrics(
    y_test_fair, fair_pred6, "KNN Regression"
  ),
  calculate_fair_metrics(
    y_test_fair, fair_pred7, "Support Vector Regression"
  )
)

print(fair_metrics_6_7)

# Save updated results
saveRDS(fair_pred6, "results/fair_model6_knn_predictions.rds")
saveRDS(fair_model7, "results/fair_model7_svr.rds")

write.csv(
  data.frame(
    Actual = y_test_fair,
    KNN = fair_pred6,
    SVR = fair_pred7
  ),
  "results/fair_models6_7_predictions.csv",
  row.names = FALSE
)



# FINAL COMPARISON OF ALL SEVEN MODELS

all_model_metrics <- rbind(
  fair_metrics_1_3,
  fair_metrics_4_5,
  fair_metrics_6_7
)

# Sort by RMSE (lower is better)
all_model_metrics <- all_model_metrics[
  order(all_model_metrics$RMSE),
]

cat("\nFINAL SEVEN-MODEL COMPARISON\n")
print(all_model_metrics, row.names = FALSE)

# Save the comparison table
write.csv(
  all_model_metrics,
  "results/all_seven_models_comparison.csv",
  row.names = FALSE
)

# Save a comparison graph
png(
  "results/seven_model_comparison.png",
  width = 1400,
  height = 900
)

par(mar = c(10, 5, 4, 2))

barplot(
  all_model_metrics$R_squared * 100,
  names.arg = all_model_metrics$Model,
  las = 2,
  cex.names = 0.8,
  ylab = "R-squared (%)",
  main = "Comparison of Seven Regression Models"
)

dev.off()

cat("\nComparison table and graph saved.\n")



# Save the selected Decision Tree model
saveRDS(
  fair_model2,
  "results/final_decision_tree_model.rds"
)

# Confirm it was saved
cat(
  "Final model saved:",
  file.exists("results/final_decision_tree_model.rds"),
  "\n"
)




























# Save the selected Decision Tree model
saveRDS(
  fair_model2,
  "results/final_decision_tree_model.rds"
)

# Confirm it was saved
cat(
  "Final model saved:",
  file.exists("results/final_decision_tree_model.rds"),
  "\n"
)

# Test prediction on one held-out record
final_model <- readRDS(
  "results/final_decision_tree_model.rds"
)

sample_job <- fair_test[1, , drop = FALSE]

actual_score <- sample_job$automation_risk_score

sample_input <- sample_job[
  , names(sample_job) != "automation_risk_score",
  drop = FALSE
]

predicted_score <- predict(
  final_model,
  newdata = sample_input
)

cat("Actual risk score:", actual_score, "\n")
cat("Predicted risk score:", as.numeric(predicted_score), "\n")











# STEP 2: CREATE THE AUTOMATION RISK PREDICTION FUNCTION

# 1. Load the saved Decision Tree model
final_model <- readRDS(
  "results/final_decision_tree_model.rds"
)

# 2. Create the prediction function
predict_automation_risk <- function(job_details) {
  
  # Required input columns from the training data
  required_columns <- setdiff(
    names(fair_train),
    "automation_risk_score"
  )
  
  # Check whether any columns are missing
  missing_columns <- setdiff(
    required_columns,
    names(job_details)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste(
        "Missing columns:",
        paste(missing_columns, collapse = ", ")
      )
    )
  }
  
  # Keep only the required columns in training order
  job_details <- job_details[
    , required_columns, drop = FALSE
  ]
  
  # Match categorical inputs to training categories
  for (column in required_columns) {
    
    if (is.factor(fair_train[[column]])) {
      
      job_details[[column]] <- factor(
        job_details[[column]],
        levels = levels(fair_train[[column]])
      )
      
      if (anyNA(job_details[[column]])) {
        stop(
          paste(
            "Invalid or unknown category in:",
            column
          )
        )
      }
    }
  }
  
  # Generate the prediction
  predicted_score <- predict(
    final_model,
    newdata = job_details
  )
  
  # Return the predicted score
  return(as.numeric(predicted_score))
}

cat("Prediction function created successfully.\n")







# TEST THE PREDICTION FUNCTION

# Select one unseen test record
sample_job <- fair_test[1, , drop = FALSE]

# Remove the actual target score from the input
sample_input <- sample_job[
  , names(sample_job) != "automation_risk_score",
  drop = FALSE
]

# Generate prediction using our function
result <- predict_automation_risk(sample_input)

cat("Predicted automation risk score:", result, "\n")










setdiff(names(fair_train), "automation_risk_score")












# TAKE ONE REAL RECORD FROM THE DATASET

# Select the first record from the held-out test set
sample_job <- fair_test[1, , drop = FALSE]

# Store its actual automation risk score separately
actual_score <- sample_job$automation_risk_score

# Remove the target so the model cannot see the answer
sample_input <- sample_job[
  , names(sample_job) != "automation_risk_score",
  drop = FALSE
]

# Display the actual job details used as input
cat("SAMPLE JOB DETAILS FROM DATASET\n")
print(sample_input)

# Predict using the selected Decision Tree model
predicted_score <- predict(
  final_model,
  newdata = sample_input
)

# Display actual and predicted scores
cat("\nACTUAL AUTOMATION RISK SCORE:",
    actual_score, "\n")

cat("PREDICTED AUTOMATION RISK SCORE:",
    as.numeric(predicted_score), "\n")

cat("ABSOLUTE ERROR:",
    abs(actual_score - as.numeric(predicted_score)),
    "\n")


# Select the second record from the test dataset
sample_job2 <- fair_test[2, , drop = FALSE]

# Keep the actual score separate
actual_score2 <- sample_job2$automation_risk_score

# Remove the target from the input
sample_input2 <- sample_job2[
  , names(sample_job2) != "automation_risk_score",
  drop = FALSE
]

# Display the actual job details
cat("JOB DETAILS FROM DATASET\n")
print(sample_input2)

# Predict using the Decision Tree model
predicted_score2 <- predict(
  final_model,
  newdata = sample_input2
)

# Display results
cat("\nACTUAL AUTOMATION RISK SCORE:",
    actual_score2, "\n")

cat("PREDICTED AUTOMATION RISK SCORE:",
    as.numeric(predicted_score2), "\n")

cat("ABSOLUTE ERROR:",
    abs(actual_score2 - as.numeric(predicted_score2)),
    "\n")

# Save the current R workspace
save.image("results/project_workspace.RData")

# List saved project outputs
list.files("results")