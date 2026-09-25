# =============================================================================
# WEEK 1 - Data Cleaning and Preliminary Analysis with R
# Output : reports/Week1_Data_Cleaning_Report.docx
#          data/processed/credit_clean.csv  (+ .rds)
#          data/processed/credit_model_ready.csv
# Run    : source("R/week1_data_cleaning.R")  from the project root
# =============================================================================

source("R/00_helpers.R")
reset_numbering()

doc <- read_docx()
doc <- title_page(doc,
  title      = "Data Cleaning and Preliminary Analysis with R",
  subtitle   = "Credit Scoring Dataset: Cleaning, Transformation and Exploratory Analysis",
  week_label = "WEEK 1 TASK")

doc <- contents_list(doc, c(
  "Introduction and Objective",
  "Dataset Selection and Overview",
  "Loading the Data and First Inspection",
  "Data Quality Assessment",
  "Handling Missing Values",
  "Outlier Detection and Treatment",
  "Feature Engineering",
  "Normalization and Scaling",
  "Encoding Categorical Variables",
  "Exploratory Data Analysis",
  "Key Insights and Next Steps",
  "Appendix: Reproducibility and Session Information"))

# -----------------------------------------------------------------------------
# 1. Introduction
# -----------------------------------------------------------------------------
doc <- h1(doc, "1. Introduction and Objective")
doc <- para(doc,
  "Banks and lending companies must decide, every day, whether a loan applicant is likely to ",
  "repay. A wrong 'approve' decision creates a credit loss; a wrong 'reject' decision loses a good ",
  "customer. Before any predictive model can support that decision, the underlying data must be ",
  "trustworthy. This report documents the first stage of that work: acquiring a public credit ",
  "scoring dataset, auditing its quality, cleaning it and producing a first exploratory analysis in R.")
doc <- para(doc, "The objectives of this week's task were to:")
doc <- bullets(doc, c(
  "Select a public dataset that has real-world complexity: missing values, outliers and a mix of categorical and numerical variables.",
  "Build a documented, reproducible cleaning pipeline in R (missing values, outliers, normalization, encoding).",
  "Describe the data with summary(), str(), descriptive statistics, correlations and visualizations.",
  "Record initial insights that will guide the visualization (Week 2) and modelling (Week 3) stages."))
doc <- para(doc,
  "Every code block in this document was executed by the accompanying R script ",
  "(R/week1_data_cleaning.R), and the console output shown beneath each block is the actual output ",
  "captured at run time, so the report is fully reproducible.")

# -----------------------------------------------------------------------------
# 2. Dataset overview
# -----------------------------------------------------------------------------
doc <- h1(doc, "2. Dataset Selection and Overview")
doc <- para(doc,
  "The dataset chosen is the Credit Scoring dataset originally published by Dr. Lluís A. Belanche ",
  "Muñoz (Universitat Politècnica de Catalunya) and redistributed on GitHub by Dr. Gaston Sanchez ",
  "(github.com/gastonstat/CreditScoring). It is openly available in R through the CRAN package ",
  "'modeldata' as credit_data. Each row is one loan applicant, and the target variable Status ",
  "records whether the loan turned out 'good' (repaid) or 'bad' (defaulted / serious arrears).")
doc <- para(doc, "Why this dataset was selected:")
doc <- bullets(doc, c(
  "Business relevance: credit-risk scoring is one of the most common data-analyst use cases in banking and fintech.",
  "It contains genuine missing values in six columns (most notably Income), not artificially injected ones.",
  "It mixes 9 numerical and 5 categorical variables, which exercises both imputation and encoding techniques.",
  "Several monetary variables are heavily right-skewed with extreme values, so outlier treatment is necessary.",
  "It has a clear binary outcome, which makes it suitable for the classification model planned in Week 3."))

data_dict <- data.frame(
  Variable = c("Status", "Seniority", "Home", "Time", "Age", "Marital", "Records",
               "Job", "Expenses", "Income", "Assets", "Debt", "Amount", "Price"),
  Type = c("Categorical (target)", "Numeric", "Categorical", "Numeric", "Numeric",
           "Categorical", "Categorical", "Categorical", "Numeric", "Numeric",
           "Numeric", "Numeric", "Numeric", "Numeric"),
  Description = c(
    "Credit outcome: good (repaid) or bad (defaulted)",
    "Years with current employer",
    "Housing situation: owner, rent, parents, private, other, ignore",
    "Requested loan term in months",
    "Applicant age in years",
    "Marital status: married, single, separated, divorced, widow",
    "Existing record of past arrears / defaults (yes/no)",
    "Employment type: fixed, freelance, part-time, others",
    "Regular monthly expenses",
    "Monthly income",
    "Value of assets owned",
    "Amount of existing debt",
    "Loan amount requested",
    "Price of the goods being financed"))
doc <- add_table(doc, data_dict, tab_cap("Data dictionary of the Credit Scoring dataset"))

doc <- h2(doc, "2.1 Downloading the dataset")
doc <- para(doc,
  "The data is obtained from the public 'modeldata' package and saved as a raw CSV file in ",
  "data/raw/. The rest of the pipeline reads from that CSV, exactly as it would with a file ",
  "downloaded from a website, so the raw file is never modified.")
doc <- run_chunk(doc, r"-(
# Download the public dataset and store an untouched raw copy
library(modeldata)
data("credit_data", package = "modeldata")
write.csv(credit_data, "data/raw/credit_data_raw.csv", row.names = FALSE)

# Read it back as our raw starting point
raw <- read.csv("data/raw/credit_data_raw.csv", stringsAsFactors = TRUE)
cat("Rows:", nrow(raw), " Columns:", ncol(raw), "\n")
)-")

# -----------------------------------------------------------------------------
# 3. First inspection
# -----------------------------------------------------------------------------
doc <- h1(doc, "3. Loading the Data and First Inspection")
doc <- para(doc,
  "The first step with any new dataset is to confirm its structure: the number of rows, the ",
  "data type R assigned to each column, and a sample of values. str() gives the structure and ",
  "head() shows the first records.")
doc <- run_chunk(doc, r"-(
str(raw)
)-")
doc <- run_chunk(doc, r"-(
head(raw, 8)
)-")
doc <- para(doc,
  "summary() gives a five-number summary for each numeric column, frequency counts for each ",
  "factor, and the count of NA (missing) values. That makes it the fastest way to spot data problems.")
doc <- run_chunk(doc, r"-(
summary(raw)
)-", max_lines = 60)
doc <- bold_para(doc, "Observations from the first inspection: ",
  sprintf(paste(
    "the data has %d applicants and %d variables. The target is imbalanced: %d applicants (%s) are",
    "'bad' and %d (%s) are 'good'. Income, Assets and Debt contain NA values, and Assets and Debt",
    "have maxima (%s and %s) far above their third quartiles, a first sign of extreme outliers. The",
    "Home variable contains a level called 'ignore' that has no business meaning."),
    nrow(raw), ncol(raw), sum(raw$Status == "bad"), pct(mean(raw$Status == "bad")),
    sum(raw$Status == "good"), pct(mean(raw$Status == "good")),
    format(max(raw$Assets, na.rm = TRUE), big.mark = ","),
    format(max(raw$Debt, na.rm = TRUE), big.mark = ",")))

# -----------------------------------------------------------------------------
# 4. Data quality assessment
# -----------------------------------------------------------------------------
doc <- h1(doc, "4. Data Quality Assessment")
doc <- h2(doc, "4.1 Missing values")
doc <- run_chunk(doc, r"-(
na_tbl <- data.frame(Variable = names(raw),
                     Missing  = colSums(is.na(raw)),
                     Percent  = round(100 * colMeans(is.na(raw)), 2)) |>
  arrange(desc(Missing))
rownames(na_tbl) <- NULL
na_tbl
cat("Rows with at least one missing value:", sum(!complete.cases(raw)),
    sprintf("(%.1f%% of rows)\n", 100 * mean(!complete.cases(raw))))
)-")
doc <- run_chunk(doc, r"-(
p_missing <- na_tbl |>
  filter(Missing > 0) |>
  ggplot(aes(x = reorder(Variable, Missing), y = Missing)) +
  geom_col(fill = "#D1495B", width = 0.6) +
  geom_text(aes(label = paste0(Missing, " (", Percent, "%)")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  expand_limits(y = max(na_tbl$Missing) * 1.25) +
  labs(title = "Missing values per variable",
       subtitle = "Only variables with at least one missing value are shown",
       x = NULL, y = "Number of missing values")
)-", show_output = FALSE)
doc <- add_plot(doc, p_missing, fig_path("week1", "01_missing_values.png"),
                fig_cap("Count and percentage of missing values by variable"), height = 3.2)

doc <- para(doc,
  "Before choosing an imputation strategy it is important to know whether values are missing ",
  "completely at random (MCAR) or whether the fact of being missing is itself informative. The check ",
  "below compares the default rate of applicants with and without a recorded income, and applies a ",
  "chi-square test of independence.")
doc <- run_chunk(doc, r"-(
raw |>
  mutate(Income_missing = is.na(Income)) |>
  group_by(Income_missing) |>
  summarise(Applicants = n(), Bad_rate_pct = round(100 * mean(Status == "bad"), 1))

chisq.test(table(Income_missing = is.na(raw$Income), Status = raw$Status))
)-")
inc_miss_bad  <- mean(raw$Status[is.na(raw$Income)] == "bad")
inc_ok_bad    <- mean(raw$Status[!is.na(raw$Income)] == "bad")
inc_chisq_p   <- chisq.test(table(is.na(raw$Income), raw$Status))$p.value
doc <- bold_para(doc, "Finding: ",
  sprintf(paste(
    "applicants without a recorded income default at %s versus %s for those with income",
    "(chi-square p-value = %s). The missingness is %s. For this reason the income values are imputed",
    "*and* a flag variable Income_missing is kept, so this signal is not lost."),
    pct(inc_miss_bad), pct(inc_ok_bad), format.pval(inc_chisq_p, digits = 3),
    ifelse(inc_chisq_p < 0.05, "therefore NOT random: it is associated with credit risk",
           "not significantly associated with the outcome")))

doc <- h2(doc, "4.2 Duplicates, invalid categories and logical consistency")
doc <- run_chunk(doc, r"-(
cat("Exact duplicate rows:", sum(duplicated(raw)), "\n")
cat("Applicants whose loan Amount exceeds the Price of the goods:",
    sum(raw$Amount > raw$Price), "\n")
cat("Age range:", range(raw$Age), " | Loan term range (months):", range(raw$Time), "\n")
table(raw$Home, useNA = "ifany")
)-")
n_dup <- sum(duplicated(raw))
doc <- para(doc,
  sprintf(paste(
    "%d exact duplicate row(s) were found: identical on all 14 variables, which is almost certainly a",
    "double entry of the same application, so they are removed. Ages (%d to %d) and loan terms (%d to %d",
    "months) fall in plausible ranges, and %s. The Home level 'ignore' (%d rows) carries no meaning",
    "and is merged into 'other'."),
    n_dup, min(raw$Age), max(raw$Age), min(raw$Time), max(raw$Time),
    ifelse(sum(raw$Amount > raw$Price) == 0,
           "no applicant asks for more than the price of the goods, so Amount and Price are logically consistent",
           sprintf("%d applicants ask for more than the price of the goods (kept, flagged via LoanToPrice)",
                   sum(raw$Amount > raw$Price))),
    sum(raw$Home == "ignore", na.rm = TRUE)))

# -----------------------------------------------------------------------------
# 5. Missing value treatment
# -----------------------------------------------------------------------------
doc <- h1(doc, "5. Handling Missing Values")
doc <- para(doc, "A different strategy was chosen for each type of variable, based on the amount ",
                 "and nature of the missing data:")
strategy <- data.frame(
  Variable = c("Home, Marital, Job", "Income", "Assets", "Debt"),
  `Missing` = c("6 / 1 / 2", "381 (8.6%)", "47 (1.1%)", "18 (0.4%)"),
  Strategy = c("Replace with the most frequent category (mode)",
               "Median income of the applicant's Job group + missing flag",
               "Overall median", "Overall median (which is 0)"),
  Rationale = c("Very few rows affected; mode keeps the category distribution intact",
                "Income depends strongly on job type; median is robust to skew; flag keeps the risk signal",
                "Small share; median is robust to the extreme right tail",
                "Most applicants have no debt, so the median of 0 is the most plausible value"),
  check.names = FALSE)
doc <- add_table(doc, strategy, tab_cap("Missing-value treatment strategy"))
doc <- para(doc,
  "Dropping incomplete rows (listwise deletion) was rejected because it would discard ",
  sprintf("%d applicants (%s of the data), and those rows have a different default rate, ",
          sum(!complete.cases(raw)), pct(mean(!complete.cases(raw)))),
  "which would bias every later analysis.")
doc <- run_chunk(doc, r"-(
# (0) Remove exact duplicate rows
clean <- raw[!duplicated(raw), ]
rownames(clean) <- NULL
cat("Rows after removing duplicates:", nrow(clean), "\n")

# (a) Merge the meaningless 'ignore' level into 'other'
clean$Home[which(clean$Home == "ignore")] <- "other"
clean$Home <- droplevels(clean$Home)

# (b) Categorical variables: impute with the mode
for (v in c("Home", "Marital", "Job")) {
  m <- mode_value(clean[[v]])
  clean[[v]][is.na(clean[[v]])] <- m
  cat(sprintf("%-8s -> missing values replaced by mode '%s'\n", v, m))
}

# (c) Keep a flag before imputing Income (missingness is informative)
clean$Income_missing <- factor(ifelse(is.na(clean$Income), "yes", "no"))

# (d) Income: median within each Job group
job_medians <- clean |> group_by(Job) |>
  summarise(Median_income = median(Income, na.rm = TRUE))
print(job_medians)
clean <- clean |>
  group_by(Job) |>
  mutate(Income = ifelse(is.na(Income), median(Income, na.rm = TRUE), Income)) |>
  ungroup() |>
  as.data.frame()

# (e) Assets and Debt: overall median
clean$Assets[is.na(clean$Assets)] <- median(clean$Assets, na.rm = TRUE)
clean$Debt[is.na(clean$Debt)]     <- median(clean$Debt,   na.rm = TRUE)

cat("\nRemaining missing values:", sum(is.na(clean)), "\n")
)-")
doc <- run_chunk(doc, r"-(
# Did imputation distort the Income distribution?
rbind(Before = summary(raw$Income)[1:6], After = summary(clean$Income))
)-")
doc <- para(doc,
  "The quartiles and mean of Income barely move after imputation, which confirms that the ",
  "group-median approach filled the gaps without distorting the overall distribution.")

# -----------------------------------------------------------------------------
# 6. Outliers
# -----------------------------------------------------------------------------
doc <- h1(doc, "6. Outlier Detection and Treatment")
doc <- para(doc,
  "Two complementary rules were used to detect outliers in every numeric variable: the IQR rule ",
  "(values below Q1 - 1.5 x IQR or above Q3 + 1.5 x IQR) and the z-score rule (|z| > 3). The IQR rule ",
  "does not assume normality, which suits these skewed monetary variables.")
doc <- run_chunk(doc, r"-(
num_vars <- c("Seniority", "Time", "Age", "Expenses", "Income",
              "Assets", "Debt", "Amount", "Price")

outlier_tbl <- do.call(rbind, lapply(num_vars, function(v) {
  x   <- clean[[v]]
  q   <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lo  <- q[1] - 1.5 * iqr
  hi  <- q[2] + 1.5 * iqr
  z   <- (x - mean(x)) / sd(x)
  data.frame(Variable = v, Q1 = q[1], Q3 = q[2], Upper_fence = hi, Max = max(x),
             IQR_outliers = sum(x < lo | x > hi), Z_outliers = sum(abs(z) > 3))
}))
rownames(outlier_tbl) <- NULL
outlier_tbl
)-")
doc <- run_chunk(doc, r"-(
p_box_before <- clean |>
  select(all_of(num_vars)) |>
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") |>
  ggplot(aes(x = "", y = Value)) +
  geom_boxplot(fill = "#9ECAE1", outlier.colour = "#D1495B", outlier.alpha = 0.4) +
  facet_wrap(~ Variable, scales = "free_y", nrow = 2) +
  labs(title = "Boxplots of numeric variables before outlier treatment",
       subtitle = "Red points are values outside the 1.5 x IQR whiskers", x = NULL, y = NULL)
)-", show_output = FALSE)
doc <- add_plot(doc, p_box_before, fig_path("week1", "02_boxplots_before.png"),
                fig_cap("Outliers in the numeric variables before treatment"), height = 4.4)
doc <- para(doc,
  "Interpretation: Seniority, Time, Age and Expenses have few or no extreme points, and their values ",
  "are all realistic. The monetary variables Income, Assets, Debt, Amount and Price have long ",
  "right tails. For Debt the IQR is zero (most applicants have no debt), so every non-zero debt is ",
  "technically flagged; this shows why a single mechanical rule should not be applied blindly.")
doc <- para(doc,
  "Treatment decision: the extreme values are not data-entry errors (a few wealthy applicants really ",
  "can hold large assets), so deleting rows is not justified. Instead the five monetary variables are ",
  "winsorized: values below the 1st percentile or above the 99th percentile are capped at those ",
  "percentiles. This keeps every applicant while stopping a handful of extreme values from ",
  "dominating means, correlations and model coefficients.")
doc <- run_chunk(doc, r"-(
winsorize <- function(x, probs = c(0.01, 0.99)) {
  q <- quantile(x, probs, na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}
cap_vars <- c("Income", "Assets", "Debt", "Amount", "Price")

before <- sapply(clean[cap_vars], function(x) c(Mean = mean(x), SD = sd(x), Max = max(x)))
for (v in cap_vars) clean[[v]] <- winsorize(clean[[v]])
after  <- sapply(clean[cap_vars], function(x) c(Mean = mean(x), SD = sd(x), Max = max(x)))

round(rbind(before, after), 1) |>
  as.data.frame() |>
  mutate(Stage = rep(c("Before", "After"), each = 3), .before = 1)
)-")
doc <- para(doc,
  "After capping, the standard deviation of Assets and Debt drops sharply while the means change far less. ",
  "The bulk of the distribution is untouched and only the extreme tail has been tamed.")

# -----------------------------------------------------------------------------
# 7. Feature engineering
# -----------------------------------------------------------------------------
doc <- h1(doc, "7. Feature Engineering")
doc <- para(doc,
  "Raw columns rarely express risk directly. Credit analysts reason in ratios (how large is the loan ",
  "compared with the goods, how heavy is the repayment compared with income), so the following ",
  "derived variables were created:")
doc <- bullets(doc, c(
  "LoanToPrice = Amount / Price: share of the purchase that is financed.",
  "Installment = Amount / Time: approximate repayment per month.",
  "PaymentBurden = Installment / Income: share of income needed for the repayment.",
  "DisposableIncome = Income - Expenses: money left after regular expenses.",
  "AgeGroup: age bands, useful for reporting and non-linear age effects.",
  "HasAssets: whether the applicant owns any assets at all."))
doc <- run_chunk(doc, r"-(
clean <- clean |>
  mutate(LoanToPrice      = Amount / Price,
         Installment      = Amount / Time,
         PaymentBurden    = Installment / Income,
         DisposableIncome = Income - Expenses,
         AgeGroup  = cut(Age, breaks = c(17, 25, 35, 45, 55, Inf),
                         labels = c("18-25", "26-35", "36-45", "46-55", "56+")),
         HasAssets = factor(ifelse(Assets > 0, "yes", "no")))

summary(clean[, c("LoanToPrice", "Installment", "PaymentBurden", "DisposableIncome")])
table(clean$AgeGroup)
)-")

# -----------------------------------------------------------------------------
# 8. Normalization
# -----------------------------------------------------------------------------
doc <- h1(doc, "8. Normalization and Scaling")
doc <- para(doc,
  "The numeric variables are measured on very different scales (Age is in tens, Assets in thousands). ",
  "Distance-based and regularized models are sensitive to scale, so two standard normalizations were ",
  "applied: z-score standardization, (x - mean) / sd, which gives mean 0 and standard deviation 1, and ",
  "min-max scaling, (x - min) / (max - min), which maps values to the range [0, 1]. In addition, a ",
  "log transformation was tested on the most skewed variables.")
doc <- run_chunk(doc, r"-(
num_model <- c(num_vars, "LoanToPrice", "Installment", "PaymentBurden", "DisposableIncome")

zscore <- function(x) (x - mean(x)) / sd(x)
minmax <- function(x) (x - min(x)) / (max(x) - min(x))

scaled_z  <- as.data.frame(lapply(clean[num_model], zscore))
scaled_mm <- as.data.frame(lapply(clean[num_model], minmax))

data.frame(z_mean = round(sapply(scaled_z, mean), 3),
           z_sd   = round(sapply(scaled_z, sd), 3),
           mm_min = sapply(scaled_mm, min),
           mm_max = sapply(scaled_mm, max))
)-")
doc <- run_chunk(doc, r"-(
# Skewness before and after a log(1 + x) transformation
skew_vars <- c("Income", "Assets", "Debt", "Amount", "Price", "Seniority")
data.frame(Skew_original = sapply(clean[skew_vars], skewness),
           Skew_log1p    = sapply(clean[skew_vars], function(x) skewness(log1p(x)))) |>
  round(2)
)-")
doc <- para(doc,
  "The log transformation reduces the skewness of Income, Amount and Price substantially. Assets and ",
  "Debt contain many zeros, so for them the binary feature HasAssets and the capped raw value are ",
  "more informative than a log. The z-scored values are the ones used in the model-ready file.")

# -----------------------------------------------------------------------------
# 9. Encoding
# -----------------------------------------------------------------------------
doc <- h1(doc, "9. Encoding Categorical Variables")
doc <- para(doc,
  "Statistical models need numeric inputs. The target Status is label-encoded as Default = 1 for ",
  "'bad' and 0 for 'good'. The categorical predictors are one-hot encoded with caret::dummyVars() ",
  "using full-rank coding (k - 1 dummy columns per variable) to avoid perfect multicollinearity, ",
  "which is also known as the dummy-variable trap.")
doc <- run_chunk(doc, r"-(
clean$Default <- ifelse(clean$Status == "bad", 1, 0)

cat_vars <- c("Home", "Marital", "Records", "Job", "AgeGroup", "HasAssets", "Income_missing")
dummies  <- caret::dummyVars(~ Home + Marital + Records + Job + AgeGroup +
                               HasAssets + Income_missing,
                             data = clean, fullRank = TRUE)
encoded  <- as.data.frame(predict(dummies, newdata = clean))

cat("Categorical variables:", length(cat_vars), "-> dummy columns:", ncol(encoded), "\n")
names(encoded)

model_ready <- cbind(Default = clean$Default, scaled_z, encoded)
cat("\nModel-ready dataset:", nrow(model_ready), "rows x", ncol(model_ready), "columns\n")
)-")

# -----------------------------------------------------------------------------
# 10. EDA
# -----------------------------------------------------------------------------
doc <- h1(doc, "10. Exploratory Data Analysis")
doc <- h2(doc, "10.1 Descriptive statistics of numeric variables")
doc <- run_chunk(doc, r"-(
desc_stats <- do.call(rbind, lapply(num_model, function(v) {
  x <- clean[[v]]
  data.frame(Variable = v, Mean = mean(x), SD = sd(x), Median = median(x),
             IQR = IQR(x), Min = min(x), Max = max(x), Skewness = skewness(x))
}))
desc_stats |> mutate(across(where(is.numeric), ~ round(.x, 2)))
)-")
doc <- h2(doc, "10.2 Categorical variables and default rates")
doc <- run_chunk(doc, r"-(
cat_summary <- do.call(rbind, lapply(cat_vars, function(v) {
  clean |>
    group_by(Level = as.character(.data[[v]])) |>
    summarise(Count = n(),
              Share_pct = round(100 * n() / nrow(clean), 1),
              Bad_rate_pct = round(100 * mean(Default), 1)) |>
    mutate(Variable = v, .before = 1)
}))
as.data.frame(cat_summary)
)-", max_lines = 40)
doc <- h2(doc, "10.3 Distributions")
doc <- run_chunk(doc, r"-(
p_hist <- clean |>
  select(Age, Seniority, Time, Expenses, Income, Amount, Price, LoanToPrice, PaymentBurden) |>
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") |>
  ggplot(aes(Value)) +
  geom_histogram(bins = 30, fill = "#2E86AB", colour = "white") +
  facet_wrap(~ Variable, scales = "free", ncol = 3) +
  labs(title = "Distribution of key numeric variables (after cleaning)", x = NULL, y = "Count")
)-", show_output = FALSE)
doc <- add_plot(doc, p_hist, fig_path("week1", "03_histograms.png"),
                fig_cap("Histograms of the cleaned numeric variables"), height = 5)
doc <- para(doc,
  "Age, Income, Amount and Price are right-skewed: most applicants are young to middle-aged and ask ",
  "for moderate loans, with a long tail of larger values. Time (loan term) is concentrated on a few ",
  "standard terms (for example 36, 48 and 60 months), which is typical of product-based lending. ",
  "LoanToPrice is concentrated below 1, so most loans finance only part of the purchase.")
doc <- h2(doc, "10.4 Good versus bad applicants")
doc <- run_chunk(doc, r"-(
status_means <- clean |>
  group_by(Status) |>
  summarise(across(c(Age, Seniority, Income, Assets, Debt, Amount, LoanToPrice,
                     PaymentBurden), ~ round(mean(.x), 2)))
as.data.frame(status_means)
)-")
doc <- run_chunk(doc, r"-(
p_box_status <- clean |>
  select(Status, Seniority, Income, Assets, Amount, LoanToPrice, Age) |>
  pivot_longer(-Status, names_to = "Variable", values_to = "Value") |>
  ggplot(aes(Status, Value, fill = Status)) +
  geom_boxplot(outlier.alpha = 0.25) +
  facet_wrap(~ Variable, scales = "free_y", nrow = 2) +
  scale_fill_manual(values = PAL) +
  labs(title = "Key variables by credit outcome", x = NULL, y = NULL) +
  theme(legend.position = "none")
)-", show_output = FALSE)
doc <- add_plot(doc, p_box_status, fig_path("week1", "04_boxplots_by_status.png"),
                fig_cap("Distribution of key variables for good and bad applicants"), height = 4.5)
doc <- run_chunk(doc, r"-(
p_bad_cat <- cat_summary |>
  filter(Variable %in% c("Records", "Job", "Home", "Marital")) |>
  ggplot(aes(x = reorder(Level, Bad_rate_pct), y = Bad_rate_pct)) +
  geom_col(fill = "#D1495B") +
  geom_hline(yintercept = 100 * mean(clean$Default), linetype = "dashed") +
  geom_text(aes(label = paste0(Bad_rate_pct, "%")), hjust = -0.15, size = 3) +
  coord_flip() +
  facet_wrap(~ Variable, scales = "free_y") +
  expand_limits(y = 80) +
  labs(title = "Default (bad) rate by category",
       subtitle = "Dashed line = overall default rate", x = NULL, y = "Bad rate (%)")
)-", show_output = FALSE)
doc <- add_plot(doc, p_bad_cat, fig_path("week1", "05_badrate_categories.png"),
                fig_cap("Default rate by level of the main categorical variables"), height = 4.6)

doc <- h2(doc, "10.5 Correlations")
doc <- run_chunk(doc, r"-(
cor_mat <- cor(cbind(clean[num_model], Default = clean$Default))

# Correlation of every numeric variable with the target
sort(round(cor_mat[, "Default"], 3)[-ncol(cor_mat)])

# Strongest pairwise correlations between predictors
cor_pairs <- as.data.frame(as.table(cor_mat[num_model, num_model])) |>
  filter(as.integer(Var1) < as.integer(Var2)) |>
  arrange(desc(abs(Freq))) |>
  rename(Correlation = Freq) |>
  mutate(Correlation = round(Correlation, 3))
head(cor_pairs, 8)
)-")
doc <- run_chunk(doc, r"-(
p_cor <- as.data.frame(as.table(cor_mat)) |>
  ggplot(aes(Var1, Var2, fill = Freq)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", Freq)), size = 2.3) +
  scale_fill_gradient2(low = "#2E86AB", mid = "white", high = "#D1495B",
                       midpoint = 0, limits = c(-1, 1), name = "r") +
  labs(title = "Correlation matrix (Pearson)", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")
)-", show_output = FALSE)
doc <- add_plot(doc, p_cor, fig_path("week1", "06_correlation_matrix.png"),
                fig_cap("Correlation heatmap of numeric variables and the Default flag"),
                width = 6.5, height = 5.6)

# -----------------------------------------------------------------------------
# 11. Insights
# -----------------------------------------------------------------------------
br <- function(var, level) cat_summary$Bad_rate_pct[cat_summary$Variable == var &
                                                    cat_summary$Level == level]
top_pair <- cor_pairs[1, ]
top_raw  <- cor_pairs |> filter(Var1 %in% num_vars, Var2 %in% num_vars) |> slice(1)
doc <- h1(doc, "11. Key Insights and Next Steps")
doc <- h2(doc, "11.1 Summary of the cleaning process")
cleaning_log <- data.frame(
  Step = c("Raw data", "Duplicates", "Invalid category", "Missing categorical", "Missing Income",
           "Missing Assets/Debt", "Outliers", "New features", "Scaling", "Encoding"),
  Action = c(sprintf("%d rows x %d columns, %d rows with NA", nrow(raw), ncol(raw),
                     sum(!complete.cases(raw))),
             sprintf("%d exact duplicate rows removed -> %d applicants", n_dup, nrow(clean)),
             "Home = 'ignore' merged into 'other'",
             "Mode imputation (Home, Marital, Job)",
             "Median by Job group + Income_missing flag",
             "Median imputation",
             "Winsorized at 1st/99th percentile (5 monetary variables)",
             "LoanToPrice, Installment, PaymentBurden, DisposableIncome, AgeGroup, HasAssets",
             "Z-score and min-max scaling; log1p tested",
             sprintf("One-hot encoding -> %d dummy columns; Status -> Default (0/1)", ncol(encoded))))
doc <- add_table(doc, cleaning_log, tab_cap("Cleaning and transformation log"))

doc <- h2(doc, "11.2 Initial insights")
doc <- bullets(doc, c(
  sprintf("Class imbalance: %s of applicants are 'bad'. Accuracy alone will be a misleading metric in Week 3; ROC-AUC, sensitivity and specificity must be reported.",
          pct(mean(clean$Default))),
  sprintf("Past arrears are the strongest single warning sign: applicants with Records = yes default at %s%% versus %s%% without records.",
          br("Records", "yes"), br("Records", "no")),
  sprintf("Job stability matters: part-time workers default at %s%% and freelancers at %s%%, compared with %s%% for fixed contracts.",
          br("Job", "partime"), br("Job", "freelance"), br("Job", "fixed")),
  sprintf("Home ownership is protective: owners default at %s%%, renters at %s%%.",
          br("Home", "owner"), br("Home", "rent")),
  sprintf("Bad applicants have lower seniority (mean %.1f vs %.1f years), lower income (%.0f vs %.0f) and finance a larger share of the purchase (LoanToPrice %.2f vs %.2f).",
          status_means$Seniority[status_means$Status == "bad"], status_means$Seniority[status_means$Status == "good"],
          status_means$Income[status_means$Status == "bad"], status_means$Income[status_means$Status == "good"],
          status_means$LoanToPrice[status_means$Status == "bad"], status_means$LoanToPrice[status_means$Status == "good"]),
  sprintf("Missing income is itself a risk signal (%s default rate vs %s), so the Income_missing flag is retained.",
          pct(inc_miss_bad), pct(inc_ok_bad)),
  sprintf("Among the original variables the strongest correlation is %s-%s (r = %.2f). Engineered ratios are naturally correlated with their parent columns (e.g. %s-%s, r = %.2f), so multicollinearity will be checked before modelling.",
          top_raw$Var1, top_raw$Var2, top_raw$Correlation,
          top_pair$Var1, top_pair$Var2, top_pair$Correlation)))
doc <- h2(doc, "11.3 Next steps")
doc <- para(doc,
  "The cleaned dataset (data/processed/credit_clean.csv) will be used in Week 2 to build a set of ",
  "visualizations for a non-technical audience, and the model-ready dataset ",
  "(data/processed/credit_model_ready.csv) will support hypothesis testing and a classification model ",
  "in Week 3.")

# Save outputs
doc <- run_chunk(doc, r"-(
write.csv(clean, "data/processed/credit_clean.csv", row.names = FALSE)
saveRDS(clean, "data/processed/credit_clean.rds")
write.csv(model_ready, "data/processed/credit_model_ready.csv", row.names = FALSE)
cat("Saved cleaned data:", nrow(clean), "rows x", ncol(clean), "columns\n")
)-")
saveRDS(list(na_tbl = na_tbl, outlier_tbl = outlier_tbl, cleaning_log = cleaning_log,
             cat_summary = cat_summary, desc_stats = desc_stats,
             inc_miss_bad = inc_miss_bad, inc_ok_bad = inc_ok_bad,
             n_raw = nrow(raw), n_incomplete = sum(!complete.cases(raw))),
        "outputs/models/week1_summary.rds")

# -----------------------------------------------------------------------------
# 12. Appendix
# -----------------------------------------------------------------------------
doc <- h1(doc, "12. Appendix: Reproducibility and Session Information")
doc <- para(doc,
  "The complete, commented script is available in the project repository at R/week1_data_cleaning.R. ",
  "Running source('R/week1_data_cleaning.R') from the project root re-creates this document, the ",
  "figures in outputs/figures/week1/ and the processed data files.")
doc <- run_chunk(doc, r"-(
cat(R.version.string, "\n")
pk <- c("dplyr", "tidyr", "ggplot2", "caret", "officer", "flextable", "modeldata")
data.frame(Package = pk, Version = sapply(pk, function(p) as.character(packageVersion(p))),
           row.names = NULL)
)-")

save_report(doc, "reports/Week1_Data_Cleaning_Report.docx")
