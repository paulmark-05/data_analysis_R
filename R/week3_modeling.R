# =============================================================================
# WEEK 3 - Statistical Analysis and Predictive Modeling using R
# Input  : data/processed/credit_clean.rds  (created by week1_data_cleaning.R)
# Output : reports/Week3_Statistical_Modeling_Report.docx
#          outputs/figures/week3/*.png
#          outputs/models/week3_results.rds  (used by the Week 4 report)
# Run    : source("R/week3_modeling.R")  from the project root
# =============================================================================

source("R/00_helpers.R")
reset_numbering()
if (!file.exists("data/processed/credit_clean.rds")) source("R/week1_data_cleaning.R")
suppressPackageStartupMessages({ library(caret); library(pROC) })

doc <- read_docx()
doc <- title_page(doc,
  title      = "Statistical Analysis and Predictive Modeling using R",
  subtitle   = "Hypothesis testing and a credit-default classification model",
  week_label = "WEEK 3 TASK")
doc <- contents_list(doc, c(
  "Introduction and Objectives",
  "Dataset Identification and Rationale",
  "Hypothesis Formulation",
  "Distribution and Assumption Checks",
  "Hypothesis Testing Results",
  "Correlation and Multicollinearity Analysis",
  "Model Building with Cross-Validation",
  "Model Evaluation on the Test Set",
  "Model Diagnostics",
  "Interpretation of the Logistic Regression Model",
  "Strengths, Limitations and Improvements",
  "Conclusion"))

# -----------------------------------------------------------------------------
doc <- h1(doc, "1. Introduction and Objectives")
doc <- para(doc,
  "Weeks 1 and 2 cleaned the credit-scoring data and showed visually which applicant characteristics ",
  "are associated with default. Visual patterns can, however, arise by chance. This week the patterns ",
  "are tested formally with hypothesis tests, and then combined into a predictive classification model ",
  "that estimates each applicant's probability of default.")
doc <- bullets(doc, c(
  "Formulate and test statistical hypotheses about the drivers of default.",
  "Check distributional assumptions (normality, equal variance) and choose tests accordingly.",
  "Build a logistic regression model, and a random forest as a benchmark, using stratified train/test splitting and repeated 10-fold cross-validation.",
  "Evaluate the models with a confusion matrix, sensitivity, specificity and ROC-AUC, and run diagnostic checks.",
  "Interpret the model for the business and identify improvements."))

# -----------------------------------------------------------------------------
doc <- h1(doc, "2. Dataset Identification and Rationale")
doc <- para(doc,
  "The Credit Scoring dataset (public; modeldata::credit_data, originally from Dr. L. A. Belanche Muñoz ",
  "via github.com/gastonstat/CreditScoring) was selected for predictive modelling because:")
doc <- bullets(doc, c(
  "It has a clearly defined binary outcome (Status: good/bad), which is ideal for a classification model.",
  "With 4,452 cleaned records it is large enough for a stratified train/test split and repeated cross-validation, and still small enough to be fully transparent.",
  "It combines numeric and categorical predictors, so it demonstrates both kinds of hypothesis test (t/Wilcoxon and chi-square) and dummy-variable modelling.",
  "The problem is realistic and has an obvious business use: automated pre-screening of loan applications.",
  "The data was already cleaned and validated in Week 1 (duplicates, missing values, outliers), so the modelling rests on a reliable foundation."))
doc <- run_chunk(doc, r"-(
credit <- readRDS("data/processed/credit_clean.rds")
credit$Status <- factor(credit$Status, levels = c("bad", "good"))   # 'bad' = event of interest
cat("Records:", nrow(credit), "\n")
round(prop.table(table(credit$Status)), 3)
)-")

# -----------------------------------------------------------------------------
doc <- h1(doc, "3. Hypothesis Formulation")
doc <- para(doc, "Based on the Week 2 visual findings, the following hypotheses were formulated. ",
                 "A significance level of α = 0.05 is used throughout.")
hyp <- data.frame(
  ID = paste0("H", 1:6),
  `Null hypothesis (H0)` = c(
    "Income has the same distribution for good and bad borrowers",
    "Job seniority has the same distribution for good and bad borrowers",
    "The loan-to-price ratio has the same mean for good and bad borrowers",
    "Having a past arrears record is independent of the credit outcome",
    "Employment type is independent of the credit outcome",
    "Loan amount and price of goods are uncorrelated"),
  `Alternative (H1)` = c("Distributions differ (bad borrowers earn less)",
                         "Distributions differ (bad borrowers have less seniority)",
                         "Means differ",
                         "Arrears record and outcome are associated",
                         "Employment type and outcome are associated",
                         "They are correlated"),
  Test = c("Wilcoxon rank-sum (+ Welch t-test)", "Wilcoxon rank-sum",
           "Welch two-sample t-test", "Pearson chi-square", "Pearson chi-square",
           "Pearson & Spearman correlation"),
  check.names = FALSE)
doc <- add_table(doc, hyp, tab_cap("Hypotheses tested"))

# -----------------------------------------------------------------------------
doc <- h1(doc, "4. Distribution and Assumption Checks")
doc <- para(doc,
  "Parametric tests such as the t-test assume approximately normal data within each group and ",
  "(for the classical version) equal variances. These assumptions were checked with the Shapiro-Wilk ",
  "test, Q-Q plots and an F-test for equality of variances.")
doc <- run_chunk(doc, r"-(
# Shapiro-Wilk normality test (valid for n <= 5000) for each group
norm_vars <- c("Income", "Seniority", "LoanToPrice", "Age", "Amount")
normality <- do.call(rbind, lapply(norm_vars, function(v) {
  data.frame(Variable = v,
             W_bad   = shapiro.test(credit[[v]][credit$Status == "bad"])$statistic,
             p_bad   = shapiro.test(credit[[v]][credit$Status == "bad"])$p.value,
             W_good  = shapiro.test(credit[[v]][credit$Status == "good"])$statistic,
             p_good  = shapiro.test(credit[[v]][credit$Status == "good"])$p.value)
}))
rownames(normality) <- NULL
normality |> mutate(across(where(is.numeric), ~ signif(.x, 3)))
)-")
doc <- add_code(doc, r"-(
par(mfrow = c(1, 3))
qqnorm(credit$Income, main = "Q-Q plot: Income", pch = 20, col = "#2E86AB55")
qqline(credit$Income, col = "#D1495B", lwd = 2)
qqnorm(log(credit$Income), main = "Q-Q plot: log(Income)", pch = 20, col = "#2E86AB55")
qqline(log(credit$Income), col = "#D1495B", lwd = 2)
qqnorm(credit$LoanToPrice, main = "Q-Q plot: LoanToPrice", pch = 20, col = "#2E86AB55")
qqline(credit$LoanToPrice, col = "#D1495B", lwd = 2)
)-")
doc <- add_base_plot(doc, function() {
  par(mfrow = c(1, 3))
  qqnorm(credit$Income, main = "Q-Q plot: Income", pch = 20, col = "#2E86AB55")
  qqline(credit$Income, col = "#D1495B", lwd = 2)
  qqnorm(log(credit$Income), main = "Q-Q plot: log(Income)", pch = 20, col = "#2E86AB55")
  qqline(log(credit$Income), col = "#D1495B", lwd = 2)
  qqnorm(credit$LoanToPrice, main = "Q-Q plot: LoanToPrice", pch = 20, col = "#2E86AB55")
  qqline(credit$LoanToPrice, col = "#D1495B", lwd = 2)
}, fig_path("week3", "01_qq_plots.png"), fig_cap("Normal Q-Q plots"), height = 3)
doc <- run_chunk(doc, r"-(
# F-test for equality of variances of Income between the two groups
var.test(Income ~ Status, data = credit)
)-")
doc <- bold_para(doc, "Conclusion on assumptions: ",
  paste("every Shapiro-Wilk test rejects normality (p < 0.001), and the Q-Q plots show clear curvature,",
        "especially in the right tail of Income; the log transform helps but does not fully normalise it. The",
        "F-test also shows unequal variances. Therefore: (1) the non-parametric Wilcoxon rank-sum test is",
        "used as the primary test for skewed variables; (2) where a t-test is reported, Welch's version",
        "(which does not assume equal variances) is used. With more than 1,200 observations per group the",
        "Central Limit Theorem makes the Welch t-test on means robust anyway, so both tests are reported",
        "for comparison."))

# -----------------------------------------------------------------------------
doc <- h1(doc, "5. Hypothesis Testing Results")
doc <- h2(doc, "5.1 Numeric variables: H1 to H3")
doc <- run_chunk(doc, r"-(
wilcox.test(Income ~ Status, data = credit)
t.test(Income ~ Status, data = credit)            # Welch t-test (var.equal = FALSE)
)-")
doc <- run_chunk(doc, r"-(
wilcox.test(Seniority ~ Status, data = credit)
t.test(LoanToPrice ~ Status, data = credit)
)-")
doc <- h2(doc, "5.2 Categorical variables: H4 and H5")
doc <- run_chunk(doc, r"-(
cramers_v <- function(tab) {
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))$statistic
  sqrt(chi / (sum(tab) * (min(dim(tab)) - 1)))
}
tab_rec <- table(credit$Records, credit$Status)
chisq.test(tab_rec)
cat("Cramer's V (Records):", round(cramers_v(tab_rec), 3), "\n\n")

tab_job <- table(credit$Job, credit$Status)
chisq.test(tab_job)
cat("Cramer's V (Job):", round(cramers_v(tab_job), 3), "\n")
cat("Minimum expected cell count:", round(min(chisq.test(tab_job)$expected), 1), "\n")
)-")
doc <- h2(doc, "5.3 Correlation: H6")
doc <- run_chunk(doc, r"-(
cor.test(credit$Amount, credit$Price, method = "pearson")
cor.test(credit$Amount, credit$Price, method = "spearman", exact = FALSE)
)-")
doc <- h2(doc, "5.4 Summary of hypothesis tests")
doc <- run_chunk(doc, r"-(
rank_biserial <- function(v) {                     # effect size for Wilcoxon
  w <- wilcox.test(credit[[v]] ~ credit$Status)$statistic
  n1 <- sum(credit$Status == "bad"); n2 <- sum(credit$Status == "good")
  abs(unname(1 - 2 * w / (n1 * n2)))
}
test_summary <- data.frame(
  Hypothesis = c("H1 Income", "H2 Seniority", "H3 LoanToPrice", "H4 Records",
                 "H5 Job", "H6 Amount~Price"),
  Test = c("Wilcoxon", "Wilcoxon", "Welch t", "Chi-square", "Chi-square", "Pearson r"),
  Statistic = c(wilcox.test(Income ~ Status, credit)$statistic,
                wilcox.test(Seniority ~ Status, credit)$statistic,
                t.test(LoanToPrice ~ Status, credit)$statistic,
                chisq.test(tab_rec)$statistic, chisq.test(tab_job)$statistic,
                cor(credit$Amount, credit$Price)),
  p_value = c(wilcox.test(Income ~ Status, credit)$p.value,
              wilcox.test(Seniority ~ Status, credit)$p.value,
              t.test(LoanToPrice ~ Status, credit)$p.value,
              chisq.test(tab_rec)$p.value, chisq.test(tab_job)$p.value,
              cor.test(credit$Amount, credit$Price)$p.value),
  Effect_size = c(rank_biserial("Income"), rank_biserial("Seniority"),
                  abs(diff(tapply(credit$LoanToPrice, credit$Status, mean))) / sd(credit$LoanToPrice),
                  cramers_v(tab_rec), cramers_v(tab_job), cor(credit$Amount, credit$Price)),
  Effect_measure = c("rank-biserial r", "rank-biserial r", "Cohen's d", "Cramer's V",
                     "Cramer's V", "Pearson r"))
test_summary$Decision <- ifelse(test_summary$p_value < 0.05, "Reject H0", "Fail to reject H0")
test_summary |> mutate(Statistic = round(Statistic, 2), Effect_size = round(Effect_size, 3),
                       p_value = format.pval(p_value, digits = 3, eps = 1e-16))
)-")
es <- setNames(test_summary$Effect_size, test_summary$Hypothesis)
doc <- para(doc, sprintf(paste(
  "All six null hypotheses are rejected at the 5%% level. With more than 4,000 observations even",
  "small differences become statistically significant, so effect sizes matter more for the business",
  "interpretation. The largest effects are for loan-to-price (Cohen's d = %.2f, close to medium) and",
  "seniority (rank-biserial r = %.2f, medium). Past arrears (Cramer's V = %.2f) and employment type",
  "(V = %.2f) show small-to-medium associations, and income has the smallest but still meaningful",
  "effect (r = %.2f). All five are therefore candidate predictors for the model."),
  es["H3 LoanToPrice"], es["H2 Seniority"], es["H4 Records"], es["H5 Job"], es["H1 Income"]))

# -----------------------------------------------------------------------------
doc <- h1(doc, "6. Correlation and Multicollinearity Analysis")
doc <- para(doc,
  "Highly correlated predictors make logistic regression coefficients unstable and hard to interpret. ",
  "The Variance Inflation Factor (VIF = 1 / (1 - R²), where R² comes from regressing one ",
  "predictor on all the others) was computed for the candidate numeric predictors. VIF > 5 is a ",
  "common warning threshold.")
doc <- run_chunk(doc, r"-(
vif_manual <- function(df) {
  sapply(names(df), function(v) {
    r2 <- summary(lm(reformulate(setdiff(names(df), v), v), data = df))$r.squared
    1 / (1 - r2)
  })
}
cand <- c("Seniority", "Time", "Age", "Expenses", "Income", "Assets", "Debt",
          "Amount", "Price", "LoanToPrice")
round(vif_manual(credit[cand]), 2)
cat("\nWithout Price:\n")
round(vif_manual(credit[setdiff(cand, "Price")]), 2)
)-")
doc <- para(doc,
  "Amount, Price and LoanToPrice are mathematically linked (LoanToPrice = Amount / Price), so keeping ",
  "all three inflates their VIFs. Dropping Price brings every VIF to an acceptable level while keeping ",
  "the information: the loan size (Amount) and the share financed (LoanToPrice). Derived variables ",
  "that duplicate other columns (Installment, PaymentBurden, DisposableIncome, AgeGroup, HasAssets) ",
  "are also excluded from the model for the same reason.")

# -----------------------------------------------------------------------------
doc <- h1(doc, "7. Model Building with Cross-Validation")
doc <- h2(doc, "7.1 Model choice")
doc <- para(doc,
  "Logistic regression was chosen as the primary model because it is the industry standard for ",
  "credit scoring: it outputs a probability of default, its coefficients translate directly into ",
  "odds ratios that regulators and credit officers can understand, and it is fast and stable. A ",
  "random forest was trained as a non-linear benchmark to check whether a more flexible model ",
  "captures substantially more signal.")
doc <- h2(doc, "7.2 Train/test split")
doc <- run_chunk(doc, r"-(
model_vars <- c("Status", "Seniority", "Home", "Time", "Age", "Marital", "Records", "Job",
                "Expenses", "Income", "Assets", "Debt", "Amount", "LoanToPrice",
                "Income_missing")
model_df <- credit[, model_vars]

set.seed(2026)
train_idx <- createDataPartition(model_df$Status, p = 0.75, list = FALSE)
train <- model_df[train_idx, ]
test  <- model_df[-train_idx, ]

rbind(Train = c(n = nrow(train), bad_rate = round(mean(train$Status == "bad"), 3)),
      Test  = c(n = nrow(test),  bad_rate = round(mean(test$Status == "bad"), 3)))
)-")
doc <- para(doc,
  "createDataPartition() performs a stratified split, so the default rate is the same in the ",
  "training (75%) and test (25%) sets. The test set is kept completely aside until the final evaluation.")
doc <- h2(doc, "7.3 Repeated 10-fold cross-validation")
doc <- para(doc,
  "Within the training set, 10-fold cross-validation repeated 3 times (30 resamples) is used to ",
  "estimate out-of-sample performance and to tune the random forest. Because the classes are ",
  "imbalanced, ROC-AUC is used as the optimisation metric instead of accuracy.")
doc <- run_chunk(doc, r"-(
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                     classProbs = TRUE, summaryFunction = twoClassSummary,
                     savePredictions = "final")

set.seed(2026)
fit_glm <- train(Status ~ ., data = train, method = "glm", family = binomial,
                 preProcess = c("center", "scale"), metric = "ROC", trControl = ctrl)

set.seed(2026)
fit_rf <- train(Status ~ ., data = train, method = "rf", ntree = 200,
                tuneGrid = data.frame(mtry = c(2, 4, 6)),
                metric = "ROC", trControl = ctrl)
fit_glm
fit_rf$results[, c("mtry", "ROC", "Sens", "Spec")]
)-", max_lines = 40)
doc <- run_chunk(doc, r"-(
cv_res <- resamples(list(Logistic = fit_glm, RandomForest = fit_rf))
summary(cv_res)$statistics$ROC
)-")
doc <- run_chunk(doc, r"-(
cv_long <- cv_res$values |>
  pivot_longer(-Resample, names_to = "key", values_to = "value") |>
  separate(key, into = c("Model", "Metric"), sep = "~") |>
  filter(Metric %in% c("ROC", "Sens", "Spec"))
p_cv <- ggplot(cv_long, aes(x = Model, y = value, fill = Model)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Metric, scales = "free_y") +
  scale_fill_manual(values = c(Logistic = "#1F4E79", RandomForest = "#F4A259")) +
  labs(title = "Cross-validated performance (30 resamples)",
       subtitle = "Sens = sensitivity for 'bad' at a 0.5 threshold; Spec = specificity",
       x = NULL, y = NULL) +
  theme(legend.position = "none")
)-", show_output = FALSE)
doc <- add_plot(doc, p_cv, fig_path("week3", "02_cv_comparison.png"),
                fig_cap("Distribution of cross-validated ROC, sensitivity and specificity"), height = 3.6)
cv_stats <- summary(cv_res)$statistics$ROC
doc <- para(doc, sprintf(paste(
  "Cross-validated mean ROC-AUC is %.3f for logistic regression and %.3f for the random forest.",
  "Both models have high specificity but modest sensitivity at the default 0.5 cut-off: with only",
  "28%% bad loans, a 0.5 threshold is too conservative for flagging risk. The threshold is therefore",
  "tuned in the next section."), cv_stats["Logistic", "Mean"], cv_stats["RandomForest", "Mean"]))

doc <- h2(doc, "7.4 Choosing a classification threshold")
doc <- para(doc,
  "The threshold is selected with Youden's J statistic (sensitivity + specificity - 1), using only the ",
  "out-of-fold cross-validation predictions from the training data. This avoids any information ",
  "leaking from the test set.")
doc <- run_chunk(doc, r"-(
cv_pred_glm <- fit_glm$pred
roc_cv <- roc(cv_pred_glm$obs, cv_pred_glm$bad, levels = c("good", "bad"),
              direction = "<", quiet = TRUE)
thr <- coords(roc_cv, "best", best.method = "youden",
              ret = c("threshold", "sensitivity", "specificity"))
thr_glm <- thr$threshold[1]
round(thr, 3)
)-")

# -----------------------------------------------------------------------------
doc <- h1(doc, "8. Model Evaluation on the Test Set")
doc <- run_chunk(doc, r"-(
test$p_glm <- predict(fit_glm, newdata = test, type = "prob")[, "bad"]
test$p_rf  <- predict(fit_rf,  newdata = test, type = "prob")[, "bad"]

roc_glm <- roc(test$Status, test$p_glm, levels = c("good", "bad"), direction = "<", quiet = TRUE)
roc_rf  <- roc(test$Status, test$p_rf,  levels = c("good", "bad"), direction = "<", quiet = TRUE)
cat("Test AUC - Logistic:", round(auc(roc_glm), 3),
    " 95% CI:", paste(round(ci.auc(roc_glm)[c(1, 3)], 3), collapse = "-"), "\n")
cat("Test AUC - Random forest:", round(auc(roc_rf), 3),
    " 95% CI:", paste(round(ci.auc(roc_rf)[c(1, 3)], 3), collapse = "-"), "\n")
roc.test(roc_glm, roc_rf)$p.value     # DeLong test: are the two AUCs different?
)-")
doc <- run_chunk(doc, r"-(
pred_class <- factor(ifelse(test$p_glm >= thr_glm, "bad", "good"), levels = c("bad", "good"))
cm_glm <- confusionMatrix(pred_class, test$Status, positive = "bad")
cm_glm
)-", max_lines = 50)
doc <- run_chunk(doc, r"-(
cm_05 <- confusionMatrix(factor(ifelse(test$p_glm >= 0.5, "bad", "good"), levels = c("bad", "good")),
                         test$Status, positive = "bad")
thr_rf <- coords(roc(fit_rf$pred$obs, fit_rf$pred$bad, levels = c("good", "bad"),
                     direction = "<", quiet = TRUE), "best", ret = "threshold")$threshold[1]
cm_rf  <- confusionMatrix(factor(ifelse(test$p_rf >= thr_rf, "bad", "good"), levels = c("bad", "good")),
                          test$Status, positive = "bad")
metric_row <- function(cm, auc_val, name, t) {
  data.frame(Model = name, Threshold = round(t, 3), AUC = round(auc_val, 3),
             Accuracy = round(cm$overall["Accuracy"], 3),
             Sensitivity = round(cm$byClass["Sensitivity"], 3),
             Specificity = round(cm$byClass["Specificity"], 3),
             Precision = round(cm$byClass["Pos Pred Value"], 3),
             F1 = round(cm$byClass["F1"], 3), Kappa = round(cm$overall["Kappa"], 3))
}
perf <- rbind(metric_row(cm_05,  auc(roc_glm), "Logistic (0.5 cut-off)", 0.5),
              metric_row(cm_glm, auc(roc_glm), "Logistic (Youden cut-off)", thr_glm),
              metric_row(cm_rf,  auc(roc_rf),  "Random forest (Youden cut-off)", thr_rf))
rownames(perf) <- NULL
perf
)-")
doc <- add_table(doc, perf, tab_cap("Test-set performance comparison"), digits = 3)
doc <- run_chunk(doc, r"-(
roc_df <- bind_rows(
  data.frame(Model = sprintf("Logistic (AUC %.3f)", auc(roc_glm)),
             FPR = 1 - roc_glm$specificities, TPR = roc_glm$sensitivities),
  data.frame(Model = sprintf("Random forest (AUC %.3f)", auc(roc_rf)),
             FPR = 1 - roc_rf$specificities, TPR = roc_rf$sensitivities))
p_roc <- ggplot(roc_df, aes(FPR, TPR, colour = Model)) +
  geom_abline(linetype = "dashed", colour = "grey60") +
  geom_path(linewidth = 1.1) +
  annotate("point", x = 1 - cm_glm$byClass["Specificity"], y = cm_glm$byClass["Sensitivity"],
           size = 3.5, colour = "#1F4E79") +
  annotate("text", x = 1 - cm_glm$byClass["Specificity"] + 0.03,
           y = cm_glm$byClass["Sensitivity"] - 0.05, hjust = 0, size = 3.2,
           label = "chosen logistic cut-off") +
  scale_colour_manual(values = c("#1F4E79", "#F4A259"), name = NULL) +
  coord_equal() +
  labs(title = "ROC curves on the held-out test set",
       x = "False positive rate (1 - specificity)", y = "True positive rate (sensitivity)")
)-", show_output = FALSE)
doc <- add_plot(doc, p_roc, fig_path("week3", "03_roc_curves.png"),
                fig_cap("ROC curves of both models on the test set"), width = 5.5, height = 5)
doc <- run_chunk(doc, r"-(
cm_df <- as.data.frame(cm_glm$table)
p_cm <- ggplot(cm_df, aes(Reference, Prediction, fill = Freq)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = Freq), size = 7, fontface = "bold") +
  scale_fill_gradient(low = "#EAF4F2", high = "#2E86AB") +
  labs(title = "Confusion matrix - logistic regression (test set)",
       subtitle = sprintf("Threshold = %.3f", thr_glm),
       x = "Actual outcome", y = "Predicted outcome") +
  theme(legend.position = "none", panel.grid = element_blank())
)-", show_output = FALSE)
doc <- add_plot(doc, p_cm, fig_path("week3", "04_confusion_matrix.png"),
                fig_cap("Confusion matrix for the logistic regression model"), width = 4.5, height = 3.8)
doc <- bold_para(doc, "Interpretation: ", sprintf(paste(
  "on %d unseen applicants the logistic model reaches an AUC of %.3f: if we pick one random bad and",
  "one random good applicant, the model ranks the bad one as riskier %.0f%% of the time. With the",
  "tuned threshold it catches %s of the bad loans (sensitivity), while correctly approving %s of the",
  "good ones (specificity). Using the naive 0.5 cut-off would catch only %s of the bad loans. The random",
  "forest reaches an AUC of %.3f; the DeLong test comparing the two ROC curves gives p = %.3f, so %s"),
  nrow(test), auc(roc_glm), 100 * auc(roc_glm), pct(cm_glm$byClass["Sensitivity"]),
  pct(cm_glm$byClass["Specificity"]), pct(cm_05$byClass["Sensitivity"]),
  auc(roc_rf), roc.test(roc_glm, roc_rf)$p.value,
  ifelse(roc.test(roc_glm, roc_rf)$p.value > 0.05,
         "there is no significant difference. The simpler, interpretable logistic model is therefore preferred.",
         "the difference is statistically significant and is weighed against interpretability below.")))

# -----------------------------------------------------------------------------
doc <- h1(doc, "9. Model Diagnostics")
doc <- para(doc,
  "For interpretation and diagnostics, the same logistic model is re-fitted with glm() on the ",
  "unscaled training data, so that coefficients are expressed in the original units. Its predictions ",
  "are identical to the caret model's.")
doc <- run_chunk(doc, r"-(
train_glm <- train |> mutate(Default = as.integer(Status == "bad")) |> select(-Status)
logit <- glm(Default ~ ., data = train_glm, family = binomial)
cat("Null deviance:", round(logit$null.deviance, 1), " on", logit$df.null, "df\n")
cat("Residual deviance:", round(deviance(logit), 1), " on", logit$df.residual, "df\n")
cat("AIC:", round(AIC(logit), 1), "\n")
cat("McFadden pseudo R-squared:", round(1 - logit$deviance / logit$null.deviance, 3), "\n")
# Likelihood-ratio test of the full model against the intercept-only model
anova(glm(Default ~ 1, data = train_glm, family = binomial), logit, test = "Chisq")
)-")
doc <- h2(doc, "9.1 Residual analysis (binned residual plot)")
doc <- para(doc,
  "Raw residuals of a logistic model are only 0/1 minus a probability, so they are not informative ",
  "individually. The binned residual plot groups observations by predicted probability and plots the ",
  "average residual in each bin; about 95% of bins should fall inside the ±2 standard-error band.")
doc <- run_chunk(doc, r"-(
bins <- data.frame(p = fitted(logit), y = train_glm$Default) |>
  mutate(bin = ntile(p, 40)) |>
  group_by(bin) |>
  summarise(p_mean = mean(p), resid = mean(y - p), n = n(),
            se = 2 * sqrt(mean(p) * (1 - mean(p)) / n()))
cat("Share of bins inside the +/-2 SE band:",
    round(mean(abs(bins$resid) <= bins$se), 3), "\n")
p_bin <- ggplot(bins, aes(p_mean, resid)) +
  geom_ribbon(aes(ymin = -se, ymax = se), fill = "grey85") +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_point(colour = "#1F4E79", size = 2) +
  labs(title = "Binned residual plot (training data, 40 bins)",
       subtitle = "Grey band = +/- 2 standard errors",
       x = "Mean predicted probability of default", y = "Mean residual (observed - predicted)")
)-")
doc <- add_plot(doc, p_bin, fig_path("week3", "05_binned_residuals.png"),
                fig_cap("Binned residual plot for the logistic regression"), height = 3.8)
doc <- h2(doc, "9.2 Calibration and goodness of fit")
doc <- run_chunk(doc, r"-(
# Hosmer-Lemeshow test (10 groups), implemented directly
hl <- data.frame(p = fitted(logit), y = train_glm$Default) |>
  mutate(g = ntile(p, 10)) |> group_by(g) |>
  summarise(obs = sum(y), exp = sum(p), n = n())
hl_stat <- sum((hl$obs - hl$exp)^2 / (hl$exp * (1 - hl$exp / hl$n)))
cat("Hosmer-Lemeshow chi-square =", round(hl_stat, 2), ", df = 8, p-value =",
    round(pchisq(hl_stat, df = 8, lower.tail = FALSE), 4), "\n")

calib <- test |> mutate(decile = ntile(p_glm, 10)) |>
  group_by(decile) |>
  summarise(predicted = mean(p_glm), observed = mean(Status == "bad"), n = n())
p_cal <- ggplot(calib, aes(predicted, observed)) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  geom_line(colour = "#1F4E79") + geom_point(size = 2.5, colour = "#1F4E79") +
  scale_x_continuous(labels = percent, limits = c(0, 1)) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  coord_equal() +
  labs(title = "Calibration on the test set", subtitle = "Deciles of predicted risk",
       x = "Mean predicted default probability", y = "Observed default rate")
)-")
doc <- add_plot(doc, p_cal, fig_path("week3", "06_calibration.png"),
                fig_cap("Calibration plot: predicted vs. observed default rate by decile"),
                width = 5, height = 5)
doc <- h2(doc, "9.3 Influential observations")
doc <- run_chunk(doc, r"-(
cooks <- cooks.distance(logit)
cut_cook <- 4 / nrow(train_glm)
cat("Observations with Cook's distance > 4/n:", sum(cooks > cut_cook),
    sprintf("(%.1f%%)\n", 100 * mean(cooks > cut_cook)))
cat("Maximum Cook's distance:", round(max(cooks), 4), "\n")
)-")
doc <- add_code(doc, r"-(
plot(cooks, type = "h", col = "#1F4E79", main = "Cook's distance",
     ylab = "Cook's distance", xlab = "Training observation")
abline(h = cut_cook, col = "#D1495B", lty = 2)
)-")
doc <- add_base_plot(doc, function() {
  plot(cooks, type = "h", col = "#1F4E79", main = "Cook's distance",
       ylab = "Cook's distance", xlab = "Training observation")
  abline(h = cut_cook, col = "#D1495B", lty = 2)
}, fig_path("week3", "07_cooks_distance.png"), fig_cap("Cook's distance for each training observation"),
height = 3.2)
bin_in <- mean(abs(bins$resid) <= bins$se)
hl_p <- pchisq(hl_stat, df = 8, lower.tail = FALSE)
doc <- bold_para(doc, "Diagnostic summary: ", sprintf(paste(
  "%s of the binned residuals fall inside the ±2 SE band, so there is no systematic",
  "misfit across the risk range. The Hosmer-Lemeshow test gives p = %.3f (%s). On the test set, the calibration",
  "points lie close to the diagonal, meaning a predicted probability of 40%% corresponds to roughly a 40%%",
  "observed default rate. This matters if the probabilities are used for pricing or provisioning. No",
  "single observation has a Cook's distance near 1, so no individual applicant is driving the results;",
  "the winsorization applied in Week 1 helped here."),
  pct(bin_in, 0), hl_p, ifelse(hl_p > 0.05, "no evidence of lack of fit",
                               "some evidence of lack of fit, discussed under improvements")))

# -----------------------------------------------------------------------------
doc <- h1(doc, "10. Interpretation of the Logistic Regression Model")
doc <- run_chunk(doc, r"-(
coef_tbl <- summary(logit)$coefficients
or_tbl <- data.frame(Term = rownames(coef_tbl),
                     Estimate = coef_tbl[, 1], Std_Error = coef_tbl[, 2],
                     z = coef_tbl[, 3], p_value = coef_tbl[, 4],
                     Odds_ratio = exp(coef_tbl[, 1]),
                     OR_low = exp(coef_tbl[, 1] - 1.96 * coef_tbl[, 2]),
                     OR_high = exp(coef_tbl[, 1] + 1.96 * coef_tbl[, 2]))
rownames(or_tbl) <- NULL
or_tbl |> mutate(across(c(Estimate, Std_Error, z), ~ round(.x, 4)),
                 across(c(Odds_ratio, OR_low, OR_high), ~ round(.x, 3)),
                 p_value = signif(p_value, 3))
)-", max_lines = 40)
doc <- run_chunk(doc, r"-(
p_or <- or_tbl |>
  filter(Term != "(Intercept)", p_value < 0.05) |>
  ggplot(aes(x = Odds_ratio, y = reorder(Term, Odds_ratio))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbar(aes(xmin = OR_low, xmax = OR_high), width = 0.25, colour = "grey40",
                orientation = "y") +
  geom_point(aes(colour = Odds_ratio > 1), size = 3) +
  scale_x_log10() +
  scale_colour_manual(values = c(`TRUE` = "#D1495B", `FALSE` = "#2E86AB"),
                      labels = c("Reduces risk", "Increases risk"), name = NULL) +
  labs(title = "Significant predictors of default (odds ratios, 95% CI)",
       subtitle = "Per one-unit increase (numeric) or vs. reference level (categorical); log scale",
       x = "Odds ratio", y = NULL)
)-", show_output = FALSE)
doc <- add_plot(doc, p_or, fig_path("week3", "08_odds_ratios.png"),
                fig_cap("Odds ratios of statistically significant predictors"), height = 4.6)
doc <- run_chunk(doc, r"-(
imp <- varImp(fit_rf)$importance |>
  tibble::rownames_to_column("Variable") |>
  arrange(desc(Overall)) |> head(12)
p_imp <- ggplot(imp, aes(Overall, reorder(Variable, Overall))) +
  geom_col(fill = "#F4A259") +
  labs(title = "Random forest variable importance (top 12)",
       x = "Relative importance (0-100)", y = NULL)
)-", show_output = FALSE)
doc <- add_plot(doc, p_imp, fig_path("week3", "09_rf_importance.png"),
                fig_cap("Variable importance from the random forest benchmark"), height = 3.8)
orv <- function(term) or_tbl$Odds_ratio[or_tbl$Term == term]
doc <- para(doc, "Key effects, holding all other variables constant:")
doc <- bullets(doc, c(
  sprintf("A past arrears record multiplies the odds of default by %.2f (Recordsyes).", orv("Recordsyes")),
  sprintf("Part-time employment multiplies the odds by %.2f compared with a fixed contract; freelance by %.2f.", orv("Jobpartime"), orv("Jobfreelance")),
  sprintf("Each additional year of seniority multiplies the odds by %.3f, i.e. about %.1f%% lower odds per year; ten years lowers the odds by about %.0f%%.",
          orv("Seniority"), 100 * (1 - orv("Seniority")), 100 * (1 - orv("Seniority")^10)),
  sprintf("A 10-percentage-point increase in the loan-to-price ratio multiplies the odds by %.2f.", orv("LoanToPrice")^0.1),
  sprintf("Renting (vs. the reference housing level 'other') has an odds ratio of %.2f, and owning a home %.2f.", orv("Homerent"), orv("Homeowner")),
  sprintf("Missing income remains significant after imputation (odds ratio %.2f), which confirms the Week 1 decision to keep the missing-value flag.", orv("Income_missingyes"))))
doc <- para(doc,
  "The random forest's importance ranking largely agrees with the logistic model: income, seniority, ",
  "loan amount, loan-to-price, arrears records and employment type are among the most influential ",
  "variables. This agreement between a linear and a non-linear model increases confidence that these ",
  "are genuine risk drivers.")

# -----------------------------------------------------------------------------
doc <- h1(doc, "11. Strengths, Limitations and Improvements")
doc <- h2(doc, "11.1 Strengths")
doc <- bullets(doc, c(
  "Interpretable: every coefficient converts into an odds ratio a credit officer can understand and audit.",
  "Validated honestly: stratified hold-out test set, repeated 10-fold cross-validation, and a threshold chosen without touching the test data.",
  "Well calibrated probabilities, so scores can be used directly for risk-based pricing or expected-loss estimates.",
  "Stable: acceptable VIFs, no dominant influential observations, and results consistent with an independent random forest model."))
doc <- h2(doc, "11.2 Limitations")
doc <- bullets(doc, c(
  sprintf("Discrimination is good but not excellent (AUC about %.2f); some bad loans remain hard to distinguish using only 14 application variables.", auc(roc_glm)),
  "The linear-logit assumption may miss non-linear effects (for example the non-monotonic age pattern seen in Week 2).",
  "Class imbalance means precision for the 'bad' class is moderate: some good applicants are flagged for review.",
  "The dataset is historical and from a single lender, so the model may not transfer to other populations or economic conditions without re-validation."))
doc <- h2(doc, "11.3 Potential improvements")
doc <- bullets(doc, c(
  "Add non-linear terms (splines for Age and Seniority) or interaction terms (e.g. Job x Home, found in the Week 2 heatmap).",
  "Try gradient boosting (xgboost) and regularised regression (glmnet) with the same cross-validation framework.",
  "Choose the threshold with a business cost matrix (the cost of a missed default vs. a lost good customer) rather than Youden's J; this is developed in the Week 4 report.",
  "Handle imbalance explicitly with class weights or SMOTE/down-sampling inside the cross-validation loop.",
  "Enrich the data with bureau scores, transaction history or macro-economic indicators."))

# -----------------------------------------------------------------------------
doc <- h1(doc, "12. Conclusion")
doc <- para(doc, sprintf(paste(
  "The hypothesis tests confirmed, with statistical rigour, the risk drivers suggested visually in",
  "Week 2: past arrears, employment type, job seniority, income and loan-to-price all differ",
  "significantly between good and bad borrowers. A logistic regression combining these factors",
  "achieves a test-set AUC of %.3f, catches %s of defaults at the tuned threshold, and produces",
  "well-calibrated, interpretable probabilities. A random forest benchmark (AUC %.3f) did not justify",
  "giving up that interpretability. The model is a solid, explainable baseline credit score. Week 4",
  "integrates these results with the cleaning and visualization work into a final report with business",
  "recommendations."),
  auc(roc_glm), pct(cm_glm$byClass["Sensitivity"]), auc(roc_rf)))

# Save results for the Week 4 report
saveRDS(list(test_summary = test_summary, perf = perf, thr_glm = thr_glm, thr_rf = thr_rf,
             auc_glm = as.numeric(auc(roc_glm)), auc_rf = as.numeric(auc(roc_rf)),
             auc_ci_glm = as.numeric(ci.auc(roc_glm)), cv_stats = cv_stats,
             or_tbl = or_tbl, test = test, cm_glm = cm_glm$table, calib = calib,
             hl_p = hl_p, pseudo_r2 = 1 - logit$deviance / logit$null.deviance,
             logit = logit, fit_glm = fit_glm),
        "outputs/models/week3_results.rds")

save_report(doc, "reports/Week3_Statistical_Modeling_Report.docx")
