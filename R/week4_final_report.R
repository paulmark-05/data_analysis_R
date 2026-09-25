# =============================================================================
# WEEK 4 - Comprehensive Data Analysis Report and Presentation
# Integrates Weeks 1-3 into one final report and adds a business-impact
# analysis (cost-based approval threshold, approval strategy, risk grades).
#
# Input  : outputs of week1/2/3 scripts (run them first, or use run_all.R)
# Output : reports/Week4_Final_Comprehensive_Report.docx
# Run    : source("R/week4_final_report.R")  from the project root
# =============================================================================

source("R/00_helpers.R")
reset_numbering()
needed <- c("outputs/models/week1_summary.rds", "outputs/figures/week2/13_risk_ladder.png",
            "outputs/models/week3_results.rds")
if (!all(file.exists(needed))) stop("Run weeks 1-3 first (source('run_all.R')).")
suppressPackageStartupMessages({ library(caret); library(pROC) })

credit <- readRDS("data/processed/credit_clean.rds")
w1 <- readRDS("outputs/models/week1_summary.rds")
w3 <- readRDS("outputs/models/week3_results.rds")
f1 <- function(n) file.path("outputs/figures/week1", n)
f2 <- function(n) file.path("outputs/figures/week2", n)
f3 <- function(n) file.path("outputs/figures/week3", n)

cs  <- w1$cat_summary
br  <- function(var, level) cs$Bad_rate_pct[cs$Variable == var & cs$Level == level]
orv <- function(term) w3$or_tbl$Odds_ratio[w3$or_tbl$Term == term]
perf_y <- w3$perf[w3$perf$Model == "Logistic (Youden cut-off)", ]

doc <- read_docx()
doc <- title_page(doc,
  title      = "Predicting Credit Default with R",
  subtitle   = "Final Comprehensive Report: Data Preparation, Visualization, Modeling and Business Impact",
  week_label = "WEEK 4 TASK: FINAL REPORT")
doc <- contents_list(doc, c(
  "Executive Summary",
  "Introduction",
  "Data Preparation",
  "Exploratory and Visual Analysis",
  "Statistical Testing and Predictive Modeling",
  "Results: Business Impact of the Model",
  "Discussion",
  "Conclusion: Lessons Learned, Challenges and Future Directions",
  "Appendix A: Project Structure and Reproducibility",
  "Appendix B: Annotated Key R Code"))

# -----------------------------------------------------------------------------
# Business-impact analysis is computed up-front so the executive summary can
# quote it. The code is shown again in Section 6.
# -----------------------------------------------------------------------------
test <- w3$test
cv   <- w3$fit_glm$pred
COST_FN <- 5; COST_FP <- 1
cost_at <- function(p, y, t) {
  flag <- p >= t
  c(FN = sum(!flag & y == "bad"), FP = sum(flag & y == "good"))
}
grid <- seq(0.05, 0.80, by = 0.005)
cv_cost <- sapply(grid, function(t) { k <- cost_at(cv$bad, cv$obs, t); COST_FN * k["FN"] + COST_FP * k["FP"] })
thr_cost <- grid[which.min(cv_cost)]
strategy <- function(name, t) {
  flag <- test$p_glm >= t
  k <- cost_at(test$p_glm, test$Status, t)
  data.frame(Strategy = name, Threshold = t, Approval_rate = mean(!flag),
             Bad_rate_approved = mean(test$Status[!flag] == "bad"),
             Defaults_caught = sum(flag & test$Status == "bad") / sum(test$Status == "bad"),
             Good_rejected = sum(flag & test$Status == "good") / sum(test$Status == "good"),
             Cost_units = unname(COST_FN * k["FN"] + COST_FP * k["FP"]))
}
strat_tbl <- rbind(strategy("Approve everyone (no model)", 1.01),
                   strategy("Model: 0.5 cut-off", 0.5),
                   strategy("Model: Youden cut-off", w3$thr_glm),
                   strategy("Model: cost-optimal cut-off", thr_cost))
strat_tbl$Cost_saving <- 1 - strat_tbl$Cost_units / strat_tbl$Cost_units[1]
best <- strat_tbl[4, ]; base <- strat_tbl[1, ]

# -----------------------------------------------------------------------------
doc <- h1(doc, "1. Executive Summary")
doc <- para(doc,
  "This project used R to build an end-to-end, reproducible analysis of a public credit-scoring ",
  sprintf("dataset of %s loan applicants. The goal was to understand what drives loan default and to ",
          comma(w1$n_raw)),
  "build a model that helps a lender decide which applications to approve automatically, review ",
  "manually, or decline. The work followed four stages: data cleaning (Week 1), visualization (Week 2), ",
  "statistical testing and predictive modeling (Week 3), and integration into business ",
  "recommendations (this report).")
doc <- para(doc, "Key findings:")
doc <- bullets(doc, c(
  sprintf("Around %s of loans default. Risk is concentrated, not random: it is driven by credit history, employment stability and affordability.",
          pct(mean(credit$Default))),
  sprintf("Applicants with past arrears default at %s%% versus %s%% without; part-time workers at %s%% versus %s%% for fixed contracts; home owners at only %s%%.",
          br("Records", "yes"), br("Records", "no"), br("Job", "partime"), br("Job", "fixed"), br("Home", "owner")),
  "All six formal hypothesis tests were significant (p < 0.001); seniority and loan-to-price ratio showed the largest effect sizes.",
  sprintf("A logistic regression credit score achieved a test-set AUC of %.3f (95%% CI %.3f-%.3f), matched a random-forest benchmark (AUC %.3f), and produced well-calibrated probabilities (Hosmer-Lemeshow p = %.2f).",
          w3$auc_glm, w3$auc_ci_glm[1], w3$auc_ci_glm[3], w3$auc_rf, w3$hl_p),
  sprintf("Business impact: assuming a missed default costs %d times as much as a wrongly declined good customer, the model's cost-optimal policy reduces total credit-decision cost by %s compared with approving everyone. It cuts the default rate among approved loans from %s to %s while still approving %s of applicants.",
          COST_FN, pct(best$Cost_saving, 0), pct(base$Bad_rate_approved), pct(best$Bad_rate_approved),
          pct(best$Approval_rate, 0))))
doc <- para(doc, "Main recommendations: adopt the score as a pre-screening tool with three risk grades ",
                 "(auto-approve, manual review, decline), make credit history and employment type verified ",
                 "fields, require documentation when income is not declared, and monitor the model quarterly.")
doc <- body_add_break(doc)

# -----------------------------------------------------------------------------
doc <- h1(doc, "2. Introduction")
doc <- h2(doc, "2.1 Business context")
doc <- para(doc,
  "Lending is a business of managing risk. Every approved loan that defaults costs the lender part of ",
  "the principal, collection effort and capital; every good applicant who is turned away is lost ",
  "revenue and a lost customer relationship. Data-driven credit scoring lets a lender make these ",
  "decisions consistently, quickly and transparently, and explain them to customers and regulators.")
doc <- h2(doc, "2.2 Objectives and research questions")
doc <- bullets(doc, c(
  "Q1 - Data: Is the available data reliable enough for decision-making, and how must it be cleaned?",
  "Q2 - Patterns: Which applicant characteristics are associated with default, and how strongly?",
  "Q3 - Evidence: Are these associations statistically significant once chance is accounted for?",
  "Q4 - Prediction: How accurately can default be predicted from application data?",
  "Q5 - Impact: How should the model be used, and what is it worth to the business?"))
doc <- h2(doc, "2.3 Dataset")
doc <- para(doc,
  "The public Credit Scoring dataset (Belanche Muñoz, via github.com/gastonstat/CreditScoring; ",
  "available in R as modeldata::credit_data) contains 14 variables per applicant: the outcome Status ",
  "(good/bad), 8 numeric variables (seniority, loan term, age, expenses, income, assets, debt, loan ",
  "amount, price of goods) and 5 categorical variables (home, marital status, arrears records, job).")
doc <- h2(doc, "2.4 Approach and tools")
doc <- para(doc,
  "All work was done in R ", as.character(getRversion()), ". dplyr and tidyr were used for data ",
  "manipulation; ggplot2, lattice and base graphics for visualization; stats, caret and pROC for testing, ",
  "modeling and evaluation; and officer and flextable to generate these Word reports directly from ",
  "code, so every number, table and chart is reproducible. The workflow is summarised below.")
workflow <- data.frame(
  Stage = c("1. Data preparation", "2. Visual analysis", "3. Statistics & modeling", "4. Integration"),
  `Main activities` = c("Audit, duplicates, missing values, outliers, feature engineering, scaling, encoding",
                        "13 charts: composition, comparisons, trends, relationships, anomalies",
                        "Assumption checks, 6 hypothesis tests, logistic regression & random forest, CV, diagnostics",
                        "Cost-based decision policy, risk grades, recommendations"),
  Output = c("credit_clean.csv, Week 1 report", "Figures, Week 2 report",
             "Trained models, Week 3 report", "This final report"),
  check.names = FALSE)
doc <- add_table(doc, workflow, tab_cap("Project workflow"))

# -----------------------------------------------------------------------------
doc <- h1(doc, "3. Data Preparation")
doc <- para(doc,
  sprintf(paste("The raw data had %s rows, of which %d (%s) contained at least one missing value.",
                "Missing values occurred in six columns, most notably Income (8.6%%)."),
          comma(w1$n_raw), w1$n_incomplete, pct(w1$n_incomplete / w1$n_raw)))
doc <- add_image(doc, f1("01_missing_values.png"), fig_cap("Missing values per variable in the raw data"),
                 width = 5.8)
doc <- h2(doc, "3.1 Cleaning methodology")
doc <- add_table(doc, w1$cleaning_log, tab_cap("Cleaning and transformation log (Week 1)"))
doc <- para(doc, "Justification of the main choices:")
doc <- bullets(doc, c(
  sprintf("Imputation instead of deletion: dropping incomplete rows would have discarded %s of applicants who default at a different rate, which would bias the analysis.",
          pct(w1$n_incomplete / w1$n_raw)),
  sprintf("Informative missingness: applicants without a declared income defaulted at %s versus %s (chi-square p < 0.001), so a flag variable was kept. It later proved to be one of the strongest model predictors (odds ratio %.1f).",
          pct(w1$inc_miss_bad), pct(w1$inc_ok_bad), orv("Income_missingyes")),
  "Group-wise median imputation: income depends on job type, so each missing income was filled with the median of the applicant's job group; the overall income distribution was preserved.",
  "Winsorization instead of removal: extreme monetary values (e.g. assets up to 300,000) are plausible rather than errors, so they were capped at the 1st/99th percentiles rather than deleted.",
  "Feature engineering: ratios such as loan-to-price and payment burden express affordability better than raw amounts; loan-to-price became a top predictor."))
doc <- add_code(doc, r"-(
# Key cleaning steps (excerpt from R/week1_data_cleaning.R)
clean <- raw[!duplicated(raw), ]                                  # 1. remove duplicates
clean$Home[which(clean$Home == "ignore")] <- "other"              # 2. fix invalid level
for (v in c("Home", "Marital", "Job"))                            # 3. mode imputation
  clean[[v]][is.na(clean[[v]])] <- mode_value(clean[[v]])
clean$Income_missing <- factor(ifelse(is.na(clean$Income), "yes", "no"))  # 4. keep signal
clean <- clean |> group_by(Job) |>                                # 5. group-median income
  mutate(Income = ifelse(is.na(Income), median(Income, na.rm = TRUE), Income)) |> ungroup()
for (v in c("Income", "Assets", "Debt", "Amount", "Price"))       # 6. winsorize 1%/99%
  clean[[v]] <- winsorize(clean[[v]])
clean <- clean |> mutate(LoanToPrice = Amount / Price,            # 7. engineered ratios
                         PaymentBurden = (Amount / Time) / Income)
)-")
doc <- h2(doc, "3.2 Resulting dataset")
doc <- run_chunk(doc, r"-(
cat("Clean dataset:", nrow(credit), "applicants x", ncol(credit), "variables;",
    "missing values:", sum(is.na(credit)), "\n")
credit |> summarise(across(c(Age, Seniority, Income, Amount, LoanToPrice),
                           list(mean = mean, median = median))) |>
  pivot_longer(everything(), names_to = c("Variable", ".value"), names_sep = "_") |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))
)-")
doc <- para(doc,
  "With a clean, complete and consistent dataset in place, the next step was to explore it visually ",
  "and find which characteristics separate good borrowers from bad ones.")

# -----------------------------------------------------------------------------
doc <- h1(doc, "4. Exploratory and Visual Analysis")
doc <- para(doc,
  "Thirteen visualizations were produced in Week 2. The four most decision-relevant are reproduced ",
  "here; each one answers a business question.")
doc <- h2(doc, "4.1 Employment stability")
doc <- add_image(doc, f2("03_badrate_job.png"), fig_cap("Default rate by employment type (from Week 2)"))
doc <- para(doc, sprintf(
  "Part-time workers default at %s%%, almost three times the %s%% of fixed-contract employees; the confidence intervals do not overlap. Income volatility translates directly into repayment risk.",
  br("Job", "partime"), br("Job", "fixed")))
doc <- h2(doc, "4.2 Experience and age")
doc <- add_image(doc, f2("07_trend_age_seniority.png"), fig_cap("Default rate trends by age and job seniority (from Week 2)"))
doc <- para(doc,
  "Default risk falls sharply over the first five years with an employer and continues to decline ",
  "after that. The age effect points the same way but is weaker and not strictly monotonic. In the ",
  "multivariate model, once seniority is accounted for, age has only a small residual effect.")
doc <- h2(doc, "4.3 Combined risk factors")
doc <- add_image(doc, f2("10_heatmap_job_home.png"), fig_cap("Default rate by employment type and housing (from Week 2)"))
seg_rate <- function(j, h) round(100 * mean(credit$Default[credit$Job == j & credit$Home == h]), 1)
doc <- para(doc, sprintf(paste(
  "Risk factors compound: part-time workers who rent default at %s%% and those in 'other' housing",
  "at %s%%, while fixed-contract home owners default at only %s%%. This is why a multivariate model",
  "is needed rather than single-variable rules."),
  seg_rate("partime", "rent"), seg_rate("partime", "other"), seg_rate("fixed", "owner")))
doc <- h2(doc, "4.4 Overall ranking of risk drivers")
doc <- add_image(doc, f2("13_risk_ladder.png"), fig_cap("Risk ladder: segment default rates versus the portfolio average (from Week 2)"),
                 width = 6)
doc <- para(doc,
  "The risk ladder summarises the visual analysis: part-time employment, undeclared income and past ",
  "arrears raise risk the most, while home ownership, fixed employment and older age groups lower it. ",
  "These visual patterns were then tested formally.")

# -----------------------------------------------------------------------------
doc <- h1(doc, "5. Statistical Testing and Predictive Modeling")
doc <- h2(doc, "5.1 Hypothesis tests")
doc <- para(doc,
  "Normality tests (Shapiro-Wilk, Q-Q plots) showed that the numeric variables are strongly skewed, so ",
  "non-parametric Wilcoxon tests and Welch t-tests (which allow unequal variances) were used. Categorical ",
  "associations were tested with chi-square tests, with effect sizes reported alongside.")
ts <- w3$test_summary |>
  mutate(Statistic = round(Statistic, 2), Effect_size = round(Effect_size, 3),
         p_value = format.pval(p_value, digits = 3, eps = 1e-16))
doc <- add_table(doc, ts, tab_cap("Summary of hypothesis tests (Week 3)"))
doc <- para(doc,
  "All six null hypotheses were rejected. Statistical significance is expected with more than 4,000 ",
  "observations, so the effect sizes are the more informative result: seniority and loan-to-price show ",
  "medium-sized effects, arrears records and job type small-to-medium ones.")
doc <- h2(doc, "5.2 Model development")
doc <- para(doc,
  "Before modelling, multicollinearity was checked with variance inflation factors. Price was dropped ",
  "because it is almost fully explained by Amount and LoanToPrice (VIF > 14). The data was split ",
  "75/25 with stratification, and two models were trained with 10-fold cross-validation repeated 3 ",
  "times, using ROC-AUC as the selection metric:")
doc <- add_code(doc, r"-(
# Model training (excerpt from R/week3_modeling.R)
train_idx <- createDataPartition(model_df$Status, p = 0.75, list = FALSE)   # stratified split
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,       # 10-fold CV x 3
                     classProbs = TRUE, summaryFunction = twoClassSummary)
fit_glm <- train(Status ~ ., data = train, method = "glm", family = binomial,
                 preProcess = c("center", "scale"), metric = "ROC", trControl = ctrl)
fit_rf  <- train(Status ~ ., data = train, method = "rf", metric = "ROC", trControl = ctrl)
# threshold chosen on out-of-fold predictions (Youden's J), never on the test set
thr <- coords(roc(fit_glm$pred$obs, fit_glm$pred$bad), "best", best.method = "youden")
)-")
doc <- h2(doc, "5.3 Model performance")
doc <- add_table(doc, w3$perf, tab_cap("Test-set performance (1,112 unseen applicants)"), digits = 3)
doc <- add_image(doc, f3("03_roc_curves.png"), fig_cap("ROC curves on the test set (from Week 3)"), width = 4.6)
doc <- para(doc, sprintf(paste(
  "The logistic regression reaches an AUC of %.3f on unseen data, essentially identical to the random",
  "forest (%.3f; DeLong test not significant). At the tuned threshold it identifies %s of the",
  "defaults while approving %s of the good applicants. The simpler model was selected because it is",
  "equally accurate, transparent, and easy to explain to applicants and regulators."),
  w3$auc_glm, w3$auc_rf, pct(perf_y$Sensitivity), pct(perf_y$Specificity)))
doc <- h2(doc, "5.4 What the model says: drivers of default")
doc <- add_image(doc, f3("08_odds_ratios.png"), fig_cap("Odds ratios of significant predictors (from Week 3)"))
doc <- bullets(doc, c(
  sprintf("Past arrears record: odds of default x%.1f.", orv("Recordsyes")),
  sprintf("Part-time employment: x%.1f versus a fixed contract; freelance: x%.1f.", orv("Jobpartime"), orv("Jobfreelance")),
  sprintf("Undeclared income: x%.1f, even after imputing a typical income.", orv("Income_missingyes")),
  sprintf("Each extra year of job seniority: odds down by %.1f%%.", 100 * (1 - orv("Seniority"))),
  sprintf("Home owner versus 'other' housing: odds x%.2f (less than half).", orv("Homeowner")),
  sprintf("Financing 10 percentage points more of the purchase: odds x%.2f.", orv("LoanToPrice")^0.1)))
doc <- h2(doc, "5.5 Diagnostics")
doc <- add_image(doc, f3("06_calibration.png"), fig_cap("Calibration of predicted probabilities (from Week 3)"), width = 4)
doc <- para(doc, sprintf(paste(
  "The model's probabilities are well calibrated (Hosmer-Lemeshow p = %.2f; points close to the",
  "diagonal), binned residuals show no systematic pattern, and no observation is unduly influential.",
  "McFadden's pseudo R-squared is %.3f, which is a good value for individual-level credit data."),
  w3$hl_p, w3$pseudo_r2))

# -----------------------------------------------------------------------------
doc <- h1(doc, "6. Results: Business Impact of the Model")
doc <- para(doc,
  "A statistical score only creates value when it is turned into a decision rule. This section ",
  "translates the model into an approval policy and quantifies its effect.")
doc <- h2(doc, "6.1 Choosing the cut-off with business costs")
doc <- para(doc, sprintf(paste(
  "Two errors are possible: approving a loan that defaults (false negative) and declining a good",
  "applicant (false positive). Their costs are not equal. Losing much of the principal on a default",
  "typically costs several times more than the margin lost on one rejected good loan. A cost ratio of",
  "%d:1 was assumed (a common rule-of-thumb; the sensitivity to this assumption is shown in 6.3). The",
  "cut-off that minimises total cost was chosen on the cross-validation predictions, and then evaluated",
  "once on the test set."), COST_FN))
doc <- run_chunk(doc, r"-(
COST_FN <- 5; COST_FP <- 1            # missed default costs 5x a wrongly declined good loan
cv  <- w3$fit_glm$pred               # out-of-fold CV predictions (training data only)
grid <- seq(0.05, 0.80, by = 0.005)
cost_curve <- data.frame(threshold = grid, cost = sapply(grid, function(t) {
  COST_FN * sum(cv$bad <  t & cv$obs == "bad") + COST_FP * sum(cv$bad >= t & cv$obs == "good")
}))
thr_cost <- cost_curve$threshold[which.min(cost_curve$cost)]
cat("Cost-optimal threshold (CV):", thr_cost,
    "| theoretical optimum for a calibrated model = 1/(1+5) =", round(1 / 6, 3), "\n")
)-")
doc <- run_chunk(doc, r"-(
p_cost <- ggplot(cost_curve, aes(threshold, cost / max(cost))) +
  geom_line(colour = "#1F4E79", linewidth = 1.1) +
  geom_vline(xintercept = thr_cost, linetype = "dashed", colour = "#D1495B") +
  geom_vline(xintercept = w3$thr_glm, linetype = "dotted", colour = "grey40") +
  annotate("text", x = thr_cost + 0.01, y = 0.95, hjust = 0, size = 3.3, colour = "#D1495B",
           label = paste("cost-optimal =", thr_cost)) +
  annotate("text", x = w3$thr_glm + 0.01, y = 0.85, hjust = 0, size = 3.3, colour = "grey30",
           label = paste("Youden =", round(w3$thr_glm, 3))) +
  labs(title = "Total decision cost by probability cut-off (5:1 cost ratio)",
       subtitle = "Cross-validation predictions; cost scaled to the maximum",
       x = "Decline if predicted probability of default >= cut-off", y = "Relative total cost")
)-", show_output = FALSE)
doc <- add_plot(doc, p_cost, fig_path("week4", "01_cost_curve.png"),
                fig_cap("Relative total decision cost across probability cut-offs"), height = 3.6)
doc <- h2(doc, "6.2 Comparison of decision strategies")
doc <- run_chunk(doc, r"-(
strat_tbl |>
  mutate(Threshold = ifelse(Threshold > 1, NA, round(Threshold, 3)),
         across(c(Approval_rate, Bad_rate_approved, Defaults_caught, Good_rejected, Cost_saving),
                ~ paste0(round(100 * .x, 1), "%")))
)-")
doc <- para(doc, sprintf(paste(
  "Without a model the lender approves everyone and %s of approved loans go bad. With the",
  "cost-optimal cut-off the model catches %s of the defaults. The price is declining or reviewing",
  "%s of good applicants, and total decision cost falls by %s. The 0.5 cut-off approves more",
  "applicants but lets many more defaults through, which is why it is the most expensive of the three",
  "model policies."),
  pct(base$Bad_rate_approved), pct(best$Defaults_caught), pct(best$Good_rejected), pct(best$Cost_saving, 0)))
doc <- run_chunk(doc, r"-(
tradeoff <- data.frame(t = seq(0.05, 0.95, by = 0.01)) |>
  rowwise() |>
  mutate(approval = mean(test$p_glm < t),
         bad_rate = mean(test$Status[test$p_glm < t] == "bad")) |>
  ungroup()
p_trade <- ggplot(tradeoff, aes(approval, bad_rate)) +
  geom_line(colour = "#1F4E79", linewidth = 1.1) +
  geom_hline(yintercept = mean(test$Status == "bad"), linetype = "dashed") +
  annotate("point", x = strat_tbl$Approval_rate[4], y = strat_tbl$Bad_rate_approved[4],
           colour = "#D1495B", size = 3.5) +
  annotate("text", x = strat_tbl$Approval_rate[4], y = strat_tbl$Bad_rate_approved[4] + 0.02,
           label = "cost-optimal policy", colour = "#D1495B", size = 3.3, hjust = 1) +
  annotate("text", x = 0.35, y = mean(test$Status == "bad") + 0.012, size = 3.2,
           label = "default rate if everyone is approved") +
  scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent) +
  labs(title = "Strategy curve: approval rate vs. default rate of the approved book",
       subtitle = "Test set; moving right approves more applicants",
       x = "Share of applicants approved", y = "Default rate among approved loans")
)-", show_output = FALSE)
doc <- add_plot(doc, p_trade, fig_path("week4", "02_strategy_curve.png"),
                fig_cap("Trade-off between approval rate and portfolio quality"), height = 3.8)
doc <- para(doc,
  "The strategy curve gives management a simple menu: each point is an achievable combination of ",
  "growth (approval rate) and risk (default rate of the approved portfolio). The business can choose ",
  "a point according to its risk appetite instead of relying on a single technical cut-off.")

doc <- h2(doc, "6.3 Sensitivity to the cost assumption")
doc <- run_chunk(doc, r"-(
sens <- do.call(rbind, lapply(c(2, 3, 5, 8, 10), function(r) {
  cc <- sapply(grid, function(t) r * sum(cv$bad < t & cv$obs == "bad") +
                                  sum(cv$bad >= t & cv$obs == "good"))
  t <- grid[which.min(cc)]
  data.frame(Cost_ratio = paste0(r, ":1"), Threshold = t,
             Approval_rate = round(100 * mean(test$p_glm < t), 1),
             Bad_rate_approved = round(100 * mean(test$Status[test$p_glm < t] == "bad"), 1))
}))
sens
)-")
doc <- para(doc,
  "As a missed default becomes more expensive, the optimal cut-off falls and the lender approves ",
  "fewer applicants with a cleaner portfolio. The model supports any of these policies; the choice ",
  "is a business decision about risk appetite, which should be agreed with the credit-risk and ",
  "finance teams.")

doc <- h2(doc, "6.4 Risk grades for operational use")
doc <- run_chunk(doc, r"-(
grades <- test |>
  mutate(Grade = cut(p_glm, breaks = c(0, 0.10, 0.20, 0.35, 0.50, 1),
                     labels = c("A", "B", "C", "D", "E"), include.lowest = TRUE)) |>
  group_by(Grade) |>
  summarise(Applicants = n(), Share = round(100 * n() / nrow(test), 1),
            Predicted_PD = round(100 * mean(p_glm), 1),
            Observed_default = round(100 * mean(Status == "bad"), 1)) |>
  mutate(Action = c("Auto-approve", "Approve", "Manual review", "Review + collateral / smaller loan",
                    "Decline")[as.integer(Grade)])
as.data.frame(grades)
)-")
doc <- add_table(doc, grades, tab_cap("Risk grades on the test set with suggested actions"))
gA <- grades[grades$Grade == "A", ]; gE <- grades[grades$Grade == "E", ]
doc <- para(doc, sprintf(paste(
  "The grades separate risk clearly: grade A applicants (%s%% of applicants) default only %s%% of",
  "the time, while grade E applicants (%s%%) default %s%% of the time. Predicted and observed default",
  "rates are close in every grade, which confirms the calibration. Grades A-B can be processed",
  "automatically, which saves underwriting time; analysts can then concentrate on grades C-D."),
  gA$Share, gA$Observed_default, gE$Share, gE$Observed_default))

# -----------------------------------------------------------------------------
doc <- h1(doc, "7. Discussion")
doc <- h2(doc, "7.1 Interpretation of results")
doc <- para(doc,
  "The three analytical stages point to the same conclusions. Descriptive analysis (Week 1), ",
  "visualization (Week 2), hypothesis testing and two different models (Week 3) all identify the same ",
  "drivers: credit history, employment stability, housing stability and affordability. When a linear ",
  "model and a non-linear random forest agree, and the formal tests confirm the visual patterns, we can ",
  "be confident the findings reflect real behaviour and not artefacts of one method.")
doc <- h2(doc, "7.2 Statistical versus practical significance")
doc <- para(doc,
  "With thousands of observations almost every difference becomes 'statistically significant'. This ",
  "report therefore always pairs p-values with effect sizes and business metrics. For example, age is ",
  "statistically significant in the model but its effect per year is small, whereas arrears records and ",
  "part-time employment multiply the odds of default several times over. Those are the variables ",
  "that should drive policy.")
doc <- h2(doc, "7.3 Business implications")
doc <- bullets(doc, c(
  "Underwriting policy: use the score to route applications into grades, automating the clear cases and focusing expert attention on the grey zone.",
  "Data capture: make arrears history, employment type and seniority mandatory, verified fields. Undeclared income should trigger a documentation request, because it is a strong risk signal in its own right.",
  "Product design: long-term, high loan-to-price loans carry more risk, so a minimum down-payment or risk-based pricing could be introduced for them.",
  "Customer fairness and transparency: the logistic model's odds ratios make each decision explainable. Variables such as marital status and age should be reviewed for fairness and regulatory compliance before deployment; marital status was not significant in the model and could be removed."))
doc <- h2(doc, "7.4 Limitations")
doc <- bullets(doc, c(
  "The data is historical and from one source; relationships may shift with the economy or the lender's customer mix.",
  "Only accepted applicants have observed outcomes (reject-inference bias), which is common to all credit-scoring data.",
  "The cost ratio is an assumption; real loss-given-default and margin figures should replace it.",
  sprintf("An AUC of %.2f is good but leaves room for improvement; richer data (bureau scores, transactions) would help most.", w3$auc_glm)))

# -----------------------------------------------------------------------------
doc <- h1(doc, "8. Conclusion: Lessons Learned, Challenges and Future Directions")
doc <- h2(doc, "8.1 Summary: the value of a data-driven approach")
doc <- para(doc, sprintf(paste(
  "Starting from a raw public dataset with missing values, duplicates and extreme outliers, this",
  "project produced a clean analytical dataset, a visual explanation of credit risk, statistically",
  "verified risk drivers, and a calibrated, interpretable credit score with a test-set AUC of %.3f.",
  "Turned into a cost-aware policy, the score reduces expected decision cost by about %s versus",
  "approving everyone. A data-driven approach replaces intuition with consistent, measurable and",
  "explainable decisions, and it makes the trade-off between growth and risk explicit, so it can be",
  "managed."), w3$auc_glm, pct(best$Cost_saving, 0)))
doc <- h2(doc, "8.2 Lessons learned")
doc <- bullets(doc, c(
  "Data cleaning is analysis: the decision to keep a missing-income flag, made during cleaning, produced one of the strongest predictors.",
  "Missing data should be investigated, not just filled; testing whether missingness relates to the outcome changed the strategy.",
  "Accuracy is misleading for imbalanced problems; ROC-AUC, sensitivity/specificity and business cost are far more informative.",
  "Thresholds are business decisions; the same model can support very different policies.",
  "Simple, interpretable models are often as good as complex ones on tabular data, and they are easier to trust and deploy.",
  "Generating reports from code (officer) guarantees that the numbers in the document match the analysis."))
doc <- h2(doc, "8.3 Challenges overcome")
challenges <- data.frame(
  Challenge = c("Missing income for 8.6% of applicants", "Heavy right-skew and extreme outliers",
                "Non-normal data", "Multicollinearity (Amount, Price, LoanToPrice)",
                "Class imbalance (28% bad)", "Sparse categories (e.g. 'ignore', 'divorced')",
                "Communicating to non-technical readers"),
  Solution = c("Group-median imputation + missing-indicator flag", "Winsorization at 1st/99th percentile; ratio features",
               "Non-parametric tests and Welch t-test", "VIF analysis; Price removed from model",
               "ROC-based tuning, Youden and cost-based thresholds", "Merged invalid level; kept small but valid levels",
               "Question-led chart titles, risk ladder, risk grades"),
  check.names = FALSE)
doc <- add_table(doc, challenges, tab_cap("Challenges encountered and how they were solved"))
doc <- h2(doc, "8.4 Recommendations for further analysis")
doc <- bullets(doc, c(
  "Test gradient boosting (xgboost) and penalised regression (glmnet) within the same cross-validation framework.",
  "Add spline terms for age and seniority and interaction terms such as Job x Home.",
  "Replace the assumed cost ratio with actual loss-given-default and profit-margin data, and optimise expected profit directly.",
  "Carry out a fairness audit across age and marital-status groups before any production use.",
  "Validate the model out-of-time on newer applications and set up monitoring for population drift (e.g. Population Stability Index).",
  "Deploy the score as an R Shiny app or a plumber API for loan officers."))

# -----------------------------------------------------------------------------
doc <- h1(doc, "Appendix A: Project Structure and Reproducibility")
doc <- add_code(doc, r"-(
data_analysis_R/
|-- run_all.R                        # runs weeks 1-4 in order
|-- R/
|   |-- 00_helpers.R                 # settings, plotting theme, Word helpers
|   |-- week1_data_cleaning.R        # cleaning + preliminary analysis
|   |-- week2_visualization.R        # 13 visualizations
|   |-- week3_modeling.R             # hypothesis tests + models
|   `-- week4_final_report.R         # this report
|-- data/raw/credit_data_raw.csv     # untouched source data
|-- data/processed/                  # cleaned + model-ready data
|-- outputs/figures/week1..week4/    # all charts (PNG, 200 dpi)
|-- outputs/models/                  # saved results used across weeks
`-- reports/                         # the four Word reports
)-")
doc <- para(doc, "To reproduce everything: open the project in RStudio and run source('run_all.R'). ",
                 "Code repository: ", GITHUB)
doc <- run_chunk(doc, r"-(
cat(R.version.string, "\n")
pk <- c("dplyr", "tidyr", "ggplot2", "lattice", "caret", "randomForest", "pROC",
        "officer", "flextable", "modeldata")
data.frame(Package = pk, Version = sapply(pk, function(p) as.character(packageVersion(p))),
           row.names = NULL)
)-")

doc <- h1(doc, "Appendix B: Annotated Key R Code")
doc <- para(doc, "Selected functions that are reused across the project:")
doc <- add_code(doc, r"-(
# Default rate with a 95% confidence interval for any grouping (Week 2)
rate_by <- function(data, ...) {
  data |> group_by(...) |>
    summarise(n = n(), bad_rate = mean(Default)) |>
    mutate(se = sqrt(bad_rate * (1 - bad_rate) / n),        # binomial standard error
           lo = pmax(0, bad_rate - 1.96 * se),              # lower 95% bound
           hi = pmin(1, bad_rate + 1.96 * se)) |> ungroup() # upper 95% bound
}

# Cap extreme values at chosen percentiles instead of deleting rows (Week 1)
winsorize <- function(x, probs = c(0.01, 0.99)) {
  q <- quantile(x, probs, na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

# Variance inflation factor without extra packages (Week 3)
vif_manual <- function(df) sapply(names(df), function(v) {
  r2 <- summary(lm(reformulate(setdiff(names(df), v), v), data = df))$r.squared
  1 / (1 - r2)                                              # VIF = 1 / (1 - R^2)
})

# Cramer's V effect size for a chi-square test (Week 3)
cramers_v <- function(tab) {
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))$statistic
  sqrt(chi / (sum(tab) * (min(dim(tab)) - 1)))
}
)-")

save_report(doc, "reports/Week4_Final_Comprehensive_Report.docx")
