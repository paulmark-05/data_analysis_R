# =============================================================================
# WEEK 2 - Data Visualization and Insight Communication using R
# Input  : data/processed/credit_clean.rds  (created by week1_data_cleaning.R)
# Output : reports/Week2_Data_Visualization_Report.docx
#          outputs/figures/week2/*.png
# Run    : source("R/week2_visualization.R")  from the project root
# =============================================================================

source("R/00_helpers.R")
reset_numbering()
if (!file.exists("data/processed/credit_clean.rds")) source("R/week1_data_cleaning.R")
suppressPackageStartupMessages(library(lattice))

doc <- read_docx()
doc <- title_page(doc,
  title      = "Data Visualization and Insight Communication using R",
  subtitle   = "What separates good borrowers from risky ones? A visual story",
  week_label = "WEEK 2 TASK")
doc <- contents_list(doc, c(
  "Introduction and Audience",
  "Data Overview and Key Variables",
  "Visualization Design Principles",
  "Who Are the Applicants? (Composition)",
  "Which Groups Are Riskier? (Comparisons)",
  "How Does Risk Change? (Trends)",
  "How Do Variables Interact? (Relationships)",
  "Anomalies and Unusual Patterns",
  "Summary Dashboard of Risk Drivers",
  "Conclusions and Recommendations",
  "Appendix: Chart Catalogue and Reproducibility"))

# -----------------------------------------------------------------------------
doc <- h1(doc, "1. Introduction and Audience")
doc <- para(doc,
  "Week 1 produced a clean, validated credit-scoring dataset. This week's goal is to turn that data ",
  "into pictures that a non-technical audience (a branch manager, a credit committee, or a product ",
  "owner) can understand in seconds. Each visualization answers one plain-language business ",
  "question, and every chart is followed by three short notes:")
doc <- bullets(doc, c(
  "Why this chart: why this chart type was the right choice for the question.",
  "What it shows: how to read the chart.",
  "Insight: the business takeaway, with the supporting numbers."))
doc <- para(doc,
  "All charts were produced with R: mainly ggplot2, plus one lattice chart and one base-R chart to ",
  "demonstrate the three major R plotting systems. The code for every chart is embedded directly above it.")

# -----------------------------------------------------------------------------
doc <- h1(doc, "2. Data Overview and Key Variables")
doc <- run_chunk(doc, r"-(
credit <- readRDS("data/processed/credit_clean.rds")
cat("Applicants:", nrow(credit), "| Variables:", ncol(credit), "\n")
cat("Overall default (bad) rate:", round(100 * mean(credit$Default), 1), "%\n")
glimpse(credit[, c("Status", "Age", "Seniority", "Job", "Home", "Records", "Income",
                   "Amount", "Price", "Time", "LoanToPrice", "PaymentBurden")])
)-")
key_vars <- data.frame(
  Variable = c("Status / Default", "Records", "Job", "Home", "Seniority", "Age",
               "Income", "Amount, Price", "LoanToPrice", "PaymentBurden", "Time"),
  Meaning = c("Loan outcome (bad = defaulted; Default = 1)",
              "Applicant has a history of arrears",
              "Employment type (fixed, freelance, part-time, others)",
              "Housing situation (owner, rent, parents, private, other)",
              "Years with current employer",
              "Age of applicant in years",
              "Monthly income (imputed and capped in Week 1)",
              "Loan requested and price of the goods financed",
              "Share of the purchase financed by the loan (Amount / Price)",
              "Monthly instalment as a share of income",
              "Loan term in months"))
doc <- add_table(doc, key_vars, tab_cap("Key variables used in the visualizations"))

# -----------------------------------------------------------------------------
doc <- h1(doc, "3. Visualization Design Principles")
doc <- bullets(doc, c(
  "One question per chart, stated in the chart title as a conclusion or question.",
  "Consistent colour code throughout: blue = good borrowers, red = bad borrowers / risk.",
  "Default rates (percentages) rather than raw counts when groups have different sizes.",
  "A dashed reference line for the overall default rate, so above-average risk is obvious.",
  "Direct labels on bars instead of forcing the reader to look up values on an axis.",
  "Chart types chosen by task: bars for comparison, histograms/violins for distribution, lines for trends across an ordered variable, scatter plots for relationships, heatmaps for two-way interactions."))
doc <- run_chunk(doc, r"-(
overall_bad <- mean(credit$Default)

# Helper: default rate + 95% confidence interval for any grouping
rate_by <- function(data, ...) {
  data |>
    group_by(...) |>
    summarise(n = n(), bad_rate = mean(Default)) |>
    mutate(se = sqrt(bad_rate * (1 - bad_rate) / n),
           lo = pmax(0, bad_rate - 1.96 * se),
           hi = pmin(1, bad_rate + 1.96 * se)) |>
    ungroup()
}
)-", show_output = FALSE)

# -----------------------------------------------------------------------------
doc <- h1(doc, "4. Who Are the Applicants? (Composition)")
doc <- h2(doc, "4.1 How many loans go bad?")
doc <- run_chunk(doc, r"-(
status_tbl <- credit |> count(Status) |> mutate(share = n / sum(n))
p1 <- ggplot(status_tbl, aes(x = Status, y = n, fill = Status)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = paste0(comma(n), "\n(", percent(share, 0.1), ")")),
            vjust = -0.2, size = 4) +
  scale_fill_manual(values = PAL) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Roughly 1 in 4 loans goes bad",
       subtitle = "Number of applicants by credit outcome",
       x = NULL, y = "Applicants") +
  theme(legend.position = "none")
status_tbl
)-")
doc <- add_plot(doc, p1, fig_path("week2", "01_class_balance.png"),
                fig_cap("Distribution of the credit outcome"), height = 3.6)
doc <- bold_para(doc, "Why this chart: ", "a simple bar chart is the clearest way to compare the size of two groups; labels give exact counts and percentages.")
doc <- bold_para(doc, "What it shows: ", "the number of good and bad loans in the portfolio.")
doc <- bold_para(doc, "Insight: ", sprintf(
  "%s of applicants (%s loans) defaulted. The classes are imbalanced, so in all later charts default *rates* are compared rather than counts, and the predictive model in Week 3 must be evaluated with metrics that respect this imbalance.",
  pct(overall_bad), comma(sum(credit$Default))))

doc <- h2(doc, "4.2 What does the typical applicant earn and borrow?")
doc <- run_chunk(doc, r"-(
p2 <- ggplot(credit, aes(x = Income, fill = Status)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.55,
                 position = "identity", colour = "white") +
  geom_density(aes(colour = Status), fill = NA, linewidth = 0.9) +
  geom_vline(data = credit |> group_by(Status) |> summarise(m = median(Income)),
             aes(xintercept = m, colour = Status), linetype = "dashed") +
  scale_fill_manual(values = PAL) + scale_colour_manual(values = PAL) +
  labs(title = "Bad borrowers are concentrated at lower incomes",
       subtitle = "Income distribution by outcome (dashed lines = medians)",
       x = "Monthly income", y = "Density")
credit |> group_by(Status) |> summarise(median_income = median(Income),
                                        mean_income = round(mean(Income), 1))
)-")
doc <- add_plot(doc, p2, fig_path("week2", "02_income_histogram.png"),
                fig_cap("Histogram and density of income for good and bad borrowers"))
inc_med <- tapply(credit$Income, credit$Status, median)
doc <- bold_para(doc, "Why this chart: ", "a histogram shows the full shape of a numeric distribution; overlaying the two groups with a density curve and median line makes the shift between them visible.")
doc <- bold_para(doc, "What it shows: ", "how income is distributed for good (blue) and bad (red) borrowers.")
doc <- bold_para(doc, "Insight: ", sprintf(
  "both distributions are right-skewed, but the bad-borrower curve peaks further left. The median income of bad borrowers is %s, compared with %s for good borrowers. Low income alone does not guarantee default, but it clearly shifts the odds.",
  comma(inc_med["bad"]), comma(inc_med["good"])))

# -----------------------------------------------------------------------------
doc <- h1(doc, "5. Which Groups Are Riskier? (Comparisons)")
doc <- h2(doc, "5.1 Employment type")
doc <- run_chunk(doc, r"-(
job_rate <- rate_by(credit, Job)
p3 <- ggplot(job_rate, aes(x = reorder(Job, bad_rate), y = bad_rate)) +
  geom_col(fill = "#D1495B", width = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15, colour = "grey30") +
  geom_hline(yintercept = overall_bad, linetype = "dashed") +
  geom_text(aes(label = paste0(percent(bad_rate, 0.1), "  (n=", n, ")")),
            hjust = -0.15, size = 3.5) +
  annotate("text", x = 0.6, y = overall_bad, label = "overall", hjust = -0.1,
           vjust = -0.5, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = percent, limits = c(0, 0.9)) +
  labs(title = "Part-time workers default three times as often as fixed employees",
       subtitle = "Default rate by employment type, with 95% confidence intervals",
       x = NULL, y = "Default rate")
job_rate |> mutate(across(bad_rate:hi, ~ round(.x, 3)))
)-")
doc <- add_plot(doc, p3, fig_path("week2", "03_badrate_job.png"),
                fig_cap("Default rate by employment type"), height = 3.4)
jr <- setNames(job_rate$bad_rate, job_rate$Job)
doc <- bold_para(doc, "Why this chart: ", "a sorted horizontal bar chart is the easiest way to rank categories; error bars show how certain each estimate is, and the dashed line marks the average.")
doc <- bold_para(doc, "What it shows: ", "the percentage of applicants in each employment type whose loan went bad.")
doc <- bold_para(doc, "Insight: ", sprintf(
  "part-time workers default at %s and freelancers at %s, against %s for fixed-contract employees. The confidence intervals do not overlap, so the difference is real and not just noise. Income stability is a major risk driver.",
  pct(jr["partime"]), pct(jr["freelance"]), pct(jr["fixed"])))

doc <- h2(doc, "5.2 Housing situation")
doc <- run_chunk(doc, r"-(
home_mix <- credit |>
  count(Home, Status) |>
  group_by(Home) |>
  mutate(share = n / sum(n), total = sum(n)) |>
  ungroup() |>
  mutate(Home = reorder(Home, ifelse(Status == "bad", share, 0), FUN = max))
p4 <- ggplot(home_mix, aes(x = Home, y = share, fill = Status)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = percent(share, 1)), position = position_stack(vjust = 0.5),
            colour = "white", size = 3.5, fontface = "bold") +
  geom_text(data = distinct(home_mix, Home, total), inherit.aes = FALSE,
            aes(x = Home, y = 1.04, label = paste0("n=", total)), size = 3) +
  coord_flip() +
  scale_fill_manual(values = PAL) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.25), limits = c(0, 1.1)) +
  labs(title = "Home owners are the safest borrowers",
       subtitle = "Share of good and bad loans within each housing group (100% stacked)",
       x = NULL, y = NULL)
)-", show_output = FALSE)
doc <- add_plot(doc, p4, fig_path("week2", "04_home_stacked.png"),
                fig_cap("Outcome mix within each housing category"), height = 3.4)
hr <- rate_by(credit, Home); hr <- setNames(hr$bad_rate, hr$Home)
doc <- bold_para(doc, "Why this chart: ", "a 100% stacked bar shows the good/bad mix inside each group regardless of group size, which suits part-to-whole comparisons.")
doc <- bold_para(doc, "What it shows: ", "for each housing situation, the share of loans that were repaid (blue) or went bad (red).")
doc <- bold_para(doc, "Insight: ", sprintf(
  "only %s of home owners defaulted, compared with %s of renters and %s of the 'other' group. Owning a home signals financial stability and provides collateral; renters and applicants with non-standard housing need closer review.",
  pct(hr["owner"]), pct(hr["rent"]), pct(hr["other"])))

doc <- h2(doc, "5.3 Loan size by outcome")
doc <- run_chunk(doc, r"-(
p5 <- ggplot(credit, aes(x = Status, y = Amount, fill = Status)) +
  geom_violin(alpha = 0.35, colour = NA) +
  geom_boxplot(width = 0.18, outlier.alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  scale_fill_manual(values = PAL) +
  scale_y_continuous(labels = comma) +
  labs(title = "Bad loans tend to be larger",
       subtitle = "Loan amount by outcome (violin = full distribution, diamond = mean)",
       x = NULL, y = "Loan amount") +
  theme(legend.position = "none")
credit |> group_by(Status) |> summarise(median_amount = median(Amount),
                                        mean_amount = round(mean(Amount)))
)-")
doc <- add_plot(doc, p5, fig_path("week2", "05_amount_violin.png"),
                fig_cap("Distribution of loan amount by outcome"), height = 3.8)
am <- tapply(credit$Amount, credit$Status, mean)
doc <- bold_para(doc, "Why this chart: ", "a violin plot combined with a boxplot shows both the full shape of the distribution and its summary statistics (median, quartiles, outliers).")
doc <- bold_para(doc, "What it shows: ", "the range of loan amounts requested by good and bad borrowers.")
doc <- bold_para(doc, "Insight: ", sprintf(
  "the average bad loan (%s) is about %s larger than the average good loan (%s). Larger loans, especially relative to the applicant's means, carry more risk.",
  comma(round(am["bad"])), pct(am["bad"] / am["good"] - 1, 0), comma(round(am["good"]))))

doc <- h2(doc, "5.4 Past payment records (base R graphics)")
doc <- run_chunk(doc, r"-(
records_tab <- table(Records = credit$Records, Status = credit$Status)
records_tab
round(prop.table(records_tab, 1), 3)
)-")
doc <- add_code(doc, r"-(
# Base-R mosaic plot: tile width = group size, tile height = outcome share
mosaicplot(records_tab, color = c("#D1495B", "#2E86AB"), border = "white",
           main = "Applicants with past arrears default more than twice as often",
           xlab = "Previous arrears record", ylab = "Credit outcome", cex.axis = 1)
)-")
doc <- add_base_plot(doc, function() {
  mosaicplot(records_tab, color = c("#D1495B", "#2E86AB"), border = "white",
             main = "Applicants with past arrears default more than twice as often",
             xlab = "Previous arrears record", ylab = "Credit outcome", cex.axis = 1)
}, fig_path("week2", "06_records_mosaic.png"),
fig_cap("Mosaic plot of previous arrears record vs. credit outcome (base R)"), height = 4)
rr <- prop.table(records_tab, 1)[, "bad"]
doc <- bold_para(doc, "Why this chart: ", "a mosaic plot shows two categorical variables at once: tile widths show how common each group is and tile heights show the outcome mix, so both size and risk are visible.")
doc <- bold_para(doc, "What it shows: ", "the narrow right column is the minority with arrears records; its large red area is their high default share.")
doc <- bold_para(doc, "Insight: ", sprintf(
  "%s of applicants with a past arrears record defaulted again, versus %s of those without. Credit history is the single strongest warning signal in the data.",
  pct(rr["yes"]), pct(rr["no"])))

# -----------------------------------------------------------------------------
doc <- h1(doc, "6. How Does Risk Change? (Trends)")
doc <- para(doc, "Line charts are used for ordered variables (age, years of employment, loan term) ",
                 "so that the reader can follow the direction of the trend.")
doc <- h2(doc, "6.1 Age and job seniority")
doc <- run_chunk(doc, r"-(
age_rate <- credit |>
  mutate(Age5 = pmin(5 * floor(Age / 5), 65)) |>
  rate_by(Age5) |> mutate(Driver = "Age (5-year bands)", x = Age5)
sen_rate <- credit |>
  mutate(Sen = pmin(Seniority, 25)) |>
  rate_by(Sen) |> mutate(Driver = "Seniority (years, 25 = 25+)", x = Sen)

p6 <- bind_rows(age_rate, sen_rate) |>
  ggplot(aes(x = x, y = bad_rate)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#D1495B", alpha = 0.15) +
  geom_line(colour = "#D1495B", linewidth = 1) +
  geom_point(aes(size = n), colour = "#D1495B") +
  geom_hline(yintercept = overall_bad, linetype = "dashed") +
  facet_wrap(~ Driver, scales = "free_x") +
  scale_y_continuous(labels = percent) +
  scale_size_continuous(range = c(1, 3.5), name = "Applicants") +
  labs(title = "Risk falls steadily with job seniority, and more gently with age",
       subtitle = "Default rate with 95% confidence band; dashed line = overall rate",
       x = NULL, y = "Default rate")
)-", show_output = FALSE)
doc <- add_plot(doc, p6, fig_path("week2", "07_trend_age_seniority.png"),
                fig_cap("Default-rate trend by age and by job seniority"), height = 4)
s0 <- sen_rate$bad_rate[sen_rate$x == 0]; s10 <- mean(sen_rate$bad_rate[sen_rate$x >= 10])
doc <- bold_para(doc, "Why this chart: ", "line charts show the direction and steepness of a trend across ordered values; the shaded band shows uncertainty, which widens where there are few applicants (small points).")
doc <- bold_para(doc, "What it shows: ", "the default rate for each age band (left) and each year of seniority with the current employer (right).")
doc <- bold_para(doc, "Insight: ", sprintf(
  "applicants in their first year with an employer default at %s, while those with 10 or more years default at about %s on average. Job stability is a strong protective factor. The age effect goes in the same direction but is weaker: younger applicants are riskier, partly because they also have less seniority.",
  pct(s0), pct(s10)))

doc <- h2(doc, "6.2 Loan term")
doc <- run_chunk(doc, r"-(
term_rate <- credit |>
  mutate(Term = cut(Time, breaks = c(0, 12, 24, 36, 48, 72),
                    labels = c("<=12", "13-24", "25-36", "37-48", "49-72"))) |>
  rate_by(Term)
p7 <- ggplot(term_rate, aes(x = Term, y = bad_rate, group = 1)) +
  geom_line(colour = "#1F4E79", linewidth = 1) +
  geom_point(aes(size = n), colour = "#1F4E79") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.1, colour = "#1F4E79", alpha = 0.6) +
  geom_text(aes(label = percent(bad_rate, 1)), vjust = -1.3, size = 3.4) +
  geom_hline(yintercept = overall_bad, linetype = "dashed") +
  scale_y_continuous(labels = percent, limits = c(0, 0.5)) +
  scale_size_continuous(range = c(1.5, 5), name = "Applicants") +
  labs(title = "Longer loans are riskier",
       subtitle = "Default rate by loan term (months)", x = "Loan term (months)",
       y = "Default rate")
term_rate |> select(Term, n, bad_rate) |> mutate(bad_rate = round(bad_rate, 3))
)-")
doc <- add_plot(doc, p7, fig_path("week2", "08_trend_term.png"),
                fig_cap("Default rate by loan term"), height = 3.6)
tr <- setNames(term_rate$bad_rate, term_rate$Term)
doc <- bold_para(doc, "Why this chart: ", "loan term is ordered, so a line connecting the groups shows the trend; point size shows how many applicants each estimate is based on.")
doc <- bold_para(doc, "What it shows: ", "the default rate for each band of loan duration.")
doc <- bold_para(doc, "Insight: ", sprintf(
  "the default rate rises from %s for short loans (up to 12 months) to %s for the longest terms (49-72 months). A longer term means more time for the borrower's circumstances to change, so long-term products may justify stricter approval rules or risk-based pricing.",
  pct(tr["<=12"]), pct(tr["49-72"])))

# -----------------------------------------------------------------------------
doc <- h1(doc, "7. How Do Variables Interact? (Relationships)")
doc <- h2(doc, "7.1 Loan amount versus price of the goods")
doc <- run_chunk(doc, r"-(
p8 <- ggplot(credit, aes(x = Price, y = Amount, colour = Status)) +
  geom_point(alpha = 0.35, size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1, formula = y ~ x) +
  annotate("text", x = 3300, y = 3500, label = "Amount = Price\n(100% financed)",
           size = 3, hjust = 1) +
  scale_colour_manual(values = PAL) +
  scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
  labs(title = "Bad borrowers finance a bigger share of what they buy",
       subtitle = "Each dot is an applicant; lines are linear trends per outcome",
       x = "Price of goods", y = "Loan amount")
cor.test(credit$Amount, credit$Price)$estimate
)-")
doc <- add_plot(doc, p8, fig_path("week2", "09_scatter_amount_price.png"),
                fig_cap("Scatter plot of loan amount against price of goods"), height = 4.2)
ltp <- tapply(credit$LoanToPrice, credit$Status, mean)
doc <- bold_para(doc, "Why this chart: ", "a scatter plot is the standard way to show the relationship between two numeric variables; colour adds the outcome as a third dimension.")
doc <- bold_para(doc, "What it shows: ", "each applicant's loan amount against the price of the goods; the dotted diagonal is 100% financing.")
doc <- bold_para(doc, "Insight: ", sprintf(
  "amount and price are strongly correlated (r = %.2f), as expected. However, the red (bad) trend line sits above the blue one: bad borrowers finance on average %s of the purchase versus %s for good borrowers. A small down-payment (high loan-to-price) is a risk signal worth adding to the approval checklist.",
  cor(credit$Amount, credit$Price), pct(ltp["bad"], 0), pct(ltp["good"], 0)))

doc <- h2(doc, "7.2 Interaction of employment and housing (heatmap)")
doc <- run_chunk(doc, r"-(
heat <- rate_by(credit, Job, Home)
p9 <- ggplot(heat, aes(x = Home, y = Job, fill = bad_rate)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = paste0(percent(bad_rate, 1), "\nn=", n)), size = 3.1) +
  scale_fill_gradient(low = "#F7FBFF", high = "#D1495B", labels = percent,
                      name = "Default rate") +
  labs(title = "Risk compounds across employment and housing",
       subtitle = "Default rate for each combination of employment type and housing",
       x = "Housing", y = "Employment") +
  theme(legend.position = "right", panel.grid = element_blank())
heat |> arrange(desc(bad_rate)) |> filter(n >= 30) |> select(Job, Home, n, bad_rate) |>
  mutate(bad_rate = round(bad_rate, 3)) |> head(5)
)-")
doc <- add_plot(doc, p9, fig_path("week2", "10_heatmap_job_home.png"),
                fig_cap("Heatmap of default rate by employment type and housing"), height = 4)
top_seg <- heat |> filter(n >= 30) |> arrange(desc(bad_rate)) |> slice(1)
low_seg <- heat |> filter(n >= 30) |> arrange(bad_rate) |> slice(1)
doc <- bold_para(doc, "Why this chart: ", "a heatmap displays a two-way table so that hot spots stand out by colour; the counts in each tile warn the reader when a cell is based on few applicants.")
doc <- bold_para(doc, "What it shows: ", "the default rate for every employment/housing combination.")
doc <- bold_para(doc, "Insight: ", sprintf(
  "risk factors add up. Among segments with at least 30 applicants, the riskiest is %s + %s (%s default rate, n = %d) and the safest is %s + %s (%s, n = %d). Segment-level rules can therefore be much sharper than rules based on a single variable.",
  top_seg$Job, top_seg$Home, pct(top_seg$bad_rate), top_seg$n,
  low_seg$Job, low_seg$Home, pct(low_seg$bad_rate), low_seg$n))

doc <- h2(doc, "7.3 Loan-to-price by employment type (lattice)")
doc <- add_code(doc, r"-(
# lattice: conditioned density plots, one panel per employment type
p10 <- densityplot(~ LoanToPrice | Job, data = credit, groups = Status,
                   plot.points = FALSE, auto.key = list(columns = 2),
                   par.settings = simpleTheme(col = c("#D1495B", "#2E86AB"), lwd = 2),
                   layout = c(4, 1), xlab = "Loan-to-price ratio",
                   main = "Bad borrowers finance a larger share in every job type")
print(p10)
)-")
p10 <- densityplot(~ LoanToPrice | Job, data = credit, groups = Status,
                   plot.points = FALSE, auto.key = list(columns = 2),
                   par.settings = simpleTheme(col = c("#D1495B", "#2E86AB"), lwd = 2),
                   layout = c(4, 1), xlab = "Loan-to-price ratio",
                   main = "Bad borrowers finance a larger share in every job type")
doc <- add_base_plot(doc, function() print(p10), fig_path("week2", "11_lattice_ltp_job.png"),
                     fig_cap("Density of loan-to-price ratio by outcome within each job type (lattice)"),
                     height = 3.4)
doc <- bold_para(doc, "Why this chart: ", "lattice's conditioning operator ( | ) makes small multiples easy: the same chart repeated for each employment type, so we can check whether a pattern holds in every subgroup.")
doc <- bold_para(doc, "What it shows: ", "the distribution of the loan-to-price ratio for good (blue) and bad (red) borrowers, separately for each job type.")
doc <- bold_para(doc, "Insight: ", "in every panel the red curve is shifted to the right, towards financing a larger share of the purchase. The loan-to-price effect is therefore not just a side-effect of job type: it is an independent risk signal, which makes it a good candidate predictor for the Week 3 model.")

# -----------------------------------------------------------------------------
doc <- h1(doc, "8. Anomalies and Unusual Patterns")
doc <- run_chunk(doc, r"-(
credit <- credit |>
  mutate(Anomaly = case_when(PaymentBurden > 1 ~ "Instalment > income",
                             Income_missing == "yes" ~ "Income not declared",
                             TRUE ~ "Regular"))
anom_tbl <- rate_by(credit, Anomaly)
anom_tbl |> select(Anomaly, n, bad_rate) |> mutate(bad_rate = round(bad_rate, 3))

p11 <- ggplot(credit, aes(x = Income, y = PaymentBurden)) +
  geom_point(aes(colour = Anomaly, shape = Status), alpha = 0.6, size = 1.6) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey30") +
  annotate("text", x = max(credit$Income), y = 1.08, label = "instalment = 100% of income", size = 3, hjust = 1) +
  scale_colour_manual(values = c("Instalment > income" = "#D1495B",
                                 "Income not declared" = "#F4A259",
                                 "Regular" = "grey70")) +
  scale_shape_manual(values = c(bad = 4, good = 16)) +
  scale_y_continuous(labels = percent) +
  labs(title = "Anomalies: impossible burdens and undeclared income",
       subtitle = "Monthly instalment as % of income; crosses = bad loans",
       x = "Monthly income", y = "Payment burden") +
  guides(colour = guide_legend(nrow = 1), shape = guide_legend(nrow = 1)) +
  theme(legend.box = "vertical")
)-")
doc <- add_plot(doc, p11, fig_path("week2", "12_anomalies.png"),
                fig_cap("Payment burden against income, with anomalous applicants highlighted"),
                height = 4.4)
an <- setNames(anom_tbl$bad_rate, anom_tbl$Anomaly); ann <- setNames(anom_tbl$n, anom_tbl$Anomaly)
doc <- bold_para(doc, "Why this chart: ", "a scatter plot with highlighted points makes a small number of unusual cases stand out against the bulk of normal ones.")
doc <- bold_para(doc, "What it shows: ", "the payment burden (instalment divided by income) for every applicant, with two anomalous groups coloured.")
doc <- bold_para(doc, "Insight: ", sprintf(
  "%d applicants have a monthly instalment larger than their entire income, which is economically implausible and default at %s. %d applicants did not declare an income; they default at %s, compared with %s for regular applicants. Both groups should trigger a manual review or a request for documentation rather than automatic approval.",
  ann["Instalment > income"], pct(an["Instalment > income"]),
  ann["Income not declared"], pct(an["Income not declared"]), pct(an["Regular"])))

# -----------------------------------------------------------------------------
doc <- h1(doc, "9. Summary Dashboard of Risk Drivers")
doc <- run_chunk(doc, r"-(
seg <- bind_rows(
  rate_by(credit, Level = Records)   |> mutate(Factor = "Past arrears"),
  rate_by(credit, Level = Job)       |> mutate(Factor = "Employment"),
  rate_by(credit, Level = Home)      |> mutate(Factor = "Housing"),
  rate_by(credit, Level = HasAssets) |> mutate(Factor = "Owns assets"),
  rate_by(credit, Level = AgeGroup)  |> mutate(Factor = "Age group"),
  rate_by(credit, Level = Income_missing) |> mutate(Factor = "Income missing")) |>
  mutate(Level = as.character(Level), lift = bad_rate - overall_bad)

p12 <- ggplot(seg, aes(x = lift, y = reorder(paste(Factor, "|", Level), lift))) +
  geom_segment(aes(x = 0, xend = lift, yend = reorder(paste(Factor, "|", Level), lift),
                   colour = lift > 0), linewidth = 1.1) +
  geom_point(aes(colour = lift > 0, size = n)) +
  geom_vline(xintercept = 0) +
  scale_colour_manual(values = c(`TRUE` = "#D1495B", `FALSE` = "#2E86AB"),
                      labels = c("Lower risk", "Higher risk"), name = NULL) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), round(100 * x), " pts")) +
  scale_size_continuous(range = c(1.5, 5), guide = "none") +
  labs(title = "Risk ladder: each group vs. the average applicant",
       subtitle = paste0("Difference from the overall default rate of ", percent(overall_bad, 0.1)),
       x = "Percentage-point difference in default rate", y = NULL)
)-", show_output = FALSE)
doc <- add_plot(doc, p12, fig_path("week2", "13_risk_ladder.png"),
                fig_cap("Risk ladder: default-rate difference from the portfolio average by segment"),
                height = 6)
doc <- bold_para(doc, "Why this chart: ", "a lollipop (dot-and-line) chart sorted by effect size is an executive-friendly summary: it ranks all segments on one common scale, with direction shown by colour.")
doc <- bold_para(doc, "What it shows: ", "how many percentage points above (red) or below (blue) the overall default rate each group sits.")
doc <- bold_para(doc, "Insight: ", sprintf(
  "the biggest risk increases come from part-time employment (+%.0f pts), undeclared income (+%.0f pts) and past arrears (+%.0f pts). The strongest protective factors are home ownership (%.0f pts) and a fixed job (%.0f pts). This ranking gives the credit team a clear order of priority for its checks.",
  100 * seg$lift[seg$Level == "partime"], 100 * seg$lift[seg$Factor == "Income missing" & seg$Level == "yes"],
  100 * seg$lift[seg$Factor == "Past arrears" & seg$Level == "yes"],
  100 * seg$lift[seg$Level == "owner"], 100 * seg$lift[seg$Level == "fixed"]))

# -----------------------------------------------------------------------------
doc <- h1(doc, "10. Conclusions and Recommendations")
doc <- para(doc, "The visual analysis tells a consistent story. Default risk is driven by three themes:")
doc <- bullets(doc, c(
  sprintf("Track record: a history of arrears is the strongest single warning sign (%s vs %s default rate).", pct(rr["yes"]), pct(rr["no"])),
  sprintf("Stability: part-time or freelance work, short job seniority, renting and younger age all raise risk; home ownership and long seniority lower it (first-year employees %s vs %s for 10+ years).", pct(s0), pct(s10)),
  sprintf("Affordability: lower income, larger loans, longer terms and a high loan-to-price ratio all increase risk (bad borrowers finance %s of the purchase vs %s).", pct(ltp["bad"], 0), pct(ltp["good"], 0))))
doc <- para(doc, "Recommendations for the business:")
doc <- bullets(doc, c(
  "Make past-arrears status and employment type mandatory, verified fields in the application form.",
  "Introduce a manual-review flag for applicants with undeclared income or an instalment above a set share of income.",
  "Consider a minimum down-payment (maximum loan-to-price ratio) for long-term loans.",
  "Use these drivers as candidate predictors in a statistical scoring model (Week 3), and test which of them remain significant when considered together."))

doc <- h1(doc, "11. Appendix: Chart Catalogue and Reproducibility")
catalogue <- data.frame(
  Figure = 1:13,
  `Chart type` = c("Bar chart", "Histogram + density", "Horizontal bar + error bars",
                   "100% stacked bar", "Violin + boxplot", "Mosaic plot (base R)",
                   "Line chart with CI band", "Line chart", "Scatter plot + trend lines",
                   "Heatmap", "Conditioned density (lattice)", "Highlighted scatter",
                   "Lollipop / risk ladder"),
  Question = c("How many loans go bad?", "How does income differ?", "Which job types are riskier?",
               "Which housing groups are riskier?", "Are bad loans larger?",
               "Does credit history matter?", "How does risk change with age/seniority?",
               "How does risk change with loan term?", "Do bad borrowers finance more?",
               "Do risk factors combine?", "Does loan-to-price matter in every job group?",
               "Which applicants look anomalous?", "What are the main risk drivers overall?"),
  check.names = FALSE)
doc <- add_table(doc, catalogue, tab_cap("Catalogue of visualizations"))
doc <- para(doc, "The script R/week2_visualization.R regenerates all figures (outputs/figures/week2/) and this document.")

save_report(doc, "reports/Week2_Data_Visualization_Report.docx")
