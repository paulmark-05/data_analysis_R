# =============================================================================
# run_all.R - reproduces the whole project (all data, figures and 4 reports)
# Open data_analysis_R.Rproj in RStudio, then:  source("run_all.R")
# Total run time: roughly 5-10 minutes (the random forest in Week 3 is the slow part)
# =============================================================================

scripts <- c("R/week1_data_cleaning.R",
             "R/week2_visualization.R",
             "R/week3_modeling.R",
             "R/week4_final_report.R")

for (s in scripts) {
  message("\n==================== Running ", s, " ====================")
  source(s, echo = FALSE)
  rm(list = setdiff(ls(globalenv()), "scripts"), envir = globalenv())  # fresh start per week
}

message("\nAll done. Reports are in the 'reports' folder:")
print(list.files("reports", pattern = "\\.docx$"))
