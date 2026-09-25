# =============================================================================
# 00_helpers.R
# Shared settings + helper functions used by all four weekly scripts.
#
# The key idea: every R code snippet shown in the Word reports is *actually
# executed* by run_chunk(), and the console output shown under it is the real
# captured output. Code, output and charts in the DOCX therefore always match.
# =============================================================================

# ---- 0. Settings you may want to change -------------------------------------
AUTHOR  <- "Nayani Paul"
PROGRAM <- "Virtual R Data Analyst Internship | YuvaIntern"
GITHUB  <- "https://github.com/paulmark-05/data_analysis_R"

# ---- 1. Packages -------------------------------------------------------------
required_pkgs <- c("modeldata", "dplyr", "tidyr", "ggplot2", "scales",
                   "officer", "flextable", "caret", "pROC", "randomForest",
                   "e1071")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                      logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(officer)
  library(flextable)
})

# ---- 2. Project paths --------------------------------------------------------
if (!file.exists(file.path("R", "00_helpers.R"))) {
  stop("Please set the working directory to the project root folder ",
       "(open 'data_analysis_R.Rproj' in RStudio, or use setwd()).")
}
dirs <- c("data/raw", "data/processed", "outputs/figures", "outputs/models",
          "reports")
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

fig_path <- function(week, name) {
  d <- file.path("outputs", "figures", week)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.path(d, name)
}

options(width = 95, scipen = 6, digits = 4, dplyr.summarise.inform = FALSE)
set.seed(2026)

# ---- 3. Common plotting theme ------------------------------------------------
PAL <- c(good = "#2E86AB", bad = "#D1495B")        # colours for Status
theme_report <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(plot.title    = element_text(face = "bold", size = base_size + 2),
          plot.subtitle = element_text(colour = "grey35"),
          plot.caption  = element_text(colour = "grey50", size = base_size - 3),
          legend.position = "top",
          panel.grid.minor = element_blank())
}
theme_set(theme_report())

# ---- 4. Word-document helpers ------------------------------------------------
CODE_FONT <- fp_text(font.family = "Consolas", font.size = 8.5, color = "#1F1F1F")
OUT_FONT  <- fp_text(font.family = "Consolas", font.size = 8,   color = "#004D40")
BODY_FONT <- fp_text(font.family = "Calibri",  font.size = 11)

code_par_style <- fp_par(shading.color = "#F3F3F3",
                         border = fp_border(color = "#BDBDBD", width = 0.75),
                         padding = 5, line_spacing = 1)
out_par_style  <- fp_par(shading.color = "#EAF4F2",
                         border = fp_border(color = "#80CBC4", width = 0.75),
                         padding = 5, line_spacing = 1)

# Build one shaded paragraph that keeps line breaks and indentation
mono_block <- function(doc, lines, font, par_style) {
  lines <- gsub("\t", "    ", lines)
  runs <- list()
  for (i in seq_along(lines)) {
    runs[[length(runs) + 1]] <- ftext(ifelse(nzchar(lines[i]), lines[i], " "), font)
    if (i < length(lines)) runs[[length(runs) + 1]] <- run_linebreak()
  }
  body_add_fpar(doc, do.call(fpar, c(runs, list(fp_p = par_style))))
}

add_code <- function(doc, code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  while (length(lines) && !nzchar(trimws(lines[1]))) lines <- lines[-1]
  while (length(lines) && !nzchar(trimws(lines[length(lines)]))) lines <- lines[-length(lines)]
  indent <- min(nchar(lines[nzchar(trimws(lines))]) -
                nchar(trimws(lines[nzchar(trimws(lines))], "left")))
  if (is.finite(indent) && indent > 0) lines <- substring(lines, indent + 1)
  doc <- body_add_par(doc, "R code", style = "graphic title")
  mono_block(doc, lines, CODE_FONT, code_par_style)
}

add_output <- function(doc, out, max_lines = 45) {
  if (length(out) > max_lines) {
    out <- c(out[seq_len(max_lines)],
             sprintf("... [%d more lines of output truncated]", length(out) - max_lines))
  }
  doc <- body_add_par(doc, "Console output", style = "graphic title")
  mono_block(doc, out, OUT_FONT, out_par_style)
}

# Execute code (in the global environment), then write code + real output
run_chunk <- function(doc, code, show_code = TRUE, show_output = TRUE,
                      max_lines = 45) {
  exprs <- parse(text = code, keep.source = FALSE)
  out <- capture.output({
    for (e in exprs) {
      res <- withVisible(eval(e, envir = globalenv()))
      if (res$visible) print(res$value)
    }
  })
  if (length(out)) cat(out, sep = "\n")                 # echo to R console too
  if (show_code) doc <- add_code(doc, code)
  if (show_output && length(out)) doc <- add_output(doc, out, max_lines)
  doc
}

# Word's heading styles number themselves (1., 1.1, ...), so any manual
# numbering written in the scripts ("3.2 Title") is stripped here.
strip_num <- function(text) sub("^[0-9]+(\\.[0-9]+)*\\.?\\s+", "", text)
h1 <- function(doc, text) body_add_par(doc, strip_num(text), style = "heading 1")
h2 <- function(doc, text) body_add_par(doc, strip_num(text), style = "heading 2")
h3 <- function(doc, text) body_add_par(doc, strip_num(text), style = "heading 3")

para <- function(doc, ...) {
  body_add_fpar(doc, fpar(ftext(paste0(...), BODY_FONT),
                          fp_p = fp_par(text.align = "justify",
                                        padding.bottom = 4)))
}

# Paragraph that starts with a bold label, e.g. bold_para(doc, "Insight: ", "...")
bold_para <- function(doc, label, text) {
  body_add_fpar(doc, fpar(ftext(label, update(BODY_FONT, bold = TRUE)),
                          ftext(text, BODY_FONT),
                          fp_p = fp_par(text.align = "justify",
                                        padding.bottom = 4)))
}

bullets <- function(doc, items) {
  for (it in items) {
    doc <- body_add_fpar(doc, fpar(ftext("•  ", BODY_FONT),
                                   ftext(it, BODY_FONT),
                                   fp_p = fp_par(padding.left = 18,
                                                 padding.bottom = 2)))
  }
  doc
}

add_table <- function(doc, df, caption = NULL, digits = 2, font_size = 9) {
  df <- as.data.frame(df)
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], function(x) round(x, digits))
  ft <- flextable(df) |>
    theme_vanilla() |>
    fontsize(size = font_size, part = "all") |>
    font(fontname = "Calibri", part = "all") |>
    bg(part = "header", bg = "#1F4E79") |>
    color(part = "header", color = "white") |>
    bold(part = "header") |>
    align(align = "center", part = "header") |>
    padding(padding = 2, part = "all") |>
    set_table_properties(layout = "autofit", width = 1)
  if (!is.null(caption)) doc <- body_add_par(doc, caption, style = "table title")
  doc <- body_add_flextable(doc, ft)
  body_add_par(doc, "", style = "Normal")
}

# Save a ggplot and insert it with a caption
add_plot <- function(doc, plot, file, caption, width = 6.5, height = 4) {
  ggsave(file, plot, width = width, height = height, dpi = 200, bg = "white")
  doc <- body_add_img(doc, src = file, width = width, height = height,
                      style = "centered")
  body_add_par(doc, caption, style = "Image Caption")
}

# Save a base-R / lattice plot (plot_fun draws it) and insert it
add_base_plot <- function(doc, plot_fun, file, caption, width = 6.5, height = 4) {
  png(file, width = width, height = height, units = "in", res = 200)
  plot_fun()
  dev.off()
  doc <- body_add_img(doc, src = file, width = width, height = height,
                      style = "centered")
  body_add_par(doc, caption, style = "Image Caption")
}

# Insert an already-saved PNG (e.g. a figure from an earlier week), keeping
# its aspect ratio by reading the pixel size from the PNG header
add_image <- function(doc, file, caption, width = 6.5) {
  b <- readBin(file, "raw", 24)
  px <- function(i) sum(as.integer(b[i]) * 256^(3:0))
  height <- width * px(21:24) / px(17:20)
  doc <- body_add_img(doc, src = file, width = width, height = height, style = "centered")
  body_add_par(doc, caption, style = "Image Caption")
}

# Title page shared by all reports
title_page <- function(doc, title, subtitle, week_label) {
  big   <- fp_text(font.family = "Calibri", font.size = 26, bold = TRUE, color = "#1F4E79")
  mid   <- fp_text(font.family = "Calibri", font.size = 15, color = "#2E75B6")
  small <- fp_text(font.family = "Calibri", font.size = 11, color = "#404040")
  ctr   <- fp_par(text.align = "center", padding.bottom = 8)
  doc |>
    body_add_par("", style = "Normal") |>
    body_add_par("", style = "Normal") |>
    body_add_fpar(fpar(ftext(week_label, mid), fp_p = ctr)) |>
    body_add_fpar(fpar(ftext(title, big), fp_p = ctr)) |>
    body_add_fpar(fpar(ftext(subtitle, mid), fp_p = ctr)) |>
    body_add_par("", style = "Normal") |>
    body_add_fpar(fpar(ftext(paste("Prepared by:", AUTHOR), small), fp_p = ctr)) |>
    body_add_fpar(fpar(ftext(PROGRAM, small), fp_p = ctr)) |>
    body_add_fpar(fpar(ftext(paste("Dataset: Credit Scoring data (modeldata::credit_data,",
                                   "4,454 loan applicants)"), small), fp_p = ctr)) |>
    body_add_fpar(fpar(ftext(paste("Tools: R", getRversion(),
                                   "| dplyr, tidyr, ggplot2, caret, officer"), small),
                       fp_p = ctr)) |>
    body_add_fpar(fpar(ftext(paste("Code repository:", GITHUB), small), fp_p = ctr)) |>
    body_add_fpar(fpar(ftext(format(Sys.Date(), "%d %B %Y"), small), fp_p = ctr)) |>
    body_add_break()
}

contents_list <- function(doc, items) {
  doc <- body_add_fpar(doc, fpar(ftext("Contents", fp_text(font.family = "Calibri",
                                                           font.size = 16, bold = TRUE,
                                                           color = "#1F4E79")),
                                 fp_p = fp_par(padding.bottom = 8)))
  for (i in seq_along(items)) {
    doc <- body_add_fpar(doc, fpar(ftext(sprintf("%d.  %s", i, items[i]), BODY_FONT),
                                   fp_p = fp_par(padding.left = 10, padding.bottom = 3)))
  }
  body_add_break(doc)
}

save_report <- function(doc, file) {
  print(doc, target = file)
  message("Report written to: ", normalizePath(file))
}

# Automatic figure / table numbering (reset at the start of each report)
.counter <- new.env()
reset_numbering <- function() { .counter$fig <- 0; .counter$tab <- 0 }
fig_cap <- function(text) { .counter$fig <- .counter$fig + 1; sprintf("Figure %d: %s", .counter$fig, text) }
tab_cap <- function(text) { .counter$tab <- .counter$tab + 1; sprintf("Table %d: %s", .counter$tab, text) }
reset_numbering()

# Small statistics helpers
mode_value <- function(x) {
  ux <- na.omit(x)
  names(sort(table(ux), decreasing = TRUE))[1]
}
skewness <- function(x) {
  x <- na.omit(x); n <- length(x)
  (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^1.5
}
pct <- function(x, d = 1) paste0(formatC(100 * x, format = "f", digits = d), "%")
