
# --------- Helper: install/load packages ----------
packages <- c("readr","dplyr","tidyr","ggplot2","janitor","skimr",
              "ggpubr","scales","corrplot","factoextra","cluster","psych")
new_pkgs <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs, repos = "https://cloud.r-project.org")
lapply(packages, library, character.only = TRUE)
setwd("C:\\Users\\LATA MALIK\\OneDrive\\Desktop\\ANSHUMAN BISWAS\\November\\26-11-2025")
# --------- 0. Setup: file path and output folders ----------
input_file <- "Shopping_CustomerData.csv"   # change if needed
out_dir <- "analysis_outputs"
if(!dir.exists(out_dir)) dir.create(out_dir)

# --------- 1. Import dataset (safe read) ----------
df <- read_csv(input_file, show_col_types = FALSE)  # from readr

# Quick peek
cat("Dataset dimensions:", dim(df), "\n")
print(colnames(df))
print(names(df))

# --------- 2. Clean column names (useable names) ----------
# Make names consistent: lower_case and snake_case
df <- df %>% 
  janitor::clean_names()  # e.g. "CustomerID" -> "customer_id"

# If certain common names exist, rename to standardized names:
# (keeps script robust across slight variations in CSV column names)
colmap <- c("customerid"="customer_id",
            "customer_id"="customer_id",
            "age"="age",
            "gender"="gender",
            "annualincome"="annual_income",
            "annual_income"="annual_income",
            "spendingscore"="spending_score",
            "spending_score"="spending_score")
for(orig in intersect(names(df), names(colmap))) {
  names(df)[names(df) == orig] <- colmap[[orig]]
}

# Show cleaned names
cat("Clean column names:\n")
print(colnames(df))

# --------- 3. Type conversions & new derived columns ----------
# Convert plausible numeric columns to numeric (safely)
safe_num <- function(x) suppressWarnings(as.numeric(x))
maybe_numeric_cols <- c("age","annual_income","spending_score","annualincome","spendingscore")
for(col in intersect(maybe_numeric_cols, names(df))) {
  df[[col]] <- safe_num(df[[col]])
}

# Standardize gender values (if present)
if("gender" %in% names(df)) {
  df$gender <- as.character(df$gender)
  df$gender <- trimws(tolower(df$gender))
  df$gender <- ifelse(df$gender %in% c("m","male"), "Male",
                      ifelse(df$gender %in% c("f","female"), "Female", df$gender))
  df$gender <- factor(df$gender)
}

# Create categorical buckets where useful: income_group and spending_group
if("annual_income" %in% names(df)) {
  df <- df %>% mutate(
    income_group = cut(annual_income,
                       breaks = c(-Inf, 30, 60, 90, Inf),
                       labels = c("Low","Lower-Mid","Upper-Mid","High"))
  )
} else {
  # fallback if column name differs
  if("annualincome" %in% names(df)) {
    df <- df %>% rename(annual_income = annualincome)
    df <- df %>% mutate(
      income_group = cut(annual_income,
                         breaks = c(-Inf, 30, 60, 90, Inf),
                         labels = c("Low","Lower-Mid","Upper-Mid","High"))
    )
  }
}
if("spending_score" %in% names(df)) {
  df <- df %>% mutate(
    spending_group = cut(spending_score,
                         breaks = c(-Inf, 25, 50, 75, Inf),
                         labels = c("Low","Medium","High","Very High"))
  )
}

# --------- 4. Basic summaries (like Practical 1 & 2) ----------
sink(file = file.path(out_dir, "summary.txt"))
cat("=== Dataset summary ===\n")
print(dim(df))
print(skimr::skim(df))
cat("\n=== Column names ===\n")
print(colnames(df))
sink()

# Also save a CSV of cleaned data
write_csv(df, file.path(out_dir, "shopping_cleaned.csv"))

# --------- 5. Handling missing values and NA-aware summaries ----------
# Show counts of NA by column
na_counts <- sapply(df, function(x) sum(is.na(x)))
write.csv(as.data.frame(na_counts), file.path(out_dir, "na_counts.csv"), row.names = TRUE)

# When computing means, always use na.rm = TRUE
# Example: mean age, income, spending
stats_basic <- data.frame(
  var = c("age","annual_income","spending_score"),
  mean = c(
    ifelse("age" %in% names(df), mean(df$age, na.rm = TRUE), NA),
    ifelse("annual_income" %in% names(df), mean(df$annual_income, na.rm = TRUE), NA),
    ifelse("spending_score" %in% names(df), mean(df$spending_score, na.rm = TRUE), NA)
  ),
  sd = c(
    ifelse("age" %in% names(df), sd(df$age, na.rm = TRUE), NA),
    ifelse("annual_income" %in% names(df), sd(df$annual_income, na.rm = TRUE), NA),
    ifelse("spending_score" %in% names(df), sd(df$spending_score, na.rm = TRUE), NA)
  )
)
write.csv(stats_basic, file.path(out_dir, "basic_stats.csv"), row.names = FALSE)

# --------- 6. Outlier handling (as per Practical 2 Advice) ----------
# Example rule: remove unrealistic ages or incomes if present
# (adapt thresholds as realistic for the dataset)
df_filtered <- df
if("age" %in% names(df)) {
  # define plausible range for age: 15 - 95
  df_filtered <- df_filtered %>% filter(is.na(age) | (age >= 15 & age <= 95))
}
if("annual_income" %in% names(df_filtered)) {
  # drop ridiculously extreme incomes (e.g. > 1 million) - adjust for your data
  df_filtered <- df_filtered %>% filter(is.na(annual_income) | annual_income <= 1000000)
}

# Save filtered dataset separately
write_csv(df_filtered, file.path(out_dir, "shopping_filtered.csv"))

# --------- 7. Frequency tables and cross-tabs (Practical 2 style) ----------
# Gender counts
if("gender" %in% names(df_filtered)) {
  gender_table <- table(df_filtered$gender)
  write.csv(as.data.frame(gender_table), file.path(out_dir, "gender_table.csv"), row.names = FALSE)
  print(gender_table)
}

# Income group x spending group cross-tabulation and proportions
if(all(c("income_group","spending_group") %in% names(df_filtered))) {
  ct <- table(df_filtered$income_group, df_filtered$spending_group)
  write.csv(as.data.frame.matrix(ct), file.path(out_dir, "income_spending_crosstab_counts.csv"))
  # prop.table across rows (margin = 1) like practical
  ct_prop_row <- prop.table(ct, margin = 1)
  write.csv(as.data.frame.matrix(ct_prop_row), file.path(out_dir, "income_spending_crosstab_props_by_income.csv"))
}

# Example: is part-time job desire similar across genders? (if column exists)
if("ptjobs" %in% names(df_filtered)) {
  pt_tab <- table(df_filtered$gender, df_filtered$ptjobs)
  write.csv(as.data.frame(pt_tab), file.path(out_dir, "ptjobs_by_gender.csv"))
}

# --------- 8. Visualizations (ggplot2) ----------
# Histogram: Age
if("age" %in% names(df_filtered)) {
  p_age <- ggplot(df_filtered, aes(x = age)) + 
    geom_histogram(binwidth = 5, boundary = 0, closed = "left") +
    labs(title = "Age distribution", x = "Age", y = "Count")
  ggsave(filename = file.path(out_dir, "age_histogram.png"), plot = p_age, width = 6, height = 4)
}

# Boxplots: Annual income by gender
if(all(c("annual_income","gender") %in% names(df_filtered))) {
  p_box_income <- ggplot(df_filtered, aes(x = gender, y = annual_income)) +
    geom_boxplot() +
    labs(title = "Annual Income by Gender", y = "Annual Income")
  ggsave(file.path(out_dir, "box_income_by_gender.png"), p_box_income, width = 6, height = 4)
}

# Scatter: Annual income vs spending score (with smoothing)
if(all(c("annual_income","spending_score") %in% names(df_filtered))) {
  p_scatter <- ggplot(df_filtered, aes(x = annual_income, y = spending_score)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess", se = TRUE) +
    labs(title = "Annual Income vs Spending Score", x = "Annual Income (k)", y = "Spending Score")
  ggsave(file.path(out_dir, "income_vs_spending.png"), p_scatter, width = 6, height = 4)
}

# Bar: counts by income_group
if("income_group" %in% names(df_filtered)) {
  p_inc_bar <- ggplot(df_filtered, aes(x = income_group)) +
    geom_bar() + labs(title = "Count by Income Group", x = "Income Group", y = "Count")
  ggsave(file.path(out_dir, "bar_income_group.png"), p_inc_bar, width = 6, height = 4)
}

# --------- 9. Correlation matrix (numeric columns) ----------
num_vars <- df_filtered %>% select(where(is.numeric))
if(ncol(num_vars) >= 2) {
  cor_mat <- cor(num_vars, use = "pairwise.complete.obs")
  png(file.path(out_dir, "correlation_matrix.png"), width = 800, height = 600)
  corrplot::corrplot(cor_mat, method = "ellipse", type = "lower", tl.cex = 0.8)
  dev.off()
  write.csv(cor_mat, file.path(out_dir, "correlation_matrix.csv"))
}

# --------- 10. Simple predictive model (example) ----------
# Linear regression: predict spending_score from annual_income and age (if available)
if(all(c("spending_score") %in% names(df_filtered) &&
       ( "annual_income" %in% names(df_filtered) || "age" %in% names(df_filtered)))) {
  lm_vars <- c("spending_score",
               intersect(c("annual_income","age"), names(df_filtered)))
  lm_df <- df_filtered %>% select(all_of(lm_vars)) %>% drop_na()
  if(nrow(lm_df) > 10) {
    formula_text <- paste("spending_score ~", paste(setdiff(names(lm_df), "spending_score"), collapse = " + "))
    lm_mod <- lm(as.formula(formula_text), data = lm_df)
    sink(file.path(out_dir, "linear_model_summary.txt"))
    print(summary(lm_mod))
    sink()
  }
}

# --------- 11. Clustering (k-means) - common for mall datasets ----------
# We'll cluster on standardized numeric features (annual_income, spending_score, age if present)
clust_cols <- intersect(c("annual_income","spending_score","age"), names(df_filtered))
if(length(clust_cols) >= 2) {
  clust_df <- df_filtered %>% select(all_of(clust_cols)) %>% drop_na()
  # scale
  clust_scaled <- scale(clust_df)
  # determine optimal k visually (elbow)
  set.seed(123)
  wss <- sapply(1:10, function(k) { kmeans(clust_scaled, centers=k, nstart=25)$tot.withinss })
  elbow <- data.frame(k=1:10, wss=wss)
  write.csv(elbow, file.path(out_dir, "kmeans_elbow.csv"), row.names = FALSE)
  png(file.path(out_dir, "kmeans_elbow.png"), width = 800, height = 500)
  plot(elbow$k, elbow$wss, type="b", xlab="k", ylab="Total within sum of squares", main="Elbow method")
  dev.off()
  # choose k = 4 as a reasonable starting point (adjustable)
  k_choice <- 4
  km <- kmeans(clust_scaled, centers = k_choice, nstart = 50)
  # attach cluster labels back (only for rows used)
  clust_df_with <- clust_df
  clust_df_with$cluster <- factor(km$cluster)
  write.csv(clust_df_with, file.path(out_dir, "kmeans_clustered.csv"), row.names = FALSE)
  # visualise using PCA footprint
  pca <- prcomp(clust_scaled, center = TRUE, scale. = TRUE)
  pca_df <- as.data.frame(pca$x[,1:2])
  pca_df$cluster <- factor(km$cluster)
  ggplot(pca_df, aes(x=PC1, y=PC2, color=cluster)) + geom_point() +
    labs(title = "K-means clusters (PCA projection)")
  ggsave(file.path(out_dir, "kmeans_pca_projection.png"), width = 6, height = 4)
}

# --------- 12. Save a short report CSVs and zipped output ----------
# List outputs
output_files <- list.files(out_dir, full.names = TRUE)
cat("Outputs written to", out_dir, ":\n")
print(output_files)

# Write a small README for outputs
readme_txt <- c(
  "This folder contains outputs from the R analysis script:",
  "- shopping_cleaned.csv: cleaned full dataset",
  "- shopping_filtered.csv: dataset after basic outlier filtering",
  "- summary.txt: skim summary of dataset",
  "- basic_stats.csv: means and sds for key numeric columns",
  "- na_counts.csv: NA counts per column",
  "- correlation_matrix.csv: numeric correlations",
  "- *_png and *_csv: visualizations and tables exported by script",
  "",
  "Adjust thresholds and the chosen clustering k as needed."
)
writeLines(readme_txt, con = file.path(out_dir, "README_analysis_outputs.txt"))

# END
cat("Analysis complete. All outputs are in the folder:", out_dir, "\n")
