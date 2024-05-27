#### ENVIRONMENT PREP  ---------

Sys.setlocale("LC_CTYPE", "lithuanian")
library(tidyverse)
library(readxl)
library(openxlsx)
library(reshape2)
library(plotly)
library(effsize)
library(coin)

# Load data
df_student <- read_excel('data/data_students.xlsx')
df_teacher <- read_excel('data/data_teachers.xlsx')
items <- read_excel("data/items.xlsx")

#### DATA PREP -----------------------

likert_range_students <- 7:84
likert_range_teachers <- 11:88
df_student[, likert_range_students] <- df_student[, likert_range_students] - 3
df_teacher[, likert_range_teachers] <- df_teacher[, likert_range_teachers] - 3

## Categorize GPA and Tenure
df_student$GPA_Category <- cut(df_student$GPA, 
                               breaks = c(0, 3.99, 5, 8, Inf),
                               labels = c("Nepatenkinamas", "Patenkinamas", "Pagrindinis", "Aukstesnysis"),
                               right = TRUE)

df_teacher$Tenure_Category <- cut(df_teacher$Tenure, 
                                  breaks = c(0, 10, 20, 30, 40, Inf),
                                  labels = c("0-10", "11-20", "21-30", "31-40", "41+"),
                                  right = TRUE)

df_teacher$Age_Category <- cut(df_teacher$Age, 
                               breaks = c(20, 40, 60, Inf),
                               labels = c("23-40", "41-60", "61+"),
                               right = TRUE)

# Convert columns to factors
df_student <- df_student %>% mutate(across(c(where(is.character), "Class"), as.factor))
df_teacher <- df_teacher %>% mutate(across(where(is.character), as.factor))

## Combine class columns for teachers
df_teacher <- df_teacher %>% rowwise() %>% mutate(Classes = paste(na.omit(c_across(starts_with("Teach"))), collapse = ", ")) %>%
  ungroup() %>% dplyr::select(-starts_with("Teach"), ID, Gender, Tenure_Category, Tenure, Age_Category, Age, Location, SchoolType, Classes, everything())

## Convert item columns to appropriate types
items <- items %>% mutate(Abbr = as.factor(Abbr), Group = as.factor(Group), Number = as.numeric(gsub("\\D", "", Vcol)))



#### PIVOT DATA ----------------------------------------------------------

##### df_long_student ----
df_long_student <- pivot_longer(df_student, cols = starts_with(c("N", "V")), names_to = "LikertType", values_to = "LikertValue")
df_long_student <- df_long_student %>% mutate(Number = as.numeric(gsub("[^0-9]", "", LikertType)))

##### df_long_teacher ----
df_long_teacher <- pivot_longer(df_teacher, cols = starts_with(c("N", "V")), names_to = "LikertType", values_to = "LikertValue")
df_long_teacher <- df_long_teacher %>% mutate(Number = as.numeric(gsub("[^0-9]", "", LikertType)))


#### AVERAGES DATAFRAMES  ----------------------------------------------------------

##### avgs_student/teacher -----
# Calculate and reshape averages for both students and teachers

calculate_reshape_averages <- function(df_long) {
  df_long %>%
    group_by(Number, Type = ifelse(substr(LikertType, 1, 1) == "N", "Naudojimas", "Veiksmingumas")) %>%
    summarize(Average = round(mean(LikertValue, na.rm = TRUE), 2), .groups = 'drop') %>%
    pivot_wider(names_from = Type, values_from = Average)
}

avgs_student <- calculate_reshape_averages(df_long_student) 
avgs_teacher <- calculate_reshape_averages(df_long_teacher)

avgs_student_merged <- left_join(avgs_student, items, by = "Number")
avgs_teacher_merged <-  left_join(avgs_teacher, items, by = "Number")

##### avg_merged_teach_stud ----
# Merge student and teacher averages with items
avg_merged_teach_stud <- items %>%
  dplyr::select(Number, Abbr2, Type_other) %>%
  full_join(avgs_student, by = "Number") %>%
  full_join(avgs_teacher, by = "Number", suffix = c("_student", "_teacher"))

write.xlsx(avg_merged_teach_stud, "results/TEST.xlsx", rowNames = FALSE)

lookup_table <- tibble(
  OldValue = c("Naudojimas_student", "Veiksmingumas_student", "Naudojimas_teacher", "Veiksmingumas_teacher"),
  NewValue = c("MOKINIAI naudojimas", "MOKINIAI veiksmingumas", "MOKYTOJAI naudojimas", "MOKYTOJAI veiksmingumas")
)

avg_merged_teach_stud <- items %>%
  dplyr::select(Number, Abbr2, Type_other) %>%
  full_join(avgs_student, by = "Number") %>%
  full_join(avgs_teacher, by = "Number", suffix = c("_student", "_teacher")) %>%
  arrange(desc(Veiksmingumas_student))

##### avg_merged_teach_stud_melted ----
# Melt and rename variables for plotting
avg_merged_teach_stud_melted <- avg_merged_teach_stud %>%
  pivot_longer(cols = c(Veiksmingumas_student, Veiksmingumas_teacher, 
                        Naudojimas_student, Naudojimas_teacher), 
               names_to = "Variable", values_to = "Average") %>%
  left_join(lookup_table, by = c("Variable" = "OldValue")) %>%
  mutate(Variable = ifelse(is.na(NewValue), Variable, NewValue)) %>%
  dplyr::select(-NewValue)


##### avgs_ by factor -----
# Calculate and save averages globally
student_features <- c("Gender", "Location", "SchoolType", "Class", "GPA_Category")
teacher_features <- c("Gender", "Location", "SchoolType", "Age_Category")

## TOFIX: gpa is not calculated for all
# Calculate and save averages globally
calculate_and_save_avgs <- function(data, features) {
  avg_list <- lapply(features, function(feature) {
    avg_data <- data %>%
      filter(grepl("^V", LikertType)) %>%  # Correct filtering for LikertType
      group_by(LikertType, !!sym(feature)) %>%
      summarize(Average = mean(LikertValue, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = !!sym(feature), values_from = Average, values_fill = NA) %>%  # Ensure all levels are included
      dplyr::select(-any_of("NA"))# %>%
      #rename_with(~ paste0(., "_", tolower(feature)), -LikertType)

    return(avg_data)
  })

  merged_avgs <- Reduce(function(x, y) full_join(x, y, by = "LikertType"), avg_list)
  return(merged_avgs)
}

merged_avgs_student <- calculate_and_save_avgs(df_long_student, student_features)
merged_avgs_teacher <- calculate_and_save_avgs(df_long_teacher, teacher_features)

merged_avgs_student <- calculate_and_save_avgs(df_long_student, student_features, "avgs_student")
merged_avgs_teacher <- calculate_and_save_avgs(df_long_teacher, teacher_features, "avgs_teacher")

write.xlsx(merged_avgs_student, "results/avgs_factor_student.xlsx", overwrite = TRUE)
write.xlsx(merged_avgs_teacher, "results/avgs_factor_teacher.xlsx", overwrite = TRUE)


#### P-VALUE AND COHEN D----------------------------------------------------------

# The function calculates p-values and Cohen's d for all possible pairs of levels for a given feature.
# The loop iterates over the number of columns in the 'pairs' matrix, which contains all possible pairs of levels for the feature.
calculate_p_and_d <- function(data, feature) {
  data[[feature]] <- as.factor(data[[feature]])
  p_values <- data.frame()
  d_values <- data.frame()
  pairs <- combn(levels(data[[feature]]), 2)

  for (i in seq_len(ncol(pairs))) {
    p1 <- pairs[1, i]
    p2 <- pairs[2, i]

    subset_data <- data %>%
      filter(!!sym(feature) %in% pairs[, i]) %>%
      mutate(!!sym(feature) := factor(as.character(!!sym(feature))))

    for (likert_prefix in c("N", "V")) {
      likert_columns <- grep(paste0("^", likert_prefix), names(data), value = TRUE)
      for (j in seq_along(likert_columns)) {
        formula <- as.formula(paste(likert_columns[j], "~", feature))
        comparison <- paste0(p1, "_vs_", p2)

        d_values[j, paste0(comparison, "_d")] <- cohen.d(formula, data = subset_data, na.rm = TRUE)$estimate

        tryCatch({
          p_values[j, paste0(comparison, "_p")] <- pvalue(wilcox_test(formula, data = subset_data))
        }, warning = function(w) {
          cat("Warning in pair", p1, "and", p2, "Type:", likert_prefix, "Item:", likert_columns[j], conditionMessage(w), "\n")
          p_values[j, paste0(comparison, "_p")] <- NA
        })
      }
    }
  }

  combined <- cbind(p_values, d_values)
  return(combined)
}


wb_student <- createWorkbook()
wb_teacher <- createWorkbook()

# Calculate and save for students
for (feature in student_features) {
  addWorksheet(wb_student, feature)
  results <- calculate_p_and_d(df_student, feature)
  writeData(wb_student, sheet = feature, x = results)
}
saveWorkbook(wb_student, "results/p_and_d_student.xlsx", overwrite = TRUE)

# Calculate and save for teachers
for (feature in teacher_features) {
  addWorksheet(wb_teacher, feature)
  results <- calculate_p_and_d(df_teacher, feature)
  writeData(wb_teacher, sheet = feature, x = results)
}
saveWorkbook(wb_teacher, "results/p_and_d_teacher.xlsx", overwrite = TRUE)



#### CORRELATION CALC ----------------------------------------------------------

# Calculate correlations
cor_test <- function(y, x) {
  test <- cor.test(y, x, method = "spearman", use = "pairwise.complete.obs")
  c(correlation = test$estimate, p_value = test$p.value)
}

v_columns <- grep("^V", names(df_student))
n_columns <- grep("^N", names(df_student))
gpa_column <- df_student$GPA

results_v <- apply(df_student[, v_columns], 2, function(x) cor_test(gpa_column, x))
results_n <- apply(df_student[, n_columns], 2, function(x) cor_test(gpa_column, x))

##### correlation_df----
correlation_df <- data.frame(
  Number = seq_along(v_columns),
  correlation_v = sapply(results_v, `[`, "correlation"),
  p_value_v = sapply(results_v, `[`, "p_value"),
  correlation_n = sapply(results_n, `[`, "correlation"),
  p_value_n = sapply(results_n, `[`, "p_value")
)

correlation_df_merged <- merge(correlation_df, items[, c("Abbr2", "Number")], by = "Number", all.x = TRUE)