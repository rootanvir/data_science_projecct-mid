library(readxl)
library(modeest)
library(naniar)
library(dplyr)
library(rsample)

data <- read_excel("D:/data science project-mid/data/Midterm_Dataset_Section(A).xlsx")



missing_rows <- data[!complete.cases(data), ]
head(missing_rows)

drop_data <- na.omit(data);
head(drop_data);

df_mean <- data
df_mean$person_income[is.na(df_mean$person_income)] <- mean(df_mean$person_income, na.rm = TRUE);
head(df_mean)

df_median <- df_mean
df_median$person_age[is.na(df_median$person_age)] <- median(df_median$person_age, na.rm = TRUE);
head(df_median)

df_mode <- df_median
mode_val <- mlv(df_mode$loan_status, method = "mfv", na.rm = TRUE);
df_mode$loan_status[is.na(df_mode$loan_status)] <- mode_val
head(as.data.frame(df_mode))

mode_val <- mlv(df_mode$person_gender, method = "mfv", na.rm = TRUE);
df_mode$person_gender[is.na(df_mode$person_gender)] <- mode_val

mode_val <- mlv(df_mode$person_education, method = "mfv", na.rm = TRUE);
df_mode$person_education[is.na(df_mode$person_education)] <- mode_val

print(sum(is.na(df_mode)))



vis_miss(data)



df_mode$loan_status <- ifelse(df_mode$loan_status == 1, "Yes", "No");
head(as.data.frame(df_mode))

df_mode$previous_loan_defaults_on_file <- ifelse(df_mode$previous_loan_defaults_on_file == "Yes", 1, 0);
head(as.data.frame(df_mode))
df <- df_mode



get_outliers <- function(data, colname) {
  if (!colname %in% names(data)) stop("Column not found.")
  if (!is.numeric(data[[colname]])) stop("Column must be numeric.")
  
  Q1 <- quantile(data[[colname]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[colname]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  
  outliers <- data[[colname]][data[[colname]] < lower | data[[colname]] > upper]
  return(outliers)
}

fix_outliers <- function(data, colname) {
  if (!colname %in% names(data)) stop("Column not found.")
  if (!is.numeric(data[[colname]])) stop("Column must be numeric.")
  
  Q1 <- quantile(data[[colname]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[colname]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  
  is_outlier <- data[[colname]] < lower | data[[colname]] > upper
  n_out <- sum(is_outlier, na.rm = TRUE)
  
  if (n_out > 0) {
    data[[colname]] <- pmin(pmax(data[[colname]], lower), upper)
  }
  
  return(data)
}

df <- df_mode
outlier_detect <- get_outliers(df, "person_age")
head(outlier_detect)

df_clean <- fix_outliers(df, "person_age")
head(as.data.frame(df_clean))



df_nor <- df
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
df_nor$loan_amnt <- normalize(df_nor$loan_amnt)
head(df_nor)



df_unique <- df
df_unique <- distinct(df, person_education, .keep_all = TRUE)
head(as.data.frame(df_unique))



filtered_data1 <- filter(df_clean, loan_amnt > 10000)
head(as.data.frame(filtered_data1))

filtered_data2 <- filter(df_clean, !is.na(person_income) & person_income > 20000)
head(as.data.frame(filtered_data2))

filtered_data3 <- filter(df_clean, person_education %in% c("Bachelor", "Master"))
head(as.data.frame(filtered_data3))



loans5 <- data
loans5 <- na.omit(loans5)
print(nrow(loans5))
head(as.data.frame(loans5))

loan5 <- data  
loan3_clean <- na.omit(loan5)
print(nrow(loan3_clean))
head(as.data.frame(loan3_clean))

num_cols <- sapply(loan5, is.numeric)      
cat_cols <- sapply(loan5, is.character)    
loan5[num_cols] <- lapply(loan5[num_cols], function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)  
  return(x)
})
loan5[cat_cols] <- lapply(loan5[cat_cols], function(x) {
  mode_val <- names(sort(table(x), decreasing = TRUE))[1] 
  x[is.na(x)] <- mode_val                                 
  return(x)
})
print(colSums(is.na(loan5[num_cols])))
print(colSums(is.na(loan5[cat_cols])))
head(as.data.frame(loan5))



df$loan_status <- ifelse(df$loan_status == "Yes", 1, 0);
under_df <- df

library(ROSE)
df$loan_status <- factor(df$loan_status, levels = c(0,1))
table(df$loan_status)
N_under <- 2 * min(table(df$loan_status))
set.seed(199)
under_df <- ovun.sample(loan_status ~ ., data = df, method = "under", N = N_under, seed = 199)$data
table(under_df$loan_status)
head(under_df)

df$loan_status <- factor(df$loan_status, levels = c(0,1))
table(df$loan_status)
N_over <- 2 * max(table(df$loan_status))
set.seed(199)
over_df <- ovun.sample(loan_status ~ ., data = df, method = "over", N = N_over, seed = 199)$data
table(over_df$loan_status)
head(over_df)

df_smote <- df_clean
set.seed(199)
df_smote$previous_loan_defaults_on_file <- factor(df_smote$previous_loan_defaults_on_file, levels = c(0,1))
df_smote[sapply(df_smote, is.character)] <- lapply(df_smote[sapply(df_smote, is.character)], factor)
df_smote <- df_smote[complete.cases(df_smote), ]  

table(df_smote$previous_loan_defaults_on_file)
rose_df <- ROSE(previous_loan_defaults_on_file ~ ., data = df_smote, N = 2000, p = 0.5)$data
table(rose_df$previous_loan_defaults_on_file)
head(rose_df)

orig <- names(df_clean)
cat_vars <- setdiff(orig[sapply(df_smote, function(x) is.factor(x) || is.character(x))],
                    "previous_loan_defaults_on_file")

pretty_df <- df_clean
for (v in cat_vars) {
  d <- grep(paste0("^", v, "_"), names(pretty_df), value = TRUE)
  if (length(d)) {
    M  <- as.matrix(pretty_df[, d, drop = FALSE])
    lv <- sub(paste0("^", v, "_"), "", d)
    pretty_df[[v]] <- factor(lv[max.col(M, ties.method = "first")],
                             levels = levels(df_smote[[v]]))
    pretty_df[d] <- NULL
  }
}

pretty_df <- pretty_df[, orig, drop = FALSE]
head(as.data.frame(pretty_df))



set.seed(123)
split <- initial_split(df, prop = 0.7)
train_data <- training(split)
test_data  <- testing(split)
dim(train_data)
dim(test_data)



age_summary <- aggregate(person_age ~ loan_status, data = df,
                         FUN = function(x) c(mean = mean(x), median = median(x),
                                             sd = sd(x), min = min(x), max = max(x)))
age_summary <- do.call(data.frame, age_summary)

income_summary <- aggregate(person_income ~ loan_status, data = df,
                            FUN = function(x) c(mean = mean(x), median = median(x),
                                                sd = sd(x), min = min(x), max = max(x)))
income_summary <- do.call(data.frame, income_summary)

print(age_summary)
print(income_summary)



aggregate(credit_score ~ loan_status, data = df, FUN = function(x) round(mean(x, na.rm=TRUE), 2))



compare_spread <- function(data, group_col, value_col) {
  library(dplyr)
  
  if (!group_col %in% names(data)) stop("Group column not found.")
  if (!value_col %in% names(data)) stop("Value column not found.")
  
  data %>%
    group_by(.data[[group_col]]) %>%
    summarise(
      count = n(),
      mean = mean(.data[[value_col]], na.rm = TRUE),
      sd = sd(.data[[value_col]], na.rm = TRUE),
      min = min(.data[[value_col]], na.rm = TRUE),
      max = max(.data[[value_col]], na.rm = TRUE),
      IQR = IQR(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    )
}

aggregate(credit_score ~ loan_status, data = data , mean, na.rm = TRUE)
compare <- compare_spread(df_clean, "person_education", "person_emp_exp")
head(as.data.frame(compare))
