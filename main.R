library(readxl)
library(modeest)
library(naniar)
library(dplyr)
library(rsample)
library(ROSE)
library(smotefamily)

data <- read_excel("D:/data science project-mid/data/Midterm_Dataset_Section(A).xlsx")



###########################################################   1
missing_rows <- data[!complete.cases(data), ]#find missing row
head(missing_rows)

drop_data <- na.omit(data);#drop missing data
head(drop_data);

df_mean <-data#replace by mean
df_mean$person_income[is.na(df_mean$person_income)] <- mean(df_mean$person_income,na.rm=TRUE);
head(df_mean)

df_median <- df_mean#replace by median
df_median$person_age[is.na(df_median$person_age)] <- median(df_median$person_age,na.rm = TRUE);
head(df_median)


#mode
df_mode <- df_median
mode_val <- mlv(df_mode$loan_status,method = "mfv",na.rm = TRUE);
df_mode$loan_status[is.na(df_mode$loan_status)] <- mode_val
head(as.data.frame(df_mode))

mode_val <- mlv(df_mode$person_gender,method = "mfv",na.rm = TRUE);
df_mode$person_gender[is.na(df_mode$person_gender)] <- mode_val

mode_val <- mlv(df_mode$person_education,method = "mfv",na.rm = TRUE);
df_mode$person_education[is.na(df_mode$person_education)] <- mode_val

#mising value
print(sum(is.na(df_mode)))




#################################################  2

#plot missing data
vis_miss(data)



#################################################   3
#numeric to categorical

df_mode$loan_status <-ifelse(df_mode$loan_status == 1 ,"Yes","No");
head(as.data.frame(df_mode))

#categorical to numeric

df_mode$previous_loan_defaults_on_file <- ifelse(df_mode$previous_loan_defaults_on_file == "Yes", 1,0);
head(as.data.frame(df_mode))
df<-df_mode
###################################################   4



# Detect Outlier
# ------------------------------
# Function 1: Detect Outliers
# ------------------------------
get_outliers <- function(data, colname) {
  if (!colname %in% names(data)) stop("Column not found.")
  if (!is.numeric(data[[colname]])) stop("Column must be numeric.")
  
  Q1 <- quantile(data[[colname]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[colname]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  
  # return only the outlier values from dataset
  outliers <- data[[colname]][data[[colname]] < lower | data[[colname]] > upper]
  
  return(outliers)
}


# ------------------------------
# Function 2: Fix Outliers
# ------------------------------
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
    message("Fixed ", n_out, " outliers in ", colname, 
            " (capped to [", round(lower, 2), ", ", round(upper, 2), "])")
  } else {
    message("No outliers to fix in ", colname)
  }
  
  return(data)
}

df<-df_mode

# Just detect
outlier_detect<-get_outliers(df, "person_age")
head(outlier_detect)

# Fix and get cleaned dataset
df_clean <- fix_outliers(df, "person_age")
head(as.data.frame(df_clean))


#################################################### 5 

df_nor <- df

normalize <- function(x) {
  return( (x - min(x)) / (max(x) - min(x)) )
}

df_nor$loan_amnt <- normalize(df_nor$loan_amnt)


############################################## 6

df_unique <- df

df_unique <- distinct(df, person_education  ,.keep_all = TRUE)




############################################### 7

df_filtered <- filter(df, person_age > 25)

##############################################  8



## --- Drop-all-NA rows version (like your loans5) ---
df_omit <- na.omit(df)
print(nrow(df_omit))
print(df_omit)

## --- Impute-NA version (like your loan5) ---
df_imp <- df

# numeric -> mean impute
num_cols <- sapply(df_imp, is.numeric)

for (nm in names(df_imp)[num_cols]) {
  m <- mean(df_imp[[nm]], na.rm = TRUE)
  if (is.nan(m)) m <- 0                 # fallback if entire column is NA
  nas <- is.na(df_imp[[nm]])
  if (any(nas)) df_imp[[nm]][nas] <- m
}

# categorical (character OR factor) -> mode (most frequent) impute
cat_cols <- sapply(df_imp, function(x) is.character(x) || is.factor(x))

mode_val <- function(x) {
  tb <- table(x, useNA = "no")
  if (length(tb) == 0) return("Unknown")
  names(tb)[which.max(tb)]
}

for (nm in names(df_imp)[cat_cols]) {
  x <- df_imp[[nm]]
  nas <- is.na(x)
  if (!any(nas)) next
  mv <- mode_val(x)
  if (is.factor(x)) {
    # ensure the mode exists as a level
    if (!(mv %in% levels(x))) levels(x) <- c(levels(x), mv)
    x[nas] <- mv
    df_imp[[nm]] <- droplevels(x)
  } else {
    x[nas] <- mv
    df_imp[[nm]] <- as.factor(x)  # keep as factor for modeling
  }
}

# NA check summaries (like your prints)
print(colSums(is.na(df_imp[num_cols, drop = FALSE])))
print(colSums(is.na(df_imp[cat_cols, drop = FALSE])))

print(df_imp)



############################################## 9

#undersampling

under_df <- ovun.sample(loan_status ~ ., data = df, method = "under", N = 201)$data
table(under_df$loan_status)   # Balanced by undersampling
head(under_df)

#Oversampling
over_df <- ovun.sample(loan_status ~ ., data = df, method = "over", N = 201)$data
table(over_df$loan_status)    # Balanced by oversampling
head(over_df)




df_mode$loan_status <- ifelse(df_mode$loan_status == "Yes", 1,0);
head(as.data.frame(df_mode))
df<-df_mode


#smote







library(ROSE)

# Use ONLY the target; leave all other columns exactly as they are
df$previous_loan_defaults_on_file <- factor(df$previous_loan_defaults_on_file, levels = c(0,1))

set.seed(199)

# Balance to 50/50 with total N = 2000 by duplicating/dropping rows only
balanced_df <- ovun.sample(previous_loan_defaults_on_file ~ ., data = df,
                           method = "both", N = 2000, p = 0.5, seed = 199)$data

# Check result
table(balanced_df$previous_loan_defaults_on_file)
head(balanced_df)



















###########################################  10



set.seed(123)
split <- initial_split(df, prop = 0.7)   # 70% train, 30% test

train_data <- training(split)
test_data  <- testing(split)
dim(train_data)
dim(test_data)

############################################ 11
# Create summary table for Age 
# y ~ x “y depends on x”



age_summary <- aggregate(person_age ~ loan_status, data = df,
                         FUN = function(x) c(mean = mean(x), median = median(x),
                                             sd = sd(x), min = min(x), max = max(x)))
age_summary <- do.call(data.frame, age_summary)

# Create summary table for Income
income_summary <- aggregate(person_income ~ loan_status, data = df,
                            FUN = function(x) c(mean = mean(x), median = median(x),
                                                sd = sd(x), min = min(x), max = max(x)))
income_summary <- do.call(data.frame, income_summary)

# Print results
print(age_summary)
print(income_summary)

############################################################### 12


aggregate(credit_score ~ loan_status, data = df, FUN = function(x) round(mean(x, na.rm=TRUE), 2))


################################################################ 13




## compare spread value
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



# Compare mean credit score between loan_status groups
aggregate(credit_score ~ loan_status, data = data , mean, na.rm = TRUE)
compare <- compare_spread(df_clean, "person_education", "person_emp_exp")
head(as.data.frame(compare))

