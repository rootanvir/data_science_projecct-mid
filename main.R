library(readxl)
library(modeest)
library(naniar)
library(dplyr)

data <- read_excel("D:/data science project-mid/data/Midterm_Dataset_Section(A).xlsx")



###########################################################   1
missing_rows <- data[!complete.cases(data), ]#find missing row
print(missing_rows)

drop_data <- na.omit(data);#drop missing data
print(drop_data);

df_mean <-data#replace by mean
df_mean$person_income[is.na(df_mean$person_income)] <- mean(df_mean$person_income,na.rm=TRUE);

df_median <- df_mean#replace by median
df_median$person_age[is.na(df_median$person_age)] <- median(df_median$person_age,na.rm = TRUE);


#mode
df_mode <- df_median
mode_val <- mlv(df_mode$loan_status,method = "mfv",na.rm = TRUE);
df_mode$loan_status[is.na(df_mode$loan_status)] <- mode_val

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


#categorical to numeric

df_mode$previous_loan_defaults_on_file <- ifelse(df_mode$previous_loan_defaults_on_file == "Yes", 1,0);

###################################################   4



# Detect Outlier
# ------------------------------
# Function 1: Detect Outliers
# ------------------------------
detect_outliers <- function(data, colname) {
  if (!colname %in% names(data)) stop("Column not found.")
  if (!is.numeric(data[[colname]])) stop("Column must be numeric.")
  
  Q1 <- quantile(data[[colname]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[colname]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  print(paste("IQR:",Q1," ",Q3))
  
  
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  
  is_outlier <- data[[colname]] < lower | data[[colname]] > upper
  n_out <- sum(is_outlier, na.rm = TRUE)
  
  message(n_out, " outliers found in ", colname)
  
  # Return data frame with values + flag
  return(
    data.frame(
      value = data[[colname]],
      isOutlier = s_outlieri
    )
  )
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
outlier_detect<-detect_outliers(df, "person_age")

# Fix and get cleaned dataset
df_clean <- fix_outliers(df, "person_age")


#################################################### 5 

df_nor <- df

normalize <- function(x) {
  return( (x - min(x)) / (max(x) - min(x)) )
}

df_nor$loan_amnt <- normalize(df_nor$loan_amnt)


############################################## 6

df_unique <- df

df_unique <- distinct(df, person_education  ,.keep_all = TRUE)



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

compare <- compare_spread(df, "person_education", "person_emp_exp")


