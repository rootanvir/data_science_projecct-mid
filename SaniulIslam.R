ld <- Loan_Datas
print(ld)
var<-c("person_age","person_gender","person_education","person_income")
head(ld[var])


## 3. for outliers
summary(ld$person_emp_exp)

Q1 <- quantile(ld$person_emp_exp, 0.25)
Q3 <- quantile(ld$person_emp_exp, 0.75)
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outliers <- ld$person_emp_exp[ld$person_emp_exp < lower_bound | ld$person_emp_exp > upper_bound]

outliers

ld$person_emp_exp[ld$person_emp_exp < lower_bound] <- lower_bound
ld$person_emp_exp[ld$person_emp_exp > upper_bound] <- upper_bound
ld_no_outliers <- ld %>% filter(person_emp_exp >= lower_bound & person_emp_exp <= upper_bound)
ld_no_outliers
num_outliers <- length(outliers)
num_outliers

if (num_outliers > 0) {
  cat("There are", num_outliers, "outliers in person_emp_exp.\n")
  cat("Outlier values are:", outliers, "\n")
} else {
  cat("No outliers detected in person_emp_exp.\n")
}
View(ld[1:200, ])

#View(ld[1:200, ])
# Returns TRUE for rows that are duplicates (except the first occurrence)
duplicated_rows <- duplicated(ld)


## 5.find normalize
loan2 <- Loan_Datas
loan2$loan_amnt_norm <- (loan2$loan_amnt - min(loan2$loan_amnt, na.rm = TRUE)) /
  (max(loan2$loan_amnt, na.rm = TRUE) - min(loan2$loan_amnt, na.rm = TRUE))

print(loan2$loan_amnt_norm)
#View(loan2$person_emp_exp_norm[1:200 ])




## 6.find duplicate and remove
loan3 <- Loan_Datas
library(dplyr)
distinct_data <- distinct(loan3, person_education, .keep_all = TRUE)
print(distinct_data)



## 7. View filtered data
loan4<-Loan_Datas
filtered_data <- filter(loan3, loan_amnt > 10000)
print(filtered_data)
filtered_data <- filter(loan3, !is.na(person_income) & person_income > 20000)
print(filtered_data)
filtered_data <- filter(loan3, person_education %in% c("Bachelor", "Master"))
print(filtered_data)


#head(filtered_data)
#View(filtered_data[1:80, ])


##8. invalid dataset and apply appropriate method
loans5<-Loan_Datas
loans5 <- na.omit(loans5)
cat("Number of rows after removing rows with missing values:\n")
print(nrow(loans5))
print(loans5)
View(loan5[1:200, ])

loan5 <- Loan_Datas  
loan3_clean <- na.omit(loan5)
print(nrow(loan3_clean))
loan3_clean
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
print(loan5)
#View(loan5[1:200, ])



## 10. split the dataset for training and test
#  Set a seed for reproducibility
#laons5<-Loan_Datas
set.seed(123)
n <- nrow(laons5)
train_index <- sample(1:n, size = 0.7 * n)
train_data <- laons5[train_index, ]   
test_data  <- laons5[-train_index, ] 
dim(train_data)
dim(test_data)

###11
loan7<-Loan_Datas
loan_clean <- na.omit(loan5_balanced)
descriptive_stats <- loan7 %>%
  group_by(loan_status) %>%   
  summarise(
    mean_age = mean(person_age, na.rm = TRUE),
    median_age = median(person_age, na.rm = TRUE),
    sd_age = sd(person_age, na.rm = TRUE),
    min_age = min(person_age, na.rm = TRUE),
    max_age = max(person_age, na.rm = TRUE),
    
    mean_income = mean(person_income, na.rm = TRUE),
    median_income = median(person_income, na.rm = TRUE),
    sd_income = sd(person_income, na.rm = TRUE),
    min_income = min(person_income, na.rm = TRUE),
    max_income = max(person_income, na.rm = TRUE),
    
    count = n()   # number of rows per class
  )

print(descriptive_stats)
View(loan7[1:200, ])



## 12 average value of person_emp_exp based on loan_status
loan6<-Loan_Datas

loan0 <- loan6$person_emp_exp[loan6$loan_status == 0]
# Count of rows
count_0 <- length(loan0)
count_0
avg_exp_0<-mean(loan6$person_emp_exp[loan5$loan_status == 0], na.rm=TRUE)
avg_exp_0

loan7<-Loan_Datas
loan7
View(loan7[1:201, ])

loan1 <- loan7$person_emp_exp[loan7$loan_status == 1]
count_1 <-length(loan1)
count_1
avg_exp_1<-mean(loan6$person_emp_exp[loan5$loan_status == 1], na.rm=TRUE)
avg_exp_1

## 12 average value of person_emp_exp based on loan_status
loan6<-Loan_Datas


loan0 <- loan6$person_emp_exp[loan6$loan_status == 0]
# Count of rows
count_0 <- length(loan0)
count_0
avg_exp_0<-mean(loan6$person_emp_exp[loan5$loan_status == 0], na.rm=TRUE)
avg_exp_0

loan7<-Loan_Datas
loan7
View(loan7[1:204, ])

loan1 <- loan7$person_emp_exp[loan7$loan_status == 1]
count_1 <-length(loan1)
count_1
avg_exp_1<-mean(loan6$person_emp_exp[loan5$loan_status == 1], na.rm=TRUE)
avg_exp_1


## 13
loan8<-Loan_Datas
loan8_clean <- subset(loan8, !is.na(person_education))

# Check the cleaned dataset
head(loan8_clean)
library(dplyr)
education_stats <- loan8 %>%
  group_by(person_education) %>%
  summarise(
    mean_exp = mean(person_emp_exp, na.rm = TRUE),
    median_exp = median(person_emp_exp, na.rm = TRUE),
    sd_exp = sd(person_emp_exp, na.rm = TRUE),
    var_exp = var(person_emp_exp, na.rm = TRUE),
    count = n()
  )

# Print the results
print(education_stats)



