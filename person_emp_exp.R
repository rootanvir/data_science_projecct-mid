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
# Normalize the column
loan2$person_emp_exp_norm <- (loan2$person_emp_exp - min(loan2$person_emp_exp)) /
  (max(ld$person_emp_exp) - min(ld$person_emp_exp))

# View first few normalized values in console
head(loan2$person_emp_exp_norm)

# View first 200 normalized values in RStudio viewer
View(loan2$person_emp_exp_norm[1:200 ])




## 6.find duplicate and remove
loan3 <- Loan_Datas

# View which rows are duplicates
duplicated_rows
loan3[duplicated(loan3), ]

distinct_data<-distinct(loan3, person_education, .keep_all = TRUE)


# View the result
print(distinct_data)

# View the cleaned dataset
head(ld_unique)
View(ld_unique[1:200, ])



## 7. View filtered data
loan4<-Loan_Datas
# Example: Filter rows where person_emp_exp > 10
filtered_data <- loan4[loan4$person_emp_exp > 3.0, ]


head(filtered_data)
View(filtered_data[1:80, ])


##5. invalid dataset and apply appropriate method
loan5 <- Loan_Datas  
loa5 <-Loan_Datas
summary(loan5$person_emp_exp)              

invalid_exp <- loan5$person_emp_exp[loan5$person_emp_exp < 0 | loan5$person_emp_exp > 60]
invalid_exp   
View(loan5[1:199, ])

# replace average value
invalid_exp <- loa5$person_emp_exp[ loan5$person_emp_exp > 60]
invalid_exp   
avg_exp <- mean(loa5$person_emp_exp, na.rm = TRUE)
print(avg_exp)
loa5$person_emp_exp[loan5$person_emp_exp > 10] <- avg_exp
View(loa5[1:200, ])



## 10. split the dataset for training and test
#  Set a seed for reproducibility
set.seed(123)  # ensures the same random split every time
n <- nrow(loan5)

# Randomly select 70% of rows for training
train_index <- sample(1:n, size = 0.7 * n)

#  Create training and testing sets
train_data <- loan5[train_index, ]   # 70% for training
test_data  <- loan5[-train_index, ]  # remaining 30% for testing

# Check dimensions
dim(train_data)
dim(test_data)



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



