library(readxl)
library(modeest)
library(naniar)

data <- read_excel("D:/data science project-mid/data/Midterm_Dataset_Section(A).xlsx")

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

#plot missing data
vis_miss(data)


#detect outlier




#numeric to categorical

df_mode$loan_status <-ifelse(df_mode$loan_status == 1 ,"Yes","No");


#categorical to numeric

df_mode$previous_loan_defaults_on_file <- ifelse(df_mode$previous_loan_defaults_on_file == "Yes", 1,0);


# normalization method for any continuous attribute. 

df<-df_mode

df$person_income<-(df$person_income -min(df$person_income))/(max(df$person_income)-min(df$person_income))


#find and remove duplicate rows.  

duplicates<-df[duplicated(df),]
df_unique <- df[!duplicated(df),]
