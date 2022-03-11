url <- "https://raw.githubusercontent.com/lemoinef/Loan-Default-Prediction/master/loans_2007.csv"
raw_data <- read.csv(url)
head(raw_data,5)
write.csv(raw_data,"raw_loan_default.csv")
str(raw_data)
library(readxl)
data<- read_excel("C:/Users/isha/Documents/LoanDefaultAnalysis_R/LCDataDictionary.xlsx")
data
dim(raw_data)
library(dplyr)  
features_1st_13_set <- c('id','member_id','loan_amnt','funded_amnt','funded_amnt_inv','term',
                         'int_rate','installment','grade','sub_grade','emp_title','emp_length','home_ownership')
subset(data, LoanStatNew %in% features_1st_13_set)
raw_data %>% 
  select(features_1st_13_set) %>% 
  head(5)
## drop columns which does not provide any meaningful interpetation.
drop_col_1st_set <- c('id', 'member_id', 'funded_amnt', 'funded_amnt_inv', 'grade', 'sub_grade', 'emp_title')
features_2nd_13_set <- c('annual_inc','verification_status','issue_d','loan_status','pymnt_plan','purpose',
                         'title','zip_code','addr_state','dti','delinq_2yrs','earliest_cr_line', 'inq_last_6mths')

subset(data, LoanStatNew %in% features_2nd_13_set)
## drop columns which does not provide any meaningful interpetation.
drop_col_2nd_set <- c('issue_d','zip_code')
features_3rd_13_set <- c('open_acc', 'pub_rec', 'revol_bal','revol_util','total_acc', 'initial_list_status','out_prncp',
                         'out_prncp_inv','total_pymnt','total_pymnt_inv','total_rec_prncp','total_rec_int', 'total_rec_late_fee')
subset(data, LoanStatNew %in% features_3rd_13_set)

raw_data %>% 
  select(features_3rd_13_set) %>% 
  head(5)
## drop columns which does not provide any meaningful interpetation.
drop_col_3rd_set <- c('out_prncp', 'out_prncp_inv', 'total_pymnt', 'total_pymnt_inv','total_rec_prncp', 'total_rec_int', 'total_rec_late_fee')

features_4th_13_set <- c('recoveries','collection_recovery_fee', 'last_pymnt_d', 'last_pymnt_amnt','last_credit_pull_d',
                         'collections_12_mths_ex_med','policy_code','application_type','acc_now_delinq','chargeoff_within_12_mths',
                         'delinq_amnt','pub_rec_bankruptcies','tax_liens')
subset(data, LoanStatNew %in% features_4th_13_set)
raw_data %>% 
  select(features_4th_13_set) %>% 
  head(5)
## drop columns which does not provide any meaningful interpetation.
drop_col_4th_set <- c('recoveries', 'collection_recovery_fee', 'last_pymnt_d', 'last_pymnt_amnt')
columns_to_drop <- c(drop_col_1st_set,drop_col_2nd_set,drop_col_3rd_set,drop_col_4th_set)
columns_to_drop
filtered_data <- raw_data %>% 
  select(select = -c(drop_col_1st_set,drop_col_2nd_set,drop_col_3rd_set,drop_col_4th_set))
filtered_data %>% head(5)
str(filtered_data)
dim(filtered_data)

write.csv(filtered_data,"filtered_loan_default.csv")
filtered_data %>%  
  count(loan_status,sort = TRUE)

library(ggplot2)
install.packages("forcats")
library(forcats)
options(repr.plot.width=15, repr.plot.height=10)

df <- filtered_data %>% 
  filter(loan_status %in% c("Fully Paid", "Charged Off"))
write.csv(df,"loan_data.csv")

hist(raw_data$loan_amnt, breaks = 30)

library(DataExplorer)
install.packages("tidyr")
library(tidyr)
library(magrittr)

install.packages("skimr")
library(skimr)
df %>% skim()
single_value_columns <- df %>%
  select_if(function(col) length(unique(col))==1)
names(single_value_columns)
c('pymnt_plan',
  'initial_list_status',
  'collections_12_mths_ex_med',
  'policy_code',
  'application_type',
  'acc_now_delinq',
  'chargeoff_within_12_mths',
  'delinq_amnt',
  'tax_liens')
subset(df, select = c('pymnt_plan',
                      'initial_list_status',
                      'collections_12_mths_ex_med',
                      'policy_code',
                      'application_type',
                      'acc_now_delinq',
                      'chargeoff_within_12_mths',
                      'delinq_amnt',
                      'tax_liens')) 
df <- df %>% 
  select(select = -c('pymnt_plan',
                     'initial_list_status',
                     'collections_12_mths_ex_med',
                     'policy_code',
                     'application_type',
                     'acc_now_delinq',
                     'chargeoff_within_12_mths',
                     'delinq_amnt',
                     'tax_liens'))
dim(df)
colSums(is.na(df))
null_count <- sapply(df, function(x) sum(is.na(x)))
data.frame(null_count)
df <- df %>% 
  select(select = -c('pub_rec_bankruptcies'))
sapply(df, class)
table(sapply(df, class))
col_names <- df %>% 
  select_if(is.character) %>% 
  names()

col_names
df <- df %>% mutate_if(is.character, as.factor)
table(sapply(df, class))
df %>%  
  count(int_rate,sort = TRUE)
df <- df %>% mutate(int_rate = as.numeric(gsub("%", "", int_rate)))
df %>%  
  count(revol_util,sort = TRUE)
df <- df %>% mutate(revol_util = as.numeric(gsub("%", "", revol_util)))
df
df %>%  
  count(term,sort = TRUE)
loan_term.map <- c(" 36 months"= 36,
                   " 60 months"= 60)    
df$term <- loan_term.map[as.character(df$term)]
df$term
df
df %>%  
  count(emp_length,sort = TRUE)
empployement_duraion.map <- c("10+ years"= 10,
                              "9 years"= 9,
                              "8 years"= 8,
                              "7 years"= 7,
                              "6 years"= 6,
                              "5 years"= 5,
                              "4 years"= 4,
                              "3 years"= 3,
                              "2 years"= 2,
                              "1 year"= 1,
                              "< 1 year"= 0,
                              "n/a"= 0)    
df$emp_length <- empployement_duraion.map[as.character(df$emp_length)]
df
df %>%  
  count(home_ownership,sort = TRUE)
df %>%  
  count(verification_status,sort = TRUE)
install.packages("caret")
library(caret)
df %>%  
  count(purpose,sort = TRUE)
df %>%  
  count(title,sort = TRUE)
df %>%  
  count(addr_state,sort = TRUE)
sample_cat_df <- subset(df ,select = c("home_ownership","verification_status","purpose"))
#define one-hot encoding function
dummy <- dummyVars(" ~ .", data=sample_cat_df)

install.packages(c("nycflights13", "gapminder", "Lahman"))

#Distribution plots

counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution",
        xlab="Number of Gears")
hist(int_rate, breaks = 30, col = "yellow" , border = "black" )

hist(annual_inc, breaks = 30, col = "yellow" , border = "black" )

ggplot(data = df) + geom_bar(mapping = aes(x = revol_util))

#Scatter plots between numnerical attributes

install.packages("tidyverse")
library(tidyverse)

#loan amount Vs other attributes:

ggplot(data = df) +
  geom_point(mapping = aes(x = loan_amnt, y = int_rate), color = "blue")

ggplot(data = df) +
  geom_point(mapping = aes(x = loan_amnt, y = installment), color = "blue")

ggplot(data = df) +
  geom_point(mapping = aes(x = loan_amnt, y = annual_inc), color = "blue")

ggplot(data = df) +
  geom_point(mapping = aes(x = loan_amnt, y = dti), color = "blue")

ggplot(data = df) +
  geom_point(mapping = aes(x = loan_amnt, y = revol_bal), color = "blue")

ggplot(data = df) +
  geom_point(mapping = aes(x = loan_amnt, y = revol_util))


#interest rate Vs other attributes:


ggplot(data = df) +
  geom_point(mapping = aes(x = int_rate, y = installment), color = "blue")

ggplot(data = df) +
  geom_point(mapping = aes(x = int_rate, y = dti), color = "red")

ggplot(data = df) +
  geom_point(mapping = aes(x = int_rate, y = annual_inc), color = "red")

ggplot(data = df) +
  geom_point(mapping = aes(x = int_rate, y = revol_bal), color = "red")

ggplot(data = df) +
  geom_point(mapping = aes(x = int_rate, y = revol_util), color = "red")

#installment Vs other attributes:

ggplot(data = df) +
  geom_point(mapping = aes(x = installment, y = annual_inc), color = "blue")

ggplot(data = df) +
  geom_point(mapping = aes(x = installment, y = dti), color = "red")

ggplot(data = df) +
  geom_point(mapping = aes(x = installment, y =  revol_bal), color = "green")

ggplot(data = df) +
  geom_point(mapping = aes(x = int_rate, y = revol_util), color = "yellow")

#annual income Vs other attributes:

ggplot(data = df) +
  geom_point(mapping = aes(x = annual_inc, y = dti), color = "blue")

ggplot(data = df) +
  geom_point(mapping = aes(x = annual_inc, y = revol_bal), color = "red")

ggplot(data = df) +
  geom_point(mapping = aes(x = annual_inc, y = revol_util), color = "green")


#revol_bal Vs other attributes:

ggplot(data = df) + geom_point(mapping = aes(x = revol_bal, y = revol_util), color = "blue")

#Bar plot or count plot of categoricl attributes:
# a.	Term: 
# b.	Employement length
# c.	Home Ownership
# d.	Verification status
# e.	Purpose/title

ggplot(data = df) + geom_bar(mapping = aes(x = term))
 

ggplot(data = df) + geom_bar(mapping = aes(x = emp_length))


ggplot(data = df) + geom_bar(mapping = aes(x = home_ownership, fill = home_ownership))
 

ggplot(data = df) + geom_bar(mapping = aes(x = verification_status, fill = verification_status))
 

ggplot(data=df) + geom_bar(mapping = aes(x = purpose, fill = purpose)) +
   theme(axis.text.x = element_text(angle = 270, vjust = 0.5),text = element_text(size = 14))
   
library("heatmap")
heatmap(df)

install.packages("ggcorrplot")
ggcorrplot(df, hc.order=TRUE, type="lower",  lab=TRUE)
superheat(df,scale =TRUE)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggcorrplot")


#Relationship between Numerical and cataegorical values

ggplot(data = df, mapping = aes(y = loan_amnt, x = purpose, fill = purpose )) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5),text = element_text(size = 14))

ggplot(data = df, mapping = aes(y = annual_inc, x = purpose, fill = purpose )) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5),text = element_text(size = 14))

ggplot(data = df, mapping = aes(y = int_rate, x = purpose, fill = purpose )) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5),text = element_text(size = 14))

ggplot(data = df, mapping = aes(y = installment, x = purpose, fill = purpose )) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5),text = element_text(size = 14))




