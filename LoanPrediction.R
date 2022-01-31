
# In this project I want to build an algorithm that will predict if an applicant can receive a loan from the bank.
# I will use the data from Kaggle platform: https://www.kaggle.com/altruistdelhite04/loan-prediction-problem-dataset
# I have the dataset: train_loan
                    

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(dslabs)
library(caret)
library(scales)
# downloading the datasets


loan_data <- read.csv("https://raw.githubusercontent.com/GeorgeMoisescu/ChooseYourOwn/main/train_loan.csv")

# getting used to data

head(loan_data)
str(loan_data)

# we see that we have 13 variables and 614 observation. the type of variables are character, integer and numeric

# transforming the data

loan_data$Gender <- as.factor(loan_data$Gender)
loan_data$Married <- as.factor(loan_data$Married)
loan_data$Dependents <- as.factor(loan_data$Dependents)
loan_data$Education <- as.factor(loan_data$Education)
loan_data$Self_Employed <- as.factor(loan_data$Self_Employed)
loan_data$CoapplicantIncome <- as.integer(loan_data$CoapplicantIncome)
loan_data$Credit_History <- as.factor(loan_data$Credit_History)
loan_data$Property_Area <- as.factor(loan_data$Property_Area)
loan_data$Loan_Status <- as.factor(loan_data$Loan_Status)
loan_data$Loan_Amount_Term <- as.factor(loan_data$Loan_Amount_Term)

summary(loan_data)

# there are missing values in the Gender, Married, Dependents, Self_Employed, Credit_History variables.

# dealing with the missing values in the loan_data:


#      Gender variable

gender_data <- loan_data %>% filter(Gender != "") %>% group_by(Gender) %>% summarize(n = n()) %>% mutate(percentage = round(n/sum(n),digits = 2))
gender_data
loan_data %>% filter(Gender == "")

# we have 19% female applicants and 81% male applicants
# we have 13 applicants with no value for Gender. 

gender_index <- which(loan_data$Gender == "")
gender_index

loan_data$Gender[c(172,461)] <- "Female"

gender_index_male <-setdiff(gender_index,c(172,461))
gender_index_male

for (i in gender_index_male) {
  loan_data$Gender[i] <- "Male"
}

loan_data$Gender <- droplevels(loan_data$Gender)
summary(loan_data$Gender)
levels(loan_data$Gender)


#     Married variable

summary(loan_data$Married)
married_data <- loan_data %>% filter(Married != "") %>% group_by(Married) %>% summarize(n = n()) %>% mutate(percentage = round(n/sum(n),digits = 2))
married_data

# we have 65% married applicants and 35% single applicants
# we have 3 applicants with no value for Married. 

married_index <- which(loan_data$Married == "")
married_index

round(0.65 * length(married_index))

loan_data$Married[c(105,229)] <- "Yes"
loan_data$Married[c(436)] <- "No"

loan_data$Married <- droplevels(loan_data$Married)
summary(loan_data$Married)
levels(loan_data$Married)


#     Dependents variable

summary(loan_data$Dependents)
dependents_data <- loan_data %>% filter(Dependents != "") %>% group_by(Dependents) %>% summarize(n = n()) %>% mutate(percentage = round(n/sum(n),digits = 2))
dependents_data

# we have 58% applicants with no dependent, 17% with 1 dependent, 17% with 2 dependents and 9% with 3 or more dependents
# we have 15 applicants with no value for Dependents. 

dependents_index <- which(loan_data$Dependents == "")
dependents_index

round(0.58 * length(dependents_index))
loan_data$Dependents[c(103,227,229,302,336,356,436,572,598)] <- 0

round(0.17 * length(dependents_index))
loan_data$Dependents[c(105,121,294)] <- 1
loan_data$Dependents[c(333,347,518)] <- 2

loan_data$Dependents <- droplevels(loan_data$Dependents)
summary(loan_data$Dependents)
levels(loan_data$Dependents)

#     Self_Employed variable

summary(loan_data$Self_Employed)
selfemp_data <- loan_data %>% filter(Self_Employed != "") %>% group_by(Self_Employed) %>% summarize(n = n(), avg_income = mean(ApplicantIncome)) %>% mutate(percentage = round(n/sum(n),digits = 2))
selfemp_data

# we have 86% applicants who are not self_employed and 14% who are self_employed
# the average income for self_employed applicant is 7381 and for those who employed is 5050
# we have 32 applicants with no value for Self_Employed.

selfemp_index <- which(loan_data$Self_Employed == "")
selfemp_index

loan_data$ApplicantIncome[selfemp_index]

round(0.14 * length(selfemp_index))

# we choose 4 applicant out of 32 with income close to average income of the category.

loan_data$Self_Employed[c(96,108,334,433)] <- "Yes"

selfemp_index_no <-setdiff(selfemp_index,c(96,108,334,433))
selfemp_index_no

for (i in selfemp_index_no) {
  loan_data$Self_Employed[i] <- "No"
}

loan_data$Self_Employed <- droplevels(loan_data$Self_Employed)
summary(loan_data$Self_Employed)
levels(loan_data$Self_Employed)


#     LoanAmount variable

loan_data %>% filter(!is.na(LoanAmount)) %>% ggplot(aes(LoanAmount)) + geom_histogram(bins = 20) + ggtitle("LoanAmount distribution in dataset")
summary(loan_data$LoanAmount)

# the distribution is right skewed
# I will fill the NA values with median value.

median(loan_data$LoanAmount,na.rm = TRUE)
loan_data$LoanAmount[is.na(loan_data$LoanAmount)] <- median(loan_data$LoanAmount,na.rm = TRUE)


#     Loan_Amount_Term variable


loanamountterm_data <- loan_data %>% filter(!is.na(Loan_Amount_Term)) %>% group_by(Loan_Amount_Term) %>% summarize(n = n()) %>% mutate(percentage = round(n/sum(n),digits = 2))
loanamountterm_data

loan_data %>% filter(Loan_Amount_Term == 360) %>% ggplot(aes(LoanAmount)) + geom_histogram(bins = 20) + ggtitle("LoanAmount distribution in dataset for Loan_Amount_Term = 360 months")
loan_data %>% filter(Loan_Amount_Term == 180) %>% ggplot(aes(LoanAmount)) + geom_histogram(bins = 20) + ggtitle("LoanAmount distribution in dataset for Loan_Amount_Term = 180 months")
loan_data %>% filter(Loan_Amount_Term == 480) %>% ggplot(aes(LoanAmount)) + geom_histogram(bins = 20) + ggtitle("LoanAmount distribution in dataset for Loan_Amount_Term = 480 months")
loan_data %>% filter(Loan_Amount_Term == 300) %>% ggplot(aes(LoanAmount)) + geom_histogram(bins = 20) + ggtitle("LoanAmount distribution in dataset for Loan_Amount_Term = 300 months")
loan_data %>% filter(Loan_Amount_Term == 240) %>% ggplot(aes(LoanAmount)) + geom_histogram(bins = 20) + ggtitle("LoanAmount distribution in dataset for Loan_Amount_Term = 240 months")
loan_data %>% filter(Loan_Amount_Term == 84) %>% ggplot(aes(LoanAmount)) + geom_histogram(bins = 20) + ggtitle("LoanAmount distribution in dataset for Loan_Amount_Term = 84 months")

loanamountterm_is_na <- loan_data %>% filter(is.na(Loan_Amount_Term))
range(loanamountterm_is_na$LoanAmount)

# we can see from this analysis that we cannot draw a conclusion which will tell us the Loan_Amount_Term based on Loan_Amount
# I am going to fill the NA's based on the proportion of each category.

loan_data %>% filter(is.na(Loan_Amount_Term)) %>%nrow()

loanamountterm_index <- which(is.na(loan_data$Loan_Amount_Term))
loanamountterm_index

round(0.85 * 14)

loan_data$Loan_Amount_Term[c(20,37,45,46,74,113,166,198,224,233,336,368)] <- 360

round(0.07 * 14)

loan_data$Loan_Amount_Term[422] <- 180
loan_data$Loan_Amount_Term[424] <- 480

summary(loan_data$Loan_Amount_Term)
levels(loan_data$Loan_Amount_Term)

#     Credit_History variable

summary(loan_data$Credit_History)

credithistory_data <- loan_data %>% filter(!is.na(Credit_History)) %>% group_by(Credit_History) %>% summarize(n = n()) %>% mutate(percentage = round(n/sum(n),digits = 2))
credithistory_data

credithistory_index <- which(is.na(loan_data$Credit_History))
credithistory_index

round(0.16 * 50)

credithistory_index_no <- c(17,43,96,130,220,260,318,393)
loan_data$Credit_History[credithistory_index_no] <- 0

credithistory_index_yes <- setdiff(credithistory_index,credithistory_index_no)
credithistory_index_yes

for (i in credithistory_index_yes) {
  loan_data$Credit_History[i] <- 1
}

loan_data <- loan_data[,-1]
head(loan_data)

# dealing with outliers

# I use Tukey's definition of an outlier

#     removing outliers for ApplicantIncome

loan_data %>% ggplot(aes(ApplicantIncome)) + geom_histogram(bins = 20)
summary(loan_data$ApplicantIncome)
cutt_off <- 1.5 * IQR(loan_data$ApplicantIncome)
cutt_off
lower_value <- quantile(loan_data$ApplicantIncome,0.25) - cutt_off
higher_Value <- quantile(loan_data$ApplicantIncome,0.75) + cutt_off
lower_value
higher_Value

appinc_dropindex <- which(loan_data$ApplicantIncome > higher_Value)
appinc_dropindex

#dropping those observations with value bigger than higher_value

loan_data <- loan_data[-appinc_dropindex,]
loan_data %>% ggplot(aes(ApplicantIncome)) + geom_histogram(bins = 20)


#     removing outliers for CoapplicantIncome

loan_data %>% ggplot(aes(CoapplicantIncome)) + geom_histogram(bins = 20)
summary(loan_data$CoapplicantIncome)
cutt_off <- 1.5 * IQR(loan_data$CoapplicantIncome)
cutt_off
lower_value <- quantile(loan_data$CoapplicantIncome,0.25) - cutt_off
higher_Value <- quantile(loan_data$CoapplicantIncome,0.75) + cutt_off
lower_value
higher_Value

coappinc_dropindex <- which(loan_data$CoapplicantIncome > higher_Value)
coappinc_dropindex


#dropping those observations with value bigger than higher_value
loan_data <- loan_data[-coappinc_dropindex,]
loan_data %>% ggplot(aes(CoapplicantIncome)) + geom_histogram(bins = 20)

head(loan_data)
str(loan_data)


# grouping the ApplicantIncome and CoapplicantIncome

loan_data <- loan_data %>% mutate(TotalIncome = ApplicantIncome + CoapplicantIncome)
loan_data <- loan_data[,-c(6,7)]
loan_data <- loan_data[,c(1:9,11,10)]

# Exploratory Data Analysis


loan_data %>% ggplot(aes(Gender, fill = Loan_Status)) + geom_bar(position=position_dodge()) + ggtitle("Gender Distribution with Status of the Loan")
loan_data %>% ggplot(aes(Married, fill = Loan_Status)) + geom_bar(position=position_dodge()) + ggtitle("Married Distribution with Status of the Loan")
loan_data %>% ggplot(aes(Dependents, fill = Loan_Status)) + geom_bar(position=position_dodge()) + ggtitle("Dependents Distribution with Status of the Loan")
loan_data %>% ggplot(aes(Education, fill = Loan_Status)) + geom_bar(position=position_dodge()) + ggtitle("Education Distribution with Status of the Loan")
loan_data %>% ggplot(aes(Self_Employed, fill = Loan_Status)) + geom_bar(position=position_dodge()) + ggtitle("Self Employed Distribution with Status of the Loan")

loan_data %>% ggplot(aes(TotalIncome,LoanAmount, color = Loan_Status)) + geom_point() + ggtitle("Variation of LoanAmount with ApplicantIncome")

loan_data %>% ggplot(aes(Loan_Amount_Term, fill = Loan_Status)) + geom_bar(position=position_dodge()) + ggtitle("Loan Amount Term Distribution with Status of the Loan")
loan_data %>% ggplot(aes(Credit_History, fill = Loan_Status)) + geom_bar(position=position_dodge()) + ggtitle("Credit History Distribution with Status of the Loan")
loan_data %>% ggplot(aes(Property_Area, fill = Loan_Status)) + geom_bar(position=position_dodge()) + ggtitle("Property Area Distribution with Status of the Loan")



# scaling the data 

loan_data$TotalIncome <- (loan_data$TotalIncome - min(loan_data$TotalIncome)) / (max(loan_data$TotalIncome) - min(loan_data$TotalIncome))

# features encoding

loan_data$Gender <- ifelse(loan_data$Gender == "Male",1,0)
loan_data$Married <- ifelse(loan_data$Married == "Yes",1,0)


loan_data$Dependents <- as.integer(loan_data$Dependents)

indexs <- which(loan_data$Dependents == 1)
loan_data$Dependents[indexs] <- 0
indexs <- which(loan_data$Dependents == 2)
loan_data$Dependents[indexs] <- 1
indexs <- which(loan_data$Dependents == 3)
loan_data$Dependents[indexs] <- 2
indexs <- which(loan_data$Dependents == 4)
loan_data$Dependents[indexs] <- 3

loan_data$Education <- ifelse(loan_data$Education == "Graduate",1,0)
loan_data$Self_Employed <- ifelse(loan_data$Self_Employed == "Yes",1,0)
loan_data$LoanAmount <- (loan_data$LoanAmount - min(loan_data$LoanAmount)) / (max(loan_data$LoanAmount) - min(loan_data$LoanAmount))
loan_data$Loan_Amount_Term <- as.numeric(as.character(loan_data$Loan_Amount_Term))
loan_data$Loan_Amount_Term <- (loan_data$Loan_Amount_Term - min(loan_data$Loan_Amount_Term)) / (max(loan_data$Loan_Amount_Term) - min(loan_data$Loan_Amount_Term))
loan_data$Credit_History <- as.numeric(as.character(loan_data$Credit_History))

levels(loan_data$Property_Area)
loan_data$Property_Area <- as.character(loan_data$Property_Area)
indexs <- which(loan_data$Property_Area == "Rural")
loan_data$Property_Area[indexs] <- 0
indexs <- which(loan_data$Property_Area == "Semiurban")
loan_data$Property_Area[indexs] <- 1
indexs <- which(loan_data$Property_Area == "Urban")
loan_data$Property_Area[indexs] <- 2
loan_data$Property_Area <- as.numeric(loan_data$Property_Area)



# Modeling approach

head(loan_data)
str(loan_data)



# splitting the data in train data and test data
set.seed(1)
index <- createDataPartition(loan_data$Loan_Status,times = 1,p = 0.8, list = FALSE)
loan_data_train <- loan_data[index,]
loan_data_test <-loan_data[-index,]



control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(Loan_Status ~ ., method = "knn", data = loan_data_train, tuneGrid = data.frame(k = seq(9, 71, 2)),trControl = control)
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune
pred_LoanStatus_knn <- predict(train_knn,loan_data_test)
knn_accuracy <- confusionMatrix(pred_LoanStatus_knn,loan_data_test$Loan_Status)$overall["Accuracy"]

train_glm <- train(Loan_Status ~ ., method = "glm", data = loan_data_train)
pred_LoanStatus_glm <- predict(train_glm,loan_data_test)
glm_accuracy <- confusionMatrix(pred_LoanStatus_glm,loan_data_test$Loan_Status)$overall["Accuracy"]

train_qda <- train(Loan_Status ~ ., method = "qda", data = loan_data_train)
pred_LoanStatus_qda <- predict(train_qda,loan_data_test)
qda_accuracy <- confusionMatrix(pred_LoanStatus_qda,loan_data_test$Loan_Status)$overall["Accuracy"]

train_lda <- train(Loan_Status ~ ., method = "lda", data = loan_data_train)
pred_LoanStatus_lda <- predict(train_lda,loan_data_test)
lda_accuracy <- confusionMatrix(pred_LoanStatus_lda,loan_data_test$Loan_Status)$overall["Accuracy"]


train_rpart <- train(Loan_Status ~ ., method = "rpart",data = loan_data_train)
pred_LoanStatus_rpart <- predict(train_rpart,loan_data_test)
rpart_accuracy <- confusionMatrix(pred_LoanStatus_rpart,loan_data_test$Loan_Status)$overall["Accuracy"]


train_rf <- train(Loan_Status ~ ., method = "rf", data = loan_data_train)
ggplot(train_rf)
pred_LoanStatus_rf <- predict(train_rf,loan_data_test)
rf_accuracy <- confusionMatrix(pred_LoanStatus_rf,loan_data_test$Loan_Status)$overall["Accuracy"]

model <- rep(0,6)

accuracy <- rep(0,6)
accuracy_summary <- data.frame(model, accuracy)
accuracy_summary
accuracy_summary$model[1] <- "knn"
accuracy_summary$accuracy[1] <- round(knn_accuracy,4)
accuracy_summary$model[2] <- "glm"
accuracy_summary$accuracy[2] <- round(glm_accuracy,4)
accuracy_summary$model[3] <- "qda"
accuracy_summary$accuracy[3] <- round(qda_accuracy,4)
accuracy_summary$model[4] <- "lda"
accuracy_summary$accuracy[4] <- round(lda_accuracy,4)
accuracy_summary$model[5] <- "rpart"
accuracy_summary$accuracy[5] <- round(rpart_accuracy,4)
accuracy_summary$model[6] <- "rf"
accuracy_summary$accuracy[6] <- round(rf_accuracy,4)

accuracy_summary %>% ggplot(aes(x = model,y = accuracy)) +geom_bar(stat = "identity") + scale_y_continuous(limits=c(0.7,0.9),oob = rescale_none) + geom_text(aes(label = accuracy), vjust = -1)

