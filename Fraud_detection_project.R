#glancing at the structure of the dataset
str(creditcard)

#converting class to a factor variable
creditcard$Class <- factor(creditcard$Class, levels = c(0,1))

#get the summary of the data
summary(creditcard)

#count the missing values
sum(is.na(creditcard))


#get the distribution of different transactions, fraud and legit in the dataset
table(creditcard$Class)

#get the percentage of the transactions
prop.table(table(creditcard$Class ))

#pie chart of the card transactions
labels <- c("legit", "fraud")
labels <- paste(labels, round(100 * prop.table(table(creditcard$Class)), 2))
labels <- paste0(labels, "%")

pie(table(creditcard$Class), labels, col = c("orange", "red"), 
    main = "Pie chart of the transactions")


#no model predictions
predictions <- rep.int(0, nrow(creditcard))
predictions <- factor(predictions, levels = c(0,1))

#install.packages('caret')
library(caret)
confusionMatrix(data = predictions, reference = creditcard$Class)
#-------------------------------------------------------------------------------
#small subset of the data to build the model
library(dplyr)

set.seed(1)
#random fraction of the datatset
creditcard <- creditcard %>% sample_frac(0.1)

table(creditcard$Class)

library(ggplot2)

str(creditcard)

ggplot(data = creditcard, aes(x = V1, y = V2, col = Class)) + 
  geom_point() +
  theme_bw() +
  scale_color_manual(values = c('pink', 'black'))
#-------------------------------------------------------------------------------
#creating training and test sets for the model

install.packages('caTools')
library(caTools)

set.seed(123)
data_sample = sample.split(creditcard$Class,SplitRatio = 0.80)

train_data = subset(creditcard,data_sample==TRUE)

test_data = subset(creditcard,data_sample==FALSE)

#-------------------------------------------------------------------------------

#balancing the data using Random Over Sampling

table(train_data$Class)

n_legit <- 22750
new_frac_legit <- 0.50
new_n_total <- n_legit/new_frac_legit

#install.packages('ROSE')
library(ROSE)
oversampling_result <- ovun.sample(Class ~ .,
                                   data = train_data,
                                   method = "over",
                                   N = new_n_total,
                                   seed = 2019)
oversampled_credit <- oversampling_result$data

table(oversampled_credit$Class)

#ggplot(data = oversampled_credit, aes(x = V1, y = V2, col = CLass))

ggplot(data = oversampled_credit, aes(x = V1, y = V2, col = Class)) + 
  geom_point(position = position_jitter(width = 0.2)) +
  theme_bw() +
  scale_color_manual(values = c('dodgerblue', 'red'))

#-------------------------------------------------------------------------------
#Random Under Sampling

table(train_data$Class)
n_fraud <- 35
new_frac_fraud <- 0.50
new_n_total <- n_fraud/new_frac_fraud

library(ROSE)
undersampling_result <- ovun.sample(Class ~ .,
                                    data = train_data,
                                    method = "under",
                                    seed = 2019)
undersampled_credit <- undersampling_result$data

table(undersampled_credit$Class)

ggplot(data = undersampled_credit, aes(x = V1, y = V2, col = Class)) + 
  geom_point() +
  theme_bw() +
  scale_color_manual(values = c('dodgerblue', 'red'))

#-------------------------------------------------------------------------------
#ROS and RUS

n_new <- nrow(train_data)
fraction_fraud_new <- 0.50

sampling_result <- ovun.sample(Class ~ .,
                               data = train_data,
                               method = "both" ,
                               N = n_new,
                               p = fraction_fraud_new,
                               seed = 2019)

sampled_credit <- sampling_result$data

table(sampled_credit$Class)

prop.table(table(sampled_credit$Class))

ggplot(data = sampled_credit, aes(x = V1, y = V2, col = Class)) + 
  geom_point(position = position_jitter(width = 0.2)) +
  theme_bw() +
  scale_color_manual(values = c('dodgerblue', 'red'))

#-------------------------------------------------------------------------------
#Balancing the data set using SMOTE

#install.packages("smotefamily")
library(smotefamily)

table(train_data$Class)

#set the number of fraud and legitimate cases and the desired percentage

n0 <- 22750
n1 <- 35
#adding synthetic samples
r0 <- 0.6


#calculate the value if the dup_size parameter of SMOTE
ntimes <-((1-r0)/r0) * (n0 / n1) - 1

smote_output = SMOTE(X = train_data[ , -c(1, 31)],
                     target = train_data$Class,
                     K = 5,
                     dup_size = ntimes)

credit_smote <- smote_output$data
  
colnames(credit_smote) [30] <- "Class"

prop.table(table(credit_smote$Class))

#Class distribution of the original data
ggplot(data = train_data, aes(x = V1, y = V2, col = Class)) + 
  geom_point() +
  scale_color_manual(values = c('dodgerblue', 'red'))

#Class distribution of over sampled data set using SMOTE
ggplot(data = credit_smote, aes(x = V1, y = V2, col = Class)) + 
  geom_point() +
  scale_color_manual(values = c('dodgerblue', 'red'))

#-------------------------------------------------------------------------------
#install.packages('rpart')
#install.packages('rpart.plot')

library(rpart)
library(rpart.plot)

CART_model <- rpart(Class ~ . , credit_smote)

rpart.plot(CART_model, extra = 0, type = 5, tweak=1.2)

#predicting fraud classes
predicted_val <- predict(CART_model, test_data, type = 'class')

#Building confusion matrix
library(caret)
confusionMatrix(predicted_val, test_data$Class)

#-------------------------------------------------------------------------------

predicted_val <- predict(CART_model, creditcard[-1], type = 'class')
confusionMatrix(predicted_val, creditcard$Class)





