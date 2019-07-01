setwd("/Users/jlee/Desktop")
library(dplyr)
library(sqldf)
library(lubridate)

#A) Assemble a dataframe with one row per customer and the following columns:
#* customer_id
#* gender
#* most_recent_order_date
#* order_count (number of orders placed by this customer)
#Sort the dataframe by customer_id ascending and display the first 10 rows.

orders <- read.csv('screening_exercise_orders_v201810.csv')

#There are a few ways to do this, but as I am familiar with SQL I use sqldf package in this case
orders_a <- 
sqldf("SELECT 
        customer_id, 
        gender, 
        MIN(date) AS most_recent_order_date, 
        count(customer_id) AS order_count
      FROM orders
      GROUP BY customer_id
      ORDER BY customer_id ASC")

head(orders_a,10)

#B) Plot the count of orders per week for the store.

#Use lubridate to add a week column
orders_b <- transform(orders, Week = isoweek(date))

#You can create a new dataframe with that info (I like to visualize my data before plotting)
orders_b <- 
sqldf("SELECT 
  week, 
  count(customer_id) AS freq
FROM orders_b 
GROUP BY Week")

plot(orders_b$Week,orders_b$freq, type='p', main='Count of Orders Per Week', xlab='Weeks', ylab='Frequency')

#C) Compute the mean order value for gender 0 and for gender 1. Do you think the difference is significant?

#Mean Order Value = Revenue / # of Orders
orders_c <- 
sqldf("SELECT 
      gender, 
      sum(value) / count(customer_id) AS mean_order_value
      FROM orders
      GROUP BY gender")

orders_c

orders_1 <- sqldf("SELECT 
      customer_id,
      sum(value) / count(customer_id) AS mean_order_value
      FROM orders
      WHERE gender = 1
      GROUP BY customer_id")

mean(orders_1$mean_order_value)
mean(orders_0$mean_order_value)

orders_0 <- sqldf("SELECT
      customer_id,
      sum(value) / count(customer_id) AS mean_order_value
      FROM orders
      WHERE gender = 0
      GROUP BY customer_id")

t.test(orders_1$mean_order_value,orders_0$mean_order_value)

#The mean order values are not significantly different (3% difference) in comparison. 

#D) Assuming a single gender prediction was made for each customer, generate a confusion matrix for predicted gender. What does the confusion matrix tell you about the quality of the predictions?

#Convert gender to factors so that both columns can be compared 1 to 1
orders$gender <- as.factor(orders$gender)
orders$predicted_gender <- as.factor(orders$predicted_gender)

#Generate a Confusion Matrix
confusionMatrix(orders$gender,orders$predicted_gender)

#The accuracy of the prediction is 63.8% and there may be room to create a better model to create a better set of predictions