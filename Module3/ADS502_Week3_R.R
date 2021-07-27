################### Partition the Data ################## ----
# Read in the bank-additional data set as bank. Next, we need to set the 'seed' for the
# random number generator we will use later on in this section
bank <- read.csv(file = "/Users/antran/Google Drive/USD_Teaching/ADS-502/WebsiteDataSets/bank-additional.csv", sep=';')
set.seed(7)

# identify how many records are in the data set
n <- dim(bank)[1]

# for simplicity, only select a few variables
bank = bank[, c('job', 'marital', 'housing', 'loan', 'y')]

# determine which records will be in the training data set via 
# a random number generator
train_ind <- runif(n) < 0.75

# Now that we have a series of TRUE and FALSE values, we will use them to
# create the training and test data sets
bank_train <- bank[ train_ind, ]
bank_test <- bank[ !train_ind, ]

# Balance the Training Data Set ----
# find out how many records in the bank_train data set have a response
# value of 'yes'
table(bank_train$y)

# there are 2767 records in the training data set and 336 of them have
# 'yes' response values. This gives us about 11% of the training data
# set having 'yes' responses. We need to resample ~840 records whose
# response is 'yes' and add them to our training data set
# First, we identify the record indices we want to resample using which()
to.resample <- which(bank_train$y == "yes")

# Next, we randomly sample from the values in to.resample
our.resample <- sample(x = to.resample, size = 840, replace = TRUE)

# Now we want to get the records whose record numbers are those in our.resample
our.resample <- bank_train[our.resample, ]

# Finally, add the resampled records back onto our original training data set
train_bank_rebal <- rbind(bank_train, our.resample)

# To confirm that the resampling has given the desired amount of rare records,
# look at the table of response values in our rebalanced data set
t1 <- table(train_bank_rebal$y)
ratio <- t1[2] / sum(t1) * 100
ratio

################ Naive Bayes ################### ----
# install.packages("e1071")
library(e1071)

# Need to convert categorical variables to factors first
cols = c('job', 'marital', 'housing', 'loan', 'y')
train_bank_rebal[, cols] <- lapply(train_bank_rebal[, cols], as.factor)

# Train the Naive Bayes estimator
nb <- naiveBayes(formula = y ~ job + marital + housing + loan,
                 data = train_bank_rebal)

# Process the test set the same way as training set
bank_test[, cols] <- lapply(bank_test[, cols], as.factor)

# make predictions
predictions <- predict(nb, bank_test)

################### model evaluation #################### ----
# install.packages('caret', dependencies = T)
library(caret)
confusionMatrix(predictions, bank_test[, 'y'], positive='yes')
