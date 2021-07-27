################## Generalized Linear Model (GLM) ################## ----
# Import the clothing_sales_training and clothing_sales_test data sets as sales_train
# and sales_test, respectively
sales_train <- read.csv(file = "/Users/antran/Google Drive/USD_Teaching/ADS-502/WebsiteDataSets/clothing_sales_training.csv")

# To run the logistic regression model, we will use the glm() command
logreg01 <- glm(formula = CC ~ Days + Web,
                data = sales_train,
                family = binomial) # binomial: logistic regression

# view the summary of the model
summary(logreg01)

# predict using trained model. type='response' to get back probabilities
sales_train$pred_prob <- predict(object = logreg01, newdata = sales_train, type='response')
head(sales_train$pred_prob)

# convert probabilities to binary classes with default threshold 0.5
sales_train$pred <- (sales_train$pred_prob > 0.5)*1
head(sales_train$pred)

# performance metrics
library(caret)
sales_train[c('CC', 'pred')] <- lapply(sales_train[c('CC', 'pred')], as.factor)
confusionMatrix(sales_train$pred, sales_train$CC, positive='1')
                
# Poisson Regression ----
# now build a Poisson regression model
sales_train <- read.csv(file = "/Users/antran/Google Drive/USD_Teaching/ADS-502/WebsiteDataSets/clothing_sales_training.csv")
poisreg01 <- glm(formula = CC ~ Days + Web,
                 data = sales_train,
                 family = poisson)

# view details about the model
summary(poisreg01)



################## Neural Networks ################### ----
# Perform min-max standardization on the Days variable
sales_train$Days.mm <- (sales_train$Days - min(sales_train$Days)) / (max(sales_train$Days) - min(sales_train$Days))

# Install the nnet and NeuralNetTools packages, and open both packages
#install.packages('nnet')
#install.packages('NeuralNetTools')
library(nnet)
library(NeuralNetTools)

# Run the neural network algorithm
nnet01 <- nnet(CC ~ Web + Days.mm,
               data = sales_train,
               maxit = 100, # number of iterations
               size = 16) # number of nodes in the hidden layer

# plot the neural network
plotnet(nnet01)

# make predictions (returns probabilities)
sales_train$pred_prob <- predict(object = nnet01, newdata = sales_train)

# convert to classes
sales_train$pred <- (sales_train$pred_prob > 0.5)*1

# performance metrics
sales_train[c('CC', 'pred')] <- lapply(sales_train[c('CC', 'pred')], as.factor)
confusionMatrix(sales_train$pred, sales_train$CC, positive='1')



