################ Simple EDA ----
# Read in the bank_marketing_training data set as bank_train
bank_train <- read.csv(file = "C:/Users/DDY/Desktop/2021-Spring-textbooks/ADS-502/Module2/Website Data Sets/bank_marketing_training.csv")

# install the package using install.packages(), 
# then open it each time you write new code using library()
# install.packages("ggplot2")
library(ggplot2)

# create a bar chart of "previous_outcome" with an overlay of "response" using fill argument
ggplot(bank_train, aes(previous_outcome)) +
    geom_bar(aes(fill = response))

# normalize the bar chart, add position = 'fill' inside geom_bar()
ggplot(bank_train, aes(previous_outcome)) +
    geom_bar(aes(fill = response), position = "fill")

# Construct Histograms with Overlay ----
# To add an overlay to the histogram using the target variable, the aes(fill =
# response) input is added to geom_histogram()
ggplot(bank_train, aes(age)) +
    geom_histogram(aes(fill =response), color="black")

# To normalize the histogram, the position = 'fill' input is added to
# geom_histogram()
ggplot(bank_train, aes(age)) +
    geom_histogram(aes(fill = response), color="black", position = "fill")

# Binning Based on Predictive Value ----
# To create our categorical variable, we will use the cut() command on the age variable
bank_train$age_binned <- cut(x = bank_train$age, breaks = c(0, 25, 60, 100),
                             right = FALSE,
                             labels = c("Under 25", "25 to 60", "Over 60"))

# Plot it with an overlay of
# response using the ggplot commands covered previously
ggplot(bank_train, aes(age_binned)) +
    geom_bar(aes(fill = response))

# normalize
ggplot(bank_train, aes(age_binned)) +
    geom_bar(aes(fill = response), position = 'fill')



#################### Decision Trees ----
# Import the training data set and name it adult_tr. Once the data set is loaded into R,
# rename 'Marital status' to 'maritalStatus' to remove the space
adult_tr <- read.csv(file = "C:/Users/DDY/Desktop/2021-Spring-textbooks/ADS-502/Module2/Website Data Sets/adult_ch6_training")
colnames(adult_tr)[1] <- "maritalStatus"

# In R, we don't need to dummy code categorical variables,
# Because R has an datatype called "factor", which is treated as 
# a series of dummy encoded values under the hood.
# We just need to change type of the categorical variables to factors
adult_tr$Income <- factor(adult_tr$Income)
adult_tr$maritalStatus <- factor(adult_tr$maritalStatus)

# To run and visualize the Decision Tree model, we need to install and open the
# required packages, rpart and rpart.plot
install.packages(c("rpart", "rpart.plot"))
library(rpart)
library(rpart.plot)

# Finally, let us run the rpart() command to build the decision tree model
cart01 <- rpart(formula = Income ~ maritalStatus + Cap_Gains_Losses,
                data = adult_tr,
                method = "class")

# We can plot the decision tree model with the default
# display options using the rpart.plot command
rpart.plot(cart01)

# Note that the plot was built using the default settings of rpart.plot().
# Run ?rpart.plot to look at the different display options and parameters to be passed in
?rpart.plot






################ Multiple Regression ----
# Load the clothing_sales_training
# Next, make sure the categorical variables are factors
sales_train <- read.csv(file = "C:/Users/DDY/Desktop/2021-Spring-textbooks/ADS-502/Module2/Website Data Sets/clothing_sales_training.csv")

# convert categorical variables to factors
sales_train$CC <- as.factor(sales_train$CC)
sales_train$Web <- as.factor(sales_train$Web)

# run the model for the training data set
model01 <- lm(formula = Sales.per.Visit ~ Days + Web + CC,
              data = sales_train)

# view a summary of the model results
summary(model01)

# Mean Absolute Error of the model
install.packages('MLmetrics', dependencies=T)
library(MLmetrics)
predictions <- predict(object=model01, newdata=sales_train[, 1:3])
MAE(y_pred=predictions, y_true=sales_train$Sales.per.Visit)

