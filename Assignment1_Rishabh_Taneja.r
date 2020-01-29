#1 Question
#a) funtion to perform arg1 exp arg2
exponentional_function <- function(a,b){
  return(a^b)
}

exponentional_function(2,3)

#2 Question
#a) Load the dataset into R

#using read.table to separate all the data into individual columns
salary_data <- read.table("https://data.princeton.edu/wws509/datasets/salary.dat", header=TRUE)
set.seed(123)
head(salary_data)

Splitting_data <- sample(x=1:nrow(salary_data), size=0.80*nrow(salary_data))
train_data = salary_data[Splitting_data,]
test_data = salary_data[-Splitting_data,]
fitting <- lm(sl~factor(sx)+factor(rk)+yr+factor(dg) + yd, data=train_data)
predict_train <- data.frame(predict(fitting, newdata = train_data))
summary(fitting)
par(mfrow=c(3,2))
plot(fitting)

predicted_value <- predict(fitting, newdata = test_data)
predicted_value
observed_value <- test_data$sl
observed_value
SSE <- sum((observed_value - predicted_value) ^ 2)
SSE
SST <- sum((observed_value - mean(observed_value)) ^ 2)
SST
R2 <- 1 - SSE/SST
R2

RMSE <- sqrt(mean((predicted_value - observed_value)**2))
RMSE
MAE <- mean(abs(observed_value - predicted_value))
MAE
RSS <- sum((predicted_value - observed_value)^2)
RSS

#3 

sales <- read.csv("http://ucanalytics.com/blogs/wp-content/uploads/2017/09/Data-L-Reg-Gradient-Descent.csv")
sales$X1plusX2 <- NULL
var_names <- c("expenditure", "income", "purchase")
names(sales) <- var_names
#Standardize predictors (X1, X2) 
sales$expenditure <- scale(sales$expenditure, scale=TRUE, center = TRUE)
sales$income <- scale(sales$income, scale=TRUE, center = TRUE)

set.seed(5689) 
sales_split <- sample(x=1:nrow(sales), size=0.75*nrow(sales))
train_dataset = sales[sales_split,]
test_dataset = sales[-sales_split,]

gen_linear_model <- glm(purchase~expenditure+income, family=binomial(link='logit') , data=train_dataset)
summary(gen_linear_model)
par(mfrow=c(3,2))
plot(gen_linear_model)

pred_value <- predict(gen_linear_model,train_dataset,type = "response")
train_value <- ifelse(pred_value > 0.5, 1,0)

train_value_table <- table(predicted=train_value, actual = train_dataset$purchase)
train_value_table

test_value <- predict(gen_linear_model,test_dataset,type = "response")
test_y <- ifelse(test_value > 0.5, 1,0)
test_y <- data.frame(test_y)
test_y <- test_y[1:100,]

test_table <- table(predicted=test_y, actual = test_dataset$purchase)
test_table

error_calculation <- function(actual,predicted){
  mean(actual!= predicted)
}

error_calculation(actual = train_dataset$purchase, predicted = train_value)
error_calculation(actual = test_dataset$purchase, predicted = test_y)

library(ROCR)
predicting_values <- prediction(test_y,test_dataset$purchase)
performance_value <- performance(predicting_values,measure = "tpr", x.measure = "fpr")
plot(performance_value) 
auc(test_dataset$purchase,test_y)

#4 question ##

sales <- read.csv("http://ucanalytics.com/blogs/wp-content/uploads/2017/09/Data-L-Reg-Gradient-Descent.csv")
sales$X1plusX2 <- NULL
var_names <- c("expenditure", "income", "purchase")
names(sales) <- var_names

sales$expenditure <- scale(sales$expenditure, scale=TRUE, center = TRUE)
sales$income <- scale(sales$income, scale=TRUE, center = TRUE)

#Predictor variables matrix
X <- as.matrix(sales[,c(1,2)])

#Add ones to Predictor variables matrix (for intercept, B0)
X <- cbind(rep(1,nrow(X)),X)

#Response variable matrix
Y <- as.matrix(sales$purchase)

logl <- function(theta,x,y){
  y <- y
  x <- as.matrix(x)
  beta <- theta[1:ncol(x)]
  loglik <-  sum(-y*log(1 + exp(-(x%*%beta))) - (1-y)*log(1 + exp(x%*%beta)))
  return(-loglik)
}
theta.start = rep(0,3)
names(theta.start) = colnames(X)
mle = optim(theta.start,logl,x=X,y=Y,hessian=T)
out = list(beta=mle$par,se=diag(sqrt(solve(mle$hessian))),ll=2*mle$value)
#Get parameter values from out object and beta attribute
#[call "out" object and "beta" attribute, separated by $, as in object$attribute]

betaVal <- out$beta
print(out)

sigmoid <- function(z){
  g <- 1/(1+exp(-z))
  return(g)
}
#Cost Function
#Create user-defined Loss Function specific to logistic regression gradient descent
#[finish the equation for J - the logistic regression cost/loss function]

logistic_cost_function <- function(beta){
  m <- nrow(X)
  g <- sigmoid(X%*%beta)
  J <-  (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
  return(J)
}

#Gradient Descent Function
#Create user-defined Gradient Descent Function specific to Logistic Regression
#[finish the equation for Beta - the updated weight (beta-alpha*delta)]

logistic_gradient_descent <- function(alpha, iterations, beta, x, y){
  for (i in 1:iterations) {
    error <- (X %*% beta - y)
    delta <- (t(X) %*% error) / length(Y)
    beta <- beta - alpha * delta
  }
  return(list(parameters=beta))
}


#Set initial parameters for gradient descent

# Define learning rate and iteration limit
initial_alpha <- 0.01 #learning rate
num_iterations <- 10000 #number of times we'll run the loop in the function
empty_beta <- matrix(c(0,0,0), nrow=3) # initialized parameters (matrix of 0s)


#Run logistic regression gradient descent to find beta parameters
#[fill in all of the arguments for the user-defined logistic gradient descent function]
output <- logistic_gradient_descent(alpha = initial_alpha, iterations = num_iterations, beta = empty_beta, x = X, y = Y)

#Get final estimated parameters from our output object:
#[call object "output" and stored attribute "parameters", separated by a $, as in object$attribute]
print(output)

#End of Logistic Regression via Gradient Descent Code Outline

#4d
sales <- read.csv("http://ucanalytics.com/blogs/wp-content/uploads/2017/09/Data-L-Reg-Gradient-Descent.csv")
variable_names <- c("expenditure", "income", "purchase")
names(sales) <- variable_names
#Standardize predictors (X1, X2) 
sales$expenditure <- scale(sales$expenditure, scale=TRUE, center = TRUE)
sales$income <- scale(sales$income, scale=TRUE, center = TRUE)

model <- glm(sales$purchase~sales$income+sales$expenditure,data=sales,family=binomial())
summary(model) # display results
confint(model) # 95% CI for the coefficients
exp(coef(model)) # exponentiated coefficients
exp(confint(model)) # 95% CI for exponentiated coefficients
predict(model, type="response") # predicted values
residuals(model, type="deviance") # residuals
par(mfrow=c(3,2))
plot(model)
