require(xgboost)
require(caret)
require(MASS)

# Load data
data = read.csv("Documents/practice/datathon/training.csv")



#data$bedbath_ratio <- round(log(data$Bedroom/data$BathFloor),3)
#data$bedbath_ratio[data$bedbath_ratio==-Inf] <- -1
#data$bedbath_ratio[data$bedbath_ratio==Inf] <- 3


# Train test split
smp_size = floor(0.75 * nrow(data))

set.seed(123)
train_ind = sample(seq_len(nrow(data)),size = smp_size)

train <- data[train_ind,]
test <- data[-train_ind,]

train_y = train['Price']
test_y = test['Price']

plot(train$Price)

train_X = subset(train, select = -c(3,1,2))
test_X = subset(test, select = -c(3,1,2))

# Training
lmod_log <- lm(log(train_y$Price) ~  .,data = train_X)
ypred = predict(lmod_log,test_X)
sqrt(mean(((2.718)^ypred-test$Price)^2)) # 488.4175


library("leaps")
sout <- summary(regsubsets(log(train$Price) ~.,data=train,nbest=1,really.big=T))
n <- length(train$Price)
p <- apply(sout$which, 1, sum)
aic <- sout$bic - log(n) * p + 2 * p
plot(p-1, aic, ylab = "AIC",xlab = "Predictors")
ibest <- seq(along = aic)[aic == min(aic)]
foo <- sout$which[ibest, ]
form <- names(foo)[foo][-1]
form <- paste(form, collapse = " + ")
form <- paste("price ~", form)
form
sout <- regsubsets(log(price) ~., data=df,nbest=1,nvmax=19)
## A graphical way of looking at these models:
plot(sout, scale="bic")

