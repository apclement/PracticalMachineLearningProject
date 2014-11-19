library(caret)
library(randomForest)

train <- read.csv('pml-training.csv')
test <- read.csv('pml-testing.csv')

nobs <- nrow(train)
# test all values to check it is NA or empty string
invalid.data <- is.na(train) | train == ''
# get the proportion of invalid values for each variable
inv.counts <- colSums(invalid.data)
inv.props <- colSums(invalid.data) / nobs
## keep only variables that have a proportion of invalid values below 40%
valid.vars <- inv.props <= .4
#valid.vars <- inv.counts == 0

#subset to keep only valid and non-empty variables
train <- train[, valid.vars]

#remove 7 first columns
train <- train[, -c(1:7)]

print(dim(train))

in.train <- createDataPartition(y = train$classe, p = .3, list = FALSE)
training <- train[in.train, ]
testing <- train[-in.train, ]

fit <- train(classe ~ ., data = training, method = "rf", 
            trControl = trainControl(method = 'cv', number = 4, allowParallel = T, verboseIter = TRUE))
print(fit)



