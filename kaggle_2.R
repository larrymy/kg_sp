library(data.table)
library(caret)
library(reshape); library(reshape2)
library(plyr); library(dplyr)

setwd('C:/Users/jy/Desktop/kaggle_sp')
source("gini_function.R")
source("clean_f.R")

train <- fread('train.csv') # target is event of interest
test <- fread('test.csv')

train_set <- clean_method_1(train)

system.time({
  test_set <- clean_method_1(test)
})
# set.seed(1)
# train_set <- upSample(x = train_set, y = as.factor(train_set$target)) %>%
#   sample_n(size = 10000) %>% as.data.table() %>% setorder(id)
t1 <- train_set[sample(1:nrow(train_set))[1:100000],]
t1 <- train_set
table(t1$target)
t1 <- subset( t1, select = -c(id) )
model <- glm(formula = target ~ ., family = binomial(link = "logit") , data = t1)

pred <- predict(model, train_set, type="response")
summary(pred)

actual <- train_set$target
nGini(actual, pred)

#predict test
pred_test <- predict(model, test_set, type="response")
summary(pred_test)

setwd('C:/Users/jy/Desktop/kaggle_sp')
  # save(model, file="lm_model.rda")
  # save(train_set, file="train_set.rda")
  # save(test_set, file="test_set.rda")
  # load(file="lm_model.rda")
  # load(file="test_set.rda")
  # load(file="train_set.rda")

  submission_df <- data.frame(id = test_set[,1], target = pred_test)
  write.csv(x = submission_df, file = "glm_binomial_submission.csv", row.names = F)

View(submission_df)
