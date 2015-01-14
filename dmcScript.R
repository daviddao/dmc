# Business Analytics
# Data Mining Cup Template

# The caret package is used (http://topepo.github.io/caret/index.html)
#install.packages("caret")
library(caret)
install.packages('party')
library(party)


# For reasons of traceability you must use a fixed seed
set.seed(42) # do NOT CHANGE this seed


######################################################
# 1. Build a Team in the DMC Manager
# https://dmc.dss.in.tum.de/dmc/
# Login with TUM login data ("TUM-Kennung")
#
# Found or join a team (size: 1-4 students)


######################################################
# 2. Load & Explore the Training Data Set
training_data = read.csv("training.csv", sep=",")

# Explore the data set...


######################################################
# 3. Data Preparation
# (using both training and test data)
# do NOT DELETE any instances in the test data
test_data = read.csv("test.csv", sep=",")

# Prepare the data for training...


######################################################
# 4. Training & Evaluation

# Train a model "model"...
model <- cforest(as.factor(income) ~ id + age + workclass + rating + education + edu + family_status + occupation + origin + gender,
               data = training_data, controls=cforest_unbiased(ntree=10, mtry=3))

######################################################
# 5. Predict Classes in Test Data
predictions <- predict(model, training_data, OOB=TRUE, type="response")

prediction_classes = predict.train(object=model, newdata=test_data, na.action=na.pass)
predictions = data.frame(id=test_data$id, prediction=prediction_classes)
predictions


######################################################
# 6. Export the Predictions
write.csv(predictions, file="predictions_group_name_number.csv", row.names=FALSE)


######################################################
# 7. Upload the Predictions and the Corresponding R Script on DMC Manager
# https://dmc.dss.in.tum.de/dmc/
# Login with TUM login data ("TUM-Kennung")
#
# Maxium number of submissions: 10
#
# Possible errors that could occur:
# - Wrong column names
# - Unknown IDs (if not in Test Data)
# - Missing IDs (if in Test Data but not in Predictions)
# - Wrong file format