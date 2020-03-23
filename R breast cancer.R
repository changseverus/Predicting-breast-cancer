# import data from AzureML

library("AzureML")
ws <- workspace()
dat <- download.datasets(ws, "Breast cancer data")

# explore data
str(dat)
head(dat)


# install corrgram package to plot the correlation matrix

if(!require("corrgram", quietly = TRUE)) install.packages("corrgram")
library(corrgram, quietly = TRUE)
corrgram(dat, order = TRUE, 
         lower.panel = panel.ellipse,
         upper.panel = panel.shade, 
         text.panel = panel.txt,
         main = "Breast cancer data in PC2/PC1 Order",
         cex.labels = 0.7)


# create a binary classifier model
# separate training and test sample 

set.seed(1)
idx <- sample.int(nrow(dat), nrow(dat) * 0.8) # create an 80% sample index
train <- dat[idx, ]  # keep the 80% sample
test  <- dat[-idx, ] # discard the 80% sample

# fit the model using logistic regression
model <- glm(Class ~ ., data = dat, family = binomial)

summary(model)



# install ROCR package to visualize model accuracy

if(!require(ROCR, quietly = TRUE)) install.packages("ROCR")
library(ROCR, quietly = TRUE)

# create predictions using the test set data
predictions <- predict(model, test, type = "response")

# Using ROCR functions to produce a simple ROC plot:
pred <- prediction(predictions, test$Class)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")     

options(repr.plot.width=5,repr.plot.height=3)
plot(perf,col=rainbow(10),main="Model Performance")
