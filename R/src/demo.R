#import packages
# install.packages("ggplot2")
# install.packages("solitude")
library(ggplot2)
library(solitude)

#create and plot sample data

#create data
n = 1000
Var1 = c(rnorm(n, 0, 0.5), rnorm(n * 0.1, -2, 1))
Var2 = c(rnorm(n, 0, 0.5), rnorm(n * 0.1, 2, 1))
outliers = c(rep(0, n), rep(1, (0.1 * n))) + 3
data = data.frame(Var1, Var2)

#plot data
ggplot(data, aes(x = Var1, y = Var2)) +
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "x", y = "y") +
  labs(alpha = "", colour = "Legend")

#create isolation forest using isolationForest function from solitude package with default parameters
iforest<- isolationForest$new()

iforest$fit(data)
#print(iforest$scores)
#predict outliers within dataset
data$pred <- iforest$predict(data)
data$outlier <- as.factor(ifelse(data$pred$anomaly_score >=0.60, "outlier", "normal"))

#plot data again with outliers identified
ggplot(data, aes(x = Var1, y = Var2, color = outlier)) +
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "x", y = "y") +
  labs(alpha = "", colour = "Legend")


