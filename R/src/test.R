source("R/src/IsolationForest.R")
# install.packages("data.tree")
library(ggplot2)
# library(data.tree)

# get_trees <- getiTrees(2,12,2)

# ifr <- IsolationForest$new()
# # ifr
# ifr$getiForest()
# ifr$getiTrees()
# ifr$getScores()
# just to check 


n = 1000
Var1 = c(rnorm(n, 0, 0.5), rnorm(n*0.1, -2, 1))
Var2 = c(rnorm(n, 0, 0.5), rnorm(n*0.1,  2, 1))
outliers = c(rep(0, n), rep(1, (0.1*n))) + 3
data = data.frame(Var1, Var2)

#plot data
ggplot(data, aes(x = Var1, y = Var2)) + 
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "x", y = "y") +
  labs(alpha = "", colour="Legend")

#create isolation forest using isolationForest function from solitude package with default parameters

iforest <- IsolationForest$new()
# iforest <- isolationForest(data,50)

#predict outliers within dataset
data$fit <- iforest$fit(data, 10)
data$pred <- iforest$predict(data)
print(data$pred)
data$outlier <- as.factor(ifelse(data$pred >=0.55, "outlier", "normal"))

#plot data again with outliers identified
ggplot(data, aes(x = Var1, y = Var2, color = outlier)) + 
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "x", y = "y") +
  labs(alpha = "", colour="Legend")
