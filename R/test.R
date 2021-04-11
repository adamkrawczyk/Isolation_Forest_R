source("IsolationForest.R")

# get_trees <- getiTrees(2,12,2)

ifr <- IsolationForest$new()
# ifr
ifr$getiForest()
ifr$getiTrees()
ifr$getScores()