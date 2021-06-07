library(R6)
library(data.tree)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

computeC <- function(n) {
  return(2.0 * (log(n - 1) + 0.5772156649) - (2.0 * (n - 1) / n * 1.0))
}


IsolationForest = R6::R6Class(

  public = list(

    sample_size = NULL
    , num_trees = NULL
    , replace = NULL
    , seed = NULL
    , nproc = NULL
    , respect_unordered_factors = NULL
    , max_depth = NULL
    , forest = NULL
    , scores = NULL
    , status = "not_initialized"
    , trees = NULL
    , model = NULL
    ,
    initialize = function(sample_size = 256
                          , num_trees = 100
                          , replace = FALSE
                          , seed = 101
                          , nproc = NULL
                          , respect_unordered_factors = NULL
                          , max_depth = ceiling(log2(sample_size))
                          ) {
      self$sample_size = sample_size
      self$num_trees = num_trees
      self$replace = replace
      self$seed = seed
      self$nproc = nproc
      self$respect_unordered_factors = respect_unordered_factors
      self$max_depth = max_depth
      self$status = "not_trained"
    },


  fit = function(data, numberOfTrees = self$num_trees, subsetSize = self$sample_size) {
    self$status = "not_trained"
    maxTreeDepth = log(subsetSize, 2)
    if (subsetSize > nrow(data)) {
      subsetSize = nrow(data)
      warning("Subset size bigger than number of data rows, 
            changing subset size to number of data rows")
    }
    print(paste("[FIT] subsetSize: ", subsetSize))
    for (i in 1:numberOfTrees) {
      tree <- Node$new("tree")
      subSet <- data[sample(nrow(data), subsetSize),]
      #print(paste0("Building tree: ", i))
      self$buildTree(tree, subSet, maxTreeDepth, 0)
      ##print("children in building forest")
      ##print(tree$children)
      trees = append(trees, tree)
      # #print(trees)
    }
    model = structure(list(trees = trees), class = "isolationForestClass")
    # return(trees)
    # return(model)
    self$trees = trees
    self$model = model
    self$status = "trained"
  },

  buildTree = function(node, data, remainingDepth, actualDepth) {
    ##print(data[1:10,])
    node$obsCount <- nrow(data)
    ##print(paste0("remainingDepth: ", remainingDepth))
    if (node$obsCount == 0) {
      child <- node$AddChild('breakpoint')
      child$feature <- ' '
      child$obsCount <- 0
      child$depth <- actualDepth
      child$leaf <- 1

    }
    else if (node$obsCount == 1 | remainingDepth == 0) {
      #construct leaf when 1 object in node or max depth reached
      child <- node$AddChild('breakpoint')
      node$feature <- tail(names(data), 1)
      child$obsCount <- 1
      child$feature <- ''
      child$depth <- actualDepth
      child$leaf <- 1
    }

    else {

      numberOfFeatures <- length(names(data))
      # set.seed(123)
      whichFeature <- sample(1:numberOfFeatures, 1, replace = TRUE)
      feature <- names(data)[whichFeature]
      vector <- data[, whichFeature, drop = FALSE]
      if (var(vector, na.rm = TRUE) == 0) {
        #create leaf if for a given feature there is only 1 value
        child <- node$AddChild('same_values')
        node$feature <- tail(names(data), 1)
        child$obsCount <- nrow(data)
        child$feature <- ''
        child$depth <- actualDepth
        child$leaf <- 1
      }
      else {
        remainingDepth <- remainingDepth - 1
        actualDepth <- actualDepth + 1
        node$feature <- feature
        minValue <- min(vector, na.rm = TRUE)
        maxValue <- max(vector, na.rm = TRUE)
        # set.seed(123)
        splitPoint <- runif(1, min = minValue, max = maxValue)
        node$splitPoint <- splitPoint
        childObs <- split(data,
                          data[, feature] > splitPoint,
                          drop = TRUE)
        for (i in 1:length(childObs)) {
          #construct a child having the name of that feature value (e.g. 'red')
          name <- paste(feature, toString(splitPoint), "greater", names(childObs)[i], sep = " ")
          child <- node$AddChild(name)
          child$leaf <- 0
          self$buildTree(child, childObs[[i]], remainingDepth, actualDepth)
        }

      }
    }
  }
  ,

  pathLength = function(tree, row) {
    # print(tree$children)

    #############aaaaaaaaaah why this is null????????????????????????????
    if (tree$children[[1]]$leaf == 1) {
      
      if (tree$children[[1]]$obsCount == 1) {
        return(tree$children[[1]]$depth)
      } else {
        return(tree$children[[1]]$depth + computeC(tree$children[[1]]$obsCount))
      }

    }
    #print("searching tree")
    feature <- tree$feature
    splitPoint <- tree$splitPoint
    if (row[1, feature] > splitPoint) {
      name <- paste(feature, toString(splitPoint), "greater", "TRUE", sep = " ")
    }
    else {
      name <- paste(feature, toString(splitPoint), "greater", "FALSE", sep = " ")
    }
    child <- tree$children[[name]]
    return(self$pathLength(child, row))
  }
  ,
  predict = function(data, subsetSize = self$sample_size, score_factor_arg = 0.5) {
    if (subsetSize > nrow(data)) {
      subsetSize = nrow(data)
      warning("Subset size bigger than number of data rows, 
              changing subset size to number of data rows")
    }

    predictions = list()
    n = subsetSize
    c_const = computeC(n)
    score_factor = score_factor_arg
    
    print(paste("score factor=", score_factor, " , subsetSize = ", n))
    
    for (i in 1:nrow(data)) {
      averageDepth <- 0
      for (j in 4:length(self$trees)) {
        depth <- self$pathLength(self$trees[[j]], data[i,])
        averageDepth <- averageDepth + depth
      }
      averageDepth <- averageDepth / (length(self$trees) - 3)
      
      s_score <- 2 ^ (-averageDepth / c_const)

      #print(s_score)
      if (s_score > score_factor) {
        tmpPrediction = 1
      } else {
        tmpPrediction = 0
      }
      # predictions <- append(predictions, 2^(-averageDepth/c_const))
      predictions <- append(predictions, tmpPrediction)
    }

    return(unlist(predictions))
  }

)
)