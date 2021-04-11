library(R6)
# setClass("IsolationForest", slots = list(trees_num = "numeric", id = "numeric", test = "numeric"))

# setMethod("show",
# "IsolationForest",
# function(object) {
#   cat("trees_num:", object@trees_num, "\n")
#   cat("Id:", object@id, "\n")
#   cat("Contact:", object@test, "\n")
# }
# )
# setMethod("testMethodIFR",
# "IsolationForest",
# function(object) {
#   cat("trees_num:", object@trees_num, "\n")
#   cat("Id:", object@id, "\n")
#   cat("Contact:", object@test, "\n")
# }
# )






IsolationForest <- R6::R6Class(
  classname = "isolationForest",

    public = list(
      num_of_trees = NULL,
      subsampling_size = NULL,
      max_depth = NULL,
      forest = NULL,
      scores = NULL,
      status = "uninitialized",

      initialize = function(num_of_trees = 100, subsampling_size = 256, num_seed = 100, max_depth = ceiling(log2(subsampling_size))) {



        self$subsampling_size = subsampling_size
        self$num_of_trees = num_of_trees
        # self$num_seed = num_seed
        self$max_depth = max_depth
        self$status = "not_trained"
      },

      getScores = function(data) {

        print("getting scores")
      },

      getiForest = function(data) {

        print("creating iForest")
      },

      getiTrees = function(data) {

        print("creating iTrees")
      }



    )

)

