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




# isolationForest = R6::R6Class(
#   "isolationForest"
#   ,
# # Implementation:
# # 'solitude' class implements the isolation forest method defined by
# # Liu, Fei Tony, Ting, Kai Ming and Zhou, Zhi-Hua.
# # "Isolation-based anomaly detection." ACM Transactions on Knowledge Discovery   # from Data (T KDD). <doi:10.1145/2133360.2133363>
# #
# # Plan:
# # 1. Build 'extratrees' forest using ranger.
# # 2. Obtain terminal node depths by parsing each tree.
# # 3. Compute anomaly scores.
# #
# # Design:
# # 1. 'initialize' method sets the params required for forest growth.
# # 2. Calling 'fit' on some dataset does these things:
# #    - Grow 'extratrees' forest.
# #    - Compute and store the depth of each terminal node in every tree.
# #    - Compute depth or pathlength for each observation.
# #    - Compute anomaly score for each observation and store it in 'scores'.
# # 3. 'predict' method computes anomaly scores on new data.
# #
#   public = list(

#     sample_size = NULL
#     , num_trees = NULL
#     , replace = NULL
#     , seed = NULL
#     , nproc = NULL
#     , respect_unordered_factors = NULL
#     , max_depth = NULL
#     , forest = NULL
#     , scores = NULL
#     , status = "not_initialized"
#     ,
# # intialize arguments required for fitting extratrees via ranger
#     initialize = function(sample_size = 256
#                           , num_trees = 100
#                           , replace = FALSE
#                           , seed = 101
#                           , nproc = NULL
#                           , respect_unordered_factors = NULL
#                           , max_depth = ceiling(log2(sample_size))
#                           ) {

#       stopifnot(is_integerish(sample_size) && length(sample_size) == 1)
#       stopifnot(0 < sample_size)
#       stopifnot(is_integerish(num_trees) && length(num_trees) == 1)
#       stopifnot(0 < num_trees)
#       stopifnot(is.logical(replace) && length(replace) == 1)
#       stopifnot(is_integerish(seed) && length(seed) == 1)
#       stopifnot(is.null(nproc) ||
#                   (is_integerish(nproc) && length(nproc == 1) && nproc >= 1)
#                 )
#       stopifnot(is.null(respect_unordered_factors) ||
#                   (is.character(respect_unordered_factors) &&
#                   length(respect_unordered_factors) == 1)
#                 )
#       stopifnot(is_integerish(max_depth) &&
#                 length(max_depth) == 1 &&
#                 max_depth > 0
#                 )

#       self$sample_size = sample_size
#       self$num_trees = num_trees
#       self$replace = replace
#       self$seed = seed
#       self$nproc = nproc
#       self$respect_unordered_factors = respect_unordered_factors
#       self$max_depth = max_depth
#       self$status = "not_trained"
#     }
#     ,
#     fit = function(dataset) {

#       # check if any rows are duplicated
#       if (anyDuplicated(dataset) > 0) {
#         lgr::lgr$info("dataset has duplicated rows")
#       }

#       # create new fit
#       if (self$status == "trained") {
#         self$status = "not_trained"
#         lgr::lgr$info("Retraining ... ")
#       }

#       # create a new 'y' column with jumbled 1:n
#       columnNames = colnames(dataset)
#       nr = nrow(dataset)
#       responseName = columnNames[[1]]
#       while (deparse(substitute(responseName)) %in% columnNames) {
#         responseName = sample(c(letters, LETTERS), 20, replace = TRUE)
#       }
#       set.seed(self$seed)
#       dataset[[deparse(substitute(responseName))]] = sample.int(nrow(dataset))

#       # deduce sample_fraction
#       stopifnot(self$sample_size <= nr)
#       private$sample_fraction = self$sample_size / nr

#       # build a extratrees forest
#       lgr::lgr$info("Building Isolation Forest ... ")
#       self$forest = ranger::ranger(
#         dependent.variable.name = deparse(substitute(responseName))
#         , data = dataset
#         , mtry = ncol(dataset) - 1L
#         , min.node.size = 1L
#         , splitrule = "extratrees"
#         , num.random.splits = 1L
#         , num.trees = self$num_trees
#         , replace = self$replace
#         , sample.fraction = private$sample_fraction
#         , respect.unordered.factors = self$respect_unordered_factors
#         , num.threads = self$nproc
#         , seed = self$seed
#         , max.depth = self$max_depth
#         )
#       lgr::lgr$info("done")

#       # compute terminal nodes depth
#       lgr::lgr$info("Computing depth of terminal nodes ... ")
#       private$terminal_nodes_depth = terminalNodesDepth(self$forest)
#       lgr::lgr$info("done")

#       # set phi -- sample size used for tree building
#       private$phi = floor(private$sample_fraction * nr)

#       # create path length extend dataframe
#       tnm = stats::predict(self$forest
#                            , dataset
#                            , type = "terminalNodes"
#                            , num.threads = self$nproc
#                            )[["predictions"]]

#       tnm = data.table::as.data.table(tnm)
#       data.table::setnames(tnm, colnames(tnm), as.character(1:ncol(tnm)))
#       tnm[, id := .I]
#       tnm = data.table::melt(tnm
#                              , id.vars = "id"
#                              , variable.name = "id_tree"
#                              , value = "id_node"
#                              , variable.factor = FALSE
#                              )
#       id_tree = NULL
#       id_node = NULL

#       tnm[, id_tree := as.integer(id_tree)]
#       tnm[, id_node := as.integer(id_node)]

#       # update train status
#       self$status = "trained"
#       lgr::lgr$info("Completed growing isolation forest")
#     }
#     ,
#     predict = function(data) {

#       tnm = stats::predict(self$forest
#                            , data
#                            , type = "terminalNodes"
#                            , num.threads = self$nproc
#                            )[["predictions"]]

#       tnm = data.table::as.data.table(tnm)
#       data.table::setnames(tnm, colnames(tnm), as.character(1:ncol(tnm)))
#       tnm[, id := .I]
#       tnm = data.table::melt(tnm
#                              , id.vars = "id"
#                              , variable.name = "id_tree"
#                              , value = "id_node"
#                              , variable.factor = FALSE
#                              )
#       id_tree = NULL
#       id_node = NULL

#       tnm[, id_tree := as.integer(id_tree)]
#       tnm[, id_node := as.integer(id_node)]

#       obs_depth = merge(tnm
#                         , private$terminal_nodes_depth
#                         , by = c("id_tree", "id_node")
#                         )

#       average_depth = NULL
#       depth = NULL
#       id = NULL
#       anomaly_score = NULL

#       scores = obs_depth[, .(average_depth = mean(depth)), by = id][
#         order(id)]
#       scores[, anomaly_score :=
#                private$computeAnomaly(average_depth, private$phi)][]

#       return(scores)
#     }
#   )
#   ,
#   private = list(

#     terminal_nodes_depth = NULL
#     , phi = NULL
#     , sample_fraction = NULL
#     ,
#     pathLengthNormalizer = function(phi) {

#       res = 0

#       if (phi == 2) {
#         res = 1
#       }
#       if (phi > 2) {
#         res = (2 * private$harmonic(phi - 1)) - (2 * (phi - 1) / phi)
#       }

#       return(res)

#     }
#     ,
#     computeAnomaly = function(depth, data_size) {

#       den = private$pathLengthNormalizer(data_size)
#       return(2 ^ (-(depth / den)))
#     }
#     ,
#     harmonic = function(n) {
#       ifelse(n > 11, log(n) + 0.577216, sum(1 / seq(1, n)))
#     }
#   )
# )
