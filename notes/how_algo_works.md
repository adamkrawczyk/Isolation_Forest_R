## Intro

decison is made based on anomaly score 

`S(x,n) = 2^(-(E(h(x))) / c(n))`

h(x) - path length of observation x

c(n) - average path length of unsuccessful search in binary search tree

n - number of external nodes

### score interpretation 

Score close to 1 is anomaly

Score much smaller than 0.5 is normal observation

If all scores are close to 0.5 then is hard to divide trening data

### Hyperparameters

Two for training one for evaluation

T:

```


## Random spliting

1. Random select feature 
2. Random select split value


### Training

Split values until all are isolated

There are two stages of training

1. build trees 
2. build iforest

Next is compute scores by path length
3. compute scores
