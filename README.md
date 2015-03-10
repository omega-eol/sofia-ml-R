## R wrapper for sofia-ml suite.

If you want more details about the suite then ckecout the [site](https://code.google.com/p/sofia-ml/) or read the papers:

* D. Sculley. Combined Regression and Ranking. Proceedings of the 16th Annual SIGKDD Conference on Knowledge Discover and Data Mining, 2010.
* D. Sculley. Web-Scale K-Means Clustering. Proceedings of the 19th international conference on World Wide Web, 2010.
* D. Sculley. Large Scale Learning to Rank. NIPS Workshop on Advances in Ranking, 2009. Presents the indexed sampling methods used learning to rank, including the rank and roc loops.

### Description
* `code/doSVM.R` R wrapper for sofia-ml suite. Before using the wrapper make sure you have installed the suit, and can run it from command line.
* `code/validate.R` computes accuracy, precision, recall, F-measure, predicted and expected distributions for a supervised model.
    * Supports case when prediction and groundtruth have different classes
    * Supports probability matrix (the class with the highest proabliry always wins) or probability vector (for binary models) or vector of predicted values
    * For binary models computes AUC, plots ROC curve, and finds an optimal threshold so that the predicted distribution as close as possible to the actual distribution
    * Can save results to CSV file
* `test/test_doSVM.R` gives you a simple example how to use the wrapper. As a dataset I used [IRIS dataset](https://archive.ics.uci.edu/ml/datasets/Iris), see UCI Machine Learning Repository for details.