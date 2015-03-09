rm(list=ls());
# Test doSVM.R function

# set working directory
setwd('~/code/sofia-ml-R/test');

# load functions
source('../code/doSVM.R'); # sofia-ml wrapper
source('../code/validate.R');

# do example with Iris data set
n = nrow(iris); # number of data points
data = iris[,1:4]; # X
k = nlevels(iris$Species); # number of classes
class_names = levels(iris$Species); # classes:  "setosa", "versicolor", "virginica" 
d = 4; # number of features

# procentage of points which will be used for training
train_ratio = 0.8;
n_train = floor(n*train_ratio);
n_test = n - n_train;

# generate train and test sets - index only
train_index = sample(n, n_train);
test_index = setdiff(1:n, train_index);

# for each level
train_prob = matrix(0, nrow=n_train, ncol=k);
test_prob = matrix(0, nrow=n_test, ncol=k);
for (i in 1:k) {
     # prepare labels - groundtruth
     labels = rep(-1, n);
     labels[iris$Species==class_names[i]] = 1;
     
     # save data for the current class in SVM lite format
     train_filename = paste0(getwd(), '/data/iris/train_', class_names[i], '.dat');
     train_svm = apply(cbind(labels[train_index], data[train_index,]), 1, 
                         function(x) paste0(x[1], ' ', paste(paste0(1:4, ':', x[2:5], ' '), collapse='' ))
                         );
     write.table(train_svm, file=train_filename, row.names=FALSE, quote=FALSE, col.names=FALSE);
     
     test_filename = paste0(getwd(), '/data/iris/test_', class_names[i], '.dat');
     test_svm = apply(cbind(labels[test_index], data[test_index,]), 1, 
                         function(x) paste0(x[1], ' ', paste(paste0(1:4, ':', x[2:5], ' '), collapse='' ) )
                         )
     write.table(test_svm, file=test_filename, row.names=FALSE, quote=FALSE, col.names=FALSE);
     
     # set Lambda
     lambda = 0.1;
     
     # train and test SVM model
     res = doSVM(train_filename, test_filename, lambda, d, learning_type = "logreg-pegasos", iterations=1000);
     train_prob[,i] = res$train_prediction$V1;
     test_prob[,i] = res$test_prediction$V1;
};

# validate test results
valid_res = validate(test_prob, iris$Species[test_index], class_names, verbose=TRUE);



