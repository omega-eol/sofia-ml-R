# doSVM training and testing
# 
# Inputs
# svm_train_filename - name of a training set file
# svm_test_filename  - name of a test set file
# lambda - value of the regularization term in SVM
# d - number of dimensions
# learning_type - Options are: pegasos, passive-aggressive, margin-perceptron, romma, sgd-svm, least-mean-squares, logreg, and logreg-pegasos
# if you don not understand this -> read the paper
# loop_type - Options are: stochastic, balanced-stochastic, roc, rank, query-norm-rank, combined-ranking
# random_seed - When set to non-zero value, use this seed instead of seed from system clock.
# This can be useful for parameter tuning in cross-validation, as setting a seed by hand 
# forces examples to be sampled in the same order.
# However for actual training/test, this should never be used.
doSVM = function(svm_train_filename, 
                 svm_test_filename, 
                 lambda, d, learning_type = "sgd-svm", loop_type = "balanced-stochastic", 
                 random_seed = 0, iterations = 10000000, eta_type = "pegasos",
                 sofia_path = "~/lib/sofia-ml/sofia-ml") {
     
     # initialization
     model_filename = tempfile();
     train_results_filename = tempfile();
     test_results_filename = tempfile();
     
     # training a SVM model
     message("Training SVM model..");
     command_train = paste(sofia_path,
                           "--learner_type", learning_type,
                           "--loop_type", loop_type,
                           "--eta_type", eta_type,
                           "--lambda",lambda,
                           "--prediction_type logistic --no_bias_term",
                           "--iterations", iterations,
                           "--random_seed", random_seed,
                           "--dimensionality", d+1,
                           "--training_file", svm_train_filename,
                           "--model_out", model_filename);
     system(command_train);     
     message("Retrieving the SVM model..");
     sv <- try(scan(file=model_filename));
     if (class(sv) == "try-error") {
          message("Error in doSVM (cannot read the model). Returning NULL");
          # clean up
          unlink(model_filename);
          return(NULL);
     }
     message('Retrieving is complete.');
     
     # Claffify the training set first
     message("Classification of the train set..");
     command_train = paste(sofia_path,
                           "--learner_type", learning_type, 
                           "--loop_type", loop_type,
                           "--eta_type", eta_type,
                           "--lambda",lambda,
                           "--prediction_type logistic --no_bias_term",
                           "--iterations 10000000",
                           "--random_seed", random_seed,
                           "--dimensionality", d+1,
                           "--test_file", svm_train_filename,
                           "--model_in", model_filename,
                           "--results_file", train_results_filename);
     system(command_train);
     
     train_prediction <- try(read.table(train_results_filename));
     if (class(train_prediction) == "try-error") {
          message("doSVM: Cannot read the training results. Returning NULL");
          # clean up
          unlink(model_filename);
          unlink(train_results_filename);
          return(NULL);
     }; 
     message("Classification of the train set is complete.");     
     
     # Claffify the test set first
     message("Classification of the test set..");
     command_train = paste(sofia_path,
                           "--learner_type", learning_type,
                           "--loop_type", loop_type,
                           "--eta_type", eta_type,
                           "--lambda",lambda,
                           "--prediction_type logistic --no_bias_term",
                           "--iterations 10000000",
                           "--random_seed", random_seed,
                           "--dimensionality", d+1,
                           "--test_file", svm_test_filename,
                           "--model_in", model_filename,
                           "--results_file", test_results_filename);
     system(command_train);     
     test_prediction <- try(read.table(test_results_filename));
     if (class(test_prediction) == "try-error") {
          message("doSVM: Cannot read the testing results. Returning NULL");
          # clean up
          unlink(model_filename);
          unlink(train_results_filename);
          unlink(test_results_filename);
          return(NULL);
     }; 
     message("Classification of the test set is complete.");     
     
     # output
     res = list("test_prediction" = test_prediction, 
                "train_prediction" = train_prediction, 
                "sv" = sv);
     
     # clean up
     unlink(model_filename);
     unlink(train_results_filename);
     unlink(test_results_filename);
     return(res); 
}
