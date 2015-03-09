# return basic validation metrics for Supervised models
validate = function(predicted, groundtruth, class_names = NULL, verbose=TRUE) {
     
     # check is the input is a probability matrix
     if (is.matrix(predicted)) {
          if (is.null(class_names)) stop('Class names is not specified');
          predicted = apply(predicted, 1, function(x) class_names[which.max(x)] );
     };
     
     # ERROR CHECK
     n = length(predicted);
     if (n != length(groundtruth)) stop("Number of points in predicted is different than in groundtruth");
     
     # confusion matrix
     tt = table(predicted, groundtruth);
     
     # find number of classes on actual data
     q = dimnames(tt); classes = q$groundtruth; k = length(classes);
     if (k != length(q$predicted)) stop("Number of classes in predicted is different than in groundtruth");
     
     # variables
     accuracy = rep(0, k); precision = rep(0, k); recall = rep(0, k);
     groundtruth_dist = colSums(tt);
     predicted_dist = rowSums(tt);
     for (i in 1:k) {
          # measures
          tp = tt[i,i];
          fp = sum(tt[i,])-tp;
          fn = sum(tt[,i])-tp;
          tn = n-tp-fp-fn;
          
          # metrics
          accuracy[i] = (tp+tn)/n;
          precision[i] = tp/(tp+fp);
          recall[i] = tp/(tp+fn);
     };
     f_measure = 2*precision*recall/(precision+recall);
     
     # compute weighted F-measure
     w_f_measure = sum(groundtruth_dist*f_measure/n);
     
     # prepare the output
     df = data.frame(classes, accuracy, precision, recall, f_measure, predicted_dist/n, groundtruth_dist/n,
                     (groundtruth_dist-predicted_dist)/groundtruth_dist );
     names(df) = c('Class', 'Accuracy', 'Precision', 'Recall', 'F-measure', 'Predicted Distribution', 'Groundtruth Distribution', 'Distribution Delta');
     rownames(df) = NULL;
     res = list("df" = df, "avg_f_measure" = mean(f_measure), "w_f_measure" = w_f_measure);
     
     # output
     if (verbose) {
          message('Validation is done base on ', n, ' samples:');
          print(res$df);
          message('Average F-measure: ', res$avg_f_measure, ' (weigthed: ',res$w_f_measure, ')');
     };
     
     return(res);
}