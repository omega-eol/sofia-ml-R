# return basic validation metrics for Supervised models
validate = function(predicted, groundtruth, class_names = NULL, th = NULL, n_points = 100, verbose=TRUE, plot_graph=TRUE, filename=NULL, compute_auc_pr=TRUE) {
     
     # check is the input is a probability matrix
     if (is.matrix(predicted)) {
          if (is.null(class_names)) stop('Class names is not specified');
          predicted = apply(predicted, 1, function(x) class_names[which.max(x)] );
     };
     
     # length check
     n = length(predicted);
     if (n != length(groundtruth)) stop("Number of points in predicted is different than in groundtruth.");
     auc = NULL; auc_pr = NULL;
     
     # check if the input is a probability vector
     if (!all(predicted == floor(predicted))) { # if TRUE we assume a binary classification problem
          
          # calculate AUC of Precision - Recall curve
          if (compute_auc_pr) {
               library(PRROC);
               auc_pr = pr.curve(scores.class0=predicted, weights.class0=groundtruth, curve=plot_graph);
               if (plot_graph) plot(auc_pr);
          } else {
               auc_pr = list(auc.davis.goadrich = 0);
          };
          
          # calculate AUC 
          ranked_prediction = rank(predicted);
          index = groundtruth==1; np = sum(index);
          auc = (sum(ranked_prediction[index]) - np*(np+1)/2)/np/(n-np);
          
          # plot ROC: sensitivity = recall = true positive rate = TP/(TP+FN)
          # number of points on the plot
          n_plot_points = min(n, n_points+2);
          
          # sort predicted probability
          sort_res = sort(predicted, index.return=TRUE);
          prob = predicted[sort_res$ix];
          ground = groundtruth[sort_res$ix];
          
          # for eah point - threshold value
          position_index = floor(seq.int(from = 1, to = n, length.out = n_plot_points));
          sensitivity = rep(0, n_plot_points); specificity = rep(0, n_plot_points); predicted_np = rep(0, n_plot_points);
          for (i in 2:n_plot_points) {
               i_th = prob[position_index[i]];
               # everything > i_th is 1
               tp = sum(ground[position_index[i]:n]==1);
               fn = sum(ground[1:(position_index[i]-1)]==1);
               tn = sum(ground[1:(position_index[i]-1)]==0);
               fp = sum(ground[position_index[i]:n]==0);
               sensitivity[i] = tp/(tp+fn);
               specificity[i] = tn/(tn+fp);
               
               # number of predicted positive records
               predicted_np[i] = n-position_index[i]+1;
          };          
          # boundary conditions
          sensitivity[1] = 1;
          specificity[n_plot_points] = 1;
          
          # do actual plot
          if (plot_graph) {
               plot(1-specificity, sensitivity, type="l", col="red", main=paste0("ROC curve\nAUC = ", sprintf("%8.7f", auc)));
               lines(x=c(0, 1), y=c(0, 1), lty=2);
          };
                
          # find optimal threshold (approximately)
          if (is.null(th)) th = prob[position_index[which.min(abs(np/n - predicted_np/n))]];
          
          # convert to predictions
          prob = rep(0, n);
          prob[predicted>=th] = 1;
          predicted = prob;
     };
     
     # confusion matrix
     tt = table(predicted, groundtruth);
     
     # find number of classes on actual data
     q = dimnames(tt); classes = q$groundtruth; k = length(classes);
     if (k != length(q$predicted) | length(setdiff(q$predicted, q$groundtruth)) != 0) {
          warning("Number of predicted classes is different than number of classes in groundtruth data.");
          
          # this makes everything a little bit complicated
          classes = sort(union(q$predicted, q$groundtruth));
          k = length(classes);
          
          # variables
          accuracy = rep(0, k); precision = rep(0, k); recall = rep(0, k);
          groundtruth_dist = rep(0, k); predicted_dist = rep(0, k);
          for (i in 1:k) {
               class_name = classes[i];
               row_index = which(q$predicted==class_name);
               col_index = which(q$groundtruth==class_name);
               
               predicted_dist[i] = sum(predicted==class_name);
               groundtruth_dist[i] = sum(groundtruth==class_name);
               
               if (length(row_index) > 0 && length(col_index) > 0) {               
                    # measures
                    tp = tt[row_index, col_index];
                    fp = sum(tt[row_index,])-tp;
                    fn = sum(tt[,col_index])-tp;
                    tn = n-tp-fp-fn;
                    
                    # metrics
                    accuracy[i] = (tp+tn)/n;
                    precision[i] = tp/(tp+fp);
                    recall[i] = tp/(tp+fn);
               };
          };
     
     } else { # this is normal case, when classes are match
          
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
          
     }; # end if
     
     # find F-measure
     f_measure = 2*precision*recall/(precision+recall);
     # replace NaNs with 0
     f_measure[is.nan(f_measure)] = 0;
     
     # compute weighted F-measure
     w_f_measure = sum(groundtruth_dist*f_measure/n);
     
     # prepare the output
     df = data.frame(classes, accuracy, precision, recall, f_measure, predicted_dist/n, groundtruth_dist/n, predicted_dist, groundtruth_dist,
                     (groundtruth_dist-predicted_dist)/groundtruth_dist, precision/(groundtruth_dist/n) );
     names(df) = c('Class', 'Accuracy', 'Precision', 'Recall', 'F-measure', 'Predicted Distribution', 'Groundtruth Distribution', 
                   '# Predicted', '# Groundtruth', 'Distribution Delta', 'Gain');
     rownames(df) = NULL;
     if (!is.null(auc)) { # if TRUE then auc_pr is also not null
          df = cbind(df, rep(auc, k), rep(auc_pr$auc.davis.goadrich, k)); 
          names(df) = c('Class', 'Accuracy', 'Precision', 'Recall', 'F-measure', 'Predicted Distribution', 'Groundtruth Distribution', 
                        '# Predicted', '# Groundtruth', 'Distribution Delta', 'Gain', 'AUC', 'AUC-PR');
     };
     
     res = list("df" = df, "avg_f_measure" = mean(f_measure), "w_f_measure" = w_f_measure, 
                "auc" = auc, "auc_pr" = auc_pr, "sensitivity" = sensitivity, "specificity" = specificity, "th" = th);
          
     # output
     if (verbose) {
          message('Validation is done base on ', n, ' samples:');
          print(res$df);
          message('Average F-measure: ', res$avg_f_measure, ' (weigthed: ',res$w_f_measure, ')');
          if (!is.null(auc)) message('AUC: ', auc, ', AUC-PR: ', auc_pr$auc.davis.goadrich);
     };
     
     # save output as csv file
     if (!is.null(filename)) {
          if (basename(filename) == filename) filename = paste0(getwd(), "/", filename);
          message('Saving results in ' ,filename, '..');
          dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE);
          write.table(res$df, file=filename, row.names=FALSE, sep=",");
     };
     
     return(res);
}