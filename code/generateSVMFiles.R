# generate training or test files in SVM lite format
# inputs
# svm_lite_data - character array in which each item is a sample in SVM Lite format
# input - array of labels
# folder_path - path where data will be saved in SVM Lite format. One file per class (one vs all).
# if folder_path is null, then folder_path - current working directory
# 
# function returns array of file names, one file per class.
generateSVMFiles = function(svm_lite_data, labels, classes=NULL, folder_path=NULL, replace=TRUE) {

     # folder path where files will be save
     if (is.null(folder_path)) folder_path = getwd();
          
     # intialization
     n = length(labels);
     if (n != length(svm_lite_data)) stop('Number of records in labels is different that in svm_lite_data.');     
     if (is.null(classes)) classes = sort(unique(labels));
     k = length(classes);   
     dir.create(folder_path, recursive = TRUE, showWarnings = FALSE);
     
     # run
     if (k > 2) {          
          # multi-class case: one-vs-all
          
          # initialization
          filenames = character(k);
          
          # for each class we generate a separete file in SVM lite format
          for (i in 1:k) {
               class_name = classes[i];
               filenames[i] = paste0(folder_path, "/svm_", class_name, ".csv");
               
               # if file already exists and raplce is FALSE then skip it!
               if (file.exists(filenames[i])&&!replace) {
                    message(filenames[i], ' already exists. Skipping it!');
               } else {
                    temp = rep(-1, n);
                    ind = labels==class_name;
                    
                    # check wheter we have at least one positive sample 
                    if (!any(ind)) stop('Dataset does not have positive samples.');
                    
                    temp[ind] = 1;
                    write(paste(temp, svm_lite_data), file=filenames[i]);
               };
          }; # end for each class - j
          
     } else if (k==2) { 
          # this is binary case
          
          # set filename
          class_name = classes[1];
          filenames = paste0(folder_path, "/svm_", class_name, "_vs_", classes[2], ".csv");
          
          # prepare labels
          temp = rep(-1, n);
          ind = labels==class_name;
          
          # check wheter we have at least one positive sample 
          if (!any(ind)) stop('Dataset does not have positive samples.');
          
          temp[ind] = 1;
          
          # write data as text file
          write(paste(temp, svm_lite_data), file=filenames);
          
     } else {
          stop('Only one class found.')
     };
     
     return(filenames); 
};