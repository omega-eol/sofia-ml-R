# generate training or test files in SVM lite format
# inputs
# svm_lite_data - character array in which each item is a sample in SVM Lite format
# input - array of labels
# folder_path - path where data will be saved in SVM Lite format. One file per class (one vs all).
# if folder_path is null, then folder_path - current working directory
# 
# function returns array of file names, one file per class.
generateSVMFiles = function(svm_lite_data, labels, folder_path=NULL) {

     # folder path where files will be save
     if (is.null(folder_path)) folder_path = getwd();
          
     # intialization
     n = length(labels);
     if (n != length(svm_lite_data)) stop('Number of records on labels is different that in svm_lite_data.');     
     classes = sort(unique(labels));
     k = length(classes);   
     dir.create(folder_path, recursive = TRUE, showWarnings = FALSE);
     
     # for each class we generate a separete train and test data file in SVM lite format
     filenames = character(k);
     for (i in 1:k) {
          class_name = classes[i];
          filenames[i] = paste0(folder_path, "/svm_", class_name, ".csv");
          temp = rep(-1, n);
          temp[labels==class_name] = 1;
          write(paste(temp, svm_lite_data), file=filenames[i]);
     }; # end for each class - j
     
     return(filenames); 
};