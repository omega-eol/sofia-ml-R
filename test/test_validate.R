# clear workspace
rm(list=ls());

# set working directory
setwd('~/code/sofia-ml-R/test');

# load validate function
source('../code/validate.R');

# create artificial scores as random numbers
prediction_probobility = runif(100);
groundtruth = round(runif(100));

# compute area under PR curve
valid_res = validate(prediction_probobility, groundtruth);
