
# Package Loading ###################################################################################################
library(dplyr)
library(tidyr)
library(pbapply)
library(rhdf5)
library(class)


# Set Local Path ####################################################################################################
setwd('/Users/yanjin1993/Google Drive/Columbia University /2016 Fall /Applied Data Science /Project_004/')


# TEST DATASET
# Data Processing ##################################################################################################
# Get local path
dir.h5 <- '/Users/yanjin1993/Google Drive/Columbia University /2016 Fall /Applied Data Science /Project_004/TestSongFile100/'
# Generate the file directories 
files.list <- as.matrix(list.files(dir.h5, recursive = TRUE))
# Get features from files 
song.features.testdf <- get.features(files.list, dir.h5)
# Save to local 
saveRDS(song.features.df, file = "lyrics_recommendation/exported_data/song_features_test.rds")


