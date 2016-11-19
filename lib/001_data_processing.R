# Package Loading ###################################################################################################
library(dplyr)
library(tidyr)
library(pbapply)
library(rhdf5)
library(class)


# Set Local Path ####################################################################################################
setwd('/Users/yanjin1993/Google Drive/Columbia University /2016 Fall /Applied Data Science /Project_004/')


# Test One Single File  #############################################################################################
# h5ls("/Users/yanjin1993/Google Drive/Columbia University /2016 Fall /Applied Data Science /Project_004/Project4_data/data/A/A/B/TRAABJV128F1460C49.h5")
# test <- h5dump("/Users/yanjin1993/Google Drive/Columbia University /2016 Fall /Applied Data Science /Project_004/Project4_data/data/A/A/B/TRAABJV128F1460C49.h5")
# analysis <- test$analysis
# analysis$bars_confidence


# Helper Functions ################################################################################################## 
# 001 Get Features from Songs 
get.features <- function(files.list, directory){
  # counters to see progress
  num <- 0  
  total <- length(files.list)
  
  # Loop through all the data files, collect results as a list.
  features <- pblapply(files.list, function(x, dir){
    
    file <- paste0(dir, x)
    h5f <- h5dump(file, load = TRUE)
    
    # Here we only use /analysis, and exclude /metddata, /musicbrainz and /analysis/songs 
    analysis <- h5f$analysis
    
    # 001: bars_confidence 
    bars_confidence_mean <- mean(analysis$bars_confidence) # mean
    bars_confidence_median <- median(analysis$bars_confidence) # median 
    bars_confidence_sd <- sd(analysis$bars_confidence) # standard deviation 
    
    # 002: bars_start
    bars_start_mean <- mean(analysis$bars_start) # mean
    bars_start_median <- median(analysis$bars_start) # median 
    bars_start_sd <- sd(analysis$bars_start) # standard deviation 
    
    # 003: beats_confidence
    beats_confidence_mean <- mean(analysis$beats_confidence) # mean
    beats_confidence_median <- median(analysis$beats_confidence) # median 
    beats_confidence_sd <- sd(analysis$beats_confidence) # standard deviation
    
    # 004: beats_start
    beats_start_mean <- mean(analysis$beats_start) # mean 
    beats_start_median <- median(analysis$beats_start) # median 
    beats_start_sd <- sd(analysis$beats_start) # standard deviation
    
    # 005: sections_confidence
    sections_confidence_mean <- mean(analysis$sections_confidence) # mean
    sections_confidence_median <- median(analysis$sections_confidence) # median 
    sections_confidence_sd <- sd(analysis$sections_confidence) # standard deviation 
    
    # 006: sections_start
    sections_start_mean <- mean(analysis$sections_start) # mean 
    sections_start_median <- median(analysis$sections_start) # median 
    sections_start_sd <- sd(analysis$sections_start) # standard deviation 
    
    # 007: segments_confidence
    segments_confidence_mean <- mean(analysis$segments_confidence) # mean 
    segments_confidence_median <- median(analysis$segments_confidence) # median 
    segments_confidence_sd <- sd(analysis$segments_confidence) # standard deviation 
    
    # 008: segments_loudness_max
    segments_loudness_max_mean <- mean(analysis$segments_loudness_max) # mean 
    segments_loudness_max_median <- median(analysis$segments_loudness_max) # median 
    segments_loudness_max_sd <- sd(analysis$segments_loudness_max) # standard deviation 
    
    # 009: segments_loudness_max_time
    segments_loudness_max_time_mean <- mean(analysis$segments_loudness_max_time) # mean 
    segments_loudness_max_time_median <- median(analysis$segments_loudness_max_time) # median 
    segments_loudness_max_time_sd <- sd(analysis$segments_loudness_max_time) # standard deviation 
    
    # 010: segments_loudness_start
    segments_loudness_start_mean <- mean(analysis$segments_loudness_start) # mean 
    segments_loudness_start_median <- median(analysis$segments_loudness_start) # median 
    segments_loudness_start_sd <- sd(analysis$segments_loudness_start) # standard deviation 
    
    # 011: segments_pitches
    segments_pitches_mean <- mean(analysis$segments_pitches) # mean 
    segments_pitches_median <- median(analysis$segments_pitches) # median 
    segments_pitches_sd <- sd(analysis$segments_pitches) # standard deviation 
    
    # 012: segments_start
    segments_start_mean <- mean(analysis$segments_start) # mean 
    segments_start_median <- mean(analysis$segments_start) # median 
    segments_start_sd <- sd(analysis$segments_start) # standard deviation 
    
    # 013: segments_timbre
    segments_timbre_mean <- mean(analysis$segments_timbre) # mean
    segments_timbre_median <- median(analysis$segments_timbre) # median
    segments_timbre_sd <- sd(analysis$segments_timbre) # standard deviation 
    
    # 014: tatums_confidence
    tatums_confidence_mean <- mean(analysis$tatums_confidence) # mean 
    tatums_confidence_median <- median(analysis$tatums_confidence) # median
    tatums_confidence_sd <- sd(analysis$tatums_confidence) # standard deviation 
    
    # 015: tatums_start
    tatums_start_mean <- mean(analysis$tatums_start) # mean 
    tatums_start_median <- median(analysis$tatums_start) # median
    tatums_start_sd <- sd(analysis$tatums_start) # standard deviation 
    
    
    song <- substr(x, start = 7, stop = nchar(x)-3)
    H5close()
    
    return(c(song, 
             bars_confidence_mean, bars_confidence_median, bars_confidence_sd, # grp 001
             bars_start_mean, bars_start_median, bars_start_sd, # grp 002
             beats_confidence_mean, beats_confidence_median, beats_confidence_sd, # grp 003
             beats_start_mean, beats_start_median, beats_start_sd, # grp 004
             sections_confidence_mean, sections_confidence_median, sections_confidence_sd, # grp 005 
             sections_start_mean, sections_start_median, sections_start_sd, # grp 006 
             segments_confidence_mean, segments_confidence_median, segments_confidence_sd, # grp 007
             segments_loudness_max_mean, segments_loudness_max_median, segments_loudness_max_sd, # grp 008
             segments_loudness_max_time_mean, segments_loudness_max_time_median, segments_loudness_max_time_sd, # grp 009
             segments_loudness_start_mean, segments_loudness_start_median, segments_loudness_start_sd, # grp 010
             segments_pitches_mean, segments_pitches_median, segments_pitches_sd, # grp 011 
             segments_start_mean, segments_start_median, segments_start_sd, # grp 012 
             segments_timbre_mean, segments_timbre_median, segments_timbre_sd, # grp 013 
             tatums_confidence_mean, tatums_confidence_median, tatums_confidence_sd, # grp 014
             tatums_start_mean, tatums_start_median, tatums_start_sd # grp 015
             ))
  },
  dir = directory
  )
  
  # Transform list into a data frame
  song.features.df <- unlist(features) %>% 
    matrix(byrow = TRUE, ncol = 46) %>%   ###### Need to be changed here 
    data.frame()
  names(song.features.df) <- c('song', 
                               'bars_confidence_mean', 'bars_confidence_median', 'bars_confidence_sd',
                               'bars_start_mean', 'bars_start_median', 'bars_start_sd',
                               'beats_confidence_mean', 'beats_confidence_median', 'beats_confidence_sd',
                               'beats_start_mean', 'beats_start_median', 'beats_start_sd',
                               'sections_confidence_mean', 'sections_confidence_median', 'sections_confidence_sd',
                               'sections_start_mean', 'sections_start_median', 'sections_start_sd',
                               'segments_confidence_mean', 'segments_confidence_median', 'segments_confidence_sd',
                               'segments_loudness_max_mean', 'segments_loudness_max_median', 'segments_loudness_max_sd',
                               'segments_loudness_max_time_mean', 'segments_loudness_max_time_median', 'segments_loudness_max_time_sd',
                               'segments_loudness_start_mean', 'segments_loudness_start_median', 'segments_loudness_start_sd', 
                               'segments_pitches_mean', 'segments_pitches_median', 'segments_pitches_sd', 
                               'segments_start_mean', 'segments_start_median', 'segments_start_sd',
                               'segments_timbre_mean', 'segments_timbre_median', 'segments_timbre_sd',
                               'tatums_confidence_mean', 'tatums_confidence_median', 'tatums_confidence_sd',
                               'tatums_start_mean', 'tatums_start_median', 'tatums_start_sd'
                               )
  
  # Change the data structure 
  song.features.dfclean <- song.features.df %>%
    mutate(song = as.character(song), ###### Need to be changed!!!!!! 
           bars_confidence_mean = as.double(bars_confidence_mean), bars_confidence_median = as.double(bars_confidence_median), bars_confidence_sd = as.double(bars_confidence_sd),
           bars_start_mean = as.double(bars_start_mean), bars_start_median = as.double(bars_start_median), bars_start_sd = as.double(bars_start_sd), 
           beats_confidence_mean = as.double(beats_confidence_mean), beats_confidence_median = as.double(beats_confidence_median), beats_confidence_sd = as.double(beats_confidence_sd), 
           beats_start_mean = as.double(beats_start_mean), beats_start_median = as.double(beats_start_median), beats_start_sd = as.double(beats_start_sd),
           sections_confidence_mean = as.double(sections_confidence_mean), sections_confidence_median = as.double(sections_confidence_median), sections_confidence_sd = as.double(sections_confidence_sd),
           sections_start_mean = as.double(sections_start_mean), sections_start_median = as.double(sections_start_median), sections_start_sd = as.double(sections_start_sd),
           segments_confidence_mean = as.double(segments_confidence_mean), segments_confidence_median = as.double(segments_confidence_median), segments_confidence_sd = as.double(segments_confidence_sd),
           segments_loudness_max_mean = as.double(segments_loudness_max_mean), segments_loudness_max_median = as.double(segments_loudness_max_median), segments_loudness_max_sd = as.double(segments_loudness_max_sd),
           segments_loudness_max_time_mean = as.double(segments_loudness_max_time_mean), segments_loudness_max_time_median = as.double(segments_loudness_max_time_median), segments_loudness_max_time_sd = as.double(segments_loudness_max_time_sd),
           segments_loudness_start_mean = as.double(segments_loudness_start_mean), segments_loudness_start_median = as.double(segments_loudness_start_median), segments_loudness_start_sd = as.double(segments_loudness_start_sd),
           segments_pitches_mean = as.double(segments_pitches_mean), segments_pitches_median = as.double(segments_pitches_median), segments_pitches_sd = as.double(segments_pitches_sd),
           segments_start_mean = as.double(segments_start_mean), segments_start_median = as.double(segments_start_median), segments_start_sd = as.double(segments_start_sd),
           segments_timbre_mean = as.double(segments_timbre_mean), segments_timbre_median = as.double(segments_timbre_median), segments_timbre_sd = as.double(segments_timbre_sd),
           tatums_confidence_mean = as.double(tatums_confidence_mean), tatums_confidence_median = as.double(tatums_confidence_median), tatums_confidence_sd = as.double(tatums_confidence_sd),
           tatums_start_mean = as.double(tatums_start_mean), tatums_start_median = as.double(tatums_start_median), tatums_start_sd = as.double(tatums_start_sd)
           )
  
  return(song.features.dfclean)
}

# Data Processing ##################################################################################################
dir.h5 <- '/Users/yanjin1993/Google Drive/Columbia University /2016 Fall /Applied Data Science /Project_004/Project4_data/data/'
# Generate the file directories 
files.list <- as.matrix(list.files(dir.h5, recursive = TRUE))
# Get features from files 
song.features.df <- get.features(files.list, dir.h5)
# Save to local 
saveRDS(song.features.df, file = "lyrics_recommendation/exported_data/song_features.rds")



