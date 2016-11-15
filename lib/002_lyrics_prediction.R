# Package Loading ###################################################################################################
library(dplyr)
library(tidyr)
library(randomForest)

# Method 1 
m <- song.features.df %>% select(-song) 
dis <- as.matrix(dist(m))+diag(2350)*10000
n <- lyr %>% select(-1) 
res <- sort(n[which.min(dis[1,]),],decreasing=T)


# Set Local Path 
setwd("/Users/yanjin1993/Google Drive/Columbia University /2016 Fall /Applied Data Science /Project_004/lyrics_recommendation/")

# Data Loading #######################################################################################################
# Load lyrics data 
load("original_data/lyr.RData")
dat.lyrics <- as.data.frame(lyr) %>% 
  select(-2, -3, -c(6:30))  # remove unnecessary columns 

colnames(dat.lyrics)[1] <- "song_code"

# Load Feature data 
dat.features <- readRDS("exported_data/song_features.rds") %>%
  rename(song_code = song)


# Random Forest #####################################################################################################
dat.features[is.na(dat.features)] <- 0 # Change all NAs into 0s 
test <- data.frame(song_code = dat.lyrics$song_code, 
                   abandon = (ifelse(dat.lyrics$abandon > 0, 1, 0))) # Change into binary code 1 and 0 



# Random Forest Testing #############################################################################################
# Test on word "Abandon" -------------------------------------
dat.abandon <-  left_join(test, dat.features)

# Test model on "Abandon" 
model.abandon <- randomForest(as.factor(abandon) ~ . -song_code, 
                      data = dat.abandon, ntree = 200)
# Unbalanced model ??????? What we can do with the unbalanced model? 


# Test model on "A" ------------------------------------------
dat.a <-  left_join(data.frame(song_code = dat.lyrics$song_code, 
                               a = (ifelse(dat.lyrics$a > 0, 1, 0))), dat.features)

model.a <- randomForest(as.factor(a) ~ . -song_code, 
                      data = dat.a, ntree = 200)


# Test model on "About" --------------------------------------
dat.about <-  left_join(data.frame(song_code = dat.lyrics$song_code, 
                               about = (ifelse(dat.lyrics$about > 0, 1, 0))), dat.features)

model.about <- randomForest(as.factor(about) ~ . -song_code, 
                        data = dat.about, ntree = 200)




