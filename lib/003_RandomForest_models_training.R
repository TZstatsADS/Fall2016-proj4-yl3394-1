# Package Loading ###################################################################################################
# for data processing
library(dplyr)
library(tidyr)

# for model training
library(randomForest)
library(stats)

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



# PCA ###############################################################################################################
# Before start Random Forest, reduce dimension first
dat.features.pca <- prcomp(dat.features %>% select(-song_code),  # remove the string column: song_code  
                           center = TRUE, scale. = TRUE)
# Visaulize 
plot(dat.features.pca, type = "l") 
summary(dat.features.pca) # first 19 PCs explains ~90%

# New dim-reduced data 
dat.features.small <- cbind(song_code = dat.features$song_code, 
                            as.data.frame(dat.features.pca$x[,1:19]))

# Random Forest #####################################################################################################
dat.features.small[is.na(dat.features.small)] <- 0 # Change all NAs into 0s 
test <- data.frame(song_code = dat.lyrics$song_code, 
                   abandon = (ifelse(dat.lyrics$abandon > 0, 1, 0))) # Change into binary code 1 and 0 


# Random Forest Testing #############################################################################################
# Test on word "Abandon" -------------------------------------
dat.abandon <-  left_join(test, dat.features.small)

# Test model on "Abandon" 
model.abandon <- randomForest(as.factor(abandon) ~ . -song_code, 
                      data = dat.abandon, ntree = 200)
# Unbalanced model ??????? What we can do with the unbalanced model? 


# Test model on "A" ------------------------------------------
dat.a <-  left_join(data.frame(song_code = dat.lyrics$song_code, 
                               # Change to binary code 1, 0 
                               a = (ifelse(dat.lyrics$a > 0, 1, 0))), 
                    dat.features.small)



model.a <- randomForest(as.factor(a) ~ . -song_code, 
                      data = dat.a, 
                      importance = TRUE, # allows us to inspect variable importance 
                      ntree = 200)
# Get vote
vote.a <- as.data.frame(model.a$votes) 


# Helper Function 001: Get RF models for 5000 words ----------------------------
# Create an empty model list 
model.list <- list()
getRankbyRF <- function(data){
  # @parameter: an input dataframe 
  # @value: a model list containing multiple RF models 
  for (i in (2: ncol(dat.lyrics))){
    dat.temp <- left_join(data.frame(song_code = dat.lyrics$song_code,
                                     # Change to binary code 1, 0
                                     word = (ifelse(dat.lyrics[, i] > 0, 1, 0))),
                          dat.features.small)
    model.name <- colnames(dat.lyrics)[i]
    # Save model to the model list 
    model.list[[model.name]] <- randomForest(as.factor(word) ~. -song_code, 
                                             data = dat.temp,
                                             ntree = 200)
    # varImpPlot(model.list[[model.name]]) 
    # Move to the next word 
    i <- i + 1
  }
  # Return the final model list containing 5000 models 
  return(model.list)
}
# ---------------------------------------------------------------------------------





# TEST  ---------------------------------------------------------------------------
# TEST 001: test on 1 word
RFmodel.list <- getRankbyRF()

# TEST 002: test on 5 words 



# ---------------------------------------------------------------------------------
