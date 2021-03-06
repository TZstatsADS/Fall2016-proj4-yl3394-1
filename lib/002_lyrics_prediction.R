# Package Loading ###################################################################################################
# for data processing
library(dplyr)
library(tidyr)

# for model training
library(randomForest)
library(stats)

# # Method 1 
# m <- song.features.df %>% select(-song) 
# dis <- as.matrix(dist(m))+diag(2350)*10000
# n <- lyr %>% select(-1) 
# res <- sort(n[which.min(dis[1,]),],decreasing=T)


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
dat.features[is.na(dat.features)] <- 0
dat.features.pca <- prcomp(dat.features %>% select(-song_code),  # remove the string column: song_code  
                           center = TRUE, scale. = TRUE)
# Visaulize 
plot(dat.features.pca, type = "l") 
summary(dat.features.pca) # first 19 PCs explains ~90%

# New dim-reduced data 
dat.features.small <- cbind(song_code = dat.features$song_code, 
                            as.data.frame(dat.features.pca$x[,1:12]))


# Random Forest Basic Setup ############################################################################################
dat.features.small[is.na(dat.features.small)] <- 0 # Change all NAs into 0s 
test <- data.frame(song_code = dat.lyrics$song_code, 
                   abandon = (ifelse(dat.lyrics$abandon > 0, 1, 0))) # Change into binary code 1 and 0 

# Remove columns whose Colsum == 0 
dat.lyrics.none0 <- dat.lyrics[, - (which(colSums(dat.lyrics %>% select(-song_code)) == 0) + 1)]
# Should we remove sumcols = 0 columns ???????????????

# Helper Function 001: Get RF models ----------------------------------------------
# Create an empty model list 
getRankbyRF <- function(data){
  # @parameter: an input dataframe 
  # @value: a model list containing multiple RF models 
  model.list <- list()
  for (i in (2: ncol(data))){
    dat.temp <- left_join(data.frame(song_code = dat.lyrics$song_code,
                                     # Change to binary code 1, 0
                                     word = (ifelse(data[, i] > 0, 1, 0))),
                          dat.features.small)
    model.name <- colnames(data)[i]
    # Save model to the model list 
    model.list[[model.name]] <- randomForest(as.factor(word) ~. -song_code, 
                                             data = dat.temp,
                                             ntree = 200)
    # varImpPlot(model.list[[model.name]]) 
    # Move to the next word 
    i <- i + 1
    print(i)
  }
  # Return the final model list containing 5000 models 
  return(model.list)
}
# ---------------------------------------------------------------------------------



# Random Forest Model Training ##########################################################################################
# Get all Random Forest models (4825) 
RFmodel4825.list <- getRankbyRF(dat.lyrics.none0)



# TEST DATASET
# Data Processing #########################################################################################################
dat.features.test <- song.features.testdf %>% 
  rename(song_code = song)

dat.features.test[is.na(dat.features.test)] <- 0
# Run PCA on the raw feature data 
dat.features.test.pca <- prcomp(dat.features.test %>% select(-song_code),  # remove the string column: song_code  
                           center = TRUE, scale. = TRUE)

# Summarize 
summary(dat.features.test.pca)  

# New dim-reduced data 
dat.features.test.small <- cbind(song_code = dat.features.test$song_code, 
                            as.data.frame(dat.features.test.pca$x[,1:12]))


# Random Forest Prediction ##############################################################################################
# Helper Function 002: Get RF predictions -----------------------------------------
getPredProbbyRF <- function(model.list, test.data, pred.result) {
  # @parameter: a RF models list, a test dataset, an empty prediction table 
  # @value: the filled-out prediction table 
  for (i in (1: length(model.list))){
    temp.pred <- as.data.frame(predict(model.list[[i]], test.data, type = "prob"))
    pred.result[, i+1] <- temp.pred$`1`
    print(i)
  }
  return(pred.result)
}
# ---------------------------------------------------------------------------------

# Make an empty dataframe for probability values 
dat.empty <- as.data.frame(cbind(song_code = dat.features.test$song_code, 
                                 ifelse(dat.lyrics.none0[1:100,] %>% 
                                          select(c(2:ncol(dat.lyrics.none0))) <0, NA, NA)))
# Make a copy of the empty dataframe 
dat.pred.prob <- dat.empty 

# Get the prediction results 
dat.pred.prob <- getPredProbbyRF(RFmodel4825.list, dat.features.test.small, dat.pred.empty)
# Save as a csv file to local
write.csv(dat.pred.prob, "exported_data/dat_pred_prob.csv")


# Ranks Generating ########################################################################################################
# Helper Function 003: Get RF ranking ---------------------------------------------
getPredRankbyRF <- function(pred.result) {
  rank.temp <- t(as.data.frame(rank(-pred.result[1,-1], 
                                      ties.method = "first")))
  for(i in (2: nrow(pred.result))){
    rank.temp <- rbind(rank.temp, 
                         t(as.data.frame(rank(-pred.result[i,-1], 
                                              ties.method = "first"))))
  }
  rank.result <- data.frame(pred.result[,1], rank.temp)
  colnames(rank.result) <- colnames(pred.result)
  return(rank.result)
}
# ---------------------------------------------------------------------------------

# Transform the probability table into a ranking table 
dat.pred.rank <- getPredRankbyRF(dat.pred.prob)
# Save as a csv file to local for submission
write.csv(dat.pred.rank, "exported_data/dat_pred_rank.csv")

