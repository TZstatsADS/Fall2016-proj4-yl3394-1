# Fearure Training and Test Datasets #########################################################################################
dat.features.small.test <- dat.features.small[2251:2350,]
dat.features.small.train <- dat.features.small[1:2250,]


# Lyrics Training and Test Datasets  #########################################################################################
dat.lyrics.train <- dat.lyrics[1:2250,]
dat.lyrics.test <- dat.lyrics[2251:2350,]

# Remove columns whose Colsum == 0 
dat.lyrics.train.none0 <- dat.lyrics.train[, - (which(colSums(dat.lyrics.train %>% select(-song_code)) == 0) + 1)]
# Check removing columns 
which(colSums(dat.lyrics.train %>% select(-song_code)) == 0) + 1


# Random Forest Training on Training dataset #################################################################################


# Train RF model on training dataset 
model.list <- list()
for (i in (2: ncol(dat.lyrics.train.none0))){
  dat.temp <- left_join(data.frame(song_code = dat.lyrics.train.none0$song_code,
                                   # Change to binary code 1, 0
                                   word = (ifelse(dat.lyrics.train.none0[, i] > 0, 1, 0))),
                        dat.features.small.train)
  model.name <- colnames(dat.lyrics.train.none0)[i]
  # Save model to the model list 
  model.list[[model.name]] <- randomForest(as.factor(word) ~. -song_code, 
                                           data = dat.temp,
                                           ntree = 200)
  # varImpPlot(model.list[[model.name]]) 
  # Move to the next word 
  i <- i + 1
  print(i)
}

# Test 
model.list[[1]]
model.list[[3]]

dat.empty.test <- as.data.frame(cbind(song_code = dat.features.small.test$song_code, ifelse(dat.lyrics.train[1:100,] %>% 
                                          select(c(2:ncol(dat.lyrics.train))) <0, NA, NA)))

dim(dat.features.small.test)
# Get the prediction results 
dat.pred.prob.test <- getPredProbbyRF(model.list, dat.features.small.test, dat.empty.test)
# Transform the probability table into a ranking table 
dat.pred.rank.test <- getPredRankbyRF(dat.pred.prob.test)


