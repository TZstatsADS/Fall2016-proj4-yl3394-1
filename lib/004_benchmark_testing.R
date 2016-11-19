t <- proc.time()
num_test <- 100
X <- dat.features.small[c(1:(2350-num_test)),] ### CHANGE  lyr2  features.df2
Y <- dat.lyrics.none0[c(1:(2350-num_test)),]   ### CHANGE  lyr2
X_test <- dat.features.small[c((2350-num_test+1):2350),]   ### CHANGE  lyr2

# Your Model

dat.empty.test <- as.data.frame(ifelse(dat.lyrics.none0[1:100,] %>% 
                                          select(c(2:ncol(dat.lyrics.none0))) <0, NA, NA))
dat.empty.test <- cbind(song_code = X_test$song_code, dat.empty.test)
# Make a copy of the empty dataframe 

rank_pre <- getPredProbbyRF(RFmodel4825.list, X_test, dat.empty.test)
rank_pre <- getPredRankbyRF(rank_pre)
rank_pre <- rank_pre[,-1]


# test performance ---------------------------------------------------------------------------------------
score <- 0
for (i in 1:num_test){
  word.rank <- rank_pre[i,]
  exist.word <- names(dat.lyrics.none0)[which(dat.lyrics.none0[2350-num_test+i,-1]!=0)+1] 
  score <- score + sum(word.rank[exist.word])/length(exist.word)
}
score/num_test
# ---------------------------------------------------------------------------------------------------------


# benchmark ----------------------------------------------------------------------------------------------
num_test <- 100
score.bm <- 0
benchMark.rank <- rank(-colSums(dat.lyrics.none0[,-1]), ties.method="first")   ### CHANGE lyr2
for (i in 1:num_test){
  exist.word <- names(dat.lyrics.none0)[which(dat.lyrics.none0[2350-num_test+i,-1]!=0)+1]   ### CHANGE lyr2
  score.bm <- score.bm + sum(benchMark.rank[exist.word])/length(exist.word)
}
score.bm/num_test
# ---------------------------------------------------------------------------------------------------------