library(RNetLogo)
nl.path <- "C:/Program Files (x86)/NetLogo 5.3.1/app"
NLStart(nl.path, gui=FALSE)
absolute.model.path <- "D:/Users/Student/Documents/zika internship stuff/netlogo models/Yap geographical sir (R).nlogo"
NLLoadModel(absolute.model.path)
master.results <- array(,c(212,8,100))
real.data <- rep(c(0,1,2,0,1,1,9,29,15,9,15,19,6,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)*7.14, each=7)
for (c in 1:100){
  results.array <- array(,c(212,8,1))
  NLCommand("setup")
  for (k in 1){
    NLCommand("rerun","go")
    results <- NLDoReportWhile("ticks < 211 and (Ihg + Img + Emg + Ehg) > 0", "go", c("Shg","Ehg","Ihg","Rhg","Smg","Emg","Img","nIh"), as.data.frame=TRUE, max.minutes=60)
    if(dim(results)[1]==0){
      results<-matrix(0,nrow=212,ncol=8)
    }else {
        results<-matrix(c(t(results), rep(0, times = 1696 - 8 * length(results[,1]))), nrow=212, ncol=8, byrow=TRUE)
      }
    results.array[,,k]<-results
  }
  print(c)
  for (i in 1:210) {
    for (j in 1:8) {
      results [i, j] <- mean(results.array[i, j, ])
    }
  }
  results[211,] <- unlist(NLReport(c("Bm","Bh","mospop","mosspread","popspread","Ah","Ch","distm")))
  results[212,1:3] <- unlist(NLReport(c("disth","Am","mosdeath")))
  diff <- sum( abs( real.data[1:210] - results[1:210,8]))
  results[212,4] <- diff
  master.results[,,c] <-as.matrix(results)
  # print("yay")
}
#results