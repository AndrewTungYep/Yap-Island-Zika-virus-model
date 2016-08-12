library(RNetLogo)
nl.path <- "C:/Program Files (x86)/NetLogo 5.3.1/app"
master.results <- array(,c(212,8,200))
real.data <- rep(c(0,1,2,0,1,1,9,29,15,9,15,19,6,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)*7.14, each=7)
absolute.model.path <- "D:/Users/Student/Documents/zika internship stuff/netlogo models/Yap geographical sir (R) V2.nlogo"
for (c in 1:200){
  perameters<-data.frame(matrix(c(runif(1,0,0.5),runif(1,0,0.5),sample(12:107,1),sample(2:9,1),sample(5:20,1),runif(1,0.132,0.227),runif(1,0.174,0.908),runif(1,0.26,0.86),runif(1,0.5,1),runif(1,0.083,0.25),runif(1,0.09,0.341)),nrow=1,ncol=11))
  colnames(perameters)<-c("Bmr","Bhr","mospopr","mosspreadr","popspreadr","Ahr","Chr","distmr","disthr","Amr","mosdeathr")
  for (m in 1){
    NLStart(nl.path, gui=FALSE)
    NLLoadModel(absolute.model.path)
    NLCommand("setup")
    NLSetAgentSet("relays",perameters)
    NLCommand("rerun","go")
    results <- NLDoReportWhile("ticks < 211 and (Ihg + Img + Emg + Ehg) > 0", "go", c("Shg","Ehg","Ihg","Rhg","Smg","Emg","Img","nIh"), as.data.frame=TRUE, max.minutes=60)
    if(dim(results)[1]==0){
      results<-matrix(0,nrow=212,ncol=8)
    }else {
      results<-matrix(c(t(results), rep(0, times = 1696 - 8 * length(results[,1]))), nrow=212, ncol=8, byrow=TRUE)
    }
    results[211,] <- unlist(NLReport(c("Bm","Bh","mospop","mosspread","popspread","Ah","Ch","distm")))
    results[212,1:3] <- unlist(NLReport(c("disth","Am","mosdeath")))
    diff <- sum( abs( real.data[1:210] - results[1:210,8]))
    results[212,4] <- diff
    master.results[,,(c)] <-as.matrix(results)
    print(c)
  }
}
#results