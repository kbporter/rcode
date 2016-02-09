# ### LINEAR REGRESSION #### 
### last upate: 11/23/15 ##

conditions <- c("Basic", "Jitter", "Move")
##completed conditions: "Basic", "Jitter", "Move"
folder <- "~/Dropbox/Rsubitizing/Exp1/"
##completed folders: "~/Dropbox/Rsubitizing/Exp1/"
nsub <- 12
setSize <- 7

for (icond in 1:length(conditions) )
{
  ##create names for input + output files
  errorName <- paste(folder,conditions[icond],"_",nsub,"s","_",setSize,".csv",sep = "")
  figname <- paste(folder,conditions[icond],"_Linear_sxxx.png",sep = "")
  saveFit <- paste(folder,"error_",conditions[icond], "_linearFitline.csv",sep = "")
  saveR2 <- paste(folder, "error_",conditions[icond],"_r2Linear.csv",sep = "")
  
  ## load data
  errors<- read.csv(errorName,header=F)
  ##change proportion errors to percent
  errors <- errors*100
  
  ##establish matrices
  fitLineStore<- matrix(1,nsub,setSize)
  r2store <- matrix(1,nsub,1)
  
  ##foreach subject
  for (i in 1:nsub)
  {
    num<- seq(1:setSize)
    yE<- as.vector(t(errors[i,1:setSize]))
    ##linear model
    lmOut<-lm(formula = yE ~ num)
    # summary(lm.out)
    ##store fitline and r2-adj values
    fitLineStore[i,1:setSize]<- lmOut$fitted.values
    r2store[i,1]<-summary(lmOut)$adj.r.squared
    
    ## plot errors, fitline, save to png and rename
    png(figname)
    plot(num,yE,main=paste("R2adj: ",round(r2store[i,1],3)), xlab="Set size (Linear)", ylab="Error %")
    abline(lmOut,col='red')
    dev.off()
    sapply(figname,FUN=function(eachPath) {
      file.rename(from=eachPath,to=sub(pattern="xxx",replacement = i,eachPath)) 
    })
  }
  ## write out fitlines and r2  
  write.csv(fitLineStore,file=saveFit)
  write.csv(r2store,file=saveR2)
}
