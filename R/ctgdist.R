
ctgdist <- function(dataset) {

  data<-as.matrix(dataset)
  maxx<-c()
  for(i in 1:ncol(data)){
    maxx[i]<-max(data[,i])
  }
  n<-max(maxx)

  model.gpcm <- paste("liking.science = 1-",ncol(data))
  results.gpcm <- mirt(data, model=model.gpcm, itemtype="gpcm", SE=TRUE, verbose=FALSE)
  coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
  coef.gpcm$item
  a<-matrix(ncol = n-1,nrow =ncol(data))
  for(i in 1:ncol(data)){
    for(j in 1:n-1){
      a[i,j]<-mean(coef.gpcm$items[i,2:n])-coef.gpcm$items[i,j+1]
    }}
  yyy<-c()
  converted<-matrix(ncol = n,nrow =ncol(data))
  normdist<-c()
  fx<-c()
  integral<-c()
  result<-c()
  for(t in 1:ncol(data)){
    for(i in 1:n-1){
      normdist[i]<-1-pnorm(a[t,i])
    }
    for(i in 1:n-1){
      fx[i]<--(1/(sqrt(2*pi)))*exp(-((a[t,i])^2/2))}
    added<-append(normdist,0,after = 0)
    added<-append(added,1,after = n+1)

    for(i in 1:n){
      integral[i]<- added[i]-added[i+1]
    }
    addedfx<-append(fx,0,after = 0)
    addedfx<-append(addedfx,0,after = n+1)

    for(i in 1:n){
      result[i]<-round((addedfx[i]-addedfx[i+1])/integral[i],3)
    }
    scalevalue<-result
    for(i in 1:n-1){
      coeff<-(n-1)/(result[n]-result[1])
      yyy[i]<-round(((result[i+1]-result[1])*coeff+1),3)
    }
    converted[t,]<-append(yyy,1,after = 0)

  }
  convertedscalevalue<-converted
  meancsv<-c()
  for(i in 1:ncol(convertedscalevalue)){
    meancsv[i]<-round(mean(convertedscalevalue[,i]),3)
  }
  convertedscalevalue<-rbind(meancsv,convertedscalevalue)
  Item<-c()
  for(i in 1:ncol(data)){
    Item[i]<- paste(i,".item")
  }
  Item<-append(Item,"Scale CSV",after = 0)
  scores<-matrix(ncol = ncol(data),nrow = nrow(data))
  for(i in 1:ncol(data)){
    for(j in 1:nrow(data)){
      if(data[j,i]==1){
        scores[j,i]<-1
      }
      if(data[j,i]==2){
        scores[j,i]<-convertedscalevalue[i,2]
      }
      if(data[j,i]==3){
        scores[j,i]<-convertedscalevalue[i,3]
      }
      if(data[j,i]==4){
        scores[j,i]<-convertedscalevalue[i,4]
      }
      if(data[j,i]==5){
        scores[j,i]<-convertedscalevalue[i,5]
      }
      if(data[j,i]==6){
        scores[j,i]<-convertedscalevalue[i,6]
      }
      if(data[j,i]==7){
        scores[j,i]<-convertedscalevalue[i,7]
      }
      if(data[j,i]==8){
        scores[j,i]<-convertedscalevalue[i,8]
      }
      if(data[j,i]==9){
        scores[j,i]<-convertedscalevalue[i,9]
      }
    }
  }
  sums<-c()
  for(i in 1:nrow(scores)){
    sums[i]<-round(sum(scores[i,]),2)
  }
  meanscores<-round(mean(sums),2)
  ScaleScore<-append(sums,meanscores,after = 0)
  HundredScale<-c()
  for(i in 1:length(ScaleScore)){
    HundredScale[i]<-round((ScaleScore[i]/(n*ncol(data)))*100,2)
  }

  IndividualNo<-c()
  for(i in 1:nrow(data)){
    IndividualNo[i]<-paste(i,".individual")
  }
  IndividualNo<-append(IndividualNo,"Mean Score",after = 0)
  lastscore<-cbind(IndividualNo,ScaleScore,HundredScale)
  lastscore<-as.data.frame(lastscore)
  lastscore

  son<-list(convertedscalevalue,lastscore)

  return(son)
}
