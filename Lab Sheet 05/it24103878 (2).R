##1
getwd()

setwd("C:\\Users\\It24103878\\Desktop\\IT24103878")

getwd()

data<- read.table("Data.txt",header = TRUE, sep=",")
data()

fix(data)
##part 01
names(data)<-c("X1","X2")
fix(data)

attach(data)

hist(X2,main="Histogram for no of Shareholders")

hist(X2,main="Histogram for no of Shareholders",breaks = seq(130,270,length=8),right=FALSE)
##02

histogram<-hist(X2,main="Histogram for no of Shareholders",breaks=seq(130,270,length=8),right = FALSE)

##03
breaks <- round(histogram$breaks)
breaks
freq <-histogram$counts
freq
mids <-histogram$mids
mids

classes<-c()
for (i in 1:length(breaks)-1){
  classes [i]<-paste0("[",breaks[i],",",breaks[i+1],"}")
}
cbind(classes=classes,frequency=freq)
##4

lines(mids,freq)
plot(mids,freq,type="l",main="freq polygon for no of shareholders",xlab="shareholders",ylab="frequency",ylim=c(0,max(freq)))
##5
cum.freq <- cumsum(freq)
cum.freq

new <-c()
for (i in 1:length(breaks)){
  if(i==1){
    new[i]=0
  }else{
    new[i]=cum.freq[i-1]
  }
}

plot(breaks,new,type='l',main="cumlative frequency polygon for shareholders",
     xlab = "shareholders",ylab="cumulative frequency",ylim = c(0,max(cum.freq)))

cbind(upper=breaks,cum.freq=new)


