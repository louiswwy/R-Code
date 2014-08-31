###########安装包############
# run only once
# install.packages("ggplot2")
# install.packages("knitr")
# install.packages("data.table")
# install.packages("gpairs")
# install.packages("plotrix")
# install.packages("pvclust")
# install.packages("cluster")
# install.packages("fpc")
# install.packages("scatterplot3d")
# install.packages("rgl")
# install.packages(arules)
# install.packages("igraph")
# source("http://bioconductor.org/biocLite.R")
# biocLite("RBGL")

######载入包##########
#ggplot
library("ggplot2")
#SVM包
#library("kernlab")
#贝叶斯包
#library("bnlearn")
#Stringr包
library("stringr")
#knitr包
library("knitr")
#data.table包
library("data.table")
library("gpairs")
library("plotrix")
#常用聚类算法
library("pvclust")
library("cluster")
#3d绘图
library("rgl")
#关联规则
library(Matrix)
library(arules)
#字符串处理
library("stringr")
#ff
library("bit")
library("ff")

library("igraph")

##############导入HTTP数据#######################
data_DisHttp<-read.table("201406191100-ltehttpwap-sig13-11675500972.DAT"
                         ,header=TRUE,sep="|",fill=TRUE,colClasses="character",quote="",comment.char="")

#######数据清理############
######删除格式错误数据
Delerror<-(data_DisHttp$EndTime!='')
data_HTTP<-data_DisHttp[Delerror,]
rm(Delerror)

#########删除流程持续时间为0,上行在线时长/下行在线时长为0的项
HttpTime<-((data_HTTP$UpTime!='0')&(data_HTTP$DownTime!='0'))  #(data_HTTP$ProcedureTime!=0)&
data_HTTP<-data_HTTP[HttpTime,]
rm(HttpTime)

########数据信息##########
#L4协议中‘0’表示TCP协议数据，‘1’表示UDP协议数据
L4<-(data_HTTP$L4=='1')
data_udp<-data_HTTP[L4,]
cat("共有：",nrow(data_HTTP),"条数据，其中udp数据包含：",nrow(data_udp),"条。")
rm(L4,data_udp)
#Timestamp Conversion

#数据记录时间
cat("业务最早开始于：",as.character(min(as.POSIXlt(as.numeric(substr(data_HTTP$StartT,1,10)),"UTC", origin="1970-01-01"))),"")
cat("业务最晚开始与：",as.character(max(as.POSIXlt(as.numeric(substr(data_HTTP$StartT,1,10)),"UTC", origin="1970-01-01"))),"")
cat("业务最早结束于：",as.character(min(as.POSIXlt(as.numeric(substr(data_HTTP$StopT,1,10)),"UTC", origin="1970-01-01"))),"")
cat("业务最晚结束于: ",as.character(max(as.POSIXlt(as.numeric(substr(data_HTTP$StopT,1,10)),"UTC", origin="1970-01-01"))),"")

StartTimes <-as.POSIXlt(as.numeric(substr(data_HTTP$StartT,1,10)),"UTC", origin="1970-01-01")
StartMilliseconds <-as.numeric(substr(data_HTTP$StartT,11,13))/1000
EndTimes   <-as.POSIXlt(as.numeric(substr(data_HTTP$StopT,1,10)),"UTC", origin="1970-01-01")
EndMilliseconds   <-as.numeric(substr(data_HTTP$StopT,11,13))/1000
RecodeTime <-data.frame(StartTimes,StartMilliseconds,EndTimes,EndMilliseconds)

# attach(RecodeTime) 
cat("Data recoded in:",max(RecodeTime$EndTimes)-min(RecodeTime$StartTimes)," minits")
# detach(RecodeTime)

########数据提取##########
#######上行平均带宽
upAvBand<-as.numeric(data_HTTP$UpTraffic)/as.numeric(data_HTTP$UpTime)
data_HTTP<-data.frame(data_HTTP,upAvBand)
rm(upAvBand)

#######下行平均带宽
downAvBand<-as.numeric(data_HTTP$DownTraffic)/as.numeric(data_HTTP$DownTime)
data_HTTP<-data.frame(data_HTTP,downAvBand)
rm(downAvBand)

########FirstRespondTime、LastPacketTime、LastAckTime
firstRespondTime<-(as.numeric(data_HTTP$FirstRespondTime))
data_HTTP<-data.frame(data_HTTP,firstRespondTime)

lastPacketTime<-(as.numeric(data_HTTP$LastPacketTime))
data_HTTP<-data.frame(data_HTTP,lastPacketTime)

lastAckTime<-(as.numeric(data_HTTP$LastAckTime))
data_HTTP<-data.frame(data_HTTP,lastAckTime)
rm(firstRespondTime,lastPacketTime,lastAckTime)

################聚类准备##################
Data_PreAnalyse<-data_HTTP[c('upAvBand','downAvBand','firstRespondTime','lastPacketTime','lastAckTime')]  #,  'UperrorRate','DownerrorRate'

Data_PreAnalyse$firstRespondTime   <-as.numeric(Data_PreAnalyse$firstRespondTime)
Data_PreAnalyse$lastPacketTime     <-as.numeric(Data_PreAnalyse$lastPacketTime)
Data_PreAnalyse$lastAckTime        <-as.numeric(Data_PreAnalyse$lastAckTime)

####draw in 3d
# attach(Data_PreAnalyse)
# plot3d(upAvBand,downAvBand,firstRespondTime,xlab="upAvBand",ylab="downAvBand",zlab="FirstRespondTime")
# plot3d(upAvBand,downAvBand,lastPacketTime,xlab="upAvBand",ylab="downAvBand",zlab="LastPacketTime")
# plot3d(upAvBand,downAvBand,lastAckTime,xlab="upAvBand",ylab="downAvBand",zlab="LastAckTime")
# plot3d(FirstRespondTime,LastPacketTime,LastAckTime,xlab="HttpFirstRespondTime(MS)",ylab="HttpLastPacketTime(MS)",zlab="HttpLastAckTime(MS)")
# detach(Data_PreAnalyse)
# Data_Scaled<-data.frame(scale(Data_PreAnalyse))
Data_Anayse<-Data_PreAnalyse

# 对数据进行聚类处理，k值为数据条数的r次方，r默认值为0.5， r值取0到1之间
ClusterC<-function(data,r=0.5){
#   向上取整
  n<-trunc(nrow(data)^r)
  cla<-kmeans(data,n)
  return(cla)
}

system.time(ClusteRes<-ClusterC(Data_Anayse))



# 数据与类结合
Data_Cluster<-data.frame(Data_Anayse,ClusteRes$cluster)

attach(Data_Anayse)
plot3d(upAvBand,downAvBand,firstRespondTime,size=3,col=ClusteRes$cluster) 
plot3d(firstRespondTime,lastPacketTime,lastAckTime,size=3,col=ClusteRes$cluster)
detach(Data_Anayse)

ClusterCenter<-data.frame(ClusteRes$centers)

# 系统内建距离矩阵函数‘dist’
system.time(Matrix <- (as.matrix(dist(ClusterCenter, method = "euclidean",diag = FALSE, upper = FALSE)))) 

# 生成包含 '距离'，'中心点1'，'中心点2'的data.frame
distMatrix<-function(data){
  #   生成矩阵  
  mat<-matrix('NA',nrow=nrow(data),ncol=nrow(data))  
  num<-c()
  r  <-c()
  co <-c()  
  i<-1
  for(x in 1:194){
    for(y in 1:194){ #nrow(data)           
      if(x==y){
        mat[x,y]<-0
      }
      else{
        if(mat[x,y]=='NA' && mat[y,x]=='NA'){
          mat[x,y]<-(sum((data[y,]-data[x,])^2))^0.5
          num[i]<-mat[x,y]
          r[i]  <-x
          co[i] <-y
          #data<-data.frame(row,data)     
          i<-i+1
        }
      }
    }
  }  
  dataR<-data.frame(num,r,co)
  return(dataR)
}

system.time(Juli<-distMatrix(ClusterCenter))

max(as.numeric(as.character(Juli[,1])))
min(as.numeric(as.character(Juli[,1])))
# 由因子转换成数字
toNumeric<-function(data){
  data[,1]<-round(as.numeric(as.character(data[,1])),4)
  data[,2]<-(as.numeric(data[,2]))
  data[,3]<-(as.numeric(data[,3]))
  return(data)
}
NumMatrix<-toNumeric(Juli)
names(NumMatrix) <- c("weight","from", "to" )


g<-graph.data.frame(NumMatrix[,-1],vertices=c(1:194),directed=F)
E(g)$weight <- NumMatrix[,1]
V(g)$size <-1

mst<-minimum.spanning.tree(g,algorithm='prim' )
plot(mst,main="MST")

#生成edges list 连接列表
mst.edges <- as.data.frame(get.edgelist(mst))
mst.edges[,1]<-as.numeric(as.character(mst.edges[,1]))
mst.edges[,2]<-as.numeric(as.character(mst.edges[,2]))  
  


findDistance<-function(edge,distance){
  f<-as.numeric(edge[,1])
  t<-as.numeric(edge[,2])
  matrix<-matrix('NA',nrow=nrow(edge),ncol=1)  
  for(i in 1:nrow(edge)){
    findF<-NumMatrix$from==f[i]
    From <- NumMatrix[findF,]
    findT<-From$to==t[i]
    FromTo<-From[findT,]    
    matrix[i]<-as.numeric(FromTo[1,1])
  }
  edgeDistance<-data.frame(edge,matrix)
  return(edgeDistance)
}

Edge_Distance<-findDistance(mst.edges,NumMatrix)
typeof(Edge_Distance[,3])

# 排序
SortDistance<-Edge_Distance[order(as.numeric(as.character(Edge_Distance[,3])), decreasing = TRUE),]

# 距离最远的C-1条边为
topRow<-function(data,C){
  C<-C-1
  top<-matrix('NA',nrow=C,ncol=3)  
  for(i in 1:C){
    top[i,1]<-data[i,1]
    top[i,2]<-data[i,2]
    top[i,3]<-as.numeric(as.character(data[i,3]))
  }
  return(top)
}

#删除的点数
C=180
BigDistance<-topRow(SortDistance,C)

BigDistance

# topg<-graph.data.frame(BigDistance[,-3],vertices=c(1:194),directed=F)
# V(topg)$size <- 1
# plot(topg,main="Deleted row")

# 删除权重最高的C-1项
DeletRow<-function(data,C){
  C<-C-1
  for(i in 1:C-1){
    data<-data[-1,]
  }
  return(data)
}

NewDistance<-DeletRow(SortDistance,C)

newg<-graph.data.frame(NewDistance[,-3],vertices=c(1:194),directed=F)
V(newg)$size <- 1
plot(newg,main="MST when deleting the 180 longest distance ")






