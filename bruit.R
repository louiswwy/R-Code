###########错误率，正确率####################
# Data.PreAnal$upAvBand   <-Data.PreAnal$upAvBand   /max(Data.AnA$upAvBand) 
# Data.PreAnal$downAvBand <-Data.PreAnal$downAvBand /max(Data.AnA$downAvBand) 

# Data_PreAnalyse$UperrorRate   <-1-Data_PreAnalyse$UperrorRate
# Data_PreAnalyse$DownerrorRate <-1-Data_PreAnalyse$DownerrorRate

# Data_PreAnalyse$FirstRespondTime   <-Data_PreAnalyse$FirstRespondTime/(max(Data_PreAnalyse$FirstRespondTime) )
# Data_PreAnalyse$LastPacketTime     <-Data_PreAnalyse$LastPacketTime  /(max(Data_PreAnalyse$LastPacketTime) )
# Data_PreAnalyse$LastAckTime        <-Data_PreAnalyse$LastAckTime     /(max(Data_PreAnalyse$LastAckTime) )
# ######改名
# names(Data_PreAnalyse)[names(Data_PreAnalyse)=="UperrorRate"]="UpCorrecteRate";
# names(Data_PreAnalyse)[names(Data_PreAnalyse)=="DownerrorRate"]="DownCorrecteRate";

# ############PCA#############
#属性不多，且各个属性均很重要。放弃
# Data.AnAP3<-data.frame(scale(Data.AnA))
# Scoreplot<-PCA(Data.AnAP3,scale.unit=TRUE,graph=FALSE)
# scores<-data.frame(Scoreplot$ind$coord)
# ggplot(scores,aes(Dim.1,Dim.2)) + geom_text(label=rownames(scores),colour="red") + geom_hline(yintercept=0) + geom_vline(xintercept=0) + labs(title="Score plot")
# 
# 
# pca<-prcomp(Data.AnAP3,scale=TRUE)#,tol=.0,,scale=TRUE
# summary(pca)
# plot(pca)
# plot(pca, type='l')
# barplot(pca$sdev/pca$sdev[1])
# #biplot(pca,main="双标图")
# #相关矩阵
# dcor<-cor(Data.AnAP3)
# dcor
# #求相关矩阵的特征向量，特征值
# deig<-eigen(dcor)
# deig
# #输出特征值
# deig$values
# sumeigv<-sum(deig$values)
# sumeigv
# sum(deig$value[1:4])/sumeigv
# #前6个主成份的累计贡献率达到90.7%的
# #输出前六个主成分的荷载系数（特征向量）
# #pca$loadings[,1:4]
# pca$sdev
# #pca$x
# head(pca$x)
# pca2<-prcomp(Data.AnAP3,scale=TRUE,tol=0.7)
# plot(pca2)
# newDat<-predict(pca2)
# # newDat2<-predict(pca,Data.AnAP2)
# # newdat<-pca$x[,1:2]
# pairs(pca2$x,main="Principal Component Analysis")
# attach(pca2)
# plot3d(PC1,PC2,PC3)
# pca2$rotation
# # pload<-abs(pca2$rotation)
# # sweep(pload,2,colSums(pload),"/")#the proportional contribution to the each principal component
# data_pca2<-data.frame(pca2$x)
# plot(data_pca2,pch=16,col=rgb(0,0,0,0.5))
# attach(data_pca2)
# # plot3d(PC1,PC2,PC3xlab="Component 1", ylab="Component 2",zlab="Component 3", col="blue",box=TRUE, size=1) 
# # plot3d(PC3,PC4,PC5)


# ###########统计信息##########
# summary(Data.AnA$UpCorrecteRate)
# summary(Data.AnA$DownCorrecteRate)
# 
# summary(Data.AnA$upAvBand)
# summary(Data.AnA$downAvBand)
# 
# summary(Data.AnA$FirstRespondTime)
# summary(Data.AnA$LastPacketTime)
# summary(Data.AnA$LastAckTime)
# 
# boxplot(Data.AnA$UpCorrecteRate)
# boxplot(Data.AnA$DownCorrecteRate)
# 
# boxplot(Data.AnA$upAvBand)
# boxplot(Data.AnA$downAvBand)
# 
# boxplot(Data.AnA$FirstRespondTime)
# boxplot(Data.AnA$LastPacketTime)
# boxplot(Data.AnA$LastAckTime)
# 
# mean(Data.AnA$UpCorrecteRate)
# mean(Data.AnA$DownCorrecteRate)
# 
# mean(Data.AnA$upAvBand)
# mean(Data.AnA$downAvBand)
# 
# mean(Data.AnA$FirstRespondTime)
# mean(Data.AnA$LastPacketTime)
# mean(Data.AnA$LastAckTime)
# 
# 
# # sd(Data.AnA$UpCorrecteRate)
# # sd(Data.AnA$DownCorrecteRate)
# # 
# # sd(Data.AnA$upAvBand)
# # sd(Data.AnA$downAvBand)
# # 
# # sd(Data.AnA$FirstRespondTime)
# # sd(Data.AnA$LastPacketTime)
# # sd(Data.AnA$LastAckTime)
# 

# #####直方图#############
# 
# #HistP<-ggplot(data=Data.AnA)
# #binsize<-diff(range(Data.AnA$UpCorrecteRate))/15
# #HistP<-HistP+geom_histogram(aes(x=UpCorrecteRate),binwidth =binsize, fill = "light green", colour = "red")
# #HistP
# 
# hist(Data.AnA$UpCorrecteRate)
# hist(Data.AnA$DownCorrecteRate)
# 
# hist(Data.AnA$upAvBand)
# hist(Data.AnA$downAvBand)
# 
# hist(Data.AnA$FirstRespondTime)
# hist(Data.AnA$LastPacketTime)
# hist(Data.AnA$LastAckTime)

# # ######核密度函数##################
# # density(Data.AnA$UpCorrecteRate)
# # density(Data.AnA$DownCorrecteRate)
# # 
# # density(Data.AnA$upAvBand)
# # density(Data.AnA$downAvBand)
# # 
# # density(Data.AnA$FirstRespondTime)
# # density(Data.AnA$LastPacketTime)
# # density(Data.AnA$LastAckTime)
# 

# #########QQ散点图###########
# qqnorm(Data.AnA$UpCorrecteRate,main='UpCorrecteRate')
# qqnorm(Data.AnA$DownCorrecteRate,main='DownCorrecteRate')
# 
# qqnorm(Data.AnA$upAvBand,main='upAvBand')
# qqnorm(Data.AnA$downAvBand,main='downAvBand')
# 
# qqnorm(Data.AnA$FirstRespondTime,main='FirstRespondTime')
# qqnorm(Data.AnA$LastPacketTime,main='LastPacketTime')
# qqnorm(Data.AnA$LastAckTime,main='LastAckTime')
# 

# ###############---###################
# Data.AnA2<-Data.AnA
# 
# ####可以使用的距离算法有"Hartigan-Wong", "Lloyd", "Forgy","MacQueen"四种
# ####   ,nstart=5, iter.max = 10
# PCAkm<-kmeans(newDat,5,nstart=25, iter.max = 10, algorithm = "Hartigan-Wong")
# plot(newDat,col=PCAkm$cluster)
# plot(PCAkm$centers)
# 
# 
# km2<-kmeans(Data.AnA2,5,nstart=25, iter.max = 10, algorithm = "Hartigan-Wong")
# plot(Data.AnA2,col=km2$cluster)
# ClusterDef2 <-function(Data,km){  
#   Data$cluster=factor(km$cluster)
#   Data.AnAP2<<-Data
#   
#   data=as.data.frame(km$centers)
#   Lev2<-rank(data[,2])
#   data<-cbind(data,Lev2)
#   centers2<<-data
#   for(i in 1:nrow(data)){  
#     cat(i)
#     cat(sprintf(": 类%s :", data[i,8])) #%s %f输出变量
#     switch(as.character(data[i,8]),
#            "1" =  {
#              cat("level 1.")
#              KM1<-(Data.AnAP2$cluster==i)  #函数内部强赋值
#              data.KM1<<-Data.AnAP2[KM1,]
#              writeLines("")
#              cat("总计：",nrow(data.KM1),"条")
#            },
#            
#            "2" =  {
#              cat("level 2.")
#              KM2<-(Data.AnAP2$cluster==i)
#              data.KM2<<-Data.AnAP2[KM2,]
#              writeLines("")
#              cat("总计：",nrow(data.KM2),"条")
#            },
#            
#            "3" =  {
#              cat("level 3.")
#              KM3<-(Data.AnAP2$cluster==i)
#              data.KM3<<-Data.AnAP2[KM3,]
#              writeLines("")
#              cat("总计：",nrow(data.KM3),"条")
#            },
#            
#            "4" =  {
#              cat("level 4.")
#              KM4<-(Data.AnAP2$cluster==i)
#              data.KM4<<-Data.AnAP2[KM4,]
#              writeLines("")
#              cat("总计：",nrow(data.KM4),"条")
#            },
#            
#            "5" =  {
#              cat("level 5.")
#              KM5<-(Data.AnAP2$cluster==i)
#              data.KM5<<-Data.AnAP2[KM5,]
#              writeLines("")
#              cat("总计：",nrow(data.KM5),"条")
#            }  
#     )
#     writeLines("")  #换行
#   }
# }
# 
# ClusterDef2(Data.AnA2,km2)
# plot(Data.AnA2,col = km2$cluster)
# # Lv2KM<-kmeans(data.KM1,5,nstart=25)
# # plot(data.KM1,col = Lv2KM$cluster)
# # 
# # ClusterDef2(data.KM1,Lv2KM)

# ############tu####################
# 
# p<-ggplot(data=Data.AnAP2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster ))
# p<-p+geom_point()
# p<-p+ xlim(0,1)+ylim(0,1)
# p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color="",size=5)) 
# p
# 
# p<-ggplot(data=data.KM1, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster ))+ggtitle("KMeans-1")
# p<-p+geom_point()
# p<-p+ xlim(0,1)+ylim(0,1)
# p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color='black',size=5)) 
# p
# 
# p<-ggplot(data=data.KM2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster ))+ggtitle("KMeans-2")
# p<-p+geom_point()
# p<-p+ xlim(0,1)+ylim(0,1)
# p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color='black',size=5)) 
# p
# 
# p<-ggplot(data=data.KM3, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster ))+ggtitle("KMeans-3")
# p<-p+geom_point()
# p<-p+ xlim(0,1)+ylim(0,1)
# p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color='black',size=5)) 
# p
# 
# p<-ggplot(data=data.KM4, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster ))+ggtitle("KMeans-4")
# p<-p+geom_point()
# p<-p+ xlim(0,1)+ylim(0,1)
# p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color='black',size=5)) 
# p
# 
# p<-ggplot(data=data.KM5, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster))+ggtitle("KMeans-5")
# p<-p+geom_point()
# p<-p+ xlim(0,1)+ylim(0,1)
# p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color='black',size=5)) 
# p

# # #######聚类分类######
# # num<-c(1:10)
# # #cbind(centers,num)
# # 
# # Center<-km$centers
# # Lev<-rank(Center[,2])
# # Center<-cbind(Center,Lev)
# # 
# # 
# # 

# # #########饼图##############
# # Values<-c(nrow(data.KM1),nrow(data.KM2),nrow(data.KM3),nrow(data.KM4),nrow(data.KM5),nrow(data.KM6),nrow(data.KM7),nrow(data.KM8),nrow(data.KM9),nrow(data.KM10))
# # Labels<-c("lev1","lev2","lev3","lev4","lev5","lev6","lev7","lev8","lev9","lev10")
# # percent_str <- paste(round(Values/sum(Values) * 100,1), "%", sep="")
# # Values <- data.frame(Percentage <- round(Values/sum(Values) * 100,1), Type = Labels,percent=percent_str )
# # names(Values)<-c("Percentage","Type","percent")
# # 
# # pie <- ggplot(Values, aes(x = "" ,y = Percentage, fill = Labels)) +  geom_bar(stat="identity",width = 3) 
# # pie = pie + coord_polar("y")
# # pie = pie + xlab('') + ylab('') + labs(fill="Types")
# # pie
# # 
# # data.table<- (table(Data.AnAP$cluster))
# # pie(data.table,main="Pie Chart of Cluster")
# # 
# # pic<-ggplot(Values,aes(x=Type,y=Percentage,fill=percent))+geom_bar(stat="identity") 
# # #pic<-pic+ylim(0,nrow(Data.AnAP))
# # pic<-pic+ggtitle("cluster ") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=20))
# # #pic<-pic+geom_text(hjust=0,vjust=-1,alpha=0.8)
# # pic
# # 

# # ########图###########
# # ggplot(data=Data.AnAP, aes(x=upAvBand, y=downAvBand,color=cluster )) + 
# #   geom_point() + 
# #   geom_point(data=centers, aes(x=upAvBand, y=downAvBand,color="#2166AC",size=10)) 
# # 
# # #gpairs(Data.AnAP, upper.pars = list(scatter = 'stats'),
# # #     scatter.pars = list(pch = 1:3,
# # #                           col = as.numeric(Data.AnAP$cluster)))
# 
# 
# #RR<-data.frame(row.names(data_HTTP5))
# # data_HTTP6<-cbind(data_HTTP5,data.frame(row.names(data_HTTP5)))
# # names(data_HTTP6)[names(data_HTTP6)=="row.names.data_HTTP5."]="RowName";
# 

# # ############类1-统计#############
# # R1<-data.frame(row.names(data.KM1))
# # names(R1)[names(R1)=="row.names.data.KM1."]="RowName";
# # Cluster1<- (data_HTTP6[,'RowName']) %in% R1$RowName
# # data.Cluster1<-data_HTTP6[Cluster1,c('eNBip','MMEIP')]
# # 
# # 
# # data.Cluster1$eNBip<-as.numeric(data.Cluster1$eNBip)
# # data.Cluster1$MMEIP<-as.numeric(data.Cluster1$MMEIP)
# # 
# # table1<-data.frame(table(data.Cluster1$eNBip))
# # 
# # table1<-table1[order(table1[,2],decreasing=TRUE),]

# # ###############---##################
# # data.Cluster5$eNBip<-as.numeric(data.Cluster5$eNBip)
# # data.Cluster5$MMEIP<-as.numeric(data.Cluster5$MMEIP)
# # 
# # table5<-data.frame(table(data.Cluster5$eNBip))
# # 
# # table5<-table5[order(table5[,2],decreasing=TRUE),]
# # 