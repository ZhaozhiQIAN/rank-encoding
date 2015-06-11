# V encoding
library(Rcpp)
library(permute)
sourceCpp("src/FindV.cpp")

CreateRank = function(n){
  return (rbind(1:n,allPerms(n)))
}
ranking3 = CreateRank(3)
FindV(ranking3,1:3)
code_mat_manual = matrix(c(0,0,0,0,0,1,0,1,0,0,1,1,1,0,0,1,0,1),ncol=3,byrow=TRUE)
# assert
all(code_mat_manual == EncodeV(ranking3,1:3))

ranking6 = CreateRank(6)
V_mat6 = FindV(ranking6,1:6)
C_mat6 = EncodeV(ranking6,1:6)
C_mat6_diff = EncodeV(ranking6,6:1)
ranking6_third_mode = sample(6,6)
C_mat6_diff2 = EncodeV(ranking6,ranking6_third_mode)
C_mat6c = cbind(C_mat6,C_mat6_diff)
C_mat6cc = cbind(C_mat6c, C_mat6_diff2)
library(Matrix)
rankMatrix(C_mat6)
ncol(C_mat6)
rankMatrix(C_mat6c)
ncol(C_mat6c)
rankMatrix(C_mat6cc)
ncol(C_mat6cc)
plot(colSums(C_mat6c),type="h")

ranks = numeric(720)
for (i in 2:720){
  ranking6_random = ranking6[i,]
  C_mat6_diff = EncodeV(ranking6,ranking6_random)
  Cmat6c = cbind(C_mat6,C_mat6_diff)
  ranks[i] = rankMatrix(Cmat6c)
  if(rankMatrix(Cmat6c)!=ncol(Cmat6c)){
    message("found",ranking6_random)
  }
}

sum(ranks!=22)


ranks2d = matrix(ncol=720,nrow=720)
for (i in 2:720){
  for (j in (i+1):720){
    ranking6_random = ranking6[i,]
    ranking6_random2 = ranking6[j,]
    C_mat6_diff = EncodeV(ranking6,ranking6_random)
    C_mat6_diff2 = EncodeV(ranking6,ranking6_random2)
    Cmat6c = cbind(C_mat6,C_mat6_diff,C_mat6_diff2)
    ranks2d[i,j] = rankMatrix(Cmat6c)
  }
}
ranks2d[is.na(ranks2d)] = 0

for (i in 1:719){
  for (j in (i+1):720){
    ranks2d[j,i] = ranks2d[i,j]
  }
}

HeatMapColor = function(min,max,value){
  spread = max-min
  ratio = (value-min)/spread
  blue = seq_along(value)
  red = blue
  green = blue
  for (i in 1:length(value)){
    if(ratio[i]<0.25){
      blue[i]=1
      green[i]=4*ratio[i]
    } else if (ratio[i]<0.5){
      green[i]=1
      blue[i]=1+4*(min - value[i] + 0.25 * spread) / spread
    } else if (ratio[i]<0.75){
      green[i] = 1
      red[i] = 4 * (value[i] - min - 0.5 * spread) / spread
    } else {
      red[i] = 1
      green[i] = 1 + 4 * (min - value[i] + 0.75 * spread) / spread
    }
  }
  red = (red-min(red))/(max(red)-min(red))
  blue = (blue-min(blue))/(max(blue)-min(blue))
  green = (green-min(green))/(max(green)-min(green))
  browser()
  heatMapColor = rgb(round(red * 255),round( green * 255),round(blue * 255),max=255)
  heatMapColor
}

pl = HeatMapColor(0,33,0:33)
pl = c("#0900E5","#1E00E5","#3301E6","#4801E7","#5D02E7","#7202E8","#8703E9","#9C03EA","#B104EA",
       "#C604EB","#DB05EC","#ED06D5","#EE07C1","#EF07AD","#EF0899","#F00885","#F10971","#F2095E","#F20A4A","#F30B36",
       "#F40B22","#F50C0E","#F51F0C","#F6340D","#F74A0E","#F75F0E","#F8740F","#F98A0F","#FA9F10","#FAB411","#FBCA11",
       "#FCDF12","#FDF413")

heatmap(ranks2d[,rev(1:720)],Rowv=NA,Colv=NA,xlab="Rank2",ylab="Rank3",reorderfun=NULL,
        labRow=NA, labCol=NA,col=pl,scale="none")

length(which(ranks2d==max(ranks2d)))


sum(ranks2d[700:720,700:720]==33)

fullrank2d = matrix(as.numeric(ranks2d==33),ncol=720)

heatmap(fullrank2d[,rev(1:720)],Rowv=NA,Colv=NA,xlab="Rank2",ylab="Rank3",reorderfun=NULL,
        labRow=NA, labCol=NA,col=c("black","white"),scale="none")

sourceCpp("src/GenPath.cpp")
GenPath(5,c(1:3))


