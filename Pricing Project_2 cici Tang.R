#Load packages
library("data.table")
library("mlogit")
library("gmnl")
library("plotly") #Package for three dimensional plot

rm(list = ls());

#setwd
setwd("/Users/heqian.guan/Library/CloudStorage/OneDrive-UniversityofRochester/12 SpringA MKT440 Pricing Analytics/Project 2")

##question 3.1
#Estimate a multinomial logit model using gmnl and mlogit
#Read data
data=fread("kiwi_bubbles_P2.csv",stringsAsFactors = F)

#Drop observations with stockout
data=data[!(data$price.KB==99),]
data=data[!(data$price.KR==99),]
data=data[!(data$price.MB==99),]

#Now columns 4 through 7 contains "Price.something" info.
mlogitdata=mlogit.data(data,id="id",varying=4:7,choice="choice",shape="wide")

#Run MLE.
mle= gmnl(choice ~  price, data = mlogitdata)
summary(mle)

coef=mle$coefficients

##question 3.2
## calculate the average prices 
avg_priceKB = mean(data$price.KB)
avg_priceKR = mean(data$price.KR)
avg_priceMB = mean(data$price.MB)

##coefficients
beta0KB = 4.25316 
beta0KR = 4.36240
beta0MB = 4.20440
beta1 = -3.73793

##Write choice probability as a function
demandKB=function(priceKB,priceKR,priceMB,beta0KB,beta0KR,beta0MB,beta1){
    prob=exp(beta0KB+beta1*priceKB)/(1+exp(beta0KB+beta1*priceKB)+exp(beta0KR+beta1*priceKR)+exp(beta0MB+beta1*priceMB))
    return(prob)
}

demandKR=function(priceKB,priceKR,priceMB,beta0KB,beta0KR,beta0MB,beta1){
    prob=exp(beta0KR+beta1*priceKR)/(1+exp(beta0KB+beta1*priceKB)+exp(beta0KR+beta1*priceKR)+exp(beta0MB+beta1*priceMB))
    return(prob)
}

demandMB=function(priceKB,priceKR,priceMB,beta0KB,beta0KR,beta0MB,beta1){
    prob=exp(beta0MB+beta1*priceMB)/(1+exp(beta0KB+beta1*priceKB)+exp(beta0KR+beta1*priceKR)+exp(beta0MB+beta1*priceMB))
    return(prob)
}

##own-elasticity
own_KB = -beta1 * avg_priceKB * (1-demandKB(avg_priceKB,avg_priceKR,avg_priceMB,beta0KB,beta0KR,beta0MB,beta1))
own_KB

own_KR = -beta1 * avg_priceKR * (1-demandKR(avg_priceKB,avg_priceKR,avg_priceMB,beta0KB,beta0KR,beta0MB,beta1))
own_KR

own_MB = -beta1 * avg_priceMB * (1-demandMB(avg_priceKB,avg_priceKR,avg_priceMB,beta0KB,beta0KR,beta0MB,beta1))
own_MB

##cross-elasticity
#from KB
cross_KB = -beta1 * avg_priceKB * demandKB(avg_priceKB,avg_priceKR,avg_priceMB,beta0KB,beta0KR,beta0MB,beta1)
cross_KB

#from KR
cross_KR = -beta1 * avg_priceKR * demandKR(avg_priceKB,avg_priceKR,avg_priceMB,beta0KB,beta0KR,beta0MB,beta1)
cross_KR

#from MB
cross_MB = -beta1 * avg_priceMB * demandMB(avg_priceKB,avg_priceKR,avg_priceMB,beta0KB,beta0KR,beta0MB,beta1)
cross_MB

##question 3.3
##unit price
uc = 0.5

##price range of KB KR
price_specific_MB = 1.43

#Choose space of prices to search for the optimal price over
aux=seq(1,3,0.01)
#Because we search over two dimensions, create complete combination 
#of the two prices
pricespace=expand.grid(aux,aux)

#profit
profitmat=matrix(0L,nrow(pricespace),1)

for (i in 1:nrow(pricespace)){
    
profitmat[i] = 1000*demandKB(pricespace[i,1],pricespace[i,2],price_specific_MB,beta0KB,beta0KR,beta0MB,beta1)*(pricespace[i,1] - uc)+1000*
    demandKR(pricespace[i,1],pricespace[i,2],price_specific_MB,beta0KB,beta0KR,beta0MB,beta1)*(pricespace[i,2] - uc)
}

#proft maximized prices
pricespace[profitmat==max(profitmat)]

#Draw figure
xaxis=list(title="P^{KB}")
yaxis=list(autorange = "reversed",title="P^{KR}")
zaxis=list(title="Profit")
p=plot_ly(x=pricespace[,1],y=pricespace[,2],z=as.numeric(profitmat),
          type="scatter3d",mode="markers",
          marker = list(color = as.numeric(profitmat), colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))%>%
    layout(scene=list(xaxis=xaxis,yaxis=yaxis,zaxis=zaxis))%>%
    config(mathjax = 'cdn')
p


##question 4.1
#Load demographic data
demo=fread("demo_P2.csv",stringsAsFactors = F)

#Number of individuals
N = 359
#Clustering
demo_cluster = kmeans(x=demo[, 2:18], centers = 8, nstart = 1000)

# now combine cluster identity into the raw data
cluster_id = data.frame(id = demo$id)
cluster_id$cluster = demo_cluster$cluster
data = merge(data, cluster_id, by = "id", all.x = T)

# for those who don't fit in any cluster, group them into one additional cluster
data$cluster[is.na(data$cluster)] = 9

# segment share
seg.share = c( table(demo_cluster$cluster),N - sum(table(demo_cluster$cluster))) / N	
seg.share
# just store the coefficients (you can store many other things)
coef.est = data.frame(segment = 1:9, intercept.KB = NA, intercept.KR = NA, 
                      intercept.MB = NA, price.coef = NA) 

#Write a for-loop. 
for (seg in 1:9) {
    # During each loop, pick subset of data of consumers from each segment.
    data.sub = subset(data, cluster == seg)
    
    #Using that data, the rest remains the same.
    mlogitdata=mlogit.data(data.sub,id="id",varying=4:7,choice="choice",shape="wide")
    
    #Run MLE.
    mle= gmnl(choice ~  price, data = mlogitdata)
    mle
    #Store the outcome in the coef.est matrix.
    coef.est[seg, 2:5] = mle$coefficients
}

coef.est
seg.share

##question 4.2
##Write choice probability as a function
demandKB=function(priceKB,priceKR,priceMB,para){
    prob=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    return(prob)
}

demandKR=function(priceKB,priceKR,priceMB,para){
    prob=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    return(prob)
}

demandMB=function(priceKB,priceKR,priceMB,para){
    prob=exp(para[3]+para[4]*priceMB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    return(prob)
}

#aggregate choice probability for KB
#agg_choiceKB=function(priceKB,priceKR,priceMB) {
    
#    agg_choice=seg.share[1]*demandKB(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))+
#        seg.share[2]*demandKB(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))+
#        seg.share[3]*demandKB(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))+
#        seg.share[4]*demandKB(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))+
#        seg.share[5]*demandKB(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))+ 
#        seg.share[6]*demandKB(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))+
#        seg.share[7]*demandKB(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))+
#        seg.share[8]*demandKB(priceKB,priceKR,priceMB,as.numeric(coef.est[8,2:5]))+
#        seg.share[9]*demandKB(priceKB,priceKR,priceMB,as.numeric(coef.est[9,2:5]))
#    
#    return(agg_choice)
#}
##simplified loop of professor's example
#aggregate choice probability for KB
agg_choiceKB = function(priceKB, priceKR, priceMB) {
    agg_choice = 0
    for (i in 1:length(seg.share)) {
        agg_choice = agg_choice + seg.share[i] * demandKB(priceKB, priceKR, priceMB, as.numeric(coef.est[i, 2:5]))
    }
    return(agg_choice)
}

#own-price elasticity under K-means
#Kmeans_own_KB = (- avg_priceKB/agg_choiceKB(avg_priceKB,avg_priceKR,avg_priceMB))*
#    (seg.share[1]*coef.est[1,5]*demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[1,2:5]))*(1-demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[1,2:5])))+
#    seg.share[2]*coef.est[2,5]*demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[2,2:5]))*(1-demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[2,2:5])))+
#    seg.share[3]*coef.est[3,5]*demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[3,2:5]))*(1-demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[3,2:5])))+
#    seg.share[4]*coef.est[4,5]*demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[4,2:5]))*(1-demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[4,2:5])))+
#    seg.share[5]*coef.est[5,5]*demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[5,2:5]))*(1-demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[5,2:5])))+
#    seg.share[6]*coef.est[6,5]*demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[6,2:5]))*(1-demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[6,2:5])))+
#    seg.share[7]*coef.est[7,5]*demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[7,2:5]))*(1-demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[7,2:5])))+
#    seg.share[8]*coef.est[8,5]*demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[8,2:5]))*(1-demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[8,2:5])))+
#    seg.share[9]*coef.est[9,5]*demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[9,2:5]))*(1-demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[9,2:5]))))
#Kmeans_own_KB

##simplified version of my own codes
#own-price elasticity under K-means for KB
Kmeans_own_KB = 0
for (i in 1:length(seg.share)) {
    Kmeans_own_KB = Kmeans_own_KB +
        (- avg_priceKB/agg_choiceKB(avg_priceKB,avg_priceKR,avg_priceMB))*
        (seg.share[i]*coef.est[i,5]*demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))*(1-demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))))

}
Kmeans_own_KB

#aggregate choice probability for KR
agg_choiceKR = function(priceKB, priceKR, priceMB) {
    agg_choice = 0
    for (i in 1:length(seg.share)) {
        agg_choice = agg_choice + seg.share[i] * demandKR(priceKB, priceKR, priceMB, as.numeric(coef.est[i, 2:5]))
    }
    return(agg_choice)
}

#own-price elasticity under K-means for KR
Kmeans_own_KR = 0
for (i in 1:length(seg.share)) {
    Kmeans_own_KR = Kmeans_own_KR +
        (- avg_priceKR/agg_choiceKR(avg_priceKB,avg_priceKR,avg_priceMB))*
        (seg.share[i]*coef.est[i,5]*demandKR(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))*(1-demandKR(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))))
    
}
Kmeans_own_KR

#aggregate choice probability for MB
agg_choiceMB = function(priceKB, priceKR, priceMB) {
    agg_choice = 0
    for (i in 1:length(seg.share)) {
        agg_choice = agg_choice + seg.share[i] * demandMB(priceKB, priceKR, priceMB, as.numeric(coef.est[i, 2:5]))
    }
    return(agg_choice)
}

#own-price elasticity under K-means for KR
Kmeans_own_MB = 0
for (i in 1:length(seg.share)) {
    Kmeans_own_MB = Kmeans_own_MB +
        (- avg_priceMB/agg_choiceMB(avg_priceKB,avg_priceKR,avg_priceMB))*
        (seg.share[i]*coef.est[i,5]*demandMB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))*(1-demandMB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))))
    
}
Kmeans_own_MB

#cross-price elasticity under K-means for KB from KR
Kmeans_cross_KB_KR = 0
for (i in 1:length(seg.share)) {
    Kmeans_cross_KB_KR = Kmeans_cross_KB_KR +
        (- avg_priceKR/agg_choiceKB(avg_priceKB,avg_priceKR,avg_priceMB))*
        (seg.share[i]*coef.est[i,5]*demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))*(demandKR(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))))
    
}
Kmeans_cross_KB_KR

#cross-price elasticity under K-means for KB from MB
Kmeans_cross_KB_MB = 0
for (i in 1:length(seg.share)) {
    Kmeans_cross_KB_MB = Kmeans_cross_KB_MB +
        (- avg_priceMB/agg_choiceKB(avg_priceKB,avg_priceKR,avg_priceMB))*
        (seg.share[i]*coef.est[i,5]*demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))*(demandMB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))))
    
}
Kmeans_cross_KB_MB

#cross-price elasticity under K-means for KR from KB
Kmeans_cross_KR_KB = 0
for (i in 1:length(seg.share)) {
    Kmeans_cross_KR_KB = Kmeans_cross_KR_KB +
        (- avg_priceKB/agg_choiceKR(avg_priceKB,avg_priceKR,avg_priceMB))*
        (seg.share[i]*coef.est[i,5]*demandKR(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))*(demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))))
    
}
Kmeans_cross_KR_KB

#cross-price elasticity under K-means for KR from MB
Kmeans_cross_KR_MB = 0
for (i in 1:length(seg.share)) {
    Kmeans_cross_KR_MB = Kmeans_cross_KR_MB +
        (- avg_priceMB/agg_choiceKR(avg_priceKB,avg_priceKR,avg_priceMB))*
        (seg.share[i]*coef.est[i,5]*demandKR(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))*(demandMB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))))
    
}
Kmeans_cross_KR_MB

#cross-price elasticity under K-means for MB from KB
Kmeans_cross_MB_KB = 0
for (i in 1:length(seg.share)) {
    Kmeans_cross_MB_KB = Kmeans_cross_MB_KB +
        (- avg_priceKB/agg_choiceMB(avg_priceKB,avg_priceKR,avg_priceMB))*
        (seg.share[i]*coef.est[i,5]*demandMB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))*(demandKB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))))
    
}
Kmeans_cross_MB_KB

#cross-price elasticity under K-means for MB from KR
Kmeans_cross_MB_KR = 0
for (i in 1:length(seg.share)) {
    Kmeans_cross_MB_KR = Kmeans_cross_MB_KR +
        (- avg_priceKR/agg_choiceMB(avg_priceKB,avg_priceKR,avg_priceMB))*
        (seg.share[i]*coef.est[i,5]*demandMB(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))*(demandKR(avg_priceKB,avg_priceKR,avg_priceMB,as.numeric(coef.est[i,2:5]))))
    
}

Kmeans_cross_MB_KR

coef.est

seg.share

#4.3
#Scatterplot of parameters - beta_0^{KB}-beta_0^{KR} against beta_1.
plot(coef.est[1,2]-coef.est[1,3],coef.est[1,5],cex=30*seg.share[1],xlim=c(-3,3),ylim=c(-9,-1.5),
     col = "blue",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta_0^KB-beta_0^KR",ylab=("beta_1"))
points(coef.est[2,2]-coef.est[2,3],coef.est[2,5],cex=20*seg.share[2],col = "red",pch=16)
points(coef.est[3,2]-coef.est[3,3],coef.est[3,5],cex=20*seg.share[3],col = "green",pch=16)
points(coef.est[4,2]-coef.est[4,3],coef.est[4,5],cex=20*seg.share[4],col = "cyan",pch=16)
points(coef.est[5,2]-coef.est[5,3],coef.est[5,5],cex=20*seg.share[5],col = "magenta",pch=16)
points(coef.est[6,2]-coef.est[6,3],coef.est[6,5],cex=20*seg.share[6],col = "yellow",pch=16)
points(coef.est[7,2]-coef.est[7,3],coef.est[7,5],cex=20*seg.share[7],col = "black",pch=16)
points(coef.est[8,2]-coef.est[8,3],coef.est[8,5],cex=20*seg.share[8],col = "gray",pch=16)
points(coef.est[9,2]-coef.est[9,3],coef.est[9,5],cex=20*seg.share[9],col = "brown",pch=16)

#Scatterplot of parameters - beta_0^{KB}-beta_0^{MB} against beta_1.
plot(coef.est[1,2]-coef.est[1,4],coef.est[1,5],cex=20*seg.share[1],xlim=c(-3,3),ylim=c(-9,-1.5),
     col = "blue",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta_0^KB-beta_0^MB",ylab=("beta_1"))
points(coef.est[2,2]-coef.est[2,4],coef.est[2,5],cex=20*seg.share[2],col = "red",pch=16)
points(coef.est[3,2]-coef.est[3,4],coef.est[3,5],cex=20*seg.share[3],col = "green",pch=16)
points(coef.est[4,2]-coef.est[4,4],coef.est[4,5],cex=20*seg.share[4],col = "cyan",pch=16)
points(coef.est[5,2]-coef.est[5,4],coef.est[5,5],cex=20*seg.share[5],col = "magenta",pch=16)
points(coef.est[6,2]-coef.est[6,4],coef.est[6,5],cex=20*seg.share[6],col = "yellow",pch=16)
points(coef.est[7,2]-coef.est[6,4],coef.est[6,5],cex=20*seg.share[7],col = "black",pch=16)
points(coef.est[8,2]-coef.est[6,4],coef.est[6,5],cex=20*seg.share[8],col = "gray",pch=16)
points(coef.est[9,2]-coef.est[6,4],coef.est[6,5],cex=20*seg.share[9],col = "brown",pch=16)

#Scatterplot of parameters - beta_0^{MB}-beta_0^{KR} against beta_1.
plot(coef.est[1,4]-coef.est[1,3],coef.est[1,5],cex=20*seg.share[1],xlim=c(-3,3),ylim=c(-9,-1.5),
     col = "blue",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta_0^MB-beta_0^KR",ylab=("beta_1"))
points(coef.est[2,4]-coef.est[2,3],coef.est[2,5],cex=20*seg.share[2],col = "red",pch=16)
points(coef.est[3,4]-coef.est[3,3],coef.est[3,5],cex=20*seg.share[3],col = "green",pch=16)
points(coef.est[4,4]-coef.est[4,3],coef.est[4,5],cex=20*seg.share[4],col = "cyan",pch=16)
points(coef.est[5,4]-coef.est[5,3],coef.est[5,5],cex=20*seg.share[5],col = "magenta",pch=16)
points(coef.est[6,4]-coef.est[6,3],coef.est[6,5],cex=20*seg.share[6],col = "yellow",pch=16)
points(coef.est[7,4]-coef.est[6,3],coef.est[6,5],cex=20*seg.share[7],col = "black",pch=16)
points(coef.est[8,4]-coef.est[6,3],coef.est[6,5],cex=20*seg.share[8],col = "gray",pch=16)
points(coef.est[9,4]-coef.est[6,3],coef.est[6,5],cex=20*seg.share[9],col = "brown",pch=16)

##################question4.4
##profit under segmentation with KB
##unit price
uc = 0.5

##price range of KB KR
price_specific_MB = 1.43

#Choose space of prices to search for the optimal price over
aux=seq(1,3,0.01)
#Because we search over two dimensions, create complete combination 
#of the two prices
pricespace=expand.grid(aux,aux)

#profit
profitmat1=matrix(0L,nrow(pricespace),1)
profitmatMB=matrix(0L,nrow(pricespace),1)

for (i in 1:nrow(pricespace)){
    
    profitmat1[i] = 1000*agg_choiceKB(pricespace[i,1], pricespace[i,2], price_specific_MB)*(pricespace[i,1] - uc)+
        1000*agg_choiceKR(pricespace[i,1], pricespace[i,2], price_specific_MB)*(pricespace[i,2] - uc)
}

#proft maximized prices
pricespace[profitmat1==max(profitmat1)]
profitmat1[profitmat1==max(profitmat1)]

#profit of MB
for (i in 1:nrow(pricespace)){
    profitmatMB[i] = 1000*agg_choiceMB(pricespace[i,1], pricespace[i,2], price_specific_MB)*(price_specific_MB - uc)
}
#proft maximized prices
profitmatMB[profitmatMB==max(profitmatMB)]

###########profit under segmentation without KB
#choice probability without KB 
demandKR_noKB=function(priceKR,priceMB,para){
    prob=exp(para[2]+para[4]*priceKR)/(1+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    return(prob)
}

demandMB_noKB=function(priceKR,priceMB,para){
    prob=exp(para[3]+para[4]*priceMB)/(1+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    return(prob)
}

#aggregate choice probability for KR without KB
agg_choiceKR_noKB = function(priceKR, priceMB) {
    agg_choice = 0
    for (i in 1:length(seg.share)) {
        agg_choice = agg_choice + seg.share[i] * demandKR_noKB(priceKR, priceMB, as.numeric(coef.est[i, 2:5]))
    }
    return(agg_choice)
}


#aggregate choice probability for MB without KB
agg_choiceMB_noKB = function(priceKR, priceMB) {
    agg_choice = 0
    for (i in 1:length(seg.share)) {
        agg_choice = agg_choice + seg.share[i] * demandMB_noKB(priceKR, priceMB, as.numeric(coef.est[i, 2:5]))
    }
    return(agg_choice)
}

##price space
pricespace_noKB = seq(1,3,0.01)

#profit
profitmat2=matrix(0L,length(pricespace_noKB),1)
profitmatMB_noKB=matrix(0L,length(pricespace_noKB),1)

#profit for KR
for (i in 1:length(pricespace_noKB)){
    
    profitmat2[i] = 1000*agg_choiceKR_noKB(pricespace_noKB[i], price_specific_MB)*(pricespace_noKB[i] - uc)
}


#proft maximized prices
pricespace_noKB[profitmat2==max(profitmat2)]
profitmat2[profitmat2==max(profitmat2)]

#profit of MB
#profit of MB
for (i in 1:length(pricespace_noKB)){
    profitmatMB_noKB[i] = 1000*agg_choiceMB_noKB(pricespace_noKB[i], price_specific_MB)*(price_specific_MB - uc)
}
#proft maximized prices
profitmatMB_noKB[profitmatMB_noKB==max(profitmatMB_noKB)]

#################question 5.1
##price range of KB KR
price_specific_KB = 1.15
price_specific_KR = 1.19

##price space
pricespace_5_1 = seq(0,3,0.01)

#profit
profitmatMB_5_1=matrix(0L,length(pricespace_5_1),1)

#profit of MB
for (i in 1:length(pricespace_5_1)){
    profitmatMB_5_1[i] = 1000*agg_choiceMB(price_specific_KB, price_specific_KR, pricespace_5_1[i])*(pricespace_5_1[i] - uc)
}
#proft maximized prices
pricespace_5_1[profitmatMB_5_1==max(profitmatMB_5_1)]
profitmatMB_5_1[profitmatMB_5_1==max(profitmatMB_5_1)]

#################question 5.2
##price range of KB KR
price_specific_MB = 0.96

#Choose space of prices to search for the optimal price over
aux=seq(0,3,0.01)
#Because we search over two dimensions, create complete combination 
#of the two prices
pricespace=expand.grid(aux,aux)

#profit
profitmat_5_2=matrix(0L,nrow(pricespace),1)

for (i in 1:nrow(pricespace)){
    
    profitmat_5_2[i] = 1000*agg_choiceKB(pricespace[i,1], pricespace[i,2], price_specific_MB)*(pricespace[i,1] - uc)+
        1000*agg_choiceKR(pricespace[i,1], pricespace[i,2], price_specific_MB)*(pricespace[i,2] - uc)
}

#proft maximized prices
pricespace[profitmat_5_2==max(profitmat_5_2)]
profitmat_5_2[profitmat_5_2==max(profitmat_5_2)]

#######################question 5.3
#round 1
##price range of KB KR
price_specific_KB = 1.02
price_specific_KR = 1.08

##price space
pricespace_5_1 = seq(0,3,0.01)

#profit
profitmatMB_5_1=matrix(0L,length(pricespace_5_1),1)

#profit of MB
for (i in 1:length(pricespace_5_1)){
    profitmatMB_5_1[i] = 1000*agg_choiceMB(price_specific_KB, price_specific_KR, pricespace_5_1[i])*(pricespace_5_1[i] - uc)
}
#proft maximized prices
pricespace_5_1[profitmatMB_5_1==max(profitmatMB_5_1)]
profitmatMB_5_1[profitmatMB_5_1==max(profitmatMB_5_1)]

#round2
##price range of KB KR
price_specific_MB = 0.92

#Choose space of prices to search for the optimal price over
aux=seq(1,3,0.01)
#Because we search over two dimensions, create complete combination 
#of the two prices
pricespace=expand.grid(aux,aux)

#profit
profitmat_5_2=matrix(0L,nrow(pricespace),1)

for (i in 1:nrow(pricespace)){
    
    profitmat_5_2[i] = 1000*agg_choiceKB(pricespace[i,1], pricespace[i,2], price_specific_MB)*(pricespace[i,1] - uc)+
        1000*agg_choiceKR(pricespace[i,1], pricespace[i,2], price_specific_MB)*(pricespace[i,2] - uc)
}

#proft maximized prices
pricespace[profitmat_5_2==max(profitmat_5_2)]
profitmat_5_2[profitmat_5_2==max(profitmat_5_2)]

#round 3
price_specific_KB = 1.01
price_specific_KR = 1.07

##price space
pricespace_5_1 = seq(0,3,0.01)

#profit
profitmatMB_5_1=matrix(0L,length(pricespace_5_1),1)

#profit of MB
for (i in 1:length(pricespace_5_1)){
    profitmatMB_5_1[i] = 1000*agg_choiceMB(price_specific_KB, price_specific_KR, pricespace_5_1[i])*(pricespace_5_1[i] - uc)
}
#proft maximized prices
pricespace_5_1[profitmatMB_5_1==max(profitmatMB_5_1)]
profitmatMB_5_1[profitmatMB_5_1==max(profitmatMB_5_1)]

#round 4
##price range of KB KR
price_specific_MB = 0.91

#Choose space of prices to search for the optimal price over
aux=seq(0,3,0.01)
#Because we search over two dimensions, create complete combination 
#of the two prices
pricespace=expand.grid(aux,aux)

#profit
profitmat_5_2=matrix(0L,nrow(pricespace),1)

for (i in 1:nrow(pricespace)){
    
    profitmat_5_2[i] = 1000*agg_choiceKB(pricespace[i,1], pricespace[i,2], price_specific_MB)*(pricespace[i,1] - uc)+
        1000*agg_choiceKR(pricespace[i,1], pricespace[i,2], price_specific_MB)*(pricespace[i,2] - uc)
}

#proft maximized prices
pricespace[profitmat_5_2==max(profitmat_5_2)]
profitmat_5_2[profitmat_5_2==max(profitmat_5_2)]

#round 5
##price range of KB KR
price_specific_KB = 1
price_specific_KR = 1.07

##price space
pricespace_5_1 = seq(0,3,0.01)

#profit
profitmatMB_5_1=matrix(0L,length(pricespace_5_1),1)

#profit of MB
for (i in 1:length(pricespace_5_1)){
    profitmatMB_5_1[i] = 1000*agg_choiceMB(price_specific_KB, price_specific_KR, pricespace_5_1[i])*(pricespace_5_1[i] - uc)
}
#proft maximized prices
pricespace_5_1[profitmatMB_5_1==max(profitmatMB_5_1)]
profitmatMB_5_1[profitmatMB_5_1==max(profitmatMB_5_1)]

#round 6
##price range of KB KR
price_specific_MB = 0.91

#Choose space of prices to search for the optimal price over
aux=seq(1,3,0.01)
#Because we search over two dimensions, create complete combination 
#of the two prices
pricespace=expand.grid(aux,aux)

#profit
profitmat_5_2=matrix(0L,nrow(pricespace),1)

for (i in 1:nrow(pricespace)){
    
    profitmat_5_2[i] = 1000*agg_choiceKB(pricespace[i,1], pricespace[i,2], price_specific_MB)*(pricespace[i,1] - uc)+
        1000*agg_choiceKR(pricespace[i,1], pricespace[i,2], price_specific_MB)*(pricespace[i,2] - uc)
}

#proft maximized prices
pricespace[profitmat_5_2==max(profitmat_5_2)]
profitmat_5_2[profitmat_5_2==max(profitmat_5_2)]
