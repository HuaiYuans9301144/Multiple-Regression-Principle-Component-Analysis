library(PoEdata)
data(iris) #print data
iris$Species<-as.numeric(iris$Species)
dat<- scale(iris) #standardized data
pca<- princomp(dat, cor=F)
pca #eigenvalue
summary(pca) #summary eigenvalue
pca$loadings #eigenvector
plot(pca,type="line")
print(-1*pca$loadings, digits=8, cutoff=0)
-1*pca$scores #principal components scores
cor(cbind(-1*pca$scores,dat), method='pearson') #loading
biplot(pca)

