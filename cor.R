# Make sure you have the required packages installed

library(readxl)
library(dplyr)
library(caTools)
library(ggplot2)
library(ggpubr)
library(plot3D)
### Importing the data set *** remember you need to change the path of file *** 

Data<- read_excel("D:/Maths/2nd Year Sem-3/Project/Data set EN.xlsx",sheet = "data_MAproFli_2018-10-11_09-46")

### Removing the first unwanted columns

df <- as.data.frame(Data[-1,-c(1:6,94:116,13,16)])

### Selecting the Market perspective columns in the dataset

mp <- select(df,MS02_01:MS05_08)
mp <- as.data.frame(mp)

### Replacing null values in a column with the median value of the column

for(i in 1:ncol(mp)){
  mp[,i] <- sapply(mp[,i],as.numeric)
  mp[,i] <- replace(mp[,i],is.na(mp[,i]),median(mp[,i],na.rm=T))
}

###
#### Creating mp2 with subcategories  ####

### Create data frame of subcategories of Market perspective with mean of respective
# columns under that subcategory

mp2 <- data.frame(row.names = 1:47)[1:47,]
mp2 <- cbind(mp2,Market.competitors = rowMeans(mp[,c("MS02_01","MS02_02","MS02_09","MS02_10","MS02_11")]))
mp2 <- cbind(mp2,Own.company = round(rowMeans(mp[,c("MS03_12","MS03_13","MS03_14","MS03_15","MS03_16","MS03_17")]),digits=1))
mp2 <- cbind(mp2,Personnel = rowMeans(mp[,c("MS04_20","MS04_19","MS04_18","MS04_21","MS04_22")]))
mp2 <- cbind(mp2,Customers = rowMeans(mp[,c("MS05_03","MS05_05","MS05_06","MS05_07","MS05_08")]))

####
### Appending Data frames mp and mp2 (incase we need it in future)
####

mp <- bind_cols(mp,mp2)

#### Linear models ###

# Not much details added eg. You can create train and test data and play around with those.
model11 <- lm(Market.competitors ~ . ,mp2)
model12 <- lm(Own.company ~ . ,mp2)
model13 <- lm(Personnel ~ . ,mp2)
model14 <- lm(Customers ~ . ,mp2)

# To view model you can try eg. print(summary(model11))
# To plot different plots of your model you can try eg. plot(model11)

#####
### Logistic regression models 
#####

model21 <- glm(Market.competitors/6 ~. , family = binomial(link = "logit"), mp2)

#####
### Plotting mp2 

# Scatterplot

pl1 <- ggplot(mp2,aes(x=Market.competitors,y=Customers,color=Personnel))+ geom_point()
pl1 <- pl1 + scale_color_continuous(high="red",low="green") +theme_bw() +geom_smooth(method="loess",se=F)


# Histogram

pl2 <- ggplot(mp2,aes(x=Market.competitors))+ geom_histogram(bins=15,fill="red")+theme_bw()


# 2D density(filled)

pl3 <- ggplot(mp2,aes(Market.competitors,y=Customers)) + geom_density2d_filled() +theme_bw()

# Density

pl41 <- ggplot(mp2,aes(x=Customers)) + geom_density() + theme_bw()
pl42 <- ggplot(mp2,aes(x=Market.competitors)) + geom_density() + theme_bw()
pl43 <- ggplot(mp2,aes(x=Own.company)) + geom_density() + theme_bw()
pl44 <- ggplot(mp2,aes(x=Personnel)) + geom_density() + theme_bw()

pl4 <- ggarrange(pl41,pl42,pl43,pl44,nrow=2,ncol=2,labels=c("A","B","C","D"))

# 3D plot(without any regression model fitted)
# For more use of plot3D visit :
# http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization

pl5 <- scatter3D(mp2$Own.company , mp2$Personnel, mp2$Market.competitors,phi=0,
                 pch = 20, cex = 1.6, ticktype = "detailed",plot=T,size=3
                 ,xlab="Own.company",ylab="Personnel",zlab="Market.competitors",main="3D Scatterplot",type='b')

# The lines in above plot can be removed by removing or changing the type argument

# 3D plot(with regression)- Visit the above link

# To display above plots just say print(x) where x is the name of the plot













