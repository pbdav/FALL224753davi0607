#This will be a different path if in the lab or at home
dird="C:\\RPACKAGES\\FALL224753davi0607\\MATH 4753 Labs\\Lab3\\"

#my function to read data 
myread=function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
#EASY WAY TO READ IN FILES
fin.df=myread("FINTUBES.csv")
spruce.df=myread("SPRUCE.csv")#MS pg478

# Or use 
#fin.df=read.table(file.choose(),header=TRUE,sep=",")

#Top six lines
head(fin.df)




#initial plot, compare to get perspective ranges and intercepts
with(spruce.df,  
  plot(Height~BHDiameter,bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),
      xlim=c(0,1.1*max(BHDiameter)), main = "Initial Scatter Plot", cex = 1.2)

)


#Load library
#make a new plot
library(s20x)
#trendscatter(Height~BHDiameter,f=0.7, data=spruce.df)

#layout
layout(matrix(1:3,nr=1,nc=3,byrow=TRUE))

#Lets look at where the plots will go
layout.show(3)

#Plot the data
#with(spruce.df, 
#     plot(Height~BHDiameter,bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)))
#)
trendscatter(Height~BHDiameter,f=0.5, data=spruce.df)
trendscatter(Height~BHDiameter,f=0.6, data=spruce.df)
trendscatter(Height~BHDiameter,f=0.7, data=spruce.df)
mtext("Dr Stewart's plot",side=3)


# make a linear model
spruce.lm=with(spruce.df, lm(Height~BHDiameter))

#plot a least squares regression line
#abline(spruce.lm)

#make a new plot
with(spruce.df, 
     plot(Height~BHDiameter,bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)))
)
abline(spruce.lm)


#make a new plot
with(spruce.df, 
     plot(Height~BHDiameter,bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)))
)
abline(spruce.lm)
#make yhat the estimates of E[HEAT | RATIO]
#yhat=with(spruce.df,predict(spruce.lm,data.frame(BHDiameter)))
#OR you could use -- (yhat values the predicted values for all the RATIO values )
yhat=fitted(spruce.lm)

# Draw in segments making the residuals (regression deviations)
with(spruce.df,{
  segments(BHDiameter,Height,BHDiameter,yhat)
})



#make a new plot
with(spruce.df, 
     plot(Height~BHDiameter,bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)))
)

#make nieve model
with(spruce.df, abline(h=mean(Height)))
abline(spruce.lm)

#make the explained deviations (explained by the model)
with(spruce.df, segments(BHDiameter,mean(Height),BHDiameter,yhat,col="Red"))



# Total  error
#make a new plot
with(spruce.df, 
     plot(Height~BHDiameter,bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)))
)
with(spruce.df,abline(h=mean(Height)))
with(spruce.df, segments(BHDiameter,Height,BHDiameter,mean(Height)))




MSS=with(spruce.df,sum((yhat-mean(Height))^2))
MSS





#residual sum of squares
RSS=with(spruce.df,sum((Height-yhat)^2))

RSS


summary(ht.lm)
#intercept =    0.2134
#Slope =     2.4264
# Equation HEAT=0.2134 +2.4264*RATIO
#Get coeffts
coef(ht.lm)

predict(spruce.lm, data.frame(BHDiameter=c(15,18,20)))


#Using a new package
#Download and install ggplot2
library(ggplot2)
g=ggplot(fin.df, aes(x=RATIO,y=HEAT,color=RATIO))
g=g+geom_point() + geom_line()+ geom_smooth(method="lm")
g+ggtitle("HEAT Vs RATIO")

library(ggplot2)
g=ggplot(spruce.df, aes(x=BHDiameter,y=Height,color=Height))
g=g+geom_point() + geom_line()+ geom_smooth(method="lm")
g+ggtitle("Height vs BHDiameter")

