cumu=read.csv(file.choose())
cumu
install.packages("graphics")
install.packages("lattice")
library(graphics)
library(lattice)
library(lme4)
library(nlme)
library(lmerTest)
library(MuMIn)


#LME log
log(mean(cumu$Day))
log(cumu$Day)
mean(log(cumu$Day))
mean(cumu$Day)
log(23.5)
int_slope=lme(log(CumCases)~I(log(Day)-mean(log(Day))),random=~I(log(Day)-mean(log(Day)))|State,cumu)
summary(int_slope)
int_slope
intervals(int_slope)
int_slope_lmlist=lmList(log(CumCases)~I(log(Day)-log(mean(Day)))|State,cumu.grp)
int_slope_lmlist

#trellis
cumu.grp=groupedData(log(CumCases)~I(log(Day)-mean(log(Day)))|State,cumu)
xyplot(log(CumCases)~I(log(Day)-mean(log(Day)))|State,cumu.grp,
       xlab = "Day Centered",ylab = "CumCases",
       panel=function(x,y){
         panel.xyplot(x,y)
         }
       )




