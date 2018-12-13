#Installing packages - one time
install.packages("dplyr")
install.packages("mvnormtest")
install.packages("pwr")
install.packages("sm")

#### INFM 600 Project ####
df<-read.csv("FinalDataset.csv")
View(FinalDataset)
df4 <- na.omit(FinalDataset)
require(dplyr)
require(mvnormtest)
require(pwr)
require(sm)

#separating variables into different datasets
INSTNM <- as.factor(FinalDataset[,1])
YEAR <- as.factor(FinalDataset[,3])
C100_4			<- as.matrix(FinalDataset[,4])
C150_4          <- as.matrix(FinalDataset[,5])
D150_4          <- as.matrix(FinalDataset[,6])
COMPL_RPY_3YR_RT<- as.matrix(FinalDataset[,8])
COMPL_RPY_5YR_RT<- as.matrix(FinalDataset[,9])
DEBT_N          <- as.matrix(FinalDataset[,10])
DEP_DEBT_MDN    <- as.matrix(FinalDataset[,11])
PCTFLOAN        <- as.matrix(FinalDataset[,12])
NPT41_PUB       <- as.matrix(FinalDataset[,13])
NPT42_PUB       <- as.matrix(FinalDataset[,14])
NPT43_PUB       <- as.matrix(FinalDataset[,15])
NPT44_PUB       <- as.matrix(FinalDataset[,16])
NPT45_PUB       <- as.matrix(FinalDataset[,17])
head(INSTNM)
head(YEAR)

#create R objects for the residuals from each treatment level
resC100_4		   =lm(FinalDataset$C100_4~FinalDataset$INSTNM)$residuals
resC150_4          =lm(FinalDataset$C150_4~FinalDataset$INSTNM)$residuals
resD150_4          =lm(FinalDataset$D150_4~FinalDataset$INSTNM)$residuals
resCOMPL_RPY_3YR_RT=lm(FinalDataset$COMPL_RPY_3YR_RT~FinalDataset$INSTNM)$residuals
resCOMPL_RPY_5YR_RT=lm(FinalDataset$COMPL_RPY_5YR_RT~FinalDataset$INSTNM)$residuals
resDEBT_N          =lm(FinalDataset$DEBT_N~FinalDataset$INSTNM)$residuals
resDEP_DEBT_MDN    =lm(FinalDataset$DEP_DEBT_MDN~FinalDataset$INSTNM)$residuals
resPCTFLOAN        =lm(FinalDataset$PCTFLOAN~FinalDataset$INSTNM)$residuals
#resNPT41_PUB       =lm(FinalDataset$NPT41_PUB~FinalDataset$INSTNM)$residuals
#resNPT42_PUB       =lm(FinalDataset$NPT42_PUB~FinalDataset$INSTNM)$residuals
#resNPT43_PUB       =lm(FinalDataset$NPT43_PUB~FinalDataset$INSTNM)$residuals
#resNPT44_PUB       =lm(FinalDataset$NPT44_PUB~FinalDataset$INSTNM)$residuals
#resNPT45_PUB       =lm(FinalDataset$NPT45_PUB~FinalDataset$INSTNM)$residuals

#checking for normality using qqplots
qqnorm(lm(FinalDataset$C100_4~FinalDataset$INSTNM)$residuals, main="resC100_4		   ", col=4)
qqline(lm(FinalDataset$C100_4~FinalDataset$INSTNM)$residuals, lwd=2,col='gray86')

qqnorm(lm(FinalDataset$C150_4~FinalDataset$INSTNM)$residuals, main="resC150_4          ", col=4)
qqline(lm(FinalDataset$C150_4~FinalDataset$INSTNM)$residuals, lwd=2,col='gray86')

qqnorm(lm(FinalDataset$D150_4~FinalDataset$INSTNM)$residuals, main="resD150_4          ", col=4)
qqline(lm(FinalDataset$D150_4~FinalDataset$INSTNM)$residuals, lwd=2,col='gray86')

qqnorm(lm(FinalDataset$COMPL_RPY_3YR_RT~FinalDataset$INSTNM)$residuals, main="resCOMPL_RPY_3YR_RT", col=4)
qqline(lm(FinalDataset$COMPL_RPY_3YR_RT~FinalDataset$INSTNM)$residuals, lwd=2,col='gray86')

qqnorm(lm(FinalDataset$COMPL_RPY_5YR_RT~FinalDataset$INSTNM)$residuals, main="resCOMPL_RPY_5YR_RT", col=4)
qqline(lm(FinalDataset$COMPL_RPY_5YR_RT~FinalDataset$INSTNM)$residuals, lwd=2,col='gray86')

qqnorm(lm(FinalDataset$DEBT_N~FinalDataset$INSTNM)$residuals, main="resDEBT_N          ", col=4)
qqline(lm(FinalDataset$DEBT_N~FinalDataset$INSTNM)$residuals, lwd=2,col='gray86')

qqnorm(lm(FinalDataset$DEP_DEBT_MDN~FinalDataset$INSTNM)$residuals, main="resDEP_DEBT_MDN    ", col=4)
qqline(lm(FinalDataset$DEP_DEBT_MDN~FinalDataset$INSTNM)$residuals, lwd=2,col='gray86')

qqnorm(lm(FinalDataset$PCTFLOAN~FinalDataset$INSTNM)$residuals, main="resPCTFLOAN        ", col=4)
qqline(lm(FinalDataset$PCTFLOAN~FinalDataset$INSTNM)$residuals, lwd=2,col='gray86')

#qqnorm(lm(FinalDataset$NPT41_PUB~FinalDataset$INSTNM)$residuals, main="resNPT41_PUB       ", col=4)
#qqline(lm(FinalDataset$NPT41_PUB~FinalDataset$INSTNM)$residuals, lwd=2,col='gray86')
#
#qqnorm(lm(FinalDataset$NPT42_PUB~FinalDataset$INSTNM)$residuals, main="resNPT42_PUB       ", col=4)
#qqline(lm(FinalDataset$NPT42_PUB~FinalDataset$INSTNM)$residuals, lwd=2,col='gray86')
#
#qqnorm(lm(FinalDataset$NPT43_PUB~FinalDataset$INSTNM)$residuals, main="resNPT43_PUB       ", col=4)
#qqline(lm(FinalDataset$NPT43_PUB~FinalDataset$INSTNM)$residuals, lwd=2,col='gray86')
#
#qqnorm(lm(FinalDataset$NPT44_PUB~FinalDataset$INSTNM)$residuals, main="resNPT44_PUB       ", col=4)
#qqline(lm(FinalDataset$NPT44_PUB~FinalDataset$INSTNM)$residuals, lwd=2,col='gray86')
#
#qqnorm(lm(FinalDataset$NPT45_PUB~FinalDataset$INSTNM)$residuals, main="resNPT45_PUB       ", col=4)
#qqline(lm(FinalDataset$NPT45_PUB~FinalDataset$INSTNM)$residuals, lwd=2,col='gray86')


#manova test
x <- manova(cbind(
  FinalDataset$C100_4			 ,
  FinalDataset$C150_4          ,
  FinalDataset$D150_4          ,
  FinalDataset$COMPL_RPY_3YR_RT,
  FinalDataset$COMPL_RPY_5YR_RT,
  FinalDataset$DEBT_N          ,
  FinalDataset$DEP_DEBT_MDN    ,
  FinalDataset$PCTFLOAN        ,
  FinalDataset$NPT41_PUB       ,
  FinalDataset$NPT42_PUB       ,
  FinalDataset$NPT43_PUB       ,
  FinalDataset$NPT44_PUB       ,
  FinalDataset$NPT45_PUB       
) ~ FinalDataset$INSTNM, data = FinalDataset)

#running wilks test
summary(x,test="Wilks")

#difference between the response vars
summary.aov(x)

