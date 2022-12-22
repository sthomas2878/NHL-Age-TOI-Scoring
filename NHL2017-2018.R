library(data.table)

##Read data from Excel
primarydata2 <- readxl::read_xlsx("2017-2018NHLData.xlsx")
str(primarydata2)

##Create ATOI Column
ATOIF <- primarydata2$TOI/primarydata2$GP
ATOI2 <- data.frame(ATOIF)

##Creating initial data with ATOI
finaldata2 <- data.frame(primarydata2, ATOI2)

library(dplyr)

##Filter for players that have played 
##at least 75% of the season
data2 <- filter(finaldata2, GP > 71)

data2Defense <- filter(data2, Pos == "D")
data2Offense <- filter(data2, Pos == "C" | Pos == "RW" 
                       | Pos == "LW")

##Analyzing Goals predicted by Age

plot(data2$Age, data2$G, col = as.factor(data2$Pos))

plot(data2$Age, data2$G)
fitBasic2 <- lm(G ~ Age, data = data2)
summary(fitBasic2)
abline(fitBasic2)

plot(data2Defense$Age, data2Defense$G)
fitD2 <- lm(G ~ Age, data = data2Defense)
summary(fitD2)
abline(fitD2)

plot(data2Offense$Age, data2Offense$G)
fitO2 <- lm(G ~ Age, data = data2Offense)
summary(fitO2)
abline(fitO2)

##Analyzing Goals predicted by TOI

plot(data2$TOI, data2$G, col = as.factor(data2$Pos))
plot(data2$ATOIF, data2$G,
     col = as.factor(data2$Pos))

##Analyze Goals predicted by Shots

plot(data2$S, data2$G)
plot(data2$S., data2$G)

##Analyze Point Shares predicted by Age

plot(data2$Age, data2$PS, col = as.factor(data2$Pos))

##Analyze Assists predicted by Age

plot(data2$Age, data2$A, col = as.factor(data2$Pos))
fitAssists2 <- lm(A ~ Age, data = data2)
summary(fitAssists2)
abline(fitAssists2)

plot(data2Defense$Age, data2Defense$A)
fitAssistsD2 <- lm(A ~ Age, data = data2Defense)
summary(fitAssistsD2)
abline(fitAssistsD2)

plot(data2Offense$Age, data2Offense$A)
fitAssistsO2 <- lm(A ~ Age, data = data2Offense)
summary(fitAssistsO2)
abline(fitAssistsO2)

##Analyze Points predicted by Age

##Analyze +/- predicted by Age
