library(data.table)

##Read data from excel
primarydata1 <- readxl::read_xlsx("2018-2019NHLData.xlsx")
str(primarydata1)

##Creating ATOI column
ATOIF <- primarydata1$TOI/primarydata1$GP
ATOI1 <- data.frame(ATOIF)

##Creating initial data with ATOI
finaldata1 <- data.frame(primarydata1, ATOI1)

library(dplyr)

##Filter for players that have played 
##at least 75% of the season
data1 <- filter(finaldata1, GP > 71)

data1Defense <- filter(data1, Pos == "D")
data1Offense <- filter(data1, Pos == "C" | Pos == "RW" 
                       | Pos == "LW")

##Analyzing Goals predicted by Age

plot(data1$Age, data1$G, col = as.factor(data1$Pos))

plot(data1$Age, data1$G)
fitBasic1 <- lm(G ~ Age, data = data1)
summary(fitBasic1)
abline(fitBasic1)

plot(data1Defense$Age, data1Defense$G)
fitD1 <- lm(G ~ Age, data = data1Defense)
summary(fitD1)
abline(fitD1)

plot(data1Offense$Age, data1Offense$G)
fitO1 <- lm(G ~ Age, data = data1Offense)
summary(fitO1)
abline(fitO1)

##Analyzing Goals predicted by TOI

plot(data1$TOI, data1$G, col = as.factor(data1$Pos))
plot(data1$ATOIF, data1$G,
     col = as.factor(data1$Pos))

##Analyze Goals predicted by Shots

plot(data1$S, data1$G)
plot(data1$S., data1$G)

##Analyze Point Shares predicted by Age

plot(data1$Age, data1$PS, col = as.factor(data1$Pos))

##Analyze Assists predicted by Age

plot(data1$Age, data1$A, col = as.factor(data1$Pos))
fitAssists1 <- lm(A ~ Age, data = data1)
summary(fitAssists1)
abline(fitAssists1)

plot(data1Defense$Age, data1Defense$A)
fitAssistsD1 <- lm(A ~ Age, data = data1Defense)
summary(fitAssistsD1)
abline(fitAssistsD1)

plot(data1Offense$Age, data1Offense$A)
fitAssistsO1 <- lm(A ~ Age, data = data1Offense)
summary(fitAssistsO1)
abline(fitAssistsO1)

##Analyze Points predicted by Age

##Analyze +/- predicted by Age
