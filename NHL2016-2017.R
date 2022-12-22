library(data.table)

##Read data from Excel
primarydata3 <- readxl::read_xlsx("2016-2017NHLData.xlsx")
str(primarydata3)

##Create ATOI Column
ATOIF <- primarydata3$TOI/primarydata3$GP
ATOI3 <- data.frame(ATOIF)

##Create initial data with ATOI
finaldata3 <- data.frame(primarydata3, ATOI3)

library(dplyr)

##Filter for players that have played 
##at least 75% of the season
data3 <- filter(finaldata3, GP > 71)

data3Defense <- filter(data3, Pos == "D")
data3Offense <- filter(data3, Pos == "C" | Pos == "RW" 
                      | Pos == "LW")

##Analyzing Goals predicted by Age

plot(data3$Age, data3$G, col = as.factor(data3$Pos))

plot(data3$Age, data3$G)
fitBasic3 <- lm(G ~ Age, data = data3)
summary(fitBasic3)
abline(fitBasic3)

plot(data3Defense$Age, data3Defense$G)
fitD3 <- lm(G ~ Age, data = data3Defense)
summary(fitD3)
abline(fitD3)

plot(data3Offense$Age, data3Offense$G)
fitO3 <- lm(G ~ Age, data = data3Offense)
summary(fitO3)
abline(fitO3)

##Analyzing Goals predicted by TOI

plot(data3$TOI, data3$G, col = as.factor(data3$Pos))

plot(data3$TOI, data3$G)
fitTOI3 <- lm(G ~ TOI, data = data3)
summary(fitTOI3)
abline(fitTOI3)

plot(data3Defense$TOI, data3Defense$G)
fitTOID3 <- lm(G ~ TOI, data = data3Defense)
summary(fitTOID3)
abline(fitTOID3)

plot(data3Offense$TOI, data3Offense$G)
fitTOIO3 <- lm(G ~ TOI, data = data3Offense)
summary(fitTOIO3)
abline(fitTOIO3)

resfitOTOI3 <- residuals(fitOTOI3)
plot(data3Offense$TOI, resfitOTOI3)

##Analyze Goals predicted by ATOI

plot(data3$ATOIF, data3$G,
     col = as.factor(data3$Pos))

plot()

##Analyze Goals predicted by Shots

plot(data3$S, data3$G)
plot(data3$S., data3$G)


##Analyze Point Shares predicted by Age

plot(data3$Age, data3$PS, col = as.factor(data3$Pos))

##Analyze Assists predicted by Age

plot(data3$Age, data3$A, col = as.factor(data3$Pos))
fitAssists3 <- lm(A ~ Age, data = data3)
summary(fitAssists3)
abline(fitAssists3)

plot(data3Defense$Age, data3Defense$A)
fitAssistsD3 <- lm(A ~ Age, data = data3Defense)
summary(fitAssistsD3)
abline(fitAssistsD3)

plot(data3Offense$Age, data3Offense$A)
fitAssistsO3 <- lm(A ~ Age, data = data3Offense)
summary(fitAssistsO3)
abline(fitAssistsO3)

##Analyze Points predicted by Age

plot(data3$Age, data3$PTS)
fitPoints3 <- lm(PTS ~ Age, data = data3)
summary(fitPoints3)
abline(fitPoints3)

##Analyze +/- predicted by Age
