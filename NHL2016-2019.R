primarydatamaster <- rbind(primarydata1, primarydata2, primarydata3)

##Master data
datamaster <- rbind(data1, data2, data3)

library (dplyr)

datamasterD <- filter(datamaster, Pos == "D")
datamasterO <- filter(datamaster, Pos == "C" | Pos == "RW" 
                      | Pos == "LW")

##Analyzing Goals predicted by Age

plot(datamaster$Age, datamaster$G, 
     col = as.factor(datamaster$Pos),
     xlab = "Age", ylab = "Goals",
     main = "Scatterplot of Goals vs Age Colored by Position")
legend("topright", legend = c("C", "RW", "LW", "D"), 
       fill = c("Black","Blue","Green","Red"), 
       ncol = 2, cex = 0.5, x.intersp = 0.4, 
       text.width = 0.5)

plot(datamaster$Age, datamaster$G)
fitbasicM <- lm(G ~ Age, data = datamaster)
summary(fitbasicM)
abline(fitbasicM)

##Goals vs Age D
plot(datamasterD$Age, datamasterD$G, xlab = "Age",
     ylab = "Goals", 
     main = "Scatterplot of Goals vs Age for Defensemen")
fitDM <- lm(G ~ Age, data = datamasterD)
summary(fitDM)
abline(fitDM)

##Goals vs Age D Diagnostic
  ##Goals vs Age Predictor

boxplot(datamasterD$Age, ylab = "Age",
        main = "Boxplot for Predictor Variable Age")

  ##Goals vs Age Res

resAgeD <- residuals(fitDM)
plot(datamasterD$Age, resAgeD, xlab = "Age", ylab = "Residual",
     main = "Scatter plot of Residuals vs Predictor Variable Age")
abline(h = 0)
hist(resAgeD, xlab = "Residual", 
     main = "Histogram of Residuals")
qqnorm(resAgeD)
qqline(resAgeD)

##Final Goals vs Age D
datamasterDFinal <- filter(datamasterD, Age < 40)
datamasterDFinal <- filter(datamasterDFinal, G < 29)
plot(datamasterDFinal$Age, datamasterDFinal$G, 
     xlab = "Age", ylab = "Goals", 
     main = "Scatterplot of Goals vs Age for Defensemen")
fitDMFinal <- lm(G ~ Age, data = datamasterDFinal)
summary(fitDMFinal)
abline(fitDMFinal)
anova(fitDMFinal)

##Goals vs Age O

plot(datamasterO$Age, datamasterO$G, 
     xlab = "Age", ylab = "Goals", 
     main = "Scatterplot of Goals vs Age for Forwards")
fitOM <- lm(G ~ Age, data = datamasterO)
summary(fitOM)
abline(fitOM)

##Goals vs Age O Diagnostic
  
  ##Goals vs Age O Predictor

boxplot(datamasterO$Age, ylab = "Age", 
        main = "Boxplot for Predictor Variable Age")

  ##Goals vs Age O Res
resAgeOM <- residuals(fitOM)

plot(datamasterO$Age, resAgeOM, 
     xlab = "Age", ylab = "Residual",
     main = "Scatterplot of Residual vs Predictor Variable Age")
abline(h=0)
hist(resAgeOM, xlab = "Residual", 
     main = "Histogram of Residuals")
qqnorm(resAgeOM)
qqline(resAgeOM)

##Final Goals vs Age O

datamasterOFinal <- filter(datamasterO, Age < 40)
plot(datamasterOFinal$Age, datamasterOFinal$G,
     xlab = "Age", ylab = "Goals",
     main = "Scatterplot of Goals vs Age of Forwards")
fitOMFinal <- lm(G ~ Age, data = datamasterOFinal)
summary(fitOMFinal)
abline(fitOMFinal)
anova(fitOMFinal)

##Appears that after age 31, there is not a sharp decrease in scoring
datamaster0Below33 <- filter(datamasterO, Age < 33)
plot(datamaster0Below33$Age, datamaster0Below33$G)
fitOMBelow33 <- lm(G ~ Age, data = datamaster0Below33)
summary(fitOMBelow33)
abline(fitOMBelow33)

datamasterOAbove32 <- filter(datamasterO, Age > 32)
plot(datamasterOAbove32$Age, datamasterOAbove32$G)
fitOMAbove32 <- lm(G ~ Age, data = datamasterOAbove32)
summary(fitOMAbove32)
abline(fitOMAbove32)

##Biggest outliers are 32, 33 year old Alex Ovechkin,
##23 year old Leon Draisaitl, 28 year old John Tavares


##Testing different models based on Age
datamaster0Below27 <- filter(datamasterO, Age < 27)
plot(datamaster0Below27$Age, datamaster0Below27$G)
fitOMBelow27 <- lm(G ~ Age, data = datamaster0Below27)
summary(fitOMBelow27)
abline(fitOMBelow27)

datamasterO27to32 <- filter(datamasterO, Age > 26)
plot(datamasterO27to32$Age, datamasterO27to32$G)
fitOMBetween2732 <- lm(G ~ Age, datamasterO27to32)
summary(fitOMBetween2732)
abline(fitOMBetween2732)

datamasterOAbove32 <- filter(datamasterO, Age > 32)
plot(datamasterOAbove32$Age, datamasterOAbove32$G)
fitOMAbove32 <- lm(G ~ Age, data = datamasterOAbove32)
summary(fitOMAbove32)
abline(fitOMAbove32)


##Analyzing Assists predicted by Age

plot(datamaster$Age, datamaster$A, 
     col = as.factor(datamaster$Pos))

plot(datamaster$Age, datamaster$A)
assistfitM <- lm(A ~ Age, data = datamaster)
summary(assistfitM)
abline(assistfitM)

plot(datamasterD$Age, datamasterD$A)
assistfitD <- lm(A ~ Age, data = datamasterD)
summary(assistfitD)
abline(assistfitD)

plot(datamasterO$Age, datamasterO$A)
assistfitO <- lm(A ~ Age, data = datamasterO)
summary(assistfitO)
abline(assistfitO)

##Analyzing +/- predicted by Age

plot(datamaster$Age, datamaster$X..., 
     col = as.factor(datamaster$Pos))

plot(datamaster$Age, datamaster$X...)
plusfitM <- lm(X... ~ Age, data = datamaster)
summary(plusfitM)
abline(plusfitM)

plot(datamasterD$Age, datamasterD$X...)
plusfitD <- lm(X... ~ Age, data = datamasterD)
summary(plusfitD)
abline(plusfitD)

plot(datamasterO$Age, datamasterO$X...)
plusfitO <- lm(X... ~ Age, data = datamasterO)
summary(plusfitO)
abline(plusfitO)


##Analyzing Goals predicted by TOI

plot(datamaster$TOI, datamaster$G, 
     col = as.factor(datamaster$Pos),
     xlab = "Time On Ice (Minutes)", ylab = "Goals",
     main = "Scatterplot of Goals vs Time On Ice Colored by Position")
legend("topright", legend = c("C", "RW", "LW", "D"), 
       fill = c("Black","Blue","Green","Red"), 
       ncol = 4, cex = 0.3, x.intersp = 0.4, 
       text.width = 0.5)

plot(datamaster$TOI, datamaster$G)
TOIfitbasicM <- lm(G ~ TOI, data = datamaster)
summary(TOIfitbasicM)
abline(TOIfitbasicM)

##Analyzing Goals predicted by TOI Defensemen
plot(datamasterD$TOI, datamasterD$G, 
     xlab = "Time On Ice (Minutes)",  ylab = "Goals",
     main = "Goals vs TOI for Defensemen")
TOIfitDM <- lm(G ~ TOI, data = datamasterD)
summary(TOIfitDM)
abline(TOIfitDM)

##Goals vs TOI D Diagnostic
  ##Goals vs TOI Predictor

boxplot(datamasterD$TOI, ylab = "Time on Ice (Minutes)",
        main = "Boxplot for Predictor Variable TOI")

  ##Goals vs TOI Res

resTOID <- residuals(TOIfitDM)
plot(datamasterD$TOI, resTOID, 
     xlab = "Time On Ice (Minutes)", ylab = "Residual",
     main = "Scatter plot of Residuals vs Predictor Variable TOI")
abline(h = 0)
hist(resTOID, xlab = "Residual", 
     main = "Histogram of Residuals")
qqnorm(resTOID)
qqline(resTOID)

##Final Goals vs TOI D

TOIdatamasterDfinal <- filter(datamasterD, TOI > 1000)
TOIdatamasterDfinal <- filter(TOIdatamasterDfinal, G < 29)

plot(TOIdatamasterDfinal$TOI, TOIdatamasterDfinal$G,
     xlab = "Time On Ice (Minutes)", ylab = "Goals",
     main = "Goals vs TOI for Defensemen")
finalTOIfitD <- lm(G ~ TOI, data = TOIdatamasterDfinal)
summary(finalTOIfitD)
abline(finalTOIfitD)
anova(finalTOIfitD)

newval <- data.frame(TOI = 1700)
predict(finalTOIfitD, newdata = newval, interval = "prediction")

##Analyzing Goals predicted by TOI Forwards
plot(datamasterO$TOI, datamasterO$G,
     xlab = "Time On Ice (Minutes)", ylab = "Goals",
     main = "Goals vs TOI for Forwards")
TOIfitOM <- lm(G ~ TOI, data = datamasterO)
summary(TOIfitOM)
abline(TOIfitOM)

##Goals vs TOI O Diagnostic

  ##Goals vs TOI O Predictor

boxplot(datamasterO$TOI, ylab = "Time On Ice (Minutes)", 
        main = "Boxplot for Predictor Variable TOI")

  ##Goals vs TOI O Res
resTOIO <- residuals(TOIfitOM)

plot(datamasterO$TOI, resTOIO, 
     xlab = "Time On Ice (Minutes)", ylab = "Residual",
     main = "Scatterplot of Residual vs Predictor Variable TOI")
abline(h=0)
hist(resTOIO, xlab = "Residual", 
     main = "Histogram of Residuals")
qqnorm(resTOIO)
qqline(resTOIO)

##Transform TOIO

trans <- data.frame(datamasterO$TOI)
trans <- 1/(trans)
newtrans <- data.frame(datamasterO$Player, datamasterO$G, trans)
plot(newtrans$datamasterO.TOI, newtrans$datamasterO.G)
transfit <- lm(datamasterO.G ~ datamasterO.TOI, data = newtrans)
summary(transfit)
abline(transfit)

  ##Diagnostic Plot TOI
boxplot(trans$datamasterO.TOI)

  ##Diagnostic Plot Res
restrans <- residuals(transfit)
plot(newtrans$datamasterO.TOI, restrans,
     xlab = "1/TOI", ylab = "Residual",
     main = "Residual vs 1/(TOI) for Forwards")

##Transform GoalsO
ytrans <- data.frame(datamasterO$G)
ytrans <- sqrt(ytrans)
newtransy <- data.frame(datamasterO$Player, ytrans, datamasterO$TOI)
plot(newtransy$datamasterO.TOI, newtransy$datamasterO.G)
ytransfit <- lm(datamasterO.G ~ datamasterO.TOI, data = newtransy)
summary(ytransfit)
abline(ytransfit)
anova(ytransfit)

  ##Diagnostic Plot TOI
  ##Diagnostic Plot Res
ytransres <- residuals(ytransfit)
plot(x = newtransy$datamasterO.TOI, y = ytransres,
     xlab = "TOI", ylab = "Residuals",
     main = "Residuals vs TOI for Sqrt(Goals) Transform")
hist(ytransres, xlab = "Residuals",
     main = "Histogram of Residuals")
qqnorm(ytransres)
qqline(ytransres)
##Final Transform Goals vs TOI
fintransy <- data.frame(datamasterO$Player, datamasterO$Age,
                        ytrans, datamasterO$TOI)
fintransy <- filter(fintransy, datamasterO.TOI > 600)
plot(fintransy$datamasterO.TOI, fintransy$datamasterO.G,
     xlab = "Time on Ice", ylab = "Sqrt(Goals)",
     main = "Transform of Goals vs TOI for Forwards")
finytransfit <- lm(datamasterO.G ~ datamasterO.TOI, data = fintransy)
summary(finytransfit)
abline(finytransfit)
anova(finytransfit)



##Analyzing Assists predicted by TOI

plot(datamaster$TOI, datamaster$A, 
     col = as.factor(datamaster$Pos))

plot(datamaster$TOI, datamaster$A)
TOIassistfitM <- lm(A ~ TOI, data = datamaster)
summary(TOIassistfitM)
abline(TOIassistfitM)

plot(datamasterD$TOI, datamasterD$A)
TOIassistfitD <- lm(A ~ TOI, data = datamasterD)
summary(TOIassistfitD)
abline(TOIassistfitD)

plot(datamasterO$TOI, datamasterO$A)
TOIassistfitO <- lm(A ~ TOI, data = datamasterO)
summary(TOIassistfitO)
abline(TOIassistfitO)

##Analyzing +/- predicted by TOI

plot(datamaster$TOI, datamaster$X..., 
     col = as.factor(datamaster$Pos))

plot(datamaster$TOI, datamaster$X...)
TOIplusfitM <- lm(X... ~ TOI, data = datamaster)
summary(TOIplusfitM)
abline(TOIplusfitM)

plot(datamasterD$TOI, datamasterD$X...)
TOIplusfitD <- lm(X... ~ TOI, data = datamasterD)
summary(TOIplusfitD)
abline(TOIplusfitD)

plot(datamasterO$TOI, datamasterO$X...)
TOIplusfitO <- lm(X... ~ TOI, data = datamasterO)
summary(TOIplusfitO)
abline(TOIplusfitO)

##Analyzing Goals predicted by Age AND TOI

AgeTOIfit <- lm(G ~ TOI + Age, data = datamaster)
summary(AgeTOIfit)
anova(AgeTOIfit)

##Analyzing Goals predicted by Age AND TOI for Defensemen

AgeTOIfitD <- lm(G ~ TOI + Age, data = datamasterD)
summary(AgeTOIfitD)
anova(AgeTOIfitD)
plot(AgeTOIfitD)

##Final Analyzing Goals predicted by Age AND TOI for Defensemen
multD <- filter(datamasterD, TOI > 1000, G < 29, Age < 40)
FinAgeTOIfitD <- lm(G ~ TOI + Age, data = multD)
summary(FinAgeTOIfitD)
anova(FinAgeTOIfitD)
plot(FinAgeTOIfitD)

##Analyzing Goals predicted by Age AND TOI for Forwards

multO <- fiter(datamasterO, Age < 40, TOI > 700)
AgeTOIfitO <- lm(G ~ TOI + Age, data = multO)
summary(AgeTOIfitO)
anova(AgeTOIfitO)
plot(AgeTOIfitO)

##Transform Goals predicted by Age AND TOI Forwards
multtransy <- filter(fintransy, datamasterO.Age < 40, 
                     datamasterO.TOI > 700)
TransAgeTOIfitO <- lm(datamasterO.G ~ datamasterO.TOI 
                      + datamasterO.Age, data = fintransy)
summary(TransAgeTOIfitO)
anova(TransAgeTOIfitO)
plot(TransAgeTOIfitO)


cfintransy = data.frame(fintransy$datamasterO.Age, 
                        fintransy$datamasterO.G,
                        fintransy$datamasterO.TOI)
pairs(cfintransy)
test <- lm(fintransy.datamasterO.Age
           ~ fintransy.datamasterO.TOI, data = cfintransy)
anova(test)

##3D Graph of Goals predicted by Age AND TOI

rm(list=ls())
library("scatterplot3d")
library(reshape2)

library("plotly")

graph_resofit <- 0.05

x_axisfit <- seq(min(datamaster$TOI), max(datamaster$TOI),
                 by = graph_resofit)
y_axisfit <- seq(min(datamaster$Age), max(datamaster$Age),
                 by = graph_resofit)
fit_lm_surface <- expand.grid(Age = x_axisfit, 
                              TOI = y_axisfit,
                              KEEP.OUT.ATTRS = F)
fit_lm_surface$G <- predict.lm(AgeTOIfit, 
                               newdata = fit_lm_surface)
fit_lm_surface <- acast(fit_lm_surface, Age ~ TOI, 
                        value.var = "G")

out_plotfit <- plot_ly(datamaster, x = ~TOI, y = ~Age, z = ~G) %>%
  add_markers(color="red") %>%
  layout(scene = list(xaxis = list(title = 'Time on Ice'),
                      yaxis = list(title = 'Age'),
                      zaxis = list(title = 'Goals')))
out_plotfit

out_plotfit <- add_trace(p = out_plotfit,
                        z = fit_lm_surface,
                        x = axis_x,
                        y = axis_y,
                        type = "surface")
out_plotfit
