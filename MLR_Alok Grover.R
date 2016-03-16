## Loading library for vif
install.packages("car")
library(car)

## Loading and Cleaning the data

# Pollutant Data

Pollutants<-read.csv("C://R//Master Spreadsheet_Pollutants.csv",na.strings=c("", "NA"), header=T)
PCl<-Pollutants[,c(1,4)]
levels(PCl[,1])
PCl <- subset(PCl, ! PCl$WatershedID  %in% c("CS ", "GC", "MT",  "NHC", "RB",  "SB"))
PCl$WatershedID<-factor(PCl$WatershedID)
PCl <- na.omit(PCl)
AverageCl<-as.data.frame(matrix(0, length(levels(PCl[,1])), 2))
AverageCl[,1]<-as.integer(levels(PCl[,1]))
AverageCl[,2]<-as.integer(with(PCl, tapply(Cl, WatershedID, mean)))
names(AverageCl) <- c("WatershedID","Average of Cl")

# Watershed Data 

WatershedMetrics<-read.csv("C://R//Watershed Metrics.csv",na.strings=c("", "NA"), header=T)
levels(WatershedMetrics$WatershedID)
WMetrics <- subset(WatershedMetrics, ! WatershedMetrics$WatershedID  %in% c("GC", "MT", "RB"))
WMetrics$WatershedID<-factor(WMetrics$WatershedID)
WMetrics<-WMetrics[, c("WatershedID", "PIPE_DENS", "FOR_CLUMPY", "SLOPE_MEAN")]
WMetrics[,1]<-as.integer(levels(WMetrics[,1]))

## Combining the Pollutant and Watershed Data

k<- match(WMetrics$WatershedID, AverageCl$WatershedID)
Data<- cbind(WMetrics, AverageCl[k,-1])
names(Data)[names(Data) == "AverageCl[k, -1]"] <- "Average.of.Cl"
Data["Basin"]<-c("Slate", "Slate","Slate","Slate","Triassic","Slate","Slate","Slate","Slate","Slate","Triassic",
            "Triassic","Triassic","Triassic","Triassic","Triassic","Triassic","Triassic","Triassic","Triassic",
            "Triassic","Triassic")
write.csv(Data,"C://R//Data.csv")


############################      MULTIPLE LINEAR REGRESSION MODEL    #########################

## Loading the Data

Data<-read.csv("C://R//Data.csv", header=T)
source("pair.fun.R")
pairs(Data, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist)

# Checking for normality
qqnorm(Data$Average.of.Cl)
qqline(Data$Average.of.Cl)
shapiro.test(Data$Average.of.Cl)

# Log transforming the data to make it conform closely to normal distribution
qqnorm(log(Data$Average.of.Cl))
qqline(log(Data$Average.of.Cl))
shapiro.test(log(Data$Average.of.Cl))

# Regression Models (Full Model to Reduced Model)

model0<-with(Data, lm(log(Average.of.Cl) ~ PIPE_DENS + FOR_CLUMPY + SLOPE_MEAN + factor(Basin)))
summary(model0)
model1<-with(Data, update(model0, .~. -factor(Basin)))
summary(model1)
model2<-with(Data, update(model1, .~. -PIPE_DENS))
summary(model2)

# AIC Model Comparison
AIC(model0, model1, model2)

# Multicollinearity check
vif(model2)

# Checking the fit of the model
par(mfrow=c(2,2))
plot(model2)

# Calculating coefficients
coeffs<-exp(coefficients(model2))
coeffs
coefs<-coefficients(model2)
coefs

# Plotting graphs
############# FOR_CLUMPY vs Average of Cl

x <- with(Data, seq(min(FOR_CLUMPY), max(FOR_CLUMPY), length=30))
with(Data, plot(FOR_CLUMPY, Average.of.Cl, las = 1, ylab = "Watershed Average Cl, mg/L",
                       xlab = "Forest Clumpiness Index", pch = 21, las=1, cex=1.2, cex.lab=1.4, bg="red"))

with(Data, curve(exp(coefs[1] + x*coefs[2]+ coefs[3]*mean(SLOPE_MEAN)), add=T, lty=2, col="blue"))

############# SLOPE_RANGE vs Average of Cl

x <- with(Data, seq(min(SLOPE_MEAN), max(SLOPE_MEAN), length=30))
with(Data, plot(SLOPE_MEAN, Average.of.Cl, las = 1, ylab = "Watershed Average Cl, mg/L",
                       xlab = "Slope Mean, Degrees", pch = 21, las=1, cex=1.2, cex.lab=1.4, bg="red"))

with(Data, curve(exp(coefs[1] + mean(FOR_CLUMPY)*coefs[2]+ coefs[3]*x), add=T, lty=2, col="blue"))