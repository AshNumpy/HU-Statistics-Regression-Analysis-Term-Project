# Importing Data:
library(readr)
hwdata <- read_table2("D:/Hacettepe/Regresyon/Ödev/21821809-Ramazan-Erduran-(veri).txt")
head(hwdata,3)

names(hwdata) <- c("y", "x1", "x2", "x3", "x4") 
hwdata$x4 <- as.factor(hwdata$x4)
class(hwdata$x4)


# Test of Normality:
attach(hwdata)

pCompare <- function(dataSet, alpha=0.05) {
  p_value <- dataSet$p.value
  if (p_value < alpha) {
    result <- "H0 reject"
  }
  else if (p_value >= alpha) {
    result <- "H0 cant reject"
  }
  return(result)
}

pCompare(shapiro.test(y),alpha=0.05)
boxplot(y, col = "red") # Seems like there is some residuals

Q1 <- quantile(y, 0.25)
Q3 <- quantile(y, 0.75)
IQR <- IQR(y)
newData <- subset(hwdata, y > (Q1 - 1.5*IQR) & y < (Q3 + 1.5*IQR))
boxplot(newData$y, col="green") # Thats cool now
detach(hwData)

qqnorm(newData$y)
qqline(newData$y)
pCompare(shapiro.test(newData$y), alpha = 0.05)


# Linearity:
pairs(newData[,1:4])

library(corrplot)
cors <- cor(newData[,1:4])
corrplot.mixed(cors[,1:4], lover = "number", upper = "pie")


# Residuals:
attach(newData)
linearModel <- lm(y~x1+x2+x3+x4)
infoGraph <- ls.diag(linearModel)

outliersStd <- as.data.frame(list(infoGraph$std.res>2 & infoGraph$std.res<(-2)))
sum(outliersStd)

outliersStud <- as.data.frame(list(infoGraph$std.res>3 & infoGraph$std.res<(-3)))
sum(outliersStud)

