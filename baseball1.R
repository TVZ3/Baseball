library(Lahman)

data(Batting)
names(Batting)
nrow(Batting)  ## 101332
ncol(Batting) ## 22
library(dplyr)
library(tidyr)

# 1 Using Lahman MLB data in R, list the top 5 teams since 2000 with the largest stolen bases per at bat ratio
### Creating a workable data frame
df <- Batting
df1 <- newdata <- subset(df, yearID >= 2000) # Select years equal to, or greater than 2000
SBperAB <- df1$SB/df1$AB # Create the "Stolen bases per at bat" ratio variable
df1 <- data.frame(df1, SBperAB) # Add the "Stolen bases per at bat" variable to the data frame
names(df1)
df1[!is.finite(df1$SBperAB),"SBperAB"] <- 0  # Call "inf" values "0"
nrow(df1) ## 22084
df1<-data.frame(df1)
write.csv (df1, file = "test.csv",row.names=FALSE)
### Create an object containing  teamID, yearID, and Stolen bases per at bat that is sorted by teamID (alphabetically) and yearID (descending)
test <- aggregate(SBperAB ~ teamID+yearID, data=df1, FUN=mean)
head(test)

### Sort data frame by year (descending) and Stolen bases per at bat (ascending)
sort1 <- test[order(test$yearID, -test$SBperAB), ]
#### Sorting by Stolen bases per at bat shifts rows around, so I rename them here
rownames(sort1) <- c(1:nrow(sort1))
sort2 <- aggregate(sort1[,"teamID"] ~ yearID, data=sort1, FUN=head)
sort2
sort2 <- data.frame(sort2[,1],sort2$sort1[,1:5]) ## Did this because the "FUN=head" above gives the top 6 teams per year, and I want the top 5
colnames(sort2) <- c("yearID","1st","2nd","3rd","4th","5th") ## Making the table "presentable"

### Final answer for Q1, displaying the 5 teams with the best Stolen Bases per At Bat values for each year between 2000 to 2015
sort2


# 2. Using this same Batting data, plot the yearly SBperAB rate.  This will be computed over the entire year rather than per team-year as above

yearlyABperSB <- aggregate(SBperAB ~ yearID, data=df1, FUN=mean)
plot(yearlyABperSB, type = "o", pch=19, xlab="Year", ylab="Annual Rate of Stolen Bases per At Bat")

# 2a.  Same plot but color each plot by lgID (LeagueID).  For this problem we only care about NL and AL, everything else can be filtered out.
yearlyABperSB2 <- aggregate(SBperAB ~ yearID*lgID, data=df1, FUN=mean)

yearlyABperSB2
ALdiv <- rbind(yearlyABperSB2[1:16,])
NLdiv <- rbind(yearlyABperSB2[17:32,])


plot(yearlyABperSB2$yearID, yearlyABperSB2$SBperAB, col=as.factor(yearlyABperSB2$lgID), pch=19, xlab="Year", ylab="Annual Rate of Stolen Bases per At Bat")
lines(SBperAB ~ yearID, col=as.factor(lgID), data = subset(yearlyABperSB2, lgID=="AL"), lty = 'solid')
lines(SBperAB ~ yearID, col=as.factor(lgID), data = subset(yearlyABperSB2, lgID=="NL"), lty = 'solid')
legend(2000,0.015, c("AL Division","NL Division"), pch=19, col=c(2,5), bty="n")

# 3. Use this Year, SB.PerAB dataset (generated in #2 above) to create a model for how year relates to SB.PerAB.  In this problem you are using only yearID to predict SB.PerAB.  Try a few model fits and determine which one is best.

hist(yearlyABperSB$SBperAB) ## determine destribution of SBperAB
library(randomForest)
library(gbm) ### Sample size ended up being too small to run a boosted regression
library(tseries)

### General linear regression model
mod1 <- lm(SBperAB ~ yearID, data=yearlyABperSB)
summary(mod1)
#### The Rsquared value of this model is a 0.1

### Random Forest model
mod2 <- randomForest(SBperAB ~ yearID, data = yearlyABperSB)
print(mod2)
#### The Rsquared value of this model is a negative number, which indicates that the random forest is not explaining any variation between Year and SBperAB

### Time-series model --- autoregressive moving average (ARMA)
yearlyABperSB
plot(yearlyABperSB,type="l",main="Raw Data for SBperAB" ,xlab="Time",ylab="SBperAB")
par(mfrow=c(2,1))
#### Determine if SBperAB is temporally autocorrelated
acf(yearlyABperSB[,2],main="Interpret the ARMA Order")
pacf(yearlyABperSB[,2],main="")
#### We can see that SBperAB is not correlated through time. Normally this would indicate that running time-series models would not be appropriate
#### I'll run a couple anyway just to show I can
(mod3 <- arima(yearlyABperSB[,2],order=c(1,0,0))) # Better model, due to lower AIC value (-164.32 is lower than -162.51)
(mod4 <- arima(yearlyABperSB[,2],order=c(1,0,1)))

#### Both of these models don't really work, but mod3 is stronger than mod4 because it has a lower AIC value
#### The R-square of this model (its fit) would be 0.19^2, which means, Rsquared=0.036

### The fit of these models is extremely low. Low fit makes sense, as Stolen Bases per At Bat should not be dependent on Year. With that said however, the best model is "mod1".  I would not trust any of these models in a predictive capacity though



# 4 A baseball player is said to be continuously playing if he's playing for consequent years. Given the years that some baseball player played in, write the function activeYears which computes the sequence of the lengths of the continuous playing. 

activeYears <- function(x) {
  cumdiff<- cumsum(c(1, diff(x)>1))
  vals <- rle(cumdiff)$lengths
  return(vals)
}
years <- c(1994,1995,1996,1998,1999,2000,2001,2003,2004,2006)
(pleasework <- activeYears(years))


activeYears(df1[df1$playerID=="aardsda01", "yearID"])
df1[df1$playerID=="aardsda01", "yearID"] ### Qualitatively just making sure it works