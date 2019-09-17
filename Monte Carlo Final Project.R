# MA 548 - Monte Carlo Simulation Final Project
# Daniel Peters and Alex Tunnell

# Pricing an ETF of the 10 highest market cap stocks as of 4/12/19
# Weighting based on market cap
# Pricing Asian puts and calls and Lookback calls and puts
# Historical data used from 6/14/16 - 4/11/19

# Lines 22-59   - Read in data, initialize variables
# Lines 60-120  - Simulate ETF behavior
# Lines 121-244 - Simulate ETF behavior under 3 adverse scenarios
# Lines 245-271 - Price Asian puts and calls and Lookback calls and puts
# Lines 272-327 - Examination of Variance Reduction Efforts
# Lines 328-406 - Data manipulation for plotting
# Lines 407-488 - Plotting


setwd("/Users/danielpeters/Desktop/Grad/MFM Spring 2019/Monte Carlo/project/")


# Read in Stock data in from Excel, returns, volatility, etc.
require(readxl)
returns<-read_excel("Stocks_Historical.xlsx", sheet = "Returns")
corr_stressed<-read_excel("Stocks_Historical.xlsx", sheet = "Correlation_Stressed")
corr_regular<-read_excel("Stocks_Historical.xlsx", sheet = "Correlation_Regular")
implied_vol<-read_excel("Stocks_Historical.xlsx", sheet = "Implied Volatility")
weighting<-read_excel("Stocks_Historical.xlsx", sheet = "Market Cap")
Initial_prices<-read_excel("Stocks_Historical.xlsx", sheet = "Initial_Price")
adverse_drift<-read_excel("Stocks_Historical.xlsx", sheet = "Adverse Drift")
ETF_BeforeSim<-read_excel("Stocks_Historical.xlsx", sheet = "Portfolio Value")
ETF_BeforeSim<-data.frame(ETF_BeforeSim)
ETF_BeforeSim<-ETF_BeforeSim[1:(nrow(ETF_BeforeSim)-1),]
# Remove day simulation starts since it will be included in simulation data frame


# Adjusting parameters before simulation
# Set weighting of assets
weighting<-weighting[1,]/sum(weighting[1,])

# Remove column names from correlation matrices
corr_stressed[,1] <- NULL
corr_regular[,1] <- NULL

# Convert correlation array to a matrix
corr_stressed<-as.matrix(corr_stressed)
corr_regular<-as.matrix(corr_regular)

# Performing Cholesky decomposition
Cholesky_Stressed<-t(chol(corr_stressed))
Cholesky_Regular<-t(chol(corr_regular))

# Calulate average historical "drift"
drift<-data.frame()
for(i in 1:10){
  drift[1,i]<-sum(returns[,i+1])/nrow(returns)
}

# Calulate daily implied volatility - 90 day implied volatility scaled to 1 day by sqrt(trading days)
volatility<-(implied_vol)/sqrt(63)


# Loop to simulate
# The outer loop could be a for loop in which case the relative errors wouldn't need to be initialized.
# There would also be no need for a max(of all relative errors) condition or variable
# Using a for loop or while loop all depends on the approach of the programmer and their contraints.

# Inside the loop day=1 is the inital price before the simulation
# Days 2-64 are split into thirds. This doesn't affect the Base_Case case or the worst case
# However, the 1 bad month and 2 bad month scenarios will have a change in drift and correlation midway through
# Each stock's price is updated daily and then the portfolio value is recalculated and stored in a dataframe

# Base_Case case data frame
ETF_Paths<-data.frame()
# Crude approach data frame 
ETF_Paths_Crude<-data.frame()
# Dataframe to store the relative error of the crude and antithetical approaches for each n
RE_compare<-data.frame()


#initialize counter variable and stopping condition
n<-1
relative_error<-100000
relative_error_crude<-100000
re<-max(relative_error,relative_error_crude)
while(re>.05){ 
  # Intial price of portfolio
  ETF_Paths[1,2*n-1]<-sum(Initial_prices*weighting)
  ETF_Paths[1,2*n]<-sum(Initial_prices*weighting)
  ETF_Paths_Crude[1,n]<-sum(Initial_prices*weighting)
  #Create a vector to track prices
  prices<-Initial_prices
  prices_antithetic<-Initial_prices
  for (day in 2:64){
    # 10 random normal variables, replicated in 10 rows for computational ease
    Z<-t(matrix(rnorm(10,0,1),10,10))
      prices<-prices*exp(drift-.5*volatility^2+volatility*rowSums(Z*Cholesky_Regular))
      prices_antithetic<-prices_antithetic*exp(drift-.5*volatility^2+volatility*rowSums(-Z*Cholesky_Regular))
      ETF_Paths_Crude[day,n]<-sum(weighting*prices)
      ETF_Paths[day,2*n-1]<-sum(weighting*prices)
      ETF_Paths[day,2*n]<-sum(weighting*prices_antithetic)
  }
  if (n>1){
    #update mean and variance
    mean_crude<-sum(ETF_Paths_Crude[day,])/(ncol(ETF_Paths_Crude))
    variance_crude<-sum((ETF_Paths_Crude[day,]-mean_crude)^2)/(ncol(ETF_Paths_Crude)-1)
    mean<-sum(ETF_Paths[day,])/(ncol(ETF_Paths))
    variance<-sum((ETF_Paths[day,]-mean)^2)/(ncol(ETF_Paths)-1)
    #update relative error of 95% confidence interval
    relative_error_crude<-(qnorm(.975)*sqrt(variance_crude/ncol(ETF_Paths_Crude)))/mean_crude
    relative_error<-(qnorm(.975)*sqrt(variance/ncol(ETF_Paths)))/mean
    
    RE_compare[1,n-1]<-relative_error_crude
    RE_compare[2,n-1]<-relative_error
    
  }
  re<-max(relative_error,relative_error_crude)
  print(c(n,re))
  n<-n+1
}

# Scenario 1
ETF_Scenario1<-data.frame()


#initialize counter variable and stopping condition
n<-1
re_scenario1<-100000
while(re_scenario1>.05){ 
  # Intial price of portfolio
  
  ETF_Scenario1[1,2*n-1]<-sum(Initial_prices*weighting)
  ETF_Scenario1[1,2*n]<-sum(Initial_prices*weighting)
  
  #Create a vector to track prices

  prices<-Initial_prices
  prices_antithetic<-Initial_prices
  for (day in 2:64){
    # 10 random normal variables, replicated in 10 rows for computational ease
    Z<-t(matrix(rnorm(10,0,1),10,10))
    if(day<=43){
      prices<-prices*exp(drift-.5*volatility^2+volatility*rowSums(Z*Cholesky_Regular))
      prices_antithetic<-prices_antithetic*exp(drift-.5*volatility^2+volatility*rowSums(-Z*Cholesky_Regular))
      ETF_Scenario1[day,2*n-1]<-sum(weighting*prices)
      ETF_Scenario1[day,2*n]<-sum(weighting*prices_antithetic)
    } else if(day>43){
      prices<-prices*exp(adverse_drift-.5*volatility^2+volatility*rowSums(Z*Cholesky_Stressed))
      prices_antithetic<-prices_antithetic*exp(adverse_drift-.5*volatility^2+volatility*rowSums(-Z*Cholesky_Stressed))
      ETF_Scenario1[day,2*n-1]<-sum(weighting*prices)
      ETF_Scenario1[day,2*n]<-sum(weighting*prices_antithetic)
    }
  }
  if (n>1){
    #update mean and variance
    mean_scenario1<-sum(ETF_Scenario1[day,])/(ncol(ETF_Scenario1))
    variance_scenario1<-sum((ETF_Scenario1[day,]-mean_scenario1)^2)/(ncol(ETF_Scenario1)-1)
    #update relative error of 95% confidence interval
    re_scenario1<-(qnorm(.975)*sqrt(variance_scenario1/ncol(ETF_Scenario1)))/mean_scenario1
  }
  print(c(n,re_scenario1))
  n<-n+1
}

# Scenario 2
ETF_Scenario2<-data.frame()


#initialize counter variable and stopping condition
n<-1
re_scenario2<-100000
while(re_scenario2>.05){ 
  # Intial price of portfolio
  
  ETF_Scenario2[1,2*n-1]<-sum(Initial_prices*weighting)
  ETF_Scenario2[1,2*n]<-sum(Initial_prices*weighting)
  
  #Create a vector to track prices
  
  prices<-Initial_prices
  prices_antithetic<-Initial_prices
  for (day in 2:64){
    # 10 random normal variables, replicated in 10 rows for computational ease
    Z<-t(matrix(rnorm(10,0,1),10,10))
    if(day<=22){
      prices<-prices*exp(drift-.5*volatility^2+volatility*rowSums(Z*Cholesky_Regular))
      prices_antithetic<-prices_antithetic*exp(drift-.5*volatility^2+volatility*rowSums(-Z*Cholesky_Regular))
      ETF_Scenario2[day,2*n-1]<-sum(weighting*prices)
      ETF_Scenario2[day,2*n]<-sum(weighting*prices_antithetic)
    } else if(day>22){
      prices<-prices*exp(adverse_drift-.5*volatility^2+volatility*rowSums(Z*Cholesky_Stressed))
      prices_antithetic<-prices_antithetic*exp(adverse_drift-.5*volatility^2+volatility*rowSums(-Z*Cholesky_Stressed))
      ETF_Scenario2[day,2*n-1]<-sum(weighting*prices)
      ETF_Scenario2[day,2*n]<-sum(weighting*prices_antithetic)
    }
  }
  if (n>1){
    #update mean and variance
    mean_scenario2<-sum(ETF_Scenario2[day,])/(ncol(ETF_Scenario2))
    variance_scenario2<-sum((ETF_Scenario2[day,]-mean_scenario2)^2)/(ncol(ETF_Scenario2)-1)
    #update relative error of 95% confidence interval
    re_scenario2<-(qnorm(.975)*sqrt(variance_scenario2/ncol(ETF_Scenario2)))/mean_scenario2
  }
  print(c(n,re_scenario2))
  n<-n+1
}

# Scenario 3
ETF_Scenario3<-data.frame()


#initialize counter variable and stopping condition
n<-1
re_scenario3<-100000
while(re_scenario3>.05){ 
  # Intial price of portfolio
  
  ETF_Scenario3[1,2*n-1]<-sum(Initial_prices*weighting)
  ETF_Scenario3[1,2*n]<-sum(Initial_prices*weighting)
  
  #Create a vector to track prices
  
  prices<-Initial_prices
  prices_antithetic<-Initial_prices
  for (day in 2:64){
    # 10 random normal variables, replicated in 10 rows for computational ease
    Z<-t(matrix(rnorm(10,0,1),10,10))
    prices<-prices*exp(adverse_drift-.5*volatility^2+volatility*rowSums(Z*Cholesky_Stressed))
    prices_antithetic<-prices_antithetic*exp(adverse_drift-.5*volatility^2+volatility*rowSums(-Z*Cholesky_Stressed))
    ETF_Scenario3[day,2*n-1]<-sum(weighting*prices)
    ETF_Scenario3[day,2*n]<-sum(weighting*prices_antithetic)
  }

if (n>1){
  #update mean and variance
  mean_scenario3<-sum(ETF_Scenario3[day,])/(ncol(ETF_Scenario3))
  variance_scenario3<-sum((ETF_Scenario3[day,]-mean_scenario3)^2)/(ncol(ETF_Scenario3)-1)
  #update relative error of 95% confidence interval
  re_scenario3<-(qnorm(.975)*sqrt(variance_scenario3/ncol(ETF_Scenario3)))/mean_scenario3
}
print(c(n,re_scenario3))
n<-n+1
}


# Option Pricing

# discount factor
disc<-exp(-(nrow(ETF_Paths)-1)*(drift+.5*volatility^2))

# Calculate 95% CI for Asian Call & Put, Lookback Call & Put
Options<-data.frame()
for(i in 1:ncol(ETF_Paths)){
  Options[1,i]<-disc*max((sum(ETF_Paths[,i])/nrow(ETF_Paths)-ETF_Paths[1,1]),0) #Asian Call Strike = ETF price at t=0
  Options[2,i]<-disc*max((ETF_Paths[1,1])-sum(ETF_Paths[,i])/nrow(ETF_Paths),0) #Asian Put Strike = ETF price at t=0
  Options[3,i]<-disc*max((max(ETF_Paths[,i])-ETF_Paths[nrow(ETF_Paths),i]),0) # Lookback put, strike = max ETF value throughout simulation
  Options[4,i]<-disc*max((ETF_Paths[nrow(ETF_Paths),i]-min(ETF_Paths[,i])),0) # Lookback call, strike = min ETF value throughout simulation
print(i)
}

Option_mean<-rowSums(Options)/ncol(Options)
Option_variance<-data.frame()
for(i in 1:4){
  Option_variance[1,i]<-sum((Options[i,]-Option_mean[i])^2)/ncol(Options) 
}

Option_upper<-Option_mean+qnorm(.975)*sqrt(Option_variance/ncol(ETF_Paths))
Option_lower<-Option_mean-qnorm(.975)*sqrt(Option_variance/ncol(ETF_Paths))
Option<-data.frame(t(rbind(Option_lower,(Option_mean),(Option_upper))))
rownames(Option)<-c("Asian call, K=S_0","Asian put, K=S_0","Lookback put","Lookback Call")
colnames(Option)<-c("Lower 95% CI","Mean","Upper 95% CI")


# Plots to evaluate effectiveness of variance reduction techniques
# Relative Error Comparison

# Compare relative error over all data
plot(c(2:(ncol(RE_compare)+1)),RE_compare[1,], type="s",col="red", pch=16, xlab="n", ylab="Relative Error with 95% CI",main ="Effect of Variance Reduction on Relative Error")
points(c(2:(ncol(RE_compare)+1)),RE_compare[2,],type="s")
grid(nx = NULL, ny = NULL, col = "black", lty = "dotted")
legend(50, .2, legend=c("Crude", "With Antithetic Variables"),
       col=c("red", "black"), lty=1, cex=0.8,
       title="Approach", text.font=4, bg='lightblue')

# Compare relative error over first 100 n, easier to see improvement here
plot(c(2:100),RE_compare[1,c(1:99)], type="s",col="red", pch=16, xlab="n", ylim=c(0,.2), ylab="Relative Error with 95% CI",main ="Effect of Variance Reduction on Relative Error")
points(c(2:100),RE_compare[2,c(1:99)],type="s")
grid(nx = NULL, ny = NULL, col = "black", lty = "dotted")
legend(50, .2, legend=c("Crude", "With Antithetic Variables"),
       col=c("red", "black"), lty=1, cex=0.8,
       title="Approach", text.font=4, bg='lightblue')

# Chart that shows to achieve a certain level of relative error how many n will be needed
# While loop approach to simulation
Compare<-data.frame((seq(.1,relative_error_crude,-.0001)))
for(i in 1:length(seq(.1,relative_error_crude,-.0001))){
  Compare[i,2]<-min(which((RE_compare[2,]<Compare[i,1])==1))+1 #antithetical
  Compare[i,3]<-min(which((RE_compare[1,]<Compare[i,1])==1))+1 #crude
  Compare[i,4]<-Compare[i,3]/Compare[i,2]  #ratio
  Compare[i,5]<-Compare[i,3]-Compare[i,2] #difference
}
colnames(Compare)<-c("Relative Error","n Needed with Antithetic Variables","n Needed with Crude Approach", "Ratio of n Needed")

# Plot to show difference in n required between crude and antithetical approach 
plot(Compare[,1],rev(Compare[,5]),type="s",axes=F,ylab="(Crude n)-(Antithetic n)",xlab="Relative Error",main="Difference in n required to achieve relative error")
axis(1, at = rev(seq(.1,relative_error_crude,-.01)), labels = paste(seq(.1,relative_error_crude,-.01)),tick=TRUE)
axis(2,at= seq(0,1500,250), labels=paste(seq(0,1500,250)) , tick=TRUE)
grid(nx = NULL, ny = NULL, col = "black", lty = "dotted")


# Ratio of number of n required to acheive error between methods 
plot(Compare[1:nrow(Compare),1],rev(Compare[1:nrow(Compare),4]),type="s",axes=F,ylab="(Crude n)/(Antithetic n)",
     xlab="Relative Error",main="Ratio of n required to acheive relative error") #useful?
axis(1, at = rev(seq(.1,relative_error_crude,-.01)), labels = paste(seq(.1,relative_error_crude,-.01)),tick=TRUE)
axis(2,at= seq(1.5,3,.25), labels=paste(seq(1.5,3,.25)) , tick=TRUE)
grid(nx = NULL, ny = NULL, col = "black", lty = "dotted")


# Chart to show how much relative error you will have with a certain simulation budget
# For loop approach to simulation
Compare2<-data.frame(c(25,50,100,250,500,1000,1500))
for(i in 1:length(Compare2[,1])){
  Compare2[i,2]<-RE_compare[1,Compare2[i,1]-1]
  Compare2[i,3]<-RE_compare[2,Compare2[i,1]-1]
}
colnames(Compare2)<- c("n","Crude Approach Relative Error","Relative Error With Antithetic Variables")
Compare2[,2]<-paste(round(100*Compare2[,2], 5), "%", sep="")
Compare2[,3]<-paste(round(100*Compare2[,3], 5), "%", sep="")

# Plotting ETF paths
# Assigning dates to data frames and combining historical data with simulated paths in preparation for plotting

# Creating Date vector for plotting that removes weekends and non-trading days
dates <- seq(as.Date('2019-04-12'),as.Date('2019-07-15'),by = 1)
dates <- dates[!weekdays(dates) %in% c('Saturday','Sunday')]
dates <- c(dates[1:which(dates=="2019-04-19")-1],dates[(which(dates=="2019-04-19")+1):(which(dates=="2019-05-27")-1)],dates[(which(dates=="2019-05-27")+1):(which(dates=="2019-07-04")-1)],dates[(which(dates=="2019-07-04")+1):(dates[length(dates)])])
dates <- dates[1:nrow(ETF_Paths)]

#add dates to portfolio path data frames
ETF_Paths<-cbind(dates,ETF_Paths)
ETF_Scenario1<-cbind(dates,ETF_Scenario1)
ETF_Scenario2<-cbind(dates,ETF_Scenario2)
ETF_Scenario3<-cbind(dates,ETF_Scenario3)

# Replicate historical portfolio performance as many times as there are stock paths
# Bind columns of historical data and scenarios to form a single path for plotting
# Process must be repeated because while loop approach to simulation ends after varying number of paths

ETF_BeforeSim[,2:ncol(ETF_Paths)]<-ETF_BeforeSim[,2]
colnames(ETF_BeforeSim)<-c("Date",rep("Price",ncol(ETF_BeforeSim)-1))
colnames(ETF_Paths)<-c("Date",rep("Price",ncol(ETF_Paths)-1))
Base_Case<-data.frame(rbind(ETF_BeforeSim,ETF_Paths))
ETF_BeforeSim[,3:ncol(ETF_BeforeSim)]<-NULL

ETF_BeforeSim[,2:ncol(ETF_Scenario1)]<-ETF_BeforeSim[,2]
colnames(ETF_BeforeSim)<-c("Date",rep("Price",ncol(ETF_BeforeSim)-1))
colnames(ETF_Scenario1)<-c("Date",rep("Price",ncol(ETF_Scenario1)-1))
Scenario_One<-data.frame(rbind(ETF_BeforeSim,ETF_Scenario1))
ETF_BeforeSim[,3:ncol(ETF_BeforeSim)]<-NULL

ETF_BeforeSim[,2:ncol(ETF_Scenario2)]<-ETF_BeforeSim[,2]
colnames(ETF_BeforeSim)<-c("Date",rep("Price",ncol(ETF_BeforeSim)-1))
colnames(ETF_Scenario2)<-c("Date",rep("Price",ncol(ETF_Scenario2)-1))
Scenario_Two<-data.frame(rbind(ETF_BeforeSim,ETF_Scenario2))
ETF_BeforeSim[,3:ncol(ETF_BeforeSim)]<-NULL

ETF_BeforeSim[,2:ncol(ETF_Scenario3)]<-ETF_BeforeSim[,2]
colnames(ETF_BeforeSim)<-c("Date",rep("Price",ncol(ETF_BeforeSim)-1))
colnames(ETF_Scenario3)<-c("Date",rep("Price",ncol(ETF_Scenario3)-1))
Scenario_Three<-data.frame(rbind(ETF_BeforeSim,ETF_Scenario3))
ETF_BeforeSim[,3:ncol(ETF_BeforeSim)]<-NULL

# Calculate a daily Moving_Averages average
Moving_Averages<-data.frame(Base_Case[713:776,1]) # Dates of simulation
i<-0
for(j in 713:776){
  i<-j-712
  #Base_Case
  Moving_Averages[i,2]<-sum(Base_Case[j,2:ncol(Base_Case)])/(ncol(Base_Case)-1)
  # 1 bad month
  Moving_Averages[i,3]<- sum(Scenario_One[j,2:ncol(Scenario_One)])/(ncol(Scenario_One)-1)
  # 2 bad months
  Moving_Averages[i,4]<-sum(Scenario_Two[j,2:ncol(Scenario_Two)])/(ncol(Scenario_Two)-1)
  # 3 bad months
  Moving_Averages[i,5]<-sum(Scenario_Three[j,2:ncol(Scenario_Three)])/(ncol(Scenario_Three)-1)
  print(i)
}

# Update column names for row bind
ETF_BeforeSim[,2:5]<-ETF_BeforeSim[,2]
colnames(ETF_BeforeSim)<-c("Date",rep("Price",ncol(ETF_BeforeSim)-1))
colnames(Moving_Averages)<-c("Date",rep("Price",ncol(Moving_Averages)-1))
Moving_Averages<-rbind(ETF_BeforeSim,Moving_Averages)
ETF_BeforeSim[,3:ncol(ETF_BeforeSim)]<-NULL


# Plots of Simulation Paths

# Actual ETF Performance Throughout Plot
During_Sim<-data.frame(read_excel("Stock_Throughout_Sim.xlsx", sheet = "Sheet1"))
ETF_DuringSim<-data.frame(During_Sim[,1])
for(i in 1:nrow(During_Sim)){
  ETF_DuringSim[i,2]<-sum(During_Sim[i,2:ncol(During_Sim)]*weighting)
}
colnames(ETF_DuringSim)<-c("Date","Price")
ETF_DuringSim<-rbind(Base_Case[1:712,1:2],ETF_DuringSim[])


# Ending Histogram
bins = seq(250, 1300, by=25)
hist(t(Base_Case[nrow(Base_Case),2:ncol(Base_Case)]),col="blue",xlab="Simulated ETF Values",main="ETF Values at End of Simulation on 07/15/19",bins,las=1, xaxt="n")
axis(1, at=axTicks(1), labels=sprintf("$%s", axTicks(1)))
grid(nx = NULL, ny = NULL, col = "black", lty = "dotted")
abline(v = ETF_DuringSim[nrow(ETF_DuringSim),ncol(ETF_DuringSim)], col = "red", lwd = 6)
abline(v = sum(Base_Case[nrow(Base_Case),2:ncol(Base_Case)])/(ncol(Base_Case)-1), col = "green", lwd = 2)
abline(v = quantile(Base_Case[nrow(Base_Case),2:ncol(Base_Case)],.5), col = "yellow", lwd = 2)
legend(750,250,legend=c("Mean Value = $596.37", "Median Value = $583.14", 
                        "Realized Value = 581.92"),col=c("green","yellow","red"),
       lty=1,cex=0.8, lwd=3,text.font=2, bg='lightblue')
box(lty = 1, col = 'black')


# Moving upper and lower percentiles, can change base case data frame to a scenario
U<-.6
L<-.4
Upper<-data.frame(ETF_BeforeSim)
Lower<-data.frame(ETF_BeforeSim)
for(i in 713:776){
  Upper[i,1]<-Base_Case[i,1]
  Lower[i,1]<-Base_Case[i,1]
  Upper[i,2]<-quantile(Base_Case[i,2:ncol(Base_Case)],U)
  Lower[i,2]<-quantile(Base_Case[i,2:ncol(Base_Case)],L)
  print(i-712)
}


plot(Moving_Averages[487:776,1],Moving_Averages[487:776,2],type="s",col="green",ylab="Portfolio Value ($)",
     xlab="Date",main="Expected Portfolio Performance")
points(abline(v=Moving_Averages[713,1]))#,abline(v=Moving_Averages[618,1]),abline(v=Moving_Averages[642,1]))
points(Upper[487:776,1],Upper[487:776,2],type="s",col="blue")
points(Lower[487:776,1],Lower[487:776,2],type="s",col="red",abline(v=Moving_Averages[713,1]))

points(ETF_DuringSim[487:776,1],ETF_DuringSim[487:776,2],type="s",col="black") # compare to how ETF actually performed
grid(nx = NULL, ny = NULL, col = "black", lty = "dotted")
legend(Moving_Averages[487,1], 480, legend=c("60th percentile", "Mean","40th percentile","Realized"),
      col=c( "blue","green","red","black"), lty=1, lwd=2, cex=0.8,
       title="CI", text.font=2, bg='lightblue')


# Scenario and historical
# 1 month bad plot

plot(Moving_Averages[587:776,1],Moving_Averages[587:776,3],type="s",col="green",ylab="Portfolio Value ($)",
     xlab="Date",main="Expected Portfolio Performance, 1 bad month")
points(abline(v=Moving_Averages[713,1]),abline(v=Moving_Averages[618,1]),abline(v=Moving_Averages[642,1]))
points(ETF_DuringSim[487:776,1],ETF_DuringSim[487:776,2],type="s",col="black") # compare to how ETF actually performed
grid(nx = NULL, ny = NULL, col = "black", lty = "dotted")
legend(Moving_Averages[706,1], 470, legend=c("Upper 95% CI", "Mean","Lower 95% CI"),
       col=c( "green","black","red"), lty=1, cex=0.8,
       title="CI", text.font=2, bg='lightblue')


# 2 months bad plot

plot(Moving_Averages[587:776,1],Moving_Averages[587:776,4],type="s",col="green",ylab="Portfolio Value ($)",
     xlab="Date",main="Expected Portfolio Performance, 2 bad months")
points(abline(v=Moving_Averages[713,1]),abline(v=Moving_Averages[618,1]),abline(v=Moving_Averages[642,1]))
points(ETF_DuringSim[487:776,1],ETF_DuringSim[487:776,2],type="s",col="black") # compare to how ETF actually performed
grid(nx = NULL, ny = NULL, col = "black", lty = "dotted")
legend(Moving_Averages[706,1], 470, legend=c("Upper 95% CI", "Mean","Lower 95% CI"),
       col=c( "green","black","red"), lty=1, cex=0.8,
       title="CI", text.font=2, bg='lightblue')


# 3 months bad plot

plot(Moving_Averages[487:776,1],Moving_Averages[487:776,5],type="s",col="green",ylab="Portfolio Value ($)",
     xlab="Date",main="Expected Portfolio Performance, Worst case")
points(abline(v=Moving_Averages[713,1]),abline(v=Moving_Averages[618,1]),abline(v=Moving_Averages[642,1]))
points(ETF_DuringSim[487:776,1],ETF_DuringSim[487:776,2],type="s",col="black") # compare to how ETF actually performed
grid(nx = NULL, ny = NULL, col = "black", lty = "dotted")


# Plot of how portfolio did in last 3 years - Historical data
plot(ETF_BeforeSim[,1],ETF_BeforeSim[,2],type="s",xlab="Time",ylab="Price ($)",main="Portfolio Value Before Simulation") #Before any predictions
grid(nx = NULL, ny = NULL, col = "black", lty = "dotted")
legend(0, 600, legend=c("Base_Case", "1 month","2 month","3 month"),
       col=c( "black","green","red","purple"), lty=1, cex=0.8,
       title="Path", text.font=2, bg='lightblue')