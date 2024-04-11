library(readxl)
library(dplyr)
#SECTION 1
#Current Pricing Model
# Define parameters
capacity_combined <- 8 * 17           # Capacity 
single_price <- 80                    # Price for Courts
mean_demand <- 120                    # Mean demand for the combined courts (Poisson)

# Expected revenue calculation under FCFS with a single price
fcfs_revenue <- function(mean_demand, capacity, price) {
  revenue = 0
  for (demand in 0:200) { # upper limit for Poisson demand
    sold = min(demand, capacity)
    revenue = revenue + price * sold * dpois(demand, mean_demand)
  }
  return(revenue) 
}

# Calculate revenue 
total_revenue_fcfs_single_price = fcfs_revenue(mean_demand, capacity_combined, single_price)

# Output
print(paste("Total Expected Revenue is:", round(total_revenue_fcfs_single_price, 2)))

#SECTION 2

#PRICING FOR MEMBERS:
# Read the Survey Data
padium = read_excel("PadiumSurvey.xlsx",col_names = TRUE)
# Row count
N=nrow(padium)

# Convert columns C to H to numeric
padium <- padium %>%
  mutate(across(Timeslot1:Timeslot6, as.numeric))

maxprice=max(padium[3:8])

nonpeakPrice=63

demandNonPeak<-rep(0,maxprice)
demandPeak<-rep(0,maxprice)
revenue<-rep(0,maxprice)


# STEP 1:
maxWTPNonPeak<-rep(0,N)
maxsurplusNonPeak<-rep(0,N)

for (i in 1:N){
  maxWTPNonPeak[i]=max(padium[i,c(3:6,8)])
  maxsurplusNonPeak[i]=max(padium[i,c(3:6,8)]-nonpeakPrice)
  
  # We can also generate new column(s) and add this information to our daya nyhc:
  padium$maxWTPNonPeak[i]=max(padium[i,c(3:6,8)])
  padium$maxsurplusNonPeak[i]=max(padium[i,c(3:6,8)]-nonpeakPrice)
}

# Viewing the first ten rows of data
padium[1:10,]

# STEP 2:
# Calculate the surplusPeak matrix
prices <- 1:maxprice
surplusPeak <- outer(padium$Timeslot5, prices, "-")

# View a subset of surplusPeak
surplusPeak[1:10, c(50, 55, 60, 65, 70, 75, 80, 85)]

# Calculate scaling factor
# Capacity : 28 players an hour * 17h = 476 players / day 
scaling_factor <- (136 / N)*0.6
for (p in 1:maxprice){
  demandNonPeak[p]=sum((maxsurplusNonPeak>surplusPeak[,p])*(maxsurplusNonPeak>=0))*scaling_factor
  demandPeak[p]=sum((surplusPeak[,p]>=maxsurplusNonPeak)*(surplusPeak[,p]>=0))*scaling_factor
  revenue[p]=nonpeakPrice*demandNonPeak[p]+p*demandPeak[p]
}

# Plotting NonPeak Demand vs Peak Period Price
xaxis=1:maxprice
plot(xaxis,demandNonPeak,pch = 16, type="s",col="blue", las=1, xaxt="n",
     xlab="Price for Peak Period (Time slot 5-9pm)",ylab="Non-Peak Period Demand")
xticks <- seq(0, maxprice, by=15)
axis(side = 1, at = xticks)

# Plotting Peak Demand vs Peak Period Price
xaxis=1:maxprice
plot(xaxis,demandPeak,pch = 16, type="s",col="blue", las=1, xaxt="n",
     xlab="Price for Peak Period (Time slot 5-9pm)",ylab="Peak Period Demand")
xticks <- seq(0, maxprice, by=15)
axis(side = 1, at = xticks)


# Plotting Revenue vs Peak Period Price
xaxis=1:maxprice
plot(xaxis,revenue/1000,pch = 16, type="s",col="blue", las=1, xaxt="n",
     xlab="Price for Peak Period (Time slot 5-9pm)",ylab="Total Revenue")
xticks <- seq(0, maxprice, by=50)
axis(side = 1, at = xticks)
revenueBest=max(revenue[nonpeakPrice:maxprice])
peakPrice=which(revenue == revenueBest)
axis(side = 1, at = peakPrice) 
lines(c(peakPrice,peakPrice),c(0, revenueBest/1000),lty=2)
axis(side = 2, at = round(revenueBest/1000,3),las=1)
lines(c(0,peakPrice),c(revenueBest/1000, revenueBest/1000),lty=2)

print(paste("When the non-peak price is 63, the optimal price for the 5-9pm slot is:",peakPrice))

#SECTION 3
#PRICING FOR NON MEMBERS:
# Read the Survey Data
padium = read_excel("PadiumSurvey.xlsx",col_names = TRUE)
# Row count
N=nrow(padium)

# Convert columns C to H to numeric
padium <- padium %>%
  mutate(across(Timeslot1:Timeslot6, as.numeric))

maxprice=max(padium[3:8])

nonpeakPrice=70

demandNonPeak<-rep(0,maxprice)
demandPeak<-rep(0,maxprice)
revenue<-rep(0,maxprice)


# STEP 1:
maxWTPNonPeak<-rep(0,N)
maxsurplusNonPeak<-rep(0,N)

for (i in 1:N){
  maxWTPNonPeak[i]=max(padium[i,c(3:6,8)])
  maxsurplusNonPeak[i]=max(padium[i,c(3:6,8)]-nonpeakPrice)
  
  # We can also generate new column(s) and add this information to our daya nyhc:
  padium$maxWTPNonPeak[i]=max(padium[i,c(3:6,8)])
  padium$maxsurplusNonPeak[i]=max(padium[i,c(3:6,8)]-nonpeakPrice)
}

# Viewing the first ten rows of data
padium[1:10,]

# STEP 2:
# Calculate the surplusPeak matrix
prices <- 1:maxprice
surplusPeak <- outer(padium$Timeslot5, prices, "-")

# View a subset of surplusPeak
surplusPeak[1:10, c(50, 55, 60, 65, 70, 75, 80, 85)]

# Calculate scaling factor
# Capacity : 28 players an hour * 17h = 476 players / day 
scaling_factor <- (136 / N)*0.4
for (p in 1:maxprice){
  demandNonPeak[p]=sum((maxsurplusNonPeak>surplusPeak[,p])*(maxsurplusNonPeak>=0))*scaling_factor
  demandPeak[p]=sum((surplusPeak[,p]>=maxsurplusNonPeak)*(surplusPeak[,p]>=0))*scaling_factor
  revenue[p]=nonpeakPrice*demandNonPeak[p]+p*demandPeak[p]
}

# Plotting NonPeak Demand vs Peak Period Price
xaxis=1:maxprice
plot(xaxis,demandNonPeak,pch = 16, type="s",col="blue", las=1, xaxt="n",
     xlab="Price for Peak Period (Time slot 5-9pm)",ylab="Non-Peak Period Demand")
xticks <- seq(0, maxprice, by=15)
axis(side = 1, at = xticks)


# Plotting Peak Demand vs Peak Period Price
xaxis=1:maxprice
plot(xaxis,demandPeak,pch = 16, type="s",col="blue", las=1, xaxt="n",
     xlab="Price for Peak Period (Time slot 5-9pm)",ylab="Peak Period Demand")
xticks <- seq(0, maxprice, by=15)
axis(side = 1, at = xticks)

# Plotting Revenue vs Peak Period Price
xaxis=1:maxprice
plot(xaxis,revenue/1000,pch = 16, type="s",col="blue", las=1, xaxt="n",
     xlab="Price for Peak Period (Time slot 5-9pm)",ylab="Total Revenue")
xticks <- seq(0, maxprice, by=50)
axis(side = 1, at = xticks)
revenueBest=max(revenue[nonpeakPrice:maxprice])
peakPrice=which(revenue == revenueBest)
axis(side = 1, at = peakPrice) 
lines(c(peakPrice,peakPrice),c(0, revenueBest/1000),lty=2)
axis(side = 2, at = round(revenueBest/1000,3),las=1)
lines(c(0,peakPrice),c(revenueBest/1000, revenueBest/1000),lty=2)

print(paste("When the non-peak price is 70, the optimal price for the 5-9pm slot is:",peakPrice))

#SECTION 4
#peak non member vs member

# Read the Survey Data
padium <- read_excel("PadiumSurvey.xlsx", 
                     col_types = c("skip", "text", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric"))

# Row count

N=nrow(padium)

# Convert columns C to H to numeric
padium <- padium %>%
  mutate(across(Timeslot1:Timeslot6, as.numeric))

# Setting up both prices simultaneously 
# Price for all time slots (i.e., nonpeak) except for time slot 5 (5-9pm) (peak)
scaling_factor <- 153 / N

surplusNonPeak<-rep(0,N)
surplusPeak<-rep(0,N)
demandNonPeak<-rep(0,N)
demandPeak<-rep(0,N)

index=1
for (membersPrice in seq(from = 50, to = 100, by = 5)){
  for (nonmembersPrice in seq(from = 50, to = 100, by = 5)){
    for (i in 1:N){
      surplusNonPeak[i]=max(padium[i,c(2:7, 7)]-membersPrice)
      surplusPeak[i]=padium[i,7]-nonmembersPrice
    }
    demandNonPeak[index]=sum((surplusNonPeak>surplusPeak)*(surplusNonPeak>=0))*scaling_factor
    demandPeak[index]=sum((surplusPeak>=surplusNonPeak)*(surplusPeak>=0))*scaling_factor
    index=index+1
  }
}

# Create a data table which we will use to run the two regressions:
newdata<-data.frame(matrix(nrow=201,ncol = 5))
colnames(newdata)=c("index","membersPrice","nonmembersPrice","NonPeakDemand", "PeakDemand")
index=1
for (membersPrice in seq(from = 50, to = 100, by = 5)){
  for (nonmembersPrice in seq(from = 50, to = 100, by = 5)){
    newdata[index,1]=index
    newdata[index,2]=membersPrice
    newdata[index,3]=nonmembersPrice
    newdata[index,4]=demandNonPeak[index]
    newdata[index,5]=demandPeak[index]
    index=index+1
  }
}

# Visualizing Revenue as a Function of Base and Peak Price
newdata$revenue=newdata$membersPrice*newdata$NonPeakDemand+newdata$nonmembersPrice*newdata$PeakDemand

library(lattice)
library(stargazer)

wireframe(revenue ~ membersPrice * nonmembersPrice, data=newdata)

# Run Regressions:

# Regression for the dependent variable NonPeakDemand

fit2NonPeak <-lm(NonPeakDemand ~ membersPrice+nonmembersPrice, data=newdata)
summary(fit2NonPeak)

a1=coef(fit2NonPeak)[1]
b11=coef(fit2NonPeak)[2]
b12=coef(fit2NonPeak)[3]

# Regression for the dependent variable NonPeakDemand

fit2Peak <-lm(PeakDemand ~ membersPrice+nonmembersPrice, data=newdata)
a2=coef(fit2Peak)[1]
b21=coef(fit2Peak)[2]
b22=coef(fit2Peak)[3]

stargazer(fit2NonPeak,fit2Peak, type="text")

# Finding optimal revenue by optimization
library("nloptr")

# Differentiated Prices

eval_f <- function(x){
  membersPrice=x[1]
  nonmembersPrice=x[2]
  NonPeakDemand=max(0,a1+b11*membersPrice+b12*nonmembersPrice)
  PeakDemand=max(0,a2+b21*membersPrice+b22*nonmembersPrice)
  revenue=membersPrice*NonPeakDemand+nonmembersPrice*PeakDemand
  objfunction=-revenue
  return(objfunction)
}

eval_g_ineq <- function(x) {
  membersPrice=x[1]
  nonmembersPrice=x[2]
  NonPeakDemand=max(0,a1+b11*membersPrice+b12*nonmembersPrice)
  PeakDemand=max(0,a2+b21*membersPrice+b22*nonmembersPrice)
  constraint <- c(-NonPeakDemand,
                  -PeakDemand,
                  x[1]-x[2])
  return(constraint)
}

# Define initial guess and optimization options
x0 <- c(50, 70)  # Initial guess for membersPrice and nonmembersPrice
opts <- list("algorithm" = "NLOPT_LN_COBYLA",
             "xtol_rel" = 1.0e-9,
             "maxeval" = 1000)

# Rerun the optimization
result <- nloptr(x0 = x0, eval_f = eval_f, 
                 eval_g_ineq = eval_g_ineq, opts = opts)

priceOpt <- result$solution
RevenueOpt <- -result$objective

print(paste("Optimal Member's Price:", priceOpt[1]))
print(paste("Optimal Non-member's Price:", priceOpt[2]))
print(paste("Optimal Revenue:", RevenueOpt))

#SECTION 5
#worst and best scenarios for pricing
# Given data
peakHours <- 4
nonPeakHours <- 13

# Pricing scenarios for 4 people bookings during peak and non-peak hours
scenarios <- list(
  peak = list(
    memberPeak = c(1, 3),      # 1 member price + 3 non-member price during peak hours
    scenario2 = c(2, 2),       # 2 basePrice + 2 bestPrice during peak hours
    scenario3 = c(3, 1),       # 3 basePrice + 1 bestPrice during peak hours
    nonMemberPeak = c(4, 0)    # 4 non-member price during peak hours
  ),
  nonPeak = list(
    nonMemberNonPeak = c(0, 4),     # 4 non-member price during non-peak hours
    scenario6 = c(3, 1),             # 3 basePrice + 1 bestPrice during non-peak hours
    scenario7 = c(2, 2),             # 2 basePrice + 2 bestPrice during non-peak hours
    memberNonPeak = c(1, 3)          # 1 member price + 3 non-member price during non-peak hours
  )
)

# Prices for different scenarios
peakMemberPrice <- 75/4
nonMemberPeakPrice <- 82/4
nonPeakMemberPrice <- 63/4
nonPeakNonMemberPrice <- 70/4

# Demand for different member types in peak and non-peak hours
demandPeakMember <- 49 / peakHours
demandNonPeakMember <- 104 / nonPeakHours
demandPeakNonMember <- 65 / peakHours
demandNonPeakNonMember <- 87 / nonPeakHours

# Calculate total revenue per day for each scenario considering different demands
calculateDailyRevenue <- function(scenario, memberPeakPrice, nonMemberPeakPrice, memberNonPeakPrice, nonMemberNonPeakPrice, peakHours, nonPeakHours, demandPeakMember, demandNonPeakMember, demandPeakNonMember, demandNonPeakNonMember) {
  totalHoursPerDay <- peakHours + nonPeakHours
  totalRevenue <- 0
  
  for (scen in names(scenario)) {
    if (scen %in% names(scenario)) {
      # Ensure demand doesn't exceed hourly capacity
      demandPeakMember <- min(demandPeakMember, 136 / peakHours)
      demandNonPeakMember <- min(demandNonPeakMember, 32 / nonPeakHours)
      demandPeakNonMember <- min(demandPeakNonMember, 136 / peakHours)
      demandNonPeakNonMember <- min(demandNonPeakNonMember, 32 / nonPeakHours)
      
      totalRevenuePeak <- sum(scenario[[scen]][1] * memberPeakPrice * demandPeakMember + scenario[[scen]][2] * nonMemberPeakPrice * demandPeakNonMember) * peakHours
      totalRevenueNonPeak <- sum(scenario[[scen]][1] * memberNonPeakPrice * demandNonPeakMember + scenario[[scen]][2] * nonMemberNonPeakPrice * demandNonPeakNonMember) * nonPeakHours
      totalRevenue <- totalRevenue + (totalRevenuePeak + totalRevenueNonPeak)
    }
  }
  
  return(totalRevenue)
}

# Find the worst scenarios for peak and non-peak hours
worstPeakScenario <- NULL
worstNonPeakScenario <- NULL
worstPeakRevenue <- Inf
worstNonPeakRevenue <- Inf

for (scen in names(scenarios$peak)) {
  totalRevenuePeak <- calculateDailyRevenue(scenarios$peak[scen], peakMemberPrice, nonMemberPeakPrice, nonPeakMemberPrice, nonPeakNonMemberPrice, peakHours, nonPeakHours, demandPeakMember, demandNonPeakMember, demandPeakNonMember, demandNonPeakNonMember)
  if (totalRevenuePeak < worstPeakRevenue) {
    worstPeakScenario <- scen
    worstPeakRevenue <- totalRevenuePeak
  }
}

for (scen in names(scenarios$nonPeak)) {
  totalRevenueNonPeak <- calculateDailyRevenue(scenarios$nonPeak[scen], peakMemberPrice, nonMemberPeakPrice, nonPeakMemberPrice, nonPeakNonMemberPrice, peakHours, nonPeakHours, demandPeakMember, demandNonPeakMember, demandPeakNonMember, demandNonPeakNonMember)
  if (totalRevenueNonPeak < worstNonPeakRevenue) {
    worstNonPeakScenario <- scen
    worstNonPeakRevenue <- totalRevenueNonPeak
  }
}

# Find the best scenarios for peak and non-peak hours
bestPeakScenario <- NULL
bestNonPeakScenario <- NULL
bestPeakRevenue <- -Inf
bestNonPeakRevenue <- -Inf

for (scen in names(scenarios$peak)) {
  totalRevenuePeak <- calculateDailyRevenue(scenarios$peak[scen], peakMemberPrice, nonMemberPeakPrice, nonPeakMemberPrice, nonPeakNonMemberPrice, peakHours, nonPeakHours, demandPeakMember, demandNonPeakMember, demandPeakNonMember, demandNonPeakNonMember)
  if (totalRevenuePeak > bestPeakRevenue) {
    bestPeakScenario <- scen
    bestPeakRevenue <- totalRevenuePeak
  }
}

for (scen in names(scenarios$nonPeak)) {
  totalRevenueNonPeak <- calculateDailyRevenue(scenarios$nonPeak[scen], peakMemberPrice, nonMemberPeakPrice, nonPeakMemberPrice, nonPeakNonMemberPrice, peakHours, nonPeakHours, demandPeakMember, demandNonPeakMember, demandPeakNonMember, demandNonPeakNonMember)
  if (totalRevenueNonPeak > bestNonPeakRevenue) {
    bestNonPeakScenario <- scen
    bestNonPeakRevenue <- totalRevenueNonPeak
  }
}

# Display results
cat("Worst scenario for peak hours:", worstPeakScenario, "with revenue:", worstPeakRevenue, "\n")
cat("Worst scenario for non-peak hours:", worstNonPeakScenario, "with revenue:", worstNonPeakRevenue, "\n")


cat("Best scenario for peak hours:", bestPeakScenario, "with revenue:", bestPeakRevenue, "\n")
cat("Best scenario for non-peak hours:", bestNonPeakScenario, "with revenue:", bestNonPeakRevenue, "\n")


worst_scenario <- worstPeakRevenue+worstNonPeakRevenue
best_scenario <- bestPeakRevenue+bestNonPeakRevenue

print(worst_scenario)
print(best_scenario)

#Section 6
# alook into WTP and how much padium can make in one hour if revenue is being maximized 
#looking at how much revenue can be generated in a single hour given WTP dummy data 
#reading in padium dataset
padium = read.csv("revenuegw_dataset.csv", header = TRUE)
#number of courts
capacity = 8

#the number of people that want to play paddle at any given time slot
n =nrow(padium)
#calculating max WTP for each customer 
for(i in 1:n){
  padium$maxWTP[i] = max(padium[i,3:8])
}
#calculating max price upper bound
maxprice = (max(padium$maxWTP))

#only 8 courts available so picking the 8th most expensive WTP ensures all courts are booked 
fullcapacityprice = (sort(padium$maxWTP)[n - (capacity - 1)])

#defining array variables 
demand = rep(NA,maxprice)
revenue = rep(NA, maxprice)

#determining which price yields the highest revenue for the top 8 WTP customers (as there are only 8 courts)

for (p in 1 :maxprice){
  if (sum(padium$maxWTP>=p) >= capacity){
    demand[p] = capacity
  }else {
    demand[p]=sum(padium$maxWTP>=p)
  }
  revenue[p] =p * demand[p]
}
revenuebest = max(revenue, na.rm = T)
bestprice = which(revenue == revenuebest)
revenue_final = demand[bestprice] * bestprice


#printing out price for max revenue and calculating max revenue 
print(paste("The optimal price is:", bestprice))


print(paste("The total revenue is:",revenue_final))




