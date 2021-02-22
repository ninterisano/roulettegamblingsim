#Roulette Simulator
#Testing for Github
#Creating our win criteria
#Numbers on an American Roulette
wheel<-c("00",0:36)

#Wanted outcomes
#Betting on Black or Red///Payout: 1:1
Black<-c(2,4,6,8,10,11,13,15,17,20,22,24,26,28,29,31,33,35)
Red<-c(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
#High Low /// Payout: 1:1
low<-c(1:18)
high<-c(19:36)
#Even odd /// Payout 1:1
even<-c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36)
odd<-c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35)

#Betting on the Third Column///Payout: 2:1
col3<-c(3,6,9,12,15,18,21,24,27,30,33,36)
  #Obviously other columns, 1,2
#Betting on a Dozen /// Payout 2:1
dozen<-c(1:12)
  #Other dozens are available

#Betting on a line /// Payout 5:1
line<-c(16:21)
  #There are multiple other lines

#Betting on Corners /// Payout 8:1
corner<-c(7,8,10,11)
  #There are multiple other corners

#Betting on a Street/// Payout 11:1
street<-c(1,2,3) 
  #There are multiple other street (4,5,6), (10,11,12) etc.

#Betting on a Split of Colour Red///Payout 17:1
oppbsplit<-c(9,12)
split<-c(16,19)
  #c(9,12,18,21,16,19,27,30) All the different splits, you are choosing one.

#Betting straight up /// Payout 35:1
straight<-c(17)
  #Obviously, can pick other numbers

#Variables Needed for Statistics
maxsim<-array() #Max Profit of each sim, based on 1000 simulations of a given bet strat.
minsim<-array() #Min "Profit" of each sim, based on 1000 simulations of a given bet strat.
posmaxsim<-array()  #Position of Max Profit in a given simulation
posminsim<-array()  #Position of Min "Profit" in a given simulation

######IF YOU WERE TO SET THE SEED, IT WOULD BE HERE#####

#Essentially, this is 1000 simulations of if you played 1000 times.
for(j in 1:1000) {
  net=array() #Net on a given number in a simulation
  oc=array() #Number picked
  total=array() #Counter of what the profit is throughout a simulation.
  k=1
  total[1]=0 #Starting Position. You aren't down money or up.
  for(i in 1:1000) {
    if (total[i] <= -500) { #If you are down -$500, stop playing, stopping rule
      break()
    }
#Creating the simulation, sampling one number at a time.
    outcome<-sample(wheel,1,replace=TRUE) #Almost like generating x in a MC Sim.
    oc[i]<-outcome #Saving the number picked

#This is based off of the position in which it seems that the max seems to occur
#most often. Little project, changes based on betting strategy.
#Can easily comment this out, change etc. This is changing how much you bet.
    if(i>=80 && i<=105) {
      pay=-100
      if(outcome %in% col3) {
        pay = pay + 150
      }
      if(outcome %in% Black) {
        pay = pay + 100
      }
  
    }
    
#The part after %in% can change based on what betting strategy you use.
#Just be sure to change the payout accordingly. Follows what we said above.
    if(i<80 || i>105) {
  #This is assumed $5 on each bet (2)
      pay=-10
      if(outcome %in% col3) {
        pay = pay + 15
      }
      if(outcome %in% Black) {
      pay = pay + 10
      }
    }
    
#Saves the Net on a given number picked and total looks at the running profit
    net[i]=pay
    total[k+1]<-net[i]+total[k]
    k=k+1
  }

#Statistics out of each strategy
  maxsim[j]<-max(total) #Saves the Max Profit in a simulation, maxsim holds all Maxes
  minsim[j]<-min(total) #Saves the Min "Profit" in a simulation, minsim holds all Mins
  posminsim[j]<-which.min(total) #Position of that min "profit" from a given simulation
  posmaxsim[j]<-which.max(total) #Position of that max profit from a given simulation
}

#Statistics out of each strategy
#Plots out the last individual simulation, more for visual representation of the ups and downs.
plot(total,ylab="Up/Down Earnings",xlab="Trial", main="Results of Betting Strategy")
abline(h=0)
for (c in 1:length(total)){
  if(total[c]>0){
    points(x=c,y=total[c],col="green")
  }
  if(total[c]<=0){
    points(x=c,y=total[c],col="red")
  }
}
lines(total)

#What the Max Winning would be for an individual simulation as well as what bet it's at 
#Bet # - 1
max(total) #Max Profit in the last simulation
min(total) #Min Profit in the last simulation
which.min(total) #Where Min in last sim
which.max(total) #Where Max in last sim

#NOTE: If you are running 1000 simulations, this will show the last simulation run.
#Each individual Outcome in a simulation
oc

#"Winnings" of each individual trial tally as well as probability of that outcome
table(net)
table(net)/1000

#Table of which numbers were chosen in the last simulation
table(oc)

#Sum of winnings (so playing out the entire 1000 trials, what the up and down is)
#As well Expected Value of each individual trials
sum(net)
mean(net) 
#So along what Kayla was saying, the expected loss in a trial should be around 5%.

#Over the 1000 simulations
#Expected Max Profit over the 1000 simulations
mean(maxsim)

#Other Statistics, you will notice there is a very left-skewed distribution.
median(maxsim)
plot(table(maxsim))
table(maxsim)/1000

#Confidence Interval Biggest Wins
marerr<-qnorm(0.975)*(sd(maxsim)/sqrt(1000))
maxupr<-mean(maxsim) + marerr
maxlwr<-mean(maxsim) - marerr
cbind(maxlwr,maxupr)

#Seeing percentage of times you'd fall lower than the interval based on the 1000 sims
percentlwr<-which(as.matrix(maxsim[1:1000])<maxlwr)
percentlwr<-length(percentlwr)/1000
percentlwr
#Seeing percentage of times '' fall above ''
percentupr<-which(as.matrix(maxsim[1:1000])>maxupr)
percentupr<-length(percentupr)/1000
percentupr
#Percentage of times it falls in the C.I. based on the 1000 simulations
1-(percentupr+percentlwr)

#Expected Biggest Loss
mean(minsim)
median(minsim)
#Confidence Interval Biggest Losses
marerr<-qnorm(0.975)*(sd(minsim)/sqrt(1000))
minupr<-mean(minsim) + marerr
minlwr<-mean(minsim) - marerr
cbind(minlwr,minupr)

#Looking at the Confidence Interval for Positioning of Max Profit
#Essentially, how many times you'd have to bet to on average, achieve the max profit
posmaxupr<-mean(posmaxsim) + qnorm(0.975)*(sd(posmaxsim)/sqrt(1000))
posmaxlwr<-mean(posmaxsim) - qnorm(0.975)*(sd(posmaxsim)/sqrt(1000))
cbind(posmaxlwr,posmaxupr)

#We only do the position where 95% of the Max Profit occurs because we want to know
#Where we'd win, not lose.
