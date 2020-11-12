# set working directory and 
setwd("C:/users/somja/Documents/Projects/representation")
library(lpSolve)
library(usmap)
library(totalcensus)

pop.est = read.csv("nst-est2019-alldata.csv")
pop.est = pop.est[,1:27]
current.votes = rep(0,57)
new.votes = rep(0,57)

current.votes[6:56] <- c(9,3,11,6,55,9,7,3,3,29,16,4,4,20,11,6,6,
                         8,8,4,10,11,16,10,6,10,3,5,6,4,14,5,29,15,
                         3,18,7,7,20,4,9,3,11,38,6,3,13,12,5,10,3)

state.data = pop.est[6:56,5:17]
state.data = state.data[,-c(2,3)]
state.data$cagr <- ((state.data$POPESTIMATE2019/state.data$POPESTIMATE2010)^(1/9))-1
state.data$POPESTIMATE2020 <- state.data$POPESTIMATE2019*(1+state.data$cagr)
state.data$CENSUSEST2020 <- state.data$POPESTIMATE2019*(1+state.data$cagr*9/12)
total.pop <- sum(state.data$CENSUSEST2020)
state.data$CENSUSEST2020.PCT <- state.data$CENSUSEST2020/total.pop
rownames(state.data) <- NULL

# columns 1-51 are for y sub i
# columns 52-102 are for x sub i
A = matrix(0,154,102)
b = rep(0,154)
sense = rep('>',154)
for (val in 1:51){
  index = val*2-1
  A[index,val] <- 1
  A[index+1,val] <- 1
  
  A[index,val+51] <- -1
  A[index+1,val+51] <- 1
  
  b[index] <- -state.data$CENSUSEST2020.PCT[val]*538
  b[index+1] <- state.data$CENSUSEST2020.PCT[val]*538
  
  
}
# x sub i must be greater than 3
A[103:153,52:102] <- diag(51)
b[103:153] <- 3

#sum of all x sub i must equal 538
A[154,52:102] <- 1
b[154] <- 538
sense[154] <- "="

# objective vector
obj = rep(0,102)
obj[1:51] <- 1

# solve
sln = lp("min",obj,A,sense,b,int.vec = seq(52,102))
sln$solution

state.fips <- fips(state.data$NAME)
state.names <- convert_fips_to_names(state.fips,states = NULL, geo_header = "STATE", in_states = "US")
results = cbind(state.names,current.votes[6:56],sln$solution[52:102])
results = data.frame(results)
colnames(results) <- c("State","Current EC Votes", "Predicted EC Votes")
