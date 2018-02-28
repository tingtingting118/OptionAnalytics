#create function to calculate p/l (payoffs)

Call_payoff<-function(s,k){
  return(ifelse(s-k>0,s-k,0))
}

Put_payoff<-function(s,k){
  return(ifelse(k-s>0,k-s,0))
}
