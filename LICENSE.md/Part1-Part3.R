 
# To see details about background introduction, check README.md

#Part 1 contruct a short put butterfly using the informtion from example
t25 <- 25/365
t15 <- 15/365
t0<-0
sig1<-0.25
sig2<-0.45
s<-seq(300,360,by=2.5)
#a) Is the option premium a debit or credit?
cost1<-2*p1[k==330] - p1[k==325] - p1[k==335]
cost1
#-0.23 is negative, it's a credit

#b)P/L at 25,15, 0 DTE

#sell puts at 330 and buy 1 put at each end (325,335)
exp_25<-exp((t45-t25)*r)
exp_15<-exp((t45-t15)*r)
exp_0 <-exp((t45*r))
#25DTE
pl25<-round(2*BS(s,330,sig,r,t25,typ='p') - BS(s,325,sig,r,t25,typ='p') -BS(s,335,
        sig,r,t25,typ='p') - cost1*exp_25,3)

delta25 <- 2*BSdelta(s,330,sig,r,t25,typ='p') - BSdelta(s,325,sig,r,t25,typ='p') -
  BSdelta(s,335,sig,r,t25,typ='p')


#15DTE
pl15<-round(2*BS(s,330,sig,r,t15,typ='p') - BS(s,325,sig,r,t15,typ='p') -BS(s,335,
                                                                            sig,r,t15,typ='p') - cost1*exp_15,3)

delta15 <- 2*BSdelta(s,330,sig,r,t15,typ='p') - BSdelta(s,325,sig,r,t15,typ='p') -
  BSdelta(s,335,sig,r,t15,typ='p')

#0 DTE - at Expiration 
pl0<-2*Put_payoff(s,330) - Put_payoff(s,325) - Put_payoff(s,335) - cost1*exp_0 
 
#c) sigma = 25% and 45%
pl25_25 <- round(2*BS(s,330,sig1,r,t25,typ='p') - BS(s,325,sig1,r,t25,typ='p') -
                   BS(s,335,sig1,r,t25,typ='p') - cost1*exp_25,3)
pl25_45 <- round(2*BS(s,330,sig2,r,t25,typ='p') - BS(s,325,sig2,r,t25,typ='p') -
                   BS(s,335,sig2,r,t25,typ='p') - cost1*exp_25,3)
shortput.butterfly <- data.frame(S=s,PL25=round(pl25,3),Delta25=round(delta25,3),
                   PL15=round(pl15,3),Delta15=round(delta15,3),
                   PL0=round(pl0,3),PL25_25=pl25_25,PL25_45=pl25_45) 


#part 2 short strangle
k_strangle<-c(325,335)
cost_strangle<- -c1[k==335] - p1[k==325]
cost_strangle
#b)P/L at 25,15, 0 DTE
#sell put option at k = 325, sell call at k = 335

#25DTE
pl25.strangle<-round(-BS(s,325,sig,r,t25,typ='p') - BS(s,335,sig,r,t25) - cost_strangle*exp_25,3)

delta25.strangle <- (-BSdelta(s,325,sig,r,t25,typ='p')) - BSdelta(s,335,sig,r,t25) 



#15DTE
pl15.strangle<-round(-BS(s,325,sig,r,t15,typ='p') - BS(s,335,sig,r,t15) - cost_strangle*exp_15,3)

delta15.strangle <- (-BSdelta(s,325,sig,r,t15,typ='p')) - BSdelta(s,335,sig,r,t15)

#0 DTE - at Expiration 
pl0.strangle<- -Put_payoff(s,325) - Call_payoff(s,335) - cost_strangle*exp_0 

#c) sigma = 25% and 45%
pl25_25.strangle <- round(- BS(s,325,sig1,r,t25,typ='p') -
                   BS(s,335,sig1,r,t25) - cost_strangle*exp_25,3)
pl25_45.strangle <- round( - BS(s,325,sig2,r,t25,typ='p') -
                   BS(s,335,sig2,r,t25) - cost_strangle*exp_25,3)
short.strangle <- data.frame(S=s,PL25=round(pl25.strangle,3),Delta25=round(delta25.strangle,3),
                   PL15=round(pl15.strangle,3),Delta15=round(delta15.strangle,3),
                   PL0=round(pl0.strangle,3),PL25_25=pl25_25.strangle,pl25_45=pl25_45.strangle) 


#Part 3 - Long put at 320,short put at 325, short call at 335, long call at 340
cost_condor<- p1[k==320] - p1[k==325] - c1[k==335] +c1[k==340]
cost_condor
#it's a credit

#25DTE
pl25.condor<-round(BS(s,320,sig,r,t25,typ = 'p') - BS(s,325,sig,r,t25,typ='p') - BS(s,335,sig,r,t25) + BS(s,340,sig,r,t25) - cost_condor*exp_25,3)

delta25.condor <- BSdelta(s,320,sig,r,t25,typ = 'p') - BSdelta(s,325,sig,r,t25,typ='p') - BSdelta(s,335,sig,r,t25) + BSdelta(s,340,sig,r,t25) 


#15DTE
pl15.condor<-round(BS(s,320,sig,r,t15,typ = 'p') - BS(s,325,sig,r,t15,typ='p') - BS(s,335,sig,r,t15) + BS(s,340,sig,r,t15) - cost_condor*exp_15,3)

delta15.condor <- BSdelta(s,320,sig,r,t15,typ = 'p') - BSdelta(s,325,sig,r,t15,typ='p') - BSdelta(s,335,sig,r,t15) + BSdelta(s,340,sig,r,t15) 



#0 DTE - at Expiration 
pl0.condor<- Put_payoff(s,320)-Put_payoff(s,325) - Call_payoff(s,335) + Call_payoff(s,340) - cost_condor*exp_0 

#c) sigma = 25% and 45%
pl25_25.condor <- round(BS(s,320,sig1,r,t25,typ = 'p') - BS(s,325,sig1,r,t25,typ='p') - BS(s,335,sig1,r,t25) + BS(s,340,sig1,r,t25) - cost_condor*exp_25,3)
pl25_45.condor <- round(BS(s,320,sig2,r,t25,typ = 'p') - BS(s,325,sig2,r,t25,typ='p') - BS(s,335,sig2,r,t25) + BS(s,340,sig2,r,t25) - cost_condor*exp_25,3)
short.condor <- data.frame(S=s,PL25=round(pl25.condor,3),Delta25=round(delta25.condor,3),
                             PL15=round(pl15.condor,3),Delta15=round(delta15.condor,3),
                             PL0=round(pl0.condor,3),PL25_25=pl25_25.condor,pl25_45=pl25_45.condor) 
short.condor
