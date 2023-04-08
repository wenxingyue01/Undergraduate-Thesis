library("deSolve")
#NW small-world model
sir_NW=function(t,state,parameters){with (as.list(c(state,parameters)),{
  dS=-1-2*p*S-alpha*S
  dI=1+2*p*S-I
  dR=I+alpha*S
  return(list(c(dS,dI,dR)))  
})
}
state=c(S=0.9,I=0.1,R=0)
parameters=c(p=1,alpha=0.2)
times=seq(0,2,by=0.0001)
out=ode(y=state,times=times,func=sir_NW,parms=parameters)
#select s<0 or r>1
select_nw=function(outcome){
  outcome=as.matrix(outcome)
  if(min(outcome[,2])<=0){
    if(max(outcome[,4])>=1){
      s0=outcome[min(which(outcome[,2]<=0)),1]*10000-1
      r0=outcome[min(which(outcome[,4]>=1)),1]*10000-1#10000是步长的倒数   
      minrow=min(s0,r0)
      outcome=outcome[1:minrow,] 
  return(outcome)
    }else{
      s0=outcome[min(which(outcome[,2]<=0)),1]*10000-1#10000是步长的倒数
      outcome=outcome[1:s0,]
      return(outcome)
      }
  }else{
  if(max(outcome[,4])>=1){
    r0=outcome[min(which(outcome[,4]>=1)),1]*10000-1#10000是步长的倒数   
    outcome=outcome[1:r0,] 
    return(outcome)
  }else{
    return(outcome)
  }
  }
}
out=select_nw(outcome=out)
N_out=nrow(out)
out_newstate=c(out[N_out,3],out[N_out,4])#get the initial value of the second process
#IR model
ir=function(t,state,parameters){with (as.list(c(state,parameters)),{
  dI=-gama*I
  dR=gama*I
  return(list(c(dI,dR)))  
})
}

times_after=seq(0,10,by=0.0001)
parameters_after=c(gama=1)
out_after=ode(y=out_newstate,times=times_after,func=ir,parms=parameters_after)

out_after=as.matrix(out_after)
out_after[,1]=out_after[,1]+matrix(data=N_out/10000, nrow = nrow(out_after), ncol = 1)
s_after=matrix(data=0, nrow = nrow(out_after), ncol = 1)
out_after=cbind(out_after[,1],s_after,out_after[,2],out_after[,3])
out1=rbind(out,out_after)

#NWseperate
out11=cbind(out1[,1],out1[,2])
out12=cbind(out1[,1],out1[,3])
out13=cbind(out1[,1],out1[,4])
#separate picture
windows(width=2000,height=1500)
plot(out11[,2]~out11[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="S(t)",ylim=c(0,1),xlim=c(0,6))
windows(width=2000,height=1500)
plot(out12[,2]~out11[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="I(t)",ylim=c(0,1),xlim=c(0,6))
windows(width=2000,height=1500)
plot(out13[,2]~out11[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="R(t)",ylim=c(0,1),xlim=c(0,6))
#Combined picture
windows(width=2000,height=1500)
text.legend=c("S(t)","I(t)","R(t)")
plot(out11[,2]~out11[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab =NA,ylim=c(0,1),xlim=c(0,6))
lines(out12[,2]~out11[,1], col = "green")
lines(out13[,2]~out11[,1], col = "blue")
legend("right",
       legend=text.legend,
       lty = 1,
       col=c("red","green","blue"))