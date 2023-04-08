library("deSolve")
library("scatterplot3d")
#WS small-world
sir=function(t,state,parameters){with (as.list(c(state,parameters)),{
  dS=-lambda*S*I-alpha*S
  dI=lambda*S*I-gama*I
  dR=gama*I+alpha*S
  return(list(c(dS,dI,dR)))
})
}

state=c(S=0.9,I=0.1,R=0)#the original date is the same as ns small-world
parametersws1=c(lambda=1,gama=1,alpha=0.2)
parametersws2=c(lambda=3,gama=1,alpha=0.2)
times=seq(0,20,by=0.0001)
out1=ode(y=state,times=times,func=sir,parms=parametersws1)
out2=ode(y=state,times=times,func=sir,parms=parametersws2)
#wsseperate
out1=as.matrix(out1)
out2=as.matrix(out2)

#NW small-world
sir_NW=function(t,state,parameters){with (as.list(c(state,parameters)),{
  dS=-1-2*p*S-alpha*S
  dI=1+2*p*S-I
  dR=I+alpha*S
  return(list(c(dS,dI,dR)))  
})
}
state=c(S=0.9,I=0.1,R=0)
parametersnw1=c(p=0.1,alpha=0.2)#alpha is the same
parametersnw2=c(p=1,alpha=0.2)#alpha is the same
times=seq(0,2,by=0.0001)
out3=ode(y=state,times=times,func=sir_NW,parms=parametersnw1)
out4=ode(y=state,times=times,func=sir_NW,parms=parametersnw2)
#select out s<0 or r>1
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
out3=select_nw(outcome=out3)
out4=select_nw(outcome=out4)
N_out3=nrow(out3)
N_out4=nrow(out4)
out_newstate3=c(out3[N_out3,3],out3[N_out3,4])
out_newstate4=c(out4[N_out4,3],out4[N_out4,4])
#IR模型
ir=function(t,state,parameters){with (as.list(c(state,parameters)),{
  dI=-gama*I
  dR=gama*I
  return(list(c(dI,dR)))  
})
}

times_after=seq(0,18,by=0.0001)
parameters_after=c(gama=1)
out_after3=ode(y=out_newstate3,times=times_after,func=ir,parms=parameters_after)
out_after4=ode(y=out_newstate4,times=times_after,func=ir,parms=parameters_after)

out_after3=as.matrix(out_after3)
out_after4=as.matrix(out_after4)

out_after3[,1]=out_after3[,1]+matrix(data=N_out3/10000, nrow=nrow(out_after3), ncol=1)
out_after4[,1]=out_after4[,1]+matrix(data=N_out4/10000, nrow=nrow(out_after4), ncol=1)
s_after3=matrix(data=0, nrow = nrow(out_after3), ncol = 1)
s_after4=matrix(data=0, nrow = nrow(out_after4), ncol = 1)
out_after3=cbind(out_after3[,1],s_after3,out_after3[,2],out_after3[,3])
out_after4=cbind(out_after4[,1],s_after4,out_after4[,2],out_after4[,3])
out3=rbind(out3,out_after3)
out4=rbind(out4,out_after4)

#ws and nw st
windows(width=2000,height=1500)
text.legend=c("WS,λ<k>=1","WS,λ<k>=3","NW,α=0.1","NW,α=1")
plot(out1[,2]~out1[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="S(t)",,ylim=c(0,0.8),xlim=c(0,10))
lines(out2[,2]~out2[,1], col = "green")
lines(out3[,2]~out3[,1], col = "blue")
lines(out4[,2]~out4[,1], col = "black")
legend("right",
       legend=text.legend,
       lty = 1,
       col=c("red","green","blue","black"))

#ws and nw it
windows(width=2000,height=1500)
text.legend=c("WS,λ<k>=1","WS,λ<k>=3","NW,α=0.1","NW,α=1")
plot(out1[,3]~out1[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="I(t)",ylim=c(0,0.8),xlim=c(0,10))
lines(out2[,3]~out2[,1], col = "green")
lines(out3[,3]~out3[,1], col = "blue")
lines(out4[,3]~out4[,1], col = "black")
legend("right",
       legend=text.legend,
       lty = 1,
       col=c("red","green","blue","black"))

#ws and nw rt
windows(width=2000,height=1500)
text.legend=c("WS,λ<k>=1","WS,λ<k>=3","NW,α=0.1","NW,α=1")
plot(out1[,4]~out1[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="R(t)",ylim=c(0,1),xlim=c(0,10))
lines(out2[,4]~out2[,1], col = "green")
lines(out3[,4]~out3[,1], col = "blue")
lines(out4[,4]~out4[,1], col = "black")
legend("right",
       legend=text.legend,
       lty = 1,
       col=c("red","green","blue","black"))