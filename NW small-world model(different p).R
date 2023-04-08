#change p
library("deSolve")
state=c(S=0.9,I=0.1,R=0)
times=seq(0,2,by=0.0001)
parameters1=c(p=0.2,alpha=0.2)
parameters2=c(p=0.4,alpha=0.2)
parameters3=c(p=0.6,alpha=0.2)
parameters4=c(p=0.8,alpha=0.2)
out21=ode(y=state,times=times,func=sir_NW,parms=parameters1)
out22=ode(y=state,times=times,func=sir_NW,parms=parameters2)
out23=ode(y=state,times=times,func=sir_NW,parms=parameters3)
out24=ode(y=state,times=times,func=sir_NW,parms=parameters4)
out21=select_nw(outcome=out21)
out22=select_nw(outcome=out22)
out23=select_nw(outcome=out23)
out24=select_nw(outcome=out24)
N_out21=nrow(out21)
N_out22=nrow(out22)
N_out23=nrow(out23)
N_out24=nrow(out24)
out_newstate21=c(out21[N_out21,3],out21[N_out21,4])
out_newstate22=c(out22[N_out22,3],out22[N_out22,4])
out_newstate23=c(out23[N_out23,3],out23[N_out23,4])
out_newstate24=c(out24[N_out24,3],out24[N_out24,4])
times_after=seq(0,10,by=0.0001)
parameters_after=c(gama=1)
out_after21=as.matrix(ode(y=out_newstate21,times=times_after,func=ir,parms=parameters_after))
out_after22=as.matrix(ode(y=out_newstate22,times=times_after,func=ir,parms=parameters_after))
out_after23=as.matrix(ode(y=out_newstate23,times=times_after,func=ir,parms=parameters_after))
out_after24=as.matrix(ode(y=out_newstate24,times=times_after,func=ir,parms=parameters_after))

out_after21[,1]=out_after21[,1]+matrix(data=N_out21/10000, nrow = nrow(out_after21), ncol = 1)
s_after21=matrix(data=0, nrow = nrow(out_after21), ncol = 1)
out_after21=cbind(out_after21[,1],s_after21,out_after21[,2],out_after21[,3])
out211=rbind(out21,out_after21)

out_after22[,1]=out_after22[,1]+matrix(data=N_out22/10000, nrow = nrow(out_after22), ncol = 1)
s_after22=matrix(data=0, nrow = nrow(out_after22), ncol = 1)
out_after22=cbind(out_after22[,1],s_after22,out_after22[,2],out_after22[,3])
out221=rbind(out22,out_after22)

out_after23[,1]=out_after23[,1]+matrix(data=N_out23/10000, nrow = nrow(out_after23), ncol = 1)
s_after23=matrix(data=0, nrow = nrow(out_after23), ncol = 1)
out_after23=cbind(out_after23[,1],s_after23,out_after23[,2],out_after23[,3])
out231=rbind(out23,out_after23)

out_after24[,1]=out_after24[,1]+matrix(data=N_out24/10000, nrow = nrow(out_after24), ncol = 1)
s_after24=matrix(data=0, nrow = nrow(out_after24), ncol = 1)
out_after24=cbind(out_after24[,1],s_after24,out_after24[,2],out_after24[,3])
out241=rbind(out24,out_after24)

#rt
windows(width=2000,height=1500)
plot(out211[,4]~out211[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="R(t)",ylim=c(0,1),xlim=c(0,2))
lines(out221[,4]~out221[,1], col = "green")
lines(out231[,4]~out231[,1], col = "blue")
lines(out241[,4]~out241[,1], col = "black")
legend("topleft",
       legend=c(expression(p==0.2),expression(p==0.4),expression(p==0.6),expression(p==0.8)),
       lty = 1,
       col=c("red","green","blue","black"))
#it
windows(width=2000,height=1500)
plot(out211[,3]~out211[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="I(t)",ylim=c(0,0.8),xlim=c(0,2))
lines(out221[,3]~out221[,1], col = "green")
lines(out231[,3]~out231[,1], col = "blue")
lines(out241[,3]~out241[,1], col = "black")
legend("topright",
       legend=c(expression(p==0.2),expression(p==0.4),expression(p==0.6),expression(p==0.8)),
       lty = 1,
       col=c("red","green","blue","black"))
#st
windows(width=2000,height=1500)
plot(out211[,2]~out211[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="S(t)",ylim=c(0,0.8),xlim=c(0,2))
lines(out221[,2]~out221[,1], col = "green")
lines(out231[,2]~out231[,1], col = "blue")
lines(out241[,2]~out241[,1], col = "black")
legend("topright",
       legend=c(expression(p==0.2),expression(p==0.4),expression(p==0.6),expression(p==0.8)),
       lty = 1,
       col=c("red","green","blue","black"))