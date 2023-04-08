library("deSolve")
library("scatterplot3d")
#WS small-world model
sir=function(t,state,parameters){with (as.list(c(state,parameters)),{
  dS=-lambda*S*I-alpha*S
  dI=lambda*S*I-gama*I
  dR=gama*I+alpha*S
  return(list(c(dS,dI,dR)))
})
}

state=c(S=0.9,I=0.1,R=0)
parameters=c(lambda=2,gama=1,alpha=0.2)
times=seq(0,20,by=0.001)

out=ode(y=state,times=times,func=sir,parms=parameters)
#wsseperate
out1=as.matrix(out)
#find the time corresponding to the peak
timax=out[which.max(out1[,3]),1]
timax
out1[timax*1000,3]

out11=cbind(out1[,1],out1[,2])
out12=cbind(out1[,1],out1[,3])
out13=cbind(out1[,1],out1[,4])

windows(width=2000,height=1500)
plot(out11[,2]~out11[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="S(t)",ylim=c(0,1),xlim=c(0,20))
windows(width=2000,height=1500)
plot(out12[,2]~out11[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="I(t)",ylim=c(0,0.2),xlim=c(0,20))
windows(width=2000,height=1500)
plot(out13[,2]~out11[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="R(t)",ylim=c(0,1),xlim=c(0,20))
#合起来的图
windows(width=2000,height=1500)
text.legend=c("S(t)","I(t)","R(t)")
plot(out11[,2]~out11[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab =NA,ylim=c(0,1),xlim=c(0,20))
lines(out12[,2]~out11[,1], col = "green")
lines(out13[,2]~out11[,1], col = "blue")
legend("right",
       legend=text.legend,
       lty = 1,
       col=c("red","green","blue"))

#change lambda
parameters1=c(lambda=1,gama=1,alpha=0.2)
parameters2=c(lambda=2,gama=1,alpha=0.2)
parameters3=c(lambda=3,gama=1,alpha=0.2)
parameters4=c(lambda=4,gama=1,alpha=0.2)
out21=as.matrix(ode(y=state,times=times,func=sir,parms=parameters1))
timax21=out21[which.max(out21[,3]),1]
timax21
out21[timax21*1000,3]

out22=as.matrix(ode(y=state,times=times,func=sir,parms=parameters2))
timax22=out22[which.max(out22[,3]),1]
timax22
out22[timax22*1000,3]

out23=as.matrix(ode(y=state,times=times,func=sir,parms=parameters3))
timax23=out23[which.max(out23[,3]),1]
timax23
out23[timax23*1000,3]

out24=as.matrix(ode(y=state,times=times,func=sir,parms=parameters4))
timax24=out21[which.max(out24[,3]),1]
timax24
out24[timax24*1000,3]

#rt
windows(width=2000,height=1500)
plot(out21[,4]~out21[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="R(t)",ylim=c(0,1),xlim=c(0,20))
lines(out22[,4]~out21[,1], col = "green")
lines(out23[,4]~out21[,1], col = "blue")
lines(out24[,4]~out21[,1], col = "black")
legend("right",
       legend=c(expression(lambda==0.1),expression(lambda==0.2),expression(lambda==0.3),expression(lambda==0.4)),
       lty = 1,
       col=c("red","green","blue","black"))
#it
windows(width=2000,height=1500)
plot(out21[,3]~out21[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="I(t)",ylim=c(0,0.5),xlim=c(0,20))
lines(out22[,3]~out21[,1], col = "green")
lines(out23[,3]~out21[,1], col = "blue")
lines(out24[,3]~out21[,1], col = "black")
legend("topright",
       legend=c(expression(lambda==0.1),expression(lambda==0.2),expression(lambda==0.3),expression(lambda==0.4)),
       lty = 1,
       col=c("red","green","blue","black"))
#st
windows(width=2000,height=1500)
plot(out21[,2]~out21[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="S(t)",ylim=c(0,1),xlim=c(0,20))
lines(out22[,2]~out21[,1], col = "green")
lines(out23[,2]~out21[,1], col = "blue")
lines(out24[,2]~out21[,1], col = "black")
legend("topright",
       legend=c(expression(lambda==0.1),expression(lambda==0.2),expression(lambda==0.3),expression(lambda==0.4)),
       lty = 1,
       col=c("red","green","blue","black"))

#change alpha
parameters1=c(lambda=2,gama=1,alpha=0.1)
parameters2=c(lambda=2,gama=1,alpha=0.2)
parameters3=c(lambda=2,gama=1,alpha=0.3)
parameters4=c(lambda=2,gama=1,alpha=0.4)
out21=as.matrix(ode(y=state,times=times,func=sir,parms=parameters1))
timax21=out21[which.max(out21[,3]),1]
timax21
out21[timax21*1000,3]

out22=as.matrix(ode(y=state,times=times,func=sir,parms=parameters2))
timax22=out22[which.max(out22[,3]),1]
timax22
out22[timax22*1000,3]

out23=as.matrix(ode(y=state,times=times,func=sir,parms=parameters3))
timax23=out23[which.max(out23[,3]),1]
timax23
out23[timax23*1000,3]

out24=as.matrix(ode(y=state,times=times,func=sir,parms=parameters4))
timax24=out21[which.max(out24[,3]),1]
timax24
out24[timax24*1000,3]
#rt
windows(width=2000,height=1500)
plot(out21[,4]~out21[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="R(t)",ylim=c(0,1),xlim=c(0,20))
lines(out22[,4]~out21[,1], col = "green")
lines(out23[,4]~out21[,1], col = "blue")
lines(out24[,4]~out21[,1], col = "black")
legend("topleft",#指定图注的位置，左上，右上，左下，右下
       legend=c(expression(alpha==0.1),expression(alpha==0.2),expression(alpha==0.3),expression(alpha==0.4)),
       lty = 1,
       col=c("red","green","blue","black"))
#it
windows(width=2000,height=1500)
plot(out21[,3]~out21[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="I(t)",ylim=c(0,0.2),xlim=c(0,20))
lines(out22[,3]~out21[,1], col = "green")
lines(out23[,3]~out21[,1], col = "blue")
lines(out24[,3]~out21[,1], col = "black")
legend("topright",#指定图注的位置，左上，右上，左下，右下
       legend=c(expression(alpha==0.1),expression(alpha==0.2),expression(alpha==0.3),expression(alpha==0.4)),
       lty = 1,
       col=c("red","green","blue","black"))
#st
windows(width=2000,height=1500)
plot(out21[,2]~out21[,1],type = "l",bty="l" ,col="red",xlab = "t",ylab ="S(t)",ylim=c(0,1),xlim=c(0,20))
lines(out22[,2]~out21[,1], col = "green")
lines(out23[,2]~out21[,1], col = "blue")
lines(out24[,2]~out21[,1], col = "black")
legend("topright",#指定图注的位置，左上，右上，左下，右下
       legend=c(expression(alpha==0.1),expression(alpha==0.2),expression(alpha==0.3),expression(alpha==0.4)),
       lty = 1,
       col=c("red","green","blue","black"))