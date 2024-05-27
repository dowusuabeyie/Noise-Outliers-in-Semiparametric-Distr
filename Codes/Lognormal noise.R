#lognormal disrtibution
#lognormal disrtibution
#lognormal disrtibution
#lognormal disrtibution
#lognormal disrtibution
#lognormal disrtibution
#lognormal disrtibution



library(mgcv)

rm(list = ls())
set.seed(123)
N =1000
n= c(50,100,200,300,400,500)

Sim.Res = vector(mode = "list", length = 12)

model_aic.f1 =  matrix(0, N, length(n))
model_dev.f1 = matrix(0, N, length(n))
model_gcv.f1 = matrix(0, N, length(n))
model_aic.f2 = matrix(0, N, length(n))
model_dev.f2 =  matrix(0, N, length(n))
model_gcv.f2 = matrix(0, N, length(n))
model_aic.f3 = matrix(0, N, length(n))
model_dev.f3 =  matrix(0, N, length(n))
model_gcv.f3 = matrix(0, N, length(n))
model_aic.f4 = matrix(0, N, length(n))
model_dev.f4 =  matrix(0, N, length(n))
model_gcv.f4 = matrix(0, N, length(n))






for (j in 1:N) {
  for (i in 1:length(n)) {
    
    xrac <- seq(0, 1, length.out = n[i])  # Independent variable
    
    yrac <- sin(2 * pi * xrac) + 6*rlnorm(n[i], meanlog = 0, sdlog = 1)  
    
    
    f1<-gam(yrac~s(xrac,k=25,bs="cr"))
    model_aic.f1[j,i] <- AIC(f1)
    model_dev.f1[j,i] <-  f1$deviance
    model_gcv.f1[j,i] <- f1$gcv.ubre
    
    
    f2<-gam(yrac~s(xrac,k=25,bs="ps"))
    model_aic.f2[j,i] <- AIC(f2)
    model_dev.f2[j,i] <- f2$deviance
    model_gcv.f2[j,i] <- f2$gcv.ubre
    
    
    f3<-gam(yrac~s(xrac,k=25,bs="tp"))
    model_aic.f3[j,i] <- AIC(f3)
    model_dev.f3[j,i] <- f3$deviance
    model_gcv.f3[j,i] <- f3$gcv.ubre
    
    
    f4<-gam(yrac~s(xrac,k=25,bs="gp"))
    model_aic.f4[j,i] <- AIC(f4)
    model_dev.f4[j,i] <- f4$deviance
    model_gcv.f4[j,i] <- f4$gcv.ubre
    
  }
  
}




Sim.Res = list(model_aic.f1 = model_aic.f1,
               model_dev.f1 = model_dev.f1,
               model_gcv.f1 = model_gcv.f1,
               model_aic.f2 = model_aic.f2,
               model_dev.f2 = model_dev.f2,
               model_gcv.f2 = model_gcv.f2,
               model_aic.f3 = model_aic.f3,
               model_dev.f3 = model_dev.f3,
               model_gcv.f3 = model_gcv.f3,
               model_aic.f4 = model_aic.f4,
               model_dev.f4 = model_dev.f4,
               model_gcv.f4 = model_gcv.f4
)



single.p = data.frame(model_aic.f1 = apply(Sim.Res[[1]], 2, mean),
                      model_dev.f1 = apply(Sim.Res[[2]], 2, mean),
                      model_gcv.f1 = apply(Sim.Res[[3]], 2, mean),
                      model_aic.f2 = apply(Sim.Res[[4]], 2, mean),
                      model_dev.f2 = apply(Sim.Res[[5]], 2, mean),
                      model_gcv.f2 = apply(Sim.Res[[6]], 2, mean),
                      model_aic.f3 = apply(Sim.Res[[7]], 2, mean),
                      model_dev.f3 = apply(Sim.Res[[8]], 2, mean),
                      model_gcv.f3 = apply(Sim.Res[[9]], 2, mean),
                      model_aic.f4 = apply(Sim.Res[[10]], 2, mean),
                      model_dev.f4 = apply(Sim.Res[[11]], 2, mean),
                      model_gcv.f4 = apply(Sim.Res[[12]], 2, mean)
)









