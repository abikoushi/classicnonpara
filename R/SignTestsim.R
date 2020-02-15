library(parallel)
library(rmutil)
#setwd("./projects/classicnonpara")

SignTest <- function(X, theta0=0, alternative="two.sided"){
  Sn <- sum(X>theta0)
  n <- length(X)
  binom.test(Sn, n, alternative = alternative)
}

caushy_sim <- function(i,n,med=0){
  set.seed(i)
  X <- rcauchy(n,med)
  c(t=t.test(X)$p.value, sign = SignTest(X)$p.value,
    wilcox = wilcox.test(X)$p.value)
}

logis_sim <- function(i,n,m=0,s=1){
  set.seed(i)
  X <- rlogis(n,m,s)
  c(t=t.test(X)$p.value, sign = SignTest(X)$p.value,
    wilcox = wilcox.test(X)$p.value) 
}

laplace_sim <- function(i,n,m=0,s=1){
  set.seed(i)
  X <- rlaplace(n,m,s)
  c(t=t.test(X)$p.value, sign = SignTest(X)$p.value,
    wilcox = wilcox.test(X)$p.value) 
}

unif_sim <- function(i,n){
  set.seed(i)
  X <- runif(n,-1,1)
  c(t=t.test(X)$p.value, sign = SignTest(X)$p.value,
    wilcox = wilcox.test(X)$p.value) 
}

norm_sim <- function(i,n,m=0,s=1){
  set.seed(i)
  X <- rnorm(n,m,s)
  c(t=t.test(X)$p.value, sign = SignTest(X)$p.value,
    wilcox = wilcox.test(X)$p.value)  
}

weibull_sim <- function(i,n,m=1,eta=1){
  set.seed(i)
  X <- rweibull(n,m,eta)
  mu <- eta*gamma(1+1/m)
  med <- eta*log(2)^(1/m)
  c(t=t.test(X,mu = mu)$p.value, 
    sign = SignTest(X,theta0 = med)$p.value,
    wilcox = wilcox.test(X,mu = med)$p.value)  
}

out_logis_5 <- mclapply(1:10000,logis_sim,n=5)

pdf("img/pv_logis_5_t.pdf")
plot(sort(simplify2array(out_logis_5)[1,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated",ylab = "nominal", main="t")
abline(0,1,lty=2)
dev.off()

pdf("img/pv_logis_5_sign.pdf")
plot(sort(simplify2array(out_logis_5)[2,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated", ylab = "nominal", main="sign")
abline(0,1,lty=2)
dev.off()

pdf("img/pv_logis_5_wilco.pdf")
plot(sort(simplify2array(out_logis_5)[3,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated", ylab = "nominal", main="Wilcoxon")
abline(0,1,lty=2)
dev.off()

out_unif_5 <- mclapply(1:10000,unif_sim,n=5)

pdf("img/pv_unif_5_t.pdf")
plot(sort(simplify2array(out_unif_5)[1,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated",ylab = "nominal", main="t")
abline(0,1,lty=2)
dev.off()

pdf("img/pv_unif_5_sign.pdf")
plot(sort(simplify2array(out_unif_5)[2,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated", ylab = "nominal", main="sign")
abline(0,1,lty=2)
dev.off()

pdf("img/pv_unif_5_wilco.pdf")
plot(sort(simplify2array(out_unif_5)[3,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated", ylab = "nominal", main="Wilcoxon")
abline(0,1,lty=2)
dev.off()

out_unif_10 <- mclapply(1:10000,unif_sim,n=10)

pdf("img/pv_unif_10_t.pdf")
plot(sort(simplify2array(out_unif_10)[1,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated",ylab = "nominal", main="t")
abline(0,1,lty=2)
dev.off()

pdf("img/pv_unif_10_sign.pdf")
plot(sort(simplify2array(out_unif_10)[2,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated", ylab = "nominal", main="sign")
abline(0,1,lty=2)
dev.off()

pdf("img/pv_unif_10_wilco.pdf")
plot(sort(simplify2array(out_unif_10)[3,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated", ylab = "nominal", main="Wilcoxon")
abline(0,1,lty=2)
dev.off()


out_unif_5 <- mclapply(1:10000,unif_sim,n=5)

pdf("img/pv_unif_5_t.pdf")
plot(sort(simplify2array(out_unif_5)[1,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated",ylab = "nominal", main="t")
abline(0,1,lty=2)
dev.off()

pdf("img/pv_unif_5_sign.pdf")
plot(sort(simplify2array(out_unif_5)[2,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated", ylab = "nominal", main="sign")
abline(0,1,lty=2)
dev.off()

pdf("img/pv_unif_5_wilco.pdf")
plot(sort(simplify2array(out_unif_5)[3,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated", ylab = "nominal", main="Wilcoxon")
abline(0,1,lty=2)
dev.off()

out_weibull_2_1_n5 <- mclapply(1:10000,weibull_sim,m=2,eta=1,n=5)

pdf("img/pv_weibull_2_1_n5_t.pdf")
plot(sort(simplify2array(out_weibull_2_1_n5)[1,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated",ylab = "nominal", main="t")
abline(0,1,lty=2)
dev.off()

pdf("img/pv_weibull_2_1_n5_sign.pdf")
plot(sort(simplify2array(out_weibull_2_1_n5)[2,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated", ylab = "nominal", main="sign")
abline(0,1,lty=2)
dev.off()

pdf("img/pv_weibull_2_1_n5_wilco.pdf")
plot(sort(simplify2array(out_weibull_2_1_n5)[3,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated", ylab = "nominal", main="Wilcoxon")
abline(0,1,lty=2)
dev.off()

out_weibull_05_1_n5 <- mclapply(1:10000,weibull_sim,m=0.5,eta=1,n=5)

pdf("img/pv_weibull_05_1_n5_t.pdf")
plot(sort(simplify2array(out_weibull_05_1_n5)[1,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated",ylab = "nominal", main="t")
abline(0,1,lty=2)
dev.off()

pdf("img/pv_weibull_05_1_n5_sign.pdf")
plot(sort(simplify2array(out_weibull_05_1_n5)[2,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated", ylab = "nominal", main="sign")
abline(0,1,lty=2)
dev.off()

pdf("img/pv_weibull_05_1_n5_wilco.pdf")
plot(sort(simplify2array(out_weibull_05_1_n5)[3,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated", ylab = "nominal", main="Wilcoxon")
abline(0,1,lty=2)
dev.off()


out_weibull_05_1_n10 <- mclapply(1:10000,weibull_sim,m=0.5,eta=1,n=10)

pdf("img/pv_weibull_05_1_n10_t.pdf")
plot(sort(simplify2array(out_weibull_05_1_n10)[1,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated",ylab = "nominal", main="t")
abline(0,1,lty=2)
dev.off()

pdf("img/pv_weibull_05_1_n10_sign.pdf")
plot(sort(simplify2array(out_weibull_05_1_n10)[2,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated", ylab = "nominal", main="sign")
abline(0,1,lty=2)
dev.off()

pdf("img/pv_weibull_05_1_n10_wilco.pdf")
plot(sort(simplify2array(out_weibull_05_1_n10)[3,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated", ylab = "nominal", main="Wilcoxon")
abline(0,1,lty=2)
dev.off()


out_weibull_05_1_n100 <- mclapply(1:10000,weibull_sim,m=0.5,eta=1,n=100)

pdf("img/pv_weibull_05_1_n100_t.pdf")
plot(sort(simplify2array(out_weibull_05_1_n100)[1,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated",ylab = "nominal", main="t")
abline(0,1,lty=2)
dev.off()

pdf("img/pv_weibull_05_1_n100_sign.pdf")
plot(sort(simplify2array(out_weibull_05_1_n100)[2,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated", ylab = "nominal", main="sign")
abline(0,1,lty=2)
dev.off()

pdf("img/pv_weibull_05_1_n100_wilco.pdf")
plot(sort(simplify2array(out_weibull_05_1_n100)[3,]),
     seq(0,1,length.out = 10000),type="s",
     xlab="simulated", ylab = "nominal", main="Wilcoxon")
abline(0,1,lty=2)
dev.off()

X <- rweibull(100,0.5,1)
hist(X,breaks = "FD", main = "Weibull(0.5,1)")
