
#setwd('C:\\Users\\jtb00\\OneDrive - CEFAS\\JTB00\\Fish Stuff\\Consultancy 2022\\Top 10 litter paper\\Analysis\\R code and data')

library(rstan)

## Generate OVERALL Top10 for each region with their Bayesian credible
## intervals.

## Data for the top 10 items shown in Table 1 come from GNS.ALL, CS.ALL,
## BB.ALL and BS.ALL

###################################################################
## GREATER NORTH SEA
###################################################################

GNS.ALL = read.csv(file='top15GNS.ALL.csv', header=T, stringsAsFactors=F)

f.all = GNS.ALL$freq
items.all = GNS.ALL$items
N.all = GNS.ALL$N
p.all = 100 * f.all / N.all
round(p.all, 1)[1:10]
ranks.all.GNS = 16 - rank(f.all)

set.seed(2001)

st_mod<- stan_model("litter.stan")

datp1 <- list(N=15, n=N.all, r=f.all, m=0, v=10, d=0.01, a=0.01)
fitp1 <- sampling(object = st_mod, data=datp1)

ex.p1 <- extract(fitp1)
dim(ex.p1$theta)                      # 4000 * 15
theta = ex.p1$theta
head(theta)

#### Each column gives the ranks for the 15 litter types
#### There are no ties
ranks.all <- 16 - t(apply(theta,1,rank))
ns = dim(ranks.all)[1]
head(ranks.all)

pconf.GNS = matrix(0, ncol=2, nrow=10)
rconf.GNS = matrix(0, ncol=2, nrow=10)

## Marginal confidence intervals of the theta
pconf.GNS[1,] = quantile(theta[,1], c(0.025, 0.975))
pconf.GNS[2,] = quantile(theta[,2], c(0.025, 0.975))
pconf.GNS[3,] = quantile(theta[,3], c(0.025, 0.975))
pconf.GNS[4,] = quantile(theta[,4], c(0.025, 0.975))
pconf.GNS[5,] = quantile(theta[,5], c(0.025, 0.975))
pconf.GNS[6,] = quantile(theta[,6], c(0.025, 0.975))
pconf.GNS[7,] = quantile(theta[,7], c(0.025, 0.975))
pconf.GNS[8,] = quantile(theta[,8], c(0.025, 0.975))
pconf.GNS[9,] = quantile(theta[,9], c(0.025, 0.975))
pconf.GNS[10,] = quantile(theta[,10], c(0.025, 0.975))
round(pconf.GNS,2)

## Marginal confidence intervals of ranks
rconf.GNS[1,] = quantile(ranks.all[,1], c(0.025, 0.975))
rconf.GNS[2,] = quantile(ranks.all[,2], c(0.025, 0.975))
rconf.GNS[3,] = quantile(ranks.all[,3], c(0.025, 0.975))
rconf.GNS[4,] = quantile(ranks.all[,4], c(0.025, 0.975))
rconf.GNS[5,] = quantile(ranks.all[,5], c(0.025, 0.975))
rconf.GNS[6,] = quantile(ranks.all[,6], c(0.025, 0.975))
rconf.GNS[7,] = quantile(ranks.all[,7], c(0.025, 0.975))
rconf.GNS[8,] = quantile(ranks.all[,8], c(0.025, 0.975))
rconf.GNS[9,] = quantile(ranks.all[,9], c(0.025, 0.975))
rconf.GNS[10,] = quantile(ranks.all[,10], c(0.025, 0.975))

## labels should match items.all
items.all
labels = c("Rope synth", "Sheet plas", "Line mono", "Bag plas",
           "Line tang", "Other plas", "Fish net", "Clothing", "Rope nat",
           "Other misc")

## Plot the rank 95% credible intervals
## The actual ranks are plotted as circles
maxrank = max(rconf.GNS[1:10,])

## Plot for FIGURE 3 (a)
#jpeg("C:\\Users\\jtb00\\OneDrive - CEFAS\\JTB00\\Fish Stuff\\Consultancy 2022\\Top 10 litter paper\\Paper 2022//GNS cred ints.jpg", height=12, width=15, res=600, pointsize=9, units="cm")
par(mar=c(4,7,4,4), mfrow=c(1,1))
plot(0:maxrank, 0:maxrank, ylab=' ', xlab='Rank', type='n', axes=F,
     ylim=c(0,10), xlim=c(1,maxrank), cex.lab=1.5)
axis(1, labels=1:maxrank, at=1:maxrank, cex=2, cex.axis=1.2)
axis(2, seq(0.5,9.5,1), rev(labels), cex=2, cex.axis=1.2, las=1)
box()

for (j in 1:10) {
  yval = 11 - j - 0.5
  if ((j==2)|(j==6)|(j==8)|(j==10)) col='black'
  if ((j==3)|(j==5)|(j==7)) col='blue'
  if ((j==1)|(j==9)) col='slategray'
  if (j==4) col='gold'
  lines(c(rconf.GNS[j,1], rconf.GNS[j,2]), c(yval,yval), col=col, lwd=2)
  points(ranks.all.GNS[j], yval, col=col,cex=1.2,lwd=2)
}
title('(a)', cex.main=2)
#dev.off()

## *********************************************************
## Temporal analysis for selected items GNS
## *********************************************************

GNS = read.csv(file='top15GNS.csv', header=T, stringsAsFactors=F)
names(GNS)
GNS.N = read.csv(file='GNS.N.csv', header=T, stringsAsFactors=F)

bag.GNS.p = matrix(0, nrow=9, ncol=2)
bag.GNS.r = matrix(0, nrow=9, ncol=2)
bottle.GNS.p = matrix(0, nrow=9, ncol=2)
bottle.GNS.r = matrix(0, nrow=9, ncol=2)
net.GNS.p = matrix(0, nrow=9, ncol=2)
net.GNS.r = matrix(0, nrow=9, ncol=2)

bag.ind = rep(0,9)
bottle.ind = rep(0,9)
net.ind = rep(0,9)

bag.rank.GNS = rep(0,9)
bottle.rank.GNS = rep(0,9)
net.rank.GNS = rep(0,9)

## Check whether each of the three items is present in each year

bag.pres = rep(0,9); bottle.pres = rep(0,9); net.pres = rep(0,9)

# bag present in every year
# bottle not present in first 2 years
# net not present at all

for (j in 1:9) {
  items = GNS[,j]
  if ("bag.bin" %in% items) bag.pres[j] = 1
  if ("bottles.plas.bin" %in% items) bottle.pres[j] = 1
  if ("fishnet.bin" %in% items) net.pres[j] = 1
}
bag.pres
bottle.pres # years 3, 5, 7 and 9
net.pres


for (j in 1:9) {
  
  f.year = GNS[,j+9]
  items.year = GNS[,j]
  N.year = rep(GNS.N$N[j], 15)
  p.year = 100 * f.year / N.year
  ranks.year.GNS = 16 - rank(f.year)
  
  index = 1:15
  bag.ind[j] = index[items.year=="bag.bin"]
  net.ind[j] = index[items.year=="fishnet.bin"]
  
  bag.rank.GNS[j] = ranks.year.GNS[bag.ind[j]]
  net.rank.GNS[j] = ranks.year.GNS[net.ind[j]]
  
  set.seed(2001)
  
  datp1 <- list(N=15, n=N.year, r=f.year, m=0, v=10, d=0.01, a=0.01)
  fitp1 <- sampling(object = st_mod, data=datp1)
  
  ex.p1 <- extract(fitp1)
  theta = ex.p1$theta
  
  #### Each column gives the ranks for the 15 litter types
  
  ranks.year <- 16 - t(apply(theta,1,rank))
  dim(ranks.year)
  
  ## Marginal confidence intervals of the theta

  bag.GNS.p[j,] = quantile(theta[,bag.ind[j]], c(0.025, 0.975))
  net.GNS.p[j,] = quantile(theta[,net.ind[j]], c(0.025, 0.975))
  
  bag.GNS.r[j,] = quantile(ranks.year[,bag.ind[j]], c(0.025, 0.975))
  net.GNS.r[j,] = quantile(ranks.year[,net.ind[j]], c(0.025, 0.975))
  
}

###################################################################
## CELTIC SEA
###################################################################

CS.ALL = read.csv(file='top15CS.ALL.csv', header=T, stringsAsFactors=F)

f.all = CS.ALL$freq
items.all = CS.ALL$items
N.all = CS.ALL$N
p.all = 100 * f.all / N.all
round(p.all, 1)[1:10]
ranks.all.CS = 16 - rank(f.all)

set.seed(2001)
st_mod<- stan_model("litter.stan")

datp1 <- list(N=15, n=N.all, r=f.all, m=0, v=10, d=0.01, a=0.01)
fitp1 <- sampling(object = st_mod, data=datp1)

ex.p1 <- extract(fitp1)
dim(ex.p1$theta)                      # 4000 * 15
theta = ex.p1$theta
head(theta)

#### Each column gives the ranks for the 15 litter types
#### There are no ties
ranks.all <- 16 - t(apply(theta,1,rank))
ns = dim(ranks.all)[1]
head(ranks.all)

pconf.CS = matrix(0, ncol=2, nrow=10)
rconf.CS = matrix(0, ncol=2, nrow=10)

## Marginal confidence intervals of the theta
pconf.CS[1,] = quantile(theta[,1], c(0.025, 0.975))
pconf.CS[2,] = quantile(theta[,2], c(0.025, 0.975))
pconf.CS[3,] = quantile(theta[,3], c(0.025, 0.975))
pconf.CS[4,] = quantile(theta[,4], c(0.025, 0.975))
pconf.CS[5,] = quantile(theta[,5], c(0.025, 0.975))
pconf.CS[6,] = quantile(theta[,6], c(0.025, 0.975))
pconf.CS[7,] = quantile(theta[,7], c(0.025, 0.975))
pconf.CS[8,] = quantile(theta[,8], c(0.025, 0.975))
pconf.CS[9,] = quantile(theta[,9], c(0.025, 0.975))
pconf.CS[10,] = quantile(theta[,10], c(0.025, 0.975))
round(pconf.CS,2)

## Marginal confidence intervals of ranks
rconf.CS[1,] = quantile(ranks.all[,1], c(0.025, 0.975))
rconf.CS[2,] = quantile(ranks.all[,2], c(0.025, 0.975))
rconf.CS[3,] = quantile(ranks.all[,3], c(0.025, 0.975))
rconf.CS[4,] = quantile(ranks.all[,4], c(0.025, 0.975))
rconf.CS[5,] = quantile(ranks.all[,5], c(0.025, 0.975))
rconf.CS[6,] = quantile(ranks.all[,6], c(0.025, 0.975))
rconf.CS[7,] = quantile(ranks.all[,7], c(0.025, 0.975))
rconf.CS[8,] = quantile(ranks.all[,8], c(0.025, 0.975))
rconf.CS[9,] = quantile(ranks.all[,9], c(0.025, 0.975))
rconf.CS[10,] = quantile(ranks.all[,10], c(0.025, 0.975))

## labels should match items.all
items.all
labels = c("Rope synth", "Sheet plas", "Line mono", "Bag plas",
           "Fish net", "Other plas", "Caps plas", "Band plas", "Bot plas",
           "Line tang")

## Plot the rank 95% credible intervals
## The actual ranks are plotted as circles
maxrank = max(rconf.CS[1:10,])

## Plot for FIGURE 3 (b)

#jpeg("C:\\Users\\jtb00\\OneDrive - CEFAS\\JTB00\\Fish Stuff\\Consultancy 2022\\Top 10 litter paper\\Paper 2022//CS cred ints.jpg", height=12, width=15, res=600, pointsize=9, units="cm")

par(mar=c(4,7,4,4), mfrow=c(1,1))
plot(0:maxrank, 0:maxrank, ylab=' ', xlab='Rank', type='n', axes=F,
     ylim=c(0,10), xlim=c(1,maxrank), cex.lab=1.5)
axis(1, labels=1:maxrank, at=1:maxrank, cex=2, cex.axis=1.2)
axis(2, seq(0.5,9.5,1), rev(labels), cex=2, cex.axis=1.2, las=1)
box()

  for (j in 1:10) {
    yval = 11 - j - 0.5
    if ((j==2)|(j==6)|(j==8)) col='black'
    if ((j==3)|(j==5)|(j==10)) col='blue'
    if (j==1) col='slategray'
    if ((j==4)|(j==7)|(j==9)) col='gold'
    lines(c(rconf.CS[j,1], rconf.CS[j,2]), c(yval,yval), col=col, lwd=2)
    points(ranks.all.CS[j], yval, col=col,cex=1.2,lwd=2)
}
title('(b)', cex.main=2)
#dev.off()

## *********************************************************
## Temporal analysis for selected items: CS
## *********************************************************

CS = read.csv(file='top15CS.csv', header=T, stringsAsFactors=F)
names(CS)
CS.N = read.csv(file='CS.N.csv', header=T, stringsAsFactors=F)

bag.CS.p = matrix(0, nrow=9, ncol=2)
bag.CS.r = matrix(0, nrow=9, ncol=2)

net.CS.p = matrix(0, nrow=9, ncol=2)
net.CS.r = matrix(0, nrow=9, ncol=2)

bag.ind = rep(0,9)
net.ind = rep(0,9)

bag.rank.CS = rep(0,9)
net.rank.CS = rep(0,9)

for (j in 1:9) {
  
  f.year = CS[,j+9]
  items.year = CS[,j]
  N.year = rep(CS.N$N[j], 15)
  p.year = 100 * f.year / N.year
  ranks.year.CS = 16 - rank(f.year)
  
  index = 1:15
  bag.ind[j] = index[items.year=="bag.bin"]
  net.ind[j] = index[items.year=="fishnet.bin"]
  
  bag.rank.CS[j] = ranks.year.CS[bag.ind[j]]
  net.rank.CS[j] = ranks.year.CS[net.ind[j]]
  
  set.seed(2001)
  
  datp1 <- list(N=15, n=N.year, r=f.year, m=0, v=10, d=0.01, a=0.01)
  fitp1 <- sampling(object = st_mod, data=datp1)
  
  ex.p1 <- extract(fitp1)
  theta = ex.p1$theta
  
  #### Each column gives the ranks for the 15 litter types
  
  ranks.year <- 16 - t(apply(theta,1,rank))
  dim(ranks.year)
  
  ## Marginal confidence intervals of the theta
  
  bag.CS.p[j,] = quantile(theta[,bag.ind[j]], c(0.025, 0.975))
  net.CS.p[j,] = quantile(theta[,net.ind[j]], c(0.025, 0.975))
  
  bag.CS.r[j,] = quantile(ranks.year[,bag.ind[j]], c(0.025, 0.975))
  net.CS.r[j,] = quantile(ranks.year[,net.ind[j]], c(0.025, 0.975))
  
}

###################################################################
## BAY OF BISCAY
###################################################################

BB.ALL = read.csv(file='top15BB.ALL.csv', header=T, stringsAsFactors=F)

f.all = BB.ALL$freq
items.all = BB.ALL$items
N.all = BB.ALL$N
p.all = 100 * f.all / N.all
round(p.all, 1)[1:10]
ranks.all.BB = 16 - rank(f.all)

set.seed(2001)
st_mod<- stan_model("litter.stan")

datp1 <- list(N=15, n=N.all, r=f.all, m=0, v=10, d=0.01, a=0.01)
fitp1 <- sampling(object = st_mod, data=datp1)

ex.p1 <- extract(fitp1)
dim(ex.p1$theta)                      # 4000 * 15
theta = ex.p1$theta
head(theta)

#### Each column gives the ranks for the 15 litter types
#### There are no ties
ranks.all <- 16 - t(apply(theta,1,rank))
ns = dim(ranks.all)[1]
head(ranks.all)

pconf.BB = matrix(0, ncol=2, nrow=10)
rconf.BB = matrix(0, ncol=2, nrow=10)

## Marginal confidence intervals of the theta
pconf.BB[1,] = quantile(theta[,1], c(0.025, 0.975))
pconf.BB[2,] = quantile(theta[,2], c(0.025, 0.975))
pconf.BB[3,] = quantile(theta[,3], c(0.025, 0.975))
pconf.BB[4,] = quantile(theta[,4], c(0.025, 0.975))
pconf.BB[5,] = quantile(theta[,5], c(0.025, 0.975))
pconf.BB[6,] = quantile(theta[,6], c(0.025, 0.975))
pconf.BB[7,] = quantile(theta[,7], c(0.025, 0.975))
pconf.BB[8,] = quantile(theta[,8], c(0.025, 0.975))
pconf.BB[9,] = quantile(theta[,9], c(0.025, 0.975))
pconf.BB[10,] = quantile(theta[,10], c(0.025, 0.975))
round(pconf.BB,2)

## Marginal confidence intervals of ranks
rconf.BB[1,] = quantile(ranks.all[,1], c(0.025, 0.975))
rconf.BB[2,] = quantile(ranks.all[,2], c(0.025, 0.975))
rconf.BB[3,] = quantile(ranks.all[,3], c(0.025, 0.975))
rconf.BB[4,] = quantile(ranks.all[,4], c(0.025, 0.975))
rconf.BB[5,] = quantile(ranks.all[,5], c(0.025, 0.975))
rconf.BB[6,] = quantile(ranks.all[,6], c(0.025, 0.975))
rconf.BB[7,] = quantile(ranks.all[,7], c(0.025, 0.975))
rconf.BB[8,] = quantile(ranks.all[,8], c(0.025, 0.975))
rconf.BB[9,] = quantile(ranks.all[,9], c(0.025, 0.975))
rconf.BB[10,] = quantile(ranks.all[,10], c(0.025, 0.975))

## labels should match items.all
items.all
labels = c("Rope.Synth", "Sheet plas", "Line mono", "Line tang", "Bag plas",
           "Fish net", "Other plas", "Clothing", "Bot plas", "Rope nat")

## Plot the rank 95% credible intervals
## The actual ranks are plotted as circles
maxrank = max(rconf.BB[1:10,])

## Plot for FIGURE 3 (c)

#jpeg("C:\\Users\\jtb00\\OneDrive - CEFAS\\JTB00\\Fish Stuff\\Consultancy 2022\\Top 10 litter paper\\Paper 2022//BB cred ints.jpg", height=12, width=15, res=600, pointsize=9, units="cm")
par(mar=c(4,7,4,4), mfrow=c(1,1))
plot(0:maxrank, 0:maxrank, ylab=' ', xlab='Rank', type='n', axes=F,
     ylim=c(0,10), xlim=c(1,maxrank), cex.lab=1.5)
axis(1, labels=1:maxrank, at=1:maxrank, cex=2, cex.axis=1.2)
axis(2, seq(0.5,9.5,1), rev(labels), cex=2, cex.axis=1.2, las=1)
box()

labels = c("Rope.Synth", "Sheet plas", "Line mono", "Line tang", "Bag plas",
           "Fish net", "Other plas", "Clothing", "Bot plas", "Rope nat")

for (j in 1:10) {
    yval = 11 - j - 0.5
    if ((j==2) | (j==7) | (j==8)) col='black'
    if ((j==3) | (j==4)|(j==6)) col='blue'
    if ((j==1) | (j==10)) col='slategray'
    if ((j==5) | (j==9)) col='gold'
    lines(c(rconf.BB[j,1], rconf.BB[j,2]), c(yval,yval), col=col, lwd=2)
    points(ranks.all.BB[j], yval, col=col, cex=1.2,lwd=2)
}
title('(c)', cex.main=2)
#dev.off()

## *********************************************************
## Temporal analysis for selected items: BB
## *********************************************************

BB = read.csv(file='top15BB.csv', header=T, stringsAsFactors=F)
names(BB)
BB.N = read.csv(file='BB.N.csv', header=T, stringsAsFactors=F)

bag.BB.p = matrix(0, nrow=9, ncol=2)
bag.BB.r = matrix(0, nrow=9, ncol=2)

net.BB.p = matrix(0, nrow=9, ncol=2)
net.BB.r = matrix(0, nrow=9, ncol=2)

bag.ind = rep(0,9)
net.ind = rep(0,9)

bag.rank.BB = rep(0,9)
net.rank.BB = rep(0,9)

## Check whether each of the three items is present in each year

bag.pres = rep(0,9); bottle.pres = rep(0,9); net.pres = rep(0,9)

for (j in 1:9) {
  items = BB[,j]
  if ("bag.bin" %in% items) bag.pres[j] = 1
  if ("bottles.plas.bin" %in% items) bottle.pres[j] = 1
  if ("fishnet.bin" %in% items) net.pres[j] = 1
}
bag.pres
net.pres

## No net in 2016 (5th entry)

for (j in 1:9) {
  
  f.year = BB[,j+9]
  items.year = BB[,j]
  N.year = rep(BB.N$N[j], 15)
  p.year = 100 * f.year / N.year
  ranks.year.BB = 16 - rank(f.year)
  
  index = 1:15
  bag.ind[j] = index[items.year=="bag.bin"]
  if (j!=5) net.ind[j] = index[items.year=="fishnet.bin"]
  
  bag.rank.BB[j] = ranks.year.BB[bag.ind[j]]
  if (j!=5) net.rank.BB[j] = ranks.year.BB[net.ind[j]]
  
  set.seed(2001)
  
  datp1 <- list(N=15, n=N.year, r=f.year, m=0, v=10, d=0.01, a=0.01)
  fitp1 <- sampling(object = st_mod, data=datp1)
  
  ex.p1 <- extract(fitp1)
  theta = ex.p1$theta
  
  #### Each column gives the ranks for the 15 litter types
  
  ranks.year <- 16 - t(apply(theta,1,rank))
  dim(ranks.year)
  
  ## Marginal confidence intervals of the theta
  
  bag.BB.p[j,] = quantile(theta[,bag.ind[j]], c(0.025, 0.975))
  net.BB.p[j,] = quantile(theta[,net.ind[j]], c(0.025, 0.975))
  
  bag.BB.r[j,] = quantile(ranks.year[,bag.ind[j]], c(0.025, 0.975))
  net.BB.r[j,] = quantile(ranks.year[,net.ind[j]], c(0.025, 0.975))
  
}

net.BB.r[5,] = c(0,0)
net.BB.r

###################################################################
## BALTIC SEA
###################################################################

BS.ALL = read.csv(file='top15BS.ALL.csv', header=T, stringsAsFactors=F)

f.all = BS.ALL$freq
items.all = BS.ALL$items
N.all = BS.ALL$N
p.all = 100 * f.all / N.all
round(p.all, 1)[1:10]
ranks.all.BS = 16 - rank(f.all)

datp1 <- list(N=15, n=N.all, r=f.all, m=0, v=10, d=0.01, a=0.01)
fitp1 <- sampling(object = st_mod, data=datp1)

ex.p1 <- extract(fitp1)
dim(ex.p1$theta)                      # 4000 * 15
theta = ex.p1$theta
head(theta)

#### Each column gives the ranks for the 15 litter types
#### There are no ties
ranks.all <- 16 - t(apply(theta,1,rank))
ns = dim(ranks.all)[1]
head(ranks.all)

pconf.BS = matrix(0, ncol=2, nrow=10)
rconf.BS = matrix(0, ncol=2, nrow=10)

## Marginal confidence intervals of the theta
pconf.BS[1,] = quantile(theta[,1], c(0.025, 0.975))
pconf.BS[2,] = quantile(theta[,2], c(0.025, 0.975))
pconf.BS[3,] = quantile(theta[,3], c(0.025, 0.975))
pconf.BS[4,] = quantile(theta[,4], c(0.025, 0.975))
pconf.BS[5,] = quantile(theta[,5], c(0.025, 0.975))
pconf.BS[6,] = quantile(theta[,6], c(0.025, 0.975))
pconf.BS[7,] = quantile(theta[,7], c(0.025, 0.975))
pconf.BS[8,] = quantile(theta[,8], c(0.025, 0.975))
pconf.BS[9,] = quantile(theta[,9], c(0.025, 0.975))
pconf.BS[10,] = quantile(theta[,10], c(0.025, 0.975))
round(pconf.BS,2)

## Marginal confidence intervals of ranks
rconf.BS[1,] = quantile(ranks.all[,1], c(0.025, 0.975))
rconf.BS[2,] = quantile(ranks.all[,2], c(0.025, 0.975))
rconf.BS[3,] = quantile(ranks.all[,3], c(0.025, 0.975))
rconf.BS[4,] = quantile(ranks.all[,4], c(0.025, 0.975))
rconf.BS[5,] = quantile(ranks.all[,5], c(0.025, 0.975))
rconf.BS[6,] = quantile(ranks.all[,6], c(0.025, 0.975))
rconf.BS[7,] = quantile(ranks.all[,7], c(0.025, 0.975))
rconf.BS[8,] = quantile(ranks.all[,8], c(0.025, 0.975))
rconf.BS[9,] = quantile(ranks.all[,9], c(0.025, 0.975))
rconf.BS[10,] = quantile(ranks.all[,10], c(0.025, 0.975))

## labels should match items.all
items.all
labels = c("Sheet plas", "Other nat", "Other plas",
           "Bag plas", "Bot plas", "Other met", "Clothing", "Other misc",
           "Wood proc", "Rope synth")

## Plot the rank 95% credible intervals
## The actual ranks are plotted as circles
maxrank = max(rconf.BS[1:10,])

## Plot for FIGURE 3 (d)

#jpeg("C:\\Users\\jtb00\\OneDrive - CEFAS\\JTB00\\Fish Stuff\\Consultancy 2022\\Top 10 litter paper\\Paper 2022//BS cred ints.jpg", height=12, width=15, res=600, pointsize=9, units="cm")

par(mar=c(4,7,4,4), mfrow=c(1,1))
plot(0:maxrank, 0:maxrank, ylab=' ', xlab='Rank', type='n', axes=F,
     ylim=c(0,10), xlim=c(1,maxrank), cex.lab=1.5)
axis(1, labels=1:maxrank, at=1:maxrank, cex=2, cex.axis=1.2)
axis(2, seq(0.5,9.5,1), rev(labels), cex=2, cex.axis=1.2, las=1)
box()

for (j in 1:10) {
  yval = 11 - j - 0.5
  if ((j==1)|(j==2)|(j==3)|(j==6)|(j==7)|(j==8)|(j==9)) col='black'
  if (j==10) col='slategray'
  if ((j==4)|(j==5)) col='gold'
  lines(c(rconf.BS[j,1], rconf.BS[j,2]), c(yval,yval), col=col, lwd=2)
  points(ranks.all.BS[j], yval, col=col, cex=1.2,lwd=2)
  
}

title('(d)', cex.main=2)
#dev.off()

## *********************************************************
## Temporal analysis for selected items BS
## *********************************************************

BS = read.csv(file='top15BS.csv', header=T, stringsAsFactors=F)
names(BS)
BS.N = read.csv(file='BS.N.csv', header=T, stringsAsFactors=F)
BS.N

bag.BS.p = matrix(0, nrow=9, ncol=2)
bag.BS.r = matrix(0, nrow=9, ncol=2)

net.BS.p = matrix(0, nrow=9, ncol=2)
net.BS.r = matrix(0, nrow=9, ncol=2)

bag.ind = rep(0,9)
net.ind = rep(0,9)

bag.rank.BS = rep(0,9)
net.rank.BS = rep(0,9)

## Check whether each of the three items is present in each year

bag.pres = rep(0,9); bottle.pres = rep(0,9); net.pres = rep(0,9)

# bag present in every year
# bottle not present in first 2 years
# net not present at all

for (j in 1:9) {
  items = BS[,j]
  if ("bag.bin" %in% items) bag.pres[j] = 1
  if ("bottles.plas.bin" %in% items) bottle.pres[j] = 1
  if ("fishnet.bin" %in% items) net.pres[j] = 1
}
bag.pres; bottle.pres; net.pres

for (j in 1:9) {
  
  f.year = BS[,j+9]
  items.year = BS[,j]
  N.year = rep(BS.N$N[j], 15)
  p.year = 100 * f.year / N.year
  ranks.year.BS = 16 - rank(f.year)
  
  index = 1:15
  bag.ind[j] = index[items.year=="bag.bin"]
  
  bag.rank.BS[j] = ranks.year.BS[bag.ind[j]]
  
  set.seed(2001)
  
  datp1 <- list(N=15, n=N.year, r=f.year, m=0, v=10, d=0.01, a=0.01)
  fitp1 <- sampling(object = st_mod, data=datp1)
  
  ex.p1 <- extract(fitp1)
  theta = ex.p1$theta
  
  #### Each column gives the ranks for the 15 litter types
  
  ranks.year <- 16 - t(apply(theta,1,rank))
  dim(ranks.year)
  
  ## Marginal confidence intervals of the theta
  
  bag.BS.p[j,] = quantile(theta[,bag.ind[j]], c(0.025, 0.975))
  
  bag.BS.r[j,] = quantile(ranks.year[,bag.ind[j]], c(0.025, 0.975))
  
}

## BAG INDIVIDUAL PLOT

## Plot the rank 95% credible intervals
## The actual ranks are plotted as circles
maxrank = max(c(bag.GNS.r, bag.CS.r, bag.BB.r, bag.BS.r))   
maxrank

## Plot for FIGURE 4

#jpeg("C:\\Users\\jtb00\\OneDrive - CEFAS\\JTB00\\Fish Stuff\\Consultancy 2022\\Top 10 litter paper\\Paper 2022//Bag temp ranks.jpg", height=12, width=15, res=600, pointsize=9, units="cm")
par(mar=c(4,5,4,4))
inc = 0.1
plot(2012:2020, 16 - bag.GNS.r[,1], ylab='Rank', xlab='Year', axes=F,
     ylim=c(1,15), type='n', xlim=c(2011.5, 2020.5), cex.axis=1.2)

for (j in 1:9) {
  lines(rep(j+2011-1.5*inc,2), c(16-bag.GNS.r[j,1], 16-bag.GNS.r[j,2]), col=1,
        cex=1.5, lty=1, lwd=1.5)
  points(j+2011-1.5*inc, 16-bag.rank.GNS[j], cex=1.2, col=1, pch=1)
}
for (j in 1:9) {
  lines(rep(j+2011-0.5*inc,2), c(16-bag.CS.r[j,1], 16-bag.CS.r[j,2]),
        col=3,cex=1.5, lty=2, lwd=1.5)
  points(j+2011-0.5*inc, 16-bag.rank.CS[j], cex=1.2, col=3,pch=2)
}

k = 0
for (j in c(1:9)) {
    k = k + 1
    lines(rep(j+2011+0.5*inc,2), c(16-bag.BB.r[k,1], 16-bag.BB.r[k,2]),
        col=2, cex=1.2, lty=3,lwd=1.5)
  points(j+2011+0.5*inc, 16-bag.rank.BB[k], cex=1.2, col=2, pch=3)
}
for (j in 1:9) {
  lines(rep(j+2011+1.5*inc,2), c(16-bag.BS.r[j,1], 16-bag.BS.r[j,2]),
        col=4, cex=1.5, lty=4,lwd=1.5)
  points(j+2011+1.5*inc, 16-bag.rank.BS[j], cex=1.2, col=4, pch=4)
}

axis(1, cex=2)
axis(2, labels=15:1, at=1:15, las=1, cex=2)
box()

legend("bottomright", legend=c("GNS", "CS", "BB", "BS"), col=c(1,3,2,4),
       pch=c(1,2,3,4), lty=c(1,2,3,4), lwd=c(1.5,1.5,1.5,1.5))
#dev.off()

## NET INDIVIDUAL PLOT

## Note that for BS no NET values in top 10 for any year. So don't
## plot NET for BS.

maxrank = max(c(net.GNS.r, net.CS.r, net.BB.r, net.BS.r), na.rm=T)   
maxrank

## Plot for FIGURE 5

#jpeg("C:\\Users\\jtb00\\OneDrive - CEFAS\\JTB00\\Fish Stuff\\Consultancy 2022\\Top 10 litter paper\\Paper 2022//NET temp ranks.jpg", height=12, width=15, res=600, pointsize=9, units="cm")
par(mar=c(4,5,4,4))
inc = 0.1
plot(2012:2020, 16 - net.GNS.r[,1], ylab='Rank', xlab='Year', axes=F,
     ylim=c(1,15), type='n', xlim=c(2011.5, 2020.5), cex.axis=1.2)

for (j in 1:9) {
  lines(rep(j+2011-1.5*inc,2), c(16-net.GNS.r[j,1], 16-net.GNS.r[j,2]), col=1,
        cex=1.5)
  points(j+2011-1.5*inc, 16-net.rank.GNS[j], cex=1.5, lty=1, lwd=1.5)
}
for (j in 1:9) {
  lines(rep(j+2011-0.5*inc,2), c(16-net.CS.r[j,1], 16-net.CS.r[j,2]),
        col=3, cex=1.5, lty=2, lwd=1.5)
  points(j+2011-0.5*inc, 16-net.rank.CS[j], cex=1.2, col=3, pch=2)
}

## For BB in 2016 there were no Nets. So leave blank in the plot.

k = 0
for (j in c(1:9)) {
  k = k + 1
  lines(rep(j+2011+0.5*inc,2), c(16-net.BB.r[k,1], 16-net.BB.r[k,2]),
        col=2, cex=1.5, lty=3, lwd=1.5)
  points(j+2011+0.5*inc, 16-net.rank.BB[k], cex=1.2, col=2, pch=3)
}

axis(1, cex=2)
axis(2, labels=15:1, at=1:15, las=1, cex=2)
box()

legend("topright", legend=c("GNS", "CS", "BB"), col=c(1,3,2),
       pch=c(1,2,3), lwd=rep(1.5, 3), lty=1:3)
#dev.off()


## ***************************************************************
## TOP 10 FROM 2012 TO 2020. Give percentage for each value.
## ***************************************************************

## Numbers for Tables S1 to S4 in supplementary material. The frequencies are
## stored in f.year and the percentages in p.year. The names of the top 10
## items in each year come from the GNS, CS, BB and BS data frames.

GNS = read.csv(file='top15GNS.csv', header=T, stringsAsFactors=F)
names(GNS)
GNS.N = read.csv(file='GNS.N.csv', header=T, stringsAsFactors=F)

p.year = matrix(0, ncol=9, nrow=15)
f.year = matrix(0, ncol=9, nrow=15)

for (j in 1:9) {
  
  temp = GNS[,j+9]
  f.year[,j] = temp
  items.year = GNS[,j]
  N.year = GNS.N$N[j]
  p.year[,j] = 100 * temp / N.year
  
}
  
f.year[1:10,]
round(p.year[1:10,])

CS = read.csv(file='top15CS.csv', header=T, stringsAsFactors=F)
names(CS)
CS.N = read.csv(file='CS.N.csv', header=T, stringsAsFactors=F)

p.year = matrix(0, ncol=9, nrow=15)
f.year = matrix(0, ncol=9, nrow=15)

for (j in 1:9) {
  
  temp = CS[,j+9]
  f.year[,j] = temp
  items.year = CS[,j]
  N.year = CS.N$N[j]
  p.year[,j] = 100 * temp / N.year
  
}

f.year[1:10,]
round(p.year[1:10,])

BB = read.csv(file='top15BB.csv', header=T, stringsAsFactors=F)
names(BB)
BB.N = read.csv(file='BB.N.csv', header=T, stringsAsFactors=F)
p.year = matrix(0, ncol=9, nrow=15)
f.year = matrix(0, ncol=9, nrow=15)

for (j in c(1:9)) {
  
  temp = BB[,j+9]
  f.year[,j] = temp
  items.year = BB[,j]
  N.year = BB.N$N[j]
  p.year[,j] = 100 * temp / N.year
  
}

f.year[1:10,]
round(p.year[1:10,],0)

BS = read.csv(file='top15BS.csv', header=T, stringsAsFactors=F)
names(BS)
BS.N = read.csv(file='BS.N.csv', header=T, stringsAsFactors=F)
BS.N
p.year = matrix(0, ncol=9, nrow=15)
f.year = matrix(0, ncol=9, nrow=15)

for (j in 1:9) {
  
  temp = BS[,j+9]
  f.year[,j] = temp
  items.year = BS[,j]
  N.year = BS.N$N[j]
  p.year[,j] = 100 * temp / N.year
  
}

f.year[1:10,]
round(p.year[1:10,])

######################################################
## PERCENTAGES FOR BT and GOV in smaller GNS region
## Used in Table 2 of the paper for comparing BT and
## GOV hauls in part of the GNS.
######################################################
## BT ##
BT = read.csv(file='top15BT.smallGNS.csv', header=T, stringsAsFactors=F)
names(BT)
perc = round(100 * BT$freq / BT$N, 1)
cbind(BT$items[1:10], BT$freq[1:10], perc[1:10])

## GOV ##
GOV = read.csv(file='top15GOV.smallGNS.csv', header=T, stringsAsFactors=F)
names(GOV)
perc = round(100 * GOV$freq / GOV$N, 1)
cbind(GOV$items[1:10], GOV$freq[1:10], perc[1:10])

######################################################
## PERCENTAGES FOR COMBINED GNS, CS AND BB
## Used in Table 1 (4th column)
######################################################

COM = read.csv(file='top15COM.ALL.csv', header=T, stringsAsFactors=F)
names(COM)
perc = round(100 * COM$freq / COM$N, 1)

cbind(COM$items[1:10], COM$freq[1:10], perc[1:10])

