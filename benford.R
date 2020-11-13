library(tidyverse)


obs <- c(205, 138, 94, 102, 44, 54, 43, 40, 35)
tot <- sum(obs)

dgt <- seq(1, 9)
e.prb <- log10(1 + 1/dgt)
xpt <- e.prb * tot

smp <- 10000

nll <- rmultinom(smp, tot, e.prb)
nll <- as.list(as.data.frame(nll))

alt <- rmultinom(smp, tot, obs/tot)
alt <- as.list(as.data.frame(alt))

dst.l1 <- function(x, y) {
  return(sum(abs(x - y)))
}

dst.l2 <- function(x, y) {
  return(sum((x - y)^2))
}

dst.nll <- c()
dst.alt <- c()
dlt <- c()
for (i in 1:smp) {
  x <- sample(1:smp, 1)
  y <- sample(1:smp, 1)
  n.e <- dst.l2(nll[[x]], nll[[y]])
  dst.nll <- c(dst.nll, n.e)
  n.a <- dst.l2(nll[[x]], alt[[y]])
  dst.alt <- c(dst.alt, n.a)
  dlt <- c(dlt, n.a - n.e)
}

df1 <- data.frame(dst = dst.nll, typ = "0")
df2 <- data.frame(dst = dst.alt, typ = "1")
df <- rbind(df1, df2)

ggplot(df, aes(dst, fill = typ)) +
  geom_histogram(alpha = 0.5, position = 'identity')
hist(dlt)
cat(sum(dlt < 0)/smp)

q.n <- quantile(dst.nll, c(0.95))
q.a <- quantile(dst.alt, c(0.05))
cat(q.a, q.n)

n.df <- data.frame(d = nll.smp, t = "null")
d.df <- data.frame(d = nll.smp, t = "null")







o.dlt <- dst.l2(xpt, obs)
p.val <- sum(nll.smp >= o.dlt)/length(nll.smp)
cat(o.dlt, p.val)


o.dlt <- sum(abs(obs - xpt)) / (2 * tot) 



err.vct <- abs(obs - xpt)
dst <- sapply(nll, function(x){y <- sum(err.vct > abs(x - xpt))})

e.dlt <- sapply(nll, function(x){y <- sum(abs(x - e.prb * sum(obs))) / (2 * sum(obs))})

err <- sapply(nll, function(x){y <- x - e.prb * sum(obs)})
err <- unlist(err)
hist(err)
shapiro.test(err)
ks.test(err, "pnorm", mean(err), sd(err))


hist(e.dlt)

p.val <- sum(o.dlt <= e.dlt) / length(e.dlt)
cat(o.dlt, p.val)

