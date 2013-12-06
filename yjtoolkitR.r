# CLT
sd.pe <- function(p, n) {sqrt(p * ( 1 - p)/n)}
z.pe <- function(alpha) {abs(qnorm(alpha/2))}
moe.pe <- function(alpha, se) {abs(qnorm(alpha/2)) * se}
lower.ci.pe <- function(p.hat, moe.pe) {p.hat - moe.pe}
upper.ci.pe <- function(p.hat, moe.pe) {p.hat + moe.pe}
# sample size calculation
size.pe <- function(alpha, p, moe) { qnorm(1-alpha/2)^2 * p * (1-p) / moe^2 }
# chi-square
chi.exp.matrix <- function(obs) { (rowSums(obs) %o% colSums(obs)) / sum(colSums(obs))}
chisq <- function(obs, exp) {sum((obs-exp)^2/exp)}
# t distribution
se.t <- function(s,n) {s/sqrt(n)}
moe.t <- function(alpha, se.t, n) {qt(1-alpha/2, n-1) * se.t}
lower.ci.t <- function(mean, moe.t) {mean - moe.t}
upper.ci.t <- function(mean, moe.t) {mean + moe.t}
t.score <- function(xbar, s, n, mu) {(xbar-mu)/(s/sqrt(n))}
p.val.t <- function(t.score, n) {pt(t.score, n-1)}
# z distribution
sd.z <- function(sigma, n) {sigma/sqrt(n)}
moe.z <- function(alpha, sd.z) {qnorm(1-alpha/2) * sd.z}
lower.ci.z <- function(xbar, moe.z) {xbar - moe.z}
upper.ci.z <- function(xbar, moe.z) {xbar + moe.z}
z.score <- function(xbar, sigma, n, mu) {(xbar-mu)/(sigma/sqrt(n))}
p.val.z <- function(z.score) {pnorm(z.score)}
# two population means from summary data
se.diff.mean <- function(s1, n1, s2, n2) {sqrt(s1^2/n1 + s2^2/n2)}
df.diff <- function(s1, n1, s2, n2) {((s1^2/n1) + (s2^2/n2))^2/(((s1^2/n1)^2/(n1-1))+((s2^2/n2)^2/(n2-1)))}
moe.diff <- function(alpha, se.diff.mean, df) {qt(1-alpha/2, df) * se.diff.mean}
upper.ci.diff <- function(diff.mean, moe) {diff.mean + moe}
lower.ci.diff <- function(diff.mean, moe) {diff.mean - moe}
# Sample size calculations
z.alpha.2 <- function(alpha) { qnorm(1-alpha/2)}
ss.prop <- function(p.hat, z.alpha.2, moe) { p.hat * (1 - p.hat) * (z.alpha.2/moe)^2 }
ss.mean <- function(z.alpha.2, sigma, moe) { (z.alpha.2 * sigma/moe)^2 }
# Simple linear regression
sum.squares <- function(x, xbar) { sum((x-xbar)^2)}
se.slope <- function(s.e, sum.squares) { s.e/sqrt(sum.squares) }
t.value <- function(b1, se.slope) { b1 / se.slope }
point.est <- function(b0, b1, x) { b0 + b1 * x }
t.alpha.2 <- function (alpha, n) { qt(1-alpha/2, df=n-2) }
ci.lower <- function (point.est, t.alpha.2, s.e, x, xbar, sum.squares, n) { point.est - t.alpha.2 * (s.e * sqrt(1/n + ((x-xbar)^2 / sum.squares)))}
ci.upper <- function (point.est, t.alpha.2, s.e, x, xbar, sum.squares, n) { point.est + t.alpha.2 * (s.e * sqrt(1/n + ((x-xbar)^2 / sum.squares)))}
pred.lower <- function (point.est, t.alpha.2, s.e, x, xbar, sum.squares, n) { point.est - t.alpha.2 * (s.e * sqrt(1+1/n + ((x-xbar)^2 / sum.squares)))}
pred.upper <- function (point.est, t.alpha.2, s.e, x, xbar, sum.squares, n) { point.est + t.alpha.2 * (s.e * sqrt(1+1/n + ((x-xbar)^2 / sum.squares)))}
