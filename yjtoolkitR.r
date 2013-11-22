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
