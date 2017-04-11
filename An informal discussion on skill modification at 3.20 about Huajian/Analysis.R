#Program informal discussion
#Author: Feixiao_L
#======================================================================================
#Core function
Dot_effect = function(x, Time = 24, Begin = 6, Prop = 0.4){
	P = matrix(0, 8, 8)
	for(i in 3:7){P[i, i-1] = (1-x) * Prop; P[i, i] = (1-x) * Prop; P[i, 8] = x}
	P[1, 1] = 1
	P[8, 7] = (1-x) * Prop; P[8, 8] = (1+x) * Prop
	P[2, 1] = 0.5; P[2, 2] = (1-x) * Prop; P[2, 8] = x * Prop
	Q = P; P = diag(8)
	for(i in 1:Time){P = P %*% Q}
	return(P[Begin+1, ])
}

Dot_renew = function(x, Time = 24, Begin = 6, Prop = 0.4){
	P = matrix(0, 8, 8)
	for(i in 2:6){P[i, i-1] = (1-x) * Prop; P[i, i] = (1-x) * Prop; P[i, 7] = x * Prop; P[i, 8] = x * Prop}
	P[1, 1] = 1
	P[7, 6] = (1-x) * Prop; P[7, 7] = 1 * Prop; P[7, 8] = x * Prop
	P[8, 7] = 1 * Prop; P[8, 8] = 1 * Prop
	Q = P; P = diag(8)
	for(i in 1:Time){P = P %*% Q}
	return(P[Begin+1, ])
}
#======================================================================================
#Plot
a = Dot_renew(0.3)
b = Dot_effect(0.3)
plot(0:7, a, xlim = c(0, 8), ylim = c(0, 1), col = 'dark blue', ylab = 'propability', xlab = 'dots', main = 'Dots after a single skill circulation(30% critical, 24 Hits)')
for(i in 0:7){segments(i, 0, i, a[i+1], col = 'dark blue')}
points(0:7+0.5, b, col = 'dark red')
for(i in 0:7){segments(i+0.5, 0, i+0.5, b[i+1], col = 'dark red')}
d = 0
e = 0
for(i in 1:8){d[i] = sum(a[1:i])}
for(i in 1:8){e[i] = sum(b[1:i])}
dd = c(0, rep(d, each = 2))[1:16]
ee = c(0, rep(e, each = 2))[1:16]
points(rep(0:7, each = 2)[2:16], dd[2:16], 'l', col = 'gray30')
points(rep(0:7, each = 2)[2:16], ee[2:16], 'l', col = 'gray20', lty = 2)
legend('topleft', legend = c('c.d.f. of renew first', 'p.m.f. of renew first', 'c.d.f. of effect first', 'p.m.f. of effect first'), col = c('gray30', 'dark blue', 'gray20', 'dark red'), lty = c(1, 1, 2, 1))
bb = 0
dd = 0
ddd = 0
bbb = 0
x = 0:1000/1000
for(i in 1:1001){bb[i] = Dot_renew(x[i], 24)[1]; dd[i] = Dot_effect(x[i], 24)[1]; bbb[i] = Dot_renew(x[i], 15)[1]; ddd[i] = Dot_effect(x[i], 15)[1]}
plot(x*100, dd, 'l', col = 'dark red', ylim = c(0, 0.12), xlab = 'Critical rate(%)', ylab = 'Propability of losing dot', main = 'Relations between propability of losing dot and critical rate')
points(x*100, bb, 'l', col = 'gray20', lty = 2)
points(x*100, ddd, 'l', col = 'dark blue')
points(x*100, bbb, 'l', col = 'gray20', lty = 3)
abline(h = c(0.05, 0.01), lty = 2, col = 'gray30')
legend('topright', legend = c('renew first; 24 hits', 'effect first; 24 hits', 'renew first; 15 hits', 'effect first; 15 hits'), col = c('gray20', 'dark red', 'gray20', 'dark blue'), lty = c(2, 1, 3, 1))