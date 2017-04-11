#Program Temp
#Author: Feixiao_L
#======================================================================================
#
Xuanmen = function(phi1, phi2, R = 20, d = 10, r = 10){
	#1. var: give the coordinate of point A, B and O
	A = c(cos(phi1), sin(phi1)) * R
	B = c(cos(phi2), sin(phi2)) * R
	O = c(d, 0)
	#2. functions:
	#2.1 give the intersection angle of 2 vectors
	Theta = function(vec1, vec2){if((vec1 %*% vec1) * (vec2 %*% vec2) == 0){return(0)}; return(acos(min(max((vec1 %*% vec2)/sqrt((vec1 %*% vec1) * (vec2 %*% vec2)), -1), 1)))}
	#2.2 give the intersection point of the line AB and the circle O
	Solve = function(){
		#calculate the line AB 
		D = A - B; k = D[2] / D[1]; b = B[2] - k * B[1]
		#calculate the coefficient of the quadratic equation
		A = 1 + k^2; B = 2 * (k * b - d); C = d^2 + b^2 - r^2
		delta = B^2 - 4 * A * C
		#calculate the result
		X = c((-B + sqrt(max(delta, 0))) / (2 * A), (-B - sqrt(max(delta, 0))) / (2 * A))
		Y = k * X + b
		return(cbind(X, Y))
	}
	#2.3 judge the points whether in circle O
	Judge = function(vec){return(ifelse(vec %*% vec - r^2 < -1e-10, TRUE, FALSE)[1, 1])}
	#2.4 give the area of triangle
	triArea = function(vec1, vec2){return(1/2 * abs(vec1[1] * vec2[2] - vec1[2] * vec2[1]))}
	#3. calculate
	#3.0 Judge the points
	#3.00 A == B
	if(identical(A, B) | identical(A, O) | identical(B, O)){return(0)}
	#3.1 A and B in circle O
	if(Judge(A-O) & Judge(B-O)){return(triArea(A - O, B - O))}
	#3.2 A or B in circle O
	if(Judge(A-O) | Judge(B-O)){
		if(Judge(B-O)){Temp = B; B = A; A = Temp}
		C = Solve()[1, ]
		D = Solve()[2, ]
		if(Theta(B-A, C-A) != 0){C = D}
		return(triArea(C - O, A - O) + 1/2 * r^2 * Theta(B - O, C - O))
	}
	#3.3 A and B not in circle O
	#3.3.1 A[1] == B[1]
	if(abs(A[1] - B[1]) < 1e-10){
	#3.3.1.1 d_OAB < r
		if(abs(A[1] - d) < r){
			C = c(A[1], sqrt(r^2 - (A[1] - d)^2))
			D = c(A[1], -sqrt(r^2 - (A[1] - d)^2))
			return((Theta(A - O, B - O) - Theta(C - O, D - O) + sin(Theta(C - O, D - O))) * r^2 * 1/2)
		}
	#3.3.1.2 d_OAB >= r
		return(Theta(A - O, B - O) * r^2 * 1/2)
	}
	#3.3.2 A[1] != B[1]
	C = Solve()[1, ]
	D = Solve()[2, ]
	#3.3.2.1 C == D
	if(identical(C, D)){return(Theta(A - O, B - O) * r^2 * 1/2)}
	#3.3.2.2 C != D
	if((Theta(A - O, B - O) - Theta(C - O, D - O) + sin(Theta(C - O, D - O))) * r^2 * 1/2 < 0){return(Theta(A - O, B - O)* r^2 * 1/2)}
	return((Theta(A - O, B - O) - Theta(C - O, D - O) + sin(Theta(C - O, D - O))) * r^2 * 1/2)
}
#======================================================================================
#
maxXuanmen = function(R, d, r = 10, meshgrid = 51){
	X = seq(0, 360, length.out = meshgrid)
	Y = X
	Re = matrix(0, meshgrid, meshgrid)
	for(i in 1:meshgrid){
		for(j in 1:meshgrid){
			Re[i, j] = Xuanmen(X[i]/180 * pi, Y[j]/180 * pi, R, d, r)
		}
	}
	return(max(Re))
}
#======================================================================================
#
X = seq(0, 360, length.out = 121)
Y = X
Re = matrix(0, 121, 121)
for(i in 1:121){
	for(j in 1:121){
		Re[i, j] = Xuanmen(X[i]/180 * pi, Y[j]/180 * pi)
	}
}
#
X = seq(0, 20, length.out = 51)
Y = seq(0, 10, length.out = 26)
Re = matrix(0, 51, 26)
for(i in 1:51){
	for(j in 1:26){
		Re[i, j] = maxXuanmen(X[i], Y[j])
	}
}
#
library(rgl)
jet.colors = colorRampPalette(c('#000080', '#0000FF', '#0080FF', '#00FFFF', '#80FF80', '#FFFF00', '#FF8000', '#FF0000', '#800000'))
color = jet.colors(100)
Recol = cut(Re, 100)
persp3d(X, Y, Re, alpha=0.3, front = 'lines', back = 'lines', col = color[Recol])
movie3d(spin3d(axis = c(0, 0, 1), rpm = 4), duration = 15, fps = 16, dir = 'C:/Users/Feixiao_L/Desktop/dir', convert = FALSE)
png('C:/Users/Feixiao_L/Desktop/contour.png', 600, 600)
filled.contour(X, Y, Re, color = jet.colors, nlevels = 100)
dev.off()














