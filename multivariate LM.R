#using the formula lm()

advertising <- read.csv("advertising.csv")
regression = lm(advertising$sales~advertising$TV+advertising$radio+advertising$newspaper)
print(regression)

#using matrices

y <- advertising$sales
x1 <- advertising$TV
x2 <- advertising$radio
x3 <- advertising$newspaper

X = cbind(matrix(1, length(x1)), matrix(x1), matrix(x2), matrix(x3))
Y = matrix(y)

Xt = t(X)
XtX = Xt%*%X
XtX_inv = solve(XtX)

theta = (XtX_inv%*%Xt)%*%Y
print(theta)