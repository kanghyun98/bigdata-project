# 회귀분석 MapReduce
regmap <- function(., v) {
  dat <- data.frame(total_amount =
  v$total_amount, trip_distance = v$trip_distance, trip_time_in_secs = v$trip_time_in_secs)
  Xk <- model.matrix(total_amount ~ trip_distance + trip_time_in_secs, dat)
  yk <- as.matrix(dat[,1]) XtXk <- crossprod(Xk, Xk) Xtyk <- crossprod(Xk, yk) ytyk <- crossprod(yk, yk) res <- list(XtXk, Xtyk, ytyk) keyval(1, res)
}

regreduce <- function(k, v) {
  XtX <- Reduce("+", v[seq_along(v) %% 3 == 1])
  Xty <- Reduce("+", v[seq_along(v) %% 3 == 2])
  yty <- Reduce("+", v[seq_along(v) %% 3 == 0])
  res <- list(XtX = XtX, Xty = Xty, yty = yty)
  keyval(1, res) 
}

# summary : beta.hat과 MSE등 분석 결과 summary
fun.summary <- function(v) {
  XtX = v$XtX
  Xty = v$Xty
  yty = v$yty
  beta.hat = solve(XtX, Xty) nn = XtX[1,1]
  ysum = Xty[1]
  ybar = ysum/nn
  stat <- list(nn = nn, beta.hat = beta.hat, ysum = ysum, ybar = ybar)
  SSE = yty - crossprod(beta.hat, Xty) SST = yty - ysum^2/nn
  SSR = SST - SSE
  SS <- list(SSR = SSR, SSE = SSE, SST =SST)

  df.reg = dim(XtX)[1L] - 1
  df.tot = nn - 1
  df.res = df.tot - df.reg
  DF <- list(df.reg = df.reg, df.res = df.res,
  df.tot = df.tot)
  MSR = SSR / df.reg
  MST = SST / df.tot
  MSE = SSE / df.res
  MS <- list(MSR = MSR, MSE = MSE, MST= MST)

  f.val = MS$MSR / MS$MSE
  p.val = pf(f.val, DF$df.reg, DF$df.res,
  lower.tail = F)
  anova <- list(DF = DF, SS = SS, MS = MS, f.val = f.val, p.val = p.val)
  res <- list(mat = v, stat = stat, anova = anova) 
}

# K-means MapReduce
dist.fun <- function(Center, Dat) apply(Center, 1, function(x) colSums((t(Dat) - x)^2))

num.clusters = 3
C = NULL

kmeans.map <- function(., P) { 
  nearest <- if(is.null(C)){
    sample(1:num.clusters, nrow(P), replace = TRUE)
  }
  else {
    D <- dist.fun(C, P)
    nearest <- max.col(-D) 
  }
  keyval(nearest, cbind(nearest,P))
}