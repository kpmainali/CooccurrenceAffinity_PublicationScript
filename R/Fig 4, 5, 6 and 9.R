library(CooccurrenceAffinity)

# Figure 4
# ---------------
AlphInts(35, c(50,70,150), lev = 0.95)
ML.Alpha(35, c(50,70,150), lev = 0.95)


# Figure 5
# -----------------
# Fit the parameters using the loglin function
par.fit <- loglin(
  array(c(35, 15, 35, 65), c(2, 2)),
  list(1:2),
  start = rep(1, 4),
  eps = 0.1,
  iter = 20,
  param = TRUE
)$param

# Perform calculations and compare
c(log(35*65/(15*35)), 4*par.fit$`1.2`[[1]])



# Figure 6
# ---------------
library(cooccur)
data("finches")
myout <- affinity(data = finches, row.or.col = 'row', squarematrix = c("all"))
plotgg(data = myout, variable = "alpha_mle", legendlimit = "datarange")
myout <- affinity(data = finches, row.or.col = 'col', squarematrix = c("all"))
plotgg(data = myout, variable = "alpha_mle", legendlimit = "datarange")



# Figure 9
# --------------------
CovrgPlot(c(50,70,150), lev=0.95)
