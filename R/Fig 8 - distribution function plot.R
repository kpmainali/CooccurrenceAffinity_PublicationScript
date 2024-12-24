library(BiasedUrn)
library(reshape)
library(ggplot2)

rm(list = ls())

xvec = seq(-3,3.3,by=0.03)
colrvec = c("black","blue","red")
ltyvec = 1:3
xcoord = c(-4,-3,-2.1,-1.4,-0.7,-0.1,0.5,1.2,1.9)

indmat = cbind(rep(1:3,3), rep(1:3,rep(3,3)))
plot(xvec,rep(0,211), ylim=c(0,1.03), xlab="alpha", ylab="Dist.Fcn(X)",
     main=paste0("Distribution Functions at X = 5,10,...,45 (left to right)",
                 "\n"," versus  alpha,  where mA=50, mB=70, N=150"), type="n")
probmat = probmat2 = array(0, c(211,9))

# save alpha hat at 50 percentile
alpha50perc_cicp95 <- data.frame()

for(i in 1:9) {
  for(j in 1:211) {
  probmat[j,i] = pFNCHypergeo(5*i, 50, 100, 70, exp(xvec[j]))
  probmat2[j,i] = pFNCHypergeo(5*i-1, 50, 100, 70, exp(xvec[j]))
  }
  tmp = ML.Alpha(5*i,c(50,70,150), lev=0.95)
  points(tmp$est, 0.5, pch=18, col="red")
  points(tmp$CI.CP, c(0.975,0.025), pch=18, col="red")
  lines(xvec, probmat[,i], col=colrvec[indmat[i,1]],
        lty = ltyvec[indmat[i,2]])
  lines(xvec, probmat2[,i], col=colrvec[indmat[i,1]],
        lty = ltyvec[indmat[i,2]])
  text(xcoord[i],1.03,5*i)

  out <- data.frame(variable = i, alpha_50perc = tmp$est, cicp_0.025 = tmp$CI.CP[1], cicp_0.975 = tmp$CI.CP[2], x = 5*i)
  alpha50perc_cicp95 <- rbind(alpha50perc_cicp95, out)
}
abline(h=0.975, col="blue", lty=5, lwd=1)
abline(h=0.5, col="blue", lty=5, lwd=1)
abline(h=0.025, col="blue", lty=5, lwd=1)


# the plot above is a basic plot that is easy to create
# the code below gives the plot as seen in the manuscript, minus some annotations

# output from script above written
head(probmat)
colnames(probmat) <- paste0("V", 1:9); head(probmat)
xvec
probmatdf <- data.frame(cbind(xvec, probmat)); head(probmatdf)
probmatmelt <- reshape::melt(probmatdf, id=c("xvec")); head(probmatmelt)
probmatmelt$groupvar <- paste0(probmatmelt$variable, "_right"); probmatmelt

head(probmat2)
colnames(probmat2) <- paste0("V", 1:9); head(probmat2)
probmatdf2 <- data.frame(cbind(xvec, probmat2)); head(probmatdf2)
probmat2melt <- reshape::melt(probmatdf2, id=c("xvec")); head(probmat2melt)
probmat2melt$groupvar <- paste0(probmat2melt$variable, "_left"); probmat2melt

probmatall <- rbind(probmatmelt, probmat2melt); head(probmatall)
table(probmatall$groupvar)


# for each line, find the value of alpha that corresponds to y axis = 0.5
# -----------------------------------------------------------------------
# V9 should be dropped for plotting median interval
lvls <- levels(as.factor(probmatall$groupvar)); lvls
lvls <- lvls[1:16]; lvls

medplotdf <- data.frame()

for(mygroupvar in lvls) {

  rm(temp, tempabove, tempbelow, tempboth, lm, pred.alpha)
  temp <- subset(probmatall, groupvar == mygroupvar)

  # find the closet number above 0.5
  tempabove <- subset(temp, value > 0.5)
  # find the closet number below 0.5
  tempbelow <- subset(temp, value <= 0.5)

  tempboth <-
    rbind(
      tempabove[nrow(tempabove),],
      tempbelow[1,]
    )
  print(tempboth)
  lm <- lm(xvec ~ value, tempboth)
  pred.alpha <- predict(lm, data.frame(value = 0.5)); pred.alpha

  out <- tempboth[1,]
  out$xvec <- pred.alpha
  out$value <- 0.5
  out

  # compile output in a single df
  medplotdf <- rbind(medplotdf, out)

}

medplotdf
medplotdf <- subset(medplotdf, select = c(variable, groupvar, xvec)); medplotdf
for(i in 1:nrow(medplotdf)) {
  medplotdf$groupvar[i] <- unlist(strsplit(medplotdf$groupvar[i], "_"))[2]
}
medplotdf
medplotdf <- cast(medplotdf, variable  ~ groupvar); medplotdf


# alpha at 50 percentile - saved already
alpha50perc_cicp95
alpha50perc_cicp95$variable <- paste0("V", alpha50perc_cicp95$variable); alpha50perc_cicp95
# eliminate top left extreme value of CI for x=5
alpha50perc_cicp95$cicp_0.025[1]
alpha50perc_cicp95$cicp_0.025[1] <- NA


# distribution function plot
# ----------------------------
# Please check the package manuscript for fully annotated version of the following figure
p <- ggplot(probmatall, aes(x=xvec, y=value, group=groupvar)) +
  geom_line(aes(color=variable)) +
  geom_hline(yintercept = 0.975, color="gray40", linetype="dashed") +
  geom_hline(yintercept = 0.025, color="gray40", linetype="dashed") +
  geom_hline(yintercept = 0.5, color="gray40", linetype="dashed") +
  geom_segment(aes(x = left, y = 0.5, xend = right, yend = 0.5, group=variable, color=variable), data = medplotdf, size=3) +
  geom_point(aes(x = alpha_50perc, y = 0.5, group = variable), data = alpha50perc_cicp95) +
  geom_point(aes(x = cicp_0.975, y = 0.025, group = variable, color=variable), data = alpha50perc_cicp95) +
  geom_point(aes(x = cicp_0.975, y = 0.025, group = variable), data = alpha50perc_cicp95, shape=1, fill=NA, color="black", size=3) +
  geom_point(aes(x = cicp_0.025, y = 0.975, group = variable, color=variable), data = alpha50perc_cicp95) +
  geom_point(aes(x = cicp_0.025, y = 0.975, group = variable), data = alpha50perc_cicp95, shape=1, fill=NA, color="black", size=3) +
  geom_text(aes(x = -3.1, y = 0.375, label = paste0("X  = ")), hjust = 0, size=3.5, fontface=2) +
  geom_text(data = alpha50perc_cicp95, aes(x = alpha_50perc+0.225, y = 0.375, group = variable, color=variable, label = x), hjust = 0, size=3.5, fontface=2) +
  theme(legend.position = "none") +
  xlab("Alpha MLE") + ylab("Distribution function of X") + xlim(-4.25,3.5)
print(p)
