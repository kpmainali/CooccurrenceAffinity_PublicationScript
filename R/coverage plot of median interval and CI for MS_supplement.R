# coverage plot showing MLE, median interval and Blaker CI
# --------------------------------------------------------
library(CooccurrenceAffinity)
library(ggplot2)
library(scales)

rm(list = ls())

CIs = array(0, c(49,7), dimnames=list(NULL,c("MedLo","MedHi","MLE","CI.BlakerLo","CI.BlakerHi", "CI.CPLo","CI.CPHi")))

x <- 5
for(x in 1:49) {
  tmp = AlphInts(x,c(50,70,150), lev=0.95)
  CIs[x,] = c(tmp$MedianIntrvl, ML.Alpha(x,c(50,70,150))$est, tmp$CI.Blaker, tmp$CI.CP)
}

CIs

CIdf <- data.frame(cbind(x=1:49, CIs))
summary(CIdf$CI.CPLo)
summary(CIdf$CI.CPHi)


colors <- c("MLE" = "black", "Median interval" = "#F8766D", "Blaker CI" = "#7CAE00")

# figure of coverage of median interval and Blaker CI
# ---------------------------------------------------
  p <- ggplot(CIdf, aes(x=x, y=MLE)) +
    geom_errorbar(aes(ymin=CI.BlakerLo, ymax=CI.BlakerHi, colour="Blaker CI"), width=0, lwd=1) +
    geom_errorbar(aes(ymin=MedLo, ymax=MedHi, colour="Median interval"), width=0, lwd=2) +
    geom_point(aes(colour="MLE"), size=0.5) +
    labs(x = "Co-occurrence", y = "Alpha", colour = "Legend") +
    scale_color_manual(name="Legend",
                       breaks = c("MLE", "Median interval", "Blaker CI"),
                       values = c("MLE" = "black", "Median interval" = "#F8766D", "Blaker CI" = "#7CAE00")) +
    theme(legend.position = "top") + guides(colour = guide_legend(nrow = 1))

  # add a decimal on y axis to match the margin width to the following figure of ratio
  scaleFUN <- function(x) sprintf("%.1f", x)
  p + scale_y_continuous(labels=scaleFUN)

  print(p)



head(CIdf)
CIdf$BlakerWidth <- CIdf$CI.BlakerHi - CIdf$CI.BlakerLo
CIdf$CPWidth <- CIdf$CI.CPHi - CIdf$CI.CPLo
CIdf$Blaker2CPratio <- CIdf$BlakerWidth/CIdf$CPWidth
head(CIdf)

# figure of ratio of Blaker CI to CP CI
# -------------------------------------
  p <- ggplot(CIdf, aes(x=x, y=Blaker2CPratio)) +
    geom_point() +
    # geom_line() +
    # geom_smooth(se = FALSE) +
    labs(x = "Co-occurrence", y = "Blaker CI width : CP CI width") +
    ylim(c(0.85,1.00))
  print(p)
