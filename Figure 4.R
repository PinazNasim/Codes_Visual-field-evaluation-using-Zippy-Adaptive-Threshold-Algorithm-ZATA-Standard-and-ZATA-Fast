# Mining required data
library(csvread)
DATA<- read.csv("C://Users//pinaz//OneDrive//DATA//CSV//TRV_DATA.csv")
source("D://CODES//SITA_ZATA_TRV_codes//blindspots_removing_codes.R")
paste(T, "_th_Day1_L" , c(1:54), sep = "")
SS<- as.numeric(unlist(DATA[  ,c(paste("SS_th_Day1_L", c(1:54),sep = ""), 
                                 paste("SS_th_Day2_L", c(1:54),sep = ""))]))
ZS<- as.numeric(unlist(DATA[  ,c(paste("ZS_th_Day1_L", c(1:54),sep = ""), 
                                 paste("ZS_th_Day2_L", c(1:54),sep = ""))]))
SF<- as.numeric(unlist(DATA[  ,c(paste("SF_th_Day1_L", c(1:54),sep = ""), 
                                 paste("SF_th_Day2_L", c(1:54),sep = ""))]))
ZF<- as.numeric(unlist(DATA[  ,c(paste("ZF_th_Day1_L", c(1:54),sep = ""), 
                                 paste("ZF_th_Day2_L", c(1:54),sep = ""))]))
Th<- vector("list", 4)
Th[["SS"]]<- SS[which(SS < 36)]
Th[["ZS"]]<- ZS[which(ZS < 36)]
Th[["SF"]]<- SF[which(SF < 36)]
Th[["ZF"]]<- ZF[which(ZF < 36)]


# Data mined

# Plotting data with the mined data
tiff("D://TVST_Manuscript_figures//Figure_4.tiff",
    h = 1800, w = 1800, units = "px",  pointsize = 12, res = 300)
layout(matrix(c(1:2), nrow = 2, ncol = 1), heights = c(1,0.75))
par(mar = c(0, 4, 1, 1))
# First plot Frequency distribution (%)

plot(1, 5, main = " a)", 
     xlim = c(2.75, 36), ylim = c(0, 2120),
     xlab = " ", axes = FALSE,
     ylab= " ", type = "n")
title(ylab= "Frequency Distribution (%)", cex.lab = 1.2, line = 3)
axis(2, at = seq(0, 2100, length.out = 6),line = 0.5,labels = round(seq((0/8316)*100, (2100/8316)*100, length.out = 6), 0),
     las = 1,cex.axis = 1)

abline (v=seq(1.5, 37.5, 5), col="grey88",lwd = 0.5, lty = 1)
abline (h= 2200 , col="grey88",lwd = 0.5, lty = 1)

Test<- c("ZS", "ZF", "SS","SF")
for (t in Test){
  test = t
  h<- hist(Th[[test]],plot = FALSE, breaks = 36)
  
  points(h$mids[2:35] + ifelse(test == "ZF", 1.25, 1) ,h$counts[2:35],
         type = "l", lwd = 4, lty = 1,
         # lty = ifelse(test == "SS",1,ifelse(test  == "ZS", 1, ifelse(test == "SF", 3, 3)))
         col = ifelse(test == "SS","#e66101",ifelse(test == "ZS", "#5e3c99",ifelse(test == "SF", "#fdb863", "#b2abd2"))))
  points(h$mids[1] + ifelse(test == "ZF", 1.25, 1.25) ,h$counts[1],
         pch = 16, cex = 1,
         col = ifelse(test == "SS","#e66101",ifelse(test == "ZS", "#5e3c99",ifelse(test == "SF", "#fdb863", "#b2abd2"))))
}
legend(2.5,1825, legend = c("ZATA Standard","ZATA Fast (shifted by 0.25 dB)", "SITA Standard",  "SITA Fast")
       , lty = 1, lwd = 6, yjust = 0.5,
       bty = "n" ,bg = "white", box.col = "white",
       col =  c("#5e3c99","#b2abd2", "#e66101", "#fdb863"),
       cex = 1)

# Second plot Cumulative distribution
par(mar = c(4, 4, 2, 1))
plot(1, 5, main = "b) ", 
     xlim = c(2.75, 36), ylim = c(0.02, 1.08),
     xlab = " ", axes = FALSE,
     ylab= " ", type = "n")

title(ylab= "Cumulative Distribution      ", cex.lab = 1.2,
      line = 3)
title(xlab= "Sensitivity (dB)", cex.lab = 1.2, line = 2.5)

axis(2,las = 1, at=seq(0, 1, 0.2),line = 0.5,
     labels=seq(0, 1, 0.2), col = 1, col.axis = 1,
     cex.axis = 1, hadj = 1)

abline (v=seq(1.5, 37.5, 5), col="grey88", lwd = 0.5, lty = 1)

axis(1, at = seq(1.5, 37.5, 5) , labels = seq(0, 36, 5),
     cex.axis = 1, line = 0, padj = 0)

for (t in Test){
  test<- t
  
  h<- hist(Th[[test]],plot = FALSE, breaks = 36)
  ec <- ecdf(Th[[test]])
  points(h$mids+ifelse(test == "ZF", 1.75, 1.5),ec(h$mids), type = "l", lty = 1,
         # lty = ifelse(test == "SS",1,ifelse(test  == "ZS", 1, ifelse(test == "SF", 3, 3)))
         lwd = 4,
         col = ifelse(test == "SS","#e66101",ifelse(test == "ZS", "#5e3c99",ifelse(test == "SF", "#fdb863", "#b2abd2"))))
}
abline (h= 0.5,col="grey88", lwd = 0.5, lty = 1)

dev.off()

