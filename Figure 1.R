library(csvread)
DATA<- read.csv("C://Users//pinaz//OneDrive//DATA//CSV//TRV_DATA.csv")

### fig 1

tiff("D://TVST_Manuscript_figures//Figure_1.tiff",
    h = 1150, w = 1150, res = 300)
par(mar = c(3, 3,0.5 ,0.5),  bty = "n")
plot(DATA$SS_Day1_md [c(23:77)], DATA$SS_Day1_sd [c(23:77)],
     pch = 3, lwd = 2,  
     ylim = c(0.7, 19.3 ), xlim = c(-33.5,4),xlab = " ", ylab = " ",
     axes= FALSE)
points(DATA$SS_Day1_md [c(1:22)], DATA$SS_Day1_sd [c(1:22)],
       pch= 1, cex = 1.5, lwd = 3, col = "#2e8924")
legend("topleft", bty = "n", legend = c("Glaucoma (n= 55)", "Healthy (n=22)"), 
       col = c ("black", "#2e8924"),pch = c(3, 1), cex = 1)

title(xlab = "Mean Deviation (dB)",cex.lab = 1,line = 2) 
title(ylab = "Pattern Standard Deviation (dB)", cex.lab = 1,
      line = 2)
axis(1, at = seq(5,-35, by = -5), labels = seq(5,-35, by = -5),cex.axis= 0.8,
     padj = -1)
axis(2,las = 1, at=seq(0,20,5),seq(0,20,5),cex.axis= 0.8,
     padj = 0.5, hadj = 0.5)
box(col = "black")
  

dev.off()

