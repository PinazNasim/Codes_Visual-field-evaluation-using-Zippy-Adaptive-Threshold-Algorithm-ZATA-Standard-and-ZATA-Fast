# Calling data
library(csvread)
DATA<- read.csv("C://Users//pinaz//OneDrive//DATA//CSV//TRV_DATA.csv")

SS<- (DATA$SS_Day1_md + DATA$SS_Day2_md)/2
ZS<- (DATA$ZS_Day1_md + DATA$ZS_Day2_md)/2
SF<- (DATA$SF_Day1_md + DATA$SF_Day2_md)/2
ZF<- (DATA$ZF_Day1_md + DATA$ZF_Day2_md)/2

Mean_SS_ZS<- (SS+ZS)/2
Diff_SS_ZS<- (ZS-SS)
Mean_SF_ZF<- (SF+ZF)/2
Diff_SF_ZF<- (ZF-SF)
SD_SS_ZS<- 1.96*sd(Diff_SS_ZS)
SD_SF_ZF<- 1.96*sd(Diff_SF_ZF)
CI_std<- SD_SS_ZS / sqrt(77)
CI_fast<- SD_SF_ZF / sqrt(77)
CI_Upper_lm_SS_ZS <- (median(Diff_SS_ZS))+CI_std
CI_Lower_lm_SS_ZS <- (median(Diff_SS_ZS))-CI_std
CI_Upper_lm_SF_ZF <- (median(Diff_SF_ZF))+CI_fast
CI_Lower_lm_SF_ZF <- (median(Diff_SF_ZF))-CI_fast

Upper_lm_SS_ZS <- (mean(Diff_SS_ZS))+SD_SS_ZS
Lower_lm_SS_ZS <- (mean(Diff_SS_ZS))-SD_SS_ZS
Upper_lm_SF_ZF <- (mean(Diff_SF_ZF))+SD_SF_ZF
Lower_lm_SF_ZF <- (mean(Diff_SF_ZF))-SD_SF_ZF

tiff(paste("D://TVST_Manuscript_figures//Figure_2.tiff", sep = "")
    ,h= 1500,w= 3200, units = "px", pointsize = 12, res = 300)
par(mfrow = c(1, 2), mar = c(4, 4.5,2, 0.2), bty = "n")

plot(1,1, xlim=c(-33.6, 3.6), ylim = c(-14, 14), cex.lab = 1.2,
     xlab=" Mean MD, ZATA Standard and SITA Standard (dB)",
     ylab="MD, ZATA Standard - SITA Standard (dB)", axes = FALSE,type = 'n')
mtext ("a) ZATA Standard and SITA Standard", side = 3, line = 0.5, cex = 1.2)

legend("topleft", bty = "n", legend = c("Glaucoma", "Healthy"), 
       col = c ("black", "#2e8924"),pch = c(3, 1), cex = 1)
polygon(c(10, -40, -40, 10), 
        c(Upper_lm_SS_ZS, Upper_lm_SS_ZS, Lower_lm_SS_ZS, Lower_lm_SS_ZS),
        col = "grey82", border = "grey82")
abline(h = mean(Diff_SS_ZS), lty = 1, col = "grey", lwd = 1)
axis(1, at =  seq(5, -35, -5), labels =  seq(5, -35, -5),
     cex.axis = 1, padj = 0.8, tck = -0.03, mgp=c(5, .3, 0))
axis(2, at = seq(15, -15, -5), labels = seq(15, -15, -5),
     cex.axis = 1, hadj = 1, las = 1, tck = -0.03, mgp=c(5, 1, 0))
box(col = "black", lwd = 1)
points(Mean_SS_ZS [c(23:77)], Diff_SS_ZS [c(23:77)], pch = 3, lwd = 2, col = "black")

points(Mean_SS_ZS [c(1:22)], Diff_SS_ZS [c(1:22)], pch = 1, col = "#2e8924", 
       lwd = 3, cex=1.5)



plot(1,1, xlim=c(-33.6, 3.6), ylim = c(-14, 14), cex.lab = 1.2,
     xlab=" Mean MD, ZATA Fast and SITA Fast (dB)",
     ylab="MD, ZATA Fast - SITA Fast (dB) ", axes = FALSE, type = 'n')
mtext ("b) ZATA Fast and SITA Fast", side = 3, line = 0.5, cex = 1.2)


legend("topleft", bty = "n", legend = c("Glaucoma", "Healthy"), 
       col = c ("black", "#2e8924"),pch = c(3, 1), cex = 1)
polygon(c(10, -40, -40, 10), 
        c(Upper_lm_SF_ZF, Upper_lm_SF_ZF, Lower_lm_SF_ZF, Lower_lm_SF_ZF),
        col = "grey82", border = "grey82")
abline(h = mean(Diff_SF_ZF), lty = 1, col = "grey", lwd = 1)
points(Mean_SF_ZF [c(23:77)], Diff_SF_ZF [c(23:77)], pch = 3, lwd = 2, col = "black")

points(Mean_SF_ZF [c(1:22)], Diff_SF_ZF [c(1:22)], pch = 1, lwd = 3, 
       col = "#2e8924", cex=1.5)

axis(1, at =  seq(5, -35, -5), labels =  seq(5, -35, -5),
     cex.axis = 1, padj = 0.8, tck = -0.03, mgp=c(5, .3, 0))
axis(2, at = seq(15, -15, -5), labels = seq(15, -15, -5),
     cex.axis = 1, hadj = 1, las = 1, tck = -0.03, mgp=c(5, 1, 0))
box(col = "black", lwd = 1)
dev.off()
