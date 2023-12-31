# Calling the required libraries
library(csvread)
# Opening the data file
DATA<- read.csv("C://Users//pinaz//OneDrive//DATA//CSV//TRV_DATA.csv")
# Mining the required data from the data file
md_D1<- vector("list", 4)
md_D2<- vector("list", 4)
Test_names<- c("ZS", "ZF", "SS", "SF")

for (name in Test_names){
  test = name
  
  md_D1[[test]]<- unlist(DATA[paste(test, "_Day1_md",sep="")])
  md_D2[[test]]<- unlist(DATA[paste(test, "_Day2_md",sep="")])
}

md_D1[["SS"]]<- as.numeric(unlist(md_D1$SS))
md_D1[["ZS"]]<- as.numeric(unlist(md_D1$ZS))
md_D1[["SF"]]<- as.numeric(unlist(md_D1$SF))
md_D1[["ZF"]]<- as.numeric(unlist(md_D1$ZF))
md_D2[["SS"]]<- as.numeric(unlist(md_D2$SS))
md_D2[["ZS"]]<- as.numeric(unlist(md_D2$ZS))
md_D2[["SF"]]<- as.numeric(unlist(md_D2$SF))
md_D2[["ZF"]]<- as.numeric(unlist(md_D2$ZF))

# plotting BlandAltman from the mined data

tiff("D://TVST_Manuscript_figures//Figure_3.tiff",
    w= 2300,h=2000, res = 300)
par(mfrow = c(2, 2), mar = c(4, 4.5,2, 0.2), bty = "n")

for (name in Test_names){
  test = name
  
  
  Diff<- (md_D1[[test]] - md_D2[[test]])
  Mean<- (md_D1[[test]] + md_D2[[test]])/2
  SD<- 1.96*sd(Diff)
  
  Upper_lm <- (mean(Diff))+SD
  Lower_lm <- (mean(Diff))-SD
  
  
  plot(1,1, xlim=c(-33.6, 3.6), ylim = c(-14, 14), 
       
       xlab=" " ,ylab=" ", axes = FALSE, cex.main = 1, type = 'n')
  
  legend("topleft", bty = "n", legend = c("Glaucoma", "Healthy"), 
         col = c ("black", "#2e8924"),pch = c(3, 1), cex = 1)
  
  polygon(c(10, -40, -40, 10), 
          c(Upper_lm, Upper_lm, Lower_lm, Lower_lm),
          col = "grey82", border = "grey82")
  
  
  mtext(ifelse(test == "SS", "c) SITA Standard", ifelse(test == "SF", "d) SITA Fast",
                                                        ifelse(test == "ZS", "a) ZATA Standard", "b) ZATA Fast"))), side = 3, line = 0.8)
  points(Mean[c(23:77)],Diff[c(23:77)],
         panel.last = abline(h = mean(Diff), lty = 1, col = "grey", lwd = 1),
         pch = 3, lwd = 2, col = "black")
  
  points(Mean[c(1:22)], Diff[c(1:22)], pch = 1, col = "#2e8924", cex=1.5, lwd = 3)
  
  axis(1, at =  seq(5, -35, -5), labels =  seq(5, -35, -5),
       cex.axis = 1, padj = 0.8, tck = -0.03, mgp=c(5, .3, 0))
  axis(2, at = seq(15, -15, -5), labels = seq(15, -15, -5),
       cex.axis = 1, hadj = 1, las = 1, tck = -0.03, mgp=c(5, 1, 0))
  mtext(text = ifelse(test == "ZS", " MD visit 1 - MD visit 2  (dB)                                                                  ",
                      " "), line = 3, side = 2, cex = 1.2)
  
  mtext(text= ifelse(test == "SF", "Mean MD of visit 1 & 2 (dB)                                                                 "," "),
        line = 2.5, cex = 1.2, side = 1)
  
  box(col = "black", lwd = 1)
  
}

dev.off()

