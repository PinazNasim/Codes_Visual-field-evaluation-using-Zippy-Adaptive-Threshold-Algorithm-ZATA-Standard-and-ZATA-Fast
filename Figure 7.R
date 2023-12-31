library(csvread)
DATA<- read.csv("C://Users//pinaz//OneDrive//DATA//CSV//TRV_DATA.csv")
source("D://CODES//SITA_ZATA_TRV_codes//blindspots_removing_codes.R")
tiff("D://TVST_Manuscript_figures//Figure_7.tiff",
    family = "Times", h = 2800, w= 2800, res = 300)
layout(matrix(c(5,1,3,5, 2,4), nrow = 3, ncol = 2), 
       widths = rep(1, 2), heights = c(0.15,1,1))
par(mar = c(8, 8, 1, 0.5), bty = "n")
Test<- c("ZS", "ZF", "SS", "SF")
for (t in Test){
  test = t
  D1<- ((which(colnames(DATA) == paste(test, "_th_Day1_L1", sep = "")): which(colnames(DATA) == paste(test, "_th_Day1_L54", sep = ""))))
  D2<- ((which(colnames(DATA) == paste(test, "_th_Day2_L1", sep = "")): which(colnames(DATA) == paste(test, "_th_Day2_L54", sep = ""))))
  CD1<- c(D1, D2)
  CD2<- c(D2, D1)
  
  x <- vector("list",34)
  x_a<- rep(NA, 34)
  x_b<- rep(NA, 34)
  
  for(participant in 1:77){
    for(threshold in 0:33){
      th = threshold
      
      
      location <- which(DATA[participant,CD1]==th)
      x[[th+1]] <- c(x[[th+1]],as.numeric(DATA[participant,CD2[location]]))
      x_a[th+1]<- quantile(x[[th+1]], 0.05)
      x_b[th+1]<- quantile(x[[th+1]], 0.95)
    }
  }
  
  Bin<- vector("list", 18)
  Bin[[1]]<- x[1]
  Bin[[2]]<- x[c(2:6)]
  Bin[[3]]<- x[c(4:8)]
  Bin[[4]]<- x[c(6:11)]
  Bin[[5]]<- x[c(8:13)]
  Bin[[6]]<- x[c(11:16)]
  Bin[[7]]<- x[c(13:18)]
  Bin[[8]]<- x[c(16:21)]
  Bin[[9]]<- x[c(18:24)]
  Bin[[10]]<- x[c(21:26)]
  Bin[[11]]<- x[27]
  Bin[[12]]<- x[28]
  Bin[[13]]<- x[29]
  Bin[[14]]<- x[30]
  Bin[[15]]<- x[31]
  Bin[[16]]<- x[32]
  Bin[[17]]<- x[33]
  Bin[[18]]<- x[34]
  
  Loc<- c(0, 2.5, 5, 7.5, 9.5, 12.5, 14.5, 17.5, 20, 22.5, c(26:33))
  
  a<- rep(NA, length(Loc))
  b<- rep(NA, length(Loc))
  c<- rep(NA, length(Loc))
  d<- rep(NA, length(Loc))
  e<- rep(NA, length(Loc))
  f<- rep(NA, length(Loc))
  for(i in 1:18){
    Bin[[i]]<- unlist(Bin[i])
    a[i]<- quantile(Bin[[i]], 0.05)
    b[i]<- quantile(Bin[[i]], 0.95)
    c[i]<- quantile(Bin[[i]], 0.1)
    d[i]<- quantile(Bin[[i]], 0.9)
    e[i]<- quantile(Bin[[i]], 0.25)
    f[i]<- quantile(Bin[[i]], 0.75)
    
  }
  
  
  # 5 and 95 th percentile to hist
  plot(c(0:35), c(0: 35), type = "n", lty = 2 , lwd = 1, 
       axes = FALSE,xlim = c(0, 34), ylim = c(1.3, 34),
       cex.main = 2, xlab = " ", ylab = " ")
  legend(1,36, ifelse(test == "SS", "c) SITA Standard", ifelse(test == "SF", "d) SITA Fast",
                                                               ifelse(test == "ZS", "a) ZATA Standard", "b) ZATA Fast"))),
         bty = "n", cex = 2)
  
  
  mtext(ifelse(test == "ZS", "Retest sensitivity (dB)                                             "," "),
        side = 2, line = 5.3, cex = 2)
  
  axis(1,at= seq(0, 35, by = 5),labels= seq(0, 35, by = 5), col = "black",
       cex.axis = 1.5, padj = 1, tck = -0.03)
  axis(2,at= seq(0, 35, by = 5),labels= seq(0, 35, by = 5), col = "black", 
       cex.axis = 1.5, hadj = 1.5, las = 1, tck = -0.03)
  
  
  polygon(c(Loc[1],Loc, Loc[18:2]), c(b[1], a, b[18:2]),col = "#efedf5", border = "#efedf5")
  polygon(c(Loc[1],Loc, Loc[18:2]), c(d[1], c, d[18:2]),col = "#bcbddc", border = "#bcbddc")
  polygon(c(Loc[1],Loc, Loc[18:2]), c(f[1], e, f[18:2]),col = "#756bb1", border = "#756bb1")
  points(c(0:35), c(0: 35), type = "l", lty = 5 , lwd = 1, col = "white")
  lines(Loc,  round(sapply(Bin, mean), digits = 2), lty = 1, col = "black",lwd = 1)
  lines(Loc,  round(sapply(Bin, median), digits = 2), lty = 2, col = "black",lwd = 1)
  text(Loc, a+1.5, sapply(Bin, length), cex = 1, col = "black", srt = 90, font = 1)
  box()
}


mtext("Baseline sensitivity (dB)                                             ",
      side = 1 ,line = 6, cex= 2)
par(mar = c(0, 7, 0, 40))
plot(5, 5, type = "n", xlab = " ", ylab = " ", bty = "n", axes = FALSE)
legend("topleft", c("5 & 95 percentile","10 & 90 percentile", "25 & 75 percentile"),
       pch = rep(15, 3) , col = c("#efedf5", "#bcbddc", "#756bb1")
       , bty = "n", cex = 1.5)
legend("topright", legend = c("Mean", "Median"), col = c("black", "black"),
       lty = c(1, 2),lwd = rep(2, 2), bty = "n", cex = 1.5)

dev.off()
