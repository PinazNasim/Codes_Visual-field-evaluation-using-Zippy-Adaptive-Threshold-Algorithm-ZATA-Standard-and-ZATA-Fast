# Extracting data
library(csvread)
DATA<- read.csv("C://Users//pinaz//OneDrive//DATA//CSV//TRV_DATA.csv")

# plots


tiff(paste("D://TVST_Manuscript_figures//Figure_8.tiff", sep = "")
    ,h= 2000,w= 2300, res = 300)
par(mfrow = c(2, 2), mar = c(4, 4,2, 0.2), bty = "n")
Test_names<- c("ZS", "ZF", "SS", "SF")
for (name in Test_names){
  test = name
  T1<- DATA[paste(test, "_Day1_md", sep = "")]
  T2<- DATA[paste(test, "_Day2_md", sep = "")]
  Test<- rowMeans(data.frame(T1, T2))
  D1<- DATA[paste(test, "_Duration_day1", sep = "")]/60
  D2<- DATA[paste(test, "_Duration_day2", sep = "")]/60
  Duration<- rowMeans(data.frame(D1, D2))
  
  plot(1, 600, type = "l", xlab = " ", 
       ylab = " " , 
       # main = ifelse(test == "SS", "a) SITA Standard", 
       #                             ifelse(test == "SF", "c) SITA Fast",
       #                                    
       #                                    ifelse(test == "ZS", "b) ZATA Standard"
       #                                           , "d) ZATA Fast"))),
       lty = 1, lwd = 2, xlim = c(-33.8, 3.6), ylim = c(0.5,13.5), 
       # cex.main = 1,
       axes = FALSE)
  axis(1, at = seq(5,-35,-5), labels = seq(5,-35,-5),cex.axis= 1,
       padj = -0.6, tck = -0.03)
  axis(2, at = seq(0,12,2), labels = seq(0,12,2)
       ,cex.axis= 1, tck = -0.03,
       padj = 0.5, hadj = 0.7, las = 1)
  
  mtext(ifelse(test == "SS", "c) SITA Standard", 
               ifelse(test == "SF", "d) SITA Fast",
                      
                      ifelse(test == "ZS", "a) ZATA Standard"
                             , "b) ZATA Fast"))), side = 3, bold = F, line = 0.8)
  mtext(ifelse(test == "ZS", " Test duration (minutes)                                                                ", " "),
        side = 2, cex = 1.2, line =2.5, bold = F)
  
  points(Test[c(23:77)], Duration[c(23:77)], pch = 3, lwd =2,
         col = "black")
  points(Test[c(1:22)], Duration[c(1:22)], pch = 1,lwd=3, col = "#2e8924", cex=1.5)
  xx <-  seq(-35, 0, length=77)
  
  fit <- loess(Duration ~ Test, span = 0.5) 
  pred <- predict(fit, xx, se=TRUE)
  lines(xx, pred$fit, lty= 1, col= "red", lwd = 1.8)
  # lines(xx, pred$fit - qt(0.975,pred$df)*pred$se , lty= 2, col= "black", lwd = 1)
  # lines(xx, pred$fit + qt(0.975,pred$df)*pred$se , lty= 2, col= "black", lwd = 1)
  # 
  
  
  # legend("topright", c("95 % CI", "Mean"), col = c("black",1), lty = c(2, 1),
  #        cex = 1, bty = "n", lwd = 1)
  # legend("topright", ("Loess fit"), col = ("black"), lty = (1),
  #        cex = 1, bty = "n", lwd = 1)
  
  Standard<- cor.test(Test, Duration, method=("spearman"))
  text(-25,103, paste("rho =",round(Standard$estimate, digits = 2), sep = " "), cex = 1)
  legend("topleft", bty = "n", legend = c("Glaucoma", "Healthy"), 
         col = c ("black", "#2e8924"),pch = c(3, 1), cex = 1)
  # legend("topleft", c( "Healthy", "Glaucoma"), 
  #        col = c ("#2e8924", "black"),bty = "n" ,
  #        pch = c(16, 17), cex = 1 )
  box()
}
mtext("Mean Deviation (dB)                                                                 ",
      side = 1, line = 2.5, cex = 1.2 , bold = F)
dev.off()

