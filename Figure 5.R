library(csvread)
library(princurve)
DATA<- read.csv("C://Users//pinaz//OneDrive//DATA//CSV//TRV_DATA.csv")
source("D://CODES//SITA_ZATA_TRV_codes//blindspots_removing_codes.R")

# Preparing data set for the experiment

#column names in the data set

SS_pt <- paste("SS_pt", 1:77, sep = "_")
ZS_pt <- paste("ZS_pt", 1:77, sep = "_")

# Create an empty data frame with the specified column names
Th_values <- data.frame(matrix(nrow = 54, ncol = length(SS_pt) + length(ZS_pt)))
colnames(Th_values) <- c(SS_pt, ZS_pt)



for (pt in c(1:77)){
  for (L in c(1:54)){
    
    Th_values[ L, pt]<- sum(c(DATA [pt , paste("SS_th_Day1_L", L,sep = "")],
                              DATA [pt , paste("SS_th_Day2_L", L,sep = "")],
                              DATA [pt , paste("SF_th_Day1_L", L,sep = "")],
                              DATA [pt , paste("SF_th_Day2_L", L,sep = "")]))/4
    
    Th_values[ L, (77+pt)]<- sum(c(DATA [pt , paste("ZS_th_Day1_L", L,sep = "")],
                                   DATA [pt , paste("ZS_th_Day2_L", L,sep = "")],
                                   DATA [pt , paste("ZF_th_Day1_L", L,sep = "")],
                                   DATA [pt , paste("ZF_th_Day2_L", L,sep = "")]))/4
  }
}


SITA_mean_N<- na.omit(as.numeric(unlist(Th_values[c(1:54), c(which(colnames(Th_values) == "SS_pt_1"):
                                                               which(colnames(Th_values) == "SS_pt_22"))])))
ZATA_mean_N<- na.omit(as.numeric(unlist(Th_values[c(1:54), c(which(colnames(Th_values) == "ZS_pt_1"):
                                                               which(colnames(Th_values) == "ZS_pt_22"))])))

SITA_mean_G<- na.omit(as.numeric(unlist(Th_values[c(1:54), c(which(colnames(Th_values) == "SS_pt_23"):
                                                               which(colnames(Th_values) == "SS_pt_77"))])))
ZATA_mean_G<- na.omit(as.numeric(unlist(Th_values[c(1:54), c(which(colnames(Th_values) == "ZS_pt_23"):
                                                               which(colnames(Th_values) == "ZS_pt_77"))])))

SITA_mean<- c(SITA_mean_N, SITA_mean_G)
ZATA_mean<- c(ZATA_mean_N, ZATA_mean_G)
###Jittering zeros####

Zero_Z_G<- ZATA_mean_G[which(ZATA_mean_G == 0)]

Zero_Z_S_G<- SITA_mean_G[which(ZATA_mean_G == 0)]

Jitter_x_Zero_Z_S_G<- c(jitter(Zero_Z_S_G[which(Zero_Z_S_G == 0)])*100, 
                        jitter(Zero_Z_S_G[which(Zero_Z_S_G != 0)]))

Jitter_y_Zero_Z_G<- jitter(Zero_Z_G)*100

Zero_S_G<- SITA_mean_G[SITA_mean_G == 0 & ZATA_mean_G != 0]
Zero_S_Z_G<- ZATA_mean_G[SITA_mean_G == 0 & ZATA_mean_G != 0]

Jitter_x_Zero_S_G<- jitter(Zero_S_G)*100

Jitter_y_Zero_S_Z_G<- c(jitter(Zero_S_Z_G[which(Zero_S_Z_G == 0)])*100,
                        jitter(Zero_S_Z_G[which(Zero_S_Z_G != 0)]))

###plotting data

tiff(paste("D://TVST_Manuscript_figures//Figure_5.tiff", sep = "")
    ,h= 1800,w= 1800, res = 300)
par(mar = c(5, 5, 1, 1), bty = "n")

plot(c(-5:37), c(-5:37),type = "l", lty = 2, lwd = 2, xlim = c(-2, 35),
     ylim = c(-2, 35),xlab = "Sensitivity estimates from SITA (dB)", cex.lab = 1.5,
     ylab = "Sensitivity estimates from ZATA (dB)", xaxt="n",yaxt="n",col = "white")

axis(side = 1, labels = c(0, seq(5, 35, 5)) , at = c(0, seq(5, 35, 5)), cex.axis = 0.8)
axis(side = 2, labels = c(0, seq(5, 35, 5)) , at = c(0, seq(5, 35, 5)), cex.axis = 0.8, las = 1)


points(SITA_mean_G[which(ZATA_mean_G !=0)],ZATA_mean_G[which(ZATA_mean_G != 0)],
       col =  "black", pch = 3, lwd = 2, cex = 1)
points(SITA_mean_G[which(SITA_mean_G <1)],ZATA_mean_G[which(SITA_mean_G <1)],
       col =  "white", pch = 3, lwd = 2, cex = 1)

points(Jitter_x_Zero_Z_S_G, Jitter_y_Zero_Z_G, col= "black", pch = 3, lwd = 2, cex = 1)

points(Jitter_x_Zero_S_G, Jitter_y_Zero_S_Z_G, col= "black", pch = 3, lwd = 2, cex = 1)

points(SITA_mean_N,ZATA_mean_N, col = "#2e8924", pch = 1, lwd = 3, cex = 1.5)

lines(c(-5:37), c(-5:37),type = "l", lty = 2, lwd = 4, col = "black")

legend("topleft", bty = "n", legend = c("Glaucoma", "Healthy"), 
       col = c ("black", "#2e8924"),pch = c(3, 1), cex = 1)



## Bootstrapping

samples<- 500
MSD<- numeric(samples)
resampled_data<- data.frame(matrix(nrow = length(SITA_mean), ncol = 2))
colnames(resampled_data) <- c("SITA_m", "ZATA_m")

for (i in 1:samples) {
  Sample <- sample(length(SITA_mean), replace = TRUE)
  resampled_data[ , which(colnames(resampled_data) == "SITA_m")] <- SITA_mean[Sample]
  resampled_data[ , which(colnames(resampled_data) == "ZATA_m")] <- ZATA_mean[Sample]
  x <- as.matrix(resampled_data)
  fit <- principal_curve(x, smoother = "lowess", f = 0.3)
  # lines(fit,col = "#fee0d2", lwd = 1)
  lines(fit,col = "lightpink", lwd = 1)
  
  # Step 1: Getting the principal curve points
  principal_curve_points <- fit$s
  # Step 2: Calculate the Euclidean distance between each original data point and the
  #corresponding principal curve point
  original_data_points <- x
  #creating an empty dataframe
  nrow_data_points <- nrow(original_data_points)
  
  squared_distances <- numeric(nrow_data_points)
  for (n in 1:nrow_data_points) {
    
    squared_distances[n] <- sum((original_data_points[n, ] - principal_curve_points[n, ])^2)
    
  }
  
  # Step 3: Calculate the mean squared distance
  
  MSD [i] <- mean(squared_distances)
  
}

library(princurve)
x <- SITA_mean
x <- cbind(SITA_mean, ZATA_mean)
fit_all <- principal_curve(x, smoother = "lowess", f = 0.25)
lines(fit_all, col = "#de2d26", lwd = 5)


dev.off()


