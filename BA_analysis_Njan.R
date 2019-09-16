#### Behavioural Audiogram ####
#
#
# script to plot data and analysis

rm(list = ls())   #empties workspace

getwd()         #shows path
setwd("F:/BA/backup(2017)_all/")    #sets path


# load libraries
library(ggplot2)
library(RColorBrewer)
library(grid)
library(tidyverse)
library(dplyr)
library(tidyr)


######################################################### data loading and preperation ################################################################
################## LOADING

# load data
BA_njan <- read.table("all_windows_Njan.txt", header = T)


################## ADDING VERIABLES

# windows as factor
BA_njan$window <- as.factor(BA_njan$window)
# adding intensity classifications to windows
int <-
  c(
    rep('sil', 5),
    rep('20dB', 10),
    rep('25dB', 10),
    rep('30dB', 10),
    rep('35dB', 10),
    rep('40dB', 10),
    rep('45dB', 10),
    rep('50dB', 10),
    rep('55dB', 10),
    rep('60dB', 10),
    rep('65dB', 10),
    rep('70dB', 10),
    rep('75dB', 10),
    rep('80dB', 10),
    rep('85dB', 10),
    rep('90dB', 10),
    rep('NA', 5)
  )
# adding new window order (per intenstiy)
windows_perint <-
  factor(c(c(1:5), rep(c(1:10), 15), rep('NA', 5)),
         levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 'NA'))
# adding both information to data
BA_njan <- cbind(BA_njan, int, windows_perint)

# adding "order of freq" to dataset
inds <- unique(BA_njan$ID)

#counter
c <- 1
empty_vec <- rep("", times=length(unique(BA_njan$ID))*4)

for (id in 1:length(inds)) {
  ID <- inds[id]
  BA_njan_ind <- BA_njan[which(BA_njan$ID == ID),]
  
  stims <- unique(BA_njan_ind$stimulus)
  
  for (st in 1:length(stims)) {
    stimulus <- stims[st]
    BA_njan_ind_stim <-
      BA_njan_ind[which(BA_njan_ind$stimulus == stimulus),]
    
    lights <- unique(BA_njan_ind_stim$light)
    
    for (li in 1:length(lights)) {
      light <- lights[li]
      BA_njan_ind_stim_li <-
        BA_njan_ind_stim[which(BA_njan_ind_stim$light == light),]
      
      leng_times <- length(unique(BA_njan_ind_stim_li$time))
      empty_vec[c] <- leng_times
      
      times <- unique(BA_njan_ind_stim_li$time)
      order_nb <- rank(times )
      
      for (ti in 1:length(times)) {
        index_rn <- row.names(BA_njan_ind_stim_li[which(BA_njan_ind_stim_li$time == times[ti]),])
        BA_njan$order[as.numeric(index_rn)] <- order_nb[ti]
      }
      
        c <- c + 1
        
      }
    }
}

### time date from file names is actually useless --> did not record the actual times :/ ---> so instead I added the "file info" in the matlab script 
# which gave reliable values


################## SUBSETTING

###  filtering out individuals that were not active at all (all values for all intensities below -80dB) 

non_act_vec <- c()
act_vec <- c()

for (cc in unique(BA_njan$ID)) {
  BA_njan_ID <-
    BA_njan[which(BA_njan$ID == cc), ]
  if (all(BA_njan_ID$RMSdB < -80)) {
    non_act_vec <- c(non_act_vec, cc)
  } else {
    act_vec <- c(act_vec, cc)
  }
}

BA_njan_act <- BA_njan[BA_njan$ID %in% act_vec, ]
BA_njan_non <- BA_njan[BA_njan$ID %in% non_act_vec, ]


## subsetting data in Supserstimulus (SS) all, active, non-active
## subsetting data further into light on/off
## subsetting data further into different stimulus types
## subsetting data further into FIRST, 35kHz and FIRST&35Khz

#load IDs
SS_all <- read.table("IDs_Njan_allSS.csv", header = T)
SS_act <- read.table("IDs_Njan_actSS.csv", header = T)
SS_non <- read.table("IDs_Njan_nonSS.csv", header = T)

BA_njan_act_SSall <-BA_njan_act[BA_njan_act$ID %in% SS_all$ID, ] 

BA_njan_act_SSall_lightoff <- BA_njan_act_SSall[which(BA_njan_act_SSall$light == 'off'),]
length(unique(BA_njan_act_SSall_lightoff$ID))
#summary(BA_njan_act_SSall_lightoff$stimulus)/(160*18)
#summary(BA_njan_act_SSall_lightoff)

BA_njan_act_SSall_lightoff_10x4  <- BA_njan_act_SSall_lightoff[which(BA_njan_act_SSall_lightoff$stimulus == '2-Sinus-10x4ms'),]
length(unique(BA_njan_act_SSall_lightoff_10x4$ID))

BA_njan_act_SSall_lightoff_10x4_35Khz <- BA_njan_act_SSall_lightoff_10x4[which(BA_njan_act_SSall_lightoff_10x4$freq == '35'),]
BA_njan_act_SSall_lightoff_10x4_FIRST <- BA_njan_act_SSall_lightoff_10x4[which(BA_njan_act_SSall_lightoff_10x4$order == 1),]
BA_njan_act_SSall_lightoff_10x4_35_FIRST <- BA_njan_act_SSall_lightoff_10x4[which(BA_njan_act_SSall_lightoff_10x4$order == 1 & BA_njan_act_SSall_lightoff_10x4$freq == '35'),]

BA_njan_act_SSact <-BA_njan_act[BA_njan_act$ID %in% SS_act$ID, ] 
length(unique(BA_njan_act_SSact$ID))

BA_njan_act_SSnon <-BA_njan_act[BA_njan_act$ID %in% SS_non$ID, ] 
length(unique(BA_njan_act_SSnon$ID))





####################################################################################################################################################
# temp


## selcet for the overlapping inds of 35Khz and FIRST
BA_njan_act_SSall_lightoff_10x4_35Khz <- BA_njan_act_SSall_lightoff_10x4_35Khz[which(BA_njan_act_SSall_lightoff_10x4_35Khz$int != 'sil' & BA_njan_act_SSall_lightoff_10x4_35Khz$int != 'NA'),]
BA_njan_act_SSall_lightoff_10x4_FIRST <- BA_njan_act_SSall_lightoff_10x4_FIRST[which(BA_njan_act_SSall_lightoff_10x4_FIRST$int != 'sil' & BA_njan_act_SSall_lightoff_10x4_FIRST$int != 'NA'),]

BA_njan_act_SSall_lightoff_10x4_35Khz <- BA_njan_act_SSall_lightoff_10x4_35Khz[BA_njan_act_SSall_lightoff_10x4_35Khz$ID %in% BA_njan_act_SSall_lightoff_10x4_FIRST$ID, ]
BA_njan_act_SSall_lightoff_10x4_FIRST <- BA_njan_act_SSall_lightoff_10x4_FIRST[BA_njan_act_SSall_lightoff_10x4_FIRST$ID %in% BA_njan_act_SSall_lightoff_10x4_35Khz$ID, ]


## 35 Khz


ggplot(BA_njan_act_SSall_lightoff_10x4_35Khz, aes(x=as.factor(windows_perint), y=RMSdB, group = interaction(ID, int), colour = ID)) + geom_line()

BA_NASSL1035_trans <- BA_njan_act_SSall_lightoff_10x4_35Khz %>% 
  group_by(ID, stimulus, freq, light, int, windows_perint) %>% 
  summarise(RMSdB=mean(RMSdB)) %>%
  spread(windows_perint, RMSdB)

# PCA
pca_a <- princomp(BA_NASSL1035_trans[,-c(1:5)])

# loadings for each PC
act_load<-pca_a$loadings

no <- seq(1,10,1)
c1 <- act_load[1:10]
c2 <-act_load[11:20]
c3 <-act_load[21:30]

loa_dat <-data.frame(cbind(no, c1,c2,c3))

plot_loadings_35 <-ggplot(loa_dat) + 
  geom_line(aes(x=no, y=c1), colour = 'dimgrey', size =2) + geom_point(aes(x=no, y=c1)) +
  geom_line(aes(x=no, y=c2), colour = 'deepskyblue4', size =2) + geom_point(aes(x=no, y=c2)) +
  geom_line(aes(x=no, y=c3), colour = 'deepskyblue1', size =2) + geom_point(aes(x=no, y=c3)) +
  ggtitle ('loadings')
  





#importance of componants
summary(pca_a)

# plot: variance for first 4 PCs
# Proportion
PoV <- pca_a$sdev^2/sum(pca_a$sdev^2)
# Percent
PoV <- PoV *100
x <- seq(1,20,1)
pov_dat <- data.frame(cbind(PoV, x))
pov_dat <- head(pov_dat, 4) 

plot_PoV_35<-ggplot(pov_dat, aes(y=PoV, x=x)) + 
  geom_point() + theme( axis.text=element_text(size=8), axis.title=element_text(size=9)) + 
  labs(x = "Principle Components ", y = "Percentage of variance (%)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#quick biplot PC1 vs PC2
biplot(pca_a)

#adding PC scores to dataset
BA_njan_act_SSall_lightoff_10x4_35Khz <- BA_NASSL1035_trans
BA_njan_act_SSall_lightoff_10x4_35Khz$pc1 <- pca_a$scores[,1]# change sign of PC1 scores for better visualistion 
BA_njan_act_SSall_lightoff_10x4_35Khz$pc2 <- pca_a$scores[,2]
BA_njan_act_SSall_lightoff_10x4_35Khz$pc3 <- pca_a$scores[,3]
BA_njan_act_SSall_lightoff_10x4_35Khz$pc4 <- pca_a$scores[,4]
BA_njan_act_SSall_lightoff_10x4_35Khz$pc5 <- pca_a$scores[,5]

# plot: biplot with colour-code for ID
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_bi<-ggplot(BA_njan_act_SSall_lightoff_10x4_35Khz, aes(x=pc1, y=pc2, colour=ID)) + geom_point(aes(group=ID)) + 
  theme( axis.text=element_text(size=8),axis.title=element_text(size=9)) + 
  labs(x = "PC1 score", y = "PC2 score") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.position = "none")

# plotting pc2 values against ID
freq35_pc1 <- ggplot(BA_njan_act_SSall_lightoff_10x4_35Khz, aes(x= ID, y = pc1, colour = ID)) + geom_jitter() + geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  theme(legend.position="none", axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle ('flight strength') + 
  theme(plot.title = element_text(colour = 'dimgrey'))
freq35_pc2 <- ggplot(BA_njan_act_SSall_lightoff_10x4_35Khz, aes(x= ID, y = pc2, colour = ID)) + geom_jitter() + geom_boxplot(alpha = 0.3,outlier.shape = NA) +
  theme(legend.position="none", axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle ('type of reaction') + 
  theme(plot.title = element_text(colour = 'deepskyblue4'))
freq35_pc3 <- ggplot(BA_njan_act_SSall_lightoff_10x4_35Khz, aes(x= ID, y = pc3, colour = ID)) + geom_jitter() + geom_boxplot(alpha = 0.3,outlier.shape = NA) +
  theme(legend.position="none",axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle ('short term reaction') + 
  theme(plot.title = element_text(colour = 'deepskyblue1'))



## FIRST 


BA_njan_act_SSall_lightoff_10x4_FIRST <- BA_njan_act_SSall_lightoff_10x4_FIRST[which(BA_njan_act_SSall_lightoff_10x4_FIRST$int != 'sil' & BA_njan_act_SSall_lightoff_10x4_FIRST$int != 'NA'),]

ggplot(BA_njan_act_SSall_lightoff_10x4_FIRST, aes(x=as.factor(windows_perint), y=RMSdB, group = interaction(ID, int), colour = ID)) + geom_line()

BA_NASSL10FIRST_trans <- BA_njan_act_SSall_lightoff_10x4_FIRST %>% 
  group_by(ID, stimulus, freq, light, int, windows_perint) %>% 
  summarise(RMSdB=mean(RMSdB)) %>%
  spread(windows_perint, RMSdB)

# PCA
pca_a <- princomp(BA_NASSL10FIRST_trans[,-c(1:5)])

# loadings for each PC
act_load<-pca_a$loadings

no <- seq(1,10,1)
c1 <- act_load[1:10]
c2 <-act_load[11:20]
c3 <-act_load[21:30]

loa_dat <-data.frame(cbind(no, c1,c2,c3))

plot_loadings_FIRST <-ggplot(loa_dat) + 
  geom_line(aes(x=no, y=c1), colour = 'dimgrey', size =2) + geom_point(aes(x=no, y=c1)) +
  geom_line(aes(x=no, y=c2), colour = 'deepskyblue4', size =2) + geom_point(aes(x=no, y=c2)) +
  geom_line(aes(x=no, y=c3), colour = 'deepskyblue1', size =2) + geom_point(aes(x=no, y=c3)) +
  ggtitle ('loadings')


#importance of componants
summary(pca_a)

# plot: variance for first 4 PCs
# Proportion
PoV <- pca_a$sdev^2/sum(pca_a$sdev^2)
# Percent
PoV <- PoV *100
x <- seq(1,20,1)
pov_dat <- data.frame(cbind(PoV, x))
pov_dat <- head(pov_dat, 4) 

plot_PoV_FIRST<-ggplot(pov_dat, aes(y=PoV, x=x)) + 
  geom_point() + theme( axis.text=element_text(size=8), axis.title=element_text(size=9)) + 
  labs(x = "Principle Components ", y = "Percentage of variance (%)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#quick biplot PC1 vs PC2
biplot(pca_a)

#adding PC scores to dataset
BA_njan_act_SSall_lightoff_10x4_FIRST <- BA_NASSL10FIRST_trans
BA_njan_act_SSall_lightoff_10x4_FIRST$pc1 <- pca_a$scores[,1]# change sign of PC1 scores for better visualistion 
BA_njan_act_SSall_lightoff_10x4_FIRST$pc2 <- pca_a$scores[,2]
BA_njan_act_SSall_lightoff_10x4_FIRST$pc3 <- pca_a$scores[,3]
BA_njan_act_SSall_lightoff_10x4_FIRST$pc4 <- pca_a$scores[,4]
BA_njan_act_SSall_lightoff_10x4_FIRST$pc5 <- pca_a$scores[,5]

# plot: biplot with colour-code for ID
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_bi<-ggplot(BA_njan_act_SSall_lightoff_10x4_FIRST, aes(x=pc1, y=pc2, colour=ID)) + geom_point(aes(group=ID)) + 
  theme( axis.text=element_text(size=8),axis.title=element_text(size=9)) + 
  labs(x = "PC1 score", y = "PC2 score") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.position = "none")

# plotting pc2 values against ID
FIRST_pc1 <- ggplot(BA_njan_act_SSall_lightoff_10x4_FIRST, aes(x= ID, y = pc1, colour = ID)) + geom_jitter() + geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  theme(legend.position="none", axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle ('flight strength') + 
  theme(plot.title = element_text(colour = 'dimgrey'))
FIRST_pc2 <- ggplot(BA_njan_act_SSall_lightoff_10x4_FIRST, aes(x= ID, y = pc2, colour = ID)) + geom_jitter() + geom_boxplot(alpha = 0.3,outlier.shape = NA) +
  theme(legend.position="none", axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle ('type of reaction') + 
  theme(plot.title = element_text(colour = 'deepskyblue4'))
FIRST_pc3 <- ggplot(BA_njan_act_SSall_lightoff_10x4_FIRST, aes(x= ID, y = pc3, colour = ID)) + geom_jitter() + geom_boxplot(alpha = 0.3,outlier.shape = NA) +
  theme(legend.position="none",axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle ('short term reaction') + 
  theme(plot.title = element_text(colour = 'deepskyblue1'))


## adapt
tiff("BA_PCAforsubsets.tiff",  width = 35,  height = 23,  units = 'cm',  res = 600)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 4)))
print(freq35_pc1, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1))
print(freq35_pc2, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 2))
print(freq35_pc3, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 3))
print(plot_loadings_35, vp = viewport(layout.pos.row = 1, layout.pos.col = 4))
print(plot_PoV_35, vp = viewport(layout.pos.row = 2, layout.pos.col = 4))
print(FIRST_pc1, vp = viewport(layout.pos.row = 3:4, layout.pos.col = 1))
print(FIRST_pc2, vp = viewport(layout.pos.row = 3:4, layout.pos.col = 2))
print(FIRST_pc3, vp = viewport(layout.pos.row = 3:4, layout.pos.col = 3))
print(plot_loadings_FIRST, vp = viewport(layout.pos.row = 3, layout.pos.col = 4))
print(plot_PoV_FIRST, vp = viewport(layout.pos.row = 4, layout.pos.col = 4))
dev.off()



## ALL 10x4 light off


BA_njan_act_SSall_lightoff_10x4 <- BA_njan_act_SSall_lightoff_10x4[which(BA_njan_act_SSall_lightoff_10x4$int != 'sil' & BA_njan_act_SSall_lightoff_10x4$int != 'NA'),]

ggplot(BA_njan_act_SSall_lightoff_10x4, aes(x=windows_perint, y=RMSdB, group = interaction(ID, int), colour = ID)) + geom_line()

BA_10x4ALL_trans <- BA_njan_act_SSall_lightoff_10x4 %>% 
  group_by(ID, stimulus, freq, order, light, int, windows_perint) %>% 
  summarise(RMSdB=mean(RMSdB)) %>%
  spread(windows_perint, RMSdB)

# PCA
pca_a <- princomp(BA_10x4ALL_trans[,-c(1:6)])

# loadings for each PC
act_load<-pca_a$loadings

no <- seq(1,10,1)
c1 <- act_load[1:10]
c2 <-act_load[11:20]
c3 <-act_load[21:30]

loa_dat <-data.frame(cbind(no, c1,c2,c3))

plot_loadings_ALL <-ggplot(loa_dat) + 
  geom_line(aes(x=no, y=c1), colour = 'dimgrey', size =2) + geom_point(aes(x=no, y=c1)) +
  geom_line(aes(x=no, y=c2), colour = 'deepskyblue4', size =2) + geom_point(aes(x=no, y=c2)) +
  geom_line(aes(x=no, y=c3), colour = 'deepskyblue1', size =2) + geom_point(aes(x=no, y=c3)) +
  ggtitle ('loadings')


#importance of componants
summary(pca_a)

# plot: variance for first 4 PCs
# Proportion
PoV <- pca_a$sdev^2/sum(pca_a$sdev^2)
# Percent
PoV <- PoV *100
x <- seq(1,20,1)
pov_dat <- data.frame(cbind(PoV, x))
pov_dat <- head(pov_dat, 4) 

plot_PoV_ALL<-ggplot(pov_dat, aes(y=PoV, x=x)) + 
  geom_point() + theme( axis.text=element_text(size=8), axis.title=element_text(size=9)) + 
  labs(x = "Principle Components ", y = "Percentage of variance (%)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#quick biplot PC1 vs PC2
biplot(pca_a)

#adding PC scores to dataset
BA_njan_act_SSall_lightoff_10x4 <- BA_10x4ALL_trans
BA_njan_act_SSall_lightoff_10x4$pc1 <- pca_a$scores[,1]# change sign of PC1 scores for better visualistion 
BA_njan_act_SSall_lightoff_10x4$pc2 <- pca_a$scores[,2]
BA_njan_act_SSall_lightoff_10x4$pc3 <- pca_a$scores[,3]
BA_njan_act_SSall_lightoff_10x4$pc4 <- pca_a$scores[,4]
BA_njan_act_SSall_lightoff_10x4$pc5 <- pca_a$scores[,5]

# plot: biplot with colour-code for ID
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_bi<-ggplot(BA_njan_act_SSall_lightoff_10x4, aes(x=pc1, y=pc2, colour=ID)) + geom_point(aes(group=ID)) + 
  theme( axis.text=element_text(size=8),axis.title=element_text(size=9)) + 
  labs(x = "PC1 score", y = "PC2 score") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.position = "none")

# plotting pc2 values against ID


ggplot(BA_njan_act_SSall_lightoff_10x4, aes(x= as.factor(freq), y = pc1, colour = ID)) + geom_jitter() + geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  theme(legend.position="none", axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle ('flight strength') + 
  theme(plot.title = element_text(colour = 'dimgrey')) +
  facet_grid(ID ~ .)

ggplot(BA_njan_act_SSall_lightoff_10x4, aes(x= as.factor(freq), y = pc2, colour = ID)) + geom_jitter() + geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  theme(legend.position="none", axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle ('type of reaction') + 
  theme(plot.title = element_text(colour = 'dimgrey')) +
  facet_grid(ID ~ .)

ggplot(BA_njan_act_SSall_lightoff_10x4, aes(x= as.factor(freq), y = pc3, colour = ID)) + geom_jitter() + geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  theme(legend.position="none", axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle ('short term reaction') + 
  theme(plot.title = element_text(colour = 'dimgrey')) +
  facet_grid(ID ~ .)

ggplot(BA_njan_act_SSall_lightoff_10x4, aes(x= as.factor(order), y = pc1, colour = ID)) + geom_jitter() + geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  theme(legend.position="none", axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle ('flight strength') + 
  theme(plot.title = element_text(colour = 'dimgrey')) +
  facet_grid(ID ~ .)

ggplot(BA_njan_act_SSall_lightoff_10x4, aes(x= as.factor(order), y = pc2, colour = ID)) + geom_jitter() + geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  theme(legend.position="none", axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle ('type of reaction') + 
  theme(plot.title = element_text(colour = 'dimgrey')) +
  facet_grid(ID ~ .)

ggplot(BA_njan_act_SSall_lightoff_10x4, aes(x= as.factor(order), y = pc3, colour = ID)) + geom_jitter() + geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  theme(legend.position="none", axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle ('short term reaction') + 
  theme(plot.title = element_text(colour = 'dimgrey')) +
  facet_grid(ID ~ .)






####################################################################################################################################################


### playing around with Machine Learning

# creating dataset

#subset to 35kHz, 10x4, light=off
BA_jan_sub <- BA_njan[which(BA_njan$stimulus == '2-Sinus-10x4ms' & BA_njan$freq == 35 & BA_njan$light == 'off' & BA_njan$int != 'sil'),]
summary(BA_jan_sub)

### vector of individuals that looked like they showed reaction --> subset for train and test dataset
ind_rec <- c('Njan_037', 'Njan_091', 'Njan_032', 'Njan_038', 'Njan_087', 'Njan_088', 'Njan_089', 'Njan_071', 'Njan_074', 'Njan_001', 'Njan_011', 'Njan_012', 'Njan_024', 'Njan_014', 'Njan_025', 'Njan_016', 'Njan_009', 'Njan_046', 'Njan_055', 'Njan_049', 'Njan_052')


BA_jan_sub_tt <- BA_njan[BA_njan$ID %in% ind_rec, ]
summary(BA_jan_sub_tt$ID)

BA_jan_sub_tt <- BA_jan_sub_tt[which(BA_jan_sub_tt$freq == 35 & BA_jan_sub_tt$int != 'sil' & BA_jan_sub_tt$int != 'NA' & BA_jan_sub_tt$light == 'off'),]
summary(BA_jan_sub_tt$int)

BA_jan_sub_tt$typ_res <- 0


# for (c_r in dim(BA_jan_sub_tt)[1]/10){
#   
#  beg_r <- 1 + ((c_r - 1) * 10)  
#   end_r <-  10 + ((c_r - 1) * 10)
#   seq_r <- seq(beg_r, end_r, 1)
#   
#  gg <- ggplot(BA_jan_sub_tt[seq_r,], aes(as.factor(windows_perint), RMSdB, group = int)) + geom_line()
# print(gg)
#     typeOR <- readline("Reaction Y/N?")
#   readline(prompt="Press [enter] to continue")
#   BA_jan_sub_tt$typ_res[seq_r,] <-typeOR
#   
# }
# 

#### normalize test dataset for better comparison
summary(BA_jan_sub_tt$RMSdB)

plot_list_reac = list()

for (c_r in 1:(dim(BA_jan_sub_tt)[1]/10)) {
  beg_r <- 1 + ((c_r - 1) * 10)  
  end_r <-  10 + ((c_r - 1) * 10)
  seq_r <- seq(beg_r, end_r, 1)
  ID_r <- unique(BA_jan_sub_tt$ID[seq_r])
  ID_int <- unique(BA_jan_sub_tt$int[seq_r])
  #min_y <- round(min(BA_jan_sub_tt$RMSdB[seq_r])) +0.5
  dat_onetrace <- BA_jan_sub_tt[seq_r,]
  
  norm_vel <- (dat_onetrace$RMSdB[which(dat_onetrace$windows_perint == 5)] + dat_onetrace$RMSdB[which(dat_onetrace$windows_perint == 6)]) / 2

  dat_onetrace$RMSdB_norm <- dat_onetrace$RMSdB - norm_vel
  
  gg <- ggplot(dat_onetrace, aes(as.factor(windows_perint), RMSdB_norm, group = int)) + geom_line() + 
  geom_text( x = 1, y = -5, label= paste0(ID_r, ':', ID_int), aes(fontface=1), hjust = 0) + coord_cartesian(ylim =  c(-7.5,7.5)) + theme(axis.title.y = element_blank(), axis.title.x = element_blank())
  plot_list_reac[[c_r]] = gg
}


plot_pp <- (dim(BA_jan_sub_tt)[1]/10)/4

for (ci in 1:4){
tiff(
  paste0('reac', ci, '.tiff') ,
  width = 54,
  height = 80,
  units = 'cm',
  res = 600
)
grid.newpage()
pushViewport(viewport(layout = grid.layout(ceiling(plot_pp / 4), 4)))
c <- 1
k <- 1
for (i in 1: plot_pp)  {
  if (c == ceiling(plot_pp / 4) + 1) {
    k <- k + 1
    c <- 1
  } else if (c == (ceiling(plot_pp / 4) * 2) + 1) {
    k <- k + 1
    c <- 1
  }
  print(plot_list_reac[[((ci - 1) * plot_pp) + i]],
        vp = viewport(layout.pos.row = c, layout.pos.col = k))
  c <- c + 1
}
dev.off()
}


BA_jan_sub_tt <- BA_jan_sub_tt[,1:10]

# library("xlsx")
# 
# write.table(BA_jan_sub_tt, file = "E:/BA/backup(2017)_all/test.txt")
# 
# write.csv(BA_jan_sub_tt, file = "E:/BA/backup(2017)_all/test.csv")
# 
# write.xlsx(BA_jan_sub_tt, "F:/BA/backup(2017)_all/test.xlsx")
# 
# summary(BA_jan_sub_tt)


###machine learning 

set.seed(1234)










### playground


BA_njan_trim20 <-
  BA_njan[which(BA_njan$stimulus == '1-Sinus-20ms'), ]
unique(BA_njan_trim20$light)

#ggplot(BA_njan_trim20, aes(x = window, y=RMSdB)) + geom_boxplot()

BA_njan_trim_s20_35kHz <-
  BA_njan_trim20[which(BA_njan_trim20$freq == 35), ]
unique(BA_njan_trim_s20_35kHz$ID)

#ggplot(BA_njan_trim_s20_35kHz, aes(x = window, y=RMSdB, group=ID)) + geom_line()

dim(BA_njan)[1] / 160




BA_njan_ML <- read.table("Machine_learning_testdataset.txt", header = T)


BA_njan_int <- BA_njan_ML[which(BA_njan_ML$int != 'NA'), ]
BA_njan_int <- BA_njan_int[which(BA_njan_int$int != 'sil'), ]

#BA_njan_int$windows_perint <- as.numeric(BA_njan_int$windows_perint)


marker = list(color = colorRampPalette(brewer.pal(9, "Blues"))(16))


BA_njan_35 <-
  BA_njan_int[which(BA_njan_int$freq == 35 &
                      BA_njan_int$ID == 'Njan_001' & BA_njan_int$int == '70dB'), ]
BA_njan_35 <- BA_njan_35[!BA_njan_35$int %in% c('sil', 'NA'), ]

ggplot(BA_njan_35, aes(
  as.factor(windows_perint),
  RMSdB,
  group = int
)) + geom_line(size = 2) + scale_colour_manual(values = c(marker$color)) + ylim(-85, -50)


summary(BA_njan_int)



#### normalization around stimulus onset



### normalize around stimulus onset


BA_njan_act35_norm <- data.frame(
  species = character(),
  ID = character(),
  stimulus = character(),
  freq = numeric(),
  light = character(),
  window = character(),
  RMS_lin = numeric(),
  RMSdB = numeric(),
  int = numeric(),
  window_perint = numeric(),
  stringsAsFactors = FALSE
)


inds <- unique(BA_njan_act35$ID)

#counter
c <- 1

for (id in 1:length(inds)) {
  ID <- inds[id]
  BA_njan_ind <- BA_njan_act35[which(BA_njan_act35$ID == ID),]
  
  stims <- unique(BA_njan_ind$stimulus)
  
  for (st in 1:length(stims)) {
    stimulus <- stims[st]
    BA_njan_ind_stim <-
      BA_njan_ind[which(BA_njan_ind$stimulus == stimulus),]
    
    lights <- unique(BA_njan_ind_stim$light)
    
    for (li in 1:length(lights)) {
      light <- lights[li]
      BA_njan_ind_stim_li <-
        BA_njan_ind_stim[which(BA_njan_ind_stim$light == light),]
      
      ints <- unique(BA_njan_ind_stim_li$int)
      #deleating sil and NA
      ints <- ints[!ints %in% c('sil', 'NA')]
      
      for (d in 1:length(ints)) {
        int <- ints[d]
        BA_njan_onetrace <-
          BA_njan_ind_stim_li[which(BA_njan_ind_stim_li$int == int),]
        BA_njan_onetrace$windows_perint <-
          as.numeric(BA_njan_onetrace$windows_perint)
        
        norm_vel <-
          (BA_njan_onetrace$RMSdB[which(BA_njan_onetrace$windows_perint == 5)] + BA_njan_onetrace$RMSdB[which(BA_njan_onetrace$windows_perint == 6)]) /
          2
        
        BA_njan_onetrace$RMSdB_norm <-
          BA_njan_onetrace$RMSdB - norm_vel
        
        
        ### fill in table here
        BA_njan_act35_norm <-
          rbind(BA_njan_act35_norm, BA_njan_onetrace)
        
        c <- c + 1
        
      }
    }
  }
}





########################################################################################################
#### calculating differences between 5&6 and 5&7 window --> histogram could show two groups (non-reactors and reactors)

# looping through ID, stimulus, light, int --> then picking windows and calculating differences, plotting differences in histogram
BA_njan_act35_norm


BA_diff <- data.frame(
  ID = character(),
  stimulus = character(),
  freq = numeric(),
  light = character(),
  int = numeric(),
  type_rea = logical(),
  dif56 = numeric(),
  dif57 = numeric(),
  dif67 = numeric(),
  dif68 = numeric(),
  var_bef = numeric(),
  var_aft = numeric(),
  range_bef = numeric(),
  range_aft = numeric(),
  mean_bef = numeric(),
  mean_aft = numeric(),
  reac_cat = logical(),
  stringsAsFactors = FALSE
)


inds <- unique(BA_njan_act35_norm$ID)

#counter
c <- 1

for (id in 1:length(inds)) {
  ID <- inds[id]
  BA_njan_ind <-
    BA_njan_act35_norm[which(BA_njan_act35_norm$ID == ID), ]
  
  stims <- unique(BA_njan_ind$stimulus)
  
  for (st in 1:length(stims)) {
    stimulus <- stims[st]
    BA_njan_ind_stim <-
      BA_njan_ind[which(BA_njan_ind$stimulus == stimulus), ]
    
    lights <- unique(BA_njan_ind_stim$light)
    
    for (li in 1:length(lights)) {
      light <- lights[li]
      BA_njan_ind_stim_fre_li <-
        BA_njan_ind_stim[which(BA_njan_ind_stim$light == light), ]
      
      ints <- unique(BA_njan_ind_stim_fre_li$int)
      #deletign sil and NA
      ints <- ints[!ints %in% c('sil', 'NA')]
      
      for (d in 1:length(ints)) {
        int <- ints[d]
        
        BA_njan_onetrace <-
          BA_njan_ind_stim_fre_li[which(BA_njan_ind_stim_fre_li$int == int), ]
        
        type_rea <- BA_njan_onetrace$typ_res[1]
        
        dif56 <-
          BA_njan_onetrace$RMSdB_norm[which(BA_njan_onetrace$windows_perint == 6)] - BA_njan_onetrace$RMSdB_norm[which(BA_njan_onetrace$windows_perint == 5)]
        
        dif57 <-
          BA_njan_onetrace$RMSdB_norm[which(BA_njan_onetrace$windows_perint == 7)] - BA_njan_onetrace$RMSdB_norm[which(BA_njan_onetrace$windows_perint == 5)]
        
        dif67 <-
          BA_njan_onetrace$RMSdB_norm[which(BA_njan_onetrace$windows_perint == 7)] - BA_njan_onetrace$RMSdB_norm[which(BA_njan_onetrace$windows_perint == 6)]
        
        dif68 <-
          BA_njan_onetrace$RMSdB_norm[which(BA_njan_onetrace$windows_perint == 8)] - BA_njan_onetrace$RMSdB_norm[which(BA_njan_onetrace$windows_perint == 6)]
        
        var_bef <- var(BA_njan_onetrace$RMSdB_norm[which(BA_njan_onetrace$windows_perint >= 1 & BA_njan_onetrace$windows_perint <= 5)])
        
        var_aft <- var(BA_njan_onetrace$RMSdB_norm[which(BA_njan_onetrace$windows_perint >= 6 & BA_njan_onetrace$windows_perint <= 10)])
        
        range_bef <- max(BA_njan_onetrace$RMSdB_norm[which(BA_njan_onetrace$windows_perint >= 1 & BA_njan_onetrace$windows_perint <= 5)])
        - min(BA_njan_onetrace$RMSdB_norm[which(BA_njan_onetrace$windows_perint >= 1 & BA_njan_onetrace$windows_perint <= 5)])
        
        range_aft <- max(BA_njan_onetrace$RMSdB_norm[which(BA_njan_onetrace$windows_perint >= 6 & BA_njan_onetrace$windows_perint <= 10)]) 
        - min(BA_njan_onetrace$RMSdB_norm[which(BA_njan_onetrace$windows_perint >= 6 & BA_njan_onetrace$windows_perint <= 10)])
        
        mean_bef <- mean(BA_njan_onetrace$RMSdB_norm[which(BA_njan_onetrace$windows_perint >= 1 & BA_njan_onetrace$windows_perint <= 5)])
        
        mean_aft <- mean(BA_njan_onetrace$RMSdB_norm[which(BA_njan_onetrace$windows_perint >= 6 & BA_njan_onetrace$windows_perint <= 10)])

        
        if (mean_aft >=1.75 && range_aft >= 1.5 && var_aft >= 1.75 && dif68 >= 2.5 && dif67 >= 2.5 && dif57 >= 1.5) {
          reac_cat <- 1
        } else {
          reac_cat <- 0
        } 
        
        ### fill in table here
        BA_diff[c, 1] <- as.character(ID)
        BA_diff[c, 2] <- as.character(stimulus)
        BA_diff[c, 3] <- 35
        BA_diff[c, 4] <- as.character(light)
        BA_diff[c, 5] <- as.character(int)
        BA_diff[c, 6]<- type_rea
        BA_diff[c, 7] <- dif56
        BA_diff[c, 8] <- dif57
        BA_diff[c, 9] <- dif67
        BA_diff[c, 10] <- dif68
        BA_diff[c, 11] <- var_bef
        BA_diff[c, 12] <- var_aft
        BA_diff[c, 13] <- range_bef
        BA_diff[c, 14] <- range_aft
        BA_diff[c, 15] <- mean_bef
        BA_diff[c, 16] <- mean_aft
        BA_diff[c, 17] <- reac_cat
        
        c <- c + 1
        
      }
    }
  }
}




#### histogram differnces ####

p_R <- ggplot(BA_diff, aes(type_rea)) + geom_histogram() + ggtitle ('# reaction')
p_d56 <- ggplot(BA_diff, aes(dif56, fill = as.factor(type_rea))) + geom_histogram(binwidth = 0.5) + ggtitle ('# dif56') + 
  theme(legend.title=element_blank(),legend.justification=c(1,1), legend.position=c(1,1), legend.text=element_text(size=5), legend.key.size = unit(0.3, "cm"))
p_d57 <- ggplot(BA_diff, aes(dif57, fill = as.factor(type_rea))) + geom_histogram(binwidth = 0.5) + ggtitle ('# dif57') + 
  theme(legend.title=element_blank(),legend.justification=c(1,1), legend.position=c(1,1), legend.text=element_text(size=5), legend.key.size = unit(0.3, "cm"))
p_d67 <- ggplot(BA_diff, aes(dif67, fill = as.factor(type_rea))) + geom_histogram(binwidth = 0.5) + ggtitle ('# dif67') + 
  theme(legend.title=element_blank(),legend.justification=c(1,1), legend.position=c(1,1), legend.text=element_text(size=5), legend.key.size = unit(0.3, "cm"))
p_d68 <- ggplot(BA_diff, aes(dif68, fill = as.factor(type_rea))) + geom_histogram(binwidth = 0.5) + ggtitle ('# dif68') + 
  theme(legend.title=element_blank(),legend.justification=c(1,1), legend.position=c(1,1), legend.text=element_text(size=5), legend.key.size = unit(0.3, "cm"))

p_var_no <- ggplot(BA_diff[which(BA_diff$type_rea == 0),]) + geom_histogram(aes(var_bef), fill = "red", alpha = 0.2,binwidth = 0.5) + 
  geom_histogram(aes(var_aft), fill = "blue", alpha = 0.2,binwidth = 0.5) +xlim (0,10) + ggtitle ('no reaction - variance')

p_var_yes <- ggplot(BA_diff[which(BA_diff$type_rea == 1),]) + geom_histogram(aes(var_bef), fill = "red", alpha = 0.2,binwidth = 0.5) + 
  geom_histogram(aes(var_aft), fill = "blue", alpha = 0.2,binwidth = 0.5) +xlim (0,10) + ggtitle ('reaction - variance')

p_range_no <- ggplot(BA_diff[which(BA_diff$type_rea == 0),]) + geom_histogram(aes(range_bef), fill = "red", alpha = 0.2,binwidth = 0.5) + 
  geom_histogram(aes(range_aft), fill = "blue", alpha = 0.2,binwidth = 0.5) +xlim (-5,10) + ggtitle ('no reaction - range')

p_range_yes <-ggplot(BA_diff[which(BA_diff$type_rea == 1),]) + geom_histogram(aes(range_bef), fill = "red", alpha = 0.2,binwidth = 0.5) + 
  geom_histogram(aes(range_aft), fill = "blue", alpha = 0.2,binwidth = 0.5) +xlim (-5,10) + ggtitle ('reaction - range')

p_mean_no <- ggplot(BA_diff[which(BA_diff$type_rea == 0),]) + geom_histogram(aes(mean_bef), fill = "red", alpha = 0.2,binwidth = 0.5) + 
  geom_histogram(aes(mean_aft), fill = "blue", alpha = 0.2,binwidth = 0.5) + xlim (-10, 10) + ggtitle ('no reaction - mean')

p_mean_yes <-ggplot(BA_diff[which(BA_diff$type_rea == 1),]) + geom_histogram(aes(mean_bef), fill = "red", alpha = 0.2,binwidth = 0.5) + 
  geom_histogram(aes(mean_aft), fill = "blue", alpha = 0.2,binwidth = 0.5) + xlim (-10, 10) + ggtitle ('reaction - mean')


tiff("BA_splitting criteria.tiff",  width = 18,  height = 14,  units = 'cm',  res = 600)
grid.newpage()
pushViewport(viewport(layout = grid.layout(6, 12)))
print(p_d56, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1:3))
print(p_d57, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 4:6))
print(p_d67, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 7:9))
print(p_d68, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 10:12))
print(p_var_no, vp = viewport(layout.pos.row = 3:4, layout.pos.col = 1:4))
print(p_var_yes, vp = viewport(layout.pos.row = 5:6, layout.pos.col = 1:4))
print(p_range_no, vp = viewport(layout.pos.row = 3:4, layout.pos.col = 5:8))
print(p_range_yes, vp = viewport(layout.pos.row = 5:6, layout.pos.col = 5:8))
print(p_mean_no, vp = viewport(layout.pos.row = 3:4, layout.pos.col = 9:12))
print(p_mean_yes, vp = viewport(layout.pos.row = 5:6, layout.pos.col = 9:12))
dev.off()


#### PCA for windows

BA_njan_act35_norm_trans <- BA_njan_act35_norm %>% 
  group_by(ID, stimulus, freq, light, int, windows_perint) %>% 
  summarise(RMSdB=mean(RMSdB)) %>%
  spread(windows_perint, RMSdB)

# PCA
pca_a <- princomp(BA_njan_act35_norm_trans[,-c(1:5)])

# loadings for each PC
act_load<-pca_a$loadings

#importance of componants
summary(pca_a)

# plot: variance for first 4 PCs
# Proportion
PoV <- pca_a$sdev^2/sum(pca_a$sdev^2)
# Percent
PoV <- PoV *100
x <- seq(1,20,1)
pov_dat <- data.frame(cbind(PoV, x))
pov_dat <- head(pov_dat, 4) 

plot_PoV<-ggplot(pov_dat, aes(y=PoV, x=x)) + 
  geom_point() + theme( axis.text=element_text(size=8), axis.title=element_text(size=9)) + 
  labs(x = "Principle Components ", y = "Percentage of variance (%)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#quick biplot PC1 vs PC2
biplot(pca_a)

#adding PC scores to dataset
PCA_dat_act <- BA_njan_act35_norm_trans
PCA_dat_act$pc1 <- pca_a$scores[,1]# change sign of PC1 scores for better visualistion 
PCA_dat_act$pc2 <- pca_a$scores[,2]
PCA_dat_act$pc3 <- pca_a$scores[,3]
PCA_dat_act$pc4 <- pca_a$scores[,4]
PCA_dat_act$pc5 <- pca_a$scores[,5]

# plot: biplot with colour-code for ID
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_bi<-ggplot(PCA_dat_act, aes(x=pc1, y=pc2, colour=ID)) + geom_point(aes(group=ID)) + 
  theme( axis.text=element_text(size=8),axis.title=element_text(size=9)) + 
  labs(x = "PC1 score", y = "PC2 score") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.position = "none")

# plotting pc2 values against ID
ggplot(PCA_dat_act, aes(x= ID, y = pc1, colour = ID)) + geom_jitter()
ggplot(PCA_dat_act, aes(x= ID, y = pc2, colour = ID)) + geom_jitter()

ggplot(PCA_dat_act, aes(x= , y = pc2, colour = ID)) + geom_jitter()



BA_njan_35_1 <-
  BA_njan_int[which(BA_njan_int$freq == 35 &
                      BA_njan_int$ID == 'Njan_089'), ]
plot1 <-
  ggplot(BA_njan_35_1, aes(
    as.factor(windows_perint),
    RMSdB,
    group = int,
    colour = int
  )) + geom_line(size = 1.5) + scale_colour_manual(values = c  (marker$color)) + ylim(-85, -50) + ggtitle('Njan_089') + theme(legend.position =
                                                                                                                                "none")


BA_njan_35_2 <-
  BA_njan_int[which(BA_njan_int$freq == 35 &
                      BA_njan_int$ID == 'Njan_088'), ]
plot2 <-
  ggplot(BA_njan_35_2, aes(
    as.factor(windows_perint),
    RMSdB,
    group = int,
    colour = int
  )) + geom_line(size = 1.5) + scale_colour_manual(values = c  (marker$color)) + ylim(-85, -50) + ggtitle('Njan_088') + theme(legend.position =
                                                                                                                                "none")


BA_njan_35_3 <-
  BA_njan_int[which(BA_njan_int$freq == 35 &
                      BA_njan_int$ID == 'Njan_087'), ]
plot3 <-
  ggplot(BA_njan_35_3, aes(
    as.factor(windows_perint),
    RMSdB,
    group = int,
    colour = int
  )) + geom_line(size = 1.5) + scale_colour_manual(values = c  (marker$color)) + ylim(-85, -50) + ggtitle('Njan_087') + theme(legend.position =
                                                                                                                                "none")


BA_njan_35_4 <-
  BA_njan_int[which(BA_njan_int$freq == 35 &
                      BA_njan_int$ID == 'Njan_039'), ]
plot4 <-
  ggplot(BA_njan_35_4, aes(
    as.factor(windows_perint),
    RMSdB,
    group = int,
    colour = int
  )) + geom_line(size = 1.5) + scale_colour_manual(values = c  (marker$color)) + ylim(-85, -50) + ggtitle('Njan_039') + theme(legend.position =
                                                                                                                                "none")


BA_njan_35_5 <-
  BA_njan_int[which(BA_njan_int$freq == 35 &
                      BA_njan_int$ID == 'Njan_037'), ]
plot5 <-
  ggplot(BA_njan_35_5, aes(
    as.factor(windows_perint),
    RMSdB,
    group = int,
    colour = int
  )) + geom_line(size = 1.5) + scale_colour_manual(values = c  (marker$color)) + ylim(-85, -50) + ggtitle('Njan_037') + theme(legend.position =
                                                                                                                                "none")


BA_njan_35_6 <-
  BA_njan_int[which(BA_njan_int$freq == 35 &
                      BA_njan_int$ID == 'Njan_016'), ]
plot6 <-
  ggplot(BA_njan_35_6, aes(
    as.factor(windows_perint),
    RMSdB,
    group = int,
    colour = int
  )) + geom_line(size = 1.5) + scale_colour_manual(values = c  (marker$color)) + ylim(-85, -50) + ggtitle('Njan_016') + theme(legend.position =
                                                                                                                                "none")



tiff(
  "example_traces_BA.tiff",
  width = 14,
  height = 21,
  units = 'cm',
  res = 600
)
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 2)))
print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(plot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(plot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(plot5, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(plot6, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
dev.off()






BA_njan_int

ggplot(BA_njan_int, aes(
  as.factor(windows_perint),
  RMSdB,
  group = int,
  colour = int
)) + geom_line() + scale_colour_manual(values = c(marker$color)) + ylim(-85, -50) + facet_grid(. ~ ID)

BA_njan_int_1 <- BA_njan_int[1:93755, ]

ggplot(BA_njan_int_1, aes(
  as.factor(windows_perint),
  RMSdB,
  group = int,
  colour = int
)) + geom_line() + scale_colour_manual(values = c(marker$color)) + ylim(-85, -50) + facet_grid(. ~ ID)

BA_njan_int_10 <- BA_njan_int[1:20795, ]

ggplot(BA_njan_int_10, aes(
  as.factor(windows_perint),
  RMSdB,
  group = int,
  colour = int
)) + geom_line() + scale_colour_manual(values = c(marker$color)) + ylim(-85, -50) + facet_grid(. ~ ID)

### playground over






### classifying reactions (1/0) per intensity

BA_reaction <- data.frame(
  ID = character(),
  stimulus = character(),
  freq = numeric(),
  light = character(),
  int = numeric(),
  reaction = numeric(),
  stringsAsFactors = FALSE
)


inds <- unique(BA_njan$ID)

#counter
c <- 1

for (id in 1:length(inds)) {
  ID <- inds[id]
  BA_njan_ind <- BA_njan[which(BA_njan$ID == ID), ]
  
  stims <- unique(BA_njan_ind$stimulus)
  
  for (st in 1:length(stims)) {
    stimulus <- stims[st]
    BA_njan_ind_stim <-
      BA_njan_ind[which(BA_njan_ind$stimulus == stimulus), ]
    
    lights <- unique(BA_njan_ind_stim$light)
    
    for (fr in 1:length(freqs)) {
      freq <- freqs[fr]
      BA_njan_ind_stim_fre <-
        BA_njan_ind_stim[which(BA_njan_ind_stim$freq == freq), ]
      
      lights <- unique(BA_njan_ind_stim_fre$light)
      
      for (li in 1:length(lights)) {
        light <- lights[li]
        BA_njan_ind_stim_fre_li <-
          BA_njan_ind_stim_fre[which(BA_njan_ind_stim_fre$light == light), ]
        
        ints <- unique(BA_njan_ind_stim_fre_li$int)
        #deletign sil and NA
        ints <- ints[!ints %in% c('sil', 'NA')]
        
        for (d in 1:length(ints)) {
          int <- ints[d]
          BA_njan_onetrace <-
            BA_njan_ind_stim_fre_li[which(BA_njan_ind_stim_fre_li$int == int), ]
          BA_njan_onetrace$windows_perint <-
            as.numeric(BA_njan_onetrace$windows_perint)
          FS_pre <-
            mean(BA_njan_onetrace$RMSdB[which(
              BA_njan_onetrace$windows_perint >= 1 &
                BA_njan_onetrace$windows_perint <= 4
            )])
          FS_post <-
            mean(BA_njan_onetrace$RMSdB[which(
              BA_njan_onetrace$windows_perint >= 7 &
                BA_njan_onetrace$windows_perint <= 10
            )])
          if (FS_post - FS_pre > 5) {
            reac_ind <- 1
          } else {
            reac_ind <- 0
          }
          
          ### fill in table here
          BA_reaction[c, 1] <- as.character(ID)
          BA_reaction[c, 2] <- as.character(stimulus)
          BA_reaction[c, 3] <- freq
          BA_reaction[c, 4] <- as.character(light)
          BA_reaction[c, 5] <- as.character(int)
          BA_reaction[c, 6] <- reac_ind
          
          c <- c + 1
          
        }
      }
    }
  }
}




ggplot(BA_reaction, aes(x = int, y = reaction, group = ID)) + geom_line() + facet_grid(stimulus ~ freq)





### New table where there is just the first increase in flight strength set as threshold

BA_threshold <- data.frame(
  ID = character(),
  stimulus = character(),
  freq = numeric(),
  light = character(),
  threshold = numeric(),
  stringsAsFactors = FALSE
)



inds <- unique(BA_reaction$ID)

#counter
c <- 1

for (id in 1:length(inds)) {
  ID <- inds[id]
  BA_reaction_ind <- BA_reaction[which(BA_reaction$ID == ID), ]
  
  stims <- unique(BA_reaction_ind$stimulus)
  
  for (st in 1:length(stims)) {
    stimulus <- stims[st]
    BA_reaction_ind_stim <-
      BA_reaction_ind[which(BA_reaction_ind$stimulus == stimulus), ]
    
    freqs <- unique(BA_reaction_ind_stim$freq)
    
    for (fr in 1:length(freqs)) {
      freq <- freqs[fr]
      BA_reaction_ind_stim_fre <-
        BA_reaction_ind_stim[which(BA_reaction_ind_stim$freq == freq), ]
      
      lights <- unique(BA_reaction_ind_stim_fre$light)
      
      for (li in 1:length(lights)) {
        light <- lights[li]
        BA_reaction_ind_stim_fre_li <-
          BA_reaction_ind_stim_fre[which(BA_reaction_ind_stim_fre$light == light), ]
        
        # find the first one in reaction and define this as threshold
        
        BA_reaction_ind_stim_fre_li$reaction
        thres_index <-
          which(BA_reaction_ind_stim_fre_li$reaction == 1)[1]
        threshold <- BA_reaction_ind_stim_fre_li$int[thres_index]
        threshold <- as.numeric(substr(threshold, 1, 2))
        
        ### fill in table here
        BA_threshold[c, 1] <- as.character(ID)
        BA_threshold[c, 2] <- as.character(stimulus)
        BA_threshold[c, 3] <- freq
        BA_threshold[c, 4] <- as.character(light)
        BA_threshold[c, 5] <- as.character(threshold)
        
        c <- c + 1
        
      }
    }
  }
}


BA_threshold
BA_threshold <- BA_threshold[!BA_threshold$threshold %in% c('NA'), ]
BA_threshold <- BA_threshold [complete.cases(BA_threshold),]

ggplot (BA_threshold, aes(x = as.factor(freq), y = as.numeric(threshold))) +
  geom_jitter(size = 1.5,
              alpha = 0.5,
              width = 0.15) +
  geom_boxplot(alpha = 0.5) +
  stat_summary(
    fun.y = mean,
    colour = "darkred",
    geom = "point",
    shape = 18,
    size = 3,
    show_guide = FALSE
  ) +
  facet_grid(stimulus ~ light)
