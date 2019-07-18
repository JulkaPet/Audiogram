#### Behavioural Audiogram ####
#
#
# script to plot data and analysis

rm(list=ls())   #empties workspace

getwd()         #shows path
setwd("E:/BA/backup(2017)_all/")    #sets path


# load libraries
library(ggplot2)
library(RColorBrewer)



# load data
BA_njan <- read.table("all_windows_Njan.txt", header=T) 
# windows as factor
BA_njan$window <- as.factor(BA_njan$window)
# adding intensity classifications to windows 
int <- c(rep('sil',5),rep('20dB',10),rep('25dB',10),rep('30dB',10),rep('35dB',10),rep('40dB',10),rep('45dB',10),rep('50dB',10),rep('55dB',10),rep('60dB',10),rep('65dB',10),rep('70dB',10),rep('75dB',10),rep('80dB',10),rep('85dB',10),rep('90dB',10), rep('NA',5))
# adding new window order (per intenstiy)
windows_perint <- factor(c(c(1:5),rep(c(1:10),15), rep('NA',5)), levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 'NA'))
# adding both information to data
BA_njan <- cbind(BA_njan, int, windows_perint)



### playground


#ggplot(BA_njan, aes(x = window, y=RMS_dB)) + geom_boxplot()

#ggplot(BA_njan, aes(x = window, y=RMS_dB)) + geom_boxplot()

BA_njan_trim20 <- BA_njan[which(BA_njan$stimulus == '1-Sinus-20ms'),]
unique(BA_njan_trim20$light)

#ggplot(BA_njan_trim20, aes(x = window, y=RMS_dB)) + geom_boxplot()

BA_njan_trim_s20_35kHz <- BA_njan_trim20[which(BA_njan_trim20$freq== 35),]
unique(BA_njan_trim_s20_35kHz$ID)

#ggplot(BA_njan_trim_s20_35kHz, aes(x = window, y=RMS_dB, group=ID)) + geom_line()

dim(BA_njan)[1]/160







BA_njan_int <- BA_njan[which(BA_njan$int != 'NA'),]

#BA_njan_int$windows_perint <- as.numeric(BA_njan_int$windows_perint)


marker = list(color = colorRampPalette(brewer.pal(9,"Blues"))(16))

BA_njan_35 <- BA_njan_int[which(BA_njan_int$freq == 5 & BA_njan_int$ID == 'Njan_001'),]
BA_njan_35 <- BA_njan_int[which(BA_njan_int$freq == 5 & BA_njan_int$ID == 'Njan_001' & BA_njan_int$int_s == '70dB'),]
BA_njan_35 <- BA_njan_35[!BA_njan_35$int_s %in% c('sil', 'NA'),]

ggplot(BA_njan_35, aes(as.factor(windows_perint),RMS_dB, group = int_s, colour = int_s)) + geom_line() + scale_colour_manual(values=c(marker$color)) + ylim(-80,-50)

summary(BA_njan_int_ex)


### playground over






### classifying reactions (1/0) per intensity

BA_reaction <- data.frame(ID=character(),
                          stimulus=character(), 
                          freq=numeric(), 
                          light=character(),
                          int=numeric(),
                          reaction=numeric(),
                          stringsAsFactors=FALSE) 


inds <- unique(BA_njan$ID)

#counter
c <- 1

for (id in 1:length(inds)) {
  ID <- inds[id]
  BA_njan_ind <- BA_njan[which(BA_njan$ID == ID),]
  
  stims <- unique(BA_njan_ind$stimulus)
  
  for (st in 1:length(stims)) {
    stimulus <- stims[st]
    BA_njan_ind_stim <- BA_njan_ind[which(BA_njan_ind$stimulus == stimulus),]
    
    freqs <- unique(BA_njan_ind_stim$freq)
    
    for (fr in 1:length(freqs)){
      freq <- freqs[fr]
      BA_njan_ind_stim_fre <- BA_njan_ind_stim[which(BA_njan_ind_stim$freq == freq),]
      
      lights <- unique(BA_njan_ind_stim_fre$light)
      
      for (li in 1:length(lights)) {
        light <- lights[li]
        BA_njan_ind_stim_fre_li <- BA_njan_ind_stim_fre[which(BA_njan_ind_stim_fre$light == light),]
        
        ints <- unique(BA_njan_ind_stim_fre_li$int)
        #deletign sil and NA
        ints <- ints[!ints %in% c('sil', 'NA')]
          
        for (d in 1:length(ints)) {
          int <- ints[d]
          BA_njan_onetrace <- BA_njan_ind_stim_fre_li[which(BA_njan_ind_stim_fre_li$int == int),]
          BA_njan_onetrace$windows_perint <- as.numeric(BA_njan_onetrace$windows_perint)
          FS_pre <- mean(BA_njan_onetrace$RMS_dB[which(BA_njan_onetrace$windows_perint >= 1 & BA_njan_onetrace$windows_perint <= 4)])
          FS_post <- mean(BA_njan_onetrace$RMS_dB[which(BA_njan_onetrace$windows_perint >= 7 & BA_njan_onetrace$windows_perint <= 10)])
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
          
          c <- c+1
          
        }
      }
    }
  }
}




ggplot(BA_reaction, aes(x=int, y=reaction, group=ID)) + geom_line() + facet_grid(stimulus ~ freq)





### New table where there is just the first increase in flight strength set as threshold

BA_threshold <- data.frame(ID=character(),
                          stimulus=character(), 
                          freq=numeric(), 
                          light=character(),
                          threshold=numeric(),
                          stringsAsFactors=FALSE) 



inds <- unique(BA_reaction$ID)

#counter
c <- 1

for (id in 1:length(inds)) {
  ID <- inds[id]
  BA_reaction_ind <- BA_reaction[which(BA_reaction$ID == ID),]
  
  stims <- unique(BA_reaction_ind$stimulus)
  
  for (st in 1:length(stims)) {
    stimulus <- stims[st]
    BA_reaction_ind_stim <- BA_reaction_ind[which(BA_reaction_ind$stimulus == stimulus),]
    
    freqs <- unique(BA_reaction_ind_stim$freq)
    
    for (fr in 1:length(freqs)){
      freq <- freqs[fr]
      BA_reaction_ind_stim_fre <- BA_reaction_ind_stim[which(BA_reaction_ind_stim$freq == freq),]
      
      lights <- unique(BA_reaction_ind_stim_fre$light)
      
      for (li in 1:length(lights)) {
        light <- lights[li]
        BA_reaction_ind_stim_fre_li <- BA_reaction_ind_stim_fre[which(BA_reaction_ind_stim_fre$light == light),]
        
        # find the first one in reaction and define this as threshold
        
        BA_reaction_ind_stim_fre_li$reaction
        thres_index <- which(BA_reaction_ind_stim_fre_li$reaction==1)[1] 
        threshold <- BA_reaction_ind_stim_fre_li$int[thres_index]
        threshold <- as.numeric(substr(threshold, 1, 2))
      
          ### fill in table here
          BA_threshold[c, 1] <- as.character(ID)
          BA_threshold[c, 2] <- as.character(stimulus)
          BA_threshold[c, 3] <- freq
          BA_threshold[c, 4] <- as.character(light)
          BA_threshold[c, 5] <- as.character(threshold)
          
          c <- c+1
          
        }
      }
    }
}


BA_threshold
BA_threshold <- BA_threshold[!BA_threshold$threshold %in% c('NA'),]
BA_threshold <- BA_threshold [complete.cases(BA_threshold ), ]

ggplot (BA_threshold, aes(x=as.factor(freq), y=as.numeric(threshold))) + geom_boxplot() + facet_grid(stimulus ~ light)






