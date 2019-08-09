#### Behavioural Audiogram ####
#
#
# script to plot data and analysis

rm(list = ls())   #empties workspace

getwd()         #shows path
setwd("E:/BA/backup(2017)_all/")    #sets path


# load libraries
library(ggplot2)
library(RColorBrewer)
library(grid)



# load data
BA_njan <- read.table("all_windows_Njan.txt", header = T)
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



### playground


#ggplot(BA_njan, aes(x = window, y=RMS_dB)) + geom_boxplot()

#ggplot(BA_njan, aes(x = window, y=RMS_dB)) + geom_boxplot()

BA_njan_trim20 <-
  BA_njan[which(BA_njan$stimulus == '1-Sinus-20ms'), ]
unique(BA_njan_trim20$light)

#ggplot(BA_njan_trim20, aes(x = window, y=RMS_dB)) + geom_boxplot()

BA_njan_trim_s20_35kHz <-
  BA_njan_trim20[which(BA_njan_trim20$freq == 35), ]
unique(BA_njan_trim_s20_35kHz$ID)

#ggplot(BA_njan_trim_s20_35kHz, aes(x = window, y=RMS_dB, group=ID)) + geom_line()

dim(BA_njan)[1] / 160







BA_njan_int <- BA_njan[which(BA_njan$int != 'NA'), ]
BA_njan_int <- BA_njan_int[which(BA_njan_int$int != 'sil'), ]

#BA_njan_int$windows_perint <- as.numeric(BA_njan_int$windows_perint)


marker = list(color = colorRampPalette(brewer.pal(9, "Blues"))(16))

BA_njan_35 <-
  BA_njan_int[which(BA_njan_int$freq == 35 &
                      BA_njan_int$ID == 'Njan_077'), ]
BA_njan_35 <-
  BA_njan_int[which(BA_njan_int$freq == 5 &
                      BA_njan_int$ID == 'Njan_001' & BA_njan_int$int == '70dB'), ]
BA_njan_35 <- BA_njan_35[!BA_njan_35$int %in% c('sil', 'NA'), ]

ggplot(BA_njan_35, aes(
  as.factor(windows_perint),
  RMS_dB,
  group = int,
  colour = int
)) + geom_line(size = 2) + scale_colour_manual(values = c(marker$color)) + ylim(-85, -50)



########################################################################################################
#### plotting all individuals seperated split by type of stimuli (without any filter)

### 10m5off

BA_njan_10m5off <-
  BA_njan_int[which(
    BA_njan_int$freq == 35 &
      BA_njan_int$stimulus == '2-Sinus-10x4ms' &
      BA_njan_int$light == 'off'
  ), ]
unique(BA_njan_10m5off$ID)

plot_list_10m5off = list()
for (c_i in unique(BA_njan_10m5off$ID)) {
  BA_njan_35 <-
    BA_njan_10m5off[which(BA_njan_10m5off$freq == 35 &
                            BA_njan_10m5off$ID == c_i), ]
  plot <-
    ggplot(BA_njan_35, aes(
      as.factor(windows_perint),
      RMS_dB,
      group = int,
      colour = int
    )) + geom_line(size = 1.5) + scale_colour_manual(values = c  (marker$color)) + ylim(-85, -45) + ggtitle(c_i) + theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )
  plot_list_10m5off[[c_i]] = plot
}



plot_list_10m5off = list()
for (c_i in unique(BA_njan_10m5off$ID)) {
  BA_njan_35 <-
    BA_njan_10m5off[which(BA_njan_10m5off$freq == 35 &
                            BA_njan_10m5off$ID == c_i), ]
  plot <-
    ggplot(BA_njan_35, aes(
      as.factor(windows_perint),
      RMS_dB,
      group = int,
      colour = int
    )) + geom_line(size = 1.5) + scale_colour_manual(values = c  (marker$color)) + ylim(-85, -45) + ggtitle(c_i) + theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )
  plot_list_10m5off[[c_i]] = plot
}



tiff(
  "10x5off.tiff",
  width = 28,
  height = 40,
  units = 'cm',
  res = 600
)
grid.newpage()
pushViewport(viewport(layout = grid.layout(ceiling(length(
  unique(BA_njan_10m5off$ID)
) / 3), 3)))
c <- 1
k <- 1
for (i in unique(BA_njan_10m5off$ID))  {
  if (c == ceiling(length(unique(BA_njan_10m5off$ID)) / 3) + 1) {
    k <- k + 1
    c <- 1
  } else if (c == (ceiling(length(unique(
    BA_njan_10m5off$ID
  )) / 3) * 2) + 1) {
    k <- k + 1
    c <- 1
  }
  print(plot_list_10m5off[[i]],
        vp = viewport(layout.pos.row = c, layout.pos.col = k))
  c <- c + 1
}
dev.off()


### 10m5on

BA_njan_10m5on <-
  BA_njan_int[which(
    BA_njan_int$freq == 35 &
      BA_njan_int$stimulus == '2-Sinus-10x4ms' &
      BA_njan_int$light == 'on'
  ), ]
unique(BA_njan_10m5on$ID)

plot_list_10m5on = list()
for (c_i in unique(BA_njan_10m5on$ID)) {
  BA_njan_35 <-
    BA_njan_10m5on[which(BA_njan_10m5on$freq == 35 &
                           BA_njan_10m5on$ID == c_i), ]
  plot <-
    ggplot(BA_njan_35, aes(
      as.factor(windows_perint),
      RMS_dB,
      group = int,
      colour = int
    )) + geom_line(size = 1.5) + scale_colour_manual(values = c  (marker$color)) + ylim(-85, -45) + ggtitle(c_i) + theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )
  plot_list_10m5on[[c_i]] = plot
}


tiff(
  "10x5on.tiff",
  width = 28,
  height = 40,
  units = 'cm',
  res = 600
)
grid.newpage()
pushViewport(viewport(layout = grid.layout(ceiling(length(
  unique(BA_njan_10m5on$ID)
) / 3), 3)))
c <- 1
k <- 1
for (i in unique(BA_njan_10m5on$ID))  {
  if (c == ceiling(length(unique(BA_njan_10m5on$ID)) / 3) + 1) {
    k <- k + 1
    c <- 1
  } else if (c == (ceiling(length(unique(
    BA_njan_10m5on$ID
  )) / 3) * 2) + 1) {
    k <- k + 1
    c <- 1
  }
  print(plot_list_10m5on[[i]], vp = viewport(layout.pos.row = c, layout.pos.col = k))
  c <- c + 1
}
dev.off()


### 5m5off

BA_njan_5m5off <-
  BA_njan_int[which(
    BA_njan_int$freq == 35 &
      BA_njan_int$stimulus == '2-Sinus-5x4ms' &
      BA_njan_int$light == 'off'
  ), ]
unique(BA_njan_5m5off$ID)

plot_list_5m5off = list()
for (c_i in unique(BA_njan_5m5off$ID)) {
  BA_njan_35 <-
    BA_njan_5m5off[which(BA_njan_5m5off$freq == 35 &
                           BA_njan_5m5off$ID == c_i), ]
  plot <-
    ggplot(BA_njan_35, aes(
      as.factor(windows_perint),
      RMS_dB,
      group = int,
      colour = int
    )) + geom_line(size = 1.5) + scale_colour_manual(values = c  (marker$color)) + ylim(-85, -45) + ggtitle(c_i) + theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )
  plot_list_5m5off[[c_i]] = plot
}


tiff(
  "5m5off.tiff",
  width = 28,
  height = 40,
  units = 'cm',
  res = 600
)
grid.newpage()
pushViewport(viewport(layout = grid.layout(ceiling(length(
  unique(BA_njan_5m5off$ID)
) / 3), 3)))
c <- 1
k <- 1
for (i in unique(BA_njan_5m5off$ID))  {
  if (c == ceiling(length(unique(BA_njan_5m5off$ID)) / 3) + 1) {
    k <- k + 1
    c <- 1
  } else if (c == (ceiling(length(unique(
    BA_njan_5m5off$ID
  )) / 3) * 2) + 1) {
    k <- k + 1
    c <- 1
  }
  print(plot_list_5m5off[[i]], vp = viewport(layout.pos.row = c, layout.pos.col = k))
  c <- c + 1
}
dev.off()



### 20off

BA_njan_20off <-
  BA_njan_int[which(
    BA_njan_int$freq == 35 &
      BA_njan_int$stimulus == '1-Sinus-20ms' &
      BA_njan_int$light == 'off'
  ), ]
unique(BA_njan_20off$ID)

plot_list_20off = list()
for (c_i in unique(BA_njan_20off$ID)) {
  BA_njan_35 <-
    BA_njan_20off[which(BA_njan_20off$freq == 35 &
                          BA_njan_20off$ID == c_i), ]
  plot <-
    ggplot(BA_njan_35, aes(
      as.factor(windows_perint),
      RMS_dB,
      group = int,
      colour = int
    )) + geom_line(size = 1.5) + scale_colour_manual(values = c  (marker$color)) + ylim(-85, -45) + ggtitle(c_i) + theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )
  plot_list_20off[[c_i]] = plot
}


tiff(
  "20off.tiff",
  width = 28,
  height = 40,
  units = 'cm',
  res = 600
)
grid.newpage()
pushViewport(viewport(layout = grid.layout(ceiling(length(
  unique(BA_njan_20off$ID)
) / 3), 3)))
c <- 1
k <- 1
for (i in unique(BA_njan_20off$ID))  {
  if (c == ceiling(length(unique(BA_njan_20off$ID)) / 3) + 1) {
    k <- k + 1
    c <- 1
  } else if (c == (ceiling(length(unique(BA_njan_20off$ID)) / 3) * 2) + 1) {
    k <- k + 1
    c <- 1
  }
  print(plot_list_20off[[i]], vp = viewport(layout.pos.row = c, layout.pos.col = k))
  c <- c + 1
}
dev.off()






continious_10m5off<- ggplot(BA_njan_10m5off, aes( as.factor(window),  RMS_dB,  group = ID,  colour = ID)) + 
  geom_line(size = 1) +  
  geom_vline(xintercept = seq(5,145,10)) +
  ylim(-85, -45) + 
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

continious_10m5on<- ggplot(BA_njan_10m5on, aes( as.factor(window),  RMS_dB,  group = ID,  colour = ID)) + 
  geom_line(size = 1) +  
  geom_vline(xintercept = seq(5,145,10)) +
  ylim(-85, -45) + 
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

continious_20off<- ggplot(BA_njan_20off, aes( as.factor(window),  RMS_dB,  group = ID,  colour = ID)) + 
  geom_line(size = 1) +  
  geom_vline(xintercept = seq(5,145,10)) +
  ylim(-85, -45) + 
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

continious_5m5off<- ggplot(BA_njan_5m5off, aes( as.factor(window),  RMS_dB,  group = ID,  colour = ID)) + 
  geom_line(size = 1) +  
  geom_vline(xintercept = seq(5,145,10)) +
  ylim(-85, -45) + 
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )


grid.newpage()
pushViewport(viewport(layout = grid.layout(4,1)))
print(continious_5m5off, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(continious_10m5off, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(continious_10m5on, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(continious_20off, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))

########################################################################################################
#### filtering out individuals that were not active at all (all values for all intensities below -80dB)

non_act_vec <- c()
act_vec <- c()

for (cc in unique(BA_njan_int$ID)) {
  BA_njan_ID <-
    BA_njan_int[which(BA_njan_int$ID == cc & BA_njan_int$freq == 35), ]
  if (all(BA_njan_ID$RMS_dB < -80)) {
    non_act_vec <- c(non_act_vec, cc)
  } else {
    act_vec <- c(act_vec, cc)
  }
}

##act data (35kHz)

BA_njan_35 <- BA_njan_int[which(BA_njan_int$freq == 35), ]
BA_njan_act35 <- BA_njan_35[which(BA_njan_35$ID %in% act_vec), ]

### normalize around stimulus onset


BA_njan_act35_norm <- data.frame(
  species = character(),
  ID = character(),
  stimulus = character(),
  freq = numeric(),
  light = character(),
  window = character(),
  RMS_lin = numeric(),
  RMS_dB = numeric(),
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
          (BA_njan_onetrace$RMS_dB[which(BA_njan_onetrace$windows_perint == 5)] + BA_njan_onetrace$RMS_dB[which(BA_njan_onetrace$windows_perint == 6)]) /
          2
        
        BA_njan_onetrace$RMS_dB_norm <-
          BA_njan_onetrace$RMS_dB - norm_vel
        
        
        ### fill in table here
        BA_njan_act35_norm <-
          rbind(BA_njan_act35_norm, BA_njan_onetrace)
        
        c <- c + 1
        
      }
    }
  }
}


### 10m5off - normalized around stimulus onset

BA_njan_10m5off <-
  BA_njan_act35_norm[which(BA_njan_act35_norm$stimulus == '2-Sinus-10x4ms' &
                             BA_njan_act35_norm$light == 'off'), ]
unique(BA_njan_10m5off$ID)

plot_list_20off = list()
for (c_i in unique(BA_njan_10m5off$ID)) {
  BA_njan_35_IDnorm <-
    BA_njan_10m5off[which(BA_njan_10m5off$ID == c_i), ]
  plot <-
    ggplot(BA_njan_35_IDnorm,
           aes(
             as.factor(windows_perint),
             RMS_dB_norm,
             group = int,
             colour = int
           )) + geom_line(size = 1.5) + scale_colour_manual(values = c  (marker$color)) + ylim(-30, +30) + ggtitle(c_i) + theme(
             legend.position = "none",
             axis.title.y = element_blank(),
             axis.title.x = element_blank()
           )
  plot_list_20off[[c_i]] = plot
}

tiff(
  "10x5off_norm.tiff",
  width = 28,
  height = 40,
  units = 'cm',
  res = 600
)
grid.newpage()
pushViewport(viewport(layout = grid.layout(ceiling(length(
  unique(BA_njan_10m5off$ID)
) / 3), 3)))
c <- 1
k <- 1
for (i in unique(BA_njan_10m5off$ID))  {
  if (c == ceiling(length(unique(BA_njan_10m5off$ID)) / 3) + 1) {
    k <- k + 1
    c <- 1
  } else if (c == (ceiling(length(unique(
    BA_njan_10m5off$ID
  )) / 3) * 2) + 1) {
    k <- k + 1
    c <- 1
  }
  print(plot_list_20off[[i]], vp = viewport(layout.pos.row = c, layout.pos.col = k))
  c <- c + 1
}
dev.off()


### 10m5on - normalized around stimulus onset

BA_njan_10m5on <-
  BA_njan_act35_norm[which(BA_njan_act35_norm$stimulus == '2-Sinus-10x4ms' &
                             BA_njan_act35_norm$light == 'on'), ]
unique(BA_njan_10m5on$ID)

plot_list_20off = list()
for (c_i in unique(BA_njan_10m5on$ID)) {
  BA_njan_35_IDnorm <-
    BA_njan_10m5on[which(BA_njan_10m5on$ID == c_i), ]
  plot <-
    ggplot(BA_njan_35_IDnorm,
           aes(
             as.factor(windows_perint),
             RMS_dB_norm,
             group = int,
             colour = int
           )) + geom_line(size = 1.5) + scale_colour_manual(values = c  (marker$color)) + ylim(-30, +30) + ggtitle(c_i) + theme(
             legend.position = "none",
             axis.title.y = element_blank(),
             axis.title.x = element_blank()
           )
  plot_list_20off[[c_i]] = plot
}

tiff(
  "10x5on_norm.tiff",
  width = 28,
  height = 40,
  units = 'cm',
  res = 600
)
grid.newpage()
pushViewport(viewport(layout = grid.layout(ceiling(length(
  unique(BA_njan_10m5on$ID)
) / 3), 3)))
c <- 1
k <- 1
for (i in unique(BA_njan_10m5on$ID))  {
  if (c == ceiling(length(unique(BA_njan_10m5on$ID)) / 3) + 1) {
    k <- k + 1
    c <- 1
  } else if (c == (ceiling(length(unique(
    BA_njan_10m5on$ID
  )) / 3) * 2) + 1) {
    k <- k + 1
    c <- 1
  }
  print(plot_list_20off[[i]], vp = viewport(layout.pos.row = c, layout.pos.col = k))
  c <- c + 1
}
dev.off()

### 5m5off - normalized around stimulus onset

BA_njan_5m5on <-
  BA_njan_act35_norm[which(BA_njan_act35_norm$stimulus == '2-Sinus-5x4ms' &
                             BA_njan_act35_norm$light == 'off'), ]
unique(BA_njan_5m5on$ID)

plot_list_20off = list()
for (c_i in unique(BA_njan_5m5on$ID)) {
  BA_njan_35_IDnorm <- BA_njan_5m5on[which(BA_njan_5m5on$ID == c_i), ]
  plot <-
    ggplot(BA_njan_35_IDnorm,
           aes(
             as.factor(windows_perint),
             RMS_dB_norm,
             group = int,
             colour = int
           )) + geom_line(size = 1.5) + scale_colour_manual(values = c  (marker$color)) + ylim(-30, +30) + ggtitle(c_i) + theme(
             legend.position = "none",
             axis.title.y = element_blank(),
             axis.title.x = element_blank()
           )
  plot_list_20off[[c_i]] = plot
}

tiff(
  "5x5off_norm.tiff",
  width = 28,
  height = 40,
  units = 'cm',
  res = 600
)
grid.newpage()
pushViewport(viewport(layout = grid.layout(ceiling(length(
  unique(BA_njan_5m5on$ID)
) / 3), 3)))
c <- 1
k <- 1
for (i in unique(BA_njan_5m5on$ID))  {
  if (c == ceiling(length(unique(BA_njan_5m5on$ID)) / 3) + 1) {
    k <- k + 1
    c <- 1
  } else if (c == (ceiling(length(unique(BA_njan_5m5on$ID)) / 3) * 2) + 1) {
    k <- k + 1
    c <- 1
  }
  print(plot_list_20off[[i]], vp = viewport(layout.pos.row = c, layout.pos.col = k))
  c <- c + 1
}
dev.off()

### 20off - normalized around stimulus onset

BA_njan_20off <-
  BA_njan_act35_norm[which(BA_njan_act35_norm$stimulus == '1-Sinus-20ms' &
                             BA_njan_act35_norm$light == 'off'), ]
unique(BA_njan_20off$ID)

plot_list_20off = list()
for (c_i in unique(BA_njan_20off$ID)) {
  BA_njan_35_IDnorm <- BA_njan_20off[which(BA_njan_20off$ID == c_i), ]
  plot <-
    ggplot(BA_njan_35_IDnorm,
           aes(
             as.factor(windows_perint),
             RMS_dB_norm,
             group = int,
             colour = int
           )) + geom_line(size = 1.5) + scale_colour_manual(values = c  (marker$color)) + ylim(-30, +30) + ggtitle(c_i) + theme(
             legend.position = "none",
             axis.title.y = element_blank(),
             axis.title.x = element_blank()
           )
  plot_list_20off[[c_i]] = plot
}

tiff(
  "20off_norm.tiff",
  width = 28,
  height = 40,
  units = 'cm',
  res = 600
)
grid.newpage()
pushViewport(viewport(layout = grid.layout(ceiling(length(
  unique(BA_njan_20off$ID)
) / 3), 3)))
c <- 1
k <- 1
for (i in unique(BA_njan_20off$ID))  {
  if (c == ceiling(length(unique(BA_njan_20off$ID)) / 3) + 1) {
    k <- k + 1
    c <- 1
  } else if (c == (ceiling(length(unique(BA_njan_20off$ID)) / 3) * 2) + 1) {
    k <- k + 1
    c <- 1
  }
  print(plot_list_20off[[i]], vp = viewport(layout.pos.row = c, layout.pos.col = k))
  c <- c + 1
}
dev.off()


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
  dif56 = numeric(),
  dif57 = numeric(),
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
        
        dif56 <-
          BA_njan_onetrace$RMS_dB_norm[which(BA_njan_onetrace$windows_perint == 6)] - BA_njan_onetrace$RMS_dB_norm[which(BA_njan_onetrace$windows_perint == 5)]
        
        dif57 <-
          BA_njan_onetrace$RMS_dB_norm[which(BA_njan_onetrace$windows_perint == 7)] - BA_njan_onetrace$RMS_dB_norm[which(BA_njan_onetrace$windows_perint == 5)]
        
        ### fill in table here
        BA_diff[c, 1] <- as.character(ID)
        BA_diff[c, 2] <- as.character(stimulus)
        BA_diff[c, 3] <- 35
        BA_diff[c, 4] <- as.character(light)
        BA_diff[c, 5] <- as.character(int)
        BA_diff[c, 6] <- dif56
        BA_diff[c, 7] <- dif57
        
        c <- c + 1
        
      }
    }
  }
}


#### histogram differnces ####

ggplot (BA_diff, aes(dif56)) + geom_histogram(binwidth = 0.05) + xlim(0,13)

ggplot (BA_diff, aes(dif57)) + geom_histogram(binwidth = 0.05) + xlim(0,13)










BA_njan_35_1 <-
  BA_njan_int[which(BA_njan_int$freq == 35 &
                      BA_njan_int$ID == 'Njan_089'), ]
plot1 <-
  ggplot(BA_njan_35_1, aes(
    as.factor(windows_perint),
    RMS_dB,
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
    RMS_dB,
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
    RMS_dB,
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
    RMS_dB,
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
    RMS_dB,
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
    RMS_dB,
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
  RMS_dB,
  group = int,
  colour = int
)) + geom_line() + scale_colour_manual(values = c(marker$color)) + ylim(-85, -50) + facet_grid(. ~ ID)

BA_njan_int_1 <- BA_njan_int[1:93755, ]

ggplot(BA_njan_int_1, aes(
  as.factor(windows_perint),
  RMS_dB,
  group = int,
  colour = int
)) + geom_line() + scale_colour_manual(values = c(marker$color)) + ylim(-85, -50) + facet_grid(. ~ ID)

BA_njan_int_10 <- BA_njan_int[1:20795, ]

ggplot(BA_njan_int_10, aes(
  as.factor(windows_perint),
  RMS_dB,
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
            mean(BA_njan_onetrace$RMS_dB[which(
              BA_njan_onetrace$windows_perint >= 1 &
                BA_njan_onetrace$windows_perint <= 4
            )])
          FS_post <-
            mean(BA_njan_onetrace$RMS_dB[which(
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
