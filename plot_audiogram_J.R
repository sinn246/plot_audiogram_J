library(ggplot2)
library(gridExtra)
library(tidyverse)

quartzFonts(HiraKaku = quartzFont(c("HiraginoSans-W5", "HiraginoSans-W7", "HiraginoSans-W5", "HiraginoSans-W7")))

plot_audiogram_J <- function(df,width=4){
  shift_haba = 0.2
  yajirusi_tate = 4
  yajirusi_yoko = 0.2

  point_size = width*0.8
  text_size = width*3
  
  ldf <- gather(df, key = "ear-mode-freq", value = "dB",-Title) %>%
    separate(col = "ear-mode-freq", into = c("ear","mode","freq"), sep = c(1,2)) %>%
    mutate(SO = ifelse(is.na(as.numeric(dB)), str_sub(dB,-1,-1),NA)) %>%
    mutate(dB = as.numeric(ifelse(is.na(as.numeric(dB)), str_sub(dB,0,-2),dB))) %>% 
    mutate(freq = as.numeric(freq)) %>%
    mutate(freqLabel = formatC(freq,format="g", big.mark=",")) %>%
    mutate(freq = log2(freq/125)) %>%
    mutate(pch = ifelse(ear=="R",ifelse(mode=="A",1,91),ifelse(mode=="A",4,93))) %>%
    mutate(col = ifelse(ear=="R","red","blue")) %>%
    mutate(shift = ifelse(mode=="A",0,ifelse(ear=="R",-1,1))) 
  
  ldfa <- subset(ldf,(mode=="A" & !is.na(dB))) %>% 
    arrange(ear,freq) %>%
    mutate(lSO = lag(SO)) %>%
    mutate(lfreq = lag(freq)) %>%
    mutate(ldB = lag(dB)) %>%
    mutate(lty = ifelse(ear=="R","solid","dashed")) 
  ldfa <- subset(ldfa,(freq>lfreq & is.na(SO) & is.na(lSO)))
  
  ldfso <- subset(ldf,SO=="+") %>%
    mutate(dir = ifelse(ear=="R",-1,1)) 
  
  ggp <- ggplot() +
    scale_shape_identity() +
    scale_color_identity() +
    scale_linetype_identity() +
    geom_hline(yintercept = 0,size=0.1) +
    geom_point(data=ldf,aes(x=freq+shift*shift_haba,y=dB,shape=pch,color=col),size=point_size)+
    geom_segment(data=ldfa,aes(x=lfreq,y=ldB,
                                xend=freq,yend=dB,
                               linetype=lty,color=col),size=0.3)+
    geom_segment(data=ldfso,aes(x=freq+shift*shift_haba+yajirusi_yoko*dir*0.5,y=dB+yajirusi_tate*0.6,
                                xend=freq+shift*shift_haba+yajirusi_yoko*dir,
                                yend=dB+yajirusi_tate*1.4,
                                color=col),size=0.3,arrow = arrow(length = unit(0.1, "cm")))+
    scale_y_reverse(limits = c(120,-20), breaks = seq(-20, 120, by=10))+
    scale_x_continuous(breaks=c(0,1,2,3,4,5,6),labels=c("125","250","500","1,000","2,000","4,000","8,000")  )+ 
             #breaks = unique(ldf$freq), labels = unique(ldf$freqLabel))+
    labs(x = "周波数 (Hz)", y = "閾値 (dB HL)")+
    theme_bw(base_family = "HiraKaku")
  if(str_length(df$Title)>0){
    ggp <- ggp + ggtitle(df$Title)
  }
  return(ggp+theme(panel.grid.minor =   element_blank(),
    plot.title = element_text(hjust = 0.5),
                   text = element_text(size=text_size)
  #                 ,axis.text.x = element_text(angle=90, hjust=1) #コメントを消すとX軸が縦書きに
         ))
}


# MODIFY FILENAME HERE
table <- read_csv("test.csv")

res <- list()
for(i in 1:nrow(table)){
  res[[i]] <- plot_audiogram_J(table[i,],width=2) 
}

ml <- marrangeGrob(res, nrow=5, ncol=4, top="")
quartz(file="audio.pdf",type = "pdf", family = "HiraKaku",width = 8,height=11.5)
print(ml)
dev.off()

