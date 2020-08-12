library(tidyverse)
library(gridExtra)

plot_audiogram_J <- function(df,width=4){
  shift_haba = 0.2
  yajirusi_tate = 4
  yajirusi_yoko = 0.2

  point_size = width*0.8
  text_size = width*3
# THIS FAILED
#  datacols <- str_subset(names(df),"^[RL][AB]")
#  ldf <- pivot_longer(df,cols=datacols,names_to=c("ear","mode","freq"),names_sep=c(1,2),values_to="dB",values_ptypes=list(dB=character()))
  ldf <- gather(df, key = "ear-mode-freq", value = "dB",starts_with(c("RA","RB","LA","LB"))) %>%
    separate(col = "ear-mode-freq", into = c("ear","mode","freq"), sep = c(1,2)) %>%
    mutate(SO = ifelse(is.na(as.numeric(dB)), str_sub(dB,-1,-1),NA)) %>%
    mutate(dB = as.numeric(ifelse(is.na(as.numeric(dB)), str_sub(dB,0,-2),dB))) %>% 
    mutate(freq = log2(as.numeric(freq)/125)) %>%
    mutate(pch = ifelse(ear=="R",ifelse(mode=="A",1,91),ifelse(mode=="A",4,93))) %>%
    mutate(col = ifelse(ear=="R","red","blue")) %>%
    mutate(shift = ifelse(mode=="B",ifelse(ear=="R",-shift_haba,shift_haba),0)) 
  
  ldfa <- subset(ldf,(mode=="A" & !is.na(dB))) %>% 
    arrange(ear,freq) %>%
    mutate(lSO = lag(SO)) %>%
    mutate(lfreq = lag(freq)) %>%
    mutate(ldB = lag(dB)) %>%
    mutate(ltype = ifelse(ear=="R","solid","dashed")) 
  ldfa <- subset(ldfa,(freq>lfreq & is.na(SO) & is.na(lSO)))
  
  ldfso <- subset(ldf,SO=="+") %>%
    mutate(dir = ifelse(ear=="R",-1,1)) 
  
  ggp <- ggplot() +
    scale_shape_identity() +
    scale_color_identity() +
    scale_linetype_identity() +
    geom_hline(yintercept = 0,size=0.1) +
    geom_point(data=ldf,aes(x=freq+shift,y=dB,shape=pch,color=col),size=point_size)+
    geom_segment(data=ldfa,aes(x=lfreq,y=ldB,
                                xend=freq,yend=dB,
                               linetype=ltype,color=col),size=0.3)+
    geom_segment(data=ldfso,aes(x=freq+shift+yajirusi_yoko*dir*0.5,y=dB+yajirusi_tate*0.6,
                                xend=freq+shift+yajirusi_yoko*dir,
                                yend=dB+yajirusi_tate*1.4,
                                color=col),size=0.3,arrow = arrow(length = unit(0.1, "cm")))+
    scale_y_reverse(limits = c(120,-20), breaks = seq(-20, 120, by=10))+
    scale_x_continuous(position = "top",breaks=c(0,1,2,3,4,5,6),labels=c("125","250","500","1,000","2,000","4,000","8,000"))+ 
    labs(x = "周波数 (Hz)", y = "聴力レベル (dB)") +
    coord_fixed(ratio=0.05)

  if(str_length(df$Title)>0){
    ggp <- ggp + ggtitle(df$Title)
  }
  return(ggp+theme(panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    text = element_text(size=text_size)
  #                 ,axis.text.x = element_text(angle=90, hjust=1) #コメントを消すとX軸が縦書きに
         ))
}


# MODIFY FILENAME HERE
table <- read_csv("test.csv")

quartzFonts(HiraKaku = quartzFont(c("HiraginoSans-W5", "HiraginoSans-W7", "HiraginoSans-W5", "HiraginoSans-W7")))
theme_set(theme_bw(base_family="HiraKaku"))

#theme_set(theme_bw(base_family="Japan1"))

res <- list()
for(i in 1:nrow(table)){
  res[[i]] <- plot_audiogram_J(table[i,],width=2) 
}

ml <- marrangeGrob(res, nrow=5, ncol=4, top="")
#ggsave(file="audiosV.pdf",plot=ml,dpi=300,width = 8,height=11.5)
print(ml)

