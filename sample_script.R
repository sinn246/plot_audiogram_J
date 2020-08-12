library(tidyverse)
library(gridExtra)

source("plot_audiogram_J.R")


output_to_screen <- FALSE
#output_to_screen <- TRUE  ##画面に表示したいとき　スケールはすこし変になることが多い

if(output_to_screen){
  #ここからがトリッキー　Macで画面に表示する場合　Windowsは手元にないのでわかりません
  quartzFonts(HiraKaku = quartzFont(c("HiraginoSans-W5", "HiraginoSans-W7", "HiraginoSans-W5", "HiraginoSans-W7")))
  theme_set(theme_bw(base_family="HiraKaku"))
}else{
  #PDFに出力するのでJapan1フォントを使用
  theme_set(theme_bw(base_family="Japan1"))
}

# 例1
# test.csvを使用した例　術前、術後の2つのデータ
table <- read_csv("test.csv")

res <- plot_audiogram_J(table[1,],width=4) 　#とりあえずデータの1行目のグラフを作成。
# width=4 は幅4インチを想定したフォントサイズということ　ちなみにA4サイズの横幅は8.27inch
# 4inch だと横に2つ並べる大きさ。

if(output_to_screen){
  print(res)
}else{
  ggsave(file="res1.pdf",plot=res,dpi=300,width = 4,height=7)
}


# 例1
# Book1.csvを使用した例　多数のデータを印刷
table <- read_csv("Book1.csv")

#PDFに出力するのでJapan1フォントを使用
theme_set(theme_bw(base_family="Japan1"))

res <- list()
for(i in 1:nrow(table)){
  res[[i]] <- plot_audiogram_J(table[i,],width=2) 
}

ml <- marrangeGrob(res, nrow=5, ncol=4, top="")
ggsave(file="audios.pdf",plot=ml,dpi=300,width = 8,height=11.5)


