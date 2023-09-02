library(ggplot2)
dat <- read.delim(file = "MAG.txt", header = T)

p1 <- ggplot(dat,aes(x=X1K,y=hMAG))+
  geom_point(alpha=.8)+
  geom_smooth(method="lm", se=T)+
  theme_bw()+
  theme(legend.position = "right",legend.background = element_blank(),legend.key = element_rect(colour = 'gray'))+
  theme(axis.text.x = element_text(size = 5,hjust = 0.5,vjust = 0), 
        axis.title = element_text(size = 6.5), legend.title = element_blank(), 
        legend.text = element_text(size = 6.5))+
  theme(axis.text.y = element_text(size = 5,hjust = 1,vjust = .5),
        axis.title = element_text(size = 6.5), legend.title = element_blank(), 
        legend.text = element_text(size = 6.5))+
  labs(x="No. of Contigs (≥1 Kbp)",y="No. of high-quality MAGs")+
  ggpubr::stat_cor(method="pearson",label.x= 10,label.y = 13, size=2.5) #spearman
p1

p2 <- ggplot(dat,aes(x=X10K,y=hMAG))+
  geom_point(alpha=.8)+
  geom_smooth(method="lm", se=T)+
  theme_bw()+
  theme(legend.position = "right",legend.background = element_blank(),legend.key = element_rect(colour = 'gray'))+
  theme(axis.text.x = element_text(size = 5,hjust = 0.5,vjust = 0), 
        axis.title = element_text(size = 6.5), legend.title = element_blank(), 
        legend.text = element_text(size = 6.5))+
  theme(axis.text.y = element_text(size = 5,hjust = 1,vjust = .5),
        axis.title = element_text(size = 6.5), legend.title = element_blank(), 
        legend.text = element_text(size = 6.5))+
  labs(x="No. of Contigs (≥10 Kbp)",y="No. of high-quality MAGs")+
  ggpubr::stat_cor(method="pearson",label.x= 10,label.y = 13, size=2.5) #spearman
p2

p3 <- ggplot(dat,aes(x=X100K,y=hMAG))+
  geom_point(alpha=.8)+
  geom_smooth(method="lm", se=T)+
  theme_bw()+
  theme(legend.position = "right",legend.background = element_blank(),legend.key = element_rect(colour = 'gray'))+
  theme(axis.text.x = element_text(size = 5,hjust = 0.5,vjust = 0), 
        axis.title = element_text(size = 6.5), legend.title = element_blank(), 
        legend.text = element_text(size = 6.5))+
  theme(axis.text.y = element_text(size = 5,hjust = 1,vjust = .5),
        axis.title = element_text(size = 6.5), legend.title = element_blank(), 
        legend.text = element_text(size = 6.5))+
  labs(x="No. of Contigs (≥100 Kbp)",y="No. of high-quality MAGs")+
  ggpubr::stat_cor(method="pearson",label.x= 10,label.y = 13, size=2.5) #spearman
p3

p4 <- ggplot(dat,aes(x=Binned_contigs,y=hMAG))+
  geom_point(alpha=.8)+
  geom_smooth(method="lm", se=T)+
  theme_bw()+
  theme(legend.position = "right",legend.background = element_blank(),legend.key = element_rect(colour = 'gray'))+
  theme(axis.text.x = element_text(size = 5,hjust = 0.5,vjust = 0), 
        axis.title = element_text(size = 6.5), legend.title = element_blank(), 
        legend.text = element_text(size = 6.5))+
  theme(axis.text.y = element_text(size = 5,hjust = 1,vjust = .5),
        axis.title = element_text(size = 6.5), legend.title = element_blank(), 
        legend.text = element_text(size = 6.5))+
  labs(x="No. of binned contigs",y="No. of high-quality MAGs")+
  ggpubr::stat_cor(method="pearson",label.x= 10,label.y = 13, size=2.5) #spearman
p4

p5 <- ggplot(dat,aes(x=Bins,y=hMAG))+
  geom_point(alpha=.8)+
  geom_smooth(method="lm", se=T)+
  theme_bw()+
  theme(legend.position = "right",legend.background = element_blank(),legend.key = element_rect(colour = 'gray'))+
  theme(axis.text.x = element_text(size = 5,hjust = 0.5,vjust = 0), 
        axis.title = element_text(size = 6.5), legend.title = element_blank(), 
        legend.text = element_text(size = 6.5))+
  theme(axis.text.y = element_text(size = 5,hjust = 1,vjust = .5),
        axis.title = element_text(size = 6.5), legend.title = element_blank(), 
        legend.text = element_text(size = 6.5))+
  labs(x="No. of total bins",y="No. of high-quality MAGs")+
  ggpubr::stat_cor(method="pearson",label.x= 10,label.y = 13, size=2.5) #spearman
p5

p6 <- ggplot(dat,aes(x=mMAG,y=hMAG))+
  geom_point(alpha=.8)+
  geom_smooth(method="lm", se=T)+
  theme_bw()+
  theme(legend.position = "right",legend.background = element_blank(),legend.key = element_rect(colour = 'gray'))+
  theme(axis.text.x = element_text(size = 5,hjust = 0.5,vjust = 0), 
        axis.title = element_text(size = 6.5), legend.title = element_blank(), 
        legend.text = element_text(size = 6.5))+
  theme(axis.text.y = element_text(size = 5,hjust = 1,vjust = .5),
        axis.title = element_text(size = 6.5), legend.title = element_blank(), 
        legend.text = element_text(size = 6.5))+
  labs(x="No. of medium-quality MAGs",y="No. of high-quality MAGs")+
  ggpubr::stat_cor(method="pearson",label.x= 10,label.y = 13, size=2.5) #spearman
p6

# merge figures
library("gridExtra")
library(ggpubr)
p0 <- grid.arrange(p1, 
                   p2 + rremove("ylab"), 
                   p3 + rremove("ylab"), 
                   p4,
                   p5 + rremove("ylab"),
                   p6 + rremove("ylab"), 
                   ncol = 3, nrow = 2) 
p0