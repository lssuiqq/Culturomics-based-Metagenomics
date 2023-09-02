library(ggplot2)
dat <- read.delim(file = "FG_KO.txt", header = T)

p1 <- ggplot(dat,aes(x=FG,y=KO))+
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
  labs(x="No. of functional groups (FAPROTAX)",y="No. of KOs (PICRUSt2)")+
  ggpubr::stat_cor(method="pearson",label.x= 0.5,label.y = 5900, size=2.5) #spearman

p1