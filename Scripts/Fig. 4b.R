library(vegan)
library(permute)
library(lattice)
library(ggrepel)
library(ape)
library(plyr)
library(ggExtra)
library(ggplot2)

data <- read.delim(file = "data_xxx.txt", row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE, header = T)
dim(data)
head(data)

cmic_group <- read.delim(file = "group.txt")
cmic_group$Tech <- factor(cmic_group$Tech, levels=c('BS', 'RS', 'BC', 'RC'))
cmic_group$Type <- factor(cmic_group$Type, levels=c('BS', 'RS', 'BCL', 'BCM', 'BCH', 'RCL', 'RCM', 'RCH'))

cmic_dist <- vegdist(data, method="jaccard", binary=T)
#cmic_dist <- vegdist(data, method="bray") 

cmic_pcoa <- cmdscale(cmic_dist, k=3, eig=T)
cmic_pcoa_points <- as.data.frame(cmic_pcoa$points)
sum_eig <- sum(cmic_pcoa$eig)
eig_percent <- round(cmic_pcoa$eig/sum_eig*100,1)
colnames(cmic_pcoa_points) <- paste0("PCoA", 1:3)
cmic_pcoa_result <- cbind(cmic_pcoa_points, cmic_group)
head(cmic_pcoa_result)


p1 <- ggplot(cmic_pcoa_result, aes(x=PCoA1, y=PCoA2, color=Type, shape=Type )) +
  labs(x=paste("PCoA axis1 (", eig_percent[1], "%)", sep=""),
       y=paste("PCoA axis2 (", eig_percent[2], "%)", sep="")) +
  scale_shape_manual(values = c(3, 4, 19, 19, 19, 15, 15, 15))+
  scale_color_manual(values = c('Black', 'Black', '#6EA2DB', '#45A8AC', '#DC8F95', '#377eb8', '#4daf4a', '#e41a1c'))+
  geom_point(size=1.5, alpha = 0.7
  ) + stat_ellipse(level=0.95) +
  
  #geom_text(size=2, aes(label = rownames(data)))+
  #geom_text_repel(size=2, aes(label = rownames(data), max.overlaps=100))+
  
  theme_bw()+
  theme(axis.text.x = element_text(color="black", size = 6.5), 
        axis.text.y = element_text(color="black", size = 6.5),
        axis.title = element_text(color="black", face="bold", size = 7),
        #legend.title = element_text(color="black", face="bold", size = 7),
        legend.title = element_blank(), 
        legend.text = element_text(color="black", size = 6.5),
        legend.key.size = unit(0.2, "inches"),
        legend.position = "bottom",
        legend.direction = "vertical") #legend.direction: vertical
p1

ggMarginal(p1, type="density", size=5, margins="both", groupColour=TRUE, groupFill=TRUE)
#ggMarginal(p, data, x, y, type = c("density", "histogram", "boxplot", "violin", "densigram"), margins = c("both", "x", "y"), size = 5, ..., xparams = list(), yparams = list(), groupColour = FALSE, groupFill = FALSE)
