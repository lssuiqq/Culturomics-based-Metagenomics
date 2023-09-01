phylum_top10 <- read.delim('data_xxx.txt', row.names = 1, stringsAsFactors = FALSE, check.names = FALSE)

library(reshape2)
library(ggplot2)

phylum_top10$Taxonomy <- factor(rownames(phylum_top10), levels = rev(rownames(phylum_top10)))
phylum_top10 <- melt(phylum_top10, id = 'Taxonomy')

group <- read.delim('group.txt', sep = '	', stringsAsFactors = FALSE)
names(group)[1] <- 'variable'
phylum_top10 <- merge(phylum_top10, group, by = 'variable')

p <- ggplot(phylum_top10, aes(variable, 1 * value, fill = Taxonomy)) +
  geom_col(position = 'stack', width = .9) +
  facet_wrap(~group, scales = 'free_x', ncol = 6) +
  facet_grid(~group, scales = "free",space = "free_x")+
  scale_fill_manual(values =  rev(c('#756FB3', '#D95F02', '#1B9E77', '#FFD92E', '#A6D753', '#E789C3', '#8D9FCA', '#FB8D61', '#66C2A4', '#A6CEE2', 'gray'))) +
  labs(fill='Phylum', x = '', y = 'No. of total ASVs') +
  scale_y_continuous(breaks=seq(0,4000,500))+
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(color = 'black', fill = 'transparent'), 
        strip.background = element_rect(color = "black"), 
        strip.text = element_text(size = 7)) +
  theme(axis.text.x = element_text(color = 'black',size = 6,angle = 45,hjust = 1,vjust = 1),
        axis.text.y = element_text(color = 'black',size = 6), 
        axis.title = element_text(color = 'black',size = 7), 
        #legend.title = element_blank(), 
        legend.title = element_text(face="bold", color = 'black',size = 7),
        legend.text = element_text(color = 'black',size = 6.5),
        legend.key.size = unit(0.15, "inches"),
        axis.ticks = (element_line(color = "black", size = 0.5)))
p
