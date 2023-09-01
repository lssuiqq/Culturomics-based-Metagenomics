phylum_top10 <- read.delim('data_xxx.txt', row.names = 1, stringsAsFactors = FALSE, check.names = FALSE)

library(reshape2)
library(ggplot2)

phylum_top10$Phylum <- factor(rownames(phylum_top10), levels = rev(rownames(phylum_top10)))
phylum_top10 <- melt(phylum_top10, id = 'Phylum')

group <- read.delim('group.txt', sep = '	', stringsAsFactors = FALSE)
names(group)[1] <- 'variable'
phylum_top10 <- merge(phylum_top10, group, by = 'variable')
phylum_top10$group <- factor(phylum_top10$group, levels=c("BCL","BCM","BCH","BC","BS","RCL","RCM","RCH","RC","RS"))

p1 <- ggplot(phylum_top10, aes(variable, 100 * value, fill = Phylum)) +
  geom_col(position = 'stack', width = 1.0) +
  facet_wrap(~group, scales = 'free_x', ncol = 5) +
  scale_fill_manual(values =  rev(c('#756FB3', '#D95F02', '#1B9E77',
                                    '#FFD92E', '#A6D753', '#E789C3',
                                    '#8D9FCA', '#FB8D61', '#66C2A4',
                                    '#A6CEE2', 'gray'))) +
  labs(x = '', y = 'Relative Abundance (%)') +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(color = 'black', fill = 'transparent'), 
        strip.background = element_rect(color = "black"),
        strip.text = element_text(face="bold", size = 7)) +
  theme(axis.text.x = element_text(color="black", size = 3,angle = 90,hjust = 1,vjust = .5), 
        axis.text.y = element_text(color="black", size = 5.5),
        axis.title = element_text(face="bold", size = 7),
        legend.title = element_text(face="bold", size = 7),
        #legend.title = element_blank(), 
        legend.text = element_text(size = 6.5),
        legend.key.size = unit(0.15, "inches")，
		axis.ticks = (element_line(color = "black")))+
  theme(legend.position = 'none')

p1