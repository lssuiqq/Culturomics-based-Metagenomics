FAPROTAX <- read.delim('xxx.txt', row.names = 1, stringsAsFactors = FALSE, check.names = FALSE)

library(reshape2)
library(ggplot2)

FAPROTAX$FUN <- factor(rownames(FAPROTAX), levels = rev(rownames(FAPROTAX)))
FAPROTAX <- melt(FAPROTAX, id = 'FUN')

group <- read.delim('group.txt', sep = '	', stringsAsFactors = FALSE)
names(group)[1] <- 'variable'
FAPROTAX <- merge(FAPROTAX, group, by = 'variable')
FAPROTAX$group <- factor(FAPROTAX$group, levels=c("BCL","BCM","BCH","BS","RCL","RCM","RCH","RS"))

p1 <- ggplot(FAPROTAX, aes(variable, 100 * value, fill = FUN)) +
  geom_col(position = 'stack', width = 1.0) +
  facet_wrap(~group, scales = 'free_x', ncol = 4) +
  scale_fill_manual(values =  rev(c('#4E79A7FF', '#A0CBE8FF', '#F28E2BFF',
                                    '#FFBE7DFF', '#59A14FFF', '#8CD17DFF',
                                    '#B6992DFF', '#F1CE63FF', '#499894FF',
                                    '#86BCB6FF', '#756BB1FF', '#BCBDDCFF',
                                    '#D37295FF', '#FABFD2FF', '#8C564BFF',
                                    '#C49C94FF', 'grey'))) +
  labs(x = '', y = 'Relative Abundance (%)') +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(color = 'black', fill = 'transparent'), 
        strip.background = element_rect(color = "black"),
        strip.text = element_text(face="bold", size = 7)) +
  theme(axis.text.x = element_text(color="black", size = 5,angle = 90,hjust = 1,vjust = .5), 
        axis.text.y = element_text(color="black", size = 5.5),
        axis.title = element_text(face="bold", size = 7),
        legend.title = element_text(face="bold", size = 7),
        #legend.title = element_blank(), 
        legend.text = element_text(size = 6.5),
        legend.key.size = unit(0.15, "inches"),
        axis.ticks = (element_line(color = "black")))+
  theme(legend.position = 'none')

p1
