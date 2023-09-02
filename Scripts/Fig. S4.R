library(ggplot2)
library(gridExtra)
library(gapminder)
library(dplyr)
library(ggsignif)
library(ggpubr)

abun <- read.delim('abun_box.txt', stringsAsFactors = FALSE, check.names = FALSE)

head(abun)

abun$Group <- factor(abun$Group, levels=c("BCL","BCM","BCH","RCL","RCM","RCH"))
abun$ASV_ID <- factor(abun$ASV_ID, levels=c("ASV1","ASV2","ASV3","ASV4","ASV5",
                                            "ASV6","ASV7","ASV8","ASV9","ASV10",
                                            "ASV11","ASV12","ASV13","ASV14","ASV15",
                                            "ASV16","ASV17","ASV18","ASV19","ASV20",
                                            "ASV21","ASV22","ASV23","ASV24","ASV25",
                                            "ASV26","ASV27","ASV28","ASV29","ASV30",
                                            "ASV31","ASV32","ASV33","ASV34","ASV35"))
my_comparisons <- list( c("BCL","BCM"),
                        c("BCM","BCH"),
                        c("BCL","BCH"),
                        c("RCL","RCM"),
                        c("RCM","RCH"),
                        c("RCL","RCH"),
                        c("BCL","RCL"),
                        c("BCM","RCM"),
                        c("BCH","RCH") )

plot <- ggplot(abun, aes(x = Group, y = Relative_abundance, fill = Group)) +
  scale_fill_manual(values=c('#6EA2DB', '#45A8AC', '#DC8F95', '#377eb8', '#4daf4a', '#e41a1c'))+
  geom_boxplot(aes(fill=Group), outlier.colour="white", color = "black", size=0.3)+
  geom_jitter(width = 0.25, size = 0.03, alpha = 0.75)+
  theme_test()+
  theme(panel.background =element_blank(),
        strip.text = element_text(face="bold", size = 5),
        #axis.line = element_line(color="black", size = 0.5),
        axis.title = element_text(color="black", face="bold", size = 7),
        axis.text = element_text(color="black", size = 5))+
  facet_wrap(~ASV_ID, ncol=7, scales = "free_y") + #scales = "free_y"
  scale_y_continuous(name = "Relative abundance (%)") +
  scale_x_discrete(labels = abbreviate, name = NULL)

plot

  