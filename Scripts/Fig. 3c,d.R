library(vegan)
library(permute)
library(lattice)

#asv table from BC or RC
asv <- read.delim('data_xxx.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
asv=asv[,-dim(asv)[2]]
asv=t(asv)

sp <- specaccum(asv, method = 'random')
summary(sp)

plot(sp, ci.type = 'poly', col = 'blue', lwd = 2, ci.lty = 0, ci.col = 'lightblue')
boxplot(sp, col = 'yellow', add = TRUE, pch = '+')


asv <- read.delim('asv_RC.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
asv=asv[,-dim(asv)[2]]
asv=t(asv)

sp <- specaccum(asv, method = 'random')
summary(sp)

plot(sp, ci.type = 'poly', col = 'blue', lwd = 2, ci.lty = 0, ci.col = 'lightblue')
boxplot(sp, col = 'yellow', add = TRUE, pch = '+')
