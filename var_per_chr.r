#Third figure: the number of variants from each chromosome
# The for loop makes the names of the chromosomes and filters the data each time, retrieving the variants in each chrom., then the number is counted and added to a data frame together with the chrom. number. This is then plotted.
nchr_total <- data.frame()
for (i in c(1:22)){
  n <- paste("chr", i, sep = "")
  a <- data %>% filter(Chromosome == n)
  b <- paste("n", n, sep = "")
  c <- nrow(a)
  nchr <- data.frame("Chromosome" = substr(n, 4 ,5), "Number of variants" = c)
  print(nchr)
  
  
  nchr_total <- rbind(nchr_total, nchr)
}
print(nchr_total)

nchr_total$Chromosome <- factor(nchr_total$Chromosome, levels = nchr_total$Chromosome)

nchr_fig <- ggplot(data=nchr_total, aes(Chromosome, Number.of.variants, fill=Number.of.variants))+ #Third figure
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("Variants per chromsome")+
  theme_classic()



dnms <- data %>% filter(parentOfOrigin == "DNM")
ndnms <- nrow(dnms)
print(ndnms)

ndnm_total <- data.frame()
for (dnmi in c(1:22)){
  dnmn <- paste("chr", dnmi, sep = "")
  dnma <- dnms %>% filter(Chromosome == dnmn)
  dnmb <- paste("dnmn", dnmn, sep = "")
  dnmc <- nrow(dnma)
  ndnm <- data.frame("Chromosome" = substr(dnmn, 4 ,5), "Number of variants" = dnmc)
  print(ndnm)
  
  
  ndnm_total <- rbind(ndnm_total, ndnm)
}
print(ndnm_total)

ndnm_total$Chromosome <- factor(ndnm_total$Chromosome, levels = ndnm_total$Chromosome)

ndnm_fig <- ggplot(data=ndnm_total, aes(Chromosome, Number.of.variants, fill=Number.of.variants))+ #Third figure
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("DNMs per chromsome")+
  theme_classic()