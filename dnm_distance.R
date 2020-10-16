#First, the DNMs are sorted by chromosome. This loop makes the word chr# and filters the dnms file based on this. Then, a new variable with the name dnm_chr_# is made, and the variants are assigned to it. 
clustered_dnms <- data.frame(Chromosome=NA, Start.position=NA, End.position=NA, Reference=NA, Variant=NA, parentOfOrigin=NA, distance=NA, distance2=NA)
for (dnm_chr_i in c(1:22)){
  dnm_chr_n <- paste("chr", dnm_chr_i, sep = "") #Make chromosome#
  dnm_chr_a <- dnms %>% filter(Chromosome == dnm_chr_n) #Filter per chromosome
  
  
  dnm_chr <- data.frame(dnm_chr_a) #Make a data frame of the filtered dnms per chromosome
  dnm_chr <- dnm_chr[order(dnm_chr$Start.position),] #Sort based on the start position
  dnm_chr <- dnm_chr %>% mutate(distance = Start.position - lag(Start.position, default = Start.position[1])) #Add a column that calculates the difference in start position 
  dnm_chr$distance2 = dnm_chr$distance
  
  shift <- function(x, n){
    c(tail(x, -n), rep(NA, n))
  }

  dnm_chr$distance2 <- shift(dnm_chr$distance2, 1) #Shift the second distance column up one row
  dnm_chr <- dnm_chr %>% filter(distance < 10000 | distance2 < 10000) #Filter on both distances >10kb
  clustered_dnms <- full_join(clustered_dnms, dnm_chr, by=NULL, copy=FALSE)
  #clustered_dnms = clustered_dnms%>% full_join(dnm_chr, by="Chromosome")
  #merge(dnm_chr, clustered_dnms, by="Chromosome", all=TRUE)
  assign(paste("dnm_clustered_chr", dnm_chr_i, sep="_"), dnm_chr) #change the name of the final output, makes a file with all the clustered variants per chr.
}




non_clustered_dnms <- data.frame(Chromosome=NA, Start.position=NA, End.position=NA, Reference=NA, Variant=NA, parentOfOrigin=NA, distance=NA, distance2=NA)
for (dnm_chr_i in c(1:22)){
  dnm_chr_n <- paste("chr", dnm_chr_i, sep = "") #Make chromosome#
  dnm_chr_a <- dnms %>% filter(Chromosome == dnm_chr_n) #Filter per chromosome
  
  
  dnm_chr <- data.frame(dnm_chr_a) #Make a data frame of the filtered dnms per chromosome
  dnm_chr <- dnm_chr[order(dnm_chr$Start.position),] #Sort based on the start position
  dnm_chr <- dnm_chr %>% mutate(distance = Start.position - lag(Start.position, default = Start.position[1])) #Add a column that calculates the difference in start position 
  dnm_chr$distance2 = dnm_chr$distance
  
  shift <- function(x, n){
    c(tail(x, -n), rep(NA, n))
  }
  
  dnm_chr$distance2 <- shift(dnm_chr$distance2, 1) #Shift the second distance column up one row
  dnm_chr <- dnm_chr %>% filter(distance > 10000, distance2 > 10000) #Filter on both distances >10kb
  non_clustered_dnms <- full_join(non_clustered_dnms, dnm_chr, by=NULL, copy=FALSE)
  #clustered_dnms = clustered_dnms%>% full_join(dnm_chr, by="Chromosome")
  #merge(dnm_chr, clustered_dnms, by="Chromosome", all=TRUE)
  assign(paste("dnm_non_clustered_chr", dnm_chr_i, sep="_"), dnm_chr) #change the name of the final output, makes a file with all the clustered variants per chr.
}

qdnms <- nrow(non_clustered_dnms) + nrow(clustered_dnms)



#Now filtering on nuc. change again, calculating the percentage of the variants over the total number of DNMs that were detected
TG_clustered_dnm <- clustered_dnms %>% filter(Reference =="T", Variant=="G")
nTG_clustered_dnm <- (nrow(TG_clustered_dnm)/ndnms)*100

TG_non_clustered_dnm <- non_clustered_dnms %>% filter(Reference =="T", Variant=="G")
nTG_non_clustered_dnm <- (nrow(TG_non_clustered_dnm)/ndnms)*100


TC_clustered_dnm <- non_clustered_dnms %>% filter(Reference =="T", Variant=="C")
nTC_clustered_dnm <- (nrow(TC_clustered_dnm)/ndnms)*100

TC_non_clustered_dnm <- clustered_dnms %>% filter(Reference =="T", Variant=="C")
nTC_non_clustered_dnm <- (nrow(TC_non_clustered_dnm)/ndnms)*100


TA_clustered_dnm <- clustered_dnms %>% filter(Reference =="T", Variant=="A")
nTA_clustered_dnm <- (nrow(TA_clustered_dnm)/ndnms)*100

TA_non_clustered_dnm <- non_clustered_dnms %>% filter(Reference =="T", Variant=="A")
nTA_non_clustered_dnm <- (nrow(TA_non_clustered_dnm)/ndnms)*100


CT_clustered_dnm <- clustered_dnms %>% filter(Reference =="C", Variant=="T")
nCT_clustered_dnm <- (nrow(CT_clustered_dnm)/ndnms)*100

CT_non_clustered_dnm <- non_clustered_dnms %>% filter(Reference =="C", Variant=="T")
nCT_non_clustered_dnm <- (nrow(CT_non_clustered_dnm)/ndnms)*100


CG_clustered_dnm <- clustered_dnms %>% filter(Reference =="C", Variant=="G")
nCG_clustered_dnm <- (nrow(CG_clustered_dnm)/ndnms)*100

CG_non_clustered_dnm <- non_clustered_dnms %>% filter(Reference =="C", Variant=="G")
nCG_non_clustered_dnm <- (nrow(CG_non_clustered_dnm)/ndnms)*100


CA_clustered_dnm <- clustered_dnms %>% filter(Reference =="C", Variant=="A")
nCA_clustered_dnm <- (nrow(CA_clustered_dnm)/ndnms)*100

CA_non_clustered_dnm <- non_clustered_dnms %>% filter(Reference =="C", Variant=="A")
nCA_non_clustered_dnm <- (nrow(CA_non_clustered_dnm)/ndnms)*100





#Making a data frame, adding all the data together
clustered <- c("Clustered", "Clustered", "Clustered", "Clustered", "Clustered", "Clustered", "Non-clustered", "Non-clustered", "Non-clustered", "Non-clustered", "Non-clustered", "Non-clustered")
dnm_var_clustered <- c("TG", "CA", "TA", "TC", "CT", "CG") #First column: what nuc. change

dnm_numbers_var_clustered <- c(nTG_clustered_dnm, nCA_clustered_dnm, nTA_clustered_dnm, nTC_clustered_dnm, nCT_clustered_dnm, nCG_clustered_dnm, nTG_non_clustered_dnm, nCA_non_clustered_dnm, nTA_non_clustered_dnm, nTC_non_clustered_dnm, nCT_non_clustered_dnm, nCG_non_clustered_dnm) #Second column: how many of this change

df_var_dnm_clustered <- data.frame(dnm_var_clustered, dnm_numbers_var_clustered, clustered) #Add them together in a file

df_var_dnm_fig_clustered <- ggplot(data=df_var_dnm_clustered, aes(dnm_var_clustered, dnm_numbers_var_clustered, fill=clustered))+ 
  geom_bar(stat="identity", position=position_dodge())+
  theme_classic()



#ab <- nTG_clustered_dnm+nCA_clustered_dnm+nTA_clustered_dnm+nTC_clustered_dnm+nCT_clustered_dnm+nCG_clustered_dnm+nTG_non_clustered_dnm+nCA_non_clustered_dnm+nTA_non_clustered_dnm+nTC_non_clustered_dnm+nCT_non_clustered_dnm+nCG_non_clustered_dnm #42% in total, somewhere, some DNMs are disappearing!
