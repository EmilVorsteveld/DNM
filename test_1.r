library(dplyr)

library(ggplot2)
library(cowplot)

data <- read.csv2("./data.csv") #load data from csv file
data[is.na(data)] = "DNM"

pat <- 816
nrow_total <- nrow(data)
print(nrow_total)
avg_variants <- nrow_total / pat


#data %>% select(Chromosome, Start.position, End.position, Reference, Variant, parentOfOrigin)
source("npatmat.r")

source("mut_type.r")

source("var_per_chr.r")

source("DNM_type.r")

source("DNM_distance.r")



plot_grid(df_fig, df_var_fig, nchr_fig, ndnm_fig, df_var_dnm_fig, df_var_dnm_fig_clustered) #Adding the figures into one image

