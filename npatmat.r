#First figure: number of variants from either the mother of the father
paternal <- data %>% filter(parentOfOrigin == "father") #filter paternal variants from the data
npaternal <- nrow(paternal) #count the number of paternal variants
print(npaternal)

maternal <- data %>% filter(parentOfOrigin == 'mother') #filter maternal variants from the data
nmaternal <- nrow(maternal) #count the number of maternal variants
print(nmaternal)

parent <- c("Paternal", "Maternal") #concatenate paternal and maternal
numbers <- c(npaternal, nmaternal) #concatenate number of pat and mat variants

df <- data.frame(parent, numbers) #make a data frame from the numbers of variants and the parent of origin

df_fig <- ggplot(data=df, aes(x=parent, y=numbers, fill=parent))+ #make a figure of the number of pat and mat variants
  geom_bar(stat="identity")+
  ggtitle("Whole genome")+ 
  theme_classic()


