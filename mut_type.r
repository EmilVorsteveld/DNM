#Second figure: number of variant by type and parent of origin
#First, the variants are filtered on type and parent of origin
TGpaternal <- paternal %>% filter(Reference =="T", Variant=="G")
nTGpaternal <- nrow(TGpaternal)

CApaternal <- paternal %>% filter(Reference =="C", Variant=="A")
nCApaternal <- nrow(CApaternal)

TApaternal <- paternal %>% filter(Reference =="T", Variant=="A")
nTApaternal <- nrow(TApaternal)

TCpaternal <- paternal %>% filter(Reference =="T", Variant=="C")
nTCpaternal <- nrow(TCpaternal)

CTpaternal <- paternal %>% filter(Reference =="C", Variant=="T")
nCTpaternal <- nrow(CTpaternal)

CGpaternal <- paternal %>% filter(Reference =="C", Variant=="G")
nCGpaternal <- nrow(CGpaternal)


TGmaternal <- maternal %>% filter(Reference =="T", Variant=="G")
nTGmaternal <- nrow(TGmaternal)

CAmaternal <- maternal %>% filter(Reference =="C", Variant=="A")
nCAmaternal <- nrow(CAmaternal)

TAmaternal <- maternal %>% filter(Reference =="T", Variant=="A")
nTAmaternal <- nrow(TAmaternal)

TCmaternal <- maternal %>% filter(Reference =="T", Variant=="C")
nTCmaternal <- nrow(TCmaternal)

CTmaternal <- maternal %>% filter(Reference =="C", Variant=="T")
nCTmaternal <- nrow(CTmaternal)

CGmaternal <- maternal %>% filter(Reference =="C", Variant=="G")
nCGmaternal <- nrow(CGmaternal)

#Making a data frame, adding all the data together
parent_var <- c("TG", "TG", "CA", "CA", "TA", "TA", "TC", "TC", "CT", "CT", "CG", "CG")

numbers_var <- c(nTGpaternal, nTGmaternal, nCApaternal, nCAmaternal, nTApaternal, nTAmaternal, nTCpaternal, nTCmaternal, nCTpaternal, nCTmaternal, nCGpaternal,   nCGmaternal)

origin_var <- c("paternal", "maternal", "paternal", "maternal", "paternal", "maternal", "paternal", "maternal", "paternal", "maternal", "paternal", "maternal")

df_var <- data.frame(origin_var, parent_var, numbers_var)

df_var_fig <- ggplot(data=df_var, aes(factor(parent_var), numbers_var, fill=origin_var))+ #Second figure
  geom_bar(stat="identity", position=position_dodge())+
  theme_classic()