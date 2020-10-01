TGdnm <- dnms %>% filter(Reference =="T", Variant=="G")
nTGdnm <- nrow(TGdnm)

TCdnm <- dnms %>% filter(Reference =="T", Variant=="C")
nTCdnm <- nrow(TCdnm)

TAdnm <- dnms %>% filter(Reference =="T", Variant=="A")
nTAdnm <- nrow(TAdnm)

CTdnm <- dnms %>% filter(Reference =="C", Variant=="T")
nCTdnm <- nrow(CTdnm)

CGdnm <- dnms %>% filter(Reference =="C", Variant=="G")
nCGdnm <- nrow(CGdnm)

CAdnm <- dnms %>% filter(Reference =="C", Variant=="A")
nCAdnm <- nrow(CAdnm)

#Making a data frame, adding all the data together
dnm_var <- c("TG", "CA", "TA", "TC", "CT", "CG")

dnm_numbers_var <- c(nTGdnm, nCAdnm, nTAdnm, nTCdnm, nCTdnm, nCGdnm)

df_var_dnm <- data.frame(dnm_var, dnm_numbers_var)

df_var_dnm_fig <- ggplot(data=df_var_dnm, aes(dnm_var, dnm_numbers_var, fill=dnm_numbers_var))+ #Fourth figure
  geom_bar(stat="identity", position=position_dodge())+
  theme_classic()