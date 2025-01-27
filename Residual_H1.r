####################################################################
###
###   Residual_H1.R
###   
###    
###   
###   M. Brinkerhoff  * UCSC  * 2024-03-07 (Th)
###   
####################################################################

### install packages if not yet installed
packages <- c("lme4","lmerTest","tidyverse","viridis", "itsadug", 
              "reshape2", "JWileymisc", "lmtest")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

### load required packages
library(viridis)
library(knitr)
library(tidyverse)
library(itsadug)
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)
library(reshape2)
library(lme4)
library(lmerTest)
library(lmtest)
library(ggsignif)
library(rmarkdown)
library(readr)
library(MuMIn)
library(fastDummies)
library(extraoperators)
library(JWileymisc)
library(multilevelTools)


### loading in the data

df <- read.csv("Voice_Master_Split.csv", header = TRUE)

### Preprocessing the data
#rename laryngealized with rearticulated. 
df['Phonation'][df['Phonation'] == "laryngealized"] <- "rearticulated"

### convert certain columns into factors.
df$Phonation <- factor(df$Phonation, levels = c("modal", 
                                                "breathy", 
                                                "checked", 
                                                "rearticulated"))
df$Speaker <- df$Speaker %>% factor()
df$Word <- df$Word %>% factor()
df$Vowel <- df$Vowel %>% factor()
df$Iter <- df$Iter %>% factor()
df$Tone <- df$Tone %>% factor()

### Rename seg_End
colnames(df)[colnames(df) == 'seg_End'] <- 'Duration'

### Rearranging values for analysis and comparison
df <- df %>% 
  mutate(idnum = row_number())

#### h1h2c

df_h1h2c <- df %>% 
  select(Speaker, 
         Word, 
         Iter, 
         Vowel, 
         Phonation, 
         Tone, 
         Duration,
         idnum,
         H1H2c_means001,
         H1H2c_means002,
         H1H2c_means003,
         H1H2c_means004,
         H1H2c_means005,
         H1H2c_means006,
         H1H2c_means007,
         H1H2c_means008,
         H1H2c_means009,
         H1H2c_means010)

df_h1h2c_trans <- melt(df_h1h2c, id = c("Speaker",
                                        "Word", 
                                        "Iter", 
                                        "Vowel", 
                                        "Phonation", 
                                        'Tone', 
                                        "Duration",
                                        "idnum"))

df_h1h2c_trans$time <- str_sub(df_h1h2c_trans$variable,-2,-1)
df_h1h2c_trans <-  df_h1h2c_trans %>%
  rename(h1h2c = value) %>%
  select(-variable)

#### h1c
df_h1c <-  df %>%
  select(idnum, H1c_means001, H1c_means002, H1c_means003,
         H1c_means004, H1c_means005, H1c_means006,
         H1c_means007, H1c_means008, H1c_means009,
         H1c_means010)

df_h1c_trans  <-  melt(df_h1c, id = c("idnum"))
df_h1c_trans$time  <-  str_sub(df_h1c_trans$variable,-2,-1)
df_h1c_trans <-  df_h1c_trans %>%
  rename(h1c = value)%>%
  select(-variable)

#####  h2h4c
df_h2h4c <-  df %>%
  select(idnum, H2H4c_means001, H2H4c_means002, H2H4c_means003,
         H2H4c_means004, H2H4c_means005, H2H4c_means006,
         H2H4c_means007, H2H4c_means008, H2H4c_means009,
         H2H4c_means010)

df_h2h4c_trans  <-  melt(df_h2h4c, id = c("idnum"))
df_h2h4c_trans$time  <-  str_sub(df_h2h4c_trans$variable,-2,-1)
df_h2h4c_trans <-  df_h2h4c_trans %>%
  rename(h2h4c = value)%>%
  select(-variable)

##### h1a1c
df_h1a1c <-  df %>%
  select(idnum, H1A1c_means001, H1A1c_means002, H1A1c_means003,
         H1A1c_means004, H1A1c_means005, H1A1c_means006,
         H1A1c_means007, H1A1c_means008, H1A1c_means009,
         H1A1c_means010)

df_h1a1c_trans  <-  melt(df_h1a1c, id = c("idnum"))
df_h1a1c_trans$time  <-  str_sub(df_h1a1c_trans$variable,-2,-1)
df_h1a1c_trans <-  df_h1a1c_trans %>%
  rename(h1a1c = value)%>%
  select(-variable)

#### h1a2c
df_h1a2c <-  df %>%
  select(idnum, H1A2c_means001, H1A2c_means002, H1A2c_means003,
         H1A2c_means004, H1A2c_means005, H1A2c_means006,
         H1A2c_means007, H1A2c_means008, H1A2c_means009,
         H1A2c_means010)

df_h1a2c_trans  <-  melt(df_h1a2c, id = c("idnum"))
df_h1a2c_trans$time  <-  str_sub(df_h1a2c_trans$variable,-2,-1)
df_h1a2c_trans <-  df_h1a2c_trans %>%
  rename(h1a2c = value)%>%
  select(-variable)

#### h1a3c
df_h1a3c <-  df %>%
  select(idnum, H1A3c_means001, H1A3c_means002, H1A3c_means003,
         H1A3c_means004, H1A3c_means005, H1A3c_means006,
         H1A3c_means007, H1A3c_means008, H1A3c_means009,
         H1A3c_means010)

df_h1a3c_trans  <-  melt(df_h1a3c, id = c("idnum"))
df_h1a3c_trans$time  <-  str_sub(df_h1a3c_trans$variable,-2,-1)
df_h1a3c_trans <-  df_h1a3c_trans %>%
  rename(h1a3c = value)%>%
  select(-variable)

#### h2h4c
df_h2h4c <-  df %>%
  select(idnum, H2H4c_means001, H2H4c_means002, H2H4c_means003,
         H2H4c_means004, H2H4c_means005, H2H4c_means006,
         H2H4c_means007, H2H4c_means008, H2H4c_means009,
         H2H4c_means010)

df_h2h4c_trans  <-  melt(df_h2h4c, id = c("idnum"))
df_h2h4c_trans$time  <-  str_sub(df_h2h4c_trans$variable,-2,-1)
df_h2h4c_trans <-  df_h2h4c_trans %>%
  rename(h2h4c = value)%>%
  select(-variable)

#### h42Kc
df_h42Kc <-  df %>%
  select(idnum, H42Kc_means001, H42Kc_means002, H42Kc_means003,
         H42Kc_means004, H42Kc_means005, H42Kc_means006,
         H42Kc_means007, H42Kc_means008, H42Kc_means009,
         H42Kc_means010)

df_h42Kc_trans  <-  melt(df_h42Kc, id = c("idnum"))
df_h42Kc_trans$time  <-  str_sub(df_h42Kc_trans$variable,-2,-1)
df_h42Kc_trans <-  df_h42Kc_trans %>%
  rename(h42Kc = value)%>%
  select(-variable)

#### h2Kh5Kc
df_h2Kh5Kc <-  df %>%
  select(idnum, H2KH5Kc_means001, H2KH5Kc_means002, H2KH5Kc_means003,
         H2KH5Kc_means004, H2KH5Kc_means005, H2KH5Kc_means006,
         H2KH5Kc_means007, H2KH5Kc_means008, H2KH5Kc_means009,
         H2KH5Kc_means010)

df_h2Kh5Kc_trans  <-  melt(df_h2Kh5Kc, id = c("idnum"))
df_h2Kh5Kc_trans$time  <-  str_sub(df_h2Kh5Kc_trans$variable,-2,-1)
df_h2Kh5Kc_trans <-  df_h2Kh5Kc_trans %>%
  rename(h2Kh5Kc = value)%>%
  select(-variable)

#### cpp
df_cpp <-  df %>%
  select(idnum, CPP_means001, CPP_means002, CPP_means003,
         CPP_means004, CPP_means005, CPP_means006,
         CPP_means007, CPP_means008, CPP_means009,
         CPP_means010)

df_cpp_trans  <-  melt(df_cpp, id = c("idnum"))
df_cpp_trans$time  <-  str_sub(df_cpp_trans$variable,-2,-1)
df_cpp_trans <-  df_cpp_trans %>%
  rename(cpp = value)%>%
  select(-variable)

#### energy
df_energy <-  df %>%
  select(idnum, Energy_means001, Energy_means002, Energy_means003,
         Energy_means004, Energy_means005, Energy_means006,
         Energy_means007, Energy_means008, Energy_means009,
         Energy_means010)

df_energy_trans  <-  melt(df_energy, id = c("idnum"))
df_energy_trans$time  <-  str_sub(df_energy_trans$variable,-2,-1)
df_energy_trans <-  df_energy_trans %>%
  rename(energy = value)%>%
  select(-variable)

#### hnr05
df_hnr05 <-  df %>%
  select(idnum, HNR05_means001, HNR05_means002, HNR05_means003,
         HNR05_means004, HNR05_means005, HNR05_means006,
         HNR05_means007, HNR05_means008, HNR05_means009,
         HNR05_means010)

df_hnr05_trans  <-  melt(df_hnr05, id = c("idnum"))
df_hnr05_trans$time  <-  str_sub(df_hnr05_trans$variable,-2,-1)
df_hnr05_trans <-  df_hnr05_trans %>%
  rename(hnr05 = value)%>%
  select(-variable)

#### hnr15
df_hnr15 <-  df %>%
  select(idnum, HNR15_means001, HNR15_means002, HNR15_means003,
         HNR15_means004, HNR15_means005, HNR15_means006,
         HNR15_means007, HNR15_means008, HNR15_means009,
         HNR15_means010)

df_hnr15_trans  <-  melt(df_hnr15, id = c("idnum"))
df_hnr15_trans$time  <-  str_sub(df_hnr15_trans$variable,-2,-1)
df_hnr15_trans <-  df_hnr15_trans %>%
  rename(hnr15 = value)%>%
  select(-variable)

#### hnr25
df_hnr25 <-  df %>%
  select(idnum, HNR25_means001, HNR25_means002, HNR25_means003,
         HNR25_means004, HNR25_means005, HNR25_means006,
         HNR25_means007, HNR25_means008, HNR25_means009,
         HNR25_means010)

df_hnr25_trans  <-  melt(df_hnr25, id = c("idnum"))
df_hnr25_trans$time  <-  str_sub(df_hnr25_trans$variable,-2,-1)
df_hnr25_trans <-  df_hnr25_trans %>%
  rename(hnr25 = value)%>%
  select(-variable)

#### hnr35
df_hnr35 <-  df %>%
  select(idnum, HNR35_means001, HNR35_means002, HNR35_means003,
         HNR35_means004, HNR35_means005, HNR35_means006,
         HNR35_means007, HNR35_means008, HNR35_means009,
         HNR35_means010)

df_hnr35_trans  <-  melt(df_hnr35, id = c("idnum"))
df_hnr35_trans$time  <-  str_sub(df_hnr35_trans$variable,-2,-1)
df_hnr35_trans <-  df_hnr35_trans %>%
  rename(hnr35 = value)%>%
  select(-variable)

#### strF0
df_strF0 <-  df %>%
  select(idnum, strF0_means001, strF0_means002, strF0_means003,
         strF0_means004, strF0_means005, strF0_means006,
         strF0_means007, strF0_means008, strF0_means009,
         strF0_means010)

df_strF0_trans  <-  melt(df_strF0, id = c("idnum"))
df_strF0_trans$time  <-  str_sub(df_strF0_trans$variable,-2,-1)
df_strF0_trans <-  df_strF0_trans %>%
  rename(strF0 = value)%>%
  select(-variable)

#### sF1
df_sF1 <-  df %>%
  select(idnum, sF1_means001, sF1_means002, sF1_means003,
         sF1_means004, sF1_means005, sF1_means006,
         sF1_means007, sF1_means008, sF1_means009,
         sF1_means010)

df_sF1_trans  <-  melt(df_sF1, id = c("idnum"))
df_sF1_trans$time  <-  str_sub(df_sF1_trans$variable,-2,-1)
df_sF1_trans <-  df_sF1_trans %>%
  rename(sF1 = value)%>%
  select(-variable)

#### sF2
df_sF2 <-  df %>%
  select(idnum, sF2_means001, sF2_means002, sF2_means003,
         sF2_means004, sF2_means005, sF2_means006,
         sF2_means007, sF2_means008, sF2_means009,
         sF2_means010)

df_sF2_trans  <-  melt(df_sF2, id = c("idnum"))
df_sF2_trans$time  <-  str_sub(df_sF2_trans$variable,-2,-1)
df_sF2_trans <-  df_sF2_trans %>%
  rename(sF2 = value)%>%
  select(-variable)

#### sB1
df_sB1 <-  df %>%
  select(idnum, sB1_means001, sB1_means002, sB1_means003,
         sB1_means004, sB1_means005, sB1_means006,
         sB1_means007, sB1_means008, sB1_means009,
         sB1_means010)

df_sB1_trans  <-  melt(df_sB1, id = c("idnum"))
df_sB1_trans$time  <-  str_sub(df_sB1_trans$variable,-2,-1)
df_sB1_trans <-  df_sB1_trans %>%
  rename(sB1 = value)%>%
  select(-variable)

#### sB2
df_sB2 <-  df %>%
  select(idnum, sB2_means001, sB2_means002, sB2_means003,
         sB2_means004, sB2_means005, sB2_means006,
         sB2_means007, sB2_means008, sB2_means009,
         sB2_means010)

df_sB2_trans  <-  melt(df_sB2, id = c("idnum"))
df_sB2_trans$time  <-  str_sub(df_sB2_trans$variable,-2,-1)
df_sB2_trans <-  df_sB2_trans %>%
  rename(sB2 = value)%>%
  select(-variable)

#### soe
df_soe <-  df %>%
  select(idnum, soe_means001, soe_means002, soe_means003,
         soe_means004, soe_means005, soe_means006,
         soe_means007, soe_means008, soe_means009,
         soe_means010)

df_soe_trans  <-  melt(df_soe, id = c("idnum"))
df_soe_trans$time  <-  str_sub(df_soe_trans$variable,-2,-1)
df_soe_trans <-  df_soe_trans %>%
  rename(soe = value)%>%
  select(-variable)

### merging
df_trans <-  list(df_h1h2c_trans,
                  df_h1c_trans,
                  df_h2h4c_trans,
                  df_h42Kc_trans,
                  df_h2Kh5Kc_trans,
                  df_h1a1c_trans,
                  df_h1a2c_trans,
                  df_h1a3c_trans,
                  df_cpp_trans,
                  df_hnr05_trans,
                  df_hnr15_trans,
                  df_hnr25_trans,
                  df_hnr35_trans,
                  df_strF0_trans,
                  df_sF1_trans,
                  df_sF2_trans,
                  df_sB1_trans,
                  df_sB2_trans,
                  df_energy_trans,
                  df_soe_trans) %>% reduce(merge, by = c("idnum","time"))

### Remove outliers by F0, Formants, and Energy
#strF0 outlier flag
df_fil = df_trans %>%
  group_by(Speaker) %>%
  mutate(strF0z = (strF0 - mean(strF0, na.rm = T))/sd(strF0, na.rm = T)) %>%
  ungroup()

df_fil = df_fil %>%
  mutate(str_outlier = if_else(abs(strF0z) > 3, "outlier", "OK"))

### Formant outlier flagging
### Calculate Mahalanobis distance for formants
vmahalanobis = function (dat) {
  if (nrow(dat) < 25) {
    dat$zF1F2 = NA
    return(dat)
  }
  means = c(mean(dat$sF1, na.rm=T), mean(dat$sF2, na.rm=T))
  cov = cov(cbind(dat$sF1, dat$sF2))
  
  dat$zF1F2 = mahalanobis(cbind(dat$sF1, dat$sF2),
                          center=means, cov=cov)
  dat
}

# Distance larger than 6 is considered as outlier
distance_cutoff = 6

# Perform Mahalanobis on dataset
df_fil =  df_fil %>%
  group_by(Vowel) %>%
  do(vmahalanobis(.)) %>%
  ungroup() %>%
  mutate(formant_outlier = NA)

# Visualize the formants with flagged values
df_fil %>% 
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = sF2, y = sF1, color = zF1F2 > distance_cutoff)) +
  geom_point(size = 0.6) +
  facet_wrap(.~Vowel)+
  scale_y_reverse(limits = c(2000,0),position = "right") + 
  scale_x_reverse(limits = c(3500,0),position = "top")+
  theme_bw()

for (i in 1:nrow(df_fil)) {
  if (!is.na(df_fil$zF1F2[i])) {
    if (df_fil$zF1F2[i] > distance_cutoff){
      df_fil$formant_outlier[i] = "outlier"
    }
  }
}

# Visualize the vowel formant after exclusion
df_fil %>% 
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = sF2, y = sF1)) +
  geom_point(size = 0.6) +
  #geom_text()+
  facet_wrap(.~Vowel)+
  #geom_density_2d() +
  #  scale_color_manual(values=c('#a6611a','#dfc27d','#018571'))+
  scale_y_reverse(limits = c(2000,0),position = "right") + 
  scale_x_reverse(limits = c(3500,0),position = "top")+
  theme_bw()

df_fil <- df_fil %>%
  filter(str_outlier == "OK")

df_fil <- df_fil %>% 
  filter(is.na(formant_outlier))

# removing energy outliers
df_fil$energy[df_fil$energy == 0] <- NA

df_fil <- df_fil %>%
  mutate(energy = log10(energy))

df_fil <- df_fil %>% filter(!is.na(energy))

### Standardization of measurements for ease of comparison across different 
### measures

df_fil <- df_fil %>% group_by(Speaker) %>%
  mutate(h1cz = (h1c - mean(h1c, na.rm = T))/sd(h1c, na.rm = T),
         h1h2cz = (h1h2c - mean(h1h2c, na.rm = T))/sd(h1h2c, na.rm = T),
         h2h4cz = (h2h4c - mean(h2h4c, na.rm = T))/sd(h2h4c, na.rm = T),
         h42Kcz = (h42Kc - mean(h42Kc, na.rm = T))/sd(h42Kc, na.rm = T),
         h2Kh5Kcz = (h2Kh5Kc - mean(h2Kh5Kc, na.rm = T))/sd(h2Kh5Kc, na.rm = T),
         h1a1cz = (h1a1c - mean(h1a1c, na.rm = T))/sd(h1a1c, na.rm = T),
         h1a2cz = (h1a2c - mean(h1a2c, na.rm = T))/sd(h1a2c, na.rm = T),
         h1a3cz = (h1a3c - mean(h1a3c, na.rm = T))/sd(h1a3c, na.rm = T),
         energyz = (energy - mean(energy, na.rm = T))/sd(energy, na.rm = T),
         hnr05z = (hnr05 - mean(hnr05, na.rm = T))/sd(hnr05, na.rm = T),
         hnr15z = (hnr15 - mean(hnr15, na.rm = T))/sd(hnr15, na.rm = T),
         hnr25z = (hnr25 - mean(hnr25, na.rm = T))/sd(hnr25, na.rm = T),
         hnr35z = (hnr35 - mean(hnr35, na.rm = T))/sd(hnr35, na.rm = T),
         cppz = (cpp - mean(cpp, na.rm = T))/sd(cpp, na.rm = T),
         f0z = (strF0 - mean(strF0, na.rm = T))/sd(strF0, na.rm = T),
         f1z = (sF1 - mean(sF1, na.rm = T))/sd(sF1, na.rm = T),
         f2z = (sF2 - mean(sF2, na.rm = T))/sd(sF2, na.rm = T),
         b1z = (sB1 - mean(sB1, na.rm = T))/sd(sB1, na.rm = T),
         b2z = (sB2 - mean(sB2, na.rm = T))/sd(sB2, na.rm = T)
  ) %>%
  mutate(log.soe = log10(soe+0.001),
         m.log.soe = mean(log.soe, na.rm=T),
         sd.log.soe = sd(log.soe, na.rm=T),
         z.log.soe = (log.soe-m.log.soe)/sd.log.soe,
         max.soe = max(log.soe),
         min.soe = min(log.soe),) %>%
  mutate(norm.soe = (log.soe-min.soe)/(max.soe-min.soe),) %>%
  select(-c(log.soe,
            m.log.soe,
            sd.log.soe,
            z.log.soe,
            max.soe,
            min.soe)) %>% 
  ungroup()

### Adding variable to sort the data into initial, middle, and end of the
### vowels
df_fil <- df_fil %>% 
  mutate(Position = case_when(time < "04" ~ 1,
                              time < "07" ~ 2,
                              time <= "10" ~ 3))
df_fil$Position <- df_fil$Position %>% factor()

### Calculating Residual h1
#### Generate the lmer model for residual h1
model.position.h1c.covariant <- lmer(h1cz ~ energyz + 
                                       (energyz||Speaker),
                                     data = df_fil,
                                     REML = FALSE)

#### extract the energy factor
energy.factor <- fixef(model.position.h1c.covariant)[2]

#### generate the residual H1 score
df_fil$H1c.resid = df_fil$h1cz - df_fil$energyz * energy.factor

# Generating plots 
h1h2z_plot <- df_fil %>% ggplot(aes(x = Position,
                                    y = h1h2cz)) +
  scale_color_viridis(discrete = T) +
  labs(title = "H1*-H2* by position and phonation",
       x = "Vowel Position",
       y = "H1*-H2* (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
h1h2z_plot

h1a3z_plot <- df_fil %>% ggplot(aes(x = Position,
                                    y = h1a3cz)) +
  scale_color_viridis(discrete = T) +
  labs(title = "H1*-A3 by position and phonation",
       x = "Vowel Position",
       y = "H1*-A3 (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
h1a3z_plot

h1z_plot <- df_fil %>% ggplot(aes(x = Position,
                                  y = H1c.resid)) +
  scale_color_viridis(discrete = T) +
  labs(title = "Residual H1* by position and phonation",
       x = "Vowel Position",
       y = "Residual H1* (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
h1z_plot

### Linear regression models
h1h2_model <- lmer(h1h2cz ~ Phonation*Position + Tone + (1|Speaker:Word:Iter) + 
                     (1|Vowel),data = df_fil, REML = F)
summary(h1h2_model)

h1a3_model <- lmer(h1a3cz ~ Phonation*Position + Tone + (1|Speaker:Word:Iter) + 
                     (1|Vowel),data = df_fil, REML = F)
summary(h1a3_model)

h1_model <- lmer(H1c.resid ~ Phonation*Position + Tone + (1|Speaker:Word:Iter) + 
                   (1|Vowel),data = df_fil, REML = F)
summary(h1_model)

cpp_model <- lmer(cppz ~ Phonation*Position + Tone + (1|Speaker:Word:Iter) 
                  # + (1|Vowel)
                  ,data = df_fil)
summary(cpp_model)

hnr05_model <- lmer(hnr05z ~ Phonation*Position + Tone + (1|Speaker:Word:Iter) 
                    # + (1|Vowel)
                    ,data = df_fil)
summary(hnr05_model)

hnr15_model <- lmer(hnr15z ~ Phonation*Position + Tone + (1|Speaker:Word:Iter)
                    # + (1|Vowel)
                    ,data = df_fil)
summary(hnr15_model)

hnr25_model <- lmer(hnr25z ~ Phonation*Position + Tone + (1|Speaker:Word:Iter) 
                    # + (1|Vowel)
                    ,data = df_fil)
summary(hnr25_model)

hnr35_model <- lmer(hnr35z ~ Phonation*Position + Tone + (1|Speaker:Word:Iter) 
                    # + (1|Vowel)
                    ,data = df_fil)
summary(hnr35_model)

### Model comparisons
### Models with a higher loglikelihood and lower AIC indicate a better model fit

anova(h1h2_model,h1_model,h1a3_model)

lrtest(h1h2_model,h1_model)
lrtest(h1a3_model,h1_model)

AIC(h1h2_model)
AIC(h1a3_model)
AIC(h1_model)

### Calculating the effect size of the model
r2_h1 <- r.squaredGLMM(h1_model)
r2_h1h2 <- r.squaredGLMM(h1h2_model)
r2_h1a3 <- r.squaredGLMM(h1a3_model)

test_h1 <- modelTest(h1_model)

r2_h1
r2_h1h2
r2_h1a3

anova(h1_model)

# Generating plots by speaker
h1h2z_plot_speaker <- df_fil %>% ggplot(aes(x = Position,
                                    y = h1h2cz)) +
  scale_color_viridis(discrete = T) +
  labs(title = "H1*-H2* by position and phonation",
       x = "Vowel Position",
       y = "H1*-H2* (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  facet_wrap(.~Speaker)+
  theme_bw()
h1h2z_plot_speaker

h1a3z_plot_speaker <- df_fil %>% ggplot(aes(x = Position,
                                    y = h1a3cz)) +
  scale_color_viridis(discrete = T) +
  labs(title = "H1*-A3 by position and phonation",
       x = "Vowel Position",
       y = "H1*-A3 (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  facet_wrap(.~Speaker)+
  theme_bw()
h1a3z_plot_speaker

h1z_plot_speaker <- df_fil %>% ggplot(aes(x = Position,
                                  y = H1c.resid)) +
  scale_color_viridis(discrete = T) +
  labs(title = "Residual H1* by position and phonation",
       x = "Vowel Position",
       y = "Residual H1* (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  facet_wrap(.~Speaker)+
  theme_bw()
h1z_plot_speaker

cpp_plot_speaker <- df_fil %>% ggplot(aes(x = Position,
                                          y = cppz)) +
  scale_color_viridis(discrete = T) +
  labs(title = "CPP by position and phonation",
       x = "Vowel Position",
       y = "CPP (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  facet_wrap(.~Speaker)+
  theme_bw()
cpp_plot_speaker

cpp_plot <- df_fil %>% ggplot(aes(x = Position,
                                          y = cppz)) +
  scale_color_viridis(discrete = T) +
  labs(title = "CPP by position and phonation",
       x = "Vowel Position",
       y = "CPP (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  # facet_wrap(.~Speaker)+
  theme_bw()
cpp_plot

## Loess smooths

h1h2_line <- df_fil %>% 
  ggplot(aes(x = time, 
             y=h1h2cz, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess") +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "H1*-H2* measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "H1*-H2* (normalized)") +
  theme_bw()
h1h2_line

h1a3_line <- df_fil %>% 
  ggplot(aes(x = time, 
             y = h1a3cz, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess") +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "H1*-A3 measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "H1*-A3 (normalized)") +
  theme_bw()
h1a3_line

h1_line <- df_fil %>% 
  ggplot(aes(x = time, 
             y = H1c.resid, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess") +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "Residual H1* measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "Residual H1* (normalized)") +
  theme_bw()
h1_line

ggsave(filename = "H1_line.png",
       plot = h1_line,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "H1A3_line.png",
       plot = h1a3_line,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "H1H2_line.png",
       plot = h1h2_line,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

### Checking gender effects
df_fil <- df_fil %>% 
  mutate(Gender = if_else(str_detect(Speaker, 'f'), "Female", "Male"))

df_fil$Gender.f <- df_fil$Gender %>% factor()

#creating the contrast matrix manually by modifying the dummy coding scheme
(c <-contr.treatment(2))
my.coding <- matrix(rep(1/2, 2), ncol=1)
my.simple <- c-my.coding
my.simple

contrasts(df_fil$Gender.f) <- my.simple
df_fil$Gender.f 

### model for gender effects
h1h2_model_gender <- lmer(h1h2cz~Phonation*Position + Tone + Gender + 
                          (1|Vowel) + (1|Speaker:Word:Iter), 
                         data = df_fil)
summary(h1h2_model_gender)

h1_model_gender <- lmer(H1c.resid~Phonation*Position + Tone + Gender + 
                        (1|Vowel) + (1|Speaker:Word:Iter), 
                       data = df_fil)
summary(h1_model_gender)

### Model comparisons
anova(h1_model, h1_model_gender)
anova(h1h2_model, h1h2_model_gender)

# Plotting with gender
h1_line_gender <- df_fil %>% 
  ggplot(aes(x = time, 
             y = H1c.resid, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess") +
  scale_color_viridis(discrete=TRUE) +
  facet_wrap(.~Gender)+
  labs(title = "Residual H1* measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "Residual H1* (normalized)") +
  theme_bw()
h1_line_gender

h1h2_line_gender <- df_fil %>% 
  ggplot(aes(x = time, 
             y=h1h2cz, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess") +
  scale_color_viridis(discrete=TRUE) +
  facet_wrap(.~Gender)+
  labs(title = "H1*-H2* measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "H1*-H2* (normalized)") +
  theme_bw()
h1h2_line_gender

# h1h2Latex <- kable(coef(summary(h1h2_model)),format = "latex", digits = 5)
# h1a3Latex <- kable(coef(summary(h1a3_model)),format = "latex", digits = 5)
# h1Latex <- kable(coef(summary(h1_model)),format = "latex", digits = 5)
# 
# kable(coef(summary(h1_model)),format = "markdown", digits = 32)
# 
# # Install the stargazer package if you haven't already
# install.packages("stargazer")
# 
# # Load necessary packages
# library(stargazer)
# library(lme4)
# 
# # Fit an lmer model
# model <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
# 
# # Extract coefficients and standard errors
# summary_model <- summary(model)
# coefs <- summary_model$coefficients
# se <- coefs[, "Std. Error"]
# t_values <- coefs[, "t value"]
# 
# # Create a data frame to hold the results
# results <- data.frame(
#   Estimate = coefs[, "Estimate"],
#   StdError = se,
#   tValue = t_values
# )
# 
# # Create the LaTeX table
# stargazer(as.matrix(results), type = "latex", title = "Linear Mixed-Effects Model Results",
#           single.row = TRUE, digits = 3, no.space = TRUE,
#           covariate.labels = c("Intercept", "Days"),
#           dep.var.labels.include = FALSE,
#           column.labels = c("Estimate", "Std. Error", "t Value"),
#           omit.table.layout = "n")

