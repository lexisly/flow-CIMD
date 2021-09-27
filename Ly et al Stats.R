library(tidyverse)
library(readxl)
library(stringr)
library(ggpubr)
library(reshape2)
library(ggstance)
library(RColorBrewer)
library(networkD3)
library(moments)
library(KSgeneral)
library(effsize)
library(rcompanion)
library(ggmosaic)
library(report)
library(table1)
library(rstatix)
library(dgof)
library(nortest)
library(Hmisc)


setwd("C:/Users/lexis/Documents/Masters/Flow/Data/May 7")
#Owner Surrender --> Adopted [SMALL ANIM]
full_data<-read.csv("full_data2.csv", stringsAsFactors = FALSE)

#correlation
incoming_data<-full_data%>%
  select(incoming_RI_q, incoming_EC_q, incoming_ED_q, incoming_SV_q)%>%
  as.matrix()

rcorr(incoming_data, type = c("spearman"))

outgoing_data<-full_data%>%
  select(outgoing_RI_q, outgoing_EC_q, outgoing_ED_q, outgoing_SV_q)%>%
  as.matrix()

rcorr(outgoing_data, type = c("spearman"))

#### SMALL ANIMAL
#Situational Vulnerability
adopted_owner_sm_hist_SV<-full_data %>%
  filter(Type == "Small Animal")
  drop_na(incoming_SV_q, outgoing_SV_q) %>%
  group_by(incoming_SV_q, outgoing_SV_q)%>%
  summarise(change = incoming_SV_q-outgoing_SV_q)

#Normality
skewness(adopted_owner_sm_hist_SV$change)
kurtosis(adopted_owner_sm_hist_SV$change)
qqnorm(adopted_owner_sm_hist_SV$change)
qqline(adopted_owner_sm_hist_SV$change)

## Wilcoxon Signed Rank (non-para two sample t-test can be used to compare two independent groups of samples)
#Does median SV score for small animals between incoming and outgoing?
SV_sm<-adopted_owner_sm_final%>%
  select(Animal.ID, incoming_SV_q, outgoing_SV_q)%>%
  pivot_longer(-Animal.ID,names_to = "incoming_outgoing", values_to = "SV_q")

SV_sm %>%
  group_by(incoming_outgoing) %>%
  get_summary_stats(SV_q, type = "median_iqr")

sm_wrs <- wilcox.test(SV_q ~ incoming_outgoing, paired = TRUE, data = SV_sm,
                     exact = FALSE)

sm_SV_wilcox <- SV_sm  %>%
  wilcox_test(SV_q ~ incoming_outgoing, paired = TRUE)
as.data.frame(sm_SV_wilcox)

SV_sm %>%
  wilcox_effsize(SV_q ~ incoming_outgoing)
#Repeated for each species and CIMD factor

#Fisher Pearson Skewness - by species
data_skew<-full_species %>%
  select(incoming_EC_q, outgoing_EC_q, incoming_SV_q, outgoing_SV_q,
         incoming_ED_q, outgoing_ED_q,incoming_RI_q, outgoing_RI_q, Type)%>%
  drop_na(incoming_EC_q, outgoing_EC_q, incoming_SV_q, outgoing_SV_q,
          incoming_ED_q, outgoing_ED_q,incoming_RI_q, outgoing_RI_q)

sm_skew <- data_skew %>%
  filter(Type == 'Small Animal')
skewness(sm_skew$incoming_EC_q, na.rm = FALSE)
skewness(sm_skew$outgoing_EC_q, na.rm = FALSE)
skewness(sm_skew$incoming_SV_q, na.rm = FALSE)
skewness(sm_skew$outgoing_SV_q, na.rm = FALSE)
skewness(sm_skew$incoming_ED_q, na.rm = FALSE)
skewness(sm_skew$outgoing_ED_q, na.rm = FALSE)
skewness(sm_skew$incoming_RI_q, na.rm = FALSE)
skewness(sm_skew$outgoing_RI_q, na.rm = FALSE)

pup_skew <- data_skew %>%
  filter(Type == 'Puppy')
skewness(pup_skew$incoming_EC_q, na.rm = FALSE)
skewness(pup_skew$outgoing_EC_q, na.rm = FALSE)
skewness(pup_skew$incoming_SV_q, na.rm = FALSE)
skewness(pup_skew$outgoing_SV_q, na.rm = FALSE)
skewness(pup_skew$incoming_ED_q, na.rm = FALSE)
skewness(pup_skew$outgoing_ED_q, na.rm = FALSE)
skewness(pup_skew$incoming_RI_q, na.rm = FALSE)
skewness(pup_skew$outgoing_RI_q, na.rm = FALSE)

dog_skew <- data_skew %>%
  filter(Type == 'Dog')
skewness(dog_skew$incoming_EC_q, na.rm = FALSE)
skewness(dog_skew$outgoing_EC_q, na.rm = FALSE)
skewness(dog_skew$incoming_SV_q, na.rm = FALSE)
skewness(dog_skew$outgoing_SV_q, na.rm = FALSE)
skewness(dog_skew$incoming_ED_q, na.rm = FALSE)
skewness(dog_skew$outgoing_ED_q, na.rm = FALSE)
skewness(dog_skew$incoming_RI_q, na.rm = FALSE)
skewness(dog_skew$outgoing_RI_q, na.rm = FALSE)

kit_skew <- data_skew %>%
  filter(Type == 'Kitten')
skewness(kit_skew$incoming_EC_q, na.rm = FALSE)
skewness(kit_skew$outgoing_EC_q, na.rm = FALSE)
skewness(kit_skew$incoming_SV_q, na.rm = FALSE)
skewness(kit_skew$outgoing_SV_q, na.rm = FALSE)
skewness(kit_skew$incoming_ED_q, na.rm = FALSE)
skewness(kit_skew$outgoing_ED_q, na.rm = FALSE)
skewness(kit_skew$incoming_RI_q, na.rm = FALSE)
skewness(kit_skew$outgoing_RI_q, na.rm = FALSE)

cat_skew <- data_skew %>%
  filter(Type == 'Cat')
skewness(cat_skew$incoming_EC_q, na.rm = FALSE)
skewness(cat_skew$outgoing_EC_q, na.rm = FALSE)
skewness(cat_skew$incoming_SV_q, na.rm = FALSE)
skewness(cat_skew$outgoing_SV_q, na.rm = FALSE)
skewness(cat_skew$incoming_ED_q, na.rm = FALSE)
skewness(cat_skew$outgoing_ED_q, na.rm = FALSE)
skewness(cat_skew$incoming_RI_q, na.rm = FALSE)
skewness(cat_skew$outgoing_RI_q, na.rm = FALSE)


