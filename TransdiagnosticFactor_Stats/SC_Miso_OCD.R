### Script for misophonia factor analysis
### Written by Sarah Banker
### Last edited April 22nd 2022

### Load Libraries ####
library(dplyr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(plyr)
library(tidyselect)
library(janitor)
library(dabestr)
library(polycor)
library(nFactors)
library(factoextra)
library(psych)
library(data.table)
library(ggpubr)
library(plotrix)

#### Load functions ####
source("SummarySE.R")

#### Import Data ####

# Questionnaire data:
db_q  <- read_excel("Prolific_QuestionnaireData_Full.xlsx")

# Task data:
db_t  <- read_excel("misophonia_social_control.xlsx")


### Factor Analysis ####
#Set up and run factor analysis#

factor_set <- subset(db_q, select=c(stait1_r, stait2, stait3_r, stait4, stait5, stait6_r, stait7_r, stait8,
                                                    stait9, stait10_r, stait11, stait12, stait13_r, stait14_r, stait15, stait16_r,
                                                    stait17, stait18, stait19_r, stait20, ucls_1, ucls_2, ucls_3_r, ucls_4, ucls_5,
                                                    ucls_6_r, ucls_7, ucls_8, sds_1, sds_2_r, sds_3, sds_4, sds_5_r, sds_6_r, sds_7,
                                                    sds_8, sds_9, sds_10, sds_11_r, sds_12_r, sds_13, sds_14_r, sds_15, sds_16_r,
                                                    sds_17_r, sds_18_r, sds_19, sds_20_r, oci_1, oci_2, oci_3, oci_4, oci_5, oci_6,
                                                    oci_7, oci_8, oci_9, oci_10, oci_11, oci_12, oci_13, oci_14, oci_15, oci_16, oci_17,
                                                    oci_18, q1sh, q2sh, q3sh, q4sh_r, pss1, pss2_r, pss3_r, pss4, amisos_1, amisos_2,
                                                    amisos_3, amisos_4, amisos_5, amisos_6, bapq_1_r, bapq_2, bapq_3_r,
                                                    bapq_4, bapq_5, bapq_6, bapq_7_r, bapq_8, bapq_9_r, bapq_10, bapq_11, bapq_12_r,
                                                    bapq_13, bapq_14, bapq_15_r, bapq_16_r, bapq_17, bapq_18, bapq_19_r, bapq_20, bapq_21_r,
                                                    bapq_22, bapq_23_r, bapq_24, bapq_25_r, bapq_26, bapq_27, bapq_28_r, bapq_29, bapq_30_r,
                                                    bapq_31, bapq_32, bapq_33, bapq_34_r, bapq_35, bapq_36_r, lsas_av_1, lsas_av_2, lsas_av_3,
                                                    lsas_av_4, lsas_av_5, lsas_av_6, lsas_av_7, lsas_av_8, lsas_av_9, lsas_av_10, lsas_av_11,
                                                    lsas_av_12, lsas_av_13, lsas_av_14, lsas_av_15, lsas_av_16, lsas_av_17, lsas_av_18,
                                                    lsas_av_19, lsas_av_20, lsas_av_21, lsas_av_22, lsas_av_23, lsas_av_24, apdis_1_2,
                                                    apdis_2_2, apdis_3_2, apdis_4_2, apdis_5_2, apdis_6_2, apdis_7_2, apdis_8_2, zbpd_1,
                                                    zbpd_2, zbpd_3, zbpd_4, zbpd_5, zbpd_6, zbpd_7, zbpd_8, zbpd_9, zbpd_10, aes_1_r,
                                                    aes_2_r, aes_3_r, aes_4_r, aes_5_r, aes_6_n, aes_7_r, aes_8_r, aes_9_r, aes_10_n, aes_11_n,
                                                    aes_12_r, aes_13_r, aes_14_r, aes_15_r, aes_16_r, aes_17_r, aes_18_r,  eat_1_new, eat_2_new,
                                                    eat_3_new, eat_4_new, eat_5_new, eat_6_new, eat_7_new, eat_8_new, eat_9_new, eat_10_new,
                                                    eat_11_new, eat_12_new, eat_13_new, eat_14_new, eat_15_new, eat_16_new, eat_17_new, eat_18_new,
                                                    eat_19_new, eat_20_new, eat_21_new, eat_22_new, eat_23_new, eat_24_new,
                                                    eat_25_new, eat_26_new, audit_1, audit_2, audit_3, audit_4, audit_5, audit_6, audit_7, audit_8,
                                                    audit_9, audit_10))

factor_set <- as.data.frame(factor_set)
## Make a heterogeneous correlation matrix (because we have both continuous and dichotomous variables) ##
## This takes a few minutes ##

factorsetcorr <- hetcor(factor_set)
FactorCorr <- factorsetcorr[["correlations"]]

## Determine the number of factors ##

results <- nCng(FactorCorr, cor = TRUE, model = "factors", details = TRUE)
results

plotuScree(FactorCorr, main=paste(results$nFactors,
                                         " factors retained by the CNG procedure",
                                         sep=""))

ggplot(results$detail, aes(x=v, y=values)) +
  geom_bar(stat="identity") + 
  labs(x = "Components", y = "Eigenvalue") + 
  theme_classic(base_size = 20) +
  xlim(0,11)

## Run factor analysis ##
threefactor <- fa(r=FactorCorr, nfactors = 3, n.obs = 1175, rotate = "oblimin", fm="ml")
print(threefactor)

## Explore variable loadings at various cutoffs (.3 considered by many to be the cutoff for a "true" loading)
print(threefactor$loadings,cutoff = 0.20)
print(threefactor$loadings,cutoff = 0.30)

## Get scores for each participant
fs <- factor.scores(factor_set, threefactor, method = "tenBerge")
fs_scores <- fs$scores

## Save and export full dataframe for later use
db_q_scores <- cbind(db_q, fs_scores)
write.table(db_q_scores, "Factor_Analysis_Data_Full.txt", sep="\t")

#### Plot Factor Analysis Results (by item, color coded for questionnaire) ####

load = threefactor$loadings
load = load[]
load = data.frame(load)
setDT(load,keep.rownames=TRUE)[]
colnames(load)[1] <- "Indicators"

colnames(load)[2:4] <- c("Mood", "Social", "Miso-OC")
load.m <- melt(load, id="Indicators", variable.name="Factors", value.name="Loading", measure = colnames(load)[2:4])
as.data.frame(load.m)

## Label each item by the questionnaire it came from 
load.m <- mutate(load.m, Scale = ifelse(Indicators == "stait1_r" | Indicators == "stait2" | Indicators == "stait3_r" | Indicators == "stait4"
                                                      | Indicators == "stait5" | Indicators == "stait6_r" | Indicators == "stait7_r" | Indicators == "stait8"
                                                      | Indicators == "stait9" | Indicators == "stait10_r" | Indicators == "stait11" | Indicators == "stait12" 
                                                      | Indicators == "stait13_r" | Indicators == "stait14_r" |Indicators == "stait15" | Indicators == "stait16_r" 
                                                      | Indicators == "stait17" | Indicators == "stait18" | Indicators == "stait19_r" | Indicators == "stait20", "Anxiety",
                                                      ifelse(Indicators == "ucls_1" | Indicators == "ucls_2" | Indicators == "ucls_3_r" | Indicators == "ucls_4"
                                                             | Indicators == "ucls_5" | Indicators == "ucls_6_r" | Indicators == "ucls_7" | Indicators == "ucls_8", "Loneliness",
                                                             ifelse(Indicators == "sds_1" | Indicators == "sds_2_r" | Indicators == "sds_3" | Indicators == "sds_4"
                                                                    | Indicators == "sds_5_r" | Indicators == "sds_6_r" | Indicators == "sds_7" | Indicators == "sds_8"
                                                                    | Indicators == "sds_9" | Indicators == "sds_10" | Indicators == "sds_11_r" | Indicators == "sds_12_r"
                                                                    | Indicators == "sds_13" | Indicators == "sds_14_r" | Indicators == "sds_15" | Indicators == "sds_16_r"
                                                                    | Indicators == "sds_17_r" | Indicators == "sds_18_r" | Indicators == "sds_19" | Indicators == "sds_20_r", "Depression",
                                                                    ifelse(Indicators == "oci_1" | Indicators == "oci_2" | Indicators == "oci_3" | Indicators == "oci_4"
                                                                           | Indicators == "oci_5" | Indicators == "oci_6" | Indicators == "oci_7" | Indicators == "oci_8"
                                                                           | Indicators == "oci_9" | Indicators == "oci_10" | Indicators == "oci_11" | Indicators == "oci_12"
                                                                           | Indicators == "oci_13" | Indicators == "oci_14" | Indicators == "oci_15" | Indicators == "oci_16"
                                                                           | Indicators == "oci_17" | Indicators == "oci_18", "OC",
                                                                           ifelse(Indicators == "q1sh" | Indicators == "q2sh" | Indicators == "q3sh" | Indicators == "q4sh_r", "Happiness",
                                                                                  ifelse(Indicators == "pss1" | Indicators == "pss2_r" | Indicators == "pss3_r" | Indicators == "pss4", "Stress",
                                                                                         ifelse(Indicators == "amisos_1" | Indicators == "amisos_2" | Indicators == "amisos_3" | Indicators == "amisos_4"
                                                                                                | Indicators == "amisos_5" | Indicators == "amisos_6", "Misophonia",
                                                                                                ifelse(Indicators == "bapq_1_r" | Indicators == "bapq_2" | Indicators == "bapq_3_r" | Indicators == "bapq_4"
                                                                                                       | Indicators == "bapq_5" | Indicators == "bapq_6" | Indicators == "bapq_7_r" | Indicators == "bapq_8"
                                                                                                       | Indicators == "bapq_9_r" | Indicators == "bapq_10" | Indicators == "bapq_11" | Indicators == "bapq_12_r" 
                                                                                                       | Indicators == "bapq_13" | Indicators == "bapq_14" |Indicators == "bapq_15_r" | Indicators == "bapq_16_r" 
                                                                                                       | Indicators == "bapq_17" | Indicators == "bapq_18" | Indicators == "bapq_19_r" | Indicators == "bapq_20"
                                                                                                       | Indicators == "bapq_21_r" | Indicators == "bapq_22" | Indicators == "bapq_23_r" | Indicators == "bapq_24"
                                                                                                       | Indicators == "bapq_25_r"| Indicators == "bapq_26" | Indicators == "bapq_27" | Indicators == "bapq_28_r"
                                                                                                       | Indicators == "bapq_29"| Indicators == "bapq_30_r" | Indicators == "bapq_31" | Indicators == "bapq_32"
                                                                                                       | Indicators == "bapq_33"| Indicators == "bapq_34_r" | Indicators == "bapq_35" | Indicators == "bapq_36_r",
                                                                                                       "ASD",
                                                                                                       ifelse(Indicators == "lsas_av_1" | Indicators == "lsas_av_2" | Indicators == "lsas_av_3" | Indicators == "lsas_av_4"
                                                                                                              | Indicators == "lsas_av_5" | Indicators == "lsas_av_6" | Indicators == "lsas_av_7" | Indicators == "lsas_av_8"
                                                                                                              | Indicators == "lsas_av_9" | Indicators == "lsas_av_10" | Indicators == "lsas_av_11" | Indicators == "lsas_av_12" 
                                                                                                              | Indicators == "lsas_av_13" | Indicators == "lsas_av_14" |Indicators == "lsas_av_15" | Indicators == "lsas_av_16" 
                                                                                                              | Indicators == "lsas_av_17" | Indicators == "lsas_av_18" | Indicators == "lsas_av_19" | Indicators == "lsas_av_20"
                                                                                                              | Indicators == "lsas_av_21" | Indicators == "lsas_av_22" | Indicators == "lsas_av_23" | Indicators == "lsas_av_24",
                                                                                                              "Social Anxiety",
                                                                                                              ifelse(Indicators == "apdis_1_2" | Indicators == "apdis_2_2" | Indicators == "apdis_3_2" 
                                                                                                                     | Indicators == "apdis_4_2" | Indicators == "apdis_5_2" | Indicators == "apdis_6_2"
                                                                                                                     | Indicators == "apdis_7_2" | Indicators == "apdis_8_2", "AVPD",
                                                                                                                     ifelse(Indicators == "zbpd_1" | Indicators == "zbpd_2" | Indicators == "zbpd_3" 
                                                                                                                            | Indicators == "zbpd_4" | Indicators == "zbpd_5" | Indicators == "zbpd_6"
                                                                                                                            | Indicators == "zbpd_7" | Indicators == "zbpd_8"| Indicators == "zbpd_9" 
                                                                                                                            | Indicators == "zbpd_10",  "BPD",
                                                                                                                            ifelse(Indicators == "aes_1_r" | Indicators == "aes_2_r" | Indicators == "aes_3_r" | Indicators == "aes_4_r"
                                                                                                                                   | Indicators == "aes_5_r" | Indicators == "aes_6_n" | Indicators == "aes_7_r" | Indicators == "aes_8_r"
                                                                                                                                   | Indicators == "aes_9_r" | Indicators == "aes_10_n" | Indicators == "aes_11_n" | Indicators == "aes_12_r"
                                                                                                                                   | Indicators == "aes_13_r" | Indicators == "aes_14_r" | Indicators == "aes_15_r" | Indicators == "aes_16_r"
                                                                                                                                   | Indicators == "aes_17_r" | Indicators == "aes_18_r", "Apathy",
                                                                                                                                   ifelse(Indicators == "eat_1_new" | Indicators == "eat_2_new" | Indicators == "eat_3_new" | Indicators == "eat_4_new"
                                                                                                                                          | Indicators == "eat_5_new" | Indicators == "eat_6_new" | Indicators == "eat_7_new" | Indicators == "eat_8_new"
                                                                                                                                          | Indicators == "eat_9_new" | Indicators == "eat_10_new" | Indicators == "eat_11_new" | Indicators == "eat_12_new" 
                                                                                                                                          | Indicators == "eat_13_new" | Indicators == "eat_14_new" |Indicators == "eat_15_new" | Indicators == "eat_16_new" 
                                                                                                                                          | Indicators == "eat_17_new" | Indicators == "eat_18_new" | Indicators == "eat_19_new" | Indicators == "eat_20_new"
                                                                                                                                          | Indicators == "eat_21_new" | Indicators == "eat_22_new" | Indicators == "eat_23_new" | Indicators == "eat_24_new"
                                                                                                                                          | Indicators == "eat_25_new"| Indicators == "eat_26_new", "Eating Disorders",
                                                                                                                                          ifelse(Indicators == "audit_1" | Indicators == "audit_2" | Indicators == "audit_3"
                                                                                                                                                 | Indicators == "audit_4" | Indicators == "audit_5"| Indicators == "audit_6" 
                                                                                                                                                 | Indicators == "audit_7" | Indicators == "audit_8"| Indicators == "audit_9" 
                                                                                                                                                 | Indicators == "audit_10", "Alcohol Use", "NA")))))))))))))))
## Reorder the scales
load.m$Scale <- factor(load.m$Scale, levels = c("Apathy",  "Stress", "Happiness", "Depression", 
                                                "Anxiety", "BPD", "Loneliness", "Social Anxiety",
                                                "AVPD", "ASD", "Eating Disorders", "Alcohol Use",
                                                  "OC", "Misophonia"))
## Reorder the items
load.m$Indicators <- factor(load.m$Indicators, levels = c("aes_1_r", "aes_2_r", "aes_3_r", "aes_4_r", "aes_5_r", "aes_6_n", "aes_7_r", "aes_8_r",
                                                                        "aes_9_r", "aes_10_n", "aes_11_n", "aes_12_r", "aes_13_r", "aes_14_r", "aes_15_r",
                                                                        "aes_16_r", "aes_17_r", "aes_18_r",
                                                                        "pss1", "pss2_r", "pss3_r", "pss4",
                                                                        "q1sh", "q2sh", "q3sh", "q4sh_r",
                                                                        "sds_1", "sds_2_r", "sds_3", "sds_4", "sds_5_r", "sds_6_r", "sds_7",
                                                                        "sds_8", "sds_9", "sds_10", "sds_11_r", "sds_12_r", "sds_13", "sds_14_r", "sds_15", "sds_16_r",
                                                                        "sds_17_r", "sds_18_r", "sds_19", "sds_20_r",
                                                                        "stait1_r", "stait2", "stait3_r", "stait4", "stait5", "stait6_r", "stait7_r", "stait8",
                                                                        "stait9", "stait10_r", "stait11", "stait12", "stait13_r", "stait14_r", "stait15", "stait16_r",
                                                                        "stait17", "stait18", "stait19_r", "stait20",
                                                                        "ucls_1", "ucls_2", "ucls_3_r", "ucls_4", "ucls_5", "ucls_6_r", "ucls_7", "ucls_8",
                                                                        "zbpd_1",  "zbpd_2", "zbpd_3", "zbpd_4", "zbpd_5", "zbpd_6", "zbpd_7", "zbpd_8", "zbpd_9", "zbpd_10",
                                                                        "lsas_av_1", "lsas_av_2", "lsas_av_3", "lsas_av_4", "lsas_av_5", "lsas_av_6", "lsas_av_7",
                                                                        "lsas_av_8", "lsas_av_9", "lsas_av_10", "lsas_av_11", "lsas_av_12", "lsas_av_13", "lsas_av_14",
                                                                        "lsas_av_15", "lsas_av_16", "lsas_av_17", "lsas_av_18", "lsas_av_19", "lsas_av_20",
                                                                        "lsas_av_21", "lsas_av_22", "lsas_av_23", "lsas_av_24",
                                                                        "apdis_1_2", "apdis_2_2", "apdis_3_2", "apdis_4_2", "apdis_5_2", "apdis_6_2", "apdis_7_2",
                                                                        "apdis_8_2",
                                                                        "bapq_1_r", "bapq_2", "bapq_3_r",
                                                                        "bapq_4", "bapq_5", "bapq_6", "bapq_7_r", "bapq_8", "bapq_9_r", "bapq_10", "bapq_11", "bapq_12_r",
                                                                        "bapq_13", "bapq_14", "bapq_15_r", "bapq_16_r", "bapq_17", "bapq_18", "bapq_19_r", "bapq_20", "bapq_21_r",
                                                                        "bapq_22", "bapq_23_r", "bapq_24", "bapq_25_r", "bapq_26", "bapq_27", "bapq_28_r", "bapq_29", "bapq_30_r",
                                                                        "bapq_31", "bapq_32", "bapq_33", "bapq_34_r", "bapq_35", "bapq_36_r",
                                                                        "eat_1_new", "eat_2_new",
                                                                        "eat_3_new", "eat_4_new", "eat_5_new", "eat_6_new", "eat_7_new", "eat_8_new", "eat_9_new", "eat_10_new",
                                                                        "eat_11_new", "eat_12_new", "eat_13_new", "eat_14_new", "eat_15_new", "eat_16_new", "eat_17_new", "eat_18_new",
                                                                        "eat_19_new", "eat_20_new", "eat_21_new", "eat_22_new", "eat_23_new", "eat_24_new",
                                                                        "eat_25_new", "eat_26_new",
                                                                        "audit_1", "audit_2", "audit_3", "audit_4", "audit_5", "audit_6", "audit_7", "audit_8",
                                                                        "audit_9", "audit_10",
                                                                        "oci_1", "oci_2", "oci_3", "oci_4", "oci_5", "oci_6",
                                                                        "oci_7", "oci_8", "oci_9", "oci_10", "oci_11", "oci_12", "oci_13", "oci_14", "oci_15", "oci_16", "oci_17",
                                                                        "oci_18",
                                                                        "amisos_1", "amisos_2", "amisos_3", "amisos_4", "amisos_5", "amisos_6"))





# Create a color palette 
Palette1 <- c("#53853c", "#f23c21", "#ff9600", "#0056ec", "#ba76c7", "#4ad0c4", "#d144bf", "#ffe536", 
              "grey62", "#ff677d", "black", "#7348cf", "#7ad847", "#99e6fb", "#ffd1d3", "#d9c395",
              "#74d78f", "#4766de", "#ff8b71", "#151503") 

load_mood <- subset(load.m, Factors == "Mood")
load_social <- subset(load.m, Factors == "Social")
load_miso_oc<- subset(load.m, Factors == "Miso-OC")

## Check mean and sd factor loadings for each scale, to make table for Figure 3
ddply(load_mood, .(Scale), summarize,  MeanLoad=mean(Loading))
ddply(load_social, .(Scale), summarize,  MeanLoad=mean(Loading))
ddply(load_miso_oc, .(Scale), summarize,  MeanLoad=mean(Loading))

ddply(load_mood, .(Scale), summarize,  SdLoad=sd(Loading))
ddply(load_social, .(Scale), summarize,  SdLoad=sd(Loading))
ddply(load_miso_oc, .(Scale), summarize,  SdLoad=sd(Loading))

p1 <- ggplot(load_mood, aes(x=Indicators, y=Loading, fill=Scale, label = FALSE)) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values = Palette1) +
  scale_y_continuous(limits = c(-.85, .85), breaks=c(-0.6, -0.3, 0, 0.3, 0.6)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p2 <- ggplot(load_social, aes(x=Indicators, y=Loading, fill=Scale, label = FALSE)) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values = Palette1) +
  scale_y_continuous(limits = c(-.85, .85), breaks=c(-0.6, -0.3, 0, 0.3, 0.6)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p3 <- ggplot(load_miso_oc, aes(x=Indicators, y=Loading, fill=Scale, label = FALSE)) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values = Palette1) +
  scale_y_continuous(limits = c(-.85, .85), breaks=c(-0.6, -0.3, 0, 0.3, 0.6)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


figure <- ggarrange(p1, p2, p3,
                    labels = c("F1", "F2", "F3"),
                    ncol = 1, nrow = 3,
                    common.legend = TRUE, legend = "right")
figure

#### Task Analyses ####

# Combine task data with output from above to get full data:
db_full <- merge(db_q_scores, db_t, by="prolific_id")

# Convert "perceived control" variables into percentages
db_full <- mutate(db_full, pc_ic = pc_ic/100)
db_full <- mutate(db_full, pc_nc = pc_nc/100)

#### Split into two groups based on factor 1 ####

top <- quantile(db_full$ML1, 0.75) #Top quartile
bottom <- quantile(db_full$ML1, 0.25) #Bottom quartile

db_full <- mutate(db_full, mood_top = ifelse(ML1 > top, 1, 0 ))  #Top quartile
db_full <- mutate(db_full, mood_bottom = ifelse(ML1 < bottom, 1, 0)) #Bottom quartile

#How many participants per group?
sum(db_full$mood_top == 1) 
sum(db_full$mood_bottom == 1) 

#Make one "group" variable for high/low factor 3 scores
db_full <- mutate(db_full, mood_group = ifelse(mood_top == 1, 1, 
                                           ifelse(mood_bottom == 1, 0, NA)))

#Subset the data to take only those in the top and bottom quartile 
#(e.g., only participants defined in the grouping variable)
db_full_f1 <- db_full[!is.na(db_full$mood_group),]
db_full_f1 <- mutate(db_full_f1, mood_group = as.factor(db_full_f1$mood_group))

#Double check that we still have the right n in each group after subsetting
sum(db_full_f1$mood_top == 1) 
sum(db_full_f1$mood_bottom == 1) 

#### Split into two groups based on factor 2 ####

top <- quantile(db_full$ML2, 0.75) #Top quartile
bottom <- quantile(db_full$ML2, 0.25) #Bottom quartile

db_full <- mutate(db_full, social_top = ifelse(ML2 > top, 1, 0 ))  #Top quartile
db_full <- mutate(db_full, social_bottom = ifelse(ML2 < bottom, 1, 0)) #Bottom quartile

#How many participants per group?
sum(db_full$social_top == 1) 
sum(db_full$social_bottom == 1) 

#Make one "group" variable for high/low factor 3 scores
db_full <- mutate(db_full, social_group = ifelse(social_top == 1, 1, 
                                             ifelse(social_bottom == 1, 0, NA)))

#Subset the data to take only those in the top and bottom quartile 
#(e.g., only participants defined in the grouping variable)
db_full_f2 <- db_full[!is.na(db_full$social_group),]
db_full_f2 <- mutate(db_full_f2, social_group = as.factor(db_full_f2$social_group))

#Double check that we still have the right n in each group after subsetting
sum(db_full_f2$social_top == 1) 
sum(db_full_f2$social_bottom == 1) 


#### Split into two groups based on factor 3 ####

top <- quantile(db_full$ML3, 0.75) #Top quartile
bottom <- quantile(db_full$ML3, 0.25) #Bottom quartile

db_full <- mutate(db_full, miso_top = ifelse(ML3 > top, 1, 0 ))  #Top quartile
db_full <- mutate(db_full, miso_bottom = ifelse(ML3 < bottom, 1, 0)) #Bottom quartile

#How many participants per group?
sum(db_full$miso_top == 1) 
sum(db_full$miso_bottom == 1) 

#Make one "group" variable for high/low factor 3 scores
db_full <- mutate(db_full, miso_group = ifelse(miso_top == 1, 1, 
                                           ifelse(miso_bottom == 1, 0, NA)))

#Subset the data to take only those in the top and bottom quartile 
#(e.g., only participants defined in the grouping variable)
db_full_f3 <- db_full[!is.na(db_full$miso_group),]
db_full_f3 <- mutate(db_full_f3, miso_group = as.factor(db_full_f3$miso_group))

#Double check that we still have the right n in each group after subsetting
sum(db_full_f3$miso_top == 1) 
sum(db_full_f3$miso_bottom == 1) 

#### Reformat the data into "long" format for parameters of interest ####

db_sc_control_pc <- subset(db_full, select = c(prolific_id, mood_group, social_group, miso_group, pc_ic, pc_nc, age, sex, icar_total))
db_sc_control_pc <- pivot_longer(db_sc_control_pc, pc_ic:pc_nc, values_to = "PercievedControl", names_to = "Condition")
db_sc_control_pc$Condition <- recode(db_sc_control_pc$Condition,
                                     pc_ic = "Controllable",
                                     pc_nc = "Uncontrollable",)

db_sc_control_gap <- subset(db_full, select = c(gap_ic, gap_nc))
db_sc_control_gap<- pivot_longer(db_sc_control_gap, gap_ic:gap_nc, values_to = "Gap", names_to = "Condition")
db_sc_control_gap$Condition <- recode(db_sc_control_gap$Condition,
                                      gap_ic = "Controllable",
                                      gap_nc = "Uncontrollable",)


db_sc_control_sn <- subset(db_full, select = c(Sensitivity_to_norm_violation_NC, Sensitivity_to_norm_violation_IC))
db_sc_control_sn<- pivot_longer(db_sc_control_sn, Sensitivity_to_norm_violation_IC:Sensitivity_to_norm_violation_NC, values_to = "NormSensitivity", names_to = "Condition")
db_sc_control_sn$Condition <- recode(db_sc_control_sn$Condition,
                                     Sensitivity_to_norm_violation_IC = "Controllable",
                                     Sensitivity_to_norm_violation_NC = "Uncontrollable",
)


db_sc_control_d <- subset(db_full, select = c(Expected_influence_delta_IC,Expected_influence_delta_NC))
db_sc_control_d <- pivot_longer(db_sc_control_d, Expected_influence_delta_IC:Expected_influence_delta_NC, values_to = "Delta", names_to = "Condition")
db_sc_control_d$Condition <- recode(db_sc_control_d$Condition,
                                    Expected_influence_delta_IC = "Controllable",
                                    Expected_influence_delta_IC = "Uncontrollable",)


## Merge all and remove repeat variables
db_sc_long <- cbind(db_sc_control_pc, db_sc_control_sn, db_sc_control_gap, db_sc_control_d)
db_sc_long <- subset(db_sc_long, select = c(prolific_id, miso_group, mood_group, social_group,  age, sex,
                                            icar_total, Condition, PercievedControl,
                                            NormSensitivity, Gap, Delta))


#### Create Histograms and get numbers for Figure 1 ####

#AMISOS
breaks <- pretty(range(db_q$amisos_score),
                 n = nclass.Sturges(db_q$amisos_score),
                 min.n = 1)

h= hist(db_q$amisos_score, plot=T, breaks=c(0:20))
my_color= ifelse(h$breaks <= 4, rgb(0.2,0.8,0.5,0.5) , ifelse (h$breaks >4 & h$breaks <=9, "blue", ifelse (h$breaks >9 & h$breaks <=14, "navy", ifelse (h$breaks >14 & h$breaks <=19, "purple", ifelse (h$breaks >19, "black", rgb(0.2,0.2,0.2,0.2))))))
plot(h, col=my_color , border=F , main="" , xlab="A-MISO-S Total Score", xlim=c(0,20), ylim=c(0,200), xaxt="n")
legend(x=11, y=325, legend=c("Subclinical (0-4), 45.1%", "Mild (5-9), 39.4%", "Moderate (10-14), 14.3%", "Severe (15-19), 1.2%"),
       col=c(rgb(0.2,0.8,0.5,0.5), "blue", "navy","purple"),  cex=.75,
       box.lty=0, pch=15,  x.intersp=.3, y.intersp=.1, border ="transparent", bg="transparent")
axis(side=1,at=h$mids,labels=seq(0,19))

#Alternative visualization, also AMISOS
ggplot(db_q, aes(amisos_score)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "A-MISO-S Total Score", y = "Count") +
  theme_classic(base_size = 18) 

#Factor 3
ggplot(db_full, aes(ML3)) +
  geom_density() +
  labs(x = "Miso-OC Factor Score", y = "Density") +
  geom_area(data = subset(df.dens, x >= top), 
            aes(x=x, y=y), fill='green4', alpha=0.8) +
  geom_area(data = subset(df.dens, x <= bottom), 
            aes(x=x, y=y), fill='#619CFF', alpha=0.8) +
  theme_classic(base_size = 18) 

#Prevalence rates by severity
sum(db_q$amisos_score <= 4)/nrow(db_q) #sub-clinical
sum(db_q$amisos_score > 4 & db_q$amisos_score <= 9)/nrow(db_q) #mild
sum(db_q$amisos_score > 9 & db_q$amisos_score <= 14)/nrow(db_q) #moderate
sum(db_q$amisos_score > 14 & db_q$amisos_score <= 19)/nrow(db_q) #severe
sum(db_q$amisos_score > 19)/nrow(db_q) #extreme

#### Test for group differences in demographic variables ####

##Misophonia and OCI scores
t.test(db_full_f3$amisos_score ~ db_full_f3$miso_group)
t.test(db_full_f3$oci_score ~ db_full_f3$miso_group)

##Age
t.test(db_full_f3$age ~ db_full_f3$miso_group) 
#low miso group is significantly older

##Sex
oneway.test(db_full_f3$sex ~ db_full_f3$miso_group, var.equal = TRUE) 
tabyl(db_full_f3, sex, miso_group) #get count per group
#low miso group has significantly more men

##Gender
oneway.test(db_full_f3$gender ~ db_full_f3$miso_group, var.equal = TRUE) 
tabyl(db_full_f3, gender, miso_group) #get count per group
#low miso group has significantly more men


##Education
oneway.test(db_full_f3$edu_level ~ db_full_f3$miso_group, var.equal = TRUE) 
tabyl(db_full_f3, edu_level, miso_group) #get count per group

##Income
oneway.test(db_full_f3$income ~ db_full_f3$miso_group, var.equal = TRUE) 
tabyl(db_full_f3, income, miso_group) #get count per group

##Cognitive ability
t.test(db_full_f3$icar_total ~ db_full_f3$miso_group) 
#low miso group has significantly higher cognitive ability (IQ proxy)

#### Test Interactions ####

# Factor 1
fit <- lm(PercievedControl ~ Condition * mood_group + age + sex + icar_total, data=db_sc_long)
anova(fit)

fit2 <- lm(NormSensitivity ~ Condition * mood_group + age + sex + icar_total, data=db_sc_long)
anova(fit2)

fit3 <- lm(Gap ~ Condition * mood_group + age + sex + icar_total, data=db_sc_long)
anova(fit3)

fit4 <- lm(Delta ~ Condition * mood_group + age + sex + icar_total, data=db_sc_long)
anova(fit4)

# Factor 2
fit5 <- lm(PercievedControl ~ Condition * social_group + age + sex + icar_total, data=db_sc_long)
anova(fit5)

fit6 <- lm(NormSensitivity ~ Condition * social_group + age + sex + icar_total, data=db_sc_long)
anova(fit6)

fit7 <- lm(Gap ~ Condition * social_group + age + sex + icar_total, data=db_sc_long)
anova(fit7)

fit8 <- lm(Delta ~ Condition * social_group + age + sex + icar_total, data=db_sc_long)
anova(fit8)

# Factor 3 - main results 
fit9 <- lm(PercievedControl ~ Condition * miso_group + age + sex + icar_total, data=db_sc_long)
anova(fit9)

fit10 <- lm(NormSensitivity ~ Condition * miso_group + age + sex + icar_total, data=db_sc_long)
anova(fit10)

fit11 <- lm(Gap ~ Condition * miso_group + age + sex + icar_total, data=db_sc_long)
anova(fit11)

fit12 <- lm(Delta ~ Condition * miso_group + age + sex + icar_total, data=db_sc_long)
anova(fit12)

#### For significant Factor 3 interactions - t-tests to assess directionality ####

#### ~ Explore differences between the groups ####

##Sensitivity to Norm Violation
t.test(db_full_f3$Sensitivity_to_norm_violation_NC ~ db_full_f3$miso_group)
t.test(db_full_f3$Sensitivity_to_norm_violation_IC ~ db_full_f3$miso_group)

##Control discrepancy
t.test(db_full_f3$gap_nc ~ db_full_f3$miso_group)
t.test(db_full_f3$gap_ic ~ db_full_f3$miso_group)

##Perceived control
t.test(db_full_f3$pc_nc ~ db_full_f3$miso_group)
t.test(db_full_f3$pc_ic ~ db_full_f3$miso_group)

#### ~ Explore differences between the conditions ####

db_sc_long_high_miso <- subset(db_sc_long, miso_group==1)
db_sc_long_high_miso$Condition <- recode(db_sc_long_high_miso$Condition,
                                         "Controllable" = 1,
                                         "Uncontrollable" = 0,
)

db_sc_long_low_miso <- subset(db_sc_long, miso_group==0)
db_sc_long_low_miso$Condition <- recode(db_sc_long_low_miso$Condition,
                                        "Controllable" = 1,
                                        "Uncontrollable" = 0,
)

t.test(db_sc_long_high_miso$NormSensitivity ~ db_sc_long_high_miso$Condition)
t.test(db_sc_long_low_miso$NormSensitivity ~ db_sc_long_low_miso$Condition)

t.test(db_sc_long_high_miso$Gap ~ db_sc_long_high_miso$Condition)
t.test(db_sc_long_low_miso$Gap ~ db_sc_long_low_miso$Condition)

t.test(db_sc_long_high_miso$PercievedControl ~ db_sc_long_high_miso$Condition)
t.test(db_sc_long_low_miso$PercievedControl ~ db_sc_long_low_miso$Condition)

#### Plot interactions (for Figures 4 and 5) ####

db_sc_long_miso <- db_sc_long[!is.na(db_sc_long$miso_group),]
  
sum1 <- summarySE(db_sc_long_miso, measurevar="PercievedControl", groupvars=c("Condition","miso_group"))
sum2 <- summarySE(db_sc_long_miso, measurevar="NormSensitivity", groupvars=c("Condition","miso_group"))
sum3 <- summarySE(db_sc_long_miso, measurevar="Gap", groupvars=c("Condition","miso_group"))

dodge <- position_dodge(width=0.9) 

db_sc_long <- mutate(db_sc_long, xcord_p1 = ifelse(miso_group == "1" & Condition ==
                                                     'Controllable', 0.775,
                                                   ifelse(miso_group == "0" & Condition ==
                                                            'Controllable', 1.225,
                                                          ifelse(miso_group == "1" & Condition ==
                                                                   'Uncontrollable', 1.775,
                                                                 ifelse(miso_group == "0" & Condition ==
                                                                          'Uncontrollable', 2.225, 0)))))


db_sc_long <- mutate(db_sc_long, xcord_p2 = ifelse(miso_group == "1" & Condition ==
                                                     'Controllable', 1.775,
                                                   ifelse(miso_group == "0" & Condition ==
                                                            'Controllable', 0.775,
                                                          ifelse(miso_group == "1" & Condition ==
                                                                   'Uncontrollable', 2.225,
                                                                 ifelse(miso_group == "0" & Condition ==
                                                                          'Uncontrollable', 1.225, 0)))))


db_sc_long <- mutate(db_sc_long, group = ifelse(miso_group == '1' & Condition ==
                                                  'Controllable', "high_miso_ic",
                                                ifelse(miso_group == '0' & Condition ==
                                                         'Controllable', "low_miso_ic",
                                                       ifelse(miso_group == '1' & Condition ==
                                                                'Uncontrollable', "high_miso_nc",
                                                              ifelse(miso_group == '0' & Condition ==
                                                                       'Uncontrollable', "low_miso_nc", NA)))))

#### ~ Perceived Control, bar plot ####
ggplot(data=sum1, aes(y=PercievedControl, x=(factor(Condition)), group=miso_group)) +
  geom_bar(aes(fill=factor(miso_group)), stat = "summary", position = "dodge", fun = "mean") +
  labs( x="Condition", y="Percieved Control") +
  geom_errorbar(aes(ymin=PercievedControl-se, ymax=PercievedControl+se),
                width=.2, position=position_dodge(0.9)) +
  scale_fill_manual(values=c("steelblue1","forestgreen"), name="Group") + #labels=c("In Control","No Control")) +
  guides(fill=FALSE) +
  scale_color_manual(values=c("darkgreen","dodgerblue4"), name="Group") +
  theme_classic(base_size = 16)  # white background

#### ~ Perceived Control, estimation plot ####

multi.two.group.paired <- 
  db_sc_long %>%
  dabest(group, PercievedControl, 
         idx = list(c("high_miso_ic", "low_miso_ic"), 
                    c("high_miso_nc", "low_miso_nc")),
         paired = FALSE
  )


multi.two.group.unpaired.meandiff <- mean_diff(multi.two.group.paired)

plot(multi.two.group.unpaired.meandiff,
     rawplot.ylabel = "Percieved Control",
     effsize.ylabel = "Mean Difference",
     color.column = miso_group,
     palette = c("steelblue1", "forestgreen"),
     theme = ggplot2::theme_classic(),
     rawplot.markersize=1.5,
     rawplot.groupwidth = 0.35,
     group.summaries = NULL,
     axes.title.fontsize = 16)

#### ~ Norm Sensitivity, bar plot ####

ggplot(data=sum2, aes(y=NormSensitivity, x=(factor(Condition)), group=miso_group)) +
  geom_bar(aes(fill=factor(miso_group)), stat = "summary", position = "dodge", fun = "mean") +
  scale_fill_manual(values=c("steelblue1","forestgreen"), name="Group") +
  labs( x="Condition", y="Sensitivity to Norm Violation") +
  geom_errorbar(aes(ymin=NormSensitivity-se, ymax=NormSensitivity+se),
                width=.2, position=position_dodge(.85)) +
  coord_cartesian(ylim=c(0.6,0.85)) +
  guides(fill=FALSE) +
  theme_classic(base_size = 16)  # white background

#### ~ Norm Sensitivity, estimation plot ####

multi.two.group.paired_2 <- 
  db_sc_long %>%
  dabest(group, NormSensitivity, 
         idx = list(c("high_miso_ic", "high_miso_nc"), 
                    c("low_miso_ic", "low_miso_nc")),
         paired = FALSE
  )


multi.two.group.unpaired.meandiff_2 <- mean_diff(multi.two.group.paired_2)

plot(multi.two.group.unpaired.meandiff_2,
     rawplot.ylabel = "Norm Sensitivity",
     effsize.ylabel = "Mean Difference",
     color.column = miso_group,
     palette = c("forestgreen", "steelblue1"),
     theme = ggplot2::theme_classic(),
     rawplot.markersize=1.5,
     rawplot.groupwidth = 0.35,
     group.summaries = NULL,
     axes.title.fontsize = 16)

#### ~ Gap, bar plot ####

ggplot(data=sum3, aes(y=Gap, x=(factor(Condition)), group=miso_group)) +
  geom_bar(aes(fill=factor(miso_group)), stat = "summary", position = "dodge", fun = "mean") +
  scale_fill_manual(values=c("steelblue1","forestgreen"), name="Group") +
  labs( x="Condition", y="Discrepency") +
  geom_errorbar(aes(ymin=Gap-se, ymax=Gap+se),
                width=.2, position=position_dodge(.85)) +
  guides(fill=FALSE) +
  theme_classic(base_size = 16)  # white background

#### ~ Gap, estimation plot ####

multi.two.group.paired_3 <- 
  db_sc_long %>%
  dabest(group, Gap, 
         idx = list(c("high_miso_ic", "low_miso_ic"), 
                    c("high_miso_nc", "low_miso_nc")),
         paired = FALSE
  )


multi.two.group.unpaired.meandiff_3 <- mean_diff(multi.two.group.paired_3)

plot(multi.two.group.unpaired.meandiff_3,
     rawplot.ylabel = "Discrepency",
     effsize.ylabel = "Mean Difference",
     color.column = miso_group,
     palette = c("forestgreen", "steelblue1"),
     theme = ggplot2::theme_classic(),
     rawplot.markersize=1.5,
     rawplot.groupwidth = 0.35,
     group.summaries = NULL,
     axes.title.fontsize = 16)


#### For Figure 4: Create data frames by trial information and condition ####

id_and_group <- subset(db_full, select = c(prolific_id, miso_group)) 

db_trials_offers_IC <- db_full %>% select(contains("offer_IC_trial"))
db_trials_offers_IC <- cbind(id_and_group, db_trials_offers_IC)
db_trials_offers_IC <- db_trials_offers_IC[!is.na(db_trials_offers_IC$miso_group),]

db_trials_offers_NC <- db_full %>% select(contains("offer_NC_trial"))
db_trials_offers_NC <- cbind(id_and_group, db_trials_offers_NC)
db_trials_offers_NC <- db_trials_offers_NC[!is.na(db_trials_offers_NC$miso_group),]

db_trials_choices_IC <- db_full %>% select(contains("choice_IC_trial"))
db_trials_choices_IC <- cbind(id_and_group, db_trials_choices_IC)
db_trials_choices_IC <- db_trials_choices_IC[!is.na(db_trials_choices_IC$miso_group),]

db_trials_choices_NC <- db_full %>% select(contains("choice_NC_trial"))
db_trials_choices_NC <- cbind(id_and_group, db_trials_choices_NC)
db_trials_choices_NC <- db_trials_choices_NC[!is.na(db_trials_choices_NC$miso_group),]

db_trials_IC <- cbind(db_trials_offers_IC, db_trials_choices_IC)
db_trials_IC <- db_trials_IC[, !duplicated(colnames(db_trials_IC))] #delete repeat variables
db_trials_NC <- cbind(db_trials_offers_NC, db_trials_choices_NC)
db_trials_NC <- db_trials_NC[, !duplicated(colnames(db_trials_NC))]  #delete repeat variables

#### Offer size ####
db_trials_offers_IC_means <- aggregate(db_trials_offers_IC[, 3:32], list(db_trials_offers_IC$miso_group), mean) #get means for each trial, by group
db_trials_offers_IC_means <- rename(db_trials_offers_IC_means, c(`Group.1` = "Group")) #rename group variable
db_trials_offers_IC_means_long <-pivot_longer(db_trials_offers_IC_means, cols = 2:31, values_to = "Offers", names_to = "Trial") #pivot long
db_trials_offers_IC_se <- aggregate(db_trials_offers_IC[, 3:32], list(db_trials_offers_IC$miso_group), std.error) #get se for each trial, by group
db_trials_offers_IC_se <- rename(db_trials_offers_IC_se, c(`Group.1` = "Group")) #rename group variable
db_trials_offers_IC_se_long <-pivot_longer(db_trials_offers_IC_se, cols = 2:31, values_to = "se", names_to = "Trial") #pivot long

offers_IC_mean <- cbind(db_trials_offers_IC_means_long, db_trials_offers_IC_se_long)
offers_IC_mean <- subset(offers_IC_mean, select = c(Group, Trial, Offers, se))
offers_IC_mean$Group<-ifelse(offers_IC_mean$Group=="1", "High Miso-OCD", "Low Miso-OCD")
c <- rep(c(1:30), times=2)
offers_IC_mean$Trial <- c

db_trials_offers_NC_means <- aggregate(db_trials_offers_NC[, 3:32], list(db_trials_offers_IC$miso_group), mean) #get means for each trial, by group
db_trials_offers_NC_means <- rename(db_trials_offers_NC_means, c(`Group.1` = "Group")) #rename group variable
db_trials_offers_NC_means_long <-pivot_longer(db_trials_offers_NC_means, cols = 2:31, values_to = "Offers", names_to = "Trial") #pivot long
db_trials_offers_NC_se <- aggregate(db_trials_offers_NC[, 3:32], list(db_trials_offers_IC$miso_group), std.error) #get se for each trial, by group
db_trials_offers_NC_se <- rename(db_trials_offers_NC_se, c(`Group.1` = "Group")) #rename group variable
db_trials_offers_NC_se_long <-pivot_longer(db_trials_offers_NC_se, cols = 2:31, values_to = "se", names_to = "Trial") #pivot long

offers_NC_mean <- cbind(db_trials_offers_NC_means_long, db_trials_offers_NC_se_long)
offers_NC_mean <- subset(offers_NC_mean, select = c(Group, Trial, Offers, se))
offers_NC_mean$Group<-ifelse(offers_NC_mean$Group=="1", "High Miso-OC", "Low Miso-OC")
offers_NC_mean$Trial <- c

# Do groups differ in offer size?
t.test(offers_IC_mean$Offers ~ offers_IC_mean$Group)
t.test(offers_NC_mean$Offers ~ offers_NC_mean$Group)

#### Plot offer size for each trial by group and condition ####

ggplot(offers_IC_mean, aes(x=Trial, y=Offers, ymin=Offers-se, ymax=Offers+se, fill=Group)) + 
  geom_line(aes(color=Group)) + 
  labs(x = "Trial", y ="Offer Size", title = "Controllable") + 
  scale_color_manual(values = c("forestgreen", "steelblue1")) +
  scale_fill_manual(values = c("forestgreen", "steelblue1")) +
  coord_cartesian(ylim=c(4.5,6.5)) +
  geom_ribbon(alpha=0.2) +
  theme_classic(base_size = 20) 

ggplot(offers_NC_mean, aes(x=Trial, y=Offers, ymin=Offers-se, ymax=Offers+se, fill=Group)) + 
  geom_line(aes(color=Group)) +
  labs(x = "Trial", y ="Offer Size", title = "Uncontrollable") + 
  scale_color_manual(values = c("forestgreen", "steelblue1")) +
  scale_fill_manual(values = c("forestgreen", "steelblue1")) +
  coord_cartesian(ylim=c(4.5,6.5)) +
  geom_ribbon(alpha=0.2) +
  theme_classic(base_size = 20) 

#### Rejection Rate ####

db_rr_ic <- subset(db_full, select=c(prolific_id, miso_group, rejR_L_ic, rejR_M_ic, rejR_H_ic)) #get rejection rates for controllable condition
db_rr_ic <- db_rr_ic[!is.na(db_rr_ic$miso_group),] #remove those not in top and bottom quartile
db_rr_ic$rejR_L_ic[db_rr_ic$rejR_L_ic == "NaN"] <- NA #recode missing values
db_rr_ic$rejR_M_ic[db_rr_ic$rejR_M_ic == "NaN"] <- NA #recode missing values
db_rr_ic$rejR_H_ic[db_rr_ic$rejR_H_ic == "NaN"] <- NA #recode missing values
db_rr_ic <- mutate(db_rr_ic, rejR_L_ic = as.numeric(rejR_L_ic)) #make numeric
db_rr_ic <- mutate(db_rr_ic, rejR_M_ic = as.numeric(rejR_M_ic)) #make numeric
db_rr_ic <- mutate(db_rr_ic, rejR_H_ic = as.numeric(rejR_H_ic)) #make numeric
db_trials_rr_ic_means <- aggregate(db_rr_ic[, 3:5], list(db_rr_ic$miso_group), function(x) mean(x,na.rm=T)) #get means for offer size group, by miso group
db_trials_rr_ic_means <- rename(db_trials_rr_ic_means, c(`Group.1` = "Group")) #rename group variable
db_trials_rr_ic_means_long <-pivot_longer(db_trials_rr_ic_means, cols = 2:4, values_to = "Rejection Rate", names_to = "Offer Size") #pivot long

db_trials_rr_ic_se <- aggregate(db_rr_ic[, 3:5], list(db_rr_ic$miso_group), std.error) #get means for offer size group, by miso group
db_trials_rr_ic_se <- rename(db_trials_rr_ic_se, c(`Group.1` = "Group")) #rename group variable
db_trials_rr_ic_se_long <-pivot_longer(db_trials_rr_ic_means, cols = 2:4, values_to = "se", names_to = "Offer Size") #pivot long

rr_IC_mean <- cbind(db_trials_rr_ic_means_long, db_trials_rr_ic_se_long) #combine means and se
rr_IC_mean <- subset(rr_IC_mean, select = c(Group,`Offer Size`, `Rejection Rate`, se)) #delete repeat variables
rr_IC_mean <- mutate(rr_IC_mean, Condition="Controllable")


rr_IC_mean <- mutate(rr_IC_mean, `Rejection Rate` = `Rejection Rate`*100) #change to percentages
rr_IC_mean$Group<-ifelse(rr_IC_mean$Group=="1", "High Miso-OC", "Low Miso-OC")  #re-code group names
rr_IC_mean$`Offer Size`<-ifelse(rr_IC_mean$`Offer Size`=="rejR_L_nc", "1-3",    #re-code offer size names
                                ifelse(rr_IC_mean$`Offer Size`=="rejR_M_nc", "4-6",
                                       ifelse(rr_IC_mean$`Offer Size`=="rejR_H_nc", "7-9",
                                              ifelse(rr_IC_mean$`Offer Size`=="rejR_L_ic", "1-3",
                                                     ifelse(rr_IC_mean$`Offer Size`=="rejR_M_ic", "4-6",
                                                            ifelse(rr_IC_mean$`Offer Size`=="rejR_H_ic", "7-9", NA))))))


db_rr_nc <- subset(db_full, select=c(prolific_id, miso_group, rejR_L_nc, rejR_M_nc, rejR_H_nc)) #get rejection rates for controllable condition
db_rr_nc <- db_rr_nc[!is.na(db_rr_nc$miso_group),] #remove those not in top and bottom quartile
db_rr_nc$rejR_L_nc[db_rr_nc$rejR_L_nc == "NaN"] <- NA #recode missing values
db_rr_nc$rejR_M_nc[db_rr_nc$rejR_M_nc == "NaN"] <- NA #recode missing values
db_rr_nc$rejR_H_nc[db_rr_nc$rejR_H_nc == "NaN"] <- NA #recode missing values
db_rr_nc <- mutate(db_rr_nc, rejR_L_nc = as.numeric(rejR_L_nc)) #make numeric
db_rr_nc <- mutate(db_rr_nc, rejR_M_nc = as.numeric(rejR_M_nc)) #make numeric
db_rr_nc <- mutate(db_rr_nc, rejR_H_nc = as.numeric(rejR_H_nc)) #make numeric
db_trials_rr_nc_means <- aggregate(db_rr_nc[, 3:5], list(db_rr_nc$miso_group), function(x) mean(x,na.rm=T)) #get means for offer size group, by miso group
db_trials_rr_nc_means <- rename(db_trials_rr_nc_means, c(`Group.1` = "Group")) #rename group variable
db_trials_rr_nc_means_long <-pivot_longer(db_trials_rr_nc_means, cols = 2:4, values_to = "Rejection Rate", names_to = "Offer Size") #pivot long

db_trials_rr_nc_se <- aggregate(db_rr_nc[, 3:5], list(db_rr_nc$miso_group), std.error) #get means for offer size group, by miso group
db_trials_rr_nc_se <- rename(db_trials_rr_nc_se, c(`Group.1` = "Group")) #rename group variable
db_trials_rr_nc_se_long <-pivot_longer(db_trials_rr_nc_means, cols = 2:4, values_to = "se", names_to = "Offer Size") #pivot long

rr_NC_mean <- cbind(db_trials_rr_nc_means_long, db_trials_rr_nc_se_long) #combine means and se
rr_NC_mean <- subset(rr_NC_mean, select = c(Group,`Offer Size`, `Rejection Rate`, se)) #delete repeat variables
rr_NC_mean <- mutate(rr_NC_mean, Condition="Uncontrollable")


rr_NC_mean <- mutate(rr_NC_mean, `Rejection Rate` = `Rejection Rate`*100) #change to percentages
rr_NC_mean$Group<-ifelse(rr_NC_mean$Group=="1", "High Miso-OC", "Low Miso-OC")  #re-code group names
rr_NC_mean$`Offer Size`<-ifelse(rr_NC_mean$`Offer Size`=="rejR_L_nc", "1-3",    #re-code offer size names
                             ifelse(rr_NC_mean$`Offer Size`=="rejR_M_nc", "4-6",
                                    ifelse(rr_NC_mean$`Offer Size`=="rejR_H_nc", "7-9",
                                           ifelse(rr_NC_mean$`Offer Size`=="rejR_L_ic", "1-3",
                                                  ifelse(rr_NC_mean$`Offer Size`=="rejR_M_ic", "4-6",
                                                         ifelse(rr_NC_mean$`Offer Size`=="rejR_H_ic", "7-9", NA))))))


#### Plot rejection rate for each offer size group by group and condition ####

ggplot(rr_IC_mean, aes(x=`Offer Size`, y=`Rejection Rate`, group = Group,
                                    ymin=(`Rejection Rate`-se),
                                    ymax=(`Rejection Rate`+se))) + 
  stat_summary(geom = "line", fun = "mean", size=0.8, aes(color=Group), na.rm = TRUE) +
  stat_summary(geom = "point", fun = "mean", size=2.8, alpha=0.8, aes(color=Group), na.rm = TRUE) +
  labs(x = "Offer Size ($)", y ="Rejection Rate (%)", title = "Controllable") + 
  scale_color_manual(values = c("forestgreen", "steelblue1")) +
  geom_errorbar(width=.2, size=0.6, alpha=0.8, aes(color=Group)) +
  coord_cartesian(ylim=c(0,100)) +
  theme_classic(base_size = 20) 


ggplot(rr_NC_mean, aes(x=`Offer Size`, y=`Rejection Rate`, group = Group,
                       ymin=(`Rejection Rate`-se),
                       ymax=(`Rejection Rate`+se))) + 
  stat_summary(geom = "line", fun = "mean", size=0.8, aes(color=Group), na.rm = TRUE) +
  stat_summary(geom = "point", fun = "mean", size=2.8, alpha=0.8, aes(color=Group), na.rm = TRUE) +
  labs(x = "Offer Size ($)", y ="Rejection Rate (%)", title = "Uncontrollable") + 
  scale_color_manual(values = c("forestgreen", "steelblue1")) +
  geom_errorbar(width=.2, size=0.6, alpha=0.8, aes(color=Group)) +
  coord_cartesian(ylim=c(0,100)) +
  theme_classic(base_size = 20) 

