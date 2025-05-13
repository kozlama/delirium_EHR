#################################################################################################################
# Differential comorbidities between patient with delirium vs controls
# Stratified by sex in UCSF database
# Visuzalition of UMAP by splitting by sex
# Generates main figure 4 in manuscript
################################################################################################################ 

# UMAP visualization and statistics ======
# Plot UMAP and color based on group:
UMAP_data %>%
  ggplot(aes(x = UMAP1, 
             y = UMAP2,
             color = as.factor(group)))+
  geom_point(alpha = 0.5)+
  labs(x = "UMAP1",
       y = "UMAP2",
       subtitle = "UMAP plot") +
  theme_classic() +
  scale_color_manual(values = c("grey","salmon")) + 
  stat_summary(fun = "mean",
               geom = "point",
               color = "black")

# Plot uMAP based on group + gender:
UMAP_data %>%
  ggplot(aes(x = UMAP1, 
             y = UMAP2,
             color = as.factor(gender)))+
  geom_point(alpha = 0.5)+
  labs(x = "UMAP1",
       y = "UMAP2",
       subtitle = "UMAP plot") +
  theme_classic() +
  scale_color_manual(values = c("mediumpurple1","palegreen3"))

# Plot UMAP components as violin plots
UMAP_data %>%
  ggplot(aes(x = UMAP1, 
             y = as.factor(group),
             fill = as.factor(group)))+
  geom_violin(trim=F)+
  theme_classic() +
  scale_fill_manual(values = c("grey","salmon"))  + 
  stat_summary(fun=mean, geom="point", size=2, color="black")

UMAP_data %>%
  ggplot(aes(x = UMAP2, 
             y = as.factor(group),
             fill = as.factor(group)))+
  geom_violin(trim=F)+
  theme_classic() +
  scale_fill_manual(values = c("grey","salmon")) + 
  stat_summary(fun = "mean",
               geom = "point",
               color = "black")

UMAP_data %>%
  ggplot(aes(x = UMAP1, 
             y = gender,
             fill = as.factor(gender)))+
  geom_violin(trim=F)+
  theme_classic() +
  scale_fill_manual(values = c("mediumpurple1","palegreen3")) + 
  stat_summary(fun = "mean",
               geom = "point",
               color = "black")

# Perform the Mann-Whitney U test
wilcox.test(UMAP1 ~ as.factor(group), data=UMAP_data, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(UMAP2 ~ as.factor(group), data=UMAP_data, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(UMAP1 ~ as.factor(group), data=subset(UMAP_data,gender=F), na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(UMAP2 ~ as.factor(group), data=subset(UMAP_data,gender=F), na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(UMAP1 ~ as.factor(group), data=subset(UMAP_data,gender=M), na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(UMAP2 ~ as.factor(group), data=subset(UMAP_data,gender=M), na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(UMAP1 ~ as.factor(gender), data=UMAP_data, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(UMAP2 ~ as.factor(gender), data=UMAP_data, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)

# Differential diagnoses by sex =======
# input full cohort matched data information
library(MatchIt)

m.out0 <- matchit(group ~ age_admit + race + died_during_admit + stay_length + service + visit_num + total_comorb + ICU,
                  data = subset(matched_data, gender == "Female"),
                  method = NULL, distance = "glm")
plot(summary(m.out0))

m.out0 <- matchit(group ~ age_admit + gender + race + died_during_admit + stay_length + service + visit_num + total_comorb + ICU,
                  data = matched_data,
                  method = NULL, distance = "glm")
plot(summary(m.out0))

m.out0 <- matchit(group ~ age_admit + race + died_during_admit + stay_length + service + visit_num + total_comorb + ICU,
                  data = subset(matched_data, gender == "Male"),
                  method = NULL, distance = "glm")
plot(summary(m.out0))

m.out0 <- matchit(group ~ age_admit + gender + race + died_during_admit + stay_length + service + visit_num + total_comorb + ICU,
                  data = matched_data,
                  method = NULL, distance = "glm")
plot(summary(m.out0))

# Connect to data
# select all visits for patients identified ======
dbExecute(con, "IF OBJECT_ID('tempdb.dbo.#tmpmatched', 'U') IS NOT NULL DROP TABLE #tmpmatched")
dbWriteTable(con, '#tmpmatched', matched_data)
diagnosis_list <- dbGetQuery(con, "SELECT DISTINCT diagnosis.[person_id], diagnosis.[condition_concept_id], 
                  diagnosis.[visit_occurrence_id], diagnosis.[condition_start_date],
                  #tmpmatched.[visit_start_date], #tmpmatched.[visit_end_date],
                  concept.[concept_name]
                  FROM omop.[condition_occurrence] AS diagnosis
                  RIGHT JOIN #tmpmatched ON diagnosis.[person_id] = #tmpmatched.[person_id] 
                  INNER JOIN omop.concept AS concept ON diagnosis.[condition_concept_id] = concept.[concept_id]
                  WHERE diagnosis.[condition_start_date] < #tmpmatched.[visit_start_date] AND
                             concept.[domain_id] LIKE 'Condition' AND
                             concept.[standard_concept] LIKE 'S'")

# Filter for first time diagnoses
diagnoses_unique <- diagnosis_list %>% 
  distinct(person_id, condition_concept_id, .keep_all = TRUE)
length(unique(diagnoses_unique$person_id)) #12522 unique patients

# filter out delirium diagnoses
diagnoses_filt <- subset(diagnoses_unique, condition_concept_id != 373995)
diagnoses_filt$group <- matched_data$group[match(diagnoses_filt$person_id, matched_data$person_id)]
diagnoses_filt$gender <- matched_data$gender[match(diagnoses_filt$person_id, matched_data$person_id)]

# write.csv(diagnoses_filtered, "Diagnoses_filtered_matched_cohort_ICU.csv")

# Differential diagnoses====================
# based on control vs delirium sex-stratified =======
diagnoses_filtered <- subset(diagnoses_filt, gender == "Female") # run analysis for male also

list.diff.diag <- list()

unique_pts <- data.frame(unique(diagnoses_filtered$person_id))
colnames(unique_pts) <- "person_id"
unique_pts$group <- diagnoses_filtered$group[match(unique_pts$person_id, diagnoses_filtered$person_id)]
unique_pts$gender <- diagnoses_filtered$gender[match(unique_pts$person_id, diagnoses_filtered$person_id)]

N = length(unique(diagnoses_filtered$concept_name))
for (i in 1:N) {
  # first create matrix
  a<-as.data.frame(table(diagnoses_filtered$concept_name, diagnoses_filtered$group)[i,])
  diag.name <- as.data.frame(table(diagnoses_filtered$concept_name, diagnoses_filtered$group))[i,1]
  colnames(a) <- "yes"
  b<-as.data.frame(table(unique_pts$group))
  a$no <- b$Freq - a[,1]
  
  b<-fisher.test(a)
  
  list.diff.diag[[i]] <- data.frame(diagnosis = diag.name,
                                    p.value = b$p.value,
                                    OR = b$estimate)
  if (i %% 20 == 0) {
    print(sprintf('working on %s of %s: %.2f percent', i, N, i/N * 100))
  }
}

full_res <- do.call("rbind", list.diff.diag)

full_res$color <- "grey"
full_res$color[full_res$p.value <= 0.05/nrow(full_res) & log2(full_res$OR) < 0] <- "blue"
full_res$color[full_res$p.value <= 0.05/nrow(full_res) & log2(full_res$OR) > 0] <- "red"
      
library(ggrepel)
ggplot(full_res, aes(x = log2(OR), y = -log10(p.value), color = color, label = diagnosis)) +
      geom_point() +
      theme_classic() +
      scale_color_manual(values = c("grey" = "grey", "blue" = "salmon", "red" = "black")) +
      geom_hline(yintercept = -log10(0.05/nrow(full_res)), linetype = "dotted") +
      geom_vline(xintercept = 0, linetype = "dotted") +
      geom_text_repel(data = subset(full_res, -log10(p.value) >= 13  ), max.overlaps = 100)
    
View(full_res %>%
           subset(color=="blue") %>%
           arrange(desc(-log10(p.value))) %>%
           top_n(50))
    
# map SNOMED diagnoses to ICD10 blocks ====
# Generate temporary table
    dbExecute(con, "IF OBJECT_ID('tempdb.dbo.#tmpdiagnosis', 'U') IS NOT NULL DROP TABLE #tmpdiagnosis")
    dbWriteTable(con, '#tmpdiagnosis', full_res[,c("diagnosis","color")])
    icd.res <- dbGetQuery(con, "SELECT concept.[concept_id],
                  #tmpdiagnosis.[color], #tmpdiagnosis.[diagnosis],
                  relationship.[concept_id_2]
                  FROM omop.concept AS concept
                  RIGHT JOIN #tmpdiagnosis ON concept.[concept_name] = #tmpdiagnosis.[diagnosis]
                  INNER JOIN omop.[concept_relationship] AS relationship ON concept.[concept_id] = relationship.[concept_id_1]
                  WHERE concept.[domain_id] LIKE 'Condition' ")
    
    dbExecute(con, "IF OBJECT_ID('tempdb.dbo.#tmpdiagnosis', 'U') IS NOT NULL DROP TABLE #tmpdiagnosis")
    dbWriteTable(con, '#tmpdiagnosis', icd.res)
    map_icd <- dbGetQuery(con, "SELECT concept.[concept_name] AS concept2, concept.[vocabulary_id], concept.[concept_code],
                  #tmpdiagnosis.[diagnosis], #tmpdiagnosis.[color], #tmpdiagnosis.[concept_id_2]
                  FROM omop.concept AS concept
                  RIGHT JOIN #tmpdiagnosis ON concept.[concept_id] = #tmpdiagnosis.[concept_id_2]
                  WHERE concept.[domain_id] LIKE 'Condition' AND
                      concept.[vocabulary_id] LIKE 'ICD10CM'")
    
    setwd("~/UCSF data analysis")
    icd_blocks <- read.csv("ICD10_blocks.csv",header=T)
    
    map_icd$letter <- substr(map_icd$concept_code, 1, 1)
    map_icd$numbers <- substr(map_icd$concept_code, 2, 3)
    map_icd$letter[which(map_icd$letter == "D" & map_icd$numbers >= 50)] <- "D2"
    map_icd$letter[which(map_icd$letter == "H" & map_icd$numbers >= 60)] <- "H2"
    
    map_icd$icd_block <- icd_blocks$Abbreviated[match(map_icd$letter, icd_blocks$Letter)]
    
    full_res$icd_block <- map_icd$icd_block[match(full_res$diagnosis, map_icd$diagnosis)]
    full_res_sig <- subset(full_res, color == "blue" | color == "red")
    setwd("~/UCSF data analysis/OMOP cohort delirium unspecified")
    write.csv(full_res_sig, "Sig_diff_diagnoses_icd_blocks_random_visits_female.csv")
    
# edited externally to fill in ICD10 blocks that were not found in OMOP
    full_res_sig <- read.csv("Sig_diff_diagnoses_icd_blocks_random_visits_female.csv", header=T, row.names=1)
    full_res_sig$block_letter <- icd_blocks$ICD10[match(full_res_sig$icd_block, icd_blocks$Abbreviated)]
    
    block_plot <- full_res_sig %>%
      group_by(icd_block) %>%
      mutate(pvalue_plot = -log10(p.value))
    
    block_plot$multiplier <- 1
    block_plot$multiplier[which(block_plot$color == "red")] <- -1
    block_plot$pvalue_plot_final <- block_plot$pvalue_plot * block_plot$multiplier
    
    ggplot(block_plot, aes(x = icd_block, y = pvalue_plot_final, color = as.factor(icd_block), label = diagnosis)) +
      geom_point(alpha = 0.75) +
      scale_size_continuous(range = c(0.5,3)) +
      labs(x = NULL, 
           y = "-log10(p-value)") + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(legend.position="none") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey") 
    
# ICD10 block male vs female
    block_plot_female <- read.csv("Sig_diff_diagnoses_icd_blocks_random_visits_female.csv", header=T, row.names=1)
    block_plot_male <- read.csv("Sig_diff_diagnoses_icd_blocks_random_visits_male.csv", header=T, row.names=1)
    
    block_plot_female$gender = "Female"
    block_plot_male$gender = "Male"
    
    block_plot_sex <- rbind(block_plot_female, block_plot_male)
    block_plot_sex <- block_plot_sex %>%
      group_by(icd_block) %>%
      mutate(pvalue_plot = -log10(p.value)) %>%
      as.data.frame()
    
    block_plot_sex$multiplier <- 1
    block_plot_sex$multiplier[which(block_plot_sex$gender == "Male")] <- -1
    block_plot_sex$pvalue_plot_final <- block_plot_sex$pvalue_plot * block_plot_sex$multiplier
    
    ggplot(subset(block_plot_sex, color == "blue"), aes(x = icd_block, y = pvalue_plot_final, color = as.factor(icd_block), label = diagnosis)) +
      geom_point(alpha = 0.75) +
      scale_size_continuous(range = c(0.5,3)) +
      labs(x = NULL, 
           y = "-log10(p-value)") + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(legend.position="none") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
      geom_text_repel(data = subset(block_plot_sex, color == "blue" & icd_block == "Diseases of digestive system"), max.overlaps = 100)
    
    
    ggplot(subset(block_plot_sex, color == "red"), aes(x = icd_block, y = pvalue_plot_final, color = as.factor(icd_block), label = diagnosis)) +
      geom_point(alpha = 0.75) +
      scale_size_continuous(range = c(0.5,3)) +
      labs(x = NULL, 
           y = "-log10(p-value)") + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(legend.position="none") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey") 
    
# log-log OR plot for female and male
    female <- read.csv("differential_comorbidities_random_visits_female.csv", header=T,row.names=1)
    male <- read.csv("differential_comorbidities_random_visits_male.csv", header=T,row.names=1)
    
    both_sex <- data.frame(matrix(ncol = 0, nrow = nrow(female))) %>%
      mutate(diagnosis = female$diagnosis,
             female_p = female$p.value, 
             female_OR = female$OR) 
    
    both_sex$male_p <- male$p.value[match(both_sex$diagnosis, male$diagnosis)]
    both_sex$male_OR <- male$OR[match(both_sex$diagnosis, male$diagnosis)]
    
    both_sex$color <- "grey"
      both_sex$color[both_sex$female_p <= 0.05/nrow(both_sex)] <- "purple"
        both_sex$color[both_sex$male_p <= 0.05/nrow(both_sex)] <- "green"
          both_sex$color[both_sex$male_p <= 0.05/nrow(both_sex) & both_sex$female_p <= 0.05/nrow(both_sex)] <- "red"
            
          both_sex$adjusted_male_OR <- log2(both_sex$male_OR) * -1
          both_sex$adjusted_female_OR <- log2(both_sex$female_OR) * -1
          
          ggplot(subset(both_sex, color != "grey"), aes(adjusted_male_OR, adjusted_female_OR, color = color, label = diagnosis)) +
            geom_point(alpha = 0.7) +
            theme_classic() +
            scale_color_manual(values = c("purple" = "mediumpurple1", "green" = "palegreen3", "red" = "salmon")) +
            geom_vline(xintercept = 0, linetype = "dashed") +
            geom_hline(yintercept = 0, linetype = "dashed") +
            geom_abline(intercept = 0, slope = 1, linetype = "dashed")  +
            xlim(0,3) +
            ylim(0,3) +
            geom_text_repel(data = subset(both_sex, color == "purple" | color == "green"), max.overlaps = 100)
          