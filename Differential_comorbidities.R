###################################################################################################
# Calculating differential comorbidities for full cohort
# Matched with total number comorbidites as co-variate
# Generates main figure 2 in manuscript
###################################################################################################

# input full cohort matched data information
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
diagnoses_filtered <- subset(diagnoses_unique, condition_concept_id != 373995)
diagnoses_filtered$group <- matched_data$group[match(diagnoses_filtered$person_id, matched_data$person_id)]
# write.csv(diagnoses_filtered, "Diagnoses_filtered_matched_cohort_ICU.csv")

# Generate UMAP of comorbidities =======
# generate table with diagnoses in columns and patient ID in rows.
# 0 if patient does not have diagnosis, 1 if patient has.
library(data.table)
res.dt = setDT(diagnoses_filtered)
diagnosis.table = dcast(res.dt[, .(person_id, condition_concept_id)], person_id ~ condition_concept_id, fun.aggregate=length)

# Generate UMAP of patients by comorbidities
library(umap)
diag.umap <- umap(diagnosis.table[,-1])
head(diag.umap$layout, 3)

library(magrittr)
diag.umap.plot <- diag.umap$layout %>%
  as.data.frame()%>%
  rename(UMAP1="V1",
         UMAP2="V2") %>%
  set_rownames(diagnosis.table$person_id)

# Merge UMAP results with demographic information
diag.umap.plot.merged <- merge(diag.umap.plot, matched_data[,c("person_id","group", "gender","race","age_admit","died_during_admit","stay_length", "service", "visit_num","total_comorb")],
                               by.x = "row.names", by.y = "person_id")

# Plot UMAP and color based on group
diag.umap.plot.merged %>%
  ggplot(aes(x = UMAP1, 
             y = UMAP2,
             color = as.factor(group)))+
  geom_point(alpha = 0.5)+
  labs(x = "UMAP1",
       y = "UMAP2",
       subtitle = "UMAP plot") +
  theme_classic() +
  scale_color_manual(values = c("0" = "grey", "1" = "salmon"))

# Plot UMAP by sex
diag.umap.plot.merged %>%
  ggplot(aes(x = UMAP1, 
             y = UMAP2,
             color = as.factor(gender)))+
  geom_point(alpha = 0.5)+
  labs(x = "UMAP1",
       y = "UMAP2",
       subtitle = "UMAP plot") +
  theme_classic() +
  scale_color_manual(values = c("Female" = "purple", "Male" = "palegreen3"))

# Differential diagnoses====================
# based on control vs delirium =======
list.diff.diag <- list()

unique_pts <- data.frame(unique(diagnoses_filtered$person_id))
colnames(unique_pts) <- "person_id"
unique_pts$group <- diagnoses_filtered$group[match(unique_pts$person_id, diagnoses_filtered$person_id)]

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

full_res <- full_res %>%
  mutate(logOR = log2(OR),
         adjusted_logOR = logOR * -1)

full_res$color <- "grey"
  full_res$color[full_res$p.value <= 0.05/nrow(full_res) & full_res$adjusted_logOR < 0] <- "control"
  full_res$color[full_res$p.value <= 0.05/nrow(full_res) & full_res$adjusted_logOR > 0] <- "delirium"
  
  library(ggrepel)
  ggplot(full_res, aes(x = adjusted_logOR, y = -log10(p.value), color = color, label = diagnosis)) +
    geom_point(size = 2) +
    theme_classic() +
    scale_color_manual(values = c("grey" = "grey", "delirium" = "salmon", "control" = "black")) +
    geom_hline(yintercept = -log10(0.05/nrow(full_res)), linetype = "dotted") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_text_repel(data = subset(full_res, -log10(p.value) >= 12 & color == "delirium" |
                                    -log10(p.value) >= 20 & color == "control"), max.overlaps = 100)
  
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
  
  # edited externally to fill in ICD10 blocks that were not found in OMOP
  full_res_sig <- read.csv("Sig_diff_diagnoses_icd_blocks_random_visits.csv", header=T, row.names=1)
  full_res_sig$block_letter <- icd_blocks$ICD10[match(full_res_sig$icd_block, icd_blocks$Abbreviated)]
  
  block_plot <- full_res_sig %>%
    group_by(icd_block) %>%
    mutate(pvalue_plot = -log10(p.value))
  
  block_plot$multiplier <- 1
  block_plot$multiplier[which(block_plot$color == "red")] <- -1
  block_plot$pvalue_plot_final <- block_plot$pvalue_plot * block_plot$multiplier
  
  ggplot(subset(block_plot, multiplier > 0), aes(x = icd_block, y = pvalue_plot_final, color = as.factor(icd_block), label = diagnosis)) +
    geom_point(alpha = 0.75) +
    scale_size_continuous(range = c(0.5,3)) +
    labs(x = NULL, 
         y = "-log10(p-value)") + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position="none") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") 
  
  