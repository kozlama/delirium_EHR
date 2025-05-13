################################################################################
# Using UCSF OMOP to extract all lab values before an inpatient visit
# Generates main figure 3 in manuscript
###################################################################################################

# import data with matched cohort information
# connect to dataset
# extract all measures from before a visit
dbExecute(con, "IF OBJECT_ID('tempdb.dbo.#tmpvisit', 'U') IS NOT NULL DROP TABLE #tmpvisit")
dbWriteTable(con, '#tmpvisit', matched_data)
res <- dbGetQuery(con, "SELECT lab.[measurement_date], lab.[value_as_number], lab.[visit_occurrence_id], lab.[unit_source_value],
                  #tmpvisit.[person_id],
                  concept.[concept_name]
                  FROM omop.measurement AS lab
                  INNER JOIN #tmpvisit ON lab.[person_id] = #tmpvisit.[person_id]
                  
                  INNER JOIN omop.concept AS concept ON lab.[measurement_concept_id] = concept.[concept_id]
                  WHERE concept.[domain_id] LIKE 'Measurement' AND 
                  concept.[standard_concept] LIKE 'S' AND
                  lab.[measurement_date] < '2022-12-30'")

# visits of interest (before the inpatient start date in question)
res$visit_threshold <- matched_data$visit_start_date[match(res$person_id, matched_data$person_id)]
res$query_date = as.Date(as.Date(res$visit_threshold)-dyears(1))
res_filtered <- subset(res, measurement_date < visit_threshold & measurement_date > query_date)

# generate table with labs in columns and patient ID in rows, taking only the median values of the labs =====
library(data.table)
res.dt = setDT(res_filtered)
lab.table = dcast(res.dt[, .(person_id, concept_name, value_as_number)], person_id ~ concept_name, value.var = "value_as_number", fun.aggregate=median)

# remove labs with >95% of patients with NA values
na_sum <- as.data.frame(colSums(is.na(lab.table)))
colnames(na_sum) = "NA_Freq"
na_sum$percent_NA <- na_sum$NA_Freq / nrow(lab.table)*100  
filtered_lab_tests <- subset(na_sum, percent_NA < 95)

lab.table.filtered <- as.data.frame(lab.table)[, which(colnames(lab.table) %in% row.names(filtered_lab_tests))]

# Calculate differential labs using Mann-Whitney U test ======
lab.table.filtered$group <- matched_data$group[match(lab.table.filtered$person_id, matched_data$person_id)]

list.diff.lab <- list()
lab_list <- colnames(lab.table.filtered)[-c(1,ncol(lab.table.filtered))]
N = length(lab_list)

for (i in 1:N){
  lab <- lab_list[[i]]
  col_num <- which(colnames(lab.table.filtered) == lab)
  a <- wilcox.test(lab.table.filtered[,col_num] ~ as.factor(group), data = lab.table.filtered, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
  list.diff.lab[[i]] = data.frame(lab = lab,
                                  p.value = a$p.value)
  if (i %% 10 == 0) {
    print(sprintf('working on %s of %s: %.2f percent', i, N, i/N * 100))
  }
}

diff_labs_all <- do.call("rbind", list.diff.lab)
diff_labs_all$significance <- "No"
diff_labs_all$significance[diff_labs_all$p.value <= 0.05/length(lab_list)] <- "Yes"

write.csv(diff_labs_all, "differential_labs.csv")
table(diff_labs_all$significance)

# Vln plot of example labs
ggplot(lab.table.sig, aes(y =Hemoglobin..Mass.volume..in.Blood, x = as.factor(group), 
                          fill = as.factor(group))) +
  scale_fill_manual(values = c("0" = "Grey", "1" = "Salmon"))+
  geom_violin() +
  theme_classic() + 
  stat_summary(fun = "mean",
               geom = "point",
               color = "black")

ggplot(lab.table.sig, aes(y =Hematocrit..Volume.Fraction..of.Blood.by.Automated.count, x = as.factor(group), 
                          fill = as.factor(group))) +
  scale_fill_manual(values = c("0" = "Grey", "1" = "Salmon"))+
  geom_violin() +
  theme_classic() + 
  stat_summary(fun = "mean",
               geom = "point",
               color = "black")

