#################################################################################################################
# Defining a delirium cohort first before matching with control cohort
# Criteria: extract patients with only delirium diagnosis (unspecified) during an inpatient visit
# Select only the first inpatient visit where they had a delirium diagnosis
# Filtered out: patients with no birthdates and nonbinary/unknown sex individuals (only accounted for 5 patients)
#################################################################################################################

# Connect to UCSF database
# Standard concept ID for "delirium" diagnosis 373995

# Extract inpatient visits with diagnosis of some form of delirium ======
sqlstring <- sprintf("SELECT DISTINCT diagnosis.[person_id], diagnosis.[condition_concept_id], diagnosis.[condition_start_date], diagnosis.[condition_end_date],diagnosis.[visit_occurrence_id],
                     concept.[concept_name],
                     visit.[visit_start_date], visit.[visit_end_date], visit.[visit_concept_id]
                     FROM [omop].[condition_occurrence] AS diagnosis
                     INNER JOIN omop.concept AS concept
                     ON diagnosis.[condition_concept_id] = concept.[concept_id]
                     
                     INNER JOIN omop.[visit_occurrence] AS visit
                     ON diagnosis.[visit_occurrence_id] = visit.[visit_occurrence_id] AND
                     diagnosis.[person_id] = visit.[person_id]
                     
                     WHERE visit.[visit_concept_id] LIKE '9201' AND 
                     diagnosis.[condition_concept_id] LIKE '373995'")

res <- dbGetQuery(con, sqlstring)

# Filter for first inpatient delirium diagnosis
# Criteria: first visit_start_date and if duplicate of the same visit, select one with the longer stay_length
res_filtered <- res %>%
  mutate(stay_length = lubridate::time_length(difftime(res$visit_end_date, res$visit_start_date), "days")) %>%
  filter(visit_concept_id == 9201) %>%
  arrange(visit_start_date, desc(stay_length)) %>%
  group_by(person_id) %>%
  top_n(n=-1, wt = visit_start_date) %>%
  top_n(n=1, wt = stay_length) %>%
  distinct(person_id, visit_start_date, .keep_all = T) %>%
  filter(stay_length >= 0)

# merge with demographic information =======
dbExecute(con, "IF OBJECT_ID('tempdb.dbo.#tmppt', 'U') IS NOT NULL DROP TABLE #tmppt")
dbWriteTable(con, '#tmppt', res_filtered)
res_demo <- dbGetQuery(con, "SELECT DISTINCT person.[person_id], person.[gender_source_value], person.[race_source_value], person.[birth_datetime],
                  death.[death_date],
                  obs.[observation_period_start_date], obs.[observation_period_end_date],
                  site.[care_site_name], 
                  visit.[care_site_id]
                  FROM omop.person AS person
                  RIGHT JOIN #tmppt ON person.[person_id] = #tmppt.[person_id] 
                  LEFT JOIN omop.[visit_Occurrence] AS visit ON visit.[visit_occurrence_id] = #tmppt.[visit_occurrence_id]
                  LEFT JOIN omop.death AS death ON death.[person_id] = #tmppt.[person_id]
                  LEFT JOIN omop.[observation_period] AS obs ON obs.[person_id] = #tmppt.[person_id]
                  LEFT JOIN omop.[care_site] AS site ON visit.[care_site_id] = site.[care_site_id]")

res_merged <- merge(res_filtered,res_demo, by="person_id")

# calculate age at admission
res_merged$age_admit <- lubridate::time_length(difftime(res_merged$visit_start_date, res_merged$birth_datetime), "years")

# calculate duration in healthcare system
res_merged$service <- lubridate::time_length(difftime(res_merged$visit_start_date, res_merged$observation_period_start_date), "years")

# Categorize whether died during admission or not
res_merged$died_during_admit <- "No"
res_merged$died_during_admit[which(res_merged$death_date <= res_merged$visit_end_date)] <- "Yes"
table(res_merged$died_during_admit)

# rename columns and clean up data 
# like remove patients with no birth dates and remove nonbinary / unknown sex
res_clean <- res_merged %>%
  rename(gender = gender_source_value,
         race = race_source_value) %>%
  filter(!is.na(birth_datetime),
         gender == "Female" | gender == "Male",
         service <= age_admit,
         stay_length < 1000) 

res_clean$race[which(res_clean$race == "Declined")] <- "Unknown/Declined"
res_clean$race[which(res_clean$race == "Unknown")] <- "Unknown/Declined"
res_clean$race[which(res_clean$race == "")] <- "Unknown/Declined"
res_clean$race[which(res_clean$race == "Other Pacific Islander")] <- "Native Hawaiian or Other Pacific Islander"

table(res_clean$race)
table(res_clean$gender)

a <- subset(res_clean, !is.na(death_date))
b <- subset(a, visit_end_date > death_date)

res_clean <- subset(res_clean, !person_id %in% b$person_id)
summary(res_clean$stay_length)

# identify the type of care environment
site_acuity <- read.csv("Care_sites_delirium.csv", header=T, row.names = 1)
site_acuity$ICU <- "No"
site_acuity$ICU[site_acuity$Acuity_level == "ICU"] <- "Yes"
site_acuity$ICU[is.na(site_acuity$Acuity_level)] <- "NA"

res_clean$service_type <- site_acuity$Service_type[match(res_clean$care_site_name, site_acuity$Care_site_name)]
res_clean$acuity_level <- site_acuity$Acuity_level[match(res_clean$care_site_name, site_acuity$Care_site_name)]
res_clean$ICU <- site_acuity$ICU[match(res_clean$care_site_name, site_acuity$Care_site_name)]


