################################################################################
# Finding matching controls for delirium cohort 
################################################################################

# Picking a random inpatient visit for controls =======
# connect to database
# import delirium patient list
# time frame of visits defined in delirium list:
delirium_patients <- delirium_patients %>%
  arrange(visit_start_date)
head(delirium_patients) # 1993-08-13
tail(delirium_patients) # 2022-12-21

# select inpatient visits during the timeframe defined by delirium cohort 
sqlstring <- sprintf("SELECT DISTINCT visit.[person_id], visit.[visit_start_date], visit.[visit_end_date], visit.[visit_occurrence_id], visit.[visit_concept_id]
                     FROM omop.[visit_occurrence] AS visit
                     WHERE visit.[visit_concept_id] LIKE '9201' AND
                     visit.[visit_start_date] < '2023-02-08' AND
                     visit.[visit_start_date] > '1993-08-13'")

ctrl_res <- dbGetQuery(con, sqlstring)

# remove patients with inpatient delirium diagnosis
ctrl_res_filtered <- ctrl_res[which(!(ctrl_res$person_id %in% delirium_patients$person_id)),]

length(unique(ctrl_res_filtered$person_id)) 

# Filter for random inpatient visit
ctrl_visit <- ctrl_res_filtered %>%
  mutate(stay_length = lubridate::time_length(difftime(ctrl_res_filtered$visit_end_date, ctrl_res_filtered$visit_start_date), "days")) %>%
  filter(visit_concept_id == 9201) %>%
  group_by(person_id) %>%
  slice_sample(n=1) %>%
  distinct(person_id, visit_start_date, .keep_all = T) %>%
  filter(stay_length >=0)

ctrl.visit.num <- data.frame(table(ctrl_res_filtered$person_id)) # calculate number of inpatient visits total per patient
ctrl_visit$visit_num <- ctrl.visit.num$Freq[match(as.factor(ctrl_visit$person_id), ctrl.visit.num$Var1)]
ctrl_visit$visit_num <- ctrl_visit$visit_num - 1
summary(ctrl_visit$visit_num)

# calculate how many visits delirium patients have had prior to their inpatient delirium visit:
del.visits <- ctrl_res[which(ctrl_res$person_id %in% delirium_patients$person_id),]
del.visits$visit_threshold <- delirium_patients$visit_start_date[match(del.visits$person_id, delirium_patients$person_id)]
del.visits.filt <- subset(del.visits, visit_end_date < visit_threshold | visit_start_date == visit_threshold)

del.visit.num <- data.frame(table(del.visits.filt$person_id))
delirium_patients$visit_num <- as.numeric(del.visit.num$Freq)[match(as.factor(delirium_patients$person_id), del.visit.num$Var1)]

ggplot(delirium_patients, aes(visit_num)) + geom_boxplot()

# merge with demographic information
dbExecute(con, "IF OBJECT_ID('tempdb.dbo.#tmppt', 'U') IS NOT NULL DROP TABLE #tmppt")
dbWriteTable(con, '#tmppt', ctrl_visit)
ctrl_demo <- dbGetQuery(con, "SELECT DISTINCT person.[person_id], person.[gender_source_value], person.[race_source_value], person.[birth_datetime],
                     death.[death_date], 
                     obs.[observation_period_start_date],
                     site.[care_site_name], 
                     visit.[care_site_id]
                     FROM omop.person AS person
                     RIGHT JOIN #tmppt ON person.[person_id] = #tmppt.[person_id]
                     LEFT JOIN omop.[visit_Occurrence] AS visit ON visit.[visit_occurrence_id] = #tmppt.[visit_occurrence_id]
                     LEFT JOIN omop.death AS death ON person.[person_id] = death.[person_id]
                     LEFT JOIN omop.[observation_period] AS obs ON obs.[person_id] = #tmppt.[person_id]
                     LEFT JOIN omop.[care_site] AS site ON visit.[care_site_id] = site.[care_site_id]")

ctrl_merged <- ctrl_demo %>%
  filter(!is.na(birth_datetime)) %>%
  rename(gender = gender_source_value,
         race = race_source_value) %>%
  merge(ctrl_visit, by = "person_id")  %>%
  filter(gender == "Female" | gender == "Male",
         stay_length < 500)

# calculate age at admission
ctrl_merged$age_admit <- lubridate::time_length(difftime(ctrl_merged$visit_start_date, ctrl_merged$birth_datetime), "years")

# calculate duration in healthcare system
ctrl_merged$service <- lubridate::time_length(difftime(ctrl_merged$visit_start_date, ctrl_merged$observation_period_start_date), "years")

# Categorize whether died during admission or not
ctrl_merged$died_during_admit <- "No"
ctrl_merged$died_during_admit[which(ctrl_merged$death_date <= ctrl_merged$visit_end_date)] <- "Yes"
table(ctrl_merged$died_during_admit)

# clean up race information
ctrl_merged$race[which(ctrl_merged$race == "Declined")] <- "Unknown/Declined"
ctrl_merged$race[which(ctrl_merged$race == "Unknown")] <- "Unknown/Declined"
ctrl_merged$race[which(ctrl_merged$race == "")] <- "Unknown/Declined"
ctrl_merged$race[which(ctrl_merged$race == "Other Pacific Islander")] <- "Native Hawaiian or Other Pacific Islander"
ctrl_merged$race[which(ctrl_merged$race == "Native Hawaiian")] <- "Native Hawaiian or Other Pacific Islander"

table(ctrl_merged$race)
table(ctrl_merged$gender)

ctrl_merged$ICU <- delirium_patients$ICU[match(ctrl_merged$care_site_id, delirium_patients$care_site_id)]

ctrl_merged[is.na(ctrl_merged$ICU),]$ICU <- ctrl_care_site$ICU[match(ctrl_merged[is.na(ctrl_merged$ICU),]$care_site_name, ctrl_care_site$Site_name)]

a <- subset(ctrl_merged, !is.na(death_date))
b <- subset(a, visit_end_date > death_date)

ctrl_merged <- subset(ctrl_merged, !person_id %in% b$person_id)
summary(ctrl_merged$stay_length)
ctrl_merged <- ctrl_merged %>%
  filter(service <= age_admit)

# merge with delirium cohort
delirium_group <- delirium_patients %>%
  select(person_id, gender, race, birth_datetime, death_date, visit_start_date,
         visit_end_date, visit_occurrence_id, stay_length, age_admit, died_during_admit, service, visit_num, care_site_name, ICU) %>%
  mutate(visit_num = visit_num - 1,
         group = 1,
         birth_datetime = as.Date(birth_datetime),
         death_date = as.Date(death_date),
         visit_start_date = as.Date(visit_start_date),
         visit_end_date = as.Date(visit_end_date))

ctrl_group <- ctrl_merged %>%
  mutate(stay_length = lubridate::time_length(difftime(ctrl_merged$visit_end_date, ctrl_merged$visit_start_date), "days")) %>%
  select(person_id, gender, race, birth_datetime, death_date, visit_start_date,
         visit_end_date, visit_occurrence_id, stay_length, age_admit, died_during_admit, service, visit_num, care_site_name, ICU) %>%
  mutate(group = 0,
         visit_occurrence_id = as.integer(visit_occurrence_id))

full_cohort <- rbind(delirium_group, ctrl_group)

# number of comorbidities
dbExecute(con, "IF OBJECT_ID('tempdb.dbo.#tmpfull', 'U') IS NOT NULL DROP TABLE #tmpmatched")
dbWriteTable(con, '#tmpfull', full_cohort)
num.diag <- dbGetQuery(con, "SELECT DISTINCT diagnosis.[person_id], 
                         diagnosis.[condition_concept_id], diagnosis.[condition_start_date]
                         FROM omop.[condition_occurrence] AS diagnosis
                         RIGHT JOIN #tmpfull ON diagnosis.[person_id] = #tmpfull.[person_id]
                         WHERE diagnosis.[condition_start_date] < #tmpfull.[visit_start_date]")

# Filter for first time diagnoses
diag.unique <- num.diag %>% 
  distinct(person_id, condition_concept_id, .keep_all = TRUE)
length(unique(diag.unique$person_id)) # 377319 unique patients

# filter out delirium diagnosis 373995
diag.filtered <- subset(diag.unique, condition_concept_id != 373995)

# calculate total diagnoses 
total.num.diag <- data.frame(table(diag.filtered$person_id))
full_cohort$total_comorb <- total.num.diag$Freq[match(full_cohort$person_id, total.num.diag$Var1)]
# write.csv(full_cohort, "OMOP_full_cohort_before_matching.csv")

# use MatchItR to identify appropriate cohort
library(MatchIt)
# No matching; constructing a pre-match matchit object
full_cohort$visit_num[which(is.na(full_cohort$visit_num))]<-0 # NA value for visit_num
full_cohort$total_comorb[which(is.na(full_cohort$total_comorb))]<-0 
full_cohort$ICU[which(is.na(full_cohort$ICU))] <- "Unknown"

# 1:1 NN PS matching w/o replacement
m.out1 <- matchit(group ~ age_admit + race + gender + died_during_admit + stay_length + service + visit_num + total_comorb + ICU,
                  data = full_cohort,
                  method = "nearest", 
                  distance = "glm",
                  ratio = 1)
# summary(m.out1, un = F)
# plot(m.out1, type = "jitter", interactive = FALSE)
# plot(m.out1, type = "qq", interactive = FALSE,
#      which.xs = c("age_admit", "total_comorb"))
plot(summary(m.out1))

matched_data <- match.data(m.out1)
