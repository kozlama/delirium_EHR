#################################################################################################################
# Outcomes after discharge: mortality / survival curve for control vs patients with delirium
# Generates figure 6 in manuscript
################################################################################################################

# Connect to database
# Input cohort data with demographic info
# Duration of data capture
dbExecute(con, "IF OBJECT_ID('tempdb.dbo.#tmppt', 'U') IS NOT NULL DROP TABLE #tmppt")
dbWriteTable(con, '#tmppt', del.pts)
obs_end <- dbGetQuery(con, "SELECT DISTINCT person.[person_id], obs.[observation_period_end_date]
                  FROM omop.person AS person
                  RIGHT JOIN #tmppt ON person.[person_id] = #tmppt.[person_id] 
                  LEFT JOIN omop.[observation_period] AS obs ON obs.[person_id] = #tmppt.[person_id]")
del.pts$obs_end <- obs_end$observation_period_end_date[match(del.pts$person_id, obs_end$person_id)]
del.pts$timespan <- lubridate::time_length(difftime(del.pts$obs_end, del.pts$visit_start_date), "years")

# Calculate Kaplan Meier curves and cox regression models for death for all patients =======
del.pts.all <- del.pts %>%
  filter(death_date >= visit_start_date | is.na(death_date))

del.pts.all$duration_to_death <- lubridate::time_length(difftime(del.pts.all$death_date, del.pts.all$visit_start_date), "years")
del.pts.all$timespan[!is.na(del.pts.all$duration_to_death)] <- del.pts.all$duration_to_death[!is.na(del.pts.all$duration_to_death)]

del.pts.all$status <- del.pts.all$duration_to_death
del.pts.all$status[is.na(del.pts.all$status)] <- 0 # patients with no death date considered surviving
del.pts.all$status[!is.na(del.pts.all$duration_to_death)] <- 1 # patients with a death date considered an event

del.pts.all$time <- del.pts.all$duration_to_death
del.pts.all$time[is.na(del.pts.all$duration_to_death)] <- del.pts.all$timespan[is.na(del.pts.all$duration_to_death)]

del.pts.all$time_adjusted <- del.pts.all$time
del.pts.all$time_adjusted[del.pts.all$time_adjusted > max(subset(del.pts.all, status == 1)$time)] = max(subset(del.pts.all, status == 1)$time) 
del.pts.all$status_adjusted <- del.pts.all$status
del.pts.all$status_adjusted[del.pts.all$time > max(subset(del.pts.all, status == 1)$time)] = 0

del.pts.all <- subset(del.pts.all, !is.na(del.pts.all$time_adjusted))
summary(del.pts.all$time_adjusted)

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(ggsurvfit)

survfit2(Surv(time_adjusted, status_adjusted) ~ group, data = del.pts.all) %>% 
  ggsurvfit() +
  labs(
    x = "Years",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval() +
  add_risktable() +
  theme_classic() +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent, 
    expand = c(0.01, 0)
  ) +
  scale_color_manual(values = c("grey","salmon")) +
  scale_fill_manual(values = c("grey","salmon")) +
  ylab("Percentage Survival") +
  xlab("Follow-up Time (years)") 

# COX regression model for group differences
cox1 <- coxph(Surv(time_adjusted, status_adjusted) ~ group, data = del.pts.all)
cox2 <- coxph(Surv(time_adjusted, status_adjusted) ~ group + gender + age_admit + race + service + visit_num + total_comorb + timespan, data = del.pts.all)
# cox3 <- coxph(Surv(time, status) ~ group + stay_length + ICU +died_during_admit, data = filtered_patients)
cox4 <- coxph(Surv(time_adjusted, status_adjusted) ~ group + gender + stay_length + age_admit + visit_num + total_comorb + ICU + service + died_during_admit + timespan, data = del.pts.all)

res1 <- summary(cox1)
res2 <- summary(cox2)
# res3 <- summary(cox3)
res4 <- summary(cox4)

full_res <- data.frame(matrix(ncol = 0, nrow = 3)) %>%
  dplyr::mutate(p_value = c(res1$waldtest["pvalue"], res2$waldtest["pvalue"],  res4$waldtest["pvalue"]),
                HR = c(res1$coefficients[,"exp(coef)"], res2$coefficients[1,"exp(coef)"],  res4$coefficients[1,"exp(coef)"]),
                HR.confint.lower = c(res1$conf.int[,"lower .95"],res2$conf.int[1,"lower .95"], res4$conf.int[1,"lower .95"]),
                HR.confint.upper = c(res1$conf.int[,"upper .95"], res2$conf.int[1,"upper .95"],  res4$conf.int[1,"upper .95"]))

ggplot(full_res, aes(x = row.names(full_res), y = HR)) +
  geom_errorbar(aes(ymin=HR.confint.lower, ymax=HR.confint.upper), width=.1) +
  geom_point(size = 5, shape = 18, color = "salmon") +
  theme_classic() +
  labs(y = "Hazard Ratio", x= "") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  coord_flip()
