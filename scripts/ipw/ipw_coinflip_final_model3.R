args <- commandArgs(trailingOnly=TRUE)

# Tutorials
# https://fukamilab.github.io/BIO202/04-C-zero-data.html
# https://rstudio-pubs-static.s3.amazonaws.com/11785_e937a5f782864e1c9053d90b2b66c796.html
# https://biol609.github.io/lectures/13_zinfl.html
# https://www.sciencedirect.com/science/article/pii/S0085253815529906


library(tidyverse)
library(survival)
library(splines)
library(rms)
library(splitstackshape)
library(Hmisc)


setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/")



# Testing
#data_bootstrap = read_tsv("/data/projects/deep_phenotyping/transfusions_bth_simple/data/processed/data_longitudinal_mortality.tsv")

#data_bootstrap %>% group_by(Trans_year) %>% distinct(unique_id)  %>% count()
#n_distinct(data_bootstrap$unique_id)

# load data
data_bootstrap <- read_tsv(args[1])
data_bootstrap <- data_bootstrap %>% arrange(unique_id,time)


# Make patient sex binary
data_bootstrap$patient_cpr_sex = ifelse(data_bootstrap$patient_cpr_sex == "M", 1, 0)

# Define exposure
data_bootstrap["exposure"] <- data_bootstrap[as.character(args[2])]

# Select columns to use
data_bootstrap <- data_bootstrap %>% 
                   dplyr::select(unique_id,event,time,exposure,patient_cpr_sex,n_transfusions,total_transfusions,
                            acute_package,patient_charlson_score,patient_age_trans,Trans_year,sin_month,cos_month,
                            AB0_patient,Rhesus_patient,bt_difference,hospital_navn)


# Make total_transfusions not included transfusions on this day k
data_bootstrap["total_transfusions_all"] = data_bootstrap["total_transfusions"]
data_bootstrap["total_transfusions"] = data_bootstrap["total_transfusions_all"] - data_bootstrap["n_transfusions"]

# Define non exposure and lags
data_bootstrap$nonexposure <- data_bootstrap$n_transfusions - data_bootstrap$exposure
data_bootstrap <- data_bootstrap %>% mutate(log_exposure = log(exposure + 1),
                                            log_nonexposure = log(exposure + 1))
data_bootstrap <- data_bootstrap %>% 
                    group_by(unique_id) %>%
                    arrange(time,.by_group = TRUE) %>%
                    mutate(total_exposure = cumsum(exposure),
                           total_nonexposure = cumsum(nonexposure))

# lag n_transfusions
data_bootstrap <- data_bootstrap %>% group_by(unique_id) %>% 
                            mutate(lag1_n_transfusions = lag(n_transfusions),
                                    lag2_n_transfusions = lag(n_transfusions,2))
data_bootstrap <- data_bootstrap %>% mutate(lag1_n_transfusions = replace_na(lag1_n_transfusions,0),
                                            lag2_n_transfusions = replace_na(lag2_n_transfusions,0))

# Ratio and difference between two treatments
data_bootstrap <- data_bootstrap %>% mutate(total_rbc_ratio = total_exposure/total_transfusions_all,
                                            total_rbc_ratio2 = total_nonexposure/total_transfusions_all)

data_bootstrap <- data_bootstrap %>% mutate(total_ratio_diff = total_rbc_ratio - total_rbc_ratio2)
#data_bootstrap <- data_bootstrap %>% mutate(total_exposure_diff = total_exposure - total_nonexposure)

################ PART 1 ####################
# First part of model on patient that acutally received a transfusion #
# This is the probability of the number of combination of fresh and old received #

data_trans <- data_bootstrap %>% filter(n_transfusions > 0)

# Make part for below thresholds
below <- uncount(data_trans, exposure)
below <- below %>% mutate(exposure_binary = 1)

# Make part above thresholds
#data_trans$exposure_above
data_trans <- data_trans %>% mutate(exposure_above = n_transfusions - exposure)
above <- uncount(data_trans, exposure_above)
above <- above %>% mutate(exposure_binary = 0)

# Stack above and below
data_trans <- bind_rows(below, above)
rm(below)
rm(above)

# Dont include time as time is not confouding on the likelihood of receiving fresh blood..
p.denom <- glm(exposure_binary==1 ~ 
                    rcs(patient_charlson_score,5) +
                    rcs(patient_age_trans,5) +
                    as.factor(patient_cpr_sex) +
                    rcs(Trans_year,3) +
                    rcs(sin_month,3) +
                    rcs(cos_month,3) +
                    as.factor(AB0_patient) +
                    as.factor(Rhesus_patient) +
                    rcspline.eval(bt_difference,knots=c(0,5,10,20,30)) +
                    rcspline.eval(total_transfusions,knots=c(0,5,10,20,30)) +
                    rcspline.eval(n_transfusions,knots=c(0,5,10,20,30)) +
                    as.factor(hospital_navn), data=data_trans, family = "binomial")


# Predict chance of receiving a fresh RBC
data_bootstrap$pd.exposure <- predict(p.denom, data_bootstrap, type="response")
# Probabilty of receiving the number of fresh transfusions received using the binomial dist
data_bootstrap <- data_bootstrap %>% 
                        rowwise() %>% 
                        mutate(pd.exposure = dbinom(exposure, size=n_transfusions, prob=pd.exposure))
#data_bootstrap <- data_bootstrap %>% mutate(pd.exposure = pmap_dbl(list(n=exposure, x = n_transfusions, y = pd.exposure), fun))


# Fit numerator
#p.num <- glm(exposure_binary==1 ~ 1, data=data_trans, family = "binomial")
p.num <- glm(exposure_binary==1 ~ rcspline.eval(total_transfusions,knots=c(0,5,10,20,30)), data=data_trans, family = "binomial")


# Predict chance of receiving a fresh RBC
data_bootstrap$pn.exposure <- predict(p.num, data_bootstrap, type="response")
# Probabilty of receiving the number of fresh transfusions received using the binomial dist
data_bootstrap <- data_bootstrap %>% rowwise() %>%
                    mutate(pn.exposure = dbinom(exposure, size=n_transfusions, prob=pn.exposure))
#data_bootstrap <- data_bootstrap %>% mutate(pn.exposure = pmap_dbl(list(n=exposure, x = n_transfusions, y = pn.exposure), fun))

data_bootstrap$sw.exposure <- data_bootstrap$pn.exposure/data_bootstrap$pd.exposure



# Only apply truncation once once the final weights are meassured

# First weight truncation on day k
data_bootstrap$sw.full <- data_bootstrap$sw.exposure

# time varying ipw
data_bootstrap <- data_bootstrap %>% group_by(unique_id) %>% arrange(time,.by_group = TRUE) %>%
                    mutate(sw.full = cumprod(sw.full))
#summary(data_bootstrap$sw.full)

# Weight truncation on cumprod ipw
q1th <- quantile(data_bootstrap$sw.full ,0.02)[[1]]
q99th <- quantile(data_bootstrap$sw.full ,0.98)[[1]]
data_bootstrap <- data_bootstrap %>% mutate(sw.full_trunc = if_else(sw.full  < q1th, q1th, sw.full ))
data_bootstrap <- data_bootstrap %>% mutate(sw.full_trunc = if_else(sw.full  > q99th, q99th, sw.full_trunc))
#summary(data_bootstrap$sw.full_trunc)
rm(data_trans)


########################################################################################
# Acute package 
# IPW for censoring
p.denom <- glm(acute_package==0 ~ 
                    rcs(patient_charlson_score,5) +
                    rcs(patient_age_trans,5) +
                    as.factor(patient_cpr_sex) +
                    rcs(Trans_year,3) +
                    rcs(sin_month,3) +
                    rcs(cos_month,3) +
                    rcspline.eval(total_exposure,knots=c(0,5,10,20,30)) +
                    rcspline.eval(total_nonexposure,knots=c(0,5,10,20,30)) +
                    rcspline.eval(total_ratio_diff,knots=c(-1,-0.5,0,0.5,1)) +
                    rcspline.eval(n_transfusions,knots=c(0,5,10,20,30)) +
                    as.factor(AB0_patient) +
                    as.factor(Rhesus_patient) +
                    as.factor(hospital_navn) +
                    time + I(time^2), data=data_bootstrap, family = "binomial")

#p.num <- glm(acute_package==0 ~ time + I(time^2), data=data_bootstrap, family = "binomial")
p.num <- glm(acute_package==0 ~ time + I(time^2) + 
                    rcspline.eval(total_exposure,knots=c(0,5,10,20,30)) +
                    rcspline.eval(total_nonexposure,knots=c(0,5,10,20,30)) +
                    rcspline.eval(total_ratio_diff,knots=c(-1,-0.5,0,0.5,1)), data=data_bootstrap, family = "binomial")

# Predict exposure
data_bootstrap$pd.exposure <- predict(p.denom, data_bootstrap, type="response")
data_bootstrap$pn.exposure <- predict(p.num, data_bootstrap, type="response")

# Censoring happens on the next day, therefore this step doesnt apply - not true
data_bootstrap <- data_bootstrap %>% mutate(pn.exposure = if_else(acute_package==0, pn.exposure, (1-pn.exposure)),
                                            pd.exposure = if_else(acute_package==0, pd.exposure, (1-pd.exposure)))

data_bootstrap$sw.acute <- data_bootstrap$pn.exposure/data_bootstrap$pd.exposure
#summary(data_bootstrap$sw.acute)


# Cumulated product of weight up til time j per patient i
data_bootstrap <- data_bootstrap %>% group_by(unique_id) %>% arrange(time,.by_group = TRUE) %>%
                    mutate(sw.acute = cumprod(sw.acute))

q1th <- quantile(data_bootstrap$sw.acute ,0.02)[[1]]
q99th <- quantile(data_bootstrap$sw.acute ,0.98)[[1]]
data_bootstrap <- data_bootstrap %>% mutate(sw.acute_trunc = if_else(sw.acute  < q1th, q1th, sw.acute ))
data_bootstrap <- data_bootstrap %>% mutate(sw.acute_trunc = if_else(sw.acute  > q99th, q99th, sw.acute_trunc))



#data_bootstrap$sw = data_bootstrap$sw.full_trunc * data_bootstrap$sw.acute
data_bootstrap$sw = data_bootstrap$sw.full_trunc * data_bootstrap$sw.acute_trunc


# Model 3
ipw.model <- glm(event==0 ~ rcspline.eval(total_ratio_diff,knots=c(-1,-0.5,0,0.5,1)) * time +
                             rcspline.eval(total_ratio_diff,knots=c(-1,-0.5,0,0.5,1)) * I(time^2) +
                             rcspline.eval(total_exposure,knots=c(0,5,10,20,30)) * time +
                             rcspline.eval(total_exposure,knots=c(0,5,10,20,30)) * I(time^2) +
                             rcspline.eval(total_nonexposure,knots=c(0,5,10,20,30)) * time +
                             rcspline.eval(total_nonexposure,knots=c(0,5,10,20,30)) * I(time^2),
              family=binomial(), weight = sw, data=data_bootstrap)


data_bootstrap_fresh <- data_bootstrap %>% 
                            dplyr::select(unique_id,time,n_transfusions,total_transfusions,total_transfusions_all) %>%
                            mutate(exposure_diff = n_transfusions,
                                      exposure = n_transfusions,
                                      nonexposure = 0,
                                      total_exposure = total_transfusions_all,
                                      total_nonexposure = 0,
                                      rbc_ratio = 1,
                                      total_rbc_ratio = 1,
                                      total_rbc_ratio2 = 0,
                                      total_ratio_diff = 1,
                                      total_exposure_diff = total_transfusions_all)
data_bootstrap_old <- data_bootstrap %>% 
                            dplyr::select(unique_id,time,n_transfusions,total_transfusions,total_transfusions_all) %>%
                            mutate(exposure_diff = (-n_transfusions),
                                    exposure = 0,
                                    nonexposure = n_transfusions,
                                    total_exposure = 0,
                                    total_nonexposure = total_transfusions_all,
                                    rbc_ratio = 0,
                                    total_rbc_ratio = 0,
                                    total_rbc_ratio2 = 1,
                                    total_ratio_diff = -1,
                                    total_exposure_diff = (-total_transfusions_all))


#data_bootstrap_old$exposure_diff
#data_bootstrap %>% group_by(n_transfusions) %>% count()
data_bootstrap_fresh["surv_prob"] <- predict(ipw.model, data_bootstrap_fresh, type="response")
data_bootstrap_old["surv_prob"] <- predict(ipw.model, data_bootstrap_old, type="response")
data_bootstrap["surv_prob"] <- predict(ipw.model, data_bootstrap, type="response")

gf.old.surv <- data_bootstrap_old %>% 
                    dplyr::select(unique_id,time,surv_prob) %>% 
                    mutate(time = as.numeric(time),surv_prob = as.numeric(surv_prob)) %>% 
                    group_by(unique_id) %>% 
                    arrange(time,.by_group = TRUE) %>%
                    mutate(surv = cumprod(surv_prob))
gf.fresh.surv <- data_bootstrap_fresh %>% 
                    dplyr::select(unique_id,time,surv_prob) %>% 
                    mutate(time = as.numeric(time),surv_prob = as.numeric(surv_prob)) %>% 
                    group_by(unique_id) %>% 
                    arrange(time,.by_group = TRUE) %>%
                    mutate(surv = cumprod(surv_prob))
gf.NC.surv <- data_bootstrap %>% 
                    dplyr::select(unique_id,time,surv_prob) %>% 
                    mutate(time = as.numeric(time),surv_prob = as.numeric(surv_prob)) %>% 
                    group_by(unique_id) %>% 
                    arrange(time,.by_group = TRUE) %>%
                    mutate(surv = cumprod(surv_prob))


gf.old.surv <- gf.old.surv %>% ungroup() %>% dplyr::select(-c(unique_id))
gf.fresh.surv <- gf.fresh.surv %>% ungroup() %>% dplyr::select(-c(unique_id))
gf.NC.surv <- gf.NC.surv %>% ungroup() %>% dplyr::select(-c(unique_id))


gf.surv_old <- gf.old.surv %>% 
                    group_by(time) %>% 
                    mutate(mean_surv = mean(surv),exposure = "Older RBC only") 
gf.surv_fresh <- gf.fresh.surv %>% 
                    group_by(time) %>% 
                    mutate(mean_surv = mean(surv), exposure = "Fresher RBC only")
gf.survNC <- gf.NC.surv %>% 
                    group_by(time) %>% 
                    mutate(mean_surv = mean(surv), exposure = "Natural course")


gf.surv_old <- gf.surv_old %>% select(time,mean_surv,exposure) %>%
                distinct(time, .keep_all= TRUE)
gf.surv_fresh <- gf.surv_fresh %>% select(time,mean_surv,exposure) %>%
                distinct(time, .keep_all= TRUE)
gf.survNC <- gf.survNC %>% select(time,mean_surv,exposure) %>%
                distinct(time, .keep_all= TRUE)


# p <- ggplot() + 
#   geom_line(data = gf.surv_old, aes(y = mean_surv,x=time,colour = exposure)) + 
#   geom_line(data = gf.surv_fresh, aes(y = mean_surv,x=time,colour = exposure)) +
#   geom_line(data = gf.survNC, aes(y = mean_surv,x=time,colour = exposure)) + 
#   xlab("Days after transfusion") + 
#   scale_x_continuous(limits = c(0, 28), breaks=seq(0,28,7)) +
#   ylab("Survival Probability") + 
#   ggtitle("Adjusted Survival Curves") + 
#   labs(colour="Trial arm",fill="Trial arm") +
#   theme_bw() 
# p

write_tsv(gf.surv_fresh,args[3])
write_tsv(gf.surv_old,args[4])
write_tsv(gf.survNC,args[5])






