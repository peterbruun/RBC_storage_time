library(tidyverse)
library(ggplot2)
library(cowplot)
library(kableExtra)
library(glue)
library(gt)


models <- list("2021-10-25_model1_interact_all_trans/", 
               "2021-10-25_model3_all_trans_knots1_3_5_9_14/", 
               "2021-10-25_model4_big_knots_total_trans/", 
               "2021-10-25_model5_0_10_20_30_40knots_total_trans/",
               "2021-10-25_model5_0_10_20_30_40knots_total_trans/thrombo_event/", 
               "2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/")

models <- list("2021-10-25_model5_0_10_20_30_40knots_total_trans/thrombo_event/")

dir = "/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/"

# loop version 1
for (model in models) {
       setwd(paste(dir,model,sep=""))



# All-cause mortality
#setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-25_model1_interact_all_trans/")
#setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-11-06_model2_splinesFix/")
# sensitivity model 1
#setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-25_model3_all_trans_knots1_3_5_9_14/")
# Edgren model
#setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-25_model4_big_knots_total_trans/")
#setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-25_model5_0_10_20_30_40knots_total_trans/")
#setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/")



# Composite event
# Thombo
#setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-18_model1_interact_all_trans/thrombo_event/")
#setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-18_model2_total_exposure/thrombo_event/")
#setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-18_model3_all_trans_knots1_3_5_9_14/thrombo_event/")
#setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-18_model4_big_knots_total_trans/thrombo_event/")
#setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-25_model5_0_10_20_30_40knots_total_trans/thrombo_event")
#setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-18_model6_0_10_20_30_40knots_total_trans_noInteract/thrombo_event")



follow_up = 28

# Load results
# Mortality
ipw.exposureNC7 = read_tsv("processed/random_treatment/survival_NC_estimates.tsv")
ipw.exposure7 = read_tsv("processed/random_treatment/survival_fresh_estimates.tsv")
ipw0.exposure7 = read_tsv("processed/random_treatment/survival_old_estimates.tsv")


# Get the column number according to how many iterations have been run
first_col = match("cumprod1",names(ipw.exposureNC7))
last_col = ncol(ipw.exposureNC7)


# # Add time 0
ipw.exposure7 <- ipw.exposure7 %>% mutate(time = time +1)
ipw.exposure7 <- rbind(c(0,rep(1,last_col-1)),ipw.exposure7)
ipw.exposure7 <- ipw.exposure7 %>% mutate(across(c(time,starts_with("cumprod")),as.numeric))
# Add time 0
ipw0.exposure7 <- ipw0.exposure7 %>% mutate(time = time +1)
ipw0.exposure7 <- rbind(c(0,rep(1,last_col-1)),ipw0.exposure7)
ipw0.exposure7 <- ipw0.exposure7 %>% mutate(across(c(time,starts_with("cumprod")),as.numeric))

# Add time 0
ipw.exposureNC7 <- ipw.exposureNC7 %>% mutate(time = time +1)
ipw.exposureNC7 <- rbind(c(0,rep(1,last_col-1)),ipw.exposureNC7)
ipw.exposureNC7 <- ipw.exposureNC7 %>% mutate(across(c(time,starts_with("cumprod")),as.numeric))


ipw.exposureNC7["exposure"] = "Natural course"
ipw.exposureNC7 <- ipw.exposureNC7 %>% relocate(exposure)

ipw.exposure7["exposure"] = "Fresher RBCs"
ipw.exposure7 <- ipw.exposure7 %>% relocate(exposure)

ipw0.exposure7["exposure"] = "Older RBCs"
ipw0.exposure7 <- ipw0.exposure7 %>% relocate(exposure)


# Get the column number according to how many iterations have been run
first_col = match("cumprod1",names(ipw.exposureNC7))
last_col = ncol(ipw.exposureNC7)


# pivot_longer
ipw.survNC7 <- ipw.exposureNC7 %>% rowwise() %>%
  mutate(mean = mean(c_across(starts_with("cumprod"))),
         stDev= sd(c_across(starts_with("cumprod")))) %>%
  mutate(lwr = mean - (qnorm(0.975) * stDev),
         upr = mean + (qnorm(0.975) * stDev)) %>%
         dplyr::select(c(time,exposure,mean,stDev,lwr,upr))

ipw.surv7 <- ipw.exposure7 %>% rowwise() %>%
  mutate(mean = mean(c_across(starts_with("cumprod"))),
         stDev= sd(c_across(starts_with("cumprod")))) %>%
  mutate(lwr = mean - (qnorm(0.975) * stDev),
         upr = mean + (qnorm(0.975) * stDev)) %>%
         dplyr::select(c(time,exposure,mean,stDev,lwr,upr))

ipw0.surv7 <- ipw0.exposure7 %>% rowwise() %>%
  mutate(mean = mean(c_across(starts_with("cumprod"))),
         stDev= sd(c_across(starts_with("cumprod")))) %>%
  mutate(lwr = mean - (qnorm(0.975) * stDev),
         upr = mean + (qnorm(0.975) * stDev)) %>%
         dplyr::select(c(time,exposure,mean,stDev,lwr,upr))


# Combine plots

p1 <- ggplot() + 
  geom_line(data = ipw.survNC7,aes(x=time,y = mean, colour = exposure)) + 
  geom_line(data = ipw.surv7,aes(x=time,y = mean, colour = exposure)) + 
  geom_line(data = ipw0.surv7,aes(x=time,y = mean, colour = exposure)) + 
  geom_ribbon(data = ipw.survNC7,aes(x=time,ymin = lwr, ymax = upr, fill = exposure, color = NULL), alpha = .15) +
  geom_ribbon(data = ipw.surv7,aes(x=time,ymin = lwr, ymax = upr, fill = exposure, color = NULL), alpha = .15) +
  geom_ribbon(data = ipw0.surv7,aes(x=time,ymin = lwr, ymax = upr, fill = exposure, color = NULL), alpha = .15) +
  xlab("Days after baseline-transfusion") + 
  scale_x_continuous(expand=c(0,0), limits = c(0, follow_up), breaks=seq(0,follow_up,7)) +
  scale_y_continuous(expand=c(0,0), limits= c(0.85, 1), breaks=seq(0.85, 1, 0.05)) +
  ylab("Survival Probability") + 
  ggtitle("Manipulated treatment") + 
  labs(colour="RBC shelf life",fill="RBC shelf life") +
  theme_bw() + 
  theme(legend.position="bottom") +
         scale_fill_manual(values = c("#1b9e77","#7570b3","#d95f02")) +
         scale_colour_manual(values = c("#1b9e77","#7570b3","#d95f02"))


# -- Risk ratio compared to other arm -- #

ipw.hzr7 <-  (ipw.exposure7[first_col:last_col]) / (ipw0.exposure7[first_col:last_col])

# Calculate mean and standard deviation
ipw.hzr7 <- ipw.hzr7 %>% rowwise() %>%
  mutate(mean_survhzr = mean(c_across(starts_with("cumprod"))),
         stDev= sd(c_across(starts_with("cumprod")))) %>%
  mutate(lwr = mean_survhzr - (qnorm(0.975) * stDev),
         upr = mean_survhzr + (qnorm(0.975) * stDev)) %>%
         dplyr::select(c(mean_survhzr,stDev,lwr,upr))

ipw.hzr7["exposure7"] = "7 Days"

# Reset time column
ipw.hzr7$time = as.integer(rownames(ipw.hzr7))-1


# plot survial hazard ratio

p2 <- ggplot() + 
  geom_line(data = ipw.hzr7, aes(x=time, y = mean_survhzr),colour="#2166ac") +  
  geom_ribbon(data = ipw.hzr7, aes(x=time, y = mean_survhzr,ymin = lwr, ymax = upr), fill = "#2166ac", alpha = .15) +
  xlab("") + 
  scale_x_continuous(limits = c(-0.01, follow_up), breaks=seq(0,follow_up,7)) +
  #scale_y_continuous(limits = c(0.95, 1.6)) +
  ylab("Risk Ratio") + 
  ggtitle("Manipulated treatment") + 
  #labs(colour="RBC of long storage (<10 days)") +
  theme_bw() + 
  theme(legend.position="bottom")

tbl7 <- ipw.hzr7 %>% 
  filter(time %in% c(7,14,21,28)) %>%
  mutate(Estimate_CI_w1 = paste(format(round(mean_survhzr,4),scientific = FALSE)," (",round(lwr,4),", ",round(upr,4),")",sep="")) %>%
  mutate(Estimate_CI_w1 = cell_spec(Estimate_CI_w1,"html", bold = ifelse((lwr>0 & upr>0) | (lwr<0 & upr<0),T,F))) %>%
  dplyr::select(time,Estimate_CI_w1)


# data_list = list(tbl7, tbl14,tbl21,tbl28)
# tbl <- data_list %>% reduce(left_join, by = "time")

kbl(tbl7,"html",escape = F,col.names=c("Days after transfusion","Random Treatment")) %>% 
  kable_classic("striped") %>%
  add_header_above(c(" " = 1, "Risk Ratio (95%CI)" = 1)) %>%
  save_kable(file = "trial_arms_tbl_risk_ratio.html", self_contained = T)



# -- Survival difference compared to other arm -- #

ipw.hzr7 <-  ((ipw.exposure7[first_col:last_col]) - (ipw0.exposure7[first_col:last_col])) * 100

# Calculate mean and standard deviation
ipw.hzr7 <- ipw.hzr7 %>% rowwise() %>%
  mutate(mean_survhzr = mean(c_across(starts_with("cumprod"))),
         stDev= sd(c_across(starts_with("cumprod")))) %>%
  mutate(lwr = mean_survhzr - (qnorm(0.975) * stDev),
         upr = mean_survhzr + (qnorm(0.975) * stDev)) %>%
         dplyr::select(c(mean_survhzr,stDev,lwr,upr))

ipw.hzr7["exposure7"] = "7 Days"

# Reset time column
ipw.hzr7$time = as.integer(rownames(ipw.hzr7))-1


# plot survial difference

p3 <- ggplot() + 
  geom_line(data = ipw.hzr7, aes(x=time, y = mean_survhzr),colour="#2166ac") +  
  geom_ribbon(data = ipw.hzr7, aes(x=time, y = mean_survhzr,ymin = lwr, ymax = upr), fill = "#2166ac", alpha = .15) +
  xlab("Days after baseline-transfusion") + 
  scale_x_continuous(expand=c(0,0),limits = c(-0.01, follow_up), breaks=seq(0,follow_up,7)) +
  scale_y_continuous(expand=c(0,0), limits=c(-1.5, 1.5), breaks=seq(-1,1,1)) +
  ylab("Risk difference (%-points)") + 
  ggtitle("Manipulated treatment") + 
  #labs(colour="RBC of long storage (<10 days)") +
  theme_bw() + 
  theme(legend.position="bottom")


p <- plot_grid(p1, p3, labels = "AUTO")
save_plot("random_treatment_combined.pdf", p, ncol = 2,nrow = 1)


# in tables


# tbl7 <- ipw.hzr7 %>% mutate(time = time + 1) %>%
#   filter(time %in% c(7,14,21,28)) %>%
#   mutate(Estimate_CI_w1 = paste(format(round(mean_survhzr,2),scientific = FALSE)," (",round(lwr,2),", ",round(upr,2),")",sep="")) %>%
#   mutate(Estimate_CI_w1 = cell_spec(Estimate_CI_w1,"html", bold = ifelse((lwr>0 & upr>0) | (lwr<0 & upr<0),T,F))) %>%
#   dplyr::select(time,Estimate_CI_w1)
# tbl14 <- ipw.hzr14 %>% mutate(time = time + 1) %>%
#   filter(time %in% c(7,14,21,28)) %>%
#   mutate(Estimate_CI_w2 = paste(format(round(mean_survhzr,2),scientific = FALSE)," (",round(lwr,2),", ",round(upr,2),")",sep="")) %>%
#   mutate(Estimate_CI_w2 = cell_spec(Estimate_CI_w2,"html", bold = ifelse((lwr>0 & upr>0) | (lwr<0 & upr<0),T,F))) %>%
#   dplyr::select(time,Estimate_CI_w2)
# tbl21 <- ipw.hzr21 %>% mutate(time = time + 1) %>%
#   filter(time %in% c(7,14,21,28)) %>%
#   mutate(Estimate_CI_w3 = paste(format(round(mean_survhzr,2),scientific = FALSE)," (",round(lwr,2),", ",round(upr,2),")",sep="")) %>%
#   mutate(Estimate_CI_w3 = cell_spec(Estimate_CI_w3,"html", bold = ifelse((lwr>0 & upr>0) | (lwr<0 & upr<0),T,F))) %>%
#   dplyr::select(time,Estimate_CI_w3)
# tbl28 <- ipw.hzr28 %>% mutate(time = time + 1) %>%
#   filter(time %in% c(7,14,21,28)) %>%
#   mutate(Estimate_CI_w4 = paste(format(round(mean_survhzr,2),scientific = FALSE)," (",round(lwr,2),", ",round(upr,2),")",sep="")) %>%
#   mutate(Estimate_CI_w4 = cell_spec(Estimate_CI_w4,"html", bold = ifelse((lwr>0 & upr>0) | (lwr<0 & upr<0),T,F))) %>%
#   dplyr::select(time,Estimate_CI_w4)


# data_list = list(tbl7, tbl14,tbl21,tbl28)
# tbl <- data_list %>% reduce(left_join, by = "time")

# kbl(tbl,"html",escape = F,col.names=c("Days after transfusion","1 Week Shelf Life","2 Weeks Shelf Life","3 Weeks Shelf Life","4 Weeks Shelf Life")) %>% 
#   kable_classic("striped") %>%
#   add_header_above(c(" " = 1, "Risk difference (%) (95%CI)" = 4)) %>%
#   save_kable(file = "random_treatment/trial_arms_tbl_risk_difference.html", self_contained = T)  

}

