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


#models <- list("2021-10-25_model5_0_10_20_30_40knots_total_trans/thrombo_event/")
# model4: # Edgren model: poor fit 

dir = "/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/"

# loop version 1
for (model in models) {
       setwd(paste(dir,model,sep=""))
       #print(paste(dir,model,sep=""))}

       if (model == "2021-10-25_model4_big_knots_total_trans/") {
              # if Edgren model
              limits_surv = NULL
              limits_rr = NULL
              limits_diff = NULL
       } 
       # else if (model == "2021-10-25_model5_0_10_20_30_40knots_total_trans/thrombo_event/") {
       #       limits_surv = c(0.84, 1)
       #       limits_rr = c(0.97, 1.05)
       #       limits_diff = c(-2.4, 4.4)
       # } 
       else {
              limits_surv = c(0.84, 1)
              limits_rr = c(0.97, 1.05)
              limits_diff = c(-2.4, 4.4)
       }


       # All-cause mortality
       #setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-25_model1_interact_all_trans/")
       # Dont run - doesnt fit
       #setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-11-06_model2_splinesFix/")

       #setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-25_model3_all_trans_knots1_3_5_9_14/")
       # Sensitivity model 3 Edgren
       #setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-25_model4_big_knots_total_trans/")
       # Final model
       #setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-25_model5_0_10_20_30_40knots_total_trans/")
       #setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/")



       # Composite event
       # Thombo
       #setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-18_model1_interact_all_trans/thrombo_event/")
       #setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-18_model2_total_exposure/thrombo_event/")
       #setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-25_model3_all_trans_knots1_3_5_9_14/thrombo_event/")
       #setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-25_model4_big_knots_total_trans/thrombo_event/")
       #setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-25_model5_0_10_20_30_40knots_total_trans/thrombo_event")
       #setwd("/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/thrombo_event")



       follow_up = 28

       # Load results
       # Mortality
       ipw.exposureNC7 = read_tsv("processed/storage_week1/survival_NC_estimates.tsv")
       ipw.exposureNC14 = read_tsv("processed/storage_week2/survival_NC_estimates.tsv")
       ipw.exposureNC21 = read_tsv("processed/storage_week3/survival_NC_estimates.tsv")
       ipw.exposureNC28 = read_tsv("processed/storage_week4/survival_NC_estimates.tsv")

       ipw.exposure7 = read_tsv("processed/storage_week1/survival_fresh_estimates.tsv")
       ipw.exposure14 = read_tsv("processed/storage_week2/survival_fresh_estimates.tsv")
       ipw.exposure21 = read_tsv("processed/storage_week3/survival_fresh_estimates.tsv")
       ipw.exposure28 = read_tsv("processed/storage_week4/survival_fresh_estimates.tsv")

       ipw0.exposure7 = read_tsv("processed/storage_week1/survival_old_estimates.tsv")
       ipw0.exposure14 = read_tsv("processed/storage_week2/survival_old_estimates.tsv")
       ipw0.exposure21 = read_tsv("processed/storage_week3/survival_old_estimates.tsv")
       ipw0.exposure28 = read_tsv("processed/storage_week4/survival_old_estimates.tsv")



       first_col = match("cumprod1",names(ipw.exposure7))
       last_col = ncol(ipw.exposure7)

       # Add time 0
       ipw.exposure7 <- ipw.exposure7 %>% mutate(time = time +1)
       ipw.exposure7 <- rbind(c(0,rep(1,last_col-1)),ipw.exposure7)
       ipw.exposure7 <- ipw.exposure7 %>% mutate(across(c(time,starts_with("cumprod")),as.numeric))
       ipw.exposure14 <- ipw.exposure14 %>% mutate(time = time +1)
       ipw.exposure14 <- rbind(c(0,rep(1,last_col-1)),ipw.exposure14)
       ipw.exposure14 <- ipw.exposure14 %>% mutate(across(c(time,starts_with("cumprod")),as.numeric))
       ipw.exposure21 <- ipw.exposure21 %>% mutate(time = time +1)
       ipw.exposure21 <- rbind(c(0,rep(1,last_col-1)),ipw.exposure21)
       ipw.exposure21 <- ipw.exposure21 %>% mutate(across(c(time,starts_with("cumprod")),as.numeric))
       ipw.exposure28 <- ipw.exposure28 %>% mutate(time = time +1)
       ipw.exposure28 <- rbind(c(0,rep(1,last_col-1)),ipw.exposure28)
       ipw.exposure28 <- ipw.exposure28 %>% mutate(across(c(time,starts_with("cumprod")),as.numeric))
       # Add time 0
       ipw0.exposure7 <- ipw0.exposure7 %>% mutate(time = time +1)
       ipw0.exposure7 <- rbind(c(0,rep(1,last_col-1)),ipw0.exposure7)
       ipw0.exposure7 <- ipw0.exposure7 %>% mutate(across(c(time,starts_with("cumprod")),as.numeric))
       ipw0.exposure14 <- ipw0.exposure14 %>% mutate(time = time +1)
       ipw0.exposure14 <- rbind(c(0,rep(1,last_col-1)),ipw0.exposure14)
       ipw0.exposure14 <- ipw0.exposure14 %>% mutate(across(c(time,starts_with("cumprod")),as.numeric))
       ipw0.exposure21 <- ipw0.exposure21 %>% mutate(time = time +1)
       ipw0.exposure21 <- rbind(c(0,rep(1,last_col-1)),ipw0.exposure21)
       ipw0.exposure21 <- ipw0.exposure21 %>% mutate(across(c(time,starts_with("cumprod")),as.numeric))
       ipw0.exposure28 <- ipw0.exposure28 %>% mutate(time = time +1)
       ipw0.exposure28 <- rbind(c(0,rep(1,last_col-1)),ipw0.exposure28)
       ipw0.exposure28 <- ipw0.exposure28 %>% mutate(across(c(time,starts_with("cumprod")),as.numeric))
       # Add time 0
       ipw.exposureNC7 <- ipw.exposureNC7 %>% mutate(time = time +1)
       ipw.exposureNC7 <- rbind(c(0,rep(1,last_col-1)),ipw.exposureNC7)
       ipw.exposureNC7 <- ipw.exposureNC7 %>% mutate(across(c(time,starts_with("cumprod")),as.numeric))
       ipw.exposureNC14 <- ipw.exposureNC14 %>% mutate(time = time +1)
       ipw.exposureNC14 <- rbind(c(0,rep(1,last_col-1)),ipw.exposureNC14)
       ipw.exposureNC14 <- ipw.exposureNC14 %>% mutate(across(c(time,starts_with("cumprod")),as.numeric))
       ipw.exposureNC21 <- ipw.exposureNC21 %>% mutate(time = time +1)
       ipw.exposureNC21 <- rbind(c(0,rep(1,last_col-1)),ipw.exposureNC21)
       ipw.exposureNC21 <- ipw.exposureNC21 %>% mutate(across(c(time,starts_with("cumprod")),as.numeric))
       ipw.exposureNC28 <- ipw.exposureNC28 %>% mutate(time = time +1)
       ipw.exposureNC28 <- rbind(c(0,rep(1,last_col-1)),ipw.exposureNC28)
       ipw.exposureNC28 <- ipw.exposureNC28 %>% mutate(across(c(time,starts_with("cumprod")),as.numeric))

       ipw.exposureNC7["exposure"] = "Natural course"
       ipw.exposureNC7 <- ipw.exposureNC7 %>% relocate(exposure)
       ipw.exposureNC14["exposure"] = "Natural course"
       ipw.exposureNC14 <- ipw.exposureNC14 %>% relocate(exposure)
       ipw.exposureNC21["exposure"] = "Natural course"
       ipw.exposureNC21 <- ipw.exposureNC21 %>% relocate(exposure)
       ipw.exposureNC28["exposure"] = "Natural course"
       ipw.exposureNC28 <- ipw.exposureNC28 %>% relocate(exposure)


       ipw.exposure7["exposure"] = "1 Week or less"
       ipw.exposure7 <- ipw.exposure7 %>% relocate(exposure)
       ipw.exposure14["exposure"] = "2 Weeks or less"
       ipw.exposure14 <- ipw.exposure14 %>% relocate(exposure)
       ipw.exposure21["exposure"] = "3 Weeks or less"
       ipw.exposure21 <- ipw.exposure21 %>% relocate(exposure)
       ipw.exposure28["exposure"] = "4 Weeks or less"
       ipw.exposure28 <- ipw.exposure28 %>% relocate(exposure)


       ipw0.exposure7["exposure"] = "More than 1 Week"
       ipw0.exposure7 <- ipw0.exposure7 %>% relocate(exposure)
       ipw0.exposure14["exposure"] = "More than 2 Weeks"
       ipw0.exposure14 <- ipw0.exposure14 %>% relocate(exposure)
       ipw0.exposure21["exposure"] = "More than 3 Weeks"
       ipw0.exposure21 <- ipw0.exposure21 %>% relocate(exposure)
       ipw0.exposure28["exposure"] = "More than 4 Weeks"
       ipw0.exposure28 <- ipw0.exposure28 %>% relocate(exposure)



       # Get the column number according to how many iterations have been run
       first_col = match("cumprod1",names(ipw.exposureNC7))
       last_col = ncol(ipw.exposureNC7)

       # pivot_longer

       #ipw.survNC7 <- ipw.exposureNC7 %>% pivot_longer(cols=starts_with("cumprod"),names_to="lol",values_to="cumprod") %>%
       #  mutate(mean = mean(c_across(starts_with("cumprod"))),
       #         stDev= sd(c_across(starts_with("cumprod")))) %>%
       #  mutate(lwr = mean - (qnorm(0.975) * stDev),
       #         upr = mean + (qnorm(0.975) * stDev)) %>%
       #         dplyr::select(c(time,exposure,mean,stDev,lwr,upr))

       ipw.survNC7 <- ipw.exposureNC7 %>% rowwise() %>%
         mutate(mean = mean(c_across(starts_with("cumprod"))),
                stDev= sd(c_across(starts_with("cumprod")))) %>%
         mutate(lwr = mean - (qnorm(0.975) * stDev),
                upr = mean + (qnorm(0.975) * stDev)) %>%
                dplyr::select(c(time,exposure,mean,stDev,lwr,upr))
       ipw.survNC14 <- ipw.exposureNC14 %>% rowwise() %>%
         mutate(mean = mean(c_across(starts_with("cumprod"))),
                stDev= sd(c_across(starts_with("cumprod")))) %>%
         mutate(lwr = mean - (qnorm(0.975) * stDev),
                upr = mean + (qnorm(0.975) * stDev)) %>%
                dplyr::select(c(time,exposure,mean,stDev,lwr,upr))
       ipw.survNC21 <- ipw.exposureNC21 %>% rowwise() %>%
         mutate(mean = mean(c_across(starts_with("cumprod"))),
                stDev= sd(c_across(starts_with("cumprod")))) %>%
         mutate(lwr = mean - (qnorm(0.975) * stDev),
                upr = mean + (qnorm(0.975) * stDev)) %>%
                dplyr::select(c(time,exposure,mean,stDev,lwr,upr))
       ipw.survNC28 <- ipw.exposureNC28 %>% rowwise() %>%
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
       ipw.surv14 <- ipw.exposure14 %>% rowwise() %>%
         mutate(mean = mean(c_across(starts_with("cumprod"))),
                stDev= sd(c_across(starts_with("cumprod")))) %>%
         mutate(lwr = mean - (qnorm(0.975) * stDev),
                upr = mean + (qnorm(0.975) * stDev)) %>%
                dplyr::select(c(time,exposure,mean,stDev,lwr,upr))
       ipw.surv21 <- ipw.exposure21 %>% rowwise() %>%
         mutate(mean = mean(c_across(starts_with("cumprod"))),
                stDev= sd(c_across(starts_with("cumprod")))) %>%
         mutate(lwr = mean - (qnorm(0.975) * stDev),
                upr = mean + (qnorm(0.975) * stDev)) %>%
                dplyr::select(c(time,exposure,mean,stDev,lwr,upr))
       ipw.surv28 <- ipw.exposure28 %>% rowwise() %>%
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
       ipw0.surv14 <- ipw0.exposure14 %>% rowwise() %>%
         mutate(mean = mean(c_across(starts_with("cumprod"))),
                stDev= sd(c_across(starts_with("cumprod")))) %>%
         mutate(lwr = mean - (qnorm(0.975) * stDev),
                upr = mean + (qnorm(0.975) * stDev)) %>%
                dplyr::select(c(time,exposure,mean,stDev,lwr,upr))
       ipw0.surv21 <- ipw0.exposure21 %>% rowwise() %>%
         mutate(mean = mean(c_across(starts_with("cumprod"))),
                stDev= sd(c_across(starts_with("cumprod")))) %>%
         mutate(lwr = mean - (qnorm(0.975) * stDev),
                upr = mean + (qnorm(0.975) * stDev)) %>%
                dplyr::select(c(time,exposure,mean,stDev,lwr,upr))
       ipw0.surv28 <- ipw0.exposure28 %>% rowwise() %>%
         mutate(mean = mean(c_across(starts_with("cumprod"))),
                stDev= sd(c_across(starts_with("cumprod")))) %>%
         mutate(lwr = mean - (qnorm(0.975) * stDev),
                upr = mean + (qnorm(0.975) * stDev)) %>%
                dplyr::select(c(time,exposure,mean,stDev,lwr,upr))         



       # Combine plots
       colors <- c("1 Week or less" = "#D55E00", "More than 1 Week" = "#0072B2","Natural course" = "#E69F00")
       colors <- c("Natural course" = "#7CAE00","1 Week or less" = "#F8766D",  "More than 1 Week" = "#00BFC4")

       # ipw.surv7 <- ipw.surv7 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean=1,lwr=1,upr=1,exposure="1 Week or less",.before=1)
       # ipw0.surv7 <- ipw0.surv7 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean=1,lwr=1,upr=1,exposure="More than 1 Week",.before=1)
       # ipw.survNC7 <- ipw.survNC7 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean=1,lwr=1,upr=1,exposure="Natural course",.before=1)

       # ipw.surv14 <- ipw.surv14 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean=1,lwr=1,upr=1,exposure="2 Weeks or less",.before=1)
       # ipw0.surv14 <- ipw0.surv14 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean=1,lwr=1,upr=1,exposure="More than 2 Weeks",.before=1)
       # ipw.survNC14 <- ipw.survNC14 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean=1,lwr=1,upr=1,exposure="Natural course",.before=1)

       # ipw.surv21 <- ipw.surv21 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean=1,lwr=1,upr=1,exposure="3 Weeks or less",.before=1)
       # ipw0.surv21 <- ipw0.surv21 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean=1,lwr=1,upr=1,exposure="More than 3 Weeks",.before=1)
       # ipw.survNC21 <- ipw.survNC21 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean=1,lwr=1,upr=1,exposure="Natural course",.before=1)

       # ipw.surv28 <- ipw.surv28 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean=1,lwr=1,upr=1,exposure="4 Weeks or less",.before=1)
       # ipw0.surv28 <- ipw0.surv28 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean=1,lwr=1,upr=1,exposure="More than 4 Weeks",.before=1)
       # ipw.survNC28 <- ipw.survNC28 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean=1,lwr=1,upr=1,exposure="Natural course",.before=1)


       #min_x = list(min(ipw.survNC7$lwr),min(ipw.survNC14$lwr),)

       p7 <- ggplot() + 
         geom_line(data = ipw.survNC7,aes(x=time,y = mean, colour = exposure)) + 
         geom_line(data = ipw.surv7,aes(x=time,y = mean, colour = exposure)) + 
         geom_line(data = ipw0.surv7,aes(x=time,y = mean, colour = exposure)) + 
         geom_ribbon(data = ipw.survNC7,aes(x=time,ymin = lwr, ymax = upr, fill = exposure, color = NULL), alpha = .15) +
         geom_ribbon(data = ipw.surv7,aes(x=time,ymin = lwr, ymax = upr, fill = exposure, color = NULL), alpha = .15) +
         geom_ribbon(data = ipw0.surv7,aes(x=time,ymin = lwr, ymax = upr, fill = exposure, color = NULL), alpha = .15) +
         xlab("") + 
         scale_x_continuous(expand=c(0,0),limits = c(0, follow_up), breaks=seq(0,follow_up,7)) +
         scale_y_continuous(expand=c(0,0),limits=limits_surv, breaks=seq(0.85, 1, 0.05)) +
         ylab("Survival probability") + 
         ggtitle("1-Week Storage Threshold") + 
         labs(colour="RBC storage time",fill="RBC storage time") +
         theme_bw() + 
         theme(legend.position="bottom") +
         scale_fill_manual(values = c("#1b9e77","#7570b3","#d95f02")) +
         scale_colour_manual(values = c("#1b9e77","#7570b3","#d95f02"))

       p14 <- ggplot() + 
         geom_line(data = ipw.survNC14,aes(x=time,y = mean, colour = exposure)) + 
         geom_line(data = ipw.surv14,aes(x=time,y = mean, colour = exposure)) + 
         geom_line(data = ipw0.surv14,aes(x=time,y = mean, colour = exposure)) + 
         geom_ribbon(data = ipw.survNC14,aes(x=time,ymin = lwr, ymax = upr, fill = exposure, color = NULL), alpha = .15) +
         geom_ribbon(data = ipw.surv14,aes(x=time,ymin = lwr, ymax = upr, fill = exposure, color = NULL), alpha = .15) +
         geom_ribbon(data = ipw0.surv14,aes(x=time,ymin = lwr, ymax = upr, fill = exposure, color = NULL), alpha = .15) +
         xlab("") + 
         scale_x_continuous(expand=c(0,0),limits = c(0, follow_up), breaks=seq(0,follow_up,7)) +
         scale_y_continuous(expand=c(0,0),limits=limits_surv, breaks=seq(0.85, 1, 0.05)) +
         ylab("") + 
         ggtitle("2-Weeks Storage Threshold") + 
         labs(colour="RBC storage time",fill="RBC storage time") +
         theme_bw() + 
         theme(legend.position="bottom") +
         scale_fill_manual(values = c("#1b9e77","#7570b3","#d95f02")) +
         scale_colour_manual(values = c("#1b9e77","#7570b3","#d95f02"))

       p21 <- ggplot() + 
         geom_line(data = ipw.survNC21,aes(x=time,y = mean, colour = exposure)) + 
         geom_line(data = ipw.surv21,aes(x=time,y = mean, colour = exposure)) + 
         geom_line(data = ipw0.surv21,aes(x=time,y = mean, colour = exposure)) + 
         geom_ribbon(data = ipw.survNC21,aes(x=time,ymin = lwr, ymax = upr, fill = exposure, color = NULL), alpha = .15) +
         geom_ribbon(data = ipw.surv21,aes(x=time,ymin = lwr, ymax = upr, fill = exposure, color = NULL), alpha = .15) +
         geom_ribbon(data = ipw0.surv21,aes(x=time,ymin = lwr, ymax = upr, fill = exposure, color = NULL), alpha = .15) +
         xlab("Days after baseline-transfusion") + 
         scale_x_continuous(expand=c(0,0),limits = c(0, follow_up), breaks=seq(0,follow_up,7)) +
         scale_y_continuous(expand=c(0,0),limits=limits_surv, breaks=seq(0.85, 1, 0.05)) +
         ylab("Survival probability") + 
         ggtitle("3-Weeks Storage Threshold") + 
         labs(colour="RBC storage time",fill="RBC storage time") +
         theme_bw() + 
         theme(legend.position="bottom") +
         scale_fill_manual(values = c("#1b9e77","#7570b3","#d95f02")) +
         scale_colour_manual(values = c("#1b9e77","#7570b3","#d95f02"))
       p28 <- ggplot() + 
         geom_line(data = ipw.survNC28,aes(x=time,y = mean, colour = exposure)) + 
         geom_line(data = ipw.surv28,aes(x=time,y = mean, colour = exposure)) + 
         geom_line(data = ipw0.surv28,aes(x=time,y = mean, colour = exposure)) + 
         geom_ribbon(data = ipw.survNC28,aes(x=time,ymin = lwr, ymax = upr, fill = exposure, color = NULL), alpha = .15) +
         geom_ribbon(data = ipw.surv28,aes(x=time,ymin = lwr, ymax = upr, fill = exposure, color = NULL), alpha = .15) +
         geom_ribbon(data = ipw0.surv28,aes(x=time,ymin = lwr, ymax = upr, fill = exposure, color = NULL), alpha = .15) +
         xlab("Days after baseline-transfusion") + 
         scale_x_continuous(expand=c(0,0),limits = c(0, follow_up), breaks=seq(0,follow_up,7)) +
         scale_y_continuous(expand=c(0,0),limits=limits_surv, breaks=seq(0.85, 1, 0.05)) +
         ylab("") + 
         ggtitle("4-Weeks Storage Threshold") + 
         labs(colour="RBC storage time",fill="RBC storage time") +
         theme_bw() + 
         theme(legend.position="bottom") +
         scale_fill_manual(values = c("#1b9e77","#7570b3","#d95f02")) +
         scale_colour_manual(values = c("#1b9e77","#7570b3","#d95f02"))


       p <- plot_grid(p7, p14, p21,p28, labels = "AUTO") 
       save_plot("trial_arms_survival_curves_combined.pdf", p, ncol = 2,nrow = 2)

       # + scale_color_manual(values = colors) + scale_fill_manual(values = colors)
       # + scale_color_manual(values = colors) + scale_fill_manual(values = colors)
       # + scale_color_manual(values = colors) + scale_fill_manual(values = colors)
       # + scale_color_manual(values = colors) + scale_fill_manual(values = colors)


       # -- Risk ratio compared to other arm -- #

       ipw.hzr7 <-  (ipw.exposure7[first_col:last_col]) / (ipw0.exposure7[first_col:last_col])
       ipw.hzr14 <- (  ipw.exposure14[first_col:last_col]) / (ipw0.exposure14[first_col:last_col])
       ipw.hzr21 <- (  ipw.exposure21[first_col:last_col]) / (ipw0.exposure21[first_col:last_col])
       ipw.hzr28 <- (  ipw.exposure28[first_col:last_col]) / (ipw0.exposure28[first_col:last_col])

       # Calculate mean and standard deviation
       ipw.hzr7 <- ipw.hzr7 %>% rowwise() %>%
         mutate(mean_survhzr = mean(c_across(starts_with("cumprod"))),
                stDev= sd(c_across(starts_with("cumprod")))) %>%
         mutate(lwr = mean_survhzr - (qnorm(0.975) * stDev),
                upr = mean_survhzr + (qnorm(0.975) * stDev)) %>%
                dplyr::select(c(mean_survhzr,stDev,lwr,upr))
       ipw.hzr14 <- ipw.hzr14 %>% rowwise() %>%
         mutate(mean_survhzr = mean(c_across(starts_with("cumprod"))),
                stDev= sd(c_across(starts_with("cumprod")))) %>%
         mutate(lwr = mean_survhzr - (qnorm(0.975) * stDev),
                upr = mean_survhzr + (qnorm(0.975) * stDev)) %>%
                dplyr::select(c(mean_survhzr,stDev,lwr,upr))
       ipw.hzr21 <- ipw.hzr21 %>% rowwise() %>%
         mutate(mean_survhzr = mean(c_across(starts_with("cumprod"))),
                stDev= sd(c_across(starts_with("cumprod")))) %>%
         mutate(lwr = mean_survhzr - (qnorm(0.975) * stDev),
                upr = mean_survhzr + (qnorm(0.975) * stDev)) %>%
                dplyr::select(c(mean_survhzr,stDev,lwr,upr))
       ipw.hzr28 <- ipw.hzr28 %>% rowwise() %>%
         mutate(mean_survhzr = mean(c_across(starts_with("cumprod"))),
                stDev= sd(c_across(starts_with("cumprod")))) %>%
         mutate(lwr = mean_survhzr - (qnorm(0.975) * stDev),
                upr = mean_survhzr + (qnorm(0.975) * stDev)) %>%
                dplyr::select(c(mean_survhzr,stDev,lwr,upr))


       # Reset time column
       ipw.hzr7$time = as.integer(rownames(ipw.hzr7))-1
       ipw.hzr14$time = as.integer(rownames(ipw.hzr14))-1
       ipw.hzr21$time = as.integer(rownames(ipw.hzr21))-1
       ipw.hzr28$time = as.integer(rownames(ipw.hzr28))-1


       # ipw.hzr7 <- ipw.hzr7 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean_survhzr=1,lwr=1,upr=1,.before=1)
       # ipw.hzr14 <- ipw.hzr14 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean_survhzr=1,lwr=1,upr=1,.before=1)
       # ipw.hzr21 <- ipw.hzr21 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean_survhzr=1,lwr=1,upr=1,.before=1)
       # ipw.hzr28 <- ipw.hzr28 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean_survhzr=1,lwr=1,upr=1,.before=1)


       ipw.hzr7["exposure7"] = "7 Days"
       ipw.hzr14["exposure14"] = "14 Days"
       ipw.hzr21["exposure21"] = "21 Days"
       ipw.hzr28["exposure28"] = "28 Days"


       # plot survial hazard ratio

       p7 <- ggplot() + 
         geom_line(data = ipw.hzr7, aes(x=time, y = mean_survhzr),colour="#2166ac") +  
         geom_ribbon(data = ipw.hzr7, aes(x=time, y = mean_survhzr,ymin = lwr, ymax = upr), fill = "#2166ac", alpha = .15) +
         xlab("") + 
         scale_x_continuous(expand=c(0,0),limits = c(0, follow_up), breaks=seq(0,follow_up,7)) +
         scale_y_continuous(expand=c(0,0),limits=limits_rr, breaks=seq(0.97,1.05,0.01)) +
         ylab("Risk Ratio") + 
         ggtitle("1-Week Storage Threshold") + 
         geom_hline(yintercept=1, linetype="dashed", color = "grey") +
         #labs(colour="RBC of long storage (<10 days)") +
         theme_bw() + 
         theme(legend.position="bottom")

       p14 <- ggplot() + 
         geom_line(data = ipw.hzr14, aes(x=time, y = mean_survhzr),colour="#2166ac") +  
         geom_ribbon(data = ipw.hzr14, aes(x=time, y = mean_survhzr,ymin = lwr, ymax = upr), fill = "#2166ac", alpha = .15) +
         xlab("") + 
         scale_x_continuous(expand=c(0,0),limits = c(0, follow_up), breaks=seq(0,follow_up,7)) +
         scale_y_continuous(expand=c(0,0),limits=limits_rr, breaks=seq(0.97,1.05,0.01)) +
         ylab("") + 
         ggtitle("2-Weeks Storage Threshold") + 
         geom_hline(yintercept=1, linetype="dashed", color = "grey") +
         #labs(colour="RBC of long storage (<10 days)") +
         theme_bw() + 
         theme(legend.position="bottom")

       p21 <- ggplot() + 
         geom_line(data = ipw.hzr21, aes(x=time, y = mean_survhzr),colour="#2166ac") +  
         geom_ribbon(data = ipw.hzr21, aes(x=time, y = mean_survhzr,ymin = lwr, ymax = upr), fill = "#2166ac", alpha = .15) +
         xlab("Days after baseline-transfusion") + 
         scale_x_continuous(expand=c(0,0),limits = c(0, follow_up), breaks=seq(0,follow_up,7)) +
         scale_y_continuous(expand=c(0,0),limits=limits_rr, breaks=seq(0.97,1.05,0.01)) +
         ylab("Risk Ratio") + 
         ggtitle("3-Weeks Storage Threshold") + 
         geom_hline(yintercept=1, linetype="dashed", color = "grey") +
         #labs(colour="RBC of long storage (<10 days)") +
         theme_bw() + 
         theme(legend.position="bottom")

       p28 <- ggplot() + 
         geom_line(data = ipw.hzr28, aes(x=time, y = mean_survhzr),colour="#2166ac") +  
         geom_ribbon(data = ipw.hzr28, aes(x=time, y = mean_survhzr,ymin = lwr, ymax = upr), fill = "#2166ac", alpha = .15) +
         xlab("Days after baseline-transfusion") + 
         scale_x_continuous(expand=c(0,0),limits = c(0, follow_up), breaks=seq(0,follow_up,7)) +
         scale_y_continuous(expand=c(0,0),limits=limits_rr, breaks=seq(0.97,1.05,0.01)) +
         ylab("") + 
         ggtitle("4-Weeks Storage Threshold") + 
         geom_hline(yintercept=1, linetype="dashed", color = "grey") +
         #labs(colour="RBC of long storage (<10 days)") +
         theme_bw() + 
         theme(legend.position="bottom")


       p <- plot_grid(p7, p14, p21,p28, labels = "AUTO")
       save_plot("RR_combined.pdf", p, ncol = 2,nrow = 2)

       tbl7 <- ipw.hzr7 %>% 
         filter(time %in% c(7,14,21,28)) %>%
         mutate(Estimate_CI_w1 = paste(format(round(mean_survhzr,4),scientific = FALSE)," (",round(lwr,4),", ",round(upr,4),")",sep="")) %>%
         mutate(Estimate_CI_w1 = cell_spec(Estimate_CI_w1,"html", bold = ifelse((lwr>1 & upr>1) | (lwr<1 & upr<1),T,F))) %>%
         dplyr::select(time,Estimate_CI_w1)
       tbl14 <- ipw.hzr14 %>% 
         filter(time %in% c(7,14,21,28)) %>%
         mutate(Estimate_CI_w2 = paste(format(round(mean_survhzr,4),scientific = FALSE)," (",round(lwr,4),", ",round(upr,4),")",sep="")) %>%
         mutate(Estimate_CI_w2 = cell_spec(Estimate_CI_w2,"html", bold = ifelse((lwr>1 & upr>1) | (lwr<1 & upr<1),T,F))) %>%
         dplyr::select(time,Estimate_CI_w2)
       tbl21 <- ipw.hzr21 %>% 
         filter(time %in% c(7,14,21,28)) %>%
         mutate(Estimate_CI_w3 = paste(format(round(mean_survhzr,4),scientific = FALSE)," (",round(lwr,4),", ",round(upr,4),")",sep="")) %>%
         mutate(Estimate_CI_w3 = cell_spec(Estimate_CI_w3,"html", bold = ifelse((lwr>1 & upr>1) | (lwr<1 & upr<1),T,F))) %>%
         dplyr::select(time,Estimate_CI_w3)
       tbl28 <- ipw.hzr28 %>% 
         filter(time %in% c(7,14,21,28)) %>%
         mutate(Estimate_CI_w4 = paste(format(round(mean_survhzr,4),scientific = FALSE)," (",round(lwr,4),", ",round(upr,4),")",sep="")) %>%
         mutate(Estimate_CI_w4 = cell_spec(Estimate_CI_w4,"html", bold = ifelse((lwr>1 & upr>1) | (lwr<1 & upr<1),T,F))) %>%
         dplyr::select(time,Estimate_CI_w4)


       data_list = list(tbl7, tbl14,tbl21,tbl28)
       tbl <- data_list %>% reduce(left_join, by = "time")

       kbl(tbl,"html",escape = F,col.names=c("Days after baseline-transfusion","1 Week Storage Threshold","2 Weeks Storage Threshold","3 Weeks Storage Threshold","4 Weeks Storage Threshold")) %>% 
         kable_classic("striped") %>%
         add_header_above(c(" " = 1, "Risk Ratio (95%CI)" = 4)) %>%
         save_kable(file = "trial_arms_tbl_risk_ratio.html", self_contained = T)



       # -- Survival difference compared to other arm -- #

       ipw.hzr7 <-  ((ipw.exposure7[first_col:last_col]) - (ipw0.exposure7[first_col:last_col])) * 100
       ipw.hzr14 <- ((ipw.exposure14[first_col:last_col]) - (ipw0.exposure14[first_col:last_col])) * 100
       ipw.hzr21 <- ((ipw.exposure21[first_col:last_col]) - (ipw0.exposure21[first_col:last_col])) * 100
       ipw.hzr28 <- ((ipw.exposure28[first_col:last_col]) - (ipw0.exposure28[first_col:last_col])) * 100


       # Calculate mean and standard deviation
       ipw.hzr7 <- ipw.hzr7 %>% rowwise() %>%
         mutate(mean_survhzr = mean(c_across(starts_with("cumprod"))),
                stDev= sd(c_across(starts_with("cumprod")))) %>%
         mutate(lwr = mean_survhzr - (qnorm(0.975) * stDev),
                upr = mean_survhzr + (qnorm(0.975) * stDev)) %>%
                dplyr::select(c(mean_survhzr,stDev,lwr,upr))
       ipw.hzr14 <- ipw.hzr14 %>% rowwise() %>%
         mutate(mean_survhzr = mean(c_across(starts_with("cumprod"))),
                stDev= sd(c_across(starts_with("cumprod")))) %>%
         mutate(lwr = mean_survhzr - (qnorm(0.975) * stDev),
                upr = mean_survhzr + (qnorm(0.975) * stDev)) %>%
                dplyr::select(c(mean_survhzr,stDev,lwr,upr))
       ipw.hzr21 <- ipw.hzr21 %>% rowwise() %>%
         mutate(mean_survhzr = mean(c_across(starts_with("cumprod"))),
                stDev= sd(c_across(starts_with("cumprod")))) %>%
         mutate(lwr = mean_survhzr - (qnorm(0.975) * stDev),
                upr = mean_survhzr + (qnorm(0.975) * stDev)) %>%
                dplyr::select(c(mean_survhzr,stDev,lwr,upr))
       ipw.hzr28 <- ipw.hzr28 %>% rowwise() %>%
         mutate(mean_survhzr = mean(c_across(starts_with("cumprod"))),
                stDev= sd(c_across(starts_with("cumprod")))) %>%
         mutate(lwr = mean_survhzr - (qnorm(0.975) * stDev),
                upr = mean_survhzr + (qnorm(0.975) * stDev)) %>%
                dplyr::select(c(mean_survhzr,stDev,lwr,upr))


       # Reset time column
       ipw.hzr7$time = as.integer(rownames(ipw.hzr7))-1
       ipw.hzr14$time = as.integer(rownames(ipw.hzr14))-1
       ipw.hzr21$time = as.integer(rownames(ipw.hzr21))-1
       ipw.hzr28$time = as.integer(rownames(ipw.hzr28))-1


       # ipw.hzr7 <- ipw.hzr7 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean_survhzr=0,lwr=0,upr=0,.before=1)
       # ipw.hzr14 <- ipw.hzr14 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean_survhzr=0,lwr=0,upr=0,.before=1)
       # ipw.hzr21 <- ipw.hzr21 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean_survhzr=0,lwr=0,upr=0,.before=1)
       # ipw.hzr28 <- ipw.hzr28 %>% 
       #                      mutate(time = time+1) %>%
       #                      add_row(time=0,mean_survhzr=0,lwr=0,upr=0,.before=1)


       ipw.hzr7["exposure7"] = "7 Days"
       ipw.hzr14["exposure14"] = "14 Days"
       ipw.hzr21["exposure21"] = "21 Days"
       ipw.hzr28["exposure28"] = "28 Days"



       # plot survial difference

       p7 <- ggplot() + 
         geom_hline(yintercept=0, linetype="dashed", color = "grey") +
         geom_line(data = ipw.hzr7, aes(x=time, y = mean_survhzr),colour="#2166ac") +  
         geom_ribbon(data = ipw.hzr7, aes(x=time, y = mean_survhzr,ymin = lwr, ymax = upr), fill = "#2166ac", alpha = .15) +
         xlab("") + 
         scale_x_continuous(expand=c(0,0),limits = c(0, follow_up), breaks=seq(0,follow_up,7)) + 
         scale_y_continuous(expand=c(0,0),limits=limits_diff, breaks=seq(-2,4,1)) +
         ylab("Risk difference (%-points)") +
         ggtitle("1-Week Storage Threshold") + 
         #labs(colour="RBC of long storage (<10 days)") +
         theme_bw() + 
         theme(legend.position="bottom")

       p14 <- ggplot() + 
         geom_hline(yintercept=0, linetype="dashed", color = "grey") +
         geom_line(data = ipw.hzr14, aes(x=time, y = mean_survhzr),colour="#2166ac") +  
         geom_ribbon(data = ipw.hzr14, aes(x=time, y = mean_survhzr,ymin = lwr, ymax = upr), fill = "#2166ac", alpha = .15) +
         xlab("") + 
         scale_x_continuous(expand=c(0,0),limits = c(0, follow_up), breaks=seq(0,follow_up,7)) +
         scale_y_continuous(expand=c(0,0),limits=limits_diff, breaks=seq(-2,4,1)) +
         ylab("") + 
         ggtitle("2-Weeks Storage Threshold") + 
         #labs(colour="RBC of long storage (<10 days)") +
         theme_bw() + 
         theme(legend.position="bottom")

       p21 <- ggplot() + 
         geom_hline(yintercept=0, linetype="dashed", color = "grey") +
         geom_line(data = ipw.hzr21, aes(x=time, y = mean_survhzr),colour="#2166ac") +  
         geom_ribbon(data = ipw.hzr21, aes(x=time, y = mean_survhzr,ymin = lwr, ymax = upr), fill = "#2166ac", alpha = .15) +
         xlab("Days after baseline-transfusion") + 
         scale_x_continuous(expand=c(0,0),limits = c(0, follow_up), breaks=seq(0,follow_up,7)) +
         scale_y_continuous(expand=c(0,0),limits=limits_diff, breaks=seq(-2,4,1)) +
         ylab("Risk difference (%-points)") +
         ggtitle("3-Weeks Storage Threshold") + 
         #labs(colour="RBC of long storage (<10 days)") +
         theme_bw() + 
         theme(legend.position="bottom")

       p28 <- ggplot() + 
         geom_hline(yintercept=0, linetype="dashed", color = "grey") +
         geom_line(data = ipw.hzr28, aes(x=time, y = mean_survhzr),colour="#2166ac") +  
         geom_ribbon(data = ipw.hzr28, aes(x=time, y = mean_survhzr,ymin = lwr, ymax = upr), fill = "#2166ac", alpha = .15) +
         xlab("Days after baseline-transfusion") + 
         scale_x_continuous(expand=c(0,0),limits = c(0, follow_up), breaks=seq(0,follow_up,7)) +
         scale_y_continuous(expand=c(0,0),limits=limits_diff, breaks=seq(-2,4,1)) +
         ylab("") + 
         ggtitle("4-Weeks Storage Threshold") + 
         #labs(colour="RBC of long storage (<10 days)") +
         theme_bw() + 
         theme(legend.position="bottom")


       p <- plot_grid(p7, p14, p21,p28, labels = "AUTO")
       save_plot("trial_arms_difference_combined.pdf", p, ncol = 2,nrow = 2)

       # in tables


       tbl7 <- ipw.hzr7 %>%
         filter(time %in% c(7,14,21,28)) %>%
         mutate(Estimate_CI_w1 = paste(format(round(mean_survhzr,2),nsmall=2,scientific = FALSE)," (",format(round(lwr,2),nsmall=2),", ",format(round(upr,2),nsmall=2),")",sep="")) %>%
         mutate(Estimate_CI_w1 = cell_spec(Estimate_CI_w1,"html", bold = ifelse((lwr>0 & upr>0) | (lwr<0 & upr<0),T,F))) %>%
         dplyr::select(time,Estimate_CI_w1)
       tbl14 <- ipw.hzr14 %>%
         filter(time %in% c(7,14,21,28)) %>%
         mutate(Estimate_CI_w2 = paste(format(round(mean_survhzr,2),nsmall=2,scientific = FALSE)," (",format(round(lwr,2),nsmall=2),", ",format(round(upr,2),nsmall=2),")",sep="")) %>%
         mutate(Estimate_CI_w2 = cell_spec(Estimate_CI_w2,"html", bold = ifelse((lwr>0 & upr>0) | (lwr<0 & upr<0),T,F))) %>%
         dplyr::select(time,Estimate_CI_w2)
       tbl21 <- ipw.hzr21 %>%
         filter(time %in% c(7,14,21,28)) %>%
         mutate(Estimate_CI_w3 = paste(format(round(mean_survhzr,2),nsmall=2,scientific = FALSE)," (",format(round(lwr,2),nsmall=2),", ",format(round(upr,2),nsmall=2),")",sep="")) %>%
         mutate(Estimate_CI_w3 = cell_spec(Estimate_CI_w3,"html", bold = ifelse((lwr>0 & upr>0) | (lwr<0 & upr<0),T,F))) %>%
         dplyr::select(time,Estimate_CI_w3)
       tbl28 <- ipw.hzr28 %>%
         filter(time %in% c(7,14,21,28)) %>%
         mutate(Estimate_CI_w4 = paste(format(round(mean_survhzr,2),nsmall=2,scientific = FALSE)," (",format(round(lwr,2),nsmall=2),", ",format(round(upr,2),nsmall=2),")",sep="")) %>%
         mutate(Estimate_CI_w4 = cell_spec(Estimate_CI_w4,"html", bold = ifelse((lwr>0 & upr>0) | (lwr<0 & upr<0),T,F))) %>%
         dplyr::select(time,Estimate_CI_w4)


       data_list = list(tbl7,tbl14,tbl21,tbl28)
       tbl <- data_list %>% reduce(left_join, by = "time")

       kbl(tbl,"html",escape = F,align=c("c",rep("c",4)),col.names=c("Day","1-Week Threshold","2-Weeks Threshold","3-Weeks Threshold","4-Weeks Threshold")) %>% 
         #kable_classic("striped") %>%
         kable_classic(full_width =F, html_font ="Calibri light") %>%
         add_header_above(c(" " = 1, "Risk difference (%-points) (95%CI)" = 4)) %>%
         save_kable(file = "trial_arms_tbl_risk_difference.html", self_contained = T) 


       }


