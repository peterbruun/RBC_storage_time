# Snakefile

import numpy as np
import pandas as pd
import datetime
pd.set_option('max_info_columns', 10**10)
pd.set_option('display.max_columns', 1000)
pd.set_option('max_info_columns', 10**10)
pd.set_option('max_info_rows', 10**10)

workdir: "/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/"

# Storage thresholds
THRESHOLDS = ['random_treatment','storage_week1','storage_week2','storage_week3','storage_week4']

TRIAL = ["fresh","old","NC"]

N_BOOTSTRAPS = 500
SENSITIVITY_N_BOOTSTRAPS = 500


BOOTSTRAP = [str(i) for i in range(1, N_BOOTSTRAPS+1)]
BOOTSTRAP_SENSITIVITY = [str(i) for i in range(1, SENSITIVITY_N_BOOTSTRAPS+1)]

# R
R_EXEC = '/services/tools/R/3.5.0-ICC-MKL/bin'


# function to adjust memory is adtional is needed
def get_mem_mb(wildcards, attempt):
	if attempt == 1:
		return 1024*20
	elif attempt == 2:
		return 1024*25
	else:
		return 1024*30


rule all:
	input:
		# Bootstrapped files
		#expand("data/bootstrapped_data/bootstrap_{bootstrap}.tsv", bootstrap = BOOTSTRAP),
		expand("data/bootstrapped_mortality/bootstrap_{bootstrap}.tsv", bootstrap = BOOTSTRAP),
		#expand("data/bootstrapped_composite/bootstrap_{bootstrap}.tsv", bootstrap = BOOTSTRAP),
		expand("data/bootstrapped_thrombo_new/bootstrap_{bootstrap}.tsv", bootstrap = BOOTSTRAP),

		# Survival analysis for each threshold on all bootstrapps
		# Model 1
		expand("results/2021-10-25_model1_interact_all_trans/interim/{storage_threshold}/survival_{trial}_bootstrap_{bootstrap}.tsv", trial = TRIAL, storage_threshold = THRESHOLDS, bootstrap = BOOTSTRAP_SENSITIVITY),
		expand("results/2021-10-25_model1_interact_all_trans/processed/{storage_threshold}/survival_{trial}_estimates.tsv", trial = TRIAL, storage_threshold = THRESHOLDS),
		"results/2021-10-25_model1_interact_all_trans/analysis_code.R",
		# # # Model 3
		expand("results/2021-10-25_model3_all_trans_knots1_3_5_9_14/interim/{storage_threshold}/survival_{trial}_bootstrap_{bootstrap}.tsv", trial = TRIAL, storage_threshold = THRESHOLDS, bootstrap = BOOTSTRAP_SENSITIVITY),
		expand("results/2021-10-25_model3_all_trans_knots1_3_5_9_14/processed/{storage_threshold}/survival_{trial}_estimates.tsv", trial = TRIAL, storage_threshold = THRESHOLDS),
		"results/2021-10-25_model3_all_trans_knots1_3_5_9_14/analysis_code.R",
		# # # Model 4
		expand("results/2021-10-25_model4_big_knots_total_trans/interim/{storage_threshold}/survival_{trial}_bootstrap_{bootstrap}.tsv", trial = TRIAL, storage_threshold = THRESHOLDS, bootstrap = BOOTSTRAP_SENSITIVITY),
		expand("results/2021-10-25_model4_big_knots_total_trans/processed/{storage_threshold}/survival_{trial}_estimates.tsv", trial = TRIAL, storage_threshold = THRESHOLDS),
		"results/2021-10-25_model4_big_knots_total_trans/analysis_code.R",
		# # Model 5: 
		expand("results/2021-10-25_model5_0_10_20_30_40knots_total_trans/interim/{storage_threshold}/survival_{trial}_bootstrap_{bootstrap}.tsv", trial = TRIAL, storage_threshold = THRESHOLDS, bootstrap = BOOTSTRAP),
		expand("results/2021-10-25_model5_0_10_20_30_40knots_total_trans/processed/{storage_threshold}/survival_{trial}_estimates.tsv", trial = TRIAL, storage_threshold = THRESHOLDS),
		"results/2021-10-25_model5_0_10_20_30_40knots_total_trans/analysis_code.R",
		# # Model 6:
		expand("results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/interim/{storage_threshold}/survival_{trial}_bootstrap_{bootstrap}.tsv", trial = TRIAL, storage_threshold = THRESHOLDS, bootstrap = BOOTSTRAP_SENSITIVITY),
		expand("results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/processed/{storage_threshold}/survival_{trial}_estimates.tsv", trial = TRIAL, storage_threshold = THRESHOLDS),
		"results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/analysis_code.R",


		# # For composite endpoint: thombo + death
		# # Model 1
		# expand("results/2021-10-25_model1_interact_all_trans/thrombo_event/interim/{storage_threshold}/survival_{trial}_bootstrap_{bootstrap}.tsv", trial = TRIAL, storage_threshold = THRESHOLDS, bootstrap = BOOTSTRAP_SENSITIVITY),
		# expand("results/2021-10-25_model1_interact_all_trans/thrombo_event/processed/{storage_threshold}/survival_{trial}_estimates.tsv", trial = TRIAL, storage_threshold = THRESHOLDS),
		# # Model 3
		# expand("results/2021-10-25_model3_all_trans_knots1_3_5_9_14/thrombo_event/interim/{storage_threshold}/survival_{trial}_bootstrap_{bootstrap}.tsv", trial = TRIAL, storage_threshold = THRESHOLDS, bootstrap = BOOTSTRAP_SENSITIVITY),
		# expand("results/2021-10-25_model3_all_trans_knots1_3_5_9_14/thrombo_event/processed/{storage_threshold}/survival_{trial}_estimates.tsv", trial = TRIAL, storage_threshold = THRESHOLDS),
		# # Model 4
		# expand("results/2021-10-25_model4_big_knots_total_trans/thrombo_event/interim/{storage_threshold}/survival_{trial}_bootstrap_{bootstrap}.tsv", trial = TRIAL, storage_threshold = THRESHOLDS, bootstrap = BOOTSTRAP_SENSITIVITY),
		# expand("results/2021-10-25_model4_big_knots_total_trans/thrombo_event/processed/{storage_threshold}/survival_{trial}_estimates.tsv", trial = TRIAL, storage_threshold = THRESHOLDS),
		# # Model 5:
		expand("results/2021-10-25_model5_0_10_20_30_40knots_total_trans/thrombo_event/interim/{storage_threshold}/survival_{trial}_bootstrap_{bootstrap}.tsv", trial = TRIAL, storage_threshold = THRESHOLDS, bootstrap = BOOTSTRAP),
		expand("results/2021-10-25_model5_0_10_20_30_40knots_total_trans/thrombo_event/processed/{storage_threshold}/survival_{trial}_estimates.tsv", trial = TRIAL, storage_threshold = THRESHOLDS),
		# #Model 6
		# expand("results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/thrombo_event/interim/{storage_threshold}/survival_{trial}_bootstrap_{bootstrap}.tsv", trial = TRIAL, storage_threshold = THRESHOLDS, bootstrap = BOOTSTRAP_SENSITIVITY),
		# expand("results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/thrombo_event/processed/{storage_threshold}/survival_{trial}_estimates.tsv", trial = TRIAL, storage_threshold = THRESHOLDS),

		# Bloodflodet
		data_mortality = "/data/projects/deep_phenotyping/transfusions_bth_simple/data/processed/data_longitudinal_mortality.tsv",
		data_thrombo = "/data/projects/deep_phenotyping/transfusions_bth_simple/data/processed/data_longitudinal_thrombo.tsv",


###################################################################################################
## Prepare data ##


rule prepare_bootstrap_files_mortality:
	input:
		data = rules.all.input.data_mortality,
	output:
		bootstrapped_data = "data/bootstrapped_mortality/bootstrap_{bootstrap}.tsv",
	resources:
		tmin = 30,
		mem_mb = 1024*5,
	threads: 1
	run:
		# Bootstrap data 200 times
		bootstrapping = [
			'Rscript scripts/bootstrapping.R',
			'{input.data}',
			'{output.bootstrapped_data}']
		shell(' '.join(bootstrapping))


rule prepare_bootstrap_files_thrombo:
	input:
		data = rules.all.input.data_thrombo,
	output:
		bootstrapped_data = "data/bootstrapped_thrombo_new/bootstrap_{bootstrap}.tsv",
	resources:
		tmin = 30,
		mem_mb = 1024*5,
	threads: 1
	run:
		# Bootstrap data 200 times
		bootstrapping = [
			'Rscript scripts/bootstrapping.R',
			'{input.data}',
			'{output.bootstrapped_data}']
		shell(' '.join(bootstrapping))


##################################################################


rule write_analysis_code_to_results_folder_model1:
	input:
		code = "scripts/ipw_coinflip_final_model1.R",
	output:
		code = "results/2021-10-25_model1_interact_all_trans/analysis_code.R",
	resources:
		tmin = 10,
		mem_mb = 1024*2,
	shell:
		"""
		cp {input.code} {output.code}
		"""


rule IPW_marginal_structural_models_model1:
	input:
		data = "data/bootstrapped_mortality/bootstrap_{bootstrap}.tsv",
	output:
		ipw_survival_fresh = "results/2021-10-25_model1_interact_all_trans/interim/{storage_threshold}/survival_fresh_bootstrap_{bootstrap}.tsv",
		ipw_survival_old = "results/2021-10-25_model1_interact_all_trans/interim/{storage_threshold}/survival_old_bootstrap_{bootstrap}.tsv",
		ipw_survival_NC = "results/2021-10-25_model1_interact_all_trans/interim/{storage_threshold}/survival_NC_bootstrap_{bootstrap}.tsv",
	params:
		storage_threshold = "{storage_threshold}"
	resources:
		tmin = 60,
		mem_mb = get_mem_mb,
	threads: 1
	run:
		# Run analysis on each bootstrap
		ipw_model = [
			'Rscript scripts/ipw_coinflip_final_model1.R',
			'{input.data}',
			'{params.storage_threshold}',
			'{output.ipw_survival_fresh}',
			'{output.ipw_survival_old}',
			'{output.ipw_survival_NC}']
		shell(' '.join(ipw_model))


rule collect_results_mortality_model1:
	input:
		results = lambda wildcards: ["results/2021-10-25_model1_interact_all_trans/interim/"+wildcards.storage_threshold+"/survival_"+wildcards.trial+"_bootstrap_"+str(i)+".tsv" for i in BOOTSTRAP],
	output:
		ipw_survival = "results/2021-10-25_model1_interact_all_trans/processed/{storage_threshold}/survival_{trial}_estimates.tsv",
	resources:
		tmin = 60,
		mem_mb = 1024*30,
	threads: 1
	script:
		"scripts/collect_results.py"


rule plot_mortality_model1:
	input:
		#results = rules.collect_results_mortality.output.ipw_survival,
		expand("results/2021-10-09_ratio_diff_and_all_trans_knots01249/processed/{storage_threshold}/survival_{trial}_estimates.tsv", trial = TRIAL, storage_threshold = THRESHOLDS),
	output:
		plot = "results/2021-10-09_ratio_diff_and_all_trans_knots01249/trial_arms_survival_curves_combined.pdf",
	params:
		workdir = "/users/data/projects/deep_phenotyping/storage_w_thresholds_full_trans/results/2021-10-09_ratio_diff_and_all_trans_knots01249/",
	resources:
		tmin = 60,
		mem_mb = 1024*10,
	threads: 1
	run:
		# Run analysis on each bootstrap
		plor_results = [
			'Rscript scripts/plot_results_trial.R',
			 '{params.workdir}']
		shell(' '.join(plot_results))



rule IPW_marginal_structural_models_model3:
	input:
		data = "data/bootstrapped_mortality/bootstrap_{bootstrap}.tsv",
	output:
		ipw_survival_fresh = "results/2021-10-25_model3_all_trans_knots1_3_5_9_14/interim/{storage_threshold}/survival_fresh_bootstrap_{bootstrap}.tsv",
		ipw_survival_old = "results/2021-10-25_model3_all_trans_knots1_3_5_9_14/interim/{storage_threshold}/survival_old_bootstrap_{bootstrap}.tsv",
		ipw_survival_NC = "results/2021-10-25_model3_all_trans_knots1_3_5_9_14/interim/{storage_threshold}/survival_NC_bootstrap_{bootstrap}.tsv",
	params:
		storage_threshold = "{storage_threshold}"
	resources:
		tmin = 60,
		mem_mb = get_mem_mb,
	threads: 1
	run:
		# Run analysis on each bootstrap
		ipw_model = [
			'Rscript scripts/ipw_coinflip_final_model3.R',
			'{input.data}',
			'{params.storage_threshold}',
			'{output.ipw_survival_fresh}',
			'{output.ipw_survival_old}',
			'{output.ipw_survival_NC}']
		shell(' '.join(ipw_model))


rule collect_results_mortality_model3:
	input:
		results = lambda wildcards: ["results/2021-10-25_model3_all_trans_knots1_3_5_9_14/interim/"+wildcards.storage_threshold+"/survival_"+wildcards.trial+"_bootstrap_"+str(i)+".tsv" for i in BOOTSTRAP],
	output:
		ipw_survival = "results/2021-10-25_model3_all_trans_knots1_3_5_9_14/processed/{storage_threshold}/survival_{trial}_estimates.tsv",
	resources:
		tmin = 60,
		mem_mb = 1024*30,
	threads: 1
	script:
		"scripts/collect_results.py"


rule write_analysis_code_to_results_folder_model3:
	input:
		code = "scripts/ipw_coinflip_final_model3.R",
	output:
		code = "results/2021-10-25_model3_all_trans_knots1_3_5_9_14/analysis_code.R",
	resources:
		tmin = 10,
		mem_mb = 1024*2,
	shell:
		"""
		cp {input.code} {output.code}
		"""


rule IPW_marginal_structural_models_model4:
	input:
		data = "data/bootstrapped_mortality/bootstrap_{bootstrap}.tsv",
	output:
		ipw_survival_fresh = "results/2021-10-25_model4_big_knots_total_trans/interim/{storage_threshold}/survival_fresh_bootstrap_{bootstrap}.tsv",
		ipw_survival_old = "results/2021-10-25_model4_big_knots_total_trans/interim/{storage_threshold}/survival_old_bootstrap_{bootstrap}.tsv",
		ipw_survival_NC = "results/2021-10-25_model4_big_knots_total_trans/interim/{storage_threshold}/survival_NC_bootstrap_{bootstrap}.tsv",
	params:
		storage_threshold = "{storage_threshold}"
	resources:
		tmin = 60,
		mem_mb = get_mem_mb,
	threads: 1
	run:
		# Run analysis on each bootstrap
		ipw_model = [
			'Rscript scripts/ipw_coinflip_final_model4.R',
			'{input.data}',
			'{params.storage_threshold}',
			'{output.ipw_survival_fresh}',
			'{output.ipw_survival_old}',
			'{output.ipw_survival_NC}']
		shell(' '.join(ipw_model))


rule collect_results_mortality_model4:
	input:
		results = lambda wildcards: ["results/2021-10-25_model4_big_knots_total_trans/interim/"+wildcards.storage_threshold+"/survival_"+wildcards.trial+"_bootstrap_"+str(i)+".tsv" for i in BOOTSTRAP],
	output:
		ipw_survival = "results/2021-10-25_model4_big_knots_total_trans/processed/{storage_threshold}/survival_{trial}_estimates.tsv",
	resources:
		tmin = 60,
		mem_mb = 1024*30,
	threads: 1
	script:
		"scripts/collect_results.py"


rule write_analysis_code_to_results_folder_model4:
	input:
		code = "scripts/ipw_coinflip_final_model4.R",
	output:
		code = "results/2021-10-25_model4_big_knots_total_trans/analysis_code.R",
	resources:
		tmin = 10,
		mem_mb = 1024*2,
	shell:
		"""
		cp {input.code} {output.code}
		"""


rule IPW_marginal_structural_models_model5:
	input:
		data = "data/bootstrapped_mortality/bootstrap_{bootstrap}.tsv",
	output:
		ipw_survival_fresh = "results/2021-10-25_model5_0_10_20_30_40knots_total_trans/interim/{storage_threshold}/survival_fresh_bootstrap_{bootstrap}.tsv",
		ipw_survival_old = "results/2021-10-25_model5_0_10_20_30_40knots_total_trans/interim/{storage_threshold}/survival_old_bootstrap_{bootstrap}.tsv",
		ipw_survival_NC = "results/2021-10-25_model5_0_10_20_30_40knots_total_trans/interim/{storage_threshold}/survival_NC_bootstrap_{bootstrap}.tsv",
	params:
		storage_threshold = "{storage_threshold}"
	resources:
		tmin = 60,
		mem_mb = get_mem_mb,
	threads: 1
	run:
		# Run analysis on each bootstrap
		ipw_model = [
			'Rscript scripts/ipw_coinflip_final_model5.R',
			'{input.data}',
			'{params.storage_threshold}',
			'{output.ipw_survival_fresh}',
			'{output.ipw_survival_old}',
			'{output.ipw_survival_NC}']
		shell(' '.join(ipw_model))


rule collect_results_mortality_model5:
	input:
		results = lambda wildcards: ["results/2021-10-25_model5_0_10_20_30_40knots_total_trans/interim/"+wildcards.storage_threshold+"/survival_"+wildcards.trial+"_bootstrap_"+str(i)+".tsv" for i in BOOTSTRAP],
	output:
		ipw_survival = "results/2021-10-25_model5_0_10_20_30_40knots_total_trans/processed/{storage_threshold}/survival_{trial}_estimates.tsv",
	resources:
		tmin = 60,
		mem_mb = 1024*30,
	threads: 1
	script:
		"scripts/collect_results.py"


rule write_analysis_code_to_results_folder_model5:
	input:
		code = "scripts/ipw_coinflip_final_model5.R",
	output:
		code = "results/2021-10-25_model5_0_10_20_30_40knots_total_trans/analysis_code.R",
	resources:
		tmin = 10,
		mem_mb = 1024*2,
	shell:
		"""
		cp {input.code} {output.code}
		"""


rule IPW_marginal_structural_models_model6:
	input:
		data = "data/bootstrapped_mortality/bootstrap_{bootstrap}.tsv",
	output:
		ipw_survival_fresh = "results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/interim/{storage_threshold}/survival_fresh_bootstrap_{bootstrap}.tsv",
		ipw_survival_old = "results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/interim/{storage_threshold}/survival_old_bootstrap_{bootstrap}.tsv",
		ipw_survival_NC = "results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/interim/{storage_threshold}/survival_NC_bootstrap_{bootstrap}.tsv",
	params:
		storage_threshold = "{storage_threshold}"
	resources:
		tmin = 60,
		mem_mb = get_mem_mb,
	threads: 1
	run:
		# Run analysis on each bootstrap
		ipw_model = [
			'Rscript scripts/ipw_coinflip_final_model6.R',
			'{input.data}',
			'{params.storage_threshold}',
			'{output.ipw_survival_fresh}',
			'{output.ipw_survival_old}',
			'{output.ipw_survival_NC}']
		shell(' '.join(ipw_model))


rule collect_results_mortality_model6:
	input:
		results = lambda wildcards: ["results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/interim/"+wildcards.storage_threshold+"/survival_"+wildcards.trial+"_bootstrap_"+str(i)+".tsv" for i in BOOTSTRAP],
	output:
		ipw_survival = "results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/processed/{storage_threshold}/survival_{trial}_estimates.tsv",
	resources:
		tmin = 60,
		mem_mb = 1024*30,
	threads: 1
	script:
		"scripts/collect_results.py"


rule write_analysis_code_to_results_folder_model6:
	input:
		code = "scripts/ipw_coinflip_final_model6.R",
	output:
		code = "results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/analysis_code.R",
	resources:
		tmin = 10,
		mem_mb = 1024*2,
	shell:
		"""
		cp {input.code} {output.code}
		"""


######################################

# Composite Thrombo endpoint

rule IPW_marginal_structural_thrombo_event_models_model1:
	input:
		data = "data/bootstrapped_thrombo_new/bootstrap_{bootstrap}.tsv",
	output:
		ipw_survival_fresh = "results/2021-10-25_model1_interact_all_trans/thrombo_event/interim/{storage_threshold}/survival_fresh_bootstrap_{bootstrap}.tsv",
		ipw_survival_old = "results/2021-10-25_model1_interact_all_trans/thrombo_event/interim/{storage_threshold}/survival_old_bootstrap_{bootstrap}.tsv",
		ipw_survival_NC = "results/2021-10-25_model1_interact_all_trans/thrombo_event/interim/{storage_threshold}/survival_NC_bootstrap_{bootstrap}.tsv",
	params:
		storage_threshold = "{storage_threshold}"
	resources:
		tmin = 60,
		mem_mb = get_mem_mb,
	threads: 1
	run:
		# Run analysis on each bootstrap
		ipw_model = [
			'Rscript scripts/ipw_coinflip_final_model1.R',
			'{input.data}',
			'{params.storage_threshold}',
			'{output.ipw_survival_fresh}',
			'{output.ipw_survival_old}',
			'{output.ipw_survival_NC}']
		shell(' '.join(ipw_model))


rule collect_results_thrombo_event_model1:
	input:
		results = lambda wildcards: ["results/2021-10-25_model1_interact_all_trans/thrombo_event/interim/"+wildcards.storage_threshold+"/survival_"+wildcards.trial+"_bootstrap_"+str(i)+".tsv" for i in BOOTSTRAP],
	output:
		ipw_survival = "results/2021-10-25_model1_interact_all_trans/thrombo_event/processed/{storage_threshold}/survival_{trial}_estimates.tsv",
	resources:
		tmin = 60,
		mem_mb = 1024*30,
	threads: 1
	script:
		"scripts/collect_results.py"



rule IPW_marginal_structural_thrombo_event_models_model3:
	input:
		data = "data/bootstrapped_thrombo_new/bootstrap_{bootstrap}.tsv",
	output:
		ipw_survival_fresh = "results/2021-10-25_model3_all_trans_knots1_3_5_9_14/thrombo_event/interim/{storage_threshold}/survival_fresh_bootstrap_{bootstrap}.tsv",
		ipw_survival_old = "results/2021-10-25_model3_all_trans_knots1_3_5_9_14/thrombo_event/interim/{storage_threshold}/survival_old_bootstrap_{bootstrap}.tsv",
		ipw_survival_NC = "results/2021-10-25_model3_all_trans_knots1_3_5_9_14/thrombo_event/interim/{storage_threshold}/survival_NC_bootstrap_{bootstrap}.tsv",
	params:
		storage_threshold = "{storage_threshold}"
	resources:
		tmin = 60,
		mem_mb = get_mem_mb,
	threads: 1
	run:
		# Run analysis on each bootstrap
		ipw_model = [
			'Rscript scripts/ipw_coinflip_final_model3.R',
			'{input.data}',
			'{params.storage_threshold}',
			'{output.ipw_survival_fresh}',
			'{output.ipw_survival_old}',
			'{output.ipw_survival_NC}']
		shell(' '.join(ipw_model))


rule collect_results_thrombo_event_model3:
	input:
		results = lambda wildcards: ["results/2021-10-25_model3_all_trans_knots1_3_5_9_14/thrombo_event/interim/"+wildcards.storage_threshold+"/survival_"+wildcards.trial+"_bootstrap_"+str(i)+".tsv" for i in BOOTSTRAP],
	output:
		ipw_survival = "results/2021-10-25_model3_all_trans_knots1_3_5_9_14/thrombo_event/processed/{storage_threshold}/survival_{trial}_estimates.tsv",
	resources:
		tmin = 60,
		mem_mb = 1024*30,
	threads: 1
	script:
		"scripts/collect_results.py"



rule IPW_marginal_structural_thrombo_models_model4:
	input:
		data = "data/bootstrapped_thrombo_new/bootstrap_{bootstrap}.tsv",
	output:
		ipw_survival_fresh = "results/2021-10-25_model4_big_knots_total_trans/thrombo_event/interim/{storage_threshold}/survival_fresh_bootstrap_{bootstrap}.tsv",
		ipw_survival_old = "results/2021-10-25_model4_big_knots_total_trans/thrombo_event/interim/{storage_threshold}/survival_old_bootstrap_{bootstrap}.tsv",
		ipw_survival_NC = "results/2021-10-25_model4_big_knots_total_trans/thrombo_event/interim/{storage_threshold}/survival_NC_bootstrap_{bootstrap}.tsv",
	params:
		storage_threshold = "{storage_threshold}"
	resources:
		tmin = 60,
		mem_mb = get_mem_mb,
	threads: 1
	run:
		# Run analysis on each bootstrap
		ipw_model = [
			'Rscript scripts/ipw_coinflip_final_model4.R',
			'{input.data}',
			'{params.storage_threshold}',
			'{output.ipw_survival_fresh}',
			'{output.ipw_survival_old}',
			'{output.ipw_survival_NC}']
		shell(' '.join(ipw_model))


rule collect_results_thrombo_model4:
	input:
		results = lambda wildcards: ["results/2021-10-25_model4_big_knots_total_trans/thrombo_event/interim/"+wildcards.storage_threshold+"/survival_"+wildcards.trial+"_bootstrap_"+str(i)+".tsv" for i in BOOTSTRAP],
	output:
		ipw_survival = "results/2021-10-25_model4_big_knots_total_trans/thrombo_event/processed/{storage_threshold}/survival_{trial}_estimates.tsv",
	resources:
		tmin = 60,
		mem_mb = 1024*30,
	threads: 1
	script:
		"scripts/collect_results.py"



rule IPW_marginal_structural_thrombo_models_model5:
	input:
		data = "data/bootstrapped_thrombo_new/bootstrap_{bootstrap}.tsv",
	output:
		ipw_survival_fresh = "results/2021-10-25_model5_0_10_20_30_40knots_total_trans/thrombo_event/interim/{storage_threshold}/survival_fresh_bootstrap_{bootstrap}.tsv",
		ipw_survival_old = "results/2021-10-25_model5_0_10_20_30_40knots_total_trans/thrombo_event/interim/{storage_threshold}/survival_old_bootstrap_{bootstrap}.tsv",
		ipw_survival_NC = "results/2021-10-25_model5_0_10_20_30_40knots_total_trans/thrombo_event/interim/{storage_threshold}/survival_NC_bootstrap_{bootstrap}.tsv",
	params:
		storage_threshold = "{storage_threshold}"
	resources:
		tmin = 60,
		mem_mb = get_mem_mb,
	threads: 1
	run:
		# Run analysis on each bootstrap
		ipw_model = [
			'Rscript scripts/ipw_coinflip_final_model5.R',
			'{input.data}',
			'{params.storage_threshold}',
			'{output.ipw_survival_fresh}',
			'{output.ipw_survival_old}',
			'{output.ipw_survival_NC}']
		shell(' '.join(ipw_model))


rule collect_results_thrombo_model5:
	input:
		results = lambda wildcards: ["results/2021-10-25_model5_0_10_20_30_40knots_total_trans/thrombo_event/interim/"+wildcards.storage_threshold+"/survival_"+wildcards.trial+"_bootstrap_"+str(i)+".tsv" for i in BOOTSTRAP],
	output:
		ipw_survival = "results/2021-10-25_model5_0_10_20_30_40knots_total_trans/thrombo_event/processed/{storage_threshold}/survival_{trial}_estimates.tsv",
	resources:
		tmin = 60,
		mem_mb = 1024*30,
	threads: 1
	script:
		"scripts/collect_results.py"


rule IPW_marginal_structural_thrombo_models_model6:
	input:
		data = "data/bootstrapped_thrombo_new/bootstrap_{bootstrap}.tsv",
	output:
		ipw_survival_fresh = "results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/thrombo_event/interim/{storage_threshold}/survival_fresh_bootstrap_{bootstrap}.tsv",
		ipw_survival_old = "results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/thrombo_event/interim/{storage_threshold}/survival_old_bootstrap_{bootstrap}.tsv",
		ipw_survival_NC = "results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/thrombo_event/interim/{storage_threshold}/survival_NC_bootstrap_{bootstrap}.tsv",
	params:
		storage_threshold = "{storage_threshold}"
	resources:
		tmin = 60,
		mem_mb = get_mem_mb,
	threads: 1
	run:
		# Run analysis on each bootstrap
		ipw_model = [
			'Rscript scripts/ipw_coinflip_final_model6.R',
			'{input.data}',
			'{params.storage_threshold}',
			'{output.ipw_survival_fresh}',
			'{output.ipw_survival_old}',
			'{output.ipw_survival_NC}']
		shell(' '.join(ipw_model))


rule collect_results_thrombo_model6:
	input:
		results = lambda wildcards: ["results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/thrombo_event/interim/"+wildcards.storage_threshold+"/survival_"+wildcards.trial+"_bootstrap_"+str(i)+".tsv" for i in BOOTSTRAP],
	output:
		ipw_survival = "results/2021-10-25_model6_0_10_20_30_40knots_total_trans_noInteract/thrombo_event/processed/{storage_threshold}/survival_{trial}_estimates.tsv",
	resources:
		tmin = 60,
		mem_mb = 1024*30,
	threads: 1
	script:
		"scripts/collect_results.py"







