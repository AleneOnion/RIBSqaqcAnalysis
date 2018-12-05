## Program:  RIBS_QC_checks_v7.R
## Purpose: Performs multiple QC checks on RIBS chemistry data. Outputs a text summary report, CSV tables of failed
  ## samples for each check, and a CSV containing all samples.
  ## v7 removed EDD combining block (now done in separate script, "Combine_RIBS_EDDs_v5.R")
## Author: Gavin Lemley
## Original version created Fall 2017
## Last updated: 10/9/2018

## Input file: Bound, joined EDD file with headers from script "Combine_RIBS_EDDs_v5.R". Ensure date format of sample date and analysis date
  # fields are compatible with date converting code below. (see block note "# Convert dates from factors to POSIXct objects"). Change either
  # the file or the code to match.

##### MAKE SURE TO CLEAR WORKSPACE BEFORE RUNNING #####


library(lubridate)


####### Define input/output directories  #########

## Set input dir, filename, and output dir below (Must first create output folder)

input_dir <- "C:/Rscripts/RIBS.Data/data/run/"
setwd(input_dir)

RIBS_input <- read.table("Combine_EDDs_output_FixedEBs.csv",sep=",",fill=TRUE,header=TRUE)

output_dir <- "C:/Rscripts/RIBS.Data/output/2018_EBs_20181009/"
setwd(output_dir)

##################################################


##### LAB QC checks #####

### Identify RPD failures; relative % diff (RPD) exceeds control limit (qc_rpd_status with asterisks)
# Returns subset dataframe using logical search (grepl)
rpd_cl_fail <- subset(RIBS_input, grepl("[*]", qc_rpd_status))

### Identify spike failures (qc_spike_status cells with asterisks)
###   and dup spike failures (qc_dup_spike_status cells with asterisks)
spike_fail <- subset(RIBS_input, grepl("[*]", qc_spike_status))
spike_dup_fail <- subset(RIBS_input, grepl("[*]", qc_dup_spike_status))

### LAB blanks checks 
#Identify lab_qualifiers and interpreted_qualifiers cells containing "B" (meaning sample results <  lab blanks; possible contamination)
lab_blank_fail_LabQual <- subset(RIBS_input, grepl("B", lab_qualifiers))
lab_blank_fail_IntQual <- subset(RIBS_input, grepl("B", interpreted_qualifiers))


##### FIELD DATA QC checks #####

# Subset field data
field_data <- subset(RIBS_input, grepl("field", sample_source,ignore.case=TRUE))

# List unique field sample names and sort for easy reference in report. Count various QAQC sample types for report.
sample_names <- sort(unique(field_data$sample_name))
# These also contain dissolved ("..._Diss") samples, which are split off from the original sample.
#sample_names2 <- !grep("Diss", sample_names)
sample_count <- length(sample_names)
EB_sample_count <- length(grep("-EB", sample_names))
FB_sample_count <- length(grep("FB", sample_names))
DUP_sample_count <- length(grep("DUP", sample_names))


### EQUIPMENT and FIELD blank (EB & FB) checks

#Creates subset dataframe of failed field EB & FB samples (result > quantitation limit)
## 2017 data- Note that "EB" or "FB" may fall in different places and have different separators in sample ID due to human error. 
## 2016 has FB sample IDs for params other than mercury. What to with these?? 
## Some locations include EB or EB (XFBK, SLEB, and GEEB)!!! Need to use regex to account for this! 
  # Code below includes results with these locations. Should redo code to split out sample id by separator into separate fields.
## Use "sample_name" field instead of sys_sample_code? Added info in sys_sample_code may be accessible in other fields.

EBfail <- subset(field_data, grepl("-EB", sys_sample_code) & (result_value > quantitation_limit))
FBfail <- subset(field_data, grepl("FB", sys_sample_code) & result_value > quantitation_limit)
# FBfail <- subset(RIBS_input, grepl("*FB", sys_sample_code) & !grepl("*FBK", sys_sample_code) & result_value > quantitation_limit)
# FBfail <- subset(RIBS_input, grepl("(?=.*FB)(?!.*FBK)(.+)", sys_sample_code) & result_value > quantitation_limit)


# Make sure "not detected" result_comment present if result_value is blank (create table if true)
RESULT_MISSING <- subset(field_data, grepl("not detected", result_comment) & !is.na(result_value))
#   If not, create table of missing results...
if(nrow(RESULT_MISSING) > 1){
  write.table(RESULT_MISSING, file="RESULT_MISSING.csv",sep=",", row.names = FALSE)
}

## Create tables for all EB or FB samples 
All_EBs <- subset(field_data, grepl("-EB", sys_sample_code) )
All_FBs <- subset(field_data, grepl("FB", sys_sample_code) )

# TEST (manually) IF THIS STILL RETURNS just FBs!
# All_FBs_NO_FBK <- subset(RIBS_input, grepl("FB", sys_sample_code) & !grepl("FBK", sys_sample_code) )
# All_FBs <- subset(RIBS_input, grepl("^(?=.*FB)(?!.*FBK)", sys_sample_code) )



### Holding times check block ###

# Load holding times reference table
holdingtimes_table <- read.csv("C:/Rscripts/RIBS.Data/ref_tables/RIBS_holding_times.csv",sep=",",fill=TRUE,header=TRUE)

# Make simpler holding time lookup table with only two columns
HTlookup_tbl <- subset(holdingtimes_table, select = c("cas_rn","HTL_withpadding"))
# Merge RIBS data frame and holding times lookup table
RIBS_data_HTs <- merge(field_data, HTlookup_tbl, by = "cas_rn", all.x = TRUE)

# Convert dates from factors to POSIXct objects
RIBS_data_HTs$sample_date <- mdy_hms(RIBS_data_HTs$sample_date)
RIBS_data_HTs$analysis_date <- mdy_hms(RIBS_data_HTs$analysis_date)

# RIBS_data_HTs$sample_date <- ymd_hms(RIBS_data_HTs$sample_date)
# RIBS_data_HTs$analysis_date <- ymd_hms(RIBS_data_HTs$analysis_date)


# Calculate difference between sample datetimes and analysis datetimes (actual holding times)
RIBS_data_HTs$holdingtime_actual_hrs <- difftime(RIBS_data_HTs$analysis_date ,RIBS_data_HTs$sample_date , units = c("hours"))

# Calculate holding time exceedances 
RIBS_data_HTs$holdingtime_exceedance <- RIBS_data_HTs$holdingtime_actual_hrs - RIBS_data_HTs$HTL_withpadding
RIBS_data_HTs$holdingtime_exceedance <- as.numeric(RIBS_data_HTs$holdingtime_exceedance)

# Create subset of samples with holding times exceedances (>0 hours)
holdingtime_fail <- subset(RIBS_data_HTs, holdingtime_exceedance > 0)



########################



## set output directory ...already did above?
# setwd("C:/Rscripts/RIBS.Data/output")

## Export all tables (if records beyond the header exist)
write.table(RIBS_input, file="RIBS_input_data.csv",sep=",", row.names = FALSE)
if(nrow(rpd_cl_fail) > 0){write.table(rpd_cl_fail, file="rpd_fail.csv",sep=",", row.names = FALSE)}
if(nrow(spike_fail) > 0){write.table(spike_fail, file="spike_fail.csv",sep=",", row.names = FALSE)}
if(nrow(spike_dup_fail) > 0){write.table(spike_dup_fail, file="spike_dup_fail.csv",sep=",", row.names = FALSE)}
if(nrow(lab_blank_fail_LabQual) > 0){write.table(lab_blank_fail_LabQual, file="lab_blank_fail_LabQual.csv",sep=",", row.names = FALSE)}
if(nrow(lab_blank_fail_IntQual) > 0){write.table(lab_blank_fail_IntQual, file="lab_blank_fail_IntQual.csv",sep=",", row.names = FALSE)}
if(nrow(EBfail) > 0){write.table(EBfail, file="EB_fail.csv",sep=",", row.names = FALSE)}
if(nrow(FBfail) > 0){write.table(FBfail, file="FB_fail.csv",sep=",", row.names = FALSE)}
if(nrow(holdingtime_fail) > 0){write.table(holdingtime_fail, file="holdingtime_fail.csv",sep=",", row.names = FALSE)}

## Create summary report
RIBS_QAQC_report<-file("RIBS_QAQC_report.txt")
writeLines(c(
  "RIBS QA check report summary", format(Sys.time(), "%a %b %d %X"),
  "\nSee \"RIBS_input.csv\" for full list of input data\n",
  "-----------------------\n",
  paste0("Total number of analysis results present: ",nrow(RIBS_input)),"\n",
  paste0("Lab QAQC checks:\n"),
  paste0(nrow(rpd_cl_fail), "  RPD failures (relative % diff between dup runs exceeds control limit; calc'd by lab; 'qc_rpd_status' field)\n"),
  paste0(nrow(spike_fail), "  Spike failures (sample result >4x spike (not useful); calc'd by lab; 'qc_spike_status' field)"),
  paste0(nrow(spike_dup_fail),"  Spike dup failures (sample result >4x spike (not useful); calc'd by lab; 'qc_dup_spike_status' field)\n"),
  paste0(nrow(lab_blank_fail_LabQual), "  Lab blank failures (possible contamination; calc'd by lab; 'lab_qualifiers' field)"),
  paste0(nrow(lab_blank_fail_IntQual), "  Lab blank failures (possible contamination; calc'd by lab; 'interpreted_qualifiers' field)\n"),
  paste0("Field sample QAQC checks:\n"),
  paste0(EB_sample_count, "  Equipment blank samples present (May include separate Diss samples)"),
  paste0(nrow(All_EBs), "  Equipment blank results present"),
  paste0(nrow(EBfail), "  Equipment blank results failed (result > quantitation limit)\n"),
  paste0(FB_sample_count, "  Field blank samples present (May include separate Diss samples)"),
  paste0(nrow(All_FBs), "  Field blank results present"),
  paste0(nrow(FBfail), "  Field blank results failed (result > quantitation limit)\n"),
  paste0(DUP_sample_count, "  Duplicate samples present (May include separate Diss samples)\n(DUP tests to be added)\n"),
  paste0(nrow(holdingtime_fail), "  Holding time failures (HT > HT limit [with added 'padding'])\n\n"),
  paste0(sample_count," unique sample names present:\n"),
  paste0(sample_names),
  
  "-----------------------\n",

  
  if(nrow(RESULT_MISSING) > 1){paste0(nrow(RESULT_MISSING), "  RESULT VALUES MISSING ('not detected' not indicated)")}
  ), RIBS_QAQC_report)
close(RIBS_QAQC_report)
