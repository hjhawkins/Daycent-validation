###############################################################################
#
# run_DDcent_savanna_advanced.R
# modified from run_DDcent_wooster_advanced.R
#
# Author: Melannie Hartman 
#         February 23, 2018
#         August 14, 2018
#
# Adjusted for savanna template: 
#         Heidi Hawkins
#         March 31, 2019
#
# Description:
#   This R script runs DayCent model simulations for savanna/grassland ecosystems, SA.
#   It is set up to run an equilibrium simulation followed by a base savanna/grassland
#   simulation with no known cultivation, followed by herbivory and fire treatments 
#   from the Brotherton Experiment (see mgmtOptions below).
#
###############################################################################

# Reset the variables 
rm(list=ls())

# Names of executable programs
daycent <- "DDcent_vDec2016.exe"
list100 <- "DDCent_list100_vDec2016.exe"

# Path to files
modelPath = "D:/Temperate_Savanna_Brotherton"
#modelPath = getwd()
setwd(modelPath)

siteName = "brotherton"

# List the managment options to simulate:
mgmtOptions <- c("nhnf", "nhbf", "nhaf", "hbf", "haf")


# Set these variables to TRUE to rerun the spinup and base cropping run
doSpin = FALSE
doBase = FALSE

# --------------- Step 1: Run equilibrium simulation --------------- 
#
# This equilibrium simulation takes a long time, so don't execute
# these commands if you already have an equilibrium binary file and 
# you haven't made any changes to any parameter files.

if (doSpin)
{
    scheq = paste(siteName, "eq", sep="_")
    scheqext = paste(scheq, ".sch", sep="")
    bineq = scheq
    bineqext = paste(bineq, ".bin", sep="")
    unlink(bineqext)   # DayCent won't allow you to write over an existing .bin file
    file.copy("outfiles_eq.in", "outfiles.in", overwrite=TRUE)  # Don't save daily files for equilibrium run

    # Standard error and output from DayCent are written to files 
    # stdout_dc_eq.log, stderr_dc_eq.log
    logfile1 <- "stdout_dc_eq.log"
    logfile2 <- "stderr_dc_eq.log"
    unlink(logfile1)
    unlink(logfile2)

    # Run DayCent: DDcent_vDec2016.exe -s <sch file> -n <new bin file>
    args1 <- paste("-s", scheq, "-n", bineq, sep=" ")
    print(paste(daycent, args1, sep=" "))
    system2(daycent, args=args1, wait=TRUE, stdout=logfile1, stderr=logfile2)


    # Delete the existing equilibrium .lis file
    lisName <- paste(bineq,".lis", sep="")
    unlink(lisName) # List100 won't allow you to write over an existing .lis file
   
    # Standard error and output from List100 are written to files 
    # stdout_list100_eq.log, stderr_list100_eq.log 
    logfile3 <- "stdout_list100_eq.log"
    logfile4 <- "stderr_list100_eq.log"
    unlink(logfile3)
    unlink(logfile4)

    #Run List100: DDCent_list100_vDec2016.exe <bin file> <lis file> outvars.txt
    args2 <- paste(bineq, bineq, "outvars.txt", sep=" ")
    print(paste(list100, args2, sep=" "))
    system2(list100, args=args2, wait=TRUE, 
            stdout=logfile3, stderr=logfile4)

}

# --------------- Step 2: Run base grassland simulation --------------- 

if (doBase)
{
    bineq = paste(siteName, "eq", sep="_")
    schbase = paste(siteName, "base", sep="_")
    schbaseext = paste(schbase, ".sch", sep="")
    binbase = paste(siteName, "base", sep="_")
    binbaseext = paste(binbase, ".bin", sep="")
    unlink(binbaseext)     # DayCent won't allow you to write over an existing .bin file
    file.copy("outfiles_eq.in", "outfiles.in", overwrite=TRUE)  # Don't save daily files for base run
    
    # Standard error and output from DayCent are written to files 
    # stdout_dc_base.log, stderr_dc_base.log

    logfile1 <- "stdout_dc_base.log"
    logfile2 <- "stderr_dc_base.log"
    unlink(logfile1)
    unlink(logfile2)

    # Run DayCent: DDcent_vDec2016.exe -s <sch file> -n <new bin file> -e <prev bin file> 
    args1 <- paste("-s", schbase, "-n", binbase, "-e", bineq, sep=" ")
    print(paste(daycent, args1, sep=" "))
    system2(daycent, args=args1, wait=TRUE, stdout=logfile1, stderr=logfile2)


    # Delete the existing base .lis file
    lisName <- paste(binbase,".lis", sep="")
    unlink(lisName) # List100 won't allow you to write over an existing .lis file
   
    # Standard error and output from List100 are written to files 
    # stdout_list100_base.log, stderr_list100_base.log 
    logfile3 <- "stdout_list100_base.log"
    logfile4 <- "stderr_list100_base.log"
    unlink(logfile3)
    unlink(logfile4)

    #Run List100: DDCent_list100_vDec2016.exe <bin file> <lis file> outvars.txt
    args2 <- paste(binbase, binbase, "outvars.txt", sep=" ")
    print(paste(list100, args2, sep=" "))
    system2(list100, args=args2, wait=TRUE, stdout=logfile3, stderr=logfile4)

}

# --------------- Step 3: Run experimental management practices --------------- 

file.copy("outfiles_exp.in", "outfiles.in", overwrite=TRUE) # Save daily files for experiment runs
binbase = paste(siteName, "base", sep="_")

for (mgmt in mgmtOptions)
{
    schmgmt = paste(siteName, mgmt, sep="_")
    schmgmtext = paste(schmgmt, ".sch", sep="")
    binmgmt = schmgmt

    if (file.exists(schmgmtext))
    {
        # Remove the existing .bin file
        binmgmtext = paste(binmgmt, ".bin", sep="")
        unlink(binmgmtext)

        # Run DayCent: DDcent_vDec2016.exe -s <sch name> -n <new bin file> -e <base bin file> 

        # Standard error and output from DayCent are written to files 
        # stdout_dc_mgmt.log, stderr_dc_mgmt.log

        logfile1 <- paste("stdout_dc_", mgmt, ".log", sep="")
        logfile2 <- paste("stderr_dc_", mgmt, ".log", sep="")
        unlink(logfile1)
        unlink(logfile2)

        # Run DayCent: DD15centEVI.exe -s <sch file> -n <new bin file> -e <prev bin file> 
        args1 <- paste("-s", schmgmt, "-n", binmgmt, "-e", binbase, sep=" ")
        print(paste(daycent, args1, sep=" "))
        system2(daycent, args=args1, wait=TRUE, stdout=logfile1, stderr=logfile2)

        # Rename out files (e.g. dc_sip.csv, vswc.out, soilc.out) from each run
        dc_sipFile = paste(modelPath, "\\dc_sip_", mgmt, ".csv", sep="")
        file.rename("dc_sip.csv", dc_sipFile)
        
        #soilcFile = paste(modelPath, "\\soilc_", mgmt, ".out", sep="")
        #file.rename("soilc.out", soilcFile)
        
        #vswcFile = paste(modelPath, "\\vswc_", mgmt, ".out", sep="")
        #file.rename("vswc.out", vswcFile)
        
        #year_summaryFile = paste(modelPath, "\\year_summary_", mgmt, ".out", sep="")
        #file.rename("year_summary.out", year_summaryFile)
        
        #year_cflowsFile = paste(modelPath, "\\year_cflows_", mgmt, ".out", sep="")
        #file.rename("year_cflows.out", year_cflowsFile)
        
        #cflowsFile = paste(modelPath, "\\cflows_", mgmt, ".out", sep="")
        #file.rename("cflows.out", cflowsFile) 
        
        
        # Delete the existing .lis file for the current management
        lisName <- paste(binmgmt,".lis", sep="")
        unlink(lisName) # List100 won't allow you to write over an existing .lis file
   
        # Standard error and output from List100 are written to files 
        # stdout_list100_mgmt.log, stderr_list100_mgmt.log 
        logfile3 <- paste("stdout_list100_", mgmt, ".log", sep="")
        logfile4 <- paste("stderr_list100_", mgmt, ".log", sep="")
        unlink(logfile3)
        unlink(logfile4)

        #Run List100: DDCent_list100_vDec2016.exe <bin file> <lis file> outvars.txt
        args2 <- paste(binmgmt, binmgmt, "outvars.txt", sep=" ")
        print(paste(list100, args2, sep=" "))
        system2(list100, args=args2, wait=TRUE, stdout=logfile3, stderr=logfile4)

    } else 
    {
       msg = paste(schmgmtext, " does not exist.")
       print(msg)
    }
}

