#This script can read in the raw data files from both EMS and TOMST dendrometers (Mini32 
#and lolly) rename columns by their tree ID and QA/QC the data using treenetproc. You can 
#further visualize the data, export graphs using treenetproc. Further down, you can look 
#at individual trees, or look at graphs by species, and other interesting groupings
#and finally calculate and plot daily values that have been zeroed at midnight

#This script is specifically for looking at PJ data
#written by: Rachael Auer

# Edited by Derek Kober July 20, 2025.
## removed tower data lines, and EMS lines.
## made daily pattern code at the end faster
## cleaned up some names
## works as of July 20, 2025. Able to run all code at once without issues.

#clears work space
rm(list=ls(all=TRUE))

#read in important libraries
library(treenetproc)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(myClim)
library(tidyverse)
library(readxl)
library(purrr)
library(cowplot)
library(RColorBrewer)
library(ggpubr)
library(beepr)
library(janitor)

###########EMS and TOMST file cleaning################ 
#TOMST data needs to be compiled manually in R studio - SEE BELOW EMS SECTION TO IMPORT TOMST DATA 

###for TOMST files that are all in one folder on the Synology drive and are the raw .xlsx files from download #######
#####this section will read in the .xlsx files from the folder and convert them into dataframes that can be used
#by the below program and combine them together and name the columns by their tree ID

# CHANGE HERE- Set the path to your folder containing the Excel files
folder_path <- "./data/raw/tomst/MPJ/2025/07.11.25"

#read in serial number and name files - DON'T CHANGE UNLESS YOU HAVE TO
names <- read.csv('./data/raw/tomst/MPJ/tomst_sn_names.csv', sep=',', stringsAsFactors=FALSE)

as.list(names)

# List all Excel files in the folder
file_list <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)

#create null object to be filled with the data
tomst <-  NULL

#for loop that does all the things
for (f in 1:length(file_list)) {
  
  
  tomstpath <- file_list[f] #sets new filepath to look at
  tomstfile <- read.csv(tomstpath) #reads in file
  file_name <- tools::file_path_sans_ext(basename(tomstpath)) #save file name for IDing columns
  file_name <- substr(file_name, 6, 13) #just extract the serial number
  new_name <- as.vector(names[names$SN == file_name,][2]) #matches serial number and saves the tree ID 
  split <- tomstfile %>%                #split file - basically Excel's 'text to column' function
    separate(1, c("index", "ts", "tz", "temp", "coor1", "coor2", "value", "coor3", "qf"), ";") 
  split$value <- as.numeric(as.character(split$value)) #need to make sure values are numeric and not characters
  split$temp <- as.numeric(as.character(split$temp)) #need to make sure values are numeric and not characters
  split <- split %>%                    #save the important columns and rename them with their tree ID 
    select(ts, temp, value) %>%
    rename(setNames("value", paste(new_name, "value", sep = "."))) %>%
    rename(setNames("temp", paste(new_name, "temp", sep = ".")))
  
  
  dup <- split %>%
    group_by(ts) %>%
    mutate(n = n()) %>%
    arrange(desc(n))
  
  #checks and removes extra values
  if (head(dup$n, n=1) > 1) {
    split <- dup[-1,]
    split <- split %>%
      arrange(ts)
  }
  
  
  #update/create new tomst dataframe with dendro values
  if (is.null(tomst)) {
    tomst <- split
  } else { 
    tomst <- full_join(tomst, split, by="ts")
    
  }
  
}



#########QA/QC data############
# Step 1: Time-alignment (L1)
#for tomst date format
tomst <- proc_L1(data=tomst, reso=15, input="wide", date_format = "%Y.%m.%d %H:%M", tz="UTC")

#we need to zero the values to when we installed

####tomst turn!
#start by saving under new name just in case
tomsttest <- tomst
#ts column needs to be converted to correct time zone
tomsttest$ts <- as_datetime(tomsttest$ts, tz="MST")

#subset to after we installed and zero the values

#this dendrometer needed to be fixed after realizing it wasn't fully installed and needs to be zeroed separately
tomstfix <- tomsttest[tomsttest$series %in% c("PHR4SAP2.temp", "PHR4SAP2.value"),]
tomstother <- tomsttest[tomsttest$series %in% c("JCP292.temp", "JCP292.value", "JCP738.temp",
                                                "JCP738.value", "JHR4SAP3.temp", "JHR4SAP3.value", 
                                                "JSAP5.temp", "JSAP5.value", "JSAP6.temp", 
                                                "JSAP6.value", "PCP728.temp", "PCP728.value", 
                                                "PCP754.temp", "PCP754.value", "PSAP11.temp",
                                                "PSAP11.value", "PSAP13.temp", "PSAP13.value", 
                                                "J20.value", "J20.temp", "P20.value", "P20.temp",
                                                "J21.value", "J21.temp", "J22.value", "J22.temp",
                                                "J23.value", "J23.temp", "P21.value", "P21.temp", 
                                                "J24.value", "J24.temp", "J25.value", "J25.temp",
                                                "J26.value", "J26.temp", "J27.value", "J27.temp",
                                                "P22.value", "P22.temp"),]
#all other dendros subsetted to date of install
tomstother <- tomstother[tomstother$ts >= "2024-07-12 18:30:00",]
#subsetted to after fix
tomstfix <- tomstfix[tomstfix$ts >= "2024-10-02 21:00:00",]
#put back together
tomsttest1 <- rbind(tomstfix, tomstother)

#installed some dendrometers in front of thermal cameras On April 09, 2025 and need to zero these values separately
tomst_therm <- tomsttest1[tomsttest1$series %in% c("J20.value", "J20.temp", "P20.value", "P20.temp",
                                                   "J21.value", "J21.temp", "J22.value", "J22.temp",
                                                   "J23.value", "J23.temp", "P21.value", "P21.temp"),]
tomst_rest <- tomsttest1[tomsttest1$series %in% c("PHR4SAP2.temp", "PHR4SAP2.value", "JCP292.temp", 
                                                  "JCP292.value", "JCP738.temp","JCP738.value", 
                                                  "JHR4SAP3.temp", "JHR4SAP3.value", "JSAP5.temp", 
                                                  "JSAP5.value", "JSAP6.temp", 
                                                  "JSAP6.value", "PCP728.temp", "PCP728.value", 
                                                  "PCP754.temp", "PCP754.value", "PSAP11.temp",
                                                  "PSAP11.value", "PSAP13.temp", "PSAP13.value", 
                                                  "J24.value", "J24.temp", "J25.value", "J25.temp",
                                                  "J26.value", "J26.temp", "J27.value", "J27.temp",
                                                  "P22.value", "P22.temp"),]
#subset to after we installed
tomst_therm <- tomst_therm[tomst_therm$ts >= "2025-04-09 7:00:00",]

#put back together
tomsttest1 <- rbind(tomst_therm, tomst_rest)


#installed some dendrometers in front of thermal cameras On April 27, 2025 and need to zero these values separately
tomst_therm <- tomsttest1[tomsttest1$series %in% c("J24.value", "J24.temp", "J25.value", "J25.temp",
                                                   "J26.value", "J26.temp", "J27.value", "J27.temp",
                                                   "P22.value", "P22.temp"),]
tomst_rest <- tomsttest1[tomsttest1$series %in% c("PHR4SAP2.temp", "PHR4SAP2.value", "JCP292.temp", 
                                                  "JCP292.value", "JCP738.temp","JCP738.value", 
                                                  "JHR4SAP3.temp", "JHR4SAP3.value", "JSAP5.temp", 
                                                  "JSAP5.value", "JSAP6.temp", 
                                                  "JSAP6.value", "PCP728.temp", "PCP728.value", 
                                                  "PCP754.temp", "PCP754.value", "PSAP11.temp",
                                                  "PSAP11.value", "PSAP13.temp", "PSAP13.value", 
                                                  "J20.value", "J20.temp", "P20.value", "P20.temp",
                                                  "J21.value", "J21.temp", "J22.value", "J22.temp",
                                                  "J23.value", "J23.temp", "P21.value", "P21.temp"),]
#subset to after we installed
tomst_therm <- tomst_therm[tomst_therm$ts >= "2025-04-27 09:15:00",]

#put back together
tomsttest1 <- rbind(tomst_therm, tomst_rest)


#need to convert data from 15 minute to hourly - select values only at the hour
tomsttest1$hour <- hour(tomsttest1$ts)
tomsttest1$minute <- minute(tomsttest1$ts)
#subset to just hour
tomsttest1 <- tomsttest1[tomsttest1$minute == 0,]

#subset to just 2025 data
tomst2025 <- tomsttest1[tomsttest1$ts >= "2024-12-31 17:00:00",]

#list of trees to cycle through to zero data
tomstlist <- c("PHR4SAP2", "JHR4SAP3", "PSAP13", "PSAP11", "JSAP5", 
               "JSAP6", "PCP728", "JCP738", "PCP754", "JCP292", 
               "J20", "P20", "J21", "J22", "J23", "P21", "J24", 
               "J25", "J26", "J27", "P22" )

#empty dataframe to store data in
tomstadj <- data.frame()
tomstadj2025 <- data.frame()

#for loop that zeros data to date of install
for (e in 1:length(tomstlist)) {
  #new tree
  tree <- tomstlist[e]
  #subset to just that trees dendro data
  tomstsub <- tomsttest1 %>%
    filter(str_detect(series, paste0(tree, ".value"))) 
  #find first intial value
  inst_val <- head(tomstsub, n=1) 
  #subtract that value from all values
  tomstsub <- tomstsub %>%
    mutate(value = value - inst_val$value)
  #get temperature data
  tomsttempsub <- tomsttest1 %>%
    filter(str_detect(series, paste0(tree, ".temp"))) 
  #put back together
  tomstsub <- rbind(tomstsub, tomsttempsub)
  #store data in dataframe
  tomstadj <- rbind(tomstadj, tomstsub)
}


#select just these columns (probably not necessary?)
tomstadj <- tomstadj %>%
  select("ts", "series", "value", "version")

#2025's turn
for (e in 1:length(tomstlist)) {
  #new tree
  tree <- tomstlist[e]
  #subset to that tree's dendrometer data
  tomstsub <- tomst2025 %>%
    filter(str_detect(series, paste0(tree, ".value"))) 
  #find initial value
  inst_val <- head(tomstsub, n=1) 
  #subtract initial value
  tomstsub <- tomstsub %>%
    mutate(value = value - inst_val$value)
  #get temp data
  tomsttempsub <- tomst2025 %>%
    filter(str_detect(series, paste0(tree, ".temp"))) 
  #combine back together
  tomstsub <- rbind(tomstsub, tomsttempsub)
  #store in dataframe
  tomstadj2025 <- rbind(tomstadj2025, tomstsub)
}


############cleaning all data at once################
#create an empty dataset to be filled in with the cleaned data

clean_dendro_tomst <- data.frame(
  series = character(),
  ts = character(),
  value = numeric(),
  max = numeric(),
  twd = numeric(),
  gro_yr = numeric(),
  frost = logical(),
  flags = logical(),
  version = character()
)

clean_dendro_tomst_2025 <- data.frame()

tree.increment.tomst.pj <- c("PHR4SAP2", "JHR4SAP3", "PSAP13", "PSAP11", "JSAP5", 
                             "JSAP6", "PCP728", "JCP738", "PCP754", "JCP292",
                             "J20", "P20", "J21", "J22", "J23", "P21", 
                             "J26", "J27", "P22")

#"J24", "J25"

#for loop that cycles through the trees, and cleans the data using temp data to correct for it. 
#also detects jumps of more  than 5 militers (change in the tol_jump argument of proc_dendro_L2)
#can take a while if you ask it to plot as well

#tomst turn
for (s in 1:length(tree.increment.tomst.pj)) {
  #new tree
  tree <- tree.increment.tomst.pj[s]
  #subset of that trees temp data
  temp_dendro <- tomstadj %>%
    filter(str_detect(series, tree)) %>%
    filter(str_detect(series, "temp"))
  #subset of that trees dendro data
  dendro_l1 <- tomstadj %>%
    filter(str_detect(series, tree)) %>%
    filter(str_detect(series, "value"))
  #function that QA/QCs data and plots
  plot_path <- paste0("./output/plots/", tree)
  dendro_l2 <- proc_dendro_L2(dendro_L1 = dendro_l1, temp_L1 = temp_dendro, tol_jump = 13, plot_period = "yearly", plot_export = T, plot_name = plot_path)
  #create date column to combine with tower data
  dendro_l2$Date <- date(dendro_l2$ts)
  #combine with tower data
  # output <- left_join(dendro_l2, tower, by= "Date") DK: don't need tower data
  output <- dendro_l2
  #store data 
  clean_dendro_tomst <- rbind(clean_dendro_tomst, output)
}

#tomst turn in 2025
for (s in 1:length(tree.increment.tomst.pj)) {
  #new tree
  tree <- tree.increment.tomst.pj[s]
  #subset of that trees temp data
  temp_dendro <- tomstadj2025 %>%
    filter(str_detect(series, tree)) %>%
    filter(str_detect(series, "temp"))
  #subset of that trees dendro data
  dendro_l1 <- tomstadj2025 %>%
    filter(str_detect(series, tree)) %>%
    filter(str_detect(series, "value"))
  #function that QA/QCs data and plots
  plot_path <- paste0("./output/plots/", tree, "_2025")
  dendro_l2 <- proc_dendro_L2(dendro_L1 = dendro_l1, temp_L1 = temp_dendro, tol_jump = 13, plot_period = "yearly", plot_export = T, plot_name = plot_path)
  #create date column to combine with tower data
  dendro_l2$Date <- date(dendro_l2$ts)
  #combine with tower data
  # output <- left_join(dendro_l2, tower, by= "Date") dk
  output <- dendro_l2
  #store data
  clean_dendro_tomst_2025 <- rbind(clean_dendro_tomst_2025, output)
  
}


##########looking at specific trees######################
#If you want to look at a specific tree (and look at the plots that are generated from treeprocnet)
'temp_dendro <- ems %>%
  filter(series == "HR4.Temp", value > 0)

dendro_l1 <- ems %>%
  filter(series == "HR4.Increment", value > 0) 

# Step 2: Error detection and processing (L2)

# Clean time-aligned (L1) dendrometer data and plot changes
dendro_l2 <- proc_dendro_L2(dendro_L1 = dendro_l1, temp_L1 = temp_dendro, plot_period = "yearly", plot_export = F)'


##################plotting all of the clean data together
#first tomst data needs to be converted to diameter
clean_dendro_tomst1 <- clean_dendro_tomst
clean_dendro_tomst1$gro_yr <- ((clean_dendro_tomst1$gro_yr)) * 2 * pi
clean_dendro_tomst1$value <- ((clean_dendro_tomst1$value)) * 2 * pi
clean_dendro_tomst1$twd <- (clean_dendro_tomst1$twd) * 2 * pi
clean_dendro_tomst1$dendro <- "TOMST"

#put them all together
all_dendro <- clean_dendro_tomst1

#create species column
all_dendro <- all_dendro %>%
  mutate(species = case_when(
    str_detect(series, "J") ~ "Juniper", 
    str_detect(series, "P") ~ "Pinon", 
    .default = NA
  ))

#save compiled, cleaned data to synology drive
write.csv(all_dendro, file = paste0("./data/processed/PJ_DENDRO.csv"))

#2025 data's need transformations
clean_dendro_tomst1_2025 <- clean_dendro_tomst_2025
clean_dendro_tomst1_2025$gro_yr <- ((clean_dendro_tomst1_2025$gro_yr)) * 2 * pi
clean_dendro_tomst1_2025$value <- ((clean_dendro_tomst1_2025$value)) * 2 * pi
clean_dendro_tomst1_2025$twd <- (clean_dendro_tomst1_2025$twd) * 2 * pi
clean_dendro_tomst1_2025$dendro <- "TOMST"

all_dendro_2025 <- clean_dendro_tomst1_2025

all_dendro_2025 <- all_dendro_2025 %>%
  mutate(species = case_when(
    str_detect(series, "J") ~ "Juniper", 
    str_detect(series, "P") ~ "Pinon", 
    .default = NA
  ))

write.csv(all_dendro_2025, file = paste0("./data/processed/PJ_DENDRO_2025.csv"))


####This section is for daily zeroed values and should be run manually because the 
#calculations take around 10 minutes (or more!) - only rerun when you want new updated plots! 
#alternatively, you can read in the previous files to make plots with the data (below)
#gs_dendro_day <- read.csv("./Compiled Data/MPJ/PJ_DENDRO_daily_zero_means.csv", sep=',', fill=TRUE, stringsAsFactors=FALSE)
#dendro_day_zero <- read.csv("./Compiled Data/MPJ/PJ_DENDRO_daily_zero_means.csv", sep=',', fill=TRUE, stringsAsFactors=FALSE)

all_dendro_2025 <- all_dendro_2025 %>% 
  clean_names()

# Create series list
series_list <- as.list(unique(all_dendro_2025$series))

# Create day-zero adjusted data
dendro_day_zero <- all_dendro_2025 |>
  group_by(series, date) |>
  arrange(series, date, .by_group = TRUE) |>
  mutate(
    day_zero_value = value - first(value),
    day_zero_change = day_zero_value - lag(day_zero_value)
  ) |>
  ungroup()

# Subset to growing season (March–August), add month and hour
gs_dendro_day <- dendro_day_zero |>
  mutate(
    month = month(date),
    hour = hour(ts)
  ) |>
  filter(month %in% 3:8)

ci <- function(x, conf = 0.95, na.rm = TRUE) {
  if (na.rm) x <- na.omit(x)
  n <- length(x)
  se <- sd(x) / sqrt(n)
  mean_x <- mean(x)
  error <- qt(conf / 2 + 0.5, df = n - 1) * se
  c(mean = mean_x, lower = mean_x - error, upper = mean_x + error)
}

gs_dendro_day_sum <- gs_dendro_day |>
  group_by(species, dendro, hour) |>
  summarize(
    species_hour_mean = mean(day_zero_change, na.rm = TRUE),
    species_hour_sd = sd(day_zero_change, na.rm = TRUE),
    species_hour_low_ci = ci(day_zero_change)[2],
    species_hour_high_ci = ci(day_zero_change)[3],
    .groups = "drop"
  )

# Replace NaNs with 0 (for nicer plots)
gs_dendro_day_sum$species_hour_mean[is.nan(gs_dendro_day_sum$species_hour_mean)] <- 0

# Plot: hourly jittered changes
ggplot(gs_dendro_day, aes(x = hour, y = day_zero_change, color = species)) +
  geom_jitter() +
  geom_hline(yintercept = 0, color = "black", size = 1.7) +
  geom_hline(yintercept = 0, color = "white", size = 1) +
  labs(y = "Hourly changes", title = "PJ all species × dendro type facet") +
  facet_wrap(dendro ~ species, scales = "free", ncol = 2) +
  theme_pubr()
ggsave("./output/plots/pj_daily_zero_hour_changes.png")

# Plot: mean ± CI of hourly changes
ggplot(gs_dendro_day_sum, aes(x = hour, y = species_hour_mean, color = species)) + 
  geom_point(aes(size = 2)) +
  geom_line(aes(group = species, linewidth = 1.5)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_errorbar(aes(ymin = species_hour_low_ci, ymax = species_hour_high_ci), width = 0.2, linewidth = 1) + 
  facet_wrap(~dendro, scales = "free") +
  theme_pubr()
ggsave("./output/plots/pj_daily_zero_means_free_scale.png")

# Save data to CSV
write.csv(gs_dendro_day, file = "./data/processed/pj_dendro_daily_zero_means_1.csv", row.names = FALSE)
write.csv(gs_dendro_day_sum, file = "./data/processed/pj_dendro_daily_zero_means_summary_1.csv", row.names = FALSE)
write.csv(dendro_day_zero, file = "./data/processed/pj_dendro_daily_zero_changes_1.csv", row.names = FALSE)



