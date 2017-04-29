## Start with a clean workspace
rm(list=ls()) # Removes list objects
graphics.off() #removes plots open

## Set the working directory specific to your computer. ctrl+shift+h on a mac shortcuts to bring up the menu, and then you can copy and paste from the console
# setwd("YOUR PATH")

library(dplyr)
library(plyr)
library(xlsx)

ds_total <- read.csv(file="Behavior_raster_sample.csv", header = TRUE)

# Analysis for all behaviors lumped together. Gives the total duration of all behaviors, the latency until the first of any of the behaviors, and the number of instances of behavior bouts

all_behavior <- ddply(ds_total,~mouseid,summarise,duration=sum(duration),latency=min(start),num_bouts=length(mouseid), .drop = FALSE)

mouseid_drug <-  unique(subset(ds_total, treatment =="drug")$mouseid)
mouseid_saline <-  unique(subset(ds_total, treatment =="saline")$mouseid) ## not strictly necessary if you only have 2 treatment groups

# carries over the "treatment" value.
for (i in mouseid_drug){
  all_behavior$treatment <- ifelse(all_behavior$mouseid == i, "drug", "saline")
}

# Analysis for individual behaviors. Gives the total duration of each behavior, the latency until the first instance, and the number of bouts. 

single_behavior <- ddply(ds_total,~ mouseid + Behavior, summarise,duration=sum(duration), latency=min(start), num_bouts=length(mouseid), .drop = FALSE)

# carries over the "treatment" value.
for (i in mouseid_drug){
  single_behavior$treatment <- ifelse(single_behavior$mouseid == i, "drug", "saline")
}

# Ignore the warning message in the console. Replace any "Inf"s with the cutoff for your trial session. For this example, the maximum trial length is 60 seconds
single_behavior <- as.data.frame(sapply(single_behavior,gsub,pattern="Inf",replacement="60.0"))

## split your master single_behavior dataframe into separate dataframes for each behavior

# convert your numbers to numeric.
factorconvert <- function(f){as.numeric(levels(f))[f]}
single_behavior[,3:5] <- lapply(single_behavior[,3:5], factorconvert)

# make a list of the unique behaviors
behaviors <- unique(single_behavior$Behavior)

# make a list of unique treatments
treatments <- unique(single_behavior$treatment)

# The next step does 3 things. 1) Makes a unique data frame for each behavior that shows up in the Global Environment. 2) Makes a list of data frames that shows up in Values. 3) Makes a list of the names of those data frames. The latter 2 items make writing the content to an excel file easier. #1 makes it easy within R to double check that the output is what you intended.
list_of_names <- list()
df_list <- list()
count = 1
for (i in behaviors){
  for (j in treatments){
    x <- subset(single_behavior, Behavior == i & treatment == j)
    assign(paste(i,j, sep='_'), x)
    df_list[[count]] <- x
    count = count + 1
    list_of_names <- append(list_of_names,(paste(i,j, sep='_'))) 
  }
}

# Writes all of the dataframes of interest to an excel file. "all_behavior" written as the last sheet

for (i in 1:length(df_list)){
  
  if (i==1)
  {write.xlsx(x=df_list[i], file = "Behavior_Raster_Analysis.xlsx", sheetName = paste(list_of_names[i]), row.names = FALSE)}
  
  else 
  {write.xlsx(x=df_list[i], file = "Behavior_Raster_Analysis.xlsx", sheetName = paste(list_of_names[i]), row.names = FALSE, append = TRUE)}
  
}

write.xlsx(x=all_behavior, file = "Behavior_Raster_Analysis.xlsx", sheetName = "all_behavior", row.names = FALSE, append = TRUE)
