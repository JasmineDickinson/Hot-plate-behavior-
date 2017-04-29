#### This 1) restructures the raster data into bins with a "yes" or "no" mark as to whether a behavior occured within that bin, 2) turns the raw bin tallies into a cumulative duration, and 3) calculates the area under the curve (AUC) for the cumulative duration 

rm(list=ls()) # Removes list objects
graphics.off() #removes plots open

# Set the working directory specific to your computer. ctrl+shift+h on a mac shortcuts to bring up the menu
# setwd("YOUR PATH")
setwd("~/Dropbox/Lab/R Projects/Clean behavior scripts Github/Raster plot and analysis")

library(dplyr)
library(reshape2)

ds_total <- read.csv(file="Behavior_raster_sample.csv", header = TRUE)

#### Sort data into 0.01 second bins (10 ms)

# Make skeleton dataframe

df <- data.frame(bins=seq(0.01,60,0.01)) # replace second number with the length of the session, in seconds
df$yes <- 0
df$id <- NA

#make a list of the unique mouseIDs points
newid1 <- unique(ds_total$mouseid) 

# This loop is slow. Print statements are useful to ensure it's working. Ignore the warnings at the ends
for (i in 1:length(newid1)){
  ##add ids to skeleton
  print(paste('Starting ID:', newid1[i]))
  print(paste('ID', i,"out of", length(newid1)))
  x <- df
  x$id <- newid1[i]
  behavior_temp <- subset(ds_total, mouseid == newid1[i])
  start_times_temp <- behavior_temp$start
  end_times_temp <- behavior_temp$end
  behaviors_vector_temp = behavior_temp$track
  for (j in 1:nrow(x)) {
    for(k in 1:length(start_times_temp)){
      if(x[j,]$bins>=start_times_temp[k] && x[j,]$bins<=end_times_temp[k])
      {
        x[j,]$yes=1
        x[j,]$behavior = behaviors_vector_temp[k]
      }
    }
  }
  
  if (i==1){
    short_bins = x}
  
  else {short_bins <-rbind(short_bins,x)}
}

# since the above process is slow for long sessions/ many animals, it's helpful to save the output dataframe so you only have to run it once.
write.csv(short_bins, file = "short_bins.csv")
rm(short_bins)


#### Sum bins and make cumulative sums with longer bins (ex: 500 ms per bin vs 10 ms per bin)

# # If you're reloading the short_bin csv, repeat the unique ID list step from above.
# short_bins <- read.csv(file = "short_bins.csv", row.names = 1)
# short_bins$id <- as.factor(short_bins$id)
# newid1 <- unique(short_bins$id)

#500 ms
for (i in 1:length(newid1)){
  print(paste('Starting ID:', newid1[i]))
  print(paste('ID', i,"out of", length(newid1)))
  tmp <- data.frame(subset(short_bins, id == newid1[i]))
  tmp_sum <- data.frame(colSums(matrix(tmp$yes, nrow=50))) #50 = 500 millisecond
  tmp_sum$id <- newid1[i]
  tmp_sum$cumulative_ms <- cumsum(tmp_sum[,1])
  tmp_sum$cumulative_sec <-tmp_sum$cumulative_ms / 100
  tmp_sum$bins <- c(1:length(tmp_sum$id))
  
  if (i==1){
    longer_bins = tmp_sum}
  
  else {longer_bins <- rbind(longer_bins,tmp_sum)}
}

colnames(longer_bins)[1] <- "yes" # this is the raw sum of the "yes" tallies from the 10ms bin dataframe.
longer_bins <- arrange(longer_bins, id)

# convert cumulative sum from long to wide
wide_500ms <- dcast(longer_bins, bins~id, value.var="cumulative_sec")


# AUC (area under the curve) for the 500ms cumulative duration. This is a quantification that your cumulative duration curves are statistically different or not
for (i in 1:length(newid1)){
  print(paste('Starting ID:', newid1[i]))
  print(paste('ID', i,"out of", length(newid1)))
  tmp <- data.frame(subset(longer_bins, id == newid1[i]))
  tmp_sum <- data.frame(sum(tmp$cumulative_sec)*0.5)
  tmp_sum$id <- newid1[i]
  
  if (i==1){
    AUC=tmp_sum}
  
  else {AUC <-rbind(AUC,tmp_sum)}
}

colnames(AUC)[1] <- "AUC"
AUC <- arrange(AUC, id)

write.xlsx(x = wide_500ms, file = "BehaviorBins_AUC.xlsx", sheetName = "500ms bins", row.names = FALSE)

write.xlsx(x = AUC, file = "BehaviorBins_AUC.xlsx", sheetName = "AUC", row.names = FALSE, append = TRUE)
