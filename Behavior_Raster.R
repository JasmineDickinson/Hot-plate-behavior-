#### This generates the raster plot of color coded behaviors
#### Credit to Andy Wong and Jon Kaslow for co-writing this script

## Start with a clean workspace
rm(list=ls()) # Removes list objects
graphics.off() #removes plots open

## Set the working directory specific to your computer. ctrl+shift+h on a mac shortcuts to bring up the menu, and then you can copy and paste from the console
# setwd("YOUR PATH")

library(ggplot2)
library(dplyr)
# library(grid)

# imports your file, assuming everything is on one sheet. Alternately, the "read.xlsx2" function works with the xlsx package. You can also save an excel sheet as a .csv
ds_total <- read.csv("Behavior_raster_sample.csv", header = T)

## imports your files if each mouse is separated on different sheets, a .csv in this case, and concatenates into one dataframe. This will load ALL files with the specified pattern that are in the folder, so make sure you only have files for the experiment of interest. It is easier to score all mice on one sheet initially to avoid having to use this import method.
# files <- list.files(path = ".",full.names=FALSE, pattern = ".csv")
# files
# length(files) #these two steps aren't necessary, but are here to verify that your files are defined and the correct number of files are found in the right folder.
# 
# for (i in 1:length(files)){
#   x <- read.table(files[i], header=TRUE)
#   #   assign(paste('ds',i, sep=""),x) #generates the separate ds's.  Not absolutely necessary
#   if (i == 1) 
#   {ds_total = x} 
#   else 
#   {ds_total = rbind(ds_total, x)}
# }
# rm(x)

ds_total_origin <- ds_total

# Reorganize your dataframe by treatment group, then mouseid. "Treatment" will use alphabetical order, so use the - sign to reverse order if necessary. I generally prefer to have the drug/ manipulation group at the top and the control group at the bottom. 
ds_total <- arrange(ds_total, treatment, mouseid)

tmp <- ds_total[,c(1:2)] #all rows and columns 1,2 (should be the mouseid's and the behaviors) 
tmp <- tmp[!duplicated(tmp),] #all the unique behaviors per animal

starts <- ds_total[,c(1:3)] # takes "mouseid", "Behavior" and the "start" times
ends <- ds_total[,c(1:2,4)] # takes "mouseid", "Behavior" and the "end" times

colnames(starts)[3] <- "time" # changes the 3rd column name of the "starts" dataframe to "time"
starts$value <- rep(1, length(starts$Behavior)) # adds a new column with a value of 1 for all rows
colnames(ends)[3] <- "time" # same as "starts" dataframe
ends$value <- rep(0, length(ends$Behavior)) # adds a new column with a value of 0 for all rows

ds_total <- rbind(starts, ends) # concatenates the "starts" and "ends" dfs. stacks "ends" after "starts"

tmp$time <- 0
tmp$value <- 0

ds_total <- rbind(ds_total, tmp)

# MAKE RASTER PLOT

p1 <- ggplot(ds_total)
p1 <- p1 + geom_rect(data=ds_total_origin, aes(xmin=start, xmax=end, ymax=1, ymin=0, group=Behavior, fill=Behavior), size=1.5, alpha=1)
p1 <- p1 + facet_wrap(~mouseid, ncol=1)
p1 <- p1 + labs(x="Time (seconds)")
p1 <- p1 + theme(axis.title.x = element_text(color="black", size=30, face="bold"))
p1 <- p1 + theme(axis.text.x = element_text(size = 25))
p1 <- p1 + theme(axis.line = element_line(colour = "black"), axis.line.y = element_line(colour = "white"))
p1 <- p1 + theme(axis.line.x = element_line(size=1))
p1 <- p1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p1 <- p1 + theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank())
p1 <- p1 + scale_fill_manual(values=c('grey42', 'midnightblue', 'steelblue'))
p1 <- p1 + theme(legend.title = element_text(colour="black", size=28, face="bold"))
p1 <- p1 + theme(legend.text = element_text(colour="black", size=20, face="bold"))
p1 <- p1 + theme(legend.key.height = unit(3, "line"))
p1 

png(file="mouse_event_chart.png", height=12, width=20, units="in", res=150) #tells it to "open" a png file. Change the size dimensions as you please.
print(p1) #need "print" to put it in the file
dev.off() #closes any writing to the file
