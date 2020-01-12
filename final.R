# set the working directory 
setwd('C:/Users/hanna/Desktop/Fall2019/gis/assessment/analysis')

# ---------------------------------------
# LIBRARIES
# ---------------------------------------

library(XML)
library(moveVis)
library(move)
library(lubridate)
library(dplyr)
library(plyr)
library(leaflet)
library(classInt)
library(tidyverse)
library(data.table)
library(raster)
library(ggplot2)
library(extrafont)
library(htmltools)
library(htmlwidgets)

# ---------------------------------------
# READ IN AND PROCESS THE TCX DATA
# ---------------------------------------

# get all files in the dataset
filenames <- list.files("data/", "*.tcx", full.names=TRUE)

# input filename and outputs a movestack object 
process <- function(data){
  
  # read in the tcx data 
  doc <- xmlParse(data)
  
  # convert xml format to dataframe 
  df <- xmlToDataFrame(nodes = getNodeSet(doc, "//ns:Trackpoint", "ns"))
  
  # convert from factors to characters
  i <- sapply(df, is.factor) # find columns that are factors 
  df[i] <- lapply(df[i], as.character) # convert column to character
  
  # convert heart rate from char to number 
  df$HeartRateBpm <- sapply(df$HeartRateBpm, as.numeric)
  
  # separate the X and Y columns 
  for (i in 1:length(df$Position)){
    a <- unlist(strsplit(df$Position[i], '-'))
    df$Y_coord[i] <- (a[1])
    df$X_coord[i] <- paste0('-',a[2])
  }
  
  # convert to numeric values 
  df$Y_coord <- sapply(df$Y_coord, as.numeric)
  df$X_coord <- sapply(df$X_coord, as.numeric)
  
  # convert time format 
  df$TimeConverted <- as.POSIXct(df$Time, tz="UTC", format="%Y-%m-%dT%H:%M:%S")
  
  # remove duplicate timestamps 
  new <- df[!duplicated(df$TimeConverted), ]
  
  # convert df to movement type 
  movement_format <- df2move(new, 
                             proj = '+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0', 
                             x = 'X_coord', 
                             y = 'Y_coord', 
                             time = 'TimeConverted', 
                             data = new) 
  
  # return the move stack object 
  return(movement_format)
  
}

# process all of the data with the above function
list <- lapply(filenames, process, sampling=5)

# combine all of the data into one 
# NOTE: brute force method - needs to be adapted if different number of files 
test3 <- list[[1]] + list[[2]] + list[[3]] + list[[4]] + list[[5]] + list[[6]] + list[[7]] + list[[8]] + list[[9]] + list[[10]] + list[[11]]+ list[[12]]+list[[13]]+list[[14]]+list[[15]]

# write to csv 
points_out <- as.data.frame(test3$TimeConverted)
points_out['X_coord'] <- test3$X_coord
points_out['Y_coord'] <- test3$Y_coord
points_out['hr']<- test3$HeartRateBpm
points_out['time']<- test3$TimeConverted

write.csv(points_out, 'all_hr.csv')

# ---------------------------------------
# READ IN AND PROCESS THE CSV DATA
# ---------------------------------------

# read in csv 
notes <- read.csv('data/voice_notes.csv')

# convert time format to POSIXct 
notes$TimeConverted <- as.POSIXct(notes$Date_time, tz='UTC')

# need to add coordinates by joining with heart rate data 
# help from: https://stackoverflow.com/questions/39282749/r-how-to-join-two-data-frames-by-nearest-time-date

# convert to data tables 
table_coords <- as.data.table(points_out)
table_notes <- as.data.table(notes)

# set join key 
setkey(table_coords, time)
setkey(table_notes, TimeConverted)

# combine with nearest time stamp
combined <- table_coords[table_notes, roll = "nearest" ]
combined 

# ---------------------------------------
# BASIC DESCRIPTIVE VIZ
# ---------------------------------------

# to change the font of the plots 
font_import()
loadfonts(device = "win")

# create histogram of heart rate values 
ggplot(data = points_out)+
  geom_histogram(aes(x=points_out$hr), color='white', fill='grey')+
  theme_minimal()+
  xlab('Heart Rate')+
  ylab('Frequency')+
  theme(text = element_text(family='Times New Roman'))

# get mean and sd 
print(mean(points_out$hr))
print(sd(points_out$hr))

# hexbin plot of x,y distribution of points 
ggplot(data=points_out)+
  geom_hex(aes(x=points_out$X_coord, y=points_out$Y_coord), bins=15)+
  theme_minimal()+
  xlab('Longitude')+
  ylab('Latitude')+
  #guides(fill=guide_legend(title='Count'))+
  theme(text = element_text(family='Times New Roman'))
  
# bar chart of personal comment themes 
ggplot(data = notes)+
  geom_bar(aes(x=notes$Theme, fill=notes$Sentiment), color='white')+
  coord_flip()+
  theme_minimal()+
  xlab('Theme')+
  ylab('Frequency')+
  guides(fill=guide_legend(title='Sentiment'))+
  theme(text = element_text(family='Times New Roman'))

# ---------------------------------------
# READ IN AND PROCESS THE RASTER DATA
# ---------------------------------------

# read in the raster 
r <- raster("outputs/smoothed.tif")

# ---------------------------------------
# CREATE THE LEAFLET MAP 
# ---------------------------------------

# colour palette for the raster 
pal_rast <- colorNumeric(c("#FBEC3C", "#F76C52", "#4B056F"), values(r),
                         na.color = "transparent")

# colour palette for the markers 
pal_markers <- colorFactor(c("#8E09DC", "#FBF3A9"), combined$Sentiment)

leaflet() %>% 
  # add basemap tiles
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(opacity = 0.9)) %>%
  # add the anxiety raster
  addRasterImage(r, 
                 colors = pal_rast, 
                 opacity = 0.9, 
                 group = 'Heart Rate') %>%
  
  # add points for the personal comments 
  addCircleMarkers(data=combined,
                   radius=4,
                   lng = ~combined$X_coord, 
                   lat = ~combined$Y_coord, 
                   group = 'Personal Comments',
                   stroke = TRUE, 
                   fillOpacity = 1, 
                   popup=paste0('<b>Direction of impact on anxiety: </b>', combined$Sentiment, '<br><b>Theme:</b> ', combined$Theme, '<br><b>Note:</b> <i>', combined$ï..Note, '</i>'), 
                   color = ~pal_markers(combined$Sentiment)) %>%
  
  # Layers control
  addLayersControl(
    overlayGroups = c("Personal Comments", "Heart Rate"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  
  # add legend for the raster values 
  addLegend(pal = pal_rast, 
            values = values(r),
            title = "Heart Rate", 
            opacity = 1, 
            group = 'Heart Rate') %>%
  
  # add legend for the personal comments 
  addLegend(pal = pal_markers, 
            values = combined$Sentiment,
            title = HTML("Personal Comments<br>(Impact on anxiety)"), 
            opacity = 1,
            group = 'Personal Comments') %>%
  
  # add scale bar to the map 
  addScaleBar(position = "bottomright") %>%
  
  # add title to the map 
  addControl(HTML('<b>Feelings of Anxiety in London</b><br>Dec 4-14, 2019'), position = "bottomleft")

  

             