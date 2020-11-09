# extracting coordinates of bounding boxes for tagged images from Nepal 2019 - can be used for any list of tags made using VOTT where each tag is a unique JSON file.
# Peggy Bevan 09/11/2020
# original code here: https://github.com/biome-health-project/handling_image_tags/blob/master/JSON_extractor_2.Rmd

# Set working directory specific to your files
setwd("Z:/biome_health_project_files/country_files/nepal/tagging_photos")
library(dplyr) #bind_rows
library(here)
library(lubridate)
library(knitr)
library(stringr)
library(taxize)
library(profvis)
library(jsonlite)
library(tidyr)
library(stringr)
library(pbapply)


# List files in your directory:

# Changed pattern to *asset.json to make sure it only includes the right type of json.
# If you have any errors running code make sure that the only type of JSON file in your list is VOTT exports.



#1min2019.1 JSON folder
OneMin2019.1 <- list.files("./2019_01_min_threshold_1", recursive= TRUE, full.names = TRUE, pattern = "*asset.json")

#1min2019.2 JSON folder
OneMin2019.2 <- list.files("./2019_01_min_threshold_2", recursive= TRUE, full.names = TRUE, pattern = "*asset.json")

#30min JSON folder
thirtyMin2019 <- list.files("./2019_30_min_threshold", recursive= TRUE, full.names = TRUE, pattern = "*asset.json")

retagging <- list.files("./Retagging", recursive= TRUE, full.names = TRUE, pattern = "*asset.json")

# random tagging
random <- list.files('./2019_random', recursive = TRUE, full.names = TRUE, pattern = '*asset.json')


###JSON extraction function - this can be resued for each folder###
json_extract<-function(json_file){
  jsin<-jsonlite::fromJSON(json_file) 
  ImageID<-jsin$asset$name
  ImageWidth<-jsin$asset$size$width
  ImageHeight<-jsin$asset$size$height
  # TagID<-gsub("-asset.json", "",basename(json_file))
  
  json_loop_out<-NULL
  for (i in 1:nrow(jsin$regions)){  #length(jsin$regions)){
    
    CommonName<-as.character(jsin$regions$tags[[i]]) # (jsin$regions[[i]]$tags) sometimes works if this doesn't
    box_id<-jsin$regions$id[i]
    box_width <- jsin$regions$boundingBox$width[i]
    box_height<-jsin$regions$boundingBox$height[i]
    xmin<-min(jsin$regions$points[[i]]$x)
    ymin<-min(jsin$regions$points[[i]]$y)
    xmax<-max(jsin$regions$points[[i]]$x)
    ymax<-max(jsin$regions$points[[i]]$y)
    
    
    jlo_out<-data.frame(CommonName,box_id, box_width, box_height, xmin, ymin, xmax, ymax)
    json_loop_out<-rbind(json_loop_out, jlo_out)
  }
  
  #TagID<-paste(TagID, json_loop_out$box_id, sep = "_")
  
  json_out<-data.frame( ImageID = ImageID, ImageWidth = ImageWidth, ImageHeight = ImageHeight, json_loop_out, JSON_filepath = json_file)
  return(json_out)
}


#Apply the function to each of your list of files
json_OneMin2019.1 <- pblapply(OneMin2019.1,json_extract) # 11mins
json_OneMin2019.2 <- pblapply(OneMin2019.2,json_extract) # 6mins
json_thirtyMin2019 <- pblapply(thirtyMin2019,json_extract) # 7mins
json_retagging <- pblapply(retagging, json_extract) #12s
json_random <- pblapply(random, json_extract) # 14m

#Convert results into data frames
df_OneMin2019.1<- do.call("rbind", json_OneMin2019.1)
df_OneMin2019.2<- do.call("rbind", json_OneMin2019.2)
df_thirtyMin2019<-do.call("rbind", json_thirtyMin2019)
df_retagging <- do.call("rbind", json_retagging)
df_random <- do.call('rbind', json_random)

#Adding extra metadata about tagger, tagging method and file location to each row

df_OneMin2019.1 <- df_OneMin2019.1 %>%
  mutate(tag_group = 'OneMin2019.1',
         method = 'Systematic',
         tagger = ifelse(grepl('LT_output', JSON_filepath), 'LT', 'PB'),
         tag_status = 'original'
         )

df_OneMin2019.2 <- df_OneMin2019.2 %>%
  mutate(tag_group = 'OneMin2019.1',
         method = 'Systematic',
         tagger = ifelse(grepl('LToutput', JSON_filepath), 'LT', ifelse(grepl('MJoutput', JSON_filepath), 'MJ', 'PB')),
         tag_status = 'original')

df_thirtyMin2019 <- df_thirtyMin2019 %>%
  mutate(tag_group = 'ThirtyMin2019',
         method = 'Systematic',
         tagger = 'LT',
         tag_status = 'original')

df_retagging <- df_retagging %>%
  mutate(tag_group = 'LTretagging2019',
         method = 'Systematic',
         tagger = 'LT',
         tag_status = 'updated')

df_random <- df_random %>%
  mutate(tag_group = 'random2019',
         method = 'Random',
         tagger = 'LT',
         tag_status = 'original')



# Add all the lists together to make one database

json_mer <- bind_rows(df_OneMin2019.1, df_OneMin2019.2, df_thirtyMin2019, df_retagging, df_random)

head(json_mer)
summary(json_mer)


# Including extra columns:'mgmt_zone'; 'camera_loc'; 'year'; 'img_dir', 
json_mer <- json_mer %>%
  mutate(year = 2019,
         mgmt_zone = ifelse(grepl('NP', ImageID),
                            'NP',
                            ifelse(grepl('OBZ', ImageID), 
                                   'OBZ', 'BZ')
         )
  )

# Extract camera location from ImageID
for (i in 1:nrow(json_mer)) {
  if (json_mer$mgmt_zone[i]=='OBZ') {
    json_mer$camera_loc[i] <- substr(json_mer$ImageID[i], 6, 10)
  } else {
    json_mer$camera_loc[i] <- substr(json_mer$ImageID[i], 6, 9)  
  }
}

#add the image directory 
json_mer <- json_mer %>%
  mutate(img_dir = paste('Z:/biome_health_project_files/country_files/nepal/working_data', mgmt_zone, camera_loc, 'CT', year, ImageID, sep = '/'))


# Save
write.csv(json_mer, 'AllNepalTags_PB_091020.csv', row.names = F)



# #######
# #IGNORE - breaking the function down step by step for troubleshooting
# 
# jsin<-jsonlite::fromJSON(OneMin2019.1[3]) 
# jsin
# ImageID<-jsin$asset$name
# # TagID<-gsub("-asset.json", "",basename(json_file))
# 
# json_loop_out<-NULL
# for (i in 1:nrow(jsin$regions)){  # changed to nrow(jsin$regions)) instead of length{
#   
#   CommonName<-as.character(jsin$regions$tags[[i]]) # (jsin$regions[[i]]$tags) sometimes works if this doesn't
#   box_id<-jsin$regions$id[i]
#   bl_x<-min(jsin$regions$points[[i]]$x)
#   bl_y<-min(jsin$regions$points[[i]]$y)
#   tr_x<-max(jsin$regions$points[[i]]$x)
#   tr_y<-max(jsin$regions$points[[i]]$y)
#   
#   jlo_out<-data.frame(CommonName, box_id, bl_x, bl_y, tr_x, tr_y)
#   json_loop_out<-rbind(json_loop_out, jlo_out)
# }
# 
# json_out<-data.frame( ImageID = ImageID, json_loop_out)
# print(jsin)
# return(json_out)
########
#merge the 4 dataframes together
# json_mer <- bind_rows(json_df_out_bz,json_df_out_np,json_df_out_obz, json_df_out_mis)


