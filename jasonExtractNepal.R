# extracting coordinates of bounding boxes for tagged images from Nepal 2019 - can be used for any list of tags made using VOTT where the output from each image is a unique JSON file.
# Peggy Bevan 09/11/2020
# original code here: https://github.com/biome-health-project/handling_image_tags/blob/master/JSON_extractor_2.Rmd




# Packages ----------------------------------------------------------------

library(dplyr) #bind_rows
#library(here)
library(lubridate)
library(knitr)
library(stringr)
#library(taxize)
#library(profvis)
library(jsonlite) #fromJson()
library(tidyr)
library(stringr)
library(pbapply) #pbapply()
library(vroom) #vroom()



# List files --------------------------------------------------------------

# Set working directory specific to your files
setwd("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/nepal/tagging_photos")
# setwd('Z:/biome_health_project_files/country_files/nepal/tagging_photos")

# Changed pattern to '*asset.json' to make sure it only includes the right type of json.
# If you have any errors running code make sure that the only type of JSON file in your list is VOTT exports.

#1min2019.1 JSON folder
OneMin2019.1 <- list.files("./2019_01_min_threshold_1", recursive= TRUE, full.names = TRUE, pattern = "*asset.json")

#1min2019.2 JSON folder # 6 mins
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
json_OneMin2019.2 <- pblapply(OneMin2019.2,json_extract) # 14mins 50
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
  mutate(tag_group = 'OneMin2019.2',
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

# in the thirty minute tag group,empty images were not labelled so there is no json output for them. here I add them to df_thirtymin2019. 

# list of images to be included in 30min tag group
thirtymins <- read.csv('~/Peggy/CTs/30min_hteshold.csv')
nrow(thirtymins) #12421
# the total number of images in vott output:
length(thirtyMin2019) # 9388
# so the missing files must be those that were empty - 
# 12421 - 9388 # = 3033

# to include this in the main dataframe, i need to isolate those 3033 images that aren't in df_thirtymin2019. 
# first i need to get the file names for thirtymins 
thirtymins$ImageID <- basename(as.character(thirtymins$new_file_structure))
empty30 <- anti_join(thirtymins, df_thirtyMin2019, by = "ImageID")
nrow(empty30) #3033. exactly what we want to see!

# now make empty 30 the same column structure as exif_mer
names(df_thirtyMin2019)
names(empty30)

empty30_df <- data.frame('ImageID' = empty30$ImageID, 'ImageWidth' = 2688, 'ImageHeight' = 1512,"CommonName" = 'Empty', "box_id" = NA, "box_width" = NA, "box_height" = NA, "xmin" = NA, "ymin" = NA, "xmax" = NA, "ymax" = NA, "JSON_filepath" = NA, "tag_group" = 'ThirtyMin2019', "method" = 'Systematic', "tagger" = 'LT',"tag_status" = 'original') 

df_thirtyMin2019 <- rbind(df_thirtyMin2019, empty30_df)

# Add all the lists together to make one database, without retagging

json_mer <- bind_rows(df_OneMin2019.1, df_OneMin2019.2, df_thirtyMin2019, df_random)

head(json_mer)
summary(json_mer)

##remove duplicates from retagging
#every row in retagging will have an imageID that matches with a duplicate image 
# so before merging retagging with the rest of the set, I should keep them seperate and match duplicates.

#remove tags which are 'yes' and 'no'
unwanted <- paste(c("yes", "no", "missing"), collapse = '|') 

retags<- df_retagging %>% 
  filter(!grepl(unwanted, CommonName))
head(retags); nrow(retags) # 2066

#remove tags from wrong folders
nrow(retags)
retags<- retags %>% 
  filter(!grepl('_30min&1min', JSON_filepath, fixed = TRUE))
nrow(retags)
retags<- retags %>% 
  filter(!grepl('_NEW1min/Unidentifiable', JSON_filepath, fixed = TRUE))
nrow(retags)

#make sure all the labels are correct
table(retags$CommonName)
# human Human
# Nilagi Nilgai
retags$CommonName <- gsub("human", "Human", retags$CommonName)
retags$CommonName <- gsub("Nilagi", "Nilgai", retags$CommonName)

# ones to remove
nrow(json_mer);nrow(retags)

# this is removing all the rows in json_mer that are found in retags.
removed2 <- anti_join(json_mer, retags, by = "ImageID")
nrow(removed2) 
# this removes 1213 rows. 
# there are 1246 retags rows in total.

# for some imageIDs in json_mer, there are multiple tags in retags.
# this is why the numbers don't add up. 
# so this method is going on an assumption that when a tag was corrected, the retagging file 
# contains ALL the original tags, whether they are updated or not. 

# in total, the retagging process will add 188 new tags to the total.
# a way to check if its the same is look at length(unique(ImageID))


names(retags)
names(removed2)
# These now have the same dimensions, so retags can simply be added on to json_mer(removed2)

updat_mer <- rbind(removed2, retags)

nrow(json_mer);nrow(removed2);nrow(updat_mer)
length(unique(json_mer$ImageID));length(unique(updat_mer$ImageID))
# the same number of images before and after adding retags 

# Including extra columns:'mgmt_zone'; 'camera_loc'; 'year'; 'img_dir', 
updat_mer <- updat_mer %>%
  mutate(year = 2019,
         mgmt_zone = ifelse(grepl('NP', ImageID),
                            'NP',
                            ifelse(grepl('OBZ', ImageID), 
                                   'OBZ', 'BZ')
         )
  )

# Extract camera location from ImageID
for (i in 1:nrow(updat_mer)) {
  if (updat_mer$mgmt_zone[i]=='OBZ') {
    updat_mer$camera_loc[i] <- substr(updat_mer$ImageID[i], 6, 10)
  } else {
    updat_mer$camera_loc[i] <- substr(updat_mer$ImageID[i], 6, 9)  
  }
}

#add the image directory 
updat_mer <- updat_mer %>%
  mutate(img_dir = paste('Z:/biome_health_project_files/country_files/nepal/working_data', mgmt_zone, camera_loc, 'CT', year, ImageID, sep = '/'))

#check for mistakes
table(updat_mer$CommonName)
#2 spellings of domestic chicken
# blank, empty
# Undidentifiable       Unidentifiable         Unidentified
updat_mer$CommonName <- gsub("Domestic chicken", "Domestic Chicken", updat_mer$CommonName)
updat_mer$CommonName <- gsub("Undidentifiable", "Unidentifiable", updat_mer$CommonName)
updat_mer$CommonName <- gsub("Unidentified", "Unidentifiable", updat_mer$CommonName)
updat_mer$CommonName <- gsub("Blank", "Empty", updat_mer$CommonName)

table(updat_mer$CommonName)


#add image metadata from exif
#which is the correct exif file?
exif <- vroom::vroom("../exif/EXIF_2019_Nepal_20200714.csv")

head(exif)
names(exif)[3]<-'ImageID'

merge1inner <- dplyr::inner_join(updat_mer, exif, by = "ImageID")
nrow(updat_mer); nrow(merge1inner)
# this removes one row in updat_mer, which for some reason there is no exif data for:
# 2019_BZ31_001212.JPG this is a human picture of someone while setting up camera so won't 
# affect analyses, so leaving for now.

# remove columns that are repeating information
exif_mer <- merge1inner[,-c(21:24)]

View(exif_mer)
# Save
write.csv(exif_mer, 'AllNepalTags_PB_201208.csv', row.names = F)



# checking the dataset matches with original master tag file.

liamtags<-vroom::vroom('../processed_data/2019_master_tags&meta.csv')

missing <- dplyr::anti_join(exif_mer, liamtags, by = c('ImageID' = 'image'))
liammissing<-dplyr::anti_join(liamtags, exif_mer, by = c('image' = 'ImageID'))
# there are no images in liamtags that aren't in exif_mer. 


nrow(subset(exif_mer, tag_group != 'random2019')) - nrow(liamtags)
# there are 24 extra rows in exif_mer

length(unique(exif_mer$ImageID[exif_mer$tag_group != 'random2019']))
length(unique(liamtags$image))
# same number of images in each dataset

sys_tags <- subset(exif_mer, method =='Systematic')
missing <- dplyr::anti_join(sys_tags, liamtags, by = c('ImageID' = 'image'))
nrow(missing) #0
