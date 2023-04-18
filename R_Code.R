

library(readr)
shelter <- read.csv("D:/Daily shelter.csv")

# descriptive analysis
str(shelter)
summary(shelter)

unique(shelter$SECTOR)
unique(shelter$PROGRAM_MODEL)
unique(shelter$OVERNIGHT_SERVICE_TYPE)
unique(shelter$PROGRAM_MODEL)
unique(shelter$PROGRAM_AREA)
unique(shelter$ CAPACITY_TYPE)
unique(shelter$LOCATION_CITY)
unique(shelter$OVERNIGHT_SERVICE_TYPE)
unique(shelter$PROGRAM_AREA)
# ********************************************************** 
# omit program model with null value

shelter <-  shelter[ shelter$PROGRAM_MODEL != "" , , drop = FALSE]

# omit LOCATION_CITY with null value

shelter <-  shelter[ shelter$LOCATION_CITY != "" , , drop = FALSE]

# ********************************************************** 
# Search for duplication

duplicated(shelter)

# ********************************************************** 
# find missing values

is.na(shelter)

# number of rows with missing values
nrow(shelter) - sum(complete.cases(shelter))

library(visdat)
vis_miss(shelter , warn_large_data = FALSE)

#Identify columns with NAs 
unlist(lapply(shelter, function(x) any(is.na(x)))) 

# find the column name of missing value

# install.packages("purrr")

library(purrr)

map_int(shelter , function(.x) sum(is.na(.x)))

# ********************************************************** 

#Replace NAs with mean from all 13 columns 
which(is.na(shelter))
shelter$CAPACITY_ACTUAL_BED[which(is.na(shelter$CAPACITY_ACTUAL_BED))] <- round(mean(shelter$CAPACITY_ACTUAL_BED, na.rm=TRUE))
shelter$UNOCCUPIED_BEDS[which(is.na(shelter$UNOCCUPIED_BEDS))] <- round(mean(shelter$UNOCCUPIED_BEDS, na.rm=TRUE))
shelter$CAPACITY_FUNDING_ROOM[which(is.na(shelter$CAPACITY_FUNDING_ROOM))] <- round(mean(shelter$CAPACITY_FUNDING_ROOM, na.rm=TRUE))
shelter$UNAVAILABLE_ROOMS[which(is.na(shelter$UNAVAILABLE_ROOMS))] <- round(mean(shelter$UNAVAILABLE_ROOMS, na.rm=TRUE))
shelter$OCCUPIED_BEDS[which(is.na(shelter$OCCUPIED_BEDS))] <- round(mean(shelter$OCCUPIED_BEDS, na.rm=TRUE))
shelter$CAPACITY_ACTUAL_ROOM[which(is.na(shelter$CAPACITY_ACTUAL_ROOM))] <- round(mean(shelter$CAPACITY_ACTUAL_ROOM, na.rm=TRUE))
shelter$UNOCCUPIED_ROOMS[which(is.na(shelter$UNOCCUPIED_ROOMS))] <- round(mean(shelter$UNOCCUPIED_ROOMS, na.rm=TRUE))
shelter$OCCUPANCY_RATE_ROOMS[which(is.na(shelter$OCCUPANCY_RATE_ROOMS))] <- round(mean(shelter$OCCUPANCY_RATE_ROOMS, na.rm=TRUE))
shelter$CAPACITY_FUNDING_BED[which(is.na(shelter$CAPACITY_FUNDING_BED))] <- round(mean(shelter$CAPACITY_FUNDING_BED, na.rm=TRUE))
shelter$UNAVAILABLE_BEDS[which(is.na(shelter$UNAVAILABLE_BEDS))] <- round(mean(shelter$UNAVAILABLE_BEDS, na.rm=TRUE))
shelter$OCCUPIED_ROOMS[which(is.na(shelter$OCCUPIED_ROOMS))] <- round(mean(shelter$OCCUPIED_ROOMS, na.rm=TRUE))
shelter$OCCUPANCY_RATE_BEDS[which(is.na(shelter$OCCUPANCY_RATE_BEDS))] <- round( mean(shelter$OCCUPANCY_RATE_BEDS, na.rm=TRUE))
shelter$LOCATION_ID[which(is.na(shelter$LOCATION_ID))] <-  round(mean(shelter$LOCATION_ID, na.rm=TRUE))

# **********************************************************
# EDA

# **********************************************************
library(tidyverse)
# Type of users
shelter_sector <- shelter %>% 
  group_by(SECTOR) %>% 
  summarise(name_rows = n())

shelter_sector$SECTOR <- factor(shelter_sector$SECTOR ,                                   
                                levels = shelter_sector$SECTOR[order(shelter_sector$name_rows, decreasing = TRUE)])

ggplot(shelter_sector , aes( x = SECTOR , y = name_rows,  fill = SECTOR))+ geom_bar(stat = "identity")+
  labs(subtitle =  " Type of Users" ,
       y = "Number",
       x = "Users",
       title = "",
       caption = "Source: Toronto.ca")


# **********************************************************
# Type of Program
shelter_program <- shelter %>% 
  group_by(PROGRAM_MODEL) %>% 
  summarise( name_rows = n())

shelter_program$PROGRAM_MODEL <- factor(shelter_program$PROGRAM_MODEL ,
                                        levels = shelter_program$PROGRAM_MODEL[order(shelter_program$name_rows , decreasing = TRUE)])

ggplot(shelter_program , aes( x = PROGRAM_MODEL , y = name_rows , fill = PROGRAM_MODEL ))+ geom_bar( stat = "identity")+
  labs(subtitle =  " Type of Programs" ,
       y = "Number",
       x = "Program",
       title = "",
       caption = "Source: Toronto.ca")

# **********************************************************


# **********************************************************
# Type of Overnight Service
shelter_service <- shelter %>% 
  group_by(OVERNIGHT_SERVICE_TYPE) %>% 
  summarise( name_rows = n())

shelter_service$OVERNIGHT_SERVICE_TYPE <- factor(shelter_service$OVERNIGHT_SERVICE_TYPE ,
                                                 levels = shelter_service$OVERNIGHT_SERVICE_TYPE[order(shelter_service$name_rows , decreasing = TRUE)])

ggplot(shelter_service , aes( x = OVERNIGHT_SERVICE_TYPE , y = name_rows , fill = OVERNIGHT_SERVICE_TYPE ))+ geom_bar( stat = "identity")+
  labs(subtitle =  " Type of Service" ,
       y = "Number",
       x = "Overnight service type",
       title = "",
       caption = "Source: Toronto.ca")

# **********************************************************
# Type of program area
shelter_area <- shelter %>% 
  group_by(PROGRAM_AREA) %>% 
  summarise( name_rows = n())

shelter_area$PROGRAM_AREA <- factor(shelter_area$PROGRAM_AREA  ,
                                    levels = shelter_area$PROGRAM_AREA [order(shelter_area$name_rows , decreasing = TRUE)])

ggplot(shelter_area , aes( x = PROGRAM_AREA , y = name_rows , fill = PROGRAM_AREA ))+ geom_bar( stat = "identity")+
  labs(subtitle =  " Type of Programs Area" ,
       y = "Number",
       x = "Program Area Types",
       title = "",
       caption = "Source: Toronto.ca")

# **********************************************************

# Shelteres' cities
shelter_Location <- shelter %>% 
  group_by(LOCATION_CITY) %>% 
  summarise( name_rows = n())

shelter_Location$LOCATION_CITY <- factor(shelter_Location$LOCATION_CITY  ,
                                         levels = shelter_Location$LOCATION_CITY[order(shelter_Location$name_rows , decreasing = TRUE)])

ggplot(shelter_Location , aes( x = LOCATION_CITY , y = name_rows , fill = LOCATION_CITY ))+ geom_bar( stat = "identity")+
  labs(subtitle =  " Shelteres' Cities" ,
       y = "Number",
       x = "Cites",
       title = "",
       caption = "Source: Toronto.ca") 

# **********************************************************
# Shelters' Capacity
shelter_Capacity <- shelter %>% 
  group_by(CAPACITY_TYPE) %>% 
  summarise( name_rows = n())

shelter_Capacity$CAPACITY_TYPE <- factor(shelter_Capacity$CAPACITY_TYPE ,
                                         levels = shelter_Capacity$CAPACITY_TYPE[order(shelter_Capacity$name_rows , decreasing = TRUE)])

ggplot(shelter_Capacity , aes( x = CAPACITY_TYPE , y = name_rows , fill = CAPACITY_TYPE ))+ geom_bar( stat = "identity")+
  labs(subtitle =  " Shelters's Type of Capacity" ,
       y = "Number",
       x = "Type of Capacity",
       title = "",
       caption = "Source: Toronto.ca")

# ********************************************************** 

# Shelters' PROGRAM_AREA
shelter_area <- shelter %>% 
  group_by(PROGRAM_AREA) %>% 
  summarise( name_rows = n())

shelter_area$PROGRAM_AREA <- factor(shelter_area$PROGRAM_AREA ,
                                    levels = shelter_area$PROGRAM_AREA[order(shelter_area$name_rows , decreasing = TRUE)])

ggplot(shelter_area , aes( x = PROGRAM_AREA , y = name_rows , fill = PROGRAM_AREA ))+ geom_bar( stat = "identity")+
  labs(subtitle =  " Shelters's PROGRAM AREA" ,
       y = "Number",
       x = "Type of PROGRAM_AREA",
       title = "",
       caption = "Source: Toronto.ca")

# ********************************************************** 

# # Shelters' PROGRAM_AREA by sector
#   shelter_area_sector <- shelter %>% 
#   group_by(PROGRAM_AREA,SECTOR) %>% 
#   summarise( name_rows = n())
# 
#   # shelter_area_sector$PROGRAM_AREA <- factor(shelter_area_sector$PROGRAM_AREA ,
#                                     # levels = shelter_area_sector$PROGRAM_AREA[order(shelter_area_sector$name_rows, decreasing = TRUE)])
#   
# 
# ggplot(shelter_area_sector , aes( x = SECTOR , y = name_rows , fill = PROGRAM_AREA ))+ geom_bar( stat = "identity")+
#   labs(subtitle =  " Shelters's PROGRAM AREA" ,
#        y = "Number",
#        x = "Type of PROGRAM_AREA",
#        title = "",
#        caption = "Source: Toronto.ca")

# ********************************************************** 

# using shelters by type in 2022

Shelter_Service_count <- shelter %>% 
  
  group_by(format(as.Date(OCCUPANCY_DATE),"%Y-%m"),PROGRAM_MODEL)%>%
  summarize(sum(SERVICE_USER_COUNT))

Shelter_Service_count <-  rename(Shelter_Service_count ,OCCUPANCY_DATE = `format(as.Date(OCCUPANCY_DATE), "%Y-%m")`)
Shelter_Service_count <-  rename(Shelter_Service_count ,Service_user_count = `sum(SERVICE_USER_COUNT)`)

Shelter_Service_count <- subset(Shelter_Service_count , OCCUPANCY_DATE != "2022-10" )

ggplot(Shelter_Service_count, aes(x = OCCUPANCY_DATE, y= Service_user_count, group=PROGRAM_MODEL , color = PROGRAM_MODEL)) +
  geom_line(size=1.5) +
  labs(subtitle="", 
       y="Number of Users", 
       x="Years, Month", 
       title="The number of Users by Model of service in 2022", 
       caption = "Source: Toronto.ca")


# **********************************************************

# using shelters by model in 2022

Shelter_Service_count_type <- shelter %>% 
  
  group_by(format(as.Date(OCCUPANCY_DATE),"%Y-%m"),OVERNIGHT_SERVICE_TYPE)%>%
  summarize(sum(SERVICE_USER_COUNT))

Shelter_Service_count_type <-  rename(Shelter_Service_count_type ,OCCUPANCY_DATE = `format(as.Date(OCCUPANCY_DATE), "%Y-%m")`)
Shelter_Service_count_type <-  rename(Shelter_Service_count_type ,Service_user_count = `sum(SERVICE_USER_COUNT)`)

Shelter_Service_count_type <- subset(Shelter_Service_count_type , OCCUPANCY_DATE != "2022-10" )

ggplot(Shelter_Service_count_type, aes(x = OCCUPANCY_DATE, y= Service_user_count, group=OVERNIGHT_SERVICE_TYPE , color = OVERNIGHT_SERVICE_TYPE)) +
  geom_line(size=1.5) +
  labs(subtitle="", 
       y="Number of Users", 
       x="Years, Month", 
       title="The number of Shelters's Users by Model of service in 2022", 
       caption = "Source: Toronto.ca")


# **********************************************************


# using shelters by type of people in 2022

Shelter_Service_count_sector <- shelter %>% 
  
  group_by(format(as.Date(OCCUPANCY_DATE),"%Y-%m"),SECTOR)%>%
  summarize(sum(SERVICE_USER_COUNT))

Shelter_Service_count_sector <-  rename(Shelter_Service_count_sector ,OCCUPANCY_DATE = `format(as.Date(OCCUPANCY_DATE), "%Y-%m")`)
Shelter_Service_count_sector <-  rename(Shelter_Service_count_sector ,Service_user_count = `sum(SERVICE_USER_COUNT)`)

Shelter_Service_count_sector <- subset(Shelter_Service_count_sector , OCCUPANCY_DATE != "2022-10" )

ggplot(Shelter_Service_count_sector, aes(x = OCCUPANCY_DATE, y= Service_user_count, group=SECTOR , color = SECTOR)) +
  geom_line(size=1.5) +
  labs(subtitle="", 
       y="Number of Users", 
       x="Years, Month", 
       title="The number of Shelters's Users by type of people in 2022", 
       caption = "Source: Toronto.ca")

# **********************************************************

# using shelters by type of area in 2022

Shelter_Service_count_area <- shelter %>% 
  
  group_by(format(as.Date(OCCUPANCY_DATE),"%Y-%m"),PROGRAM_AREA)%>%
  summarize(sum(SERVICE_USER_COUNT))

Shelter_Service_count_area <-  rename(Shelter_Service_count_area ,OCCUPANCY_DATE = `format(as.Date(OCCUPANCY_DATE), "%Y-%m")`)
Shelter_Service_count_area <-  rename(Shelter_Service_count_area ,Service_user_count_area = `sum(SERVICE_USER_COUNT)`)

Shelter_Service_count_area <- subset(Shelter_Service_count_area , OCCUPANCY_DATE != "2022-10" )

ggplot(Shelter_Service_count_area, aes(x = OCCUPANCY_DATE, y= Service_user_count_area, group=PROGRAM_AREA , color = PROGRAM_AREA)) +
  geom_line(size=1.5) +
  labs(subtitle="", 
       y="Number of Users", 
       x="Years, Month", 
       title="The number of Shelters's Users by type of program area", 
       caption = "Source: Toronto.ca")



# **********************************************************


# The number of UNOCCUPIED_BEDS each month in 2022

unoccupied_bed <- shelter %>% 
  
  group_by(format(as.Date(OCCUPANCY_DATE),"%Y-%m"))%>%
  summarize(sum(UNOCCUPIED_BEDS))

unoccupied_bed <-  rename(unoccupied_bed ,OCCUPANCY_DATE = `format(as.Date(OCCUPANCY_DATE), "%Y-%m")`)
unoccupied_bed <-  rename(unoccupied_bed ,unoccupied_Count = `sum(UNOCCUPIED_BEDS)`)

unoccupied_bed <- subset(unoccupied_bed , OCCUPANCY_DATE != "2022-10" )

ggplot(unoccupied_bed, aes(x = OCCUPANCY_DATE, y= unoccupied_Count, group=1 )) +
  geom_line(size=1.5 , color = "#DC143C") +
  labs(subtitle="", 
       y="Number of UNOCCUPIED_BEDS", 
       x="Years, Month", 
       title="Monthly unoccupied beds in 2022", 
       caption = "Source: Toronto.ca")


# **********************************************************


# The number of UNOCCUPIED_Rooms each month in 2022

unoccupied_room <- shelter %>% 
  
  group_by(format(as.Date(OCCUPANCY_DATE),"%Y-%m"))%>%
  summarize(sum(UNOCCUPIED_ROOMS))

unoccupied_room <-  rename(unoccupied_room ,OCCUPANCY_DATE = `format(as.Date(OCCUPANCY_DATE), "%Y-%m")`)
unoccupied_room <-  rename(unoccupied_room ,unoccupied_room_Count = `sum(UNOCCUPIED_ROOMS)`)

unoccupied_room <- subset(unoccupied_room , OCCUPANCY_DATE != "2022-10" )

ggplot(unoccupied_room, aes(x = OCCUPANCY_DATE, y= unoccupied_room_Count, group=1 )) +
  geom_line(size=1.5 , color = "#DC143C") +
  labs(subtitle="", 
       y="Number of UNOCCUPIED_Rooms", 
       x="Years, Month", 
       title="Monthly unoccupied Rooms in 2022", 
       caption = "Source: Toronto.ca")


# **********************************************************
# END EDA

# **********************************************************

# newshelter <- data.frame(shelter[c(13:14,21:30)], row.names = shelter$PROGRAM_ID)


newshelter <- shelter[1:5000,c(1,11, 15:18,23:24, 28:29)]

newshelter <- as.data.frame(newshelter)
str(newshelter)

newshelter$SECTOR <- as.factor(newshelter$SECTOR)
newshelter$PROGRAM_MODEL <- as.factor(newshelter$PROGRAM_MODEL)
newshelter$OVERNIGHT_SERVICE_TYPE <- as.factor(newshelter$OVERNIGHT_SERVICE_TYPE)
newshelter$PROGRAM_AREA <- as.factor(newshelter$PROGRAM_AREA)
newshelter$LOCATION_CITY <- as.factor(newshelter$LOCATION_CITY)

# install.packages("cluster")
library(cluster )

# Calculating Distance using Gower distance
gower_dist <- daisy(newshelter,metric = "gower", type = list(logratio = 3))


# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
# Note that despite logratio being called, 
# the type remains coded as "I"

summary(gower_dist)

# ********************************************************** 

#  print out the most similar and dissimilar pair in the data
gower_mat <- as.matrix(gower_dist)

# ********************************************************** 

# Output most similar pair
newshelter[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Output most dissimilar pair
newshelter[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]


# ********************************************************** 
# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:8){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width" , main =  "Optimal Number of clusters")
lines(1:8, sil_width , col = "darkorchid" , lwd = 3)

# ********************************************************** 


# Perform PAM clustering Algorithm

# running the algorithm and selecting three clusters

pam_fit <- pam(gower_dist, diss = TRUE, k = 2)

library(dplyr)
pam_results <- newshelter %>%
  dplyr::select(X_id,LOCATION_CITY,SECTOR,PROGRAM_MODEL,OVERNIGHT_SERVICE_TYPE, PROGRAM_AREA,
                OCCUPIED_BEDS,UNOCCUPIED_BEDS,OCCUPIED_ROOMS,UNOCCUPIED_ROOMS) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))


pam_results$the_summary

# medoids serve as exemplars of each cluster

newshelter[pam_fit$medoids, ]


# ********************************************************** 
# Visualization
# install.packages("CRAN")

# install.packages("Rtsne")
library(Rtsne)
library(ggplot2)

tsne_obj <- Rtsne(gower_dist, is_distance = FALSE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = newshelter$X_id)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))+ 
  labs(subtitle="Cluster Plot")

tsne_data %>% ggplot(aes(X, Y, color=cluster))+
  geom_point(alpha = 0.3, pch=21)+
  stat_ellipse(size=0.7)+labs(subtitle="Cluster Plot")

