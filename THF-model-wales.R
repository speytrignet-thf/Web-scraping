#####################################################
################### INTRODUCTION ####################
#####################################################

# For this exercise, not having access to real patient or care data, I was keen to talk you through a small analysis
# that involves survey data with health outcomes, but also linked to external sources of data

# • A brief explanation of what problem you are trying to solve and the data you have used

# RESEARCH QUESTION:
# What is the relationship between the accessibility of medical services (distance to GP surgeries and hospitals)
# and the likelihood of their use? Focus on inpatient care among a general population cohort in Wales in 2017.

# WHY?:
# According to Royal College of Nursing last year - Up to 10 million people in rural Britain live in 'healthcare deserts'.
# If we move towards a digital health service, this could get even worse.
# Are there already differences in patterns of healthcare use based on proximity?
# I have restricted analysis to Wales because, through recent work for the Welsh Government for a transport
# scheme, I have already processed data on service accessibility and I am familiar with the
# survey data available for that region.

# DATA:
# - Survey data from waves 8 and 9 of Understanding Society - the largest nationally-representative
# longitudinal survey in the UK (Funded by UK government, frequently used to conduct research).
# It has a module on health and healthcare use.
# - Locations of GP surgeries - from NHS Digital database.
# - Locations of hospitals - web-scraped from Open Street Maps using Overpass Turbo.

# • The approach you have taken to solving the problem

# METHODOLOGY:
# - Survey data: create new outcome in wave 8: 'will require inpatient care in the next 12 months'.
# End up with a cross-section, but with info on future.
# - Data imputation: household location is not available in the public version of Understanding Society.
# For the sake of this exercise, I resort to second-best solution: impute postcodes at random
# based on whether repondents live in a rural or urban area (e.g. if you live in urban area in Wales,
# you may get assigned a Cardiff or Swansea postcode).
# - GIS analysis: How did I create the new predictor?
# I wrote a function that summarizes for each postcode: how close your nearest GP surgery/hospital is.
# This new predictor is merged back into the survey responses based on the imputed postcode.
# - Models: I used logistic regressions to assess the impact of those factors on the likelihood of accessing
# inpatient care and I assess the marginal impact of those factors.

##############################################
################### SETUP ####################
##############################################

################## Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,gmodels,Rmisc,DescTools,data.table,Hmisc,tibble,rgdal,leaflet,rgeos,raster,plotly,pbapply,pbmcapply,skimr,ROCR,pROC,margins,jtools)

################## Clean up the global environment
rm(list = ls())

################## Set directory
setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("Documents/GitHub/THF-model-wales/Files/")

########################################################################
################### PRE-LOAD USER-WRITTEN FUNCTIONS ####################
########################################################################

################## Function to produce 'distance to GP surgery' predictor based on place of residence
number.within.buffer <- function(k,distpar_km,adminpoints.spdf,interestpoints.spdf){
  
  aux.refpoint <- adminpoints.spdf[k,] #Isolate the admin-area-center
  
  #Store its coordinates
  long.point <- spTransform(aux.refpoint, CRS(latlong))@coords[1,1]
  lat.point <- spTransform(aux.refpoint, CRS(latlong))@coords[1,2]
  a <- c(long.point,lat.point)
  
  #Create a buffer around the amdin-area-center
  aux.refpoint.buffer <- rgeos::gBuffer(aux.refpoint,width=distpar_km*1000,byid=TRUE)
  
  # plot(aux.refpoint.buffer)
  # points(aux.refpoint,col="red")
  # points(healthcare_resources_shp,col="blue")
  
  #Extract the candidate items in that buffer
  aux.overlay <- over(interestpoints.spdf,aux.refpoint.buffer)
  candidates.spdf <- interestpoints.spdf[which(!is.na(aux.overlay[,1])),]
  N_aux <- nrow(candidates.spdf@data) #How many did we find?
  
  if (N_aux>=1) {
    
    #Extract the info about the candidate items
    candidates.data <- candidates.spdf@data
    
    #This matrix will store the information about distances
    cand.mat <- as.data.frame(matrix(NA, nrow = N_aux, ncol = 6))
    names(cand.mat)=c("pcode","country","buffer.km","N.in.buffer","dist.to.point","name")
    
    #Compute distances to admin-area-centre in a loop
    for (s in 1:N_aux){
      
      #Coordinates of candidate point
      long.item <- spTransform(candidates.spdf, CRS(latlong))@coords[s,1]
      lat.item <- spTransform(candidates.spdf, CRS(latlong))@coords[s,2]
      b <- c(long.item,lat.item)
      
      #Compute distance
      distKm <- geosphere::distCosine(a, b, r=6378137)/1000
      #distMin=(gmapsdistance(origin=m,destination=n,mode="walking")$Time)/60
      
      #Populate the results matrix
      cand.mat[s,1]=as.character(aux.refpoint@data$pcode)
      cand.mat[s,2]=as.character(aux.refpoint@data$ctry)
      cand.mat[s,3]=distpar_km
      cand.mat[s,4]=N_aux
      cand.mat[s,5]=distKm
      cand.mat[s,6]=as.character(candidates.spdf@data$Name[s])
      
    } 
    
    #Extract the nearest one, which will be the final output
    idx.nearest <- which(cand.mat[,5]==min(cand.mat[,5]))[1]
    end.mat <- cand.mat[idx.nearest,]
    
  }
  
  else {
    
    #This matrix will store the results - which are always 0 if there were no candidates
    end.mat <- as.data.frame(matrix(NA, nrow = N_aux, ncol = 6))
    
    names(end.mat)=c("pcode","country","buffer.km","N.in.buffer","dist.to.point","name")
    
    end.mat[1,1]=as.character(aux.refpoint@data$pcode)
    end.mat[1,2]=as.character(aux.refpoint@data$ctry)
    end.mat[1,3]=distpar_km
    end.mat[1,4]=N_aux
    end.mat[1,5]=NA
    end.mat[1,6]=NA
    
  }
  
  return(end.mat)}

################## Projection codes
ukgrid = "+init=epsg:27700"
latlong="+init=epsg:4326"

###################################################
################### SURVEY DATA ###################
###################################################

################## Import dataset (*)
USoc <- fread("Understanding-Society-Wave8.csv", header=TRUE, sep=",", check.names=T) %>% 
          filter(.,gor_dv=="[10] wales") %>% as_tibble()
skim(USoc)

##################  10% of sample needed hospital care the year after being surveyed (*)
round(mean(USoc$inpatient_nexttyear)*100,1)
ggplot()+geom_text(aes(x=1,y=1),size=20,label=paste0(round(mean(USoc$inpatient_nexttyear)*100,1),"%")
) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(), panel.background = element_blank())

################## The median age of those who needed hospital care was 6 years higher (*)
mu_age <- ddply(USoc, "inpatient_nexttyear", summarise, age.median=median(age))
USoc %>% ggplot(., aes(x=age, fill=factor(inpatient_nexttyear), color=factor(inpatient_nexttyear))) +
  geom_density(alpha=0.5) + theme(panel.background = element_blank(),legend.position="bottom") + ggtitle("Distribution of age") +
  geom_vline(data=mu_age, aes(xintercept=age.median, color=factor(inpatient_nexttyear)),
             linetype="dashed") + scale_colour_brewer(type="qual",labels = c("No", "Yes"),palette=4) + scale_fill_brewer(type="qual",labels = c("No", "Yes"),palette=4) + labs(fill = "Inpatient care next 12m",col="Inpatient care next 12m")
rm(mu_age)

##################  Those with a pre-existing long-term health condition were twice as likely (*)
##################  to need hospital care
USoc %>% ddply(., "LT_health", summarise, rate.inpatient=mean(inpatient_nexttyear)*100) %>% round(.,1) %>%
  ggplot(., aes(x=factor(LT_health), y=rate.inpatient, fill=factor(LT_health))) +
  geom_bar(stat="identity") + geom_text(aes(label=rate.inpatient, y = rate.inpatient + 1), position=position_dodge(width=0.9)) +
  scale_fill_brewer(type="qual",labels = c("No", "Yes"),palette=1) + labs(fill = "Long-term health condition") + xlab("Long-term health condition") + ylab("%") + ggtitle("% requiring inpatient care next 12m") + 
  theme(panel.background = element_blank(),legend.position="bottom")

##################  There is, at first sight, no relationship between living in an urban area (*)
##################  and needing hospital care
USoc %>% ddply(., "urban", summarise, rate.inpatient=mean(inpatient_nexttyear)*100) %>% round(.,1) %>%
  ggplot(., aes(x=factor(urban), y=rate.inpatient, fill=factor(urban))) +
  geom_bar(stat="identity") + geom_text(aes(label=rate.inpatient, y = rate.inpatient + 1), position=position_dodge(width=0.9)) +
  scale_fill_brewer(type="qual",labels = c("No", "Yes"),palette=2) + labs(fill = "Living in urban area") + xlab("Living in urban area") + ylab("%") + ggtitle("% requiring inpatient care next 12m") +
  theme(panel.background = element_blank(),legend.position="bottom")

#What if we had more precise data on distance to the nearest GP/Hospital, would we find the same thing? (*)
#We don't know where people live in this version of the dataset, but we do have an urban/rural indicator
#So, as a thought experiment, let's impute postcodes to respondents based on that indicator

#####################################################################
################### LOOKUP TABLES FOR GEOGRAPHIES ###################
#####################################################################

################## Import directory of Welsh postcodes
Wales_postcodes_small <- fread("Welsh postcodes small.csv",header=TRUE, sep=",", check.names=T)

################## Randomly select postcodes to merge into the survey data
################## Separately for urban and rural respondents
USoc_urban <- filter(USoc,urban==1) %>% select(.,pidp)
urban_postcodes <- filter(Wales_postcodes_small,urban==1)
samples_postcodes_urban_idx <- sample(1:nrow(urban_postcodes),nrow(USoc_urban), replace = TRUE, prob = NULL)
USoc_imputed_pcode_urban <- urban_postcodes[samples_postcodes_urban_idx,] %>% select(.,pcode) %>% cbind.data.frame(.,USoc_urban)

USoc_rural <- filter(USoc,urban==0) %>% select(.,pidp)
rural_postcodes <- filter(Wales_postcodes_small,urban==0)
samples_postcodes_rural_idx <- sample(1:nrow(rural_postcodes),nrow(USoc_rural), replace = TRUE, prob = NULL)
USoc_imputed_pcode_rural <- urban_postcodes[samples_postcodes_rural_idx,] %>% select(.,pcode) %>% cbind.data.frame(.,USoc_rural)

imputed_postcodes <- rbind(USoc_imputed_pcode_urban,USoc_imputed_pcode_rural) %>% as.data.table()

################## Merge imputed postcodes back into dataset
USoc <- left_join(USoc,imputed_postcodes,by="pidp")
rm(urban_postcodes,samples_postcodes_urban_idx,USoc_imputed_pcode_urban,rural_postcodes,samples_postcodes_rural_idx,USoc_imputed_pcode_rural,USoc_rural,USoc_urban,imputed_postcodes)
  
################## Visualize (imputed) locations of survey respondents (*)
Survey_postcodes_shp <- SpatialPointsDataFrame(cbind(Wales_postcodes_small$long,Wales_postcodes_small$lat),
                                              data = Wales_postcodes_small,
                                              proj4string = CRS(latlong)) %>% subset(., pcode %in% USoc$pcode)

leaflet(Survey_postcodes_shp) %>% addProviderTiles(providers$Stamen.Terrain) %>% addCircleMarkers(data=Survey_postcodes_shp,fillColor = "blue",radius=5, fillOpacity = 0.5,stroke=T,col="#737373",weight = 1)

############################################################################
################### IMPORT (WEB-SCRAPED) GEOSPATIAL DATA ###################
############################################################################

################## Import GP surgery locations (NHS Digital)
gp_surgeries <- fread("epraccur-clean.csv", header = T, sep = ',', data.table = T)

gp_surgeries_shp <- SpatialPointsDataFrame(cbind(gp_surgeries$long,gp_surgeries$lat),
                                          data = gp_surgeries[,1:2],
                                          proj4string = CRS(latlong))

################## Import hospital locations (Open Street Maps)
OSM_points_shp <- readOGR("hospitals.geojson", "hospitals", require_geomType="wkbPoint") #Import shapefile
OSM_points_shp <- spTransform(OSM_points_shp, CRS(latlong)) #Set to the same projection
OSM_points_shp@data <- select(OSM_points_shp@data,name,amenity) %>%
  rename(.,Name=name,Type=amenity)

################## Visualize web-scraped geodata (*)
################## Note areas in the middle with much lower provison (relative to population density)
healthcare_resources_shp <- raster::bind(OSM_points_shp,gp_surgeries_shp)
rm(OSM_points_shp,gp_surgeries_shp,gp_surgeries,Wales_postcodes_small) #Clean up environment

palher <- colorFactor(palette=c("#e7298a","#e6ab02"), levels = c("GP","hospital"))
leaflet(healthcare_resources_shp) %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  addCircleMarkers(data=healthcare_resources_shp,fillColor = ~palher(Type),radius=5,
                   fillOpacity = 0.5,stroke=T,col="#737373",weight = 1) %>% addLegend("bottomright", col=c("#e7298a","#e6ab02"), title = 'Amenity', labels=c("GP","hospital"),opacity = 1) # legend title

#Now, let's create the distance to nearest GP/hospital for these survey respondents (*)

##############################################################
################### PRODUCE NEW PREDICTORS ###################
##############################################################

##################  Test the user-written function for first 5 postcodes among survey responses (*)
loop.support.one <- 1:5
Survey_postcodes_shp <- spTransform(Survey_postcodes_shp, CRS(ukgrid))
healthcare_resources_shp <- spTransform(healthcare_resources_shp, CRS(ukgrid))
survey.predictors.wales <- pbmclapply(loop.support.one,number.within.buffer,
                                      distpar_km=20,adminpoints.spdf=Survey_postcodes_shp,
                                      interestpoints.spdf=healthcare_resources_shp) %>% data.table::rbindlist(.)
survey.predictors.wales

##################  Applying can take up to 30min, so let's import the ready-made results instead (*)
survey.predictors.wales <- fread("Welsh postcodes small 20km.csv",header=TRUE, sep=",", check.names=T)

################## How is this new predictor distributed? (*)
################## This confirms that, overall, access is good (median distance 700m)
################## but about 5% of postcodes are more than 6km away from a GP surgery
round(median(survey.predictors.wales$dist.to.point*1000),1)
ggplot(survey.predictors.wales, aes(dist.to.point, fill = cut(dist.to.point, 100))) +
  geom_histogram(show.legend = FALSE) + theme_minimal() + labs(x = "Km to nearest GP/hospital", y = "n") +
  ggtitle("Histogram") + scale_fill_discrete(h = c(240, 10), c = 120, l = 70)

################## Merge new predictor into the survey (*)
USoc <- left_join(USoc,survey.predictors.wales,by="pcode")

#Now, let's run our logistic regression models (*)

#########################################################################
###################  REGRESSION MODELS TO ASSESS IMPACT #################
#########################################################################

##################  Age is associated with a higher likelihood of needing hospital (*)
Model_1 <-  glm(inpatient_nexttyear ~ age+male+leq_hhincome,data=USoc, family=binomial)
jtools::plot_summs(Model_1, scale = TRUE)
cplot(Model_1, "age")

##################  But after adjusting for health conditions, this is more likely related to health (*)
##################  In fact, age is no longer a significant predictor and we see that those with pre-existing
##################  long-term conditions are still almost twice as likely to need inpatient care
Model_2 <-  glm(inpatient_nexttyear ~ age+male+leq_hhincome+LT_health,data=USoc, family=binomial)
jtools::plot_summs(Model_2, scale = TRUE)

##################  In fact, according to model, those with health conditions are twice as likely (*)
##################  to require hospital care
new_data <- rbind(USoc %>% select(.,age,male,leq_hhincome) %>% apply(.,2,mean) %>% t() %>% as.data.frame() %>% cbind(LT_health=1,.),
      USoc %>% select(.,age,male,leq_hhincome) %>% apply(.,2,mean) %>% t() %>% as.data.frame() %>% cbind(LT_health=0,.))
predicted_data <- predict(Model_2, newdata = new_data, type="response")

vis_model2 <- cbind.data.frame(LT_health=new_data$LT_health,mean.likelihood=predicted_data*100) %>% round(.,1) %>%
  ggplot(., aes(x=factor(LT_health), y=mean.likelihood,
                fill=factor(LT_health))) + geom_bar(stat="identity") + geom_text(aes(label=mean.likelihood), position=position_dodge(width=1)) + scale_fill_brewer(type="qual",labels = c("No", "Yes"),palette=4) +
  labs(fill = "Previous health condition") + xlab("Previous health condition") + ylab("%") + theme_minimal() + ggtitle("Predicted likelihood of needing hospital care")
vis_model2

################## Living in an urban area is not associated with more hospital stays (*)
Model_3 <-  glm(inpatient_nexttyear ~ age+male+leq_hhincome+LT_health+urban,data=USoc, family=binomial)
jtools::plot_summs(Model_3, scale = TRUE)

################## Actually - neither is living further away from a GP or hospital (*)
Model_4 <-  glm(inpatient_nexttyear ~ age+male+leq_hhincome+LT_health+dist.to.point,data=USoc, family=binomial)
jtools::plot_summs(Model_4, scale = TRUE)

#########################################################
############# ASSESSMENT OF PREDICTIVE MODEL ############
#########################################################

################## This model has poor predictive performance (AUC of 0.6)
################## Only slightly better than random prediction
predict_model4 <- predict(Model_4, type="response")
AUC <- pROC::roc(Model_4$data$inpatient_nexttyear,predict_model4,levels = c(0, 1), direction = "<")
AUC

################## Also reflected in the ROC curve
ROCRpred_model4 <-  ROCR::prediction(predict_model4, Model_4$data$inpatient_nexttyear)
ROCRperf_model4 <- performance(ROCRpred_model4, "tpr", "fpr")
plot(ROCRperf_model4, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#############################################################
################### SUMMARISE THE POINTS BELOW ##############
#############################################################

# • What you learned in this project from an analysis and coding perspective

# Analysis perspective:

# Descriptive findings quite striking: suggests approx. 10% of Welsh population needed hospital care in 2017.
# But just under 50% consider themselves to have a long-term health condition (includes hypertension, asthma, mental health).
# I was pleasantly surprised that the distance to a surgery in Wales is quite short, although there are
# exceptions - and a short linear distance doesn't always translate into a short journey time (no roads).

# Coding perspective:

# Taking longitudinal surveys and using future information to create datasets that accomodate
# predictive analytics.
# R enables powerful tools for GIS analysis (e.g. raster and sp packages).
# Displaying results in maps (extensions are heat maps and choropleth maps).
# A good example of putting user-written functions to use to create new predictors.
# Making code run faster: exploited multi-core element and parallel computing (mclapply function).
# Using jtools package to summarise regression results in a very intuitive way.

# • Reflections on what you would do differently in another project

# In a real-world, setting I would request real household locations (otherwise just info from urban/rural
# plus noise). This would also allow me to see whether my dataset does have the most remote
# potentially vulnerable households.
# Look at non-urgent/outpatient care where distance may be more of a factor in deciding to see a doctor.
# Journey times by car/PT rather than as-the-crow-flies distance.
# Prediction with small samples is prone to over-fitting, so I would use methods for cross-validation.
# Try a different model for whole UK and allow interactions to assess different impacts according to region.
# Use R Notebook to present analysis.
