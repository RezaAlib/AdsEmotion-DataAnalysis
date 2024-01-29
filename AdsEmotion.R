library("MASS")
library("stargazer")
library(ggplot2)
library(plot3D)
library(plotly)
library(countreg)
library(remotes)
library("Hmisc")
library(fitdistrplus)
library(tidyr)
library(sirus)
library(imbalance)
library(stringr)
library(svMisc)
library(dlnm)
library(glarma)
library(imputeTS)
library(fitdistrplus)
library(logspline)
library(dplyr)
library(tidyverse)
library(tseries)
library(forecast)
library(xts)
library(gcmr)
library(GGally)
library(corpcor)
library(vars)
library(dynlm)
library(Jmisc)
library(psych)
library(Hmisc)
library(kableExtra)


setwd("C:/Users/reza.alibakhshi/iCloudDrive/Research Projects and Activities/Research Projects/UGC - Ads Emotion/Data")
pdf<-read.csv("AdsPanelData.csv" , header = T ,stringsAsFactors=FALSE)
ads<-read.csv("AdsAnalysis.csv" , header = T ,stringsAsFactors=FALSE)


########********** Phase 1 - Variables Descriptions **********############
ads$fileId <- paste(ads$profile_name,'/',str_replace(substr(ads$video_name, 0, nchar(ads$video_name)),'mp4','json'),sep = "") # creating new file if for matching with panel data
adsfileId<-unique(ads$fileId) # selecting unique values
# Updating the duration of videos based on their number of frames
for (videoname in unique(pdf$fileId)){
  if (sum(is.na(pdf[pdf$fileId == videoname,]$video_duration))>1){
    if (sum(is.na(pdf[pdf$fileId==videoname,]$video_duration))>1){
      print(unique(pdf[pdf$fileId==videoname,]$video_duration))
      framenumb = max(ads[ads$fileId==videoname,]$frame_number,na.rm = TRUE)
      print(framenumb)
      vidduration = ifelse(is.infinite(framenumb),0,framenumb)/24
      pdf[pdf$fileId==videoname,]$video_duration = vidduration
    }
  }
}

pdf<-pdf[!is.na(pdf$video_duration),]

#updating the arima model for the EI of hapiness in initial and whole duration of video ads
# for (videoname in unique(pdf$fileId)) {
#   
#   subdata <- ads[ads$fileId==videoname,]
#   subdata <- subdata[order(subdata$frame_number),]
#   
#   subdata3 <- subdata[subdata$frame_number<73,]
#   
#   if (nrow(subdata3)>1){
#     print(videoname)
#     
#     fit1<-auto.arima(subdata$faceAttributes.emotion.happiness, seasonal = TRUE)
#     fit2<-auto.arima(subdata3$faceAttributes.emotion.happiness, seasonal = TRUE)
#     
#     pdf[pdf$fileId==videoname,]$VEI_happiness <- arimaorder(fit1)[[1]]
#     pdf[pdf$fileId==videoname,]$VEI_happiness_3 <- arimaorder(fit2)[[1]]
#   }
# 
# }


pdf$has_audio<-factor(pdf$has_audio)
pdf$initialTGR_happiness[is.infinite(pdf$initialTGR_happiness)]<- 999 #Assigning a big value to TGR for Inf values in dataset
pdf$overallTGR_happiness[is.infinite(pdf$overallTGR_happiness)]<- 999 #Assigning a big value to TGR for Inf values in dataset
pdf$location.has_public_page<-factor(pdf$location.has_public_page)
pdf$viewer_can_reshare<-factor(pdf$viewer_can_reshare)
pdf$brandFE<-factor(pdf$profile_name)
pdf$IndustryFE<-factor(pdf$business_category_name)
pdf$ageofvideo <- as.numeric(difftime(as.Date(pdf$collected_date), as.Date(pdf$video_creation_date)))/86400 #from seconds to days



### Adding additional video production quality measures as contro variables
# Read the CSV file
vidquality <- read.csv("video_properties.csv")

# where video_name is present in both datasets
pdf_videoquality <- merge(pdf, vidquality, by.x="fileId", by.y="video_name", all=TRUE)

## chnage resolutions to total pixels
# Define a function that parses the values from the string and multiplies them
multiply_values <- function(resolution) {
  # Remove the brackets and split the string into parts
  parts <- strsplit(gsub("\\[|\\]", "", resolution), ",")[[1]]
  # Convert the parts to numbers and multiply them together
  return(prod(as.numeric(parts)))
}

# Use sapply() to apply the function to each value in the size_values column
# and create a new column with the results
pdf_videoquality$resolution <- sapply(pdf_videoquality$resolution, multiply_values)


## taking care of inf
# Check for Inf or -Inf in each column
any(sapply(pdf_videoquality, function(x) any(is.infinite(x))))
# check for Inf or -Inf
# Check for NaN in each column
any(sapply(pdf_videoquality, function(x) any(is.nan(x))))

# Define a function to replace NA, Inf, -Inf, and NaN values with column mean
replace_with_mean <- function(x) {
  # Calculate the mean, ignoring NA, Inf, -Inf, and NaN values
  m <- min(x[!is.na(x) & !is.infinite(x) & !is.nan(x)], na.rm = TRUE)
  
  # Replace NA, Inf, -Inf, and NaN values with the mean
  x[is.na(x) | is.infinite(x) | is.nan(x)] <- m
  
  # Return the modified column
  return(x)
}


columns<-c("resolution", "aspect_ratio", "frame_rate",
           "average_contrast", "average_brightness", "average_saturation",
           "average_blue_yellow_ratio", "average_red_cyan_ratio", 
           "temporal_consistency", "average_audio_volume",
           "average_optical_flow_magnitude", "average_optical_flow_direction")

pdf_videoquality[columns] <- data.frame(sapply(pdf_videoquality[columns], replace_with_mean))

pdf<-pdf_videoquality



############################################################
## New Version of the Study with Cross-sectional analysis ##
library(dplyr)
pdfCross<- pdf %>% ## new dataset with crossectional lifetime engagement
  arrange((collected_date)) %>% 
  group_by(fileId) %>% slice_tail(n=1)

pdfCross<-as.data.frame(pdfCross)
### Summary stat for draft
summarydata<-pdfCross[c('video_view_count','like_count', 
                   'VEV_happiness_3','VEV_happiness','VEI_happiness_3','VEI_happiness',
                   'VEV_surprise_3','VEV_anger_3','VEV_fear_3','VEV_sadness_3','VEI_surprise_3','VEI_anger_3','VEI_fear_3','VEI_sadness_3',
                   'VEV_surprise','VEV_anger','VEV_fear','VEV_sadness','VEI_surprise','VEI_anger','VEI_fear','VEI_sadness',
                   'TEV_happiness','TEV_surprise','TEV_anger','TEV_fear','TEV_sadness','TEI_happiness','TEI_surprise','TEI_anger','TEI_fear','TEI_sadness',
                   'initialpeak_happiness_mag','initialpeak_happiness_dur','overallpeak_happiness_mag','overallpeak_happiness_dur',
                   'FSE_happiness','LSE_happiness','initialAVG_happiness','overallAVG_happiness','initialTGR_happiness','overallTGR_happiness',
                   'caption_happiness','caption_surprise','caption_anger','caption_fear','caption_sadness','video_duration','has_audio',
                   'location.has_public_page','viewer_can_reshare',
                   "ageofvideo",
                   "resolution", "aspect_ratio", "frame_rate",
                   "average_contrast", "average_brightness", "average_saturation",
                   "average_blue_yellow_ratio", "average_red_cyan_ratio", 
                   "temporal_consistency", "average_audio_volume",
                   "average_optical_flow_magnitude", "average_optical_flow_direction"
                   ,'followed_by_count','edge_follow_count')]


summarystat<-describe(summarydata)
stargazer(summarydata,type = 'text')
kable(summarystat) %>% kableExtra::kable_styling() #preparing view format of the summary stat

##Testing for Multicollinearity among variables
stargazer(cor2pcor(cov(na.omit(summarydata[c('video_view_count','like_count', 
                                             'VEV_happiness_3','VEV_happiness')])))
          ,type = "html")






#########################################################
#### Main analysis for the hypothesis and engagement ####
#########################################################

pdf2<-pdfCross[c('profile_name','fileId','collected_date', 'video_view_count','like_count',
            'VEV_happiness_3','VEV_happiness','VEI_happiness_3','VEI_happiness',
            'VEV_surprise_3','VEV_anger_3','VEV_fear_3','VEV_sadness_3','VEI_surprise_3','VEI_anger_3','VEI_fear_3','VEI_sadness_3',
            'VEV_surprise','VEV_anger','VEV_fear','VEV_sadness','VEI_surprise','VEI_anger','VEI_fear','VEI_sadness',
            'TEV_happiness','TEV_surprise','TEV_anger','TEV_fear','TEV_sadness','TEI_happiness','TEI_surprise','TEI_anger','TEI_fear','TEI_sadness',
            'initialpeak_happiness_mag','initialpeak_happiness_dur','overallpeak_happiness_mag','overallpeak_happiness_dur',
            'FSE_happiness','LSE_happiness','initialAVG_happiness','overallAVG_happiness','initialTGR_happiness','overallTGR_happiness',
            'caption_happiness','caption_surprise','caption_anger','caption_fear','caption_sadness','video_duration','has_audio',
            'location.has_public_page','viewer_can_reshare','followed_by_count','edge_follow_count','brandFE','IndustryFE','video_creation_date',
            'ind_cluster_id','glb_cluster_id',
            "ageofvideo",
            "resolution", "aspect_ratio", "frame_rate",
            "average_contrast", "average_brightness", "average_saturation",
            "average_blue_yellow_ratio", "average_red_cyan_ratio", 
            "temporal_consistency", "average_audio_volume",
            "average_optical_flow_magnitude", "average_optical_flow_direction")]


model_view_main <- glm.nb(video_view_count ~ followed_by_count
                          +VEV_happiness_3
                          +VEV_surprise_3+VEV_anger_3+VEV_fear_3+VEV_sadness_3
                          +initialpeak_happiness_mag+initialpeak_happiness_dur
                          +FSE_happiness
                          +brandFE
                          +has_audio+location.has_public_page
                          +viewer_can_reshare
                          +ageofvideo
                          +log(resolution+1) + aspect_ratio + frame_rate
                          + average_contrast + average_brightness 
                          + average_saturation + average_blue_yellow_ratio + average_red_cyan_ratio
                          + temporal_consistency  
                          + average_optical_flow_magnitude + average_optical_flow_magnitude
                          ,data = pdf2)
  

model_like_main <- glm.nb(like_count ~ followed_by_count 
                          +VEV_happiness 
                          +VEV_surprise+VEV_anger+VEV_fear+VEV_sadness 
                          +TEV_happiness+TEV_surprise+TEV_anger+TEV_fear+TEV_sadness
                          +overallpeak_happiness_mag+overallpeak_happiness_dur
                          +LSE_happiness
                          +brandFE
                          +has_audio+location.has_public_page
                          +viewer_can_reshare
                          +ageofvideo
                          +log(resolution) + aspect_ratio 
                          + average_contrast + average_brightness 
                          + average_saturation + average_blue_yellow_ratio + average_red_cyan_ratio
                          + temporal_consistency  
                          + average_optical_flow_magnitude + average_optical_flow_magnitude
                          ,data = pdf2)

summary(model_view_main)
summary(model_like_main)
stargazer(model_view_main,model_like_main, type = 'text')

## test of overdispersion for poisson alternative
library(AER)
poissonview<- glm(video_view_count ~ followed_by_count
                  +VEV_happiness_3
                  +VEV_surprise_3+VEV_anger_3+VEV_fear_3+VEV_sadness_3
                  +initialpeak_happiness_mag+initialpeak_happiness_dur
                  +FSE_happiness
                  +brandFE
                  +has_audio+location.has_public_page
                  +viewer_can_reshare
                  +ageofvideo
                  +log(resolution+1) + aspect_ratio + frame_rate
                  + average_contrast + average_brightness 
                  + average_saturation + average_blue_yellow_ratio + average_red_cyan_ratio
                  + temporal_consistency  
                  + average_optical_flow_magnitude + average_optical_flow_magnitude
                  ,data = pdf2, family = 'poisson')

poissonlike<- glm(like_count ~ followed_by_count 
                  +VEV_happiness 
                  +VEV_surprise+VEV_anger+VEV_fear+VEV_sadness 
                  +TEV_happiness+TEV_surprise+TEV_anger+TEV_fear+TEV_sadness
                  +overallpeak_happiness_mag+overallpeak_happiness_dur
                  +LSE_happiness
                  +brandFE
                  +has_audio+location.has_public_page
                  +viewer_can_reshare
                  +ageofvideo
                  +log(resolution) + aspect_ratio 
                  + average_contrast + average_brightness 
                  + average_saturation + average_blue_yellow_ratio + average_red_cyan_ratio
                  + temporal_consistency  
                  + average_optical_flow_magnitude + average_optical_flow_magnitude
                  ,data = pdf2, family = 'poisson')

dispersiontest(poissonview)
dispersiontest(poissonlike)






#########################################################
################ Robustness analyses ####################
#########################################################


################################################ Tests 1 ##########################################
########## Heterogenous Effects of Happiness Variability in Short and Long-Format Videos  #########
durmodel_like <- glm.nb(like_count ~ followed_by_count 
                        +VEV_happiness*video_duration
                        +VEV_surprise+VEV_anger+VEV_fear+VEV_sadness 
                        +TEV_happiness+TEV_surprise+TEV_anger+TEV_fear+TEV_sadness
                        +overallpeak_happiness_mag+overallpeak_happiness_dur
                        +LSE_happiness
                        +brandFE
                        +has_audio+location.has_public_page
                        +viewer_can_reshare
                        +ageofvideo
                        +log(resolution) + aspect_ratio 
                        + average_contrast + average_brightness 
                        + average_saturation + average_blue_yellow_ratio + average_red_cyan_ratio
                        + temporal_consistency  
                        + average_optical_flow_magnitude + average_optical_flow_magnitude
                        ,data = pdf2)


stargazer(durmodel_like, type = 'text')





############################### Tests 2 ###################################
########## Quadratic Relationship between EV and User Engagement  #########
# pdfQ<-pdf2 %>% 
#   arrange(desc(collected_date)) %>% 
#   group_by(fileId) %>% slice_head(n=1)
# 
# pdfQ<-na.omit(pdfQ)
# pdfQ<-pdfQ %>% 
#   filter_all(all_vars(!is.infinite(.)))

library(MASS)
Qmodel_view <- glm.nb(video_view_count ~ followed_by_count #time_varying variables. You will only get within-entity estimates for these variables
                      +poly(VEV_happiness_3,2) #time_invariant main IVs 
                      +VEV_surprise_3+VEV_anger_3+VEV_fear_3+VEV_sadness_3 #time_invariant controls
                      +initialpeak_happiness_mag+initialpeak_happiness_dur
                      +FSE_happiness
                      +brandFE
                      +has_audio+location.has_public_page
                      +viewer_can_reshare
                      +ageofvideo
                      +log(resolution) + aspect_ratio 
                      + average_contrast + average_brightness 
                      + average_saturation + average_blue_yellow_ratio + average_red_cyan_ratio
                      + temporal_consistency  
                      + average_optical_flow_magnitude + average_optical_flow_magnitude
                      ,data = pdf2)

Qmodel_like <- glm.nb(like_count ~ followed_by_count #time_varying variables. You will only get within-entity estimates for these variables
                      +poly(VEV_happiness,2) #time_invariant main IVs
                      +VEV_surprise+VEV_anger+VEV_fear+VEV_sadness #time_invariant controls
                      +TEV_happiness+TEV_surprise+TEV_anger+TEV_fear+TEV_sadness
                      +overallpeak_happiness_mag+overallpeak_happiness_dur
                      +LSE_happiness
                      +brandFE
                      +has_audio+location.has_public_page
                      +viewer_can_reshare
                      +ageofvideo
                      +log(resolution) + aspect_ratio 
                      + average_contrast + average_brightness 
                      + average_saturation + average_blue_yellow_ratio + average_red_cyan_ratio
                      + temporal_consistency  
                      + average_optical_flow_magnitude + average_optical_flow_magnitude
                      ,data = pdf2)


stargazer(Qmodel_view,Qmodel_like, type = 'text')





#################################################
##  Causality via Control Function Analysis #####
### IV - instead of using observed JobTraing, IV bases our estimate on the
# predicted value based on use of an instrumental variable
# Load dplyr package
library(dplyr)
library(lubridate)

pdf2 <- pdf2 %>%
  arrange(IndustryFE, profile_name, video_creation_date)


# create lagged variables
pdf2 <- pdf2 %>%
  group_by(profile_name) %>%
  arrange(video_creation_date) %>%
  mutate(
    lagged_VEV_happiness_3 = dplyr::lag(VEV_happiness_3),
    lagged_VEV_happiness = dplyr::lag(VEV_happiness)
  )


#second instrument 
pdf2$avg_VEV_happiness_3_other_profiles_prior <- NA
pdf2$avg_VEV_happiness_other_profiles_prior <- NA
pdf2<-as.data.frame(pdf2)


pdf2 <- pdf2 %>%
  arrange(IndustryFE, video_creation_date) %>% # Ensuring data is sorted by Industry and date
  group_by(IndustryFE) %>% 
  mutate(
    avg_VEV_happiness_3_other_profiles_prior = ifelse(profile_name != lag(profile_name), 
                                                      mean(VEV_happiness_3[profile_name != lag(profile_name)], na.rm = TRUE), 
                                                      NA_real_),
    
    avg_VEV_happiness_other_profiles_prior = ifelse(profile_name != lag(profile_name), 
                                                    mean(VEV_happiness[profile_name != lag(profile_name)], na.rm = TRUE), 
                                                    NA_real_),
    
    avg_VEV_happiness_3_other_profiles_month = ifelse(profile_name != lag(profile_name) & abs(difftime(video_creation_date, lag(video_creation_date), units = "days")) <= 30,
                                                      mean(VEV_happiness_3[profile_name != lag(profile_name) & abs(difftime(video_creation_date, lag(video_creation_date), units = "days")) <= 30], na.rm = TRUE), 
                                                      NA_real_),
    
    avg_VEV_happiness_other_profiles_month = ifelse(profile_name != lag(profile_name) & abs(difftime(video_creation_date, lag(video_creation_date), units = "days")) <= 30,
                                                    mean(VEV_happiness[profile_name != lag(profile_name) & abs(difftime(video_creation_date, lag(video_creation_date), units = "days")) <= 30], na.rm = TRUE), 
                                                    NA_real_)
  ) %>%
ungroup()



# 
# 
# for(i in 2:nrow(pdf2)){
#   # check if the profile and industry is the same as the previous row
#   if(pdf2$IndustryFE[i] == pdf2$IndustryFE[i-1] & pdf2$profile_name[i] != pdf2$profile_name[i-1]){
#     # if it is, calculate the average of VEV_happiness_3 from other profiles before the current video
#     pdf2$avg_VEV_happiness_3_other_profiles_prior[i] <- mean(pdf2$VEV_happiness_3[1:(i-1)][pdf2$IndustryFE[1:(i-1)] == pdf2$IndustryFE[i] & pdf2$profile_name[1:(i-1)] != pdf2$profile_name[i]], na.rm = TRUE)
#     pdf2$avg_VEV_happiness_other_profiles_prior[i] <- mean(pdf2$VEV_happiness[1:(i-1)][pdf2$IndustryFE[1:(i-1)] == pdf2$IndustryFE[i] & pdf2$profile_name[1:(i-1)] != pdf2$profile_name[i]], na.rm = TRUE)
#     # calculate the average of VEV_happiness_3 from other profiles in the same industry within a one month time window
#     pdf2$avg_VEV_happiness_3_other_profiles_month[i] <- mean(pdf2$VEV_happiness_3[1:(i-1)][pdf2$IndustryFE[1:(i-1)] == pdf2$IndustryFE[i] & pdf2$profile_name[1:(i-1)] != pdf2$profile_name[i] & abs(difftime(pdf2$video_creation_date[i], pdf2$video_creation_date[1:(i-1)], units = "days")) <= 30], na.rm = TRUE)
#     pdf2$avg_VEV_happiness_other_profiles_month[i] <- mean(pdf2$VEV_happiness[1:(i-1)][pdf2$IndustryFE[1:(i-1)] == pdf2$IndustryFE[i] & pdf2$profile_name[1:(i-1)] != pdf2$profile_name[i] & abs(difftime(pdf2$video_creation_date[i], pdf2$video_creation_date[1:(i-1)], units = "days")) <= 30], na.rm = TRUE)
#   }
# }

# replace NA with 0
pdf2$lagged_VEV_happiness_3[is.na(pdf2$lagged_VEV_happiness_3)] <- 0
pdf2$lagged_VEV_happiness[is.na(pdf2$lagged_VEV_happiness)] <- 0
pdf2$avg_VEV_happiness_3_other_profiles_prior[is.na(pdf2$avg_VEV_happiness_3_other_profiles_prior)] <- 0
pdf2$avg_VEV_happiness_other_profiles_prior[is.na(pdf2$avg_VEV_happiness_other_profiles_prior)] <- 0
pdf2$avg_VEV_happiness_3_other_profiles_month[is.na(pdf2$avg_VEV_happiness_3_other_profiles_month)] <- 0
pdf2$avg_VEV_happiness_other_profiles_month[is.na(pdf2$avg_VEV_happiness_other_profiles_month)] <- 0



# First stage 
FirstStage.initial <- lm(VEV_happiness_3 ~ lagged_VEV_happiness_3+avg_VEV_happiness_3_other_profiles_month+profile_name
                          ,data = pdf2)
FirstStage.overall <- lm(VEV_happiness~ lagged_VEV_happiness+avg_VEV_happiness_other_profiles_month+profile_name,data = pdf2)
summary(FirstStage.initial)
summary(FirstStage.overall)

SD_hat.initial <- predict(FirstStage.initial)
SD_pred.initial <- pnorm(SD_hat.initial)

SD_hat.overall <- predict(FirstStage.overall)
SD_pred.overall <- pnorm(SD_hat.overall)


### Control function approach
pdf2$ExpectedError.initial <- residuals(FirstStage.initial)
pdf2$ExpectedError.overall <- residuals(FirstStage.overall)

CFmodel_view <- glm.nb(video_view_count ~ followed_by_count
                       +VEV_happiness_3
                       +VEV_surprise_3+VEV_anger_3+VEV_fear_3+VEV_sadness_3
                       +initialpeak_happiness_mag+initialpeak_happiness_dur
                       +FSE_happiness
                       #+brandFE this is not necessary as we include it in the first stage
                       +has_audio+location.has_public_page
                       +viewer_can_reshare
                       +ageofvideo
                       +log(resolution+1) + aspect_ratio + frame_rate
                       + average_contrast + average_brightness 
                       + average_saturation + average_blue_yellow_ratio + average_red_cyan_ratio
                       + temporal_consistency  
                       + average_optical_flow_magnitude + average_optical_flow_magnitude
                       +ExpectedError.initial
                       ,data = pdf2)


CFmodel_like <- glm.nb(like_count ~ followed_by_count 
                       +VEV_happiness 
                       +VEV_surprise+VEV_anger+VEV_fear+VEV_sadness 
                       +TEV_happiness+TEV_surprise+TEV_anger+TEV_fear+TEV_sadness
                       +overallpeak_happiness_mag+overallpeak_happiness_dur
                       +LSE_happiness
                       #+brandFE this is not necessary as we include it in the first stage
                       +has_audio+location.has_public_page
                       +viewer_can_reshare
                       +ageofvideo
                       +log(resolution) + aspect_ratio 
                       + average_contrast + average_brightness 
                       + average_saturation + average_blue_yellow_ratio + average_red_cyan_ratio
                       + temporal_consistency  
                       + average_optical_flow_magnitude + average_optical_flow_magnitude
                       +ExpectedError.overall
                       ,data = pdf2)

summary(CFmodel_view)
summary(CFmodel_like)

stargazer(CFmodel_view,CFmodel_like,type = 'text')



library(AER)
# First stage 
IVModel.initial <- ivreg(log(video_view_count+1) ~ VEV_happiness_3|lagged_VEV_happiness_3+avg_VEV_happiness_3_other_profiles_month+profile_name,
                            data = pdf2)
IVModel.overall <- ivreg(log(like_count) ~ VEV_happiness|lagged_VEV_happiness+avg_VEV_happiness_other_profiles_month+profile_name,
                            data = pdf2)
summary(IVModel.initial,diagnostics = TRUE) #if Wu-Hausman is significant it means that instrument is not weak
summary(IVModel.overall,diagnostics = TRUE) #if Wu-Hausman is significant it means that instrument is not weak
