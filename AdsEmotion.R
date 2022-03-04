library("MASS")
library("stargazer")
library(ggplot2)
library(plot3D)
library(plotly)
#library(countreg)
library(remotes)
library("Hmisc")
library(fitdistrplus)
library(tidyr)
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
library(corpcor)

setwd("/Users/reza/Library/Mobile Documents/com~apple~CloudDocs/Research Projects and Activities/Research Projects/Ads Emotion")
pdf<-read.csv("Data/AdsPanelData.csv" , header = T ,stringsAsFactors=FALSE)
ads<-read.csv("Data/AdsAnalysis.csv" , header = T ,stringsAsFactors=FALSE)


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
for (videoname in unique(pdf$fileId)) {
  
  subdata <- ads[ads$fileId==videoname,]
  subdata <- subdata[order(subdata$frame_number),]
  
  subdata3 <- subdata[subdata$frame_number<73,]
  
  if (nrow(subdata3)>1){
    print(videoname)
    
    fit1<-auto.arima(subdata$faceAttributes.emotion.happiness, seasonal = TRUE)
    fit2<-auto.arima(subdata3$faceAttributes.emotion.happiness, seasonal = TRUE)
    
    pdf[pdf$fileId==videoname,]$VEI_happiness <- arimaorder(fit1)[[1]]
    pdf[pdf$fileId==videoname,]$VEI_happiness_3 <- arimaorder(fit2)[[1]]
  }

}


pdf$has_audio<-factor(pdf$has_audio)
pdf$initialTGR_happiness[is.infinite(pdf$initialTGR_happiness)]<- 999 #Assigning a big value to TGR for Inf values in dataset
pdf$overallTGR_happiness[is.infinite(pdf$overallTGR_happiness)]<- 999 #Assigning a big value to TGR for Inf values in dataset
pdf$location.has_public_page<-factor(pdf$location.has_public_page)
pdf$viewer_can_reshare<-factor(pdf$viewer_can_reshare)
pdf$brandFE<-factor(pdf$profile_name)
pdf$IndustryFE<-factor(pdf$business_category_name)



############################################################
## New Version of the Study with Cross-sectional analysis ##
library(dplyr)
pdfCross<- pdf %>% ## new dataset with crossectional lifetime engagement
  arrange((collected_date)) %>% 
  group_by(fileId) %>% slice_tail(n=1)


### Summary stat for draft
summarydata<-pdfCross[c('video_view_count','like_count', 
                   'VEV_happiness_3','VEV_happiness','VEI_happiness_3','VEI_happiness',
                   'VEV_surprise_3','VEV_anger_3','VEV_fear_3','VEV_sadness_3','VEI_surprise_3','VEI_anger_3','VEI_fear_3','VEI_sadness_3',
                   'VEV_surprise','VEV_anger','VEV_fear','VEV_sadness','VEI_surprise','VEI_anger','VEI_fear','VEI_sadness',
                   'TEV_happiness','TEV_surprise','TEV_anger','TEV_fear','TEV_sadness','TEI_happiness','TEI_surprise','TEI_anger','TEI_fear','TEI_sadness',
                   'initialpeak_happiness_mag','initialpeak_happiness_dur','overallpeak_happiness_mag','overallpeak_happiness_dur',
                   'FSE_happiness','LSE_happiness','initialAVG_happiness','overallAVG_happiness','initialTGR_happiness','overallTGR_happiness',
                   'caption_happiness','caption_surprise','caption_anger','caption_fear','caption_sadness','video_duration','has_audio',
                   'location.has_public_page','viewer_can_reshare','followed_by_count','edge_follow_count')]


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
            'ind_cluster_id','glb_cluster_id')]


model_view_main <- glm.nb(video_view_count ~ followed_by_count
                      +VEV_happiness_3
                      +VEV_surprise_3+VEV_anger_3+VEV_fear_3+VEV_sadness_3
                      +initialpeak_happiness_mag+initialpeak_happiness_dur
                      +FSE_happiness
                      +has_audio+location.has_public_page
                      +viewer_can_reshare
                      ,data = pdf2)
  

model_like_main <- glm.nb(like_count ~ followed_by_count 
                       +VEV_happiness 
                       +VEV_surprise+VEV_anger+VEV_fear+VEV_sadness 
                       +TEV_happiness+TEV_surprise+TEV_anger+TEV_fear+TEV_sadness
                       +overallpeak_happiness_mag+overallpeak_happiness_dur
                       +LSE_happiness
                       +has_audio+location.has_public_page
                       +viewer_can_reshare
                       ,data = pdf2)

summary(model_view_main)
summary(model_like_main)

## test of overdispersion for poisson alternative
library(AER)
poissonview<- glm(video_view_count ~ followed_by_count
                  +VEV_happiness_3
                  +VEV_surprise_3+VEV_anger_3+VEV_fear_3+VEV_sadness_3
                  +initialpeak_happiness_mag+initialpeak_happiness_dur
                  +FSE_happiness
                  +has_audio+location.has_public_page
                  +viewer_can_reshare
                  ,data = pdf2, family = 'poisson')
poissonlike<- glm(like_count ~ followed_by_count 
                  +VEV_happiness 
                  +VEV_surprise+VEV_anger+VEV_fear+VEV_sadness 
                  +TEV_happiness+TEV_surprise+TEV_anger+TEV_fear+TEV_sadness
                  +overallpeak_happiness_mag+overallpeak_happiness_dur
                  +LSE_happiness
                  +has_audio+location.has_public_page
                  +viewer_can_reshare
                  ,data = pdf2, family = 'poisson')

dispersiontest(poissonview)
dispersiontest(poissonlike)






#########################################################
################ Robustness analyses ####################
#########################################################

################### Misc Code ##########################
######### Plotting time series of a video view ##########
ptseries_view<-ggplot(pdf3[pdf3$fileId==unique(pdf3$fileId)[10],], aes(x=collected_date, y=video_view_count)) +
  geom_line(color="turquoise4")+
  scale_x_date(name = 'Collected Date', date_breaks = '1 week',
               date_labels = '%d-%m-%Y')+
  theme_minimal() + 
  labs(x="Collected Date", y="Video Views", title="Evolution of Viewing Video by Followers") +
  theme(plot.title = element_text(hjust=0.5, size=20, face="bold"))

ptseries_like<-ggplot(pdf3[pdf3$fileId==unique(pdf3$fileId)[11],], aes(x=collected_date, y=like_count)) +
  geom_line(color="turquoise4")+
  scale_x_date(name = 'Collected Date', date_breaks = '1 week',
               date_labels = '%d-%m-%Y')+
  theme_minimal() + 
  labs(x="Collected Date", y="Video Likes", title="Evolution of Liking Video by Followers") +
  theme(plot.title = element_text(hjust=0.5, size=20, face="bold"))




########## Test 1 - Causality through Counterfactual Analysis #########
## Ref: https://onlinelibrary.wiley.com/doi/10.3982/ECTA10582
## Manual: https://cran.r-project.org/web/packages/Counterfactual/Counterfactual.pdf

library(Counterfactual)

ads_noemotion<-ads[(ads$faceAttributes.emotion.contempt==0&ads$faceAttributes.emotion.anger==0&ads$faceAttributes.emotion.disgust==0
                   &ads$faceAttributes.emotion.fear==0&ads$faceAttributes.emotion.happiness==0&ads$faceAttributes.emotion.neutral==0
                   &ads$faceAttributes.emotion.sadness==0&ads$faceAttributes.emotion.surprise==0),]


pdf_cf<-pdf2[pdf2$VEV_happiness==0&pdf2$VEV_happiness_3==0,] ##counterfactrual dataset
pdf2$CF.Group <- ifelse(pdf2$fileId %in% unique(pdf_cf$fileId), 1, 0) # 0 indicates the reference group and 1 indicates the counterfactual group


CFmodel_view <- counterfactual(log(video_view_count) ~ VEV_happiness_3
                               # +log(followed_by_count+1)+VEV_surprise_3+VEV_anger_3+VEV_fear_3+VEV_sadness_3+VEI_surprise_3+VEI_anger_3+VEI_fear_3+VEI_sadness_3
                               # +initialpeak_happiness_mag+initialpeak_happiness_dur
                               # +FSE_happiness
                               # +has_audio+location.has_public_page
                               # +viewer_can_reshare
                               , data = pdf2, group = CF.Group)

CFmodel_likes <- counterfactual(log(like_count) ~ log(followed_by_count) + VEV_happiness #time_invariant main IVs
                                # +VEV_surprise+VEV_anger+VEV_fear+VEV_sadness+VEI_surprise+VEI_anger+VEI_fear+VEI_sadness #time_invariant controls
                                # +TEV_happiness+TEV_surprise+TEV_anger+TEV_fear+TEV_sadness+TEI_happiness+TEI_surprise+TEI_anger+TEI_fear+TEI_sadness
                                # +overallpeak_happiness_mag+overallpeak_happiness_dur
                                # +LSE_happiness
                                # +has_audio+location.has_public_page
                                # +viewer_can_reshare
                                , data = pdf2, group = CF.Group)






######################## Test 2 #################################################
############ Controlling for different observation period for videos ############
library(dplyr)
pdf2.NE<- pdf2

pdf2.NE$ageofvideo <- difftime(as.Date(pdf2$collected_date), as.Date(pdf2$video_creation_date))


model_view_NE <- glm.nb(video_view_count ~ followed_by_count
                        +VEV_happiness_3
                        +VEV_surprise_3+VEV_anger_3+VEV_fear_3+VEV_sadness_3
                        +initialpeak_happiness_mag+initialpeak_happiness_dur
                        +FSE_happiness
                        +has_audio+location.has_public_page
                        +viewer_can_reshare +ageofvideo
                    ,data = pdf2.NE)

model_like_NE <- glm.nb(like_count ~ followed_by_count 
                        +VEV_happiness 
                        +VEV_surprise+VEV_anger+VEV_fear+VEV_sadness 
                        +TEV_happiness+TEV_surprise+TEV_anger+TEV_fear+TEV_sadness
                        +overallpeak_happiness_mag+overallpeak_happiness_dur
                        +LSE_happiness
                        +has_audio+location.has_public_page
                        +viewer_can_reshare +ageofvideo
                    ,data = pdf2.NE)


summary(model_view_NE)
summary(model_like_NE)






############################### Tests 3 ################################
########## Moderating Effect of Video Duration on Variability  #########
durmodel_like <- glm.nb(like_count ~ followed_by_count 
                        +VEV_happiness*video_duration
                        +VEV_surprise+VEV_anger+VEV_fear+VEV_sadness 
                        +TEV_happiness+TEV_surprise+TEV_anger+TEV_fear+TEV_sadness
                        +overallpeak_happiness_mag+overallpeak_happiness_dur
                        +LSE_happiness
                        +has_audio+location.has_public_page
                        +viewer_can_reshare
                        ,data = pdf2)
  

summary(durmodel_like)





############################### Tests 4 ###################################
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
                     +has_audio+location.has_public_page
                     +viewer_can_reshare
                     ,data = pdf2)

Qmodel_like <- glm.nb(like_count ~ followed_by_count #time_varying variables. You will only get within-entity estimates for these variables
                      +poly(VEV_happiness,2) #time_invariant main IVs
                     +VEV_surprise+VEV_anger+VEV_fear+VEV_sadness #time_invariant controls
                     +TEV_happiness+TEV_surprise+TEV_anger+TEV_fear+TEV_sadness
                     +overallpeak_happiness_mag+overallpeak_happiness_dur
                     +LSE_happiness
                     +has_audio+location.has_public_page
                     +viewer_can_reshare
                     ,data = pdf2)

summary(Qmodel_view)
summary(Qmodel_like)






