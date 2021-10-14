# This script takes the postcode focussed data and after plotting key variables
#makes regression models
#to investigate the relationship between car use and dog ownership  


#The process is broadly: 
#plot key variables
#look at correlation
#OLS regression
#use Moran's I to investigate spatial auto-correlation of the OLS residuals
#construct a global spatial regression (Lepage 2014 suggests The Spatial Durbin as a
#the most senible start for regional science applications)
#explore and visualise spatial-non-stationarity further using 
#Geographically Weighted Regression (GWR)




#ACKNOWLEDGMENTS

# The following tutorials are particularly helpful for those interested in learning 
# about the types of spatial statistics being applied here.  


#The spatial econometric tutorials at https://www.burkeyacademy.com/
#And the GWR tutorial at https://andrewmaclachlan.github.io/CASA0005repo/index.html 
 
#This research forms part of a UKRI ESRC funded project ES/S001743/1


#--- packages  ---
require(sf)
require(tidyverse)
require(tmap)
require(sp)
require(spdep)

#-- read data -------- 

#this is the dataset wrangled in script 01
#pcdist2 <- st_read("output_postcode_focus/postcode_focussed_data290621.gpkg")
pcdist2 <- st_read("output_postcode_focus/postcode_focussed_data270921.gpkg")




#####################################################################
##### ANALYSIS OF DOGS PP AND CAR USE AT POSTCODE DISTRICT RESOLUTION
#####################################################################

#---  dogs vs km pp --- by income quintile  

##I decided not to transform dogs_pp to log(dogs_pp) because keeping 
#the original values makes interpretation easier. 
#Using logs boosts the R squared slightly in the regression models,
# but the findings are the same: dog ownership remains a significant predictor even 
#controlling for several different variables 



require(hrbrthemes)
df <- pcdist2

#the grey error bars are 95% confidence interval 
#https://stackoverflow.com/questions/29554796/meaning-of-band-width-in-ggplot-geom-smooth-lm

#This plot is figure 1 in the paper 
df %>% filter(!is.na(meankm_pp)) %>%
  filter(!is.na(income_quintile)) %>%
  ggplot()+
  geom_point(aes(x = Dogs_pp, y = meankm_pp,colour=factor(income_quintile)))+
  geom_smooth(aes(x = Dogs_pp, y = meankm_pp),se = T)+
  facet_wrap(~income_quintile)+
  #theme_ipsum() +
  scale_color_ipsum(name = "income quintile\n(1 = lowest)")+
  xlab("Dogs per person")+
  ylab("Car km travelled per person per year")

ggsave("revisions_output/Fig1.jpg",dpi = 300)

#correlations are high
cor(df$Dogs_pp,df$meankm_pp,use= "pairwise.complete.obs",method = "pearson")
#0.55
cor(df$Dogs_pp,df$meankm_pp,use= "pairwise.complete.obs",method = "spearman")
#0.667





#---------------------- plot Figure 2 Relationship between dog ownership and rural areas (R = 0.63 Spearman)---

require(hrbrthemes)
df <- pcdist2

levels(df$ru5) <- c("Major Conurbation","Minor conurbation", "City & town",
                    "Rural town & fringe","Rural village & dispersed")


#this plot makes figure 2 in the paper 
df %>% filter(!is.na(meankm_pp)) %>%
  filter(!is.na(ru5)) %>% #filter(!is.na(Dogs_pp)) %>%
  ggplot()+
  geom_point(aes(x = Dogs_pp, y = meankm_pp,colour=factor(ru5)))+
  geom_smooth(aes(x = Dogs_pp, y = meankm_pp),se = TRUE)+
  facet_wrap(~ru5)+
  #theme_ipsum() +
  scale_color_ipsum(name = "Urbanisation\n(1 = most urban)")+
  xlab("Dogs per person")+
  ylab("Car km travelled per person per year")
ggsave("revisions_output/Fig2.jpg",dpi = 300)
#ggsave("revisions_output/Fig2_names_errorbars.png")

#correl;ations by RU5 
c <- df %>% filter(!is.na(meankm_pp)) %>%
  filter(ru5 ==1)
cor(c$Dogs_pp,c$meankm_pp,use= "pairwise.complete.obs",method = "pearson") #0.46
cor(c$Dogs_pp,c$meankm_pp,use= "pairwise.complete.obs",method = "spearman")#0.55

c <- df %>% filter(!is.na(meankm_pp)) %>%
  filter(ru5 ==2)
cor(c$Dogs_pp,c$meankm_pp,use= "pairwise.complete.obs",method = "pearson") #0.22
cor(c$Dogs_pp,c$meankm_pp,use= "pairwise.complete.obs",method = "spearman")#0.15

c <- df %>% filter(!is.na(meankm_pp)) %>%
  filter(ru5 ==3)
cor(c$Dogs_pp,c$meankm_pp,use= "pairwise.complete.obs",method = "pearson") #0.41
cor(c$Dogs_pp,c$meankm_pp,use= "pairwise.complete.obs",method = "spearman")#0.43

c <- df %>% filter(!is.na(meankm_pp)) %>%
  filter(ru5 ==4)
cor(c$Dogs_pp,c$meankm_pp,use= "pairwise.complete.obs",method = "pearson") #0.10
cor(c$Dogs_pp,c$meankm_pp,use= "pairwise.complete.obs",method = "spearman")#0.14

c <- df %>% filter(!is.na(meankm_pp)) %>%
  filter(ru5 ==5)
cor(c$Dogs_pp,c$meankm_pp,use= "pairwise.complete.obs",method = "pearson") #0.11
cor(c$Dogs_pp,c$meankm_pp,use= "pairwise.complete.obs",method = "spearman")#-0.03




################## REGRESSION ANALYSIS #########################



##############  OLS  regressions  ###################################




#-------- make a regression dataset.  
#The sf dataframe dfDE has all the variables for models D&E as well as the simpler models
# and missing values are removed 


dfDE <- pcdist2 %>% dplyr::select(PostDist,meankm_pp,Dogs_pp,income_quintile,ru5,age_median,dwel_detached_percent,
                                  dwel_semi_detached_percent,dwel_terraced_percent,flat_percent,
                                  econ_all_econ_active_percent,
                                  nocar_percent,pc_female)%>% filter(!is.na(PostDist))%>%
  filter(!is.na(meankm_pp)) %>% 
  filter(!is.na(income_quintile)) %>%
  filter(!is.na(Dogs_pp)) %>% 
  filter(!is.na(ru5))

# save a shapefile for geoda 
#st_write(dfDE,dsn = "revisions_output/dfdeshape", layer = "dfDE", driver = "ESRI Shapefile")


                    
#------- run the ols regressions 
modA_ols <- lm(data = dfDE,meankm_pp~ Dogs_pp)
#simpler covariates models 

summary(modA_ols)

## This is an interesting finding. Even without removing any outliers or transforming the predictor variable (dogs per person)  
## An R squared of 0.32  is achieved when dogs per person is the only 
## predictor of car km travelled in England and Wales, 
## and it supports the hypothesis that dog ownership is a car dependent practice. 
## the rest of the analysis builds on this.  


modB_ols <- lm(data = dfDE,meankm_pp~ Dogs_pp + income_quintile + ru5)
modC_ols <- lm(data = dfDE,meankm_pp~ income_quintile + ru5)


#The kent & Mulley variables plus the covariates from model B&C
modD_ols <- lm(data = dfDE,meankm_pp~ Dogs_pp+  income_quintile +ru5 +  age_median + pc_female + dwel_detached_percent +
                 dwel_semi_detached_percent + dwel_terraced_percent + flat_percent+
                 econ_all_econ_active_percent+
                 nocar_percent
)

modE_ols <- lm(data = dfDE,meankm_pp~ income_quintile +ru5 +  age_median + pc_female + dwel_detached_percent +
                 dwel_semi_detached_percent + dwel_terraced_percent + flat_percent+
                 econ_all_econ_active_percent+
                 nocar_percent
)



require(stargazer) # package for tidy regression results 
sg_ols <- stargazer(modA_ols, modB_ols,modC_ols,modD_ols,modE_ols,type = "html" ,title = "TITLE:OLS Regression results")
stargazer(modA_ols, modB_ols,modC_ols,modD_ols,modE_ols,type = "html" ,title = "TITLE:OLS Regression results")
sg_ols

#We need to make the table twice 

#this give 0 decial places which is best for coeffs
stargazer(modA_ols, modB_ols,modC_ols,modD_ols,modE_ols,
          type = "html" ,title = "OLS Regression results",
          digits = 0,
          out = "revisions_output/OLS2.doc"
)

#this give 4 decimal places which is best for reporting adj r squared 
stargazer(modA_ols, modB_ols,modC_ols,modD_ols,modE_ols,
          type = "html" ,title = "OLS Regression results",
          digits = 4,
          out = "revisions_output/OLS2_4dp.doc"
)


###### SPATIAL AUTOCORRELATION #######################

#Check the residuals of the ols models  for spatial autocorrelation ----------


####  examine results
dfDE2 <- dfDE
st_geometry(dfDE2)<- NULL
dfDE2 <- dfDE2  %>% dplyr::filter(!is.na(meankm_pp)) %>% 
  filter(!is.na(income_quintile)) %>%
  filter(!is.na(ru5))

#append the residuals 
dfDE2$residsA <- modA_ols$residuals
dfDE2$residsB <- modB_ols$residuals
dfDE2$residsC <- modC_ols$residuals
dfDE2$residsD <- modD_ols$residuals
dfDE2$residsE <- modE_ols$residuals



#-------------------- map the residuals --------------------------- 

dfDE2 <- dfDE2 %>% dplyr::select(PostDist,residsA,residsB,residsC,residsD,residsE)
dfDE <- dplyr::left_join(dfDE,dfDE2,by = c("PostDist"="PostDist"))

#uncomment to run these blocks of code to map the residuals, they run slowly
# tmap_mode("plot")
# 
# rA <- tm_shape(dfDE)+
#   tm_polygons(col = "residsA", n = 5, palette = "RdBu",border.lwd = 0,
#               border.alpha = 0)
# 
# rB <- tm_shape(dfDE)+
#   tm_polygons(col = "residsB", n = 5, palette = "RdBu",border.lwd = 0,
#               border.alpha = 0)
# 
# rC <- tm_shape(dfDE)+
#   tm_polygons(col = "residsC", n = 5, palette = "RdBu",border.lwd = 0,
#               border.alpha = 0)
# 
# 
# rD <- tm_shape(dfDE)+
#   tm_polygons(col = "residsD", n = 5, palette = "RdBu",border.lwd = 0,
#               border.alpha = 0)
# 
# 
# rE <- tm_shape(dfDE)+
#   tm_polygons(col = "residsE", n = 5, palette = "RdBu",border.lwd = 0,
#               border.alpha = 0)
# 
# 
# #print the residuals maps as small multiples
# tmap_arrange(rA,rB,rC,rD,rE,ncol = 3)

#--------- Morans tests ------------------------------ 

# I'm using knn on pcdist2 centroids
#because of generalisation there are some polygons which may not seem contiguous which actually are
#With the weights matrix we run a global Morans I test on the old residuals
#Where there is spatial autocorrelation of the ols residuals we then run a spatial error model



#---------- knn weights used to calculate Morans I ------ 
require(sp) #so far I've used sf to hold spatial data. Some spatial statistics packages use sp
require(spdep) # spatial dependence weighting schemes for Morans measures of spatial autocorrelation

coords <- st_centroid(dfDE)
temp <- as.data.frame(st_coordinates(coords))
coords$x = temp$X
coords$y = temp$Y

coordscsv <- coords
st_geometry(coordscsv) <- NULL
write_csv(coordscsv,"revisions_output/coords.csv")


knn <- 4 # 4 is same number of neighbours as rook contiguity & gives similar results to knn = 5 and 10)
nb.test <- knn2nb(knearneigh(coords, k=knn), row.names=coords$PostDist)
#turns matrix into a symetric matrix
nb <- make.sym.nb(nb.test)
Wknn <- nb2mat(nb, style = "B")
Wknn.list <- nb2listw(neighbours = nb, style = "B")

# 
lm.morantest(modA_ols,Wknn.list) # 0.4943026189  p-value < 2.2e-16
lm.morantest(modB_ols,Wknn.list) #0.5184799717  p-value < 2.2e-16
lm.morantest(modC_ols,Wknn.list) # 0.5244731875 p-value < 2.2e-16
lm.morantest(modD_ols,Wknn.list) #  0.2962059203 p-value < 2.2e-16
lm.morantest(modE_ols,Wknn.list) # 0.2901657011 sig  2.2e-16 # still significant but less strong than the simple covariates model.  


#------ we can also run local Morans I to see where autocorrelation is statistically significant -----


#these blocks run the local morans for each ols model 
#and tidy the result
LMI_A <- localmoran(x = coords$residsA, listw = Wknn.list)
LMI_A <- as.data.frame(LMI_A) %>% 
  select(LI_A = Ii, pr_LI_A = `Pr(z > 0)`)

LMI_B <- localmoran(x = coords$residsB, listw = Wknn.list)
LMI_B <- as.data.frame(LMI_B) %>% 
  select(LI_B = Ii, pr_LI_B = `Pr(z > 0)`)

LMI_C <- localmoran(x = coords$residsC, listw = Wknn.list)
LMI_C <- as.data.frame(LMI_C) %>% 
  select(LI_C = Ii, pr_LI_C = `Pr(z > 0)`)

LMI_D <- localmoran(x = coords$residsD, listw = Wknn.list)
LMI_D <- as.data.frame(LMI_D) %>% 
  select(LI_D = Ii, pr_LI_D = `Pr(z > 0)`)

LMI_E <- localmoran(x = coords$residsE, listw = Wknn.list)
LMI_E <- as.data.frame(LMI_E) %>% 
  select(LI_E = Ii, pr_LI_E = `Pr(z > 0)`)

#bind the local morans results onto coords for mapping
LMImap <- cbind(coords, as.data.frame(LMI_A))
LMImap <- cbind(LMImap, as.data.frame(LMI_B))
LMImap <- cbind(LMImap, as.data.frame(LMI_C))
LMImap <- cbind(LMImap, as.data.frame(LMI_D))
LMImap <- cbind(LMImap, as.data.frame(LMI_E))

glimpse(LMImap)

#map where the residuals are significant to 
#help understand the pattern of where autocorrelation happens 
#tmap_mode("view")


lmiA <- LMImap %>% filter(pr_LI_A < 0.05) %>%
  tm_shape()+
  tm_dots(col = "LI_A", pal = "-RdBu",midpoint = 0)

#modA there is high significant positive residual, occurring most often in connurbations.  
#this means that the the actual value of km_pp was MORE than the predicted value suggested by dog ownership levels 
#e.g. in connurbations, owning a dog more often increases car use more than in rural areas.  
# There were some positive significant residuals outside connurbations too.  

#tmap_mode("view")
lmiB <- LMImap %>% filter(pr_LI_B < 0.05) %>%
  tm_shape()+
  tm_dots(col = "LI_B", pal = "-RdBu",midpoint = 0)

#In model B, significant positive residuals are again most often found
#in connurbations, but, there are also pockets in west cumbria towns,
#the Isle of white and the East of England 


lmiC <- LMImap %>% filter(pr_LI_C < 0.05) %>%
  tm_shape()+
  tm_dots(col = "LI_C", pal = "-RdBu",midpoint = 0)


#tmap_mode("view")
lmiD <- LMImap %>% filter(pr_LI_D < 0.05) %>%
  tm_shape()+
  tm_dots(col = "LI_D", pal = "-RdBu",midpoint = 0)

#In model D, with the addition of many more co-variates, the pattern is less obvious
#  There are still groupings of higher significand residuals
#post code outcode areas in west cumbria, the Isle of White and Outer London 
#(but acknowledge visual bias because these places have more pcdists as they are more populous)


#tmap_mode("view")
lmiE <- LMImap %>% filter(pr_LI_E < 0.05) %>%
  tm_shape()+
  tm_dots(col = "LI_E", pal = "-RdBu",midpoint = 0)


#Just a basic map not so great as there's no UK border or backdrop map. 
tmap_arrange(lmiA,lmiB,lmiC,lmiD,lmiE,ncol = 3)




############ SPATIAL REGRESSION ###################################### 


#------------------- run spatial regression  ----------- 


require(spatialreg) # nb this can cause conflict with dplyr filter()


#the Burkey spatial regression tutorial (https://www.youtube.com/watch?v=MbQ4s8lwqGI)
#and associated cheatsheet  points us at the paper by Lepage 2014
# What the paper says is that if you think you need a spatial model then 
# start with a Spatial Durbin Model.  

#LeSage, James P., 
#What Regional Scientists Need to Know About Spatial Econometrics 
#(January 5, 2014). 
#Available at SSRN: https://ssrn.com/abstract=2420725 or 
#http://dx.doi.org/10.2139/ssrn.2420725

#Spatial Durbin Model considers Both spatially endogenous interactions and spatial interactions in the error
#term as well as exogenous interactions: 

#[Burkey Academy tutorial videos spatial regression in R 
#https://www.youtube.com/watch?v=b3HtV2Mhmvk 
#]
#https://www.youtube.com/watch?v=MbQ4s8lwqGI




#------run Spatial Durbin models for models A-E  ----------------- 

modA_SDM <- lagsarlm(formula = meankm_pp ~  Dogs_pp,
                     data = coords, Wknn.list, type = "mixed"
)


modB_SDM <- lagsarlm(formula = meankm_pp ~  Dogs_pp +  income_quintile +ru5 ,
                     data = coords, Wknn.list, type = "mixed"
)

modC_SDM <- lagsarlm(formula = meankm_pp ~  income_quintile +ru5 ,
                     data = coords, Wknn.list, type = "mixed"
)

modD_SDM <- lagsarlm(formula = meankm_pp ~  Dogs_pp + income_quintile +ru5 +  age_median + pc_female + dwel_detached_percent +
                       dwel_semi_detached_percent + dwel_terraced_percent + flat_percent+
                       econ_all_econ_active_percent+
                       nocar_percent,data = coords, Wknn.list, type = "mixed"
)

modE_SDM <- lagsarlm(formula = meankm_pp~ income_quintile +ru5 +  age_median + pc_female + dwel_detached_percent +
                       dwel_semi_detached_percent + dwel_terraced_percent + flat_percent+
                       econ_all_econ_active_percent+
                       nocar_percent,data = coords, Wknn.list, type = "mixed")



require(stargazer)
stargazer(modA_SDM,modB_SDM,modC_SDM,modD_SDM,modE_SDM,
          type = "html" ,title = "TITLE: Spatial Durbin Model Regression results",
          digits = 0,
          out = "revisions_output/sdm2.doc"
          )
#stargazer shows that the models with dogs (B & D ) have lower AIC than their comparitor without (C&E)

sg_SDM <- stargazer(modA_SDM,modB_SDM,modC_SDM,modD_SDM,modE_SDM,type = "text" ,title = "TITLE: Spatial Durbin Model Regression results")

stargazer(modA_SDM,modB_SDM,modC_SDM,modD_SDM,modE_SDM,type = "text" ,title = "TITLE: Spatial Durbin Model Regression results")


#this summary will give the AIC of the SEM and also the AIC of the OLS.  
#AIC is a useful way to compare models 
#R2 increases if you add more covariates all else being equal 
#AIC is a way to compare the improvement in model fit which penalises increases in variables.  
#AIC is only useful for comparison between models its an index of fit ratehr than a measure of it.  
#If AIC is bigger the fit is worse, if AIC is smaller the fit is better.  
#See Field A 2013 Discovering statistics 4th Edition P324 section 8.5.2 for a nice explanation


#require(broom)
# tidy(modA_SDM)
# write_csv(tidy(modA_SDM),"revisions_output/modA_SDM.csv")
# write_csv(tidy(modB_SDM),"revisions_output/modB_SDM.csv")
# write_csv(tidy(modC_SDM),"revisions_output/modC_SDM.csv")
# write_csv(tidy(modD_SDM),"revisions_output/modD_SDM.csv")
# write_csv(tidy(modE_SDM),"revisions_output/modE_SDM.csv")
# 

# ----------Check are the residuals of the SDM autocorrelated?  ------
coords$modA_SDM_resid <- modA_SDM$residuals
moran.mc(x = coords$modA_SDM_resid, listw = Wknn.list, nsim = 10000)
# statistic = -0.017136, observed rank = 1112, p-value = 0.8888

coords$modB_SDM_resid <- modB_SDM$residuals
moran.mc(x = coords$modB_SDM_resid, listw = Wknn.list, nsim = 10000)
#statistic = -0.020236, observed rank = 759, p-value = 0.9241

coords$modC_SDM_resid <- modC_SDM$residuals
moran.mc(x = coords$modC_SDM_resid, listw = Wknn.list, nsim = 10000)
#statistic = -0.017163, observed rank = 1129, p-value = 0.8871

coords$modD_SDM_resid <- modD_SDM$residuals
moran.mc(x = coords$modD_SDM_resid, listw = Wknn.list, nsim = 10000)
#statistic = -0.0073043, observed rank = 3200, p-value = 0.68

coords$modE_SDM_resid <- modE_SDM$residuals
moran.mc(x = coords$modE_SDM_resid, listw = Wknn.list, nsim = 10000)
#statistic = -0.0053695, observed rank = 3690, p-value = 0.631


# for all 5 SEM models we see the autocorrelation of residuals are not statistically significant



#IN SUMMARY 
#The spatial Durbin Model deals with spatial autocorelation AND the AIC improves relative to OLS




########### Geographically Weighted Regression (GWR) #######################

#-------------- Non stationarity of process and GWR ----------- 

#see this for excellent explanations and tutorial 
#https://andrewmaclachlan.github.io/CASA0005repo/gwr-and-spatially-lagged-regression.html


#Spatial durbin & other spatial econometric models assume that 
# there is global process - the relationship between variables is the same 
#across the country. 

#GWR may be useful for examining ‘non-stationarity’ 
#"this is when the global model does 
#not represent the relationships between variables that might vary locally".



require(spgwr)

coords_sp <- as(coords,'Spatial')


#------- build GWR for the models with dogs ------ 
#each of these takes quite a long time to run 

#modelA
bandwidth_modA <- gwr.sel(
  formula = meankm_pp ~ Dogs_pp,
  data = coords_sp,adapt = T
)
gwr_modA <- gwr(formula = meankm_pp ~ Dogs_pp + income_quintile + ru5,
                data = coords_sp,
                adapt = bandwidth_modA,
                se.fit=T,
                hatmatrix =T )


gwr_modA
gwr_modA_results <- as.data.frame(gwr_modA$SDF)

#modelB
bandwidth_modB <- gwr.sel(
  formula = meankm_pp ~ Dogs_pp + income_quintile + ru5,
  data = coords_sp,
  method = "aic",
  #adapt = FALSE
  )
gwr_modB <- gwr(formula = meankm_pp ~ Dogs_pp + income_quintile + ru5,
                data = coords_sp, 
                bandwidth = bandwidth_modB,
                #adapt =  bandwidth_modB,
                #se.fit=T,
                hatmatrix = T)
                

gwr_modB
gwr_modB_results <- as.data.frame(gwr_modB$SDF)


#modelD
bandwidth_modD <- gwr.sel(
  formula = meankm_pp ~ Dogs_pp + income_quintile + ru5 +  age_median + pc_female + dwel_detached_percent +
    dwel_semi_detached_percent + dwel_terraced_percent + flat_percent+
    econ_all_econ_active_percent+
    nocar_percent,
  data = coords_sp
  #,adapt = TRUE
)
gwr_modD <- gwr(
  formula = meankm_pp ~ Dogs_pp + income_quintile + ru5 +  age_median + pc_female + dwel_detached_percent +
                  dwel_semi_detached_percent + dwel_terraced_percent + flat_percent +
                  econ_all_econ_active_percent + nocar_percent,
                data = coords_sp,
                bandwidth = 50000,
                #adapt = bandwidth_modD,
                se.fit=T,
                hatmatrix =T )


summary(gwr_modD)
gwr_modD
gwr_modD_results <- as.data.frame(gwr_modD$SDF)

#----------- prepare GWR results for mapping ------------  

gwrmaps <- dfDE 


#--- for each model make a column for the coefficient for dogs pp at each location
gwrmaps$modAcoeffDogsPP <- gwr_modA_results$Dogs_pp
gwrmaps$modA_localR2 <- gwr_modA_results$localR2
summary(gwr_modA_results$localR2)

gwrmaps$modBcoeffDogsPP <- gwr_modB_results$Dogs_pp
gwrmaps$modBcoeffDogsPP_se <- gwr_modB_results$Dogs_pp_se
gwrmaps$modB_localR2 <- gwr_modB_results$localR2

gwrmaps$modDcoeffDogsPP <- gwr_modD_results$Dogs_pp
gwrmaps$modD_localR2 <- gwr_modD_results$localR2
gwrmaps$modDcoeffDogsPP_se <- gwr_modD_results$Dogs_pp_se


#------- Map the dogs pp coefficeint for model D --------

#This is the most relevant map.  We've accounted for coavriates in the global models
#and dogs_pp is the predictor variable wer'e most interested in.  

#run with fisher to get natural breaks
gwrcoeffD <-tm_shape(gwrmaps)+
  tm_polygons(col = "modDcoeffDogsPP", n = 5, style = "fisher",
              palette = "-RdBu",
              border.lwd = 0,
              border.alpha = 0)+
  tm_layout(bg.color = 'lightgrey')

gwrcoeffD 
tmap_save(gwrcoeffD,filename = "revisions_output/GWR_ModD_dog_coeff_hires.jpg",dpi = 300,
          width = 21,height = 29.7, units = "cm")

#good to see what it looks like with another classification too.  
# gwrcoeffD <-tm_shape(gwrmaps)+
#   tm_polygons(col = "modDcoeffDogsPP", n = 5, style = "quantile",
#               palette = "-RdBu",
#               border.lwd = 0,
#               border.alpha = 0)+
#   tm_layout(bg.color = 'lightgrey')
# 
# 
# 
# gwrcoeffD 




#--------- other models can be plotted too ------------ 

names(gwrmaps)
tmap_mode("plot")
gwrcoeffA <- tm_shape(gwrmaps)+
  tm_polygons(col = "modAcoeffDogsPP", n = 5, palette = "-RdBu",border.lwd = 0,
              border.alpha = 0)

gwrcoeffA
gwrcoeffB <-tm_shape(gwrmaps)+
  tm_polygons(col = "modBcoeffDogsPP", n = 5, palette = "-RdBu",border.lwd = 0,
              border.alpha = 0)+
  tm_layout(bg.color = 'lightgrey')

gwrcoeffB 

gwrcoeffBse <-tm_shape(gwrmaps)+
  tm_polygons(col = "modBcoeffDogsPP_se", n = 5, palette = "-RdBu",border.lwd = 0,
              border.alpha = 0)


gwrcoeffD <-tm_shape(gwrmaps)+
  tm_polygons(col = "modDcoeffDogsPP", n = 5, style = "quantile",
              palette = "-RdBu",
              border.lwd = 0,
              border.alpha = 0)+
  tm_layout(bg.color = 'lightgrey')

gwrcoeffD 

gwrcoeffDse <-tm_shape(gwrmaps)+
  tm_polygons(col = "modDcoeffDogsPP_se", n = 5,style = "quantile",
              palette = "-RdBu",border.lwd = 0,
              border.alpha = 0)+
  tm_layout(bg.color = 'lightgrey')



gwrR2A <- tm_shape(gwrmaps)+
  tm_polygons(col = "modA_localR2", n = 5, border.lwd = 0,
              border.alpha = 0)

gwrR2B <- tm_shape(gwrmaps)+
  tm_polygons(col = "modB_localR2", n = 5,border.lwd = 0,
              border.alpha = 0)

gwrR2D <- tm_shape(gwrmaps)+
  tm_polygons(col = "modD_localR2", n = 5,border.lwd = 0,
              border.alpha = 0)








