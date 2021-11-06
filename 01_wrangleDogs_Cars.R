
### This R script wrangles the data 
### Plots and regression analysis is done in script 02.  


#---------- read data ---------------- 


library(tidyverse)
library(tmap)
library(sf)

#--------------read datasets to be wrangled ------------------------
#dog data 
dogs <- read_csv("data/DFEFAdogdata.csv")
postcode_oa <- read_csv("data/OA_postcode.csv")
lookup <- read_csv("data/OA11_LSOA11_MSOA11_LAD11_EW_LUv2.csv")

postcodepop <- read_csv("population_postcodes/Postcode_Estimates_Table_1.csv")
sum(postcodepop$Total)

#This is a set of various variables including rural urban classification
clusterdata <- read_csv("MOTdata/LSOAamasterEcc030418subset.csv")
clusterdata <- clusterdata %>% dplyr::select(LSOA11CD,income,green900,RUC11,ru5)
#This is the % of families with 1 or more dependent children 
#from the 2011 census table QS118ew
child <- read_csv("data/QS118ew_dep_child.csv")

#MOT data (version 9) Data from the MOT project 
#see ref to Cairns et al 2014 in paper table 1 
mot <- read_csv("data/v9carsAndPop.csv")


#spatial data 
#lsoa polygons
lsoa <- st_read(dsn = "LSOA11SG", layer = "LSOAEW11SG")


#English and Welsh LSOA population weighted centroids
pwc <- st_read("data/dist_to_stations.gpkg") # pop weighted cents with rail access info/  
pwc <- pwc %>% dplyr::select(code,name,dist_to_nearest_station)



#geodemographic data 
income <- read_csv("data/ExperianMedianHHIncome2011.csv")
census <- read_csv("data/selected_UKcensus2011.csv")
census <- census %>% dplyr::select(LSOA11CD,nocar_percent,
                            car1_percent,
                            car2_percent,
                            car3_percent,
                            car4_or_more_percent,
                            age_median,
                            white_percent,
                            dwel_detached_percent,
                            dwel_semi_detached_percent,
                            dwel_terraced_percent,
                            flat_percent,
                            econ_all_econ_active_percent)


female <- read_csv("data/urp_pc_female.csv")
female <- female %>% dplyr::select(llsoa11cd,pc_female)

census <- left_join(census,female,by = c("LSOA11CD" = "llsoa11cd" ))


#vul index I didn't use this in the analysis in the end 
vul <- read_csv("data/Mattiolietal.csv")


#postcode districts 
pcdist <- st_read("GB_Postcodes/PostalDistrict.shp")
#qtm(pcdist)

summary(census$pc_female)

##### wrangling data ###################

#We have data at 2 spatial resolutions, dog data at postcode district resolution which are larger areas
#than lsoas which is the resolution of the MOT data.  
#WE can aggregate to postcode districts: this smooths out some variation in the mot vehicle use data
#or we can attribute the dog ownership level to each LSOA.  This preserves variation in MOT vehicle use data,
#but there can be some ecological fallacy issues.  




#----------- wrangle the dogs data so the spatial resolution is postcode outcode--------

#wrangle to postcodes to outcode (e.g. LS2 also known as the postcode district).  
#Outcode and Postcode district are the same thing.  #
#generate a human population estimate for the postcode districts, by joining data to the dogs data 
#spatial join the postcode polygon attributes to points 
#join the dog data.  


#------------------ join postcode population to postcode district ----

#first aggregate postcode population to postcode district population 
postcode_split2 <- postcodepop %>%
  extract(Postcode, into = c('out', 'in'), '(\\S{2,4})\\s*(\\d\\w\\w)') %>%
  mutate(Postcode = sprintf('% -4s%s', out, `in`))

names(postcodepop)
names(postcode_split2)

#group 
groups <- group_by(postcode_split2,out)
#summarize
postcodedistpop <- dplyr::summarize(groups, totalhumanpop = sum(Total))
sum(postcodedistpop$totalhumanpop)


#This gives a population per postcode district.  Next we can 
#make a postcode area variable of dogs per person.  
#this will eventually get compared to the MOT miles per person variable. 

dogs <- left_join(dogs,postcodedistpop,by = c("PostcodeDistrict" = "out"))
names(dogs)
sum(dogs$totalhumanpop,na.rm=T)
sum(dogs$EstimatedDogPopulation,na.rm=T)

dogs$Dogs_pp <- dogs$EstimatedDogPopulation / dogs$totalhumanpop
summary(dogs$Dogs_pp)
summary(dogs$totalhumanpop)

plot(dogs$totalhumanpop,dogs$EstimatedDogPopulation,main = "Dog population vs Human Population")

cor(dogs$totalhumanpop,dogs$EstimatedDogPopulation,use= "pairwise.complete.obs",method = "pearson")
#0.437772
cor(dogs$totalhumanpop,dogs$EstimatedDogPopulation,use= "pairwise.complete.obs",method = "spearman")
#0.5213059
#there is a moderately strong correlation, th plot shows a cone.  
#There is likely another factor / factors which might provide a stronger 
#relationship with multiple regression 


names(dogs)
#[1] "PostcodeDistrict"       "EstimatedDogPopulation" "totalhumanpop"          "Dogs_pp"  

n = dogs %>% dplyr::select(PostcodeDistrict) %>% distinct()
duplicates = dogs %>% 
  group_by(PostcodeDistrict) %>% 
  filter(n()>1)



#------------- process LSOA data ------------------ 

#you can uncomment this and make a map if you want but it takes a while to render
# tm_shape(pcdist)+
#   tm_polygons(alpha = 0.5)+
#   tm_shape(pwc)+
#   tm_dots(col = "red")

#------------ dplyr::select columns required from the mot data
names(mot)
mot2 <- mot %>% dplyr::select(LSOA,Pop,HH,N,km_pp,km_perAdult,emfac_co2Av,litresAv,sizeAv)


pwc <- left_join(pwc,mot2,by = c("code" = "LSOA"))
pwc <- pwc %>% filter(!is.na(km_pp)) # removes scottish lsoa/dz as we only have dogs data for England and Wales 


pwc<- left_join(pwc,clusterdata,by = c("code" = "LSOA11CD"))
pwc<- left_join(pwc,income,by = c("code" = "LSOA11CD"))
#make an income quintile
pwc <- pwc %>% mutate(income_quintile = ntile(Median_Household_Income, 5))

#remove outlier lsoas where mean kmpp is over 20,000 pa 
filtered <- pwc %>% filter(km_pp > 20000)
pwc <- pwc %>% filter(km_pp < 20000)


#join dplyr::selected census variables 
pwc<- left_join(pwc,census,by = c("code" = "LSOA11CD"))
pwc$multicarpc <- pwc$car4_or_more_percent + pwc$car3_percent +pwc$car2_percent


names(child)
pwc <- left_join(pwc,child, by = c("code" = "lsoa11cd"))


#join vul index
pwc <- left_join(pwc,vul, by = c("code" = "LSOA11CD"))

#######---------- spatial joining -------########


#----- make data at postcode district resolution ---------
st_crs(pwc)
pcdist <- st_transform(pcdist,crs = 27700)

names(pwc)

pcdist2 <- st_join(pcdist, pwc %>% filter(!is.na(ru5))) %>% group_by(PostDist) %>% 
  summarise(meankm_pp = mean(km_pp,na.rm = T),
            meankm_per_adult = mean(km_perAdult,na.rm = T),
            mean_emfac_co2Av = mean(emfac_co2Av,na.rm = T),
            mean_engine_size = mean(sizeAv,na.rm = T),
            mean_fuelpp_l = mean(litresAv,na.rm = T),
            tot_pop_mot = sum(Pop,na.rm = T),
            num_cars = sum(N,na.rm = T),
            HH_mot = sum(HH,na.rm = T),
            dist_to_nearest_station = mean(dist_to_nearest_station,na.rm = T),
            median_hh_income = mean(Median_Household_Income,na.rm = T),
            ru5 = min(ru5,na.rm = T),
            nocar_percent = mean(nocar_percent,na.rm = T),
            multicarpc = mean(multicarpc,na.rm = T),
            age_median = mean(age_median,na.rm = T),
            white_percent = mean(white_percent,na.rm = T),
            dwel_detached_percent = mean(dwel_detached_percent,na.rm = T),
            dwel_semi_detached_percent = mean(dwel_semi_detached_percent,na.rm = T),
            dwel_terraced_percent = mean(dwel_terraced_percent,na.rm = T),
            flat_percent = mean(flat_percent,na.rm =T),
            econ_all_econ_active_percent = mean(econ_all_econ_active_percent,na.rm =T),
            vul = mean(vul,na.rm = T),
            pc_female = mean(pc_female,na.rm = T),
            pc_child = mean(pc_families_dep_children)
            )

#---remove scotland unfortunately because *data* is a devolved issue and England and Scotland publish data separately --
#scotland postcodes will have na for km _pp because the MOT data I read in is for England and Wales
pcdist2 <- pcdist2 %>% filter(!is.na(meankm_pp))

#---  deal with RU5 
summary(pwc$ru5)
summary(pcdist2$ru5)


test <- pcdist2 %>% filter(is.na(ru5)) 

pcdist2$ru5 <- as.factor(pcdist2$ru5)

pcdist2 <- pcdist2 %>% mutate(income_quintile = ntile(median_hh_income, 5))
summary(pcdist2$income_quintile)

#--------- check for duplicates ---- 
duplicates = pcdist2 %>% 
  group_by(PostDist) %>% 
  filter(n()>1) #no duplicates found


#note outliers of a mean of over 20000km per person pa per lsoa have been removed
#remove 9 outlier postcode districts #done above
#filtered <- pcdist2 %>% filter(meankm_pp > 20000) #done above

hist(pcdist2$meankm_pp)

#map the mot data mean km per person aggregated to postcode districts
#tm_shape(pcdist2)+
#  tm_polygons(col = "meankm_pp",n=5,style = "quantile",
#              alpha = 0.5)

glimpse(pcdist2)


#-----join to the dogs data ------
pcdist2 <- left_join(pcdist2,dogs,by = c("PostDist" = "PostcodeDistrict" ))

#there are no missing values for dog population 
pcdist2 %>% filter(is.na(EstimatedDogPopulation))

#make the dogs per person metrics with both population figures
pcdist2$Dogs_ppmot <- pcdist2$EstimatedDogPopulation/pcdist2$tot_pop_mot
pcdist2$Dogs_pppcd <- pcdist2$EstimatedDogPopulation/pcdist2$totalhumanpop

#base r plot
plot(pcdist2$Dogs_ppmot,pcdist2$Dogs_pppcd)
#ggplot
pcdist2 %>% 
  ggplot()+
  geom_point(aes(x = Dogs_ppmot, y = Dogs_pppcd))+
  geom_smooth(aes(x = Dogs_ppmot, y = Dogs_pppcd))


cor(pcdist2$Dogs_ppmot,pcdist2$Dogs_pppcd,use= "pairwise.complete.obs",method = "pearson")
#0.96
cor(pcdist2$Dogs_ppmot,pcdist2$Dogs_pppcd,use= "pairwise.complete.obs",method = "spearman")
#0.99

#dissimilarity increases with high values -  places 
#with more than 1 dog per person mot and more than 1.5 pcd
summary(pcdist2$Dogs_ppmot)
summary(pcdist2$Dogs_pppcd)

#---- human population estimates seem consistent
#we have an estimate of postcode population, and also census population from LSOAs.  
#when lsoa data is aggregated to postcode level it seems consistent 
#correlation between human population estimates is very high and linear
pcdist2 %>% 
  ggplot()+
  geom_point(aes(x = tot_pop_mot, y = totalhumanpop))+
  geom_smooth(aes(x = tot_pop_mot, y = totalhumanpop))


#4 postcode districts have more than 1.5 dogs per person, 
#DL11, Upper Swaledale 
#LA20 duddon eskdale and wasdale,
#LL23, Bala N Wales 
#and 
#PO41 Isle of white Yarmouth 
topdogs <- filter(pcdist2, Dogs_ppmot >1.5)



#use dogs postcode district in the analysis 
pcdist2$Dogs_pp <- pcdist2$Dogs_pppcd

#------ map the dogs data --------------
# #tm_shape(pcdist2)+
# #  tm_polygons(col = "Dogs_pp",n=5,style = "quantile",
# #              alpha = 0.5)
# 
# tm_shape(pcdist2)+
#   tm_polygons(col = "Dogs_pp",n=5,style = "fisher",
#               alpha = 0.5)


pcdist2$cars_pp_pcdist <- pcdist2$num_cars/pcdist2$totalhumanpop


#make a set of dummy variables for RU5 
pcdist2$MajorConurbation <- 0
pcdist2$MajorConurbation[pcdist2$ru5 == 1] <- 1

pcdist2$MinorConurbation <- 0
pcdist2$MinorConurbation[pcdist2$ru5 == 2] <- 1

pcdist2$CityTown <- 0
pcdist2$CityTown[pcdist2$ru5 == 3] <- 1


pcdist2$RuralTownFringe <- 0
pcdist2$RuralTownFringe[pcdist2$ru5 == 4] <- 1

pcdist2$RuralVillageDispersed <- 0
pcdist2$RuralVillageDispersed[pcdist2$ru5 == 5] <- 1

glimpse(pcdist2)
#---------save key datasets -----------------

#if you want to rewrite these then un-comment here
#st_write(pcdist2,"output_postcode_focus/postcode_focussed_data281021.gpkg")


pcdist2csv <- pcdist2
st_geometry(pcdist2csv) <- NULL
#write_csv(pcdist2csv,"output_postcode_focus/postcode_focussed_data281021.csv")








