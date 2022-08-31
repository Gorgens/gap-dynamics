require(raster)
require(rgdal)
require(tidyverse)


### Chi-squared test

ducke1 = shapefile('./vector-based/data/ducke_2017gapsClean.shp')
IC = 100
total = dim(ducke1@data)[1]
resume = ducke1@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/IC)*IC+(IC/2)) %>%
  mutate(site = "Ducke") %>%
  mutate(year = 2017) %>%
  select(-c(ano17))

ducke2 = shapefile('./vector-based/data/ducke_2020gapsClean.shp')
total = dim(ducke2@data)[1]
temp = ducke2@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/IC)*IC+(IC/2)) %>%
  mutate(site = "Ducke") %>%
  mutate(year = 2020) %>%
  select(-c(ano20))
resume = rbind(resume, temp)

tanguro1 = shapefile('./vector-based/data/tanguro_2018gapsClean.shp')
total = dim(tanguro1@data)[1]
temp = tanguro1@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/IC)*IC+(IC/2)) %>%
  mutate(site = "Tanguro") %>%
  mutate(year = 2018) %>%
  select(-c(ano17))
resume = rbind(resume, temp) 

tanguro2 = shapefile('./vector-based/data/tanguro_2020gapsClean.shp')
total = dim(tanguro2@data)[1]
temp = tanguro2@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/IC)*IC+(IC/2)) %>%
  mutate(site = "Tanguro") %>%
  mutate(year = 2020) %>%
  select(-c(ano20))
resume = rbind(resume, temp)

tapajos1 = shapefile('./vector-based/data/tapajos_2017gapsClean.shp')
total = dim(tapajos1@data)[1]
temp = tapajos1@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/IC)*IC+(IC/2)) %>%
  mutate(site = "Tapajos") %>%
  mutate(year = 2017) %>%
  select(-c(ano17))
resume = rbind(resume, temp) 

tapajos2 = shapefile('./vector-based/data/tapajos_2020gapsClean.shp')
total = dim(tapajos2@data)[1]
temp = tapajos2@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/IC)*IC+(IC/2)) %>%
  mutate(site = "Tapajos") %>%
  mutate(year = 2020) %>%
  select(-c(ano20))
resume = rbind(resume, temp)

jariA1 = shapefile('./vector-based/data/jari_2017agapsClean.shp')
total = dim(jariA1@data)[1]
temp = jariA1@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/IC)*IC+(IC/2)) %>%
  mutate(site = "Jari A") %>%
  mutate(year = 2017) %>%
  select(-c(ano17))
resume = rbind(resume, temp)

jariA2 = shapefile('./vector-based/data/jari_2020agapsClean.shp')
total = dim(jariA2@data)[1]
temp = jariA2@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/IC)*IC+(IC/2)) %>%
  mutate(site = "Jari A") %>%
  mutate(year = 2020) %>%
  select(-c(ano20))
resume = rbind(resume, temp)

jariB1 = shapefile('./vector-based/data/jari_2017bgapsClean.shp')
total = dim(jariB1@data)[1]
temp = jariB1@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/IC)*IC+(IC/2)) %>%
  mutate(site = "Jari B") %>%
  mutate(year = 2017) %>%
  select(-c(ano17))
resume = rbind(resume, temp)

jariB2 = shapefile('./vector-based/data/jari_2020bgapsClean.shp')
total = dim(jariB2@data)[1]
temp = jariB2@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/IC)*IC+(IC/2)) %>%
  mutate(site = "Jari B") %>%
  mutate(year = 2020) %>%
  select(-c(ano20))
resume = rbind(resume, temp)

jariC1 = shapefile('./vector-based/data/jari_2017cgapsClean.shp')
total = dim(jariC1@data)[1]
temp = jariC1@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/IC)*IC+(IC/2)) %>%
  mutate(site = "Jari C") %>%
  mutate(year = 2017) %>%
  select(-c(ano17))
resume = rbind(resume, temp)

jariC2 = shapefile('./vector-based/data/jari_2020cgapsClean.shp')
total = dim(jariC2@data)[1]
temp = jariC2@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/IC)*IC+(IC/2)) %>%
  mutate(site = "Jari C") %>%
  mutate(year = 2020) %>%
  select(-c(ano20))
resume = rbind(resume, temp)

temp = resume %>%
  filter(year == 2017 | year == 2018)
chisq.test(temp$site, temp$bin, correct=FALSE)

temp = resume %>%
  filter(year == 2020)
chisq.test(temp$site, temp$bin, correct=FALSE)

temp = resume %>%
  filter(site == 'Ducke')
chisq.test(temp$year, temp$bin, correct=FALSE)

temp = resume %>%
  filter(site == 'Tanguro')
chisq.test(temp$year, temp$bin, correct=FALSE)

temp = resume %>%
  filter(site == 'Tapajos')
chisq.test(temp$year, temp$bin, correct=FALSE)

temp = resume %>%
  filter(site == 'Jari A')
chisq.test(temp$year, temp$bin, correct=FALSE)

temp = resume %>%
  filter(site == 'Jari B')
chisq.test(temp$year, temp$bin, correct=FALSE)

temp = resume %>%
  filter(site == 'Jari C')
chisq.test(temp$year, temp$bin, correct=FALSE)
