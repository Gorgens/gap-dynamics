require(raster)
require(rgdal)
require(tidyverse)

### Frequencia e area de gaps por site

ducke1 = shapefile('./vector-based/data/ducke_2017gapsClean.shp')
ducke1 <- within(ducke1@data, {   
  gapsize <- NA # need to initialize variable
  gapsize[area < 100] <- "Small"
  gapsize[area >= 100 & area < 500] <- "Mid"
  gapsize[area >= 500] <- "Large"
} )

ducke2017 = ducke1 %>%                                    # count number of gaps per area bins
  group_by(gapsize) %>%
  summarise(area = sum(area),
            m2.ha = sum(area)/1205,
            n = n(),
            n.ha = n()/1205)

ducke2 = shapefile('./vector-based/data/ducke_2020gapsClean.shp')
ducke2 <- within(ducke2@data, {   
  gapsize <- NA # need to initialize variable
  gapsize[area < 100] <- "Small"
  gapsize[area >= 100 & area < 500] <- "Mid"
  gapsize[area >= 500] <- "Large"
} )

ducke2020 = ducke2 %>%                                    # count number of gaps per area bins
  group_by(gapsize) %>%
  summarise(area = sum(area),
            m2.ha = sum(area)/1205,
            n = n(),
            n.ha = n()/1205)

tanguro1 = shapefile('./vector-based/data/tanguro_2018gapsClean_clipped.shp')
tanguro1 <- within(tanguro1@data, {   
  gapsize <- NA # need to initialize variable
  gapsize[area < 100] <- "Small"
  gapsize[area >= 100 & area < 500] <- "Mid"
  gapsize[area >= 500] <- "Large"
} )

tanguro2018 = tanguro1 %>%                                    # count number of gaps per area bins
  group_by(gapsize) %>%
  summarise(area = sum(area),
            m2.ha = sum(area)/590,
            n = n(),
            n.ha = n()/590)

tanguro2 = shapefile('./vector-based/data/tanguro_2020gapsClean_clipped.shp')
tanguro2 <- within(tanguro2@data, {   
  gapsize <- NA # need to initialize variable
  gapsize[area < 100] <- "Small"
  gapsize[area >= 100 & area < 500] <- "Mid"
  gapsize[area >= 500] <- "Large"
} )

tanguro2020 = tanguro2 %>%                                    # count number of gaps per area bins
  group_by(gapsize) %>%
  summarise(area = sum(area),
            m2.ha = sum(area)/590,
            n = n(),
            n.ha = n()/590)

tapajos1 = shapefile('./vector-based/data/tapajos_2017gapsClean.shp')
tapajos1 <- within(tapajos1@data, {   
  gapsize <- NA # need to initialize variable
  gapsize[area < 100] <- "Small"
  gapsize[area >= 100 & area < 500] <- "Mid"
  gapsize[area >= 500] <- "Large"
} )

tapajos2017 = tapajos1 %>%                                    # count number of gaps per area bins
  group_by(gapsize) %>%
  summarise(area = sum(area),
            m2.ha = sum(area)/1026,
            n = n(),
            n.ha = n()/1026)

tapajos2 = shapefile('./vector-based/data/tapajos_2020gapsClean.shp')
tapajos2 <- within(tapajos2@data, {   
  gapsize <- NA # need to initialize variable
  gapsize[area < 100] <- "Small"
  gapsize[area >= 100 & area < 500] <- "Mid"
  gapsize[area >= 500] <- "Large"
} )

tapajos2020 = tapajos2 %>%                                    # count number of gaps per area bins
  group_by(gapsize) %>%
  summarise(area = sum(area),
            m2.ha = sum(area)/1026,
            n = n(),
            n.ha = n()/1026)

jariA1 = shapefile('./vector-based/data/jari_2017agapsClean.shp')
jariA1 <- within(jariA1@data, {   
  gapsize <- NA # need to initialize variable
  gapsize[area < 100] <- "Small"
  gapsize[area >= 100 & area < 500] <- "Mid"
  gapsize[area >= 500] <- "Large"
} )

jariB1 = shapefile('./vector-based/data/jari_2017bgapsClean.shp')
jariB1 <- within(jariB1@data, {   
  gapsize <- NA # need to initialize variable
  gapsize[area < 100] <- "Small"
  gapsize[area >= 100 & area < 500] <- "Mid"
  gapsize[area >= 500] <- "Large"
} )

jariC1 = shapefile('./vector-based/data/jari_2017cgapsClean.shp')
jariC1 <- within(jariC1@data, {   
  gapsize <- NA # need to initialize variable
  gapsize[area < 100] <- "Small"
  gapsize[area >= 100 & area < 500] <- "Mid"
  gapsize[area >= 500] <- "Large"
} )

jari = rbind(jariA1, jariB1, jariC1)

jari2017 = jari %>%                                    # count number of gaps per area bins
  group_by(gapsize) %>%
  summarise(area = sum(area),
            m2.ha = sum(area)/813,
            n = n(),
            n.ha = n()/813)

jariA2 = shapefile('./vector-based/data/jari_2020agapsClean.shp')
jariA2 <- within(jariA2@data, {   
  gapsize <- NA # need to initialize variable
  gapsize[area < 100] <- "Small"
  gapsize[area >= 100 & area < 500] <- "Mid"
  gapsize[area >= 500] <- "Large"
} )

jariB2 = shapefile('./vector-based/data/jari_2020bgapsClean.shp')
jariB2 <- within(jariB2@data, {   
  gapsize <- NA # need to initialize variable
  gapsize[area < 100] <- "Small"
  gapsize[area >= 100 & area < 500] <- "Mid"
  gapsize[area >= 500] <- "Large"
} )

jariC2 = shapefile('./vector-based/data/jari_2020cgapsClean.shp')
jariC2 <- within(jariC2@data, {   
  gapsize <- NA # need to initialize variable
  gapsize[area < 100] <- "Small"
  gapsize[area >= 100 & area < 500] <- "Mid"
  gapsize[area >= 500] <- "Large"
} )

jari2 = rbind(jariA2, jariB2, jariC2)

jari2020 = jari2 %>%                                    # count number of gaps per area bins
  group_by(gapsize) %>%
  summarise(area = sum(area),
            m2.ha = sum(area)/813,
            n = n(),
            n.ha = n()/813)
