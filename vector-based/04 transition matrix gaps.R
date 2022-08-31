require(raster)
require(rgdal)
require(ggplot2)
require(tidyverse)
require(gridExtra)
require(grid)
require(pivottabler)

### Ducke

duckeOverlay = shapefile('./vector-based/data/ducke_overlay.shp')
duckeOverlay$area = area(duckeOverlay)

singleCatb = duckeOverlay@data %>%                                              # check single gaps
  group_by(b_cat) %>%
  summarise(count = n()) %>%
  filter(count == 1) %>%
  select(b_cat)

# Marca novos gaps
duckeOverlay@data$newGaps = ifelse(duckeOverlay@data$b_cat %in% singleCatb$b_cat, 1, 0)
table(duckeOverlay$newGaps)

# Marca área de floresta
duckeOverlay@data$a_ano17 = replace(duckeOverlay@data$a_ano17 ,is.na(duckeOverlay@data$a_ano17 ),'non.gap')
duckeOverlay@data$b_ano20 = replace(duckeOverlay@data$b_ano20 ,is.na(duckeOverlay@data$b_ano20 ),'non.gap')
table(duckeOverlay$a_ano17)
table(duckeOverlay$b_ano20)

# Marca novos gaps
duckeOverlay@data$b_ano20 = replace(duckeOverlay@data$b_ano20 ,(duckeOverlay@data$newGaps == 1 & duckeOverlay@data$a_ano17 != 'gap'),'new gap')
table(duckeOverlay$b_ano20)

pt = PivotTable$new()
pt$addData(duckeOverlay@data)
pt$addColumnDataGroups("b_ano20")
pt$addRowDataGroups("a_ano17")
pt$defineCalculation(calculationName="m2.ha.ano", summariseExpression="sum(area)/1025/3")
pt$evaluatePivot()
areaTable = pt$asDataFrame()
areaTable

pt = PivotTable$new()
pt$addData(duckeOverlay@data)
pt$addColumnDataGroups("b_ano20")
pt$addRowDataGroups("a_ano17")
pt$defineCalculation(calculationName="n.ha.ano", summariseExpression="n()/1025/3")
pt$evaluatePivot()
freqTable = pt$asDataFrame()
freqTable

#writeOGR(duckeOverlay, ".", "duckeOverlayDplyr", driver = "ESRI Shapefile")


### Jari A

jariAOverlay = shapefile('./vector-based/data/jariA_overlay.shp')
jariAOverlay$area = area(jariAOverlay)

singleCatb = jariAOverlay@data %>%
  group_by(b_cat) %>%
  summarise(count = n()) %>%
  filter(count == 1) %>%
  select(b_cat)

jariAOverlay@data$newGaps = ifelse(jariAOverlay@data$b_cat %in% singleCatb$b_cat, 1, 0)
table(jariAOverlay$newGaps)

jariAOverlay@data$a_ano17 = replace(jariAOverlay@data$a_ano17 ,is.na(jariAOverlay@data$a_ano17 ),'non.gap')
jariAOverlay@data$b_ano20 = replace(jariAOverlay@data$b_ano20 ,is.na(jariAOverlay@data$b_ano20 ),'non.gap')
table(jariAOverlay$a_ano17)
table(jariAOverlay$b_ano20)
jariAOverlay@data$b_ano20 = replace(jariAOverlay@data$b_ano20 ,(jariAOverlay@data$newGaps == 1 & jariAOverlay@data$a_ano17 != 'gap'),'new gap')
table(jariAOverlay$b_ano20)

### Jari B

jariBOverlay = shapefile('./vector-based/data/jariB_overlay.shp')
jariBOverlay$area = area(jariBOverlay)

singleCatb = jariBOverlay@data %>%
  group_by(b_cat) %>%
  summarise(count = n()) %>%
  filter(count == 1) %>%
  select(b_cat)

jariBOverlay@data$newGaps = ifelse(jariBOverlay@data$b_cat %in% singleCatb$b_cat, 1, 0)
table(jariBOverlay$newGaps)

jariBOverlay@data$a_ano17 = replace(jariBOverlay@data$a_ano17 ,is.na(jariBOverlay@data$a_ano17 ),'non.gap')
jariBOverlay@data$b_ano20 = replace(jariBOverlay@data$b_ano20 ,is.na(jariBOverlay@data$b_ano20 ),'non.gap')
table(jariBOverlay$a_ano17)
table(jariBOverlay$b_ano20)
jariBOverlay@data$b_ano20 = replace(jariBOverlay@data$b_ano20 ,(jariBOverlay@data$newGaps == 1 & jariBOverlay@data$a_ano17 != 'gap'),'new gap')
table(jariBOverlay$b_ano20)

### Jari C

jariCOverlay = shapefile('./vector-based/data/jariC_overlay.shp')
jariCOverlay$area = area(jariCOverlay)

singleCatb = jariCOverlay@data %>%
  group_by(b_cat) %>%
  summarise(count = n()) %>%
  filter(count == 1) %>%
  select(b_cat)

jariCOverlay@data$newGaps = ifelse(jariCOverlay@data$b_cat %in% singleCatb$b_cat, 1, 0)
table(jariCOverlay$newGaps)

jariCOverlay@data$a_ano17 = replace(jariCOverlay@data$a_ano17 ,is.na(jariCOverlay@data$a_ano17 ),'non.gap')
jariCOverlay@data$b_ano20 = replace(jariCOverlay@data$b_ano20 ,is.na(jariCOverlay@data$b_ano20 ),'non.gap')
table(jariCOverlay$a_ano17)
table(jariCOverlay$b_ano20)
jariCOverlay@data$b_ano20 = replace(jariCOverlay@data$b_ano20 ,(jariCOverlay@data$newGaps == 1 & jariCOverlay@data$a_ano17 != 'gap'),'new gap')
table(jariCOverlay$b_ano20)


jari = rbind(jariAOverlay@data, jariBOverlay@data, jariCOverlay@data)

pt = PivotTable$new()
pt$addData(jari)
pt$addColumnDataGroups("b_ano20")
pt$addRowDataGroups("a_ano17")
pt$defineCalculation(calculationName="areaGaps", summariseExpression="sum(area)/813/3")
pt$evaluatePivot()
areaTable = pt$asDataFrame()
areaTable

pt = PivotTable$new()
pt$addData(jari)
pt$addColumnDataGroups("b_ano20")
pt$addRowDataGroups("a_ano17")
pt$defineCalculation(calculationName="areaGaps", summariseExpression="sum(area)/182708*100")
pt$evaluatePivot()
areaTable = pt$asDataFrame()
areaTable

pt = PivotTable$new()
pt$addData(jari)
pt$addColumnDataGroups("b_ano20")
pt$addRowDataGroups("a_ano17")
pt$defineCalculation(calculationName="numberGaps", summariseExpression="n()/813/3")
pt$evaluatePivot()
freqTable = pt$asDataFrame()
freqTable

#writeOGR(jariCOverlay, ".", "jariCOverlayDplyr", driver = "ESRI Shapefile")

### Tanguro

tanguroOverlay = shapefile('./vector-based/data/tanguro_overlay_clipped.shp')
tanguroOverlay$area = area(tanguroOverlay)
sum(tanguroOverlay$area)

singleCatb = tanguroOverlay@data %>%
  group_by(b_cat) %>%
  summarise(count = n()) %>%
  filter(count == 1) %>%
  select(b_cat)

tanguroOverlay@data$newGaps = ifelse(tanguroOverlay@data$b_cat %in% singleCatb$b_cat, 1, 0)
table(tanguroOverlay$newGaps)

tanguroOverlay@data$a_ano17 = replace(tanguroOverlay@data$a_ano17 ,is.na(tanguroOverlay@data$a_ano17 ),'non.gap')
tanguroOverlay@data$b_ano20 = replace(tanguroOverlay@data$b_ano20 ,is.na(tanguroOverlay@data$b_ano20 ),'non.gap')
table(tanguroOverlay$a_ano17)
table(tanguroOverlay$b_ano20)
tanguroOverlay@data$b_ano20 = replace(tanguroOverlay@data$b_ano20 ,(tanguroOverlay@data$newGaps == 1 & tanguroOverlay@data$a_ano17 != 'gap'),'new gap')
table(tanguroOverlay$b_ano20)

pt = PivotTable$new()
pt$addData(tanguroOverlay@data)
pt$addColumnDataGroups("b_ano20")
pt$addRowDataGroups("a_ano17")
pt$defineCalculation(calculationName="areaGaps", summariseExpression="sum(area)/590/2")
pt$evaluatePivot()
areaTable = pt$asDataFrame()
areaTable

pt = PivotTable$new()
pt$addData(tanguroOverlay@data)
pt$addColumnDataGroups("b_ano20")
pt$addRowDataGroups("a_ano17")
pt$defineCalculation(calculationName="areaGaps", summariseExpression="sum(area)/375735.5*100")
pt$evaluatePivot()
areaTable = pt$asDataFrame()
areaTable

pt = PivotTable$new()
pt$addData(tanguroOverlay@data)
pt$addColumnDataGroups("b_ano20")
pt$addRowDataGroups("a_ano17")
pt$defineCalculation(calculationName="numberGaps", summariseExpression="n()/590/2")
pt$evaluatePivot()
freqTable = pt$asDataFrame()
freqTable

#writeOGR(tanguroOverlay, ".", "tanguroOverlayDplyr", driver = "ESRI Shapefile")


### Tapajos

tapajosOverlay = shapefile('./vector-based/data/tapajos_overlay.shp')
tapajosOverlay$area = area(tapajosOverlay)

singleCatb = tapajosOverlay@data %>%
  group_by(b_cat) %>%
  summarise(count = n()) %>%
  filter(count == 1) %>%
  select(b_cat)

tapajosOverlay@data$newGaps = ifelse(tapajosOverlay@data$b_cat %in% singleCatb$b_cat, 1, 0)
table(tapajosOverlay$newGaps)

tapajosOverlay@data$a_ano17 = replace(tapajosOverlay@data$a_ano17 ,is.na(tapajosOverlay@data$a_ano17 ),'non.gap')
tapajosOverlay@data$b_ano20 = replace(tapajosOverlay@data$b_ano20 ,is.na(tapajosOverlay@data$b_ano20 ),'non.gap')
table(tapajosOverlay$a_ano17)
table(tapajosOverlay$b_ano20)
tapajosOverlay@data$b_ano20 = replace(tapajosOverlay@data$b_ano20 ,(tapajosOverlay@data$newGaps == 1 & tapajosOverlay@data$a_ano17 != 'gap'),'new gap')
table(tapajosOverlay$b_ano20)

pt = PivotTable$new()
pt$addData(tapajosOverlay@data)
pt$addColumnDataGroups("b_ano20")
pt$addRowDataGroups("a_ano17")
pt$defineCalculation(calculationName="areaGaps", summariseExpression="sum(area)/1026/3")
pt$evaluatePivot()
areaTable = pt$asDataFrame()
areaTable

pt = PivotTable$new()
pt$addData(tapajosOverlay@data)
pt$addColumnDataGroups("b_ano20")
pt$addRowDataGroups("a_ano17")
pt$defineCalculation(calculationName="areaGaps", summariseExpression="sum(area)/1157741.5*100")
pt$evaluatePivot()
areaTable = pt$asDataFrame()
areaTable

pt = PivotTable$new()
pt$addData(tapajosOverlay@data)
pt$addColumnDataGroups("b_ano20")
pt$addRowDataGroups("a_ano17")
pt$defineCalculation(calculationName="numberGaps", summariseExpression="n()/1026/3")
pt$evaluatePivot()
freqTable = pt$asDataFrame()
freqTable

#writeOGR(tapajosOverlay, ".", "tapajosOverlayDplyr", driver = "ESRI Shapefile")