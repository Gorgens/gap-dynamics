require(raster)
require(rgdal)
require(ggplot2)
require(tidyverse)
require(gridExtra)
require(grid)
require(pivottabler)

## Gráfico log-log

# Percentage
gaps = shapefile('./vector-based/data/ducke_2017gapsClean.shp')

total = dim(gaps@data)[1]
gapsResume = gaps@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin) %>%
  mutate(nperc = n / total)

gaps2 = shapefile('./vector-based/data/ducke_2020gapsClean.shp')

total = dim(gaps2@data)[1]
gaps2Resume = gaps2@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin) %>%
  mutate(nperc = n / total)

colors <- c("First flight" = "black", "Second flight" = "gray")

duckePerc = ggplot(gapsResume) +                                                   # gráfico log-log da área pela frequência
  geom_point(aes(bin, nperc, color = 'First flight')) +
  geom_point(data = gaps2Resume, aes(bin, nperc, color = 'Second flight')) +
  scale_y_log10(breaks=c(0.001, 0.01, 0.1)) +
  labs(x = 'Size class', y = "Proportion of gaps") +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position="bottom") +
  xlim(0, 12000) +
  scale_color_manual("",values = colors)

duckePerc

# Counting
gaps = shapefile('./vector-based/data/ducke_2017gapsClean.shp')

gapsResume = gaps@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin)

gaps2 = shapefile('./vector-based/data/ducke_2020gapsClean.shp') 

gaps2Resume = gaps2@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin)

ducke = ggplot(gapsResume) +                                                   # gráfico log-log da área pela frequência
  geom_point(aes(bin, n)) +
  geom_point(data = gaps2Resume, aes(bin, n), color = 'gray') +
  scale_x_log10(breaks=c(25, 50, 100, 250, 500, 1000)) + 
  scale_y_log10(breaks=c(25, 50, 100, 250, 500, 1000)) +
  labs(x = 'Size class', y = "Number of gaps") +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

ducke

# Percentage
gaps = shapefile('./vector-based/data/tanguro_2018gapsClean_clipped.shp') 

total = dim(gaps@data)[1]
gapsResume = gaps@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin) %>%
  mutate(nperc = n / total)

gaps2 = shapefile('./vector-based/data/tanguro_2020gapsClean_clipped.shp')

total = dim(gaps2@data)[1]
gaps2Resume = gaps2@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin) %>%
  mutate(nperc = n / total)

colors <- c("First flight" = "black", "Second flight" = "gray")

tanguroPerc = ggplot(gapsResume) +                                                   # gráfico log-log da área pela frequência
  geom_point(aes(bin, nperc, color = 'First flight')) +
  geom_point(data = gaps2Resume, aes(bin, nperc, color = 'Second flight')) +
  scale_x_log10(breaks=c(25, 50, 100, 250, 500, 1000)) + 
  scale_y_log10(breaks=c(0.001, 0.01, 0.1)) +
  labs(x = 'Size class', y = "Proportion of gaps") +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position="bottom") +
  xlim(0, 12000) +
  scale_color_manual("",values = colors)

tanguroPerc

# Counting
gaps = shapefile('./vector-based/data/tanguro_2018gapsClean_clipped.shp')

gapsResume = gaps@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin)

gaps2 = shapefile('./vector-based/data/tanguro_2020gapsClean_clipped.shp')

gaps2Resume = gaps2@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin)

tanguro = ggplot(gapsResume) +                                                   # gráfico log-log da área pela frequência
  geom_point(aes(bin, n)) +
  geom_point(data = gaps2Resume, aes(bin, n), color = 'gray') +
  scale_x_log10(breaks=c(25, 50, 100, 250, 500, 1000)) + 
  scale_y_log10(breaks=c(25, 50, 100, 250, 500, 1000)) +
  labs(x = 'Size class', y = "Number of gaps") +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

tanguro

#percentage
gaps = shapefile('./vector-based/data/tapajos_2017gapsClean.shp') 

total = dim(gaps@data)[1]
gapsResume = gaps@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin) %>%
  mutate(nperc = n / total)

gaps2 = shapefile('./vector-based/data/tapajos_2020gapsClean.shp')

total = dim(gaps2@data)[1]
gaps2Resume = gaps2@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin) %>%
  mutate(nperc = n / total)

colors <- c("First flight" = "black", "Second flight" = "gray")

tapajosPerc = ggplot(gapsResume) +                                                   # gráfico log-log da área pela frequência
  geom_point(aes(bin, nperc, color = 'First flight')) +
  geom_point(data = gaps2Resume, aes(bin, nperc, color = 'Second flight')) +
  scale_x_log10(breaks=c(25, 50, 100, 250, 500, 1000)) + 
  scale_y_log10(breaks=c(0.001, 0.01, 0.1)) +
  labs(x = 'Size class', y = "Proportion of gaps") +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position="bottom") +
  xlim(0, 12000) +
  scale_color_manual("",values = colors)


tapajosPerc


#counting
gaps = shapefile('./vector-based/data/tapajos_2017gapsClean.shp')

gapsResume = gaps@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin)

gaps2 = shapefile('./vector-based/data/tapajos_2020gapsClean.shp')

gaps2Resume = gaps2@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin)

tapajos = ggplot(gapsResume) +                                                   # gráfico log-log da área pela frequência
  geom_point(aes(bin, n)) +
  geom_point(data = gaps2Resume, aes(bin, n), color = 'gray') +
  scale_x_log10(breaks=c(25, 50, 100, 250, 500, 1000)) + 
  scale_y_log10(breaks=c(25, 50, 100, 250, 500, 1000)) +
  labs(x = 'Size class', y = "Number of gaps") +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

tapajos

#percentage
gaps = shapefile('./vector-based/data/jari_2017agapsClean.shp')
temp_gap = gaps@data

total = dim(gaps@data)[1]
gapsResume = gaps@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin) %>%
  mutate(nperc = n / total)

gaps2 = shapefile('./vector-based/data/jari_2020agapsClean.shp')
temp_gap2 = gaps2@data

total = dim(gaps2@data)[1]
gaps2Resume = gaps2@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin) %>%
  mutate(nperc = n / total)

colors <- c("First flight" = "black", "Second flight" = "gray")

jariAPerc = ggplot(gapsResume) +                                                   # gráfico log-log da área pela frequência
  geom_point(aes(bin, nperc, color = 'First flight')) +
  geom_point(data = gaps2Resume, aes(bin, nperc, color = 'Second flight')) +
  scale_x_log10(breaks=c(25, 50, 100, 250, 500, 1000)) + 
  scale_y_log10(breaks=c(0.001, 0.01, 0.1)) +
  labs(x = 'Size class', y = "Proportion of gaps") +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position="bottom") +
  scale_color_manual("",values = colors)


jariAPerc


#couting
gaps = shapefile('./vector-based/data/jari_2017agapsClean.shp')

gapsResume = gaps@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin)

gaps2 = shapefile('./vector-based/data/jari_2020agapsClean.shp') 

gaps2Resume = gaps2@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin)

jariA = ggplot(gapsResume) +                                                   # gráfico log-log da área pela frequência
  geom_point(aes(bin, n)) +
  geom_point(data = gaps2Resume, aes(bin, n), color = 'gray') +
  scale_x_log10(breaks=c(25, 50, 100, 250, 500, 1000)) + 
  scale_y_log10(breaks=c(25, 50, 100, 250, 500, 1000)) +
  labs(x = 'Size class', y = "Number of gaps") +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

jariA


#percentage
gaps = shapefile('./vector-based/data/jari_2017bgapsClean.shp')
temp_gap = rbind(temp_gap, gaps@data)

total = dim(gaps@data)[1]
gapsResume = gaps@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin) %>%
  mutate(nperc = n / total)

gaps2 = shapefile('./vector-based/data/jari_2020bgapsClean.shp')
temp_gap2 = rbind(temp_gap2, gaps2@data)

total = dim(gaps2@data)[1]
gaps2Resume = gaps2@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin) %>%
  mutate(nperc = n / total)

colors <- c("First flight" = "black", "Second flight" = "gray")

jariBPerc = ggplot(gapsResume) +                                                   # gráfico log-log da área pela frequência
  geom_point(aes(bin, nperc, color = 'First flight')) +
  geom_point(data = gaps2Resume, aes(bin, nperc, color = 'Second flight')) +
  scale_x_log10(breaks=c(25, 50, 100, 250, 500, 1000)) + 
  scale_y_log10(breaks=c(0.001, 0.01, 0.1)) +
  labs(x = 'Size class', y = "Proportion of gaps") +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position="bottom") +
  scale_color_manual("",values = colors)


jariBPerc


#counting
gaps = shapefile('./vector-based/data/jari_2017bgapsClean.shp')

gapsResume = gaps@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin)

gaps2 = shapefile('./vector-based/data/jari_2020bgapsClean.shp')

gaps2Resume = gaps2@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin)

jariB = ggplot(gapsResume) +                                                   # gráfico log-log da área pela frequência
  geom_point(aes(bin, n)) +
  geom_point(data = gaps2Resume, aes(bin, n), color = 'gray') +
  scale_x_log10(breaks=c(25, 50, 100, 250, 500, 1000)) + 
  scale_y_log10(breaks=c(25, 50, 100, 250, 500, 1000)) +
  labs(x = 'Size class', y = "Number of gaps") +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

jariB


#percentage
gaps = shapefile('./vector-based/data/jari_2017cgapsClean.shp')
temp_gap = rbind(temp_gap, gaps@data)

total = dim(gaps@data)[1]
gapsResume = gaps@data %>%                                                      # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin) %>%
  mutate(nperc = n / total)

gaps2 = shapefile('./vector-based/data/jari_2020cgapsClean.shp')
temp_gap2 = rbind(temp_gap2, gaps2@data)

total = dim(gaps2@data)[1]
gaps2Resume = gaps2@data %>%                                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin) %>%
  mutate(nperc = n / total)

colors <- c("First flight" = "black", "Second flight" = "gray")

jariCPerc = ggplot(gapsResume) +                                                   # gráfico log-log da área pela frequência
  geom_point(aes(bin, nperc, color = 'First flight')) +
  geom_point(data = gaps2Resume, aes(bin, nperc, color = 'Second flight')) +
  scale_x_log10(breaks=c(25, 50, 100, 250, 500, 1000)) + 
  scale_y_log10(breaks=c(0.001, 0.01, 0.1)) +
  labs(x = 'Size class', y = "Proportion of gaps") +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position="bottom") +
  scale_color_manual("",values = colors)


jariCPerc


#counting
gaps = shapefile('./vector-based/data/jari_2017cgapsClean.shp') 

gapsResume = gaps@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin)

gaps2 = shapefile('./vector-based/data/jari_2020cgapsClean.shp')

gaps2Resume = gaps2@data %>%                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin)

jariC = ggplot(gapsResume) +                                                   # gráfico log-log da área pela frequência
  geom_point(aes(bin, n)) +
  geom_point(data = gaps2Resume, aes(bin, n), color = 'gray') +
  scale_x_log10(breaks=c(25, 50, 100, 250, 500, 1000)) + 
  scale_y_log10(breaks=c(25, 50, 100, 250, 500, 1000)) +
  labs(x = 'Size class', y = "Number of gaps") +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

jariC

## Jari together
total = dim(temp_gap)[1]
gapsResume = temp_gap %>%                                                      # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin) %>%
  mutate(nperc = n / total)

total = dim(temp_gap2)[1]
gaps2Resume = temp_gap2 %>%                                                    # count number of gaps per area bins
  mutate(bin = floor(area/2)*2+1) %>%
  group_by(bin) %>%
  count(bin) %>%
  mutate(nperc = n / total)

colors <- c("First flight" = "black", "Second flight" = "gray")

jariPerc = ggplot(gapsResume) +                                                   # gráfico log-log da área pela frequência
  geom_point(aes(bin, nperc, color = 'First flight')) +
  geom_point(data = gaps2Resume, aes(bin, nperc, color = 'Second flight')) +
  scale_x_log10(breaks=c(25, 50, 100, 250, 500, 1000)) + 
  scale_y_log10(breaks=c(0.001, 0.01, 0.1)) +
  labs(x = 'Size class', y = "Proportion of gaps") +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position="bottom") +
  xlim(0, 12000) +
  scale_color_manual("",values = colors)

jariPerc

#percentage
myplot1 <- arrangeGrob(duckePerc, top = textGrob("Ducke", x = unit(0, "npc")
                                                 , y   = unit(1, "npc"), just=c("left","top"),
                                                 gp=gpar(col="black", fontsize=10, fontfamily="Times Roman")))

myplot2 <- arrangeGrob(tanguroPerc, top = textGrob("Tanguro", x = unit(0, "npc")
                                                   , y = unit(1, "npc"), just=c("left","top"),
                                                   gp=gpar(col="black", fontsize=10, fontfamily="Times Roman")))

myplot3 <- arrangeGrob(tapajosPerc, top = textGrob("Tapajos", x = unit(0, "npc")
                                                   , y  = unit(1, "npc"), just=c("left","top"),
                                                   gp=gpar(col="black", fontsize=10, fontfamily="Times Roman")))

myplot4 <- arrangeGrob(jariAPerc, top = textGrob("b", x = unit(0, "npc")
                                                 , y = unit(1, "npc"), just=c("left","top"),
                                                 gp=gpar(col="black",    fontsize=10, fontfamily="Times Roman")))

myplot5 <- arrangeGrob(jariBPerc, top = textGrob("d", x = unit(0, "npc")
                                                 , y  = unit(1, "npc"), just=c("left","top"),
                                                 gp=gpar(col="black", fontsize=10, fontfamily="Times Roman")))

myplot6 <- arrangeGrob(jariCPerc, top = textGrob("e", x = unit(0, "npc")
                                                 , y = unit(1, "npc"), just=c("left","top"),
                                                 gp=gpar(col="black",    fontsize=10, fontfamily="Times Roman")))

myplot7 <- arrangeGrob(jariPerc, top = textGrob("Jari", x = unit(0, "npc")
                                                 , y = unit(1, "npc"), just=c("left","top"),
                                                 gp=gpar(col="black",    fontsize=10, fontfamily="Times Roman")))

ggsave("../plot/log-Perc_v28012022.png", plot=grid.arrange(myplot2, myplot3,
                                            myplot1, myplot7,
                                            nrow=2),
       width = 20, height = 24, units = 'cm', dpi = 300)


#counting
myplot1 <- arrangeGrob(ducke, top = textGrob("a", x = unit(0, "npc")
                                             , y   = unit(1, "npc"), just=c("left","top"),
                                             gp=gpar(col="black", fontsize=10, fontfamily="Times Roman")))

myplot2 <- arrangeGrob(tanguro, top = textGrob("c", x = unit(0, "npc")
                                               , y = unit(1, "npc"), just=c("left","top"),
                                               gp=gpar(col="black", fontsize=10, fontfamily="Times Roman")))

myplot3 <- arrangeGrob(tapajos, top = textGrob("e", x = unit(0, "npc")
                                               , y  = unit(1, "npc"), just=c("left","top"),
                                               gp=gpar(col="black", fontsize=10, fontfamily="Times Roman")))

myplot4 <- arrangeGrob(jariA, top = textGrob("b", x = unit(0, "npc")
                                             , y = unit(1, "npc"), just=c("left","top"),
                                             gp=gpar(col="black",    fontsize=10, fontfamily="Times Roman")))

myplot5 <- arrangeGrob(jariB, top = textGrob("d", x = unit(0, "npc")
                                             , y  = unit(1, "npc"), just=c("left","top"),
                                             gp=gpar(col="black", fontsize=10, fontfamily="Times Roman")))

myplot6 <- arrangeGrob(jariC, top = textGrob("f", x = unit(0, "npc")
                                             , y = unit(1, "npc"), just=c("left","top"),
                                             gp=gpar(col="black",    fontsize=10, fontfamily="Times Roman")))

ggsave("log-log.png", plot=grid.arrange(myplot1, myplot4,
                                        myplot2, myplot5,
                                        myplot3, myplot6, nrow=3),
       width = 12, height = 9, units = 'cm', dpi = 300)