

# Site7_Wares.R
# Establish a DBI connection to DAACS PostgreSQL database and submnit SQL queries
# Created by:  FDN  12.10.2018
# Update: FDN 10.17.2024 


# load the libraries
library(RPostgreSQL)
library(tidyverse)
library(tidyr)
library(mgcv)
library (ca)
library (plotrix)
library(ggplot2)
library(ggrepel)
library(ggridges)
library(viridis)
library(Hmisc)
library(DirichletMultinomial)
library(cowplot)



#### 1. Tell DBI which driver to use #### 
pgSQL <- dbDriver("PostgreSQL")
# establish the connection
DRCcon<-dbConnect(pgSQL, host='drc.iath.virginia.edu', port='5432',
                  dbname='daacs-production',
                  user='drcquery', password='!queryacct!')


#### 2. Submit a SQL query for ceramics. ####
# note the use of \ as an escape sequence 
# note the LEFT JOIN on the Feature table retains non-feature contexts
# Fill in your ProjectID

wareTypeAndSizeData <- dbGetQuery(DRCcon,'
 SELECT
 "public"."tblCeramic"."Quantity",
 "public"."tblCeramic"."SherdWeight",
 "public"."tblCeramic"."SherdThickness",
 "public"."tblCeramic"."MaximumSherdMeasurement",
 "public"."tblCeramicWare"."Ware",
 "public"."tblCeramicGenre"."CeramicGenre",
 "public"."tblCeramicCEWType"."CeramicCEWType",
 "public"."tblCeramicForm"."CeramicForm",
 "a"."CeramicGlaze" AS "InteriorGlaze",
 "b"."CeramicGlaze" AS "ExteriorGlaze", 
 "public"."tblProjectName"."ProjectName",
 "public"."tblContext"."ProjectID",
 "public"."tblContext"."Context",
 "public"."tblContextDepositType"."DepositType",
 "public"."tblContext"."DAACSStratigraphicGroup",
 "public"."tblContext"."DAACSStratigraphicGroupInterpretation",
 "public"."tblContext"."MasterContextNumber",
 "public"."tblContext"."FeatureNumber",
 "public"."tblContextFeatureType"."FeatureType",
 "public"."tblContext"."QuadratID",
 "public"."tblContext"."DAACSPhase"
 
 FROM
 "public"."tblProjectName"
 INNER JOIN "public"."tblProject" 
  ON "public"."tblProject"."ProjectNameID" = 
  "public"."tblProjectName"."ProjectNameID"
 INNER JOIN "public"."tblContext" 
  ON "public"."tblContext"."ProjectID" = "public"."tblProject"."ProjectID"
 LEFT JOIN "public"."tblContextDepositType" 
  ON "public"."tblContext"."DepositTypeID" = "public"."tblContextDepositType"."DepositTypeID"
 LEFT JOIN "tblContextFeature"  
  ON  "tblContext"."FeatureNumber" = "tblContextFeature"."FeatureNumber" 
  AND "tblContext"."ProjectID" = "tblContextFeature"."ProjectID"
 LEFT JOIN "tblContextFeatureType" 
  ON "tblContextFeature"."FeatureTypeID" = "tblContextFeatureType"."FeatureTypeID"
 INNER JOIN "public"."tblContextSample" 
  ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
 INNER JOIN "public"."tblGenerateContextArtifactID" 
  ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
 INNER JOIN "public"."tblCeramic" 
  ON "public"."tblCeramic"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
 INNER JOIN "public"."tblCeramicWare" 
  ON "public"."tblCeramic"."WareID" = "public"."tblCeramicWare"."WareID"
 LEFT JOIN "public"."tblCeramicGenre" 
  ON "public"."tblCeramic"."CeramicGenreID" = "public"."tblCeramicGenre"."CeramicGenreID"
 LEFT JOIN "public"."tblCeramicCEWType" 
  ON "public"."tblCeramic"."CeramicCEWTypeID" = "public"."tblCeramicCEWType"."CeramicCEWTypeID"
 LEFT JOIN "public"."tblCeramicForm"	
	ON "public"."tblCeramic"."CeramicFormID" = "public"."tblCeramicForm"."CeramicFormID"
 LEFT JOIN "public"."tblCeramicGlaze" AS "a"
 	ON "public"."tblCeramic"."InteriorGlazeID" =  "a"."CeramicGlazeID"
 LEFT JOIN "public"."tblCeramicGlaze" AS "b"
 	ON "public"."tblCeramic"."ExteriorGlazeID" =  "b"."CeramicGlazeID"	
 WHERE "public"."tblContext"."ProjectID" = \'107\'
 ')


options(tibble.print_max=Inf)

# check the SG and deposit types
# unique(wareTypeAndSizeData[c("DepositType","DAACSStratigraphicGroup", 
#                             "DAACSStratigraphicGroupInterpretation")])

# get rid of contexts related to modern dumping n the site
wareTypeAndSizeData1 <- wareTypeAndSizeData %>%
  dplyr::filter((CeramicForm != 'Flower Pot')|(InteriorGlaze != 'Albany Slip'))


#### 3. SQL to get Northings and Eastings ####
quadCoords <- dbGetQuery(DRCcon,'
SELECT
  "public"."tblContextQuadratBoundary"."ProjectID",
  "public"."tblContextQuadratBoundary"."QuadratID",
  Avg("public"."tblContextQuadratBoundary"."QuadratNorthing") AS "Northing",
  Avg("public"."tblContextQuadratBoundary"."QuadratEasting") AS "Easting"
FROM
  "public"."tblContext"
   INNER JOIN "public"."tblContextQuadratBoundary" ON 
  "public"."tblContext"."ProjectID" = "public"."tblContextQuadratBoundary"."ProjectID" AND 
  "public"."tblContext"."QuadratID" = "public"."tblContextQuadratBoundary"."QuadratID"
WHERE
  "public"."tblContext"."ProjectID" = \'107\' 
GROUP BY
  "public"."tblContextQuadratBoundary"."ProjectID",
  "public"."tblContextQuadratBoundary"."QuadratID" 
ORDER BY
  "public"."tblContextQuadratBoundary"."ProjectID",
  "public"."tblContextQuadratBoundary"."QuadratID" 
 ')

print(quadCoords, digits=20)

 
#### 4. Map the Quads ####

theme_set(theme_classic(base_size = 18))
p1 <- ggplot(data=quadCoords, aes(x=Easting, y=Northing)) +
  geom_point(size=4, shape=22, fill= grey(.5)) + 
  geom_text_repel( aes(label=QuadratID), size=2) +
  coord_fixed() +
  labs( title = 'Site 7: Quadrat Locations',
        subtitle = '',
        caption= '')
p1







#### 5. Select and recode context and artifact variables #### 
wareTypeAndSizeData2 <- wareTypeAndSizeData1 %>% 
  # keep only a few variables
  dplyr::select(QuadratID, Context, FeatureNumber, Ware, CeramicGenre, 
                SherdWeight, SherdThickness, MaximumSherdMeasurement, 
                Quantity) %>%
  #get rid of commas, slashes, dashes, spaces  
  
  
  dplyr::mutate(Ware = gsub(" ", ".", 
                            gsub("  ", " ",
                                gsub("[[:punct:]]"," ", Ware ))))



#### 5.1 Summarize by ware type and feature number and preview counts ####
wareTypeAndSizeData2 %>% 
  group_by(FeatureNumber, Ware) %>% 
  dplyr::summarize(totalCount = sum(Quantity))

wareTypeAndSizeData2 %>% 
  group_by(FeatureNumber) %>% 
  dplyr::summarize(totalCount = sum(Quantity))


#### 5.2 Recode Delft and Keep Jefferson-era ware types ... ####
wareTypeAndSizeData3 <- wareTypeAndSizeData2 %>% 
 mutate (Ware = ifelse (Ware %in% c( 'Delftware.Dutch.British',
                                       'Tin.Enameled.unidentified' ), 'Delft',
                          Ware)) %>% 
  mutate (Ware = ifelse (Ware %in% c( 'British.Brown.Fulham.Type',
                                      'British.Stoneware' ), 
                         'British.Brown.Stoneware',
                         Ware)) %>%   
  
  filter (Ware %in% c('American.Stoneware',                           # 6
                      'Astbury.Type'      ,                           # 2
                      'British.Brown.Fulham.Type',                    # 6
                      'British.Stoneware',                            # 131
                      'Coarse.Earthenware.unidentified',              # 66
                      'Creamware',                                    # 1050
                  #   'Delftware.Dutch.British',                      # 201
                      'German.Stoneware',                             #  2
                  #    'Ironstone.White.Granite',                      #  2
                       'Jackfield.Type',                              #  6
                      'Native.American' ,                             # 11
                       'Nottingham' ,                                 #   8
                       'Pearlware',                                   # 179
                       'Porcelain.Chinese',                           #  59
                  #     'Porcellaneous.Hard.Paste',                    #   2
                       'Redware',                                     #  363
                  #     'Redware.refined',                             #   1
                  #     'Refined.Earthenware.modern',                  #   1
                       'Refined.Earthenware.unidentifiable',          # 151
                  #     'Refined.Stoneware.unidentifiable',            #   2
                       'Slipware.North.Midlands.Staffordshire',       #  48
                  #     'Staffordshire.Brown.Stoneware',               #   1
                       'Staffordshire.Mottled.Glaze',                 #   6
                  #     'Stoneware.unidentifiable',                    #  13
                  #    'Tin.Enameled.unidentified',                   # 294
                       'Westerwald.Rhenish',                          #  95
                       'Whieldon.type.Ware',                          #  69
                       'White.Salt.Glaze' ,                            # 234
                        'Delft',
                        'British.Brown.Stoneware'
                       # 'Whiteware'                                      3
  ))


#### 6. Do the ridgeline plots for sherd thickness ####

p0 <- ggplot(wareTypeAndSizeData3, aes(x = log(SherdThickness), 
                                 y = reorder(Ware,SherdThickness,
                                             FUN= 'median',na.rm = TRUE))) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = .5,
                      scale=1,
                      alpha= .75, fill= 'grey' ) +
  geom_point(pch=21) +
  labs( title = 'Sherd Thickness by Ware ',
        subtitle =  'Site 7',
        y= 'Ware',
        x= 'Sherd Thickness: ln(mm)') +
  theme( plot.title.position = "plot")
p0



ggsave( p0, file = 'Figure1.png',  
        dpi=600, width=10, height=6, scale=1) 


#### 7. get a DF of quads and coords by ware counts  ####
wareCountsByQuadT <- wareTypeAndSizeData3 %>% 
  dplyr::group_by(QuadratID, Ware) %>% 
  dplyr::summarize(Count= sum(Quantity)) %>% 
  pivot_wider(id_cols = QuadratID, names_from = Ware, values_from= Count, 
              values_fill = 0) %>% 
  # get rid of any records from contexts with NA or '' quadratID
  dplyr::filter(QuadratID != '') 
wareCountsByQuadT$totalCount <- rowSums(wareCountsByQuadT[,-1])
coordsAndData <- left_join(quadCoords, wareCountsByQuadT, by = 'QuadratID') %>%
  # set NAs to 0
  replace(., is.na(.),0) 
  
#### 8. Compile a helper function for raster plotting ####
makeGrid <- function(gridSpacing, tooFar, Easting, Northing ) {
  # function to create a grids of points for a area sampled by scattered points
  # Arguments: 
  #   gridSpacing: the desired spacing for the grid points.
  #   tooFar: stipulate the minimum distance grid points must be from samples 
  #   Easting and Northing:  coordinates for sample locations. 
  #           e.g. quadrat centroids.
  y <- seq(min(Northing), max(Northing), by = gridSpacing) 
  x <- seq(min(Easting), max(Easting), by = gridSpacing) 
  xy <- as.matrix(expand.grid(x,y))
  colnames(xy) <-  c( 'Easting', 'Northing')
  coords <- cbind(Easting, Northing)
  if (tooFar == 0){
    cHullCoords <- coords[chull(coords),]
    inOut <- in.out(cHullCoords, xy)
    xy <- data.frame(xy[inOut,])
    plot(xy)
    return(xy)
  }
  else {
    minDistance <- function(x){x[which.min(x)]}
    fromXY <- analogue::distance(xy, coords)
    minDists <- apply(fromXY, 1, function(x) minDistance(x))
    xy <-data.frame(xy[minDists <= tooFar,]) 
    return(xy)
  }
}
names(coordsAndData)

#### 9. Bubble plot for total ceramics ####

theme_set(theme_classic(base_size = 18))
p1 <- ggplot(data = coordsAndData, aes(x=Easting, y=Northing)) +
  geom_point(aes(size= totalCount), shape=21, fill= grey(.75)) + 
#  geom_text_repel( aes(label=QuadratID), size=2) +
  coord_fixed() +
  labs( title = '18th-Century Ceramics',
        subtitle = 'Site 7',
        caption= '')
p1


ggsave("Site7.TotalCeramics-Bubble.png", p1, width=6, height=8, dpi=600)

#### 10. GAMs for Ceramics ####

# 10.1 18th c. Ceramics
g1 <- gam(totalCount       
           ~ s(Easting, Northing, k=100 ), 
          family=  poisson(link=log),  data=coordsAndData, method="REML")
gam.sum <- summary(g1)
AIC(g1)
gam.check(g1, pch=21, cex=2, bg='dark grey',  rep  =500, lwd=1)


gridPts<- makeGrid(gridSpacing=5, tooFar=25, coordsAndData$Easting, 
                   coordsAndData$Northing  ) 
pred <- predict(g1, gridPts, type="link",se.fit=FALSE)
pred1 <- data.frame(gridPts, Predicted = pred)


theme_set(theme_classic(base_size = 18))

p1 <- ggplot(data = pred1 , aes(x = Easting, y = Northing )) +
  geom_raster(aes (fill = exp(Predicted))) + 
  scale_fill_viridis(option= 'plasma',  trans='log' ,alpha=.66, 
                     labels = scales::label_number(accuracy=.01),
                     breaks = exp(seq(min(pred1$Predicted), 
                                            max(pred1$Predicted), 
                                            length.out=11))) +
  #scale_fill_gradientn(colours = grey.colors(100, start=.9, end=.1)) +
  geom_contour(aes(z= exp(Predicted)),
               breaks = round(exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), length.out=11)),1),
               color= 'black', 
               linewidth=.5, show.legend=T) +
  #geom_contour_filled(aes(z= exp(Predicted)),
  #               breaks = round(exp(seq(min(pred1$Predicted), 
  #                               max(pred1$Predicted), length.out=11)),1),
  #             color= 'black', linewidth=.5, show.legend=T) +
  #scale_fill_grey(start=.99, end=.4) +
  #scale_fill_viridis(option= 'plasma', discrete= T, alpha =.66) +  
  geom_point(data= coordsAndData, aes(x=Easting, y=Northing), size=1 ) + 
  coord_fixed() +
  labs( title = '18th-Century Ceramics',
        subtitle =  'Site 7',
        fill = 'Count',
        caption= paste ('Poisson GAM. Deviance explained: ', 
        round(gam.sum$dev.expl,2),'. ',  
                scales::pvalue(gam.sum$s.pv,
                accuracy = 0.001, # Number to round to
                decimal.mark = ".", # The character to be used 
                # to indicate the numeric decimal point
                add_p = TRUE) # Add "p=" before the value?
        )) +
  theme(plot.caption = element_text(face = "italic", size=12),
        legend.text = element_text(size = 12),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(.5, 'cm'))
#        axis.text = element_text(size=10)) 
        
p1


ggsave("Site7.TotalCeramics-GAM.png", p1, width=6, height=8, dpi=600)



# 10.2 Delft
g1 <- gam(Delft       
          ~ s(Easting, Northing, k=100 ), 
          family=  poisson(link=log),  data=coordsAndData, method="REML")
gam.sum <- summary(g1)
AIC(g1)
gam.check(g1, pch=21, cex=2, bg='dark grey',  rep  =500, lwd=1)


gridPts<- makeGrid(gridSpacing=5, tooFar=25, coordsAndData$Easting, 
                   coordsAndData$Northing  ) 
pred <- predict(g1, gridPts, type="link",se.fit=FALSE)
pred1 <- data.frame(gridPts, Predicted = pred)


theme_set(theme_classic(base_size = 18))

p2 <- ggplot(data = pred1 , aes(x = Easting, y = Northing )) +
  geom_raster(aes (fill = exp(Predicted))) + 
  scale_fill_viridis(option= 'plasma',  trans='log' ,alpha=.66, 
                     labels = scales::label_number(accuracy=.01),
                     breaks = exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), 
                                      length.out=11))) +
  #scale_fill_gradientn(colours = grey.colors(100, start=.9, end=.1)) +
  geom_contour(aes(z= exp(Predicted)),
               breaks = round(exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), length.out=11)),1),
               color= 'black', 
               linewidth=.5, show.legend=T) +
  #geom_contour_filled(aes(z= exp(Predicted)),
  #               breaks = round(exp(seq(min(pred1$Predicted), 
  #                               max(pred1$Predicted), length.out=11)),1),
  #             color= 'black', linewidth=.5, show.legend=T) +
  #scale_fill_grey(start=.99, end=.4) +
  #scale_fill_viridis(option= 'plasma', discrete= T, alpha =.66) +  
  geom_point(data= coordsAndData, aes(x=Easting, y=Northing), size=1 ) + 
  coord_fixed() +
  labs( title = 'Delft',
        subtitle =  'Site 7',
        fill = 'Count',
        caption= paste ('Poisson GAM. Deviance explained: ', 
                        round(gam.sum$dev.expl,2),'. ',  
                        scales::pvalue(gam.sum$s.pv,
                                       accuracy = 0.001, # Number to round to
                                       decimal.mark = ".",  #The character to be used 
                                       #to indicate the numeric decimal point
                                       add_p = TRUE) # Add "p=" before the value?
        )) +
  theme(plot.caption = element_text(face = "italic", size=12),
        legend.text = element_text(size = 12),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(.5, 'cm'),
        axis.text = element_text(size=10)) 

p2

 
# 10.3 "White.Salt.Glaze" 

g1 <- gam(White.Salt.Glaze        
          ~ s(Easting, Northing, k=100 ), 
          family=  poisson(link=log),  data=coordsAndData, method="REML")
gam.sum <- summary(g1)
AIC(g1)
gam.check(g1, pch=21, cex=2, bg='dark grey',  rep  =500, lwd=1)


gridPts<- makeGrid(gridSpacing=5, tooFar=25, coordsAndData$Easting, 
                   coordsAndData$Northing  ) 
pred <- predict(g1, gridPts, type="link",se.fit=FALSE)
pred1 <- data.frame(gridPts, Predicted = pred)



p3 <- ggplot(data = pred1 , aes(x = Easting, y = Northing )) +
  geom_raster(aes (fill = exp(Predicted))) + 
  scale_fill_viridis(option= 'plasma',  trans='log' ,alpha=.66, 
                     labels = scales::label_number(accuracy=.01),
                     breaks = exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), 
                                      length.out=11))) +
  #scale_fill_gradientn(colours = grey.colors(100, start=.9, end=.1)) +
  geom_contour(aes(z= exp(Predicted)),
               breaks = round(exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), length.out=11)),1),
               color= 'black', 
               linewidth=.5, show.legend=T) +
  #geom_contour_filled(aes(z= exp(Predicted)),
  #               breaks = round(exp(seq(min(pred1$Predicted), 
  #                               max(pred1$Predicted), length.out=11)),1),
  #             color= 'black', linewidth=.5, show.legend=T) +
  #scale_fill_grey(start=.99, end=.4) +
  #scale_fill_viridis(option= 'plasma', discrete= T, alpha =.66) +  
  geom_point(data= coordsAndData, aes(x=Easting, y=Northing), size=1 ) + 
  coord_fixed() +
  labs( title = 'White Salt Glaze',
        subtitle =  'Site 7',
        fill = 'Count',
        caption= paste ('Poisson GAM. Deviance explained: ', 
                        round(gam.sum$dev.expl,2),'. ',  
                        scales::pvalue(gam.sum$s.pv,
                                       accuracy = 0.001, # Number to round to
                                       decimal.mark = ".", # The character to be used 
                                       #to indicate the numeric decimal point
                                       add_p = TRUE) # Add "p=" before the value?
        )) +
  theme(plot.caption = element_text(face = "italic", size=12),
        legend.text = element_text(size = 12),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(.5, 'cm'),
        axis.text = element_text(size=10)) 

p3





# 10.4 Whieldon.type.Ware 

g1 <- gam(Whieldon.type.Ware        
          ~ s(Easting, Northing, k=100 ), 
          family=  poisson(link=log),  data=coordsAndData, method="REML")
gam.sum <- summary(g1)
AIC(g1)
gam.check(g1, pch=21, cex=2, bg='dark grey',  rep  =500, lwd=1)


gridPts<- makeGrid(gridSpacing=5, tooFar=25, coordsAndData$Easting, 
                   coordsAndData$Northing  ) 
pred <- predict(g1, gridPts, type="link",se.fit=FALSE)
pred1 <- data.frame(gridPts, Predicted = pred)


theme_set(theme_classic(base_size = 18))

p4 <- ggplot(data = pred1 , aes(x = Easting, y = Northing )) +
  geom_raster(aes (fill = exp(Predicted))) + 
  scale_fill_viridis(option= 'plasma',  trans='log' ,alpha=.66, 
                     labels = scales::label_number(accuracy=.01),
                     breaks = exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), 
                                      length.out=11))) +
  #scale_fill_gradientn(colours = grey.colors(100, start=.9, end=.1)) +
  geom_contour(aes(z= exp(Predicted)),
               breaks = round(exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), length.out=11)),1),
               color= 'black', 
               linewidth=.5, show.legend=T) +
  #geom_contour_filled(aes(z= exp(Predicted)),
  #               breaks = round(exp(seq(min(pred1$Predicted), 
  #                               max(pred1$Predicted), length.out=11)),1),
  #             color= 'black', linewidth=.5, show.legend=T) +
  #scale_fill_grey(start=.99, end=.4) +
  #scale_fill_viridis(option= 'plasma', discrete= T, alpha =.66) +  
  geom_point(data= coordsAndData, aes(x=Easting, y=Northing), size=1 ) + 
  coord_fixed() +
  labs( title = 'Whieldon',
        subtitle =  'Site 7',
        fill = 'Count',
        caption= paste ('Poisson GAM. Deviance explained: ', 
                        round(gam.sum$dev.expl,2),'. ',  
                        scales::pvalue(gam.sum$s.pv,
                                       accuracy = 0.001, # Number to round to
                                       decimal.mark = ".", # The character to be 
                                       #used to indicate the numeric decimal point
                                       add_p = TRUE) # Add "p=" before the value?
        )) +
  theme(plot.caption = element_text(face = "italic", size=12),
        legend.text = element_text(size = 12),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(.5, 'cm'),
        axis.text = element_text(size=10)) 

p4


# 10.5 Early Thin-Bodied Tea and Table Wares  


coordsAndData <- coordsAndData %>% 
  mutate(EarlyThinBodied = Whieldon.type.Ware + White.Salt.Glaze + Delft,
         LateThinBodied = Pearlware + Porcelain.Chinese + Creamware,
         ThinBodied = EarlyThinBodied + LateThinBodied)



g1 <- gam( EarlyThinBodied ~ s(Easting, Northing, k=100 ), 
          family=  poisson(link=log),  data=coordsAndData, method="REML")
gam.sum <- summary(g1)
AIC(g1)
gam.check(g1, pch=21, cex=2, bg='dark grey',  rep  =500, lwd=1)


gridPts<- makeGrid(gridSpacing=5, tooFar=25, coordsAndData$Easting, 
                   coordsAndData$Northing  ) 
pred <- predict(g1, gridPts, type="link",se.fit=FALSE)
pred1 <- data.frame(gridPts, Predicted = pred)


theme_set(theme_classic(base_size = 18))

p5 <- ggplot(data = pred1 , aes(x = Easting, y = Northing )) +
  geom_raster(aes (fill = exp(Predicted))) + 
  scale_fill_viridis(option= 'plasma',  trans='log' ,alpha=.66, 
                     labels = scales::label_number(accuracy=.01),
                     breaks = exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), 
                                      length.out=11))) +
  #scale_fill_gradientn(colours = grey.colors(100, start=.9, end=.1)) +
  geom_contour(aes(z= exp(Predicted)),
               breaks = round(exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), length.out=11)),1),
               color= 'black', 
               linewidth=.5, show.legend=T) +
  #geom_contour_filled(aes(z= exp(Predicted)),
  #               breaks = round(exp(seq(min(pred1$Predicted), 
  #                               max(pred1$Predicted), length.out=11)),1),
  #             color= 'black', linewidth=.5, show.legend=T) +
  #scale_fill_grey(start=.99, end=.4) +
  #scale_fill_viridis(option= 'plasma', discrete= T, alpha =.66) +  
  geom_point(data= coordsAndData, aes(x=Easting, y=Northing), size=1 ) + 
  coord_fixed() +
  labs( title = 'Early Thin-Bodied Wares',
        subtitle =  'Site 7',
        fill = 'Count',
        caption= paste ('Poisson GAM. Deviance explained: ', 
                        round(gam.sum$dev.expl,2),'. ',  
                        scales::pvalue(gam.sum$s.pv,
                                       accuracy = 0.001, # Number to round to
                                       decimal.mark = ".", # The character to be used 
                                       # to indicate the numeric decimal point
                                       add_p = TRUE) # Add "p=" before the value?
        )) +
  geom_text(x= 11499020, y=3891280, label='C1', color='black', size=8, 
            fontface='italic' ) +
  geom_text(x= 11499010, y=3891150, label='C2', color='black', size=8, 
            fontface='italic' ) +
  geom_text(x= 11499050, y=3891210, label='Borrow Pit', color='black', size=8, 
            fontface='italic' ) +
  theme(plot.caption = element_text(face = "italic", size=12),
        legend.text = element_text(size = 12),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(.5, 'cm'),
        axis.text = element_text(size=10)) 

p5



# 10.6 Put 4 early thin-bodied wares together in a plot using cowplot

Figure2 <- plot_grid(p5, p2, p3, p4,
                    # labels = c('1.', '2.', '3.', '4.'),
                     label_size =24,
                     nrow=1, ncol=4,
                     align="hv") 
Figure2

ggsave( Figure2, file = 'Figure2.png',  
        dpi=600, width=10, height=6, scale=2.5) 







# 10.7 Creamware

g1 <- gam( Creamware ~ s(Easting, Northing, k=100 ), 
           family=  poisson(link=log),  data=coordsAndData, method="REML")
gam.sum <- summary(g1)
AIC(g1)
gam.check(g1, pch=21, cex=2, bg='dark grey',  rep  =500, lwd=1)


gridPts<- makeGrid(gridSpacing=5, tooFar=25, coordsAndData$Easting, 
                   coordsAndData$Northing  ) 
pred <- predict(g1, gridPts, type="link",se.fit=FALSE)
pred1 <- data.frame(gridPts, Predicted = pred)


theme_set(theme_classic(base_size = 18))


p6 <- ggplot(data = pred1 , aes(x = Easting, y = Northing )) +
  geom_raster(aes (fill = exp(Predicted))) + 
  scale_fill_viridis(option= 'plasma',  trans='log' ,alpha=.66, 
                     labels = scales::label_number(accuracy=.01),
                     breaks = exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), 
                                      length.out=11))) +
  #scale_fill_gradientn(colours = grey.colors(100, start=.9, end=.1)) +
  geom_contour(aes(z= exp(Predicted)),
               breaks = round(exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), length.out=11)),1),
               color= 'black', 
               linewidth=.5, show.legend=T) +
  #geom_contour_filled(aes(z= exp(Predicted)),
  #               breaks = round(exp(seq(min(pred1$Predicted), 
  #                               max(pred1$Predicted), length.out=11)),1),
  #             color= 'black', linewidth=.5, show.legend=T) +
  #scale_fill_grey(start=.99, end=.4) +
  #scale_fill_viridis(option= 'plasma', discrete= T, alpha =.66) +  
  geom_point(data= coordsAndData, aes(x=Easting, y=Northing), size=1 ) + 
  coord_fixed() +
  labs( title = 'Creamware',
        subtitle =  'Site 7',
        fill = 'Count',
        caption= paste ('Poisson GAM. Deviance explained: ', 
                        round(gam.sum$dev.expl,2),'. ',  
                        scales::pvalue(gam.sum$s.pv,
                                       accuracy = 0.001, # Number to round to
                                       decimal.mark = ".", # The character to be used 
                                       # to indicate the numeric decimal point
                                       add_p = TRUE) # Add "p=" before the value?
        )) +
  theme(plot.caption = element_text(face = "italic", size=12),
        legend.text = element_text(size = 12),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(.5, 'cm'),
        axis.text = element_text(size=10)) 

p6



# 10.8 Chinese.Porcelain

g1 <- gam(Porcelain.Chinese ~ s(Easting, Northing, k=100 ), 
           family=  poisson(link=log),  data=coordsAndData, method="REML")
gam.sum <- summary(g1)
AIC(g1)
gam.check(g1, pch=21, cex=2, bg='dark grey',  rep  =500, lwd=1)


gridPts<- makeGrid(gridSpacing=5, tooFar=25, coordsAndData$Easting, 
                   coordsAndData$Northing  ) 
pred <- predict(g1, gridPts, type="link",se.fit=FALSE)
pred1 <- data.frame(gridPts, Predicted = pred)


theme_set(theme_classic(base_size = 18))


p7 <- ggplot(data = pred1 , aes(x = Easting, y = Northing )) +
  geom_raster(aes (fill = exp(Predicted))) + 
  scale_fill_viridis(option= 'plasma',  trans='log' ,alpha=.66, 
                     labels = scales::label_number(accuracy=.01),
                     breaks = exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), 
                                      length.out=11))) +
  #scale_fill_gradientn(colours = grey.colors(100, start=.9, end=.1)) +
  geom_contour(aes(z= exp(Predicted)),
               breaks = round(exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), length.out=11)),1),
               color= 'black', 
               linewidth=.5, show.legend=T) +
  #geom_contour_filled(aes(z= exp(Predicted)),
  #               breaks = round(exp(seq(min(pred1$Predicted), 
  #                               max(pred1$Predicted), length.out=11)),1),
  #             color= 'black', linewidth=.5, show.legend=T) +
  #scale_fill_grey(start=.99, end=.4) +
  #scale_fill_viridis(option= 'plasma', discrete= T, alpha =.66) +  
  geom_point(data= coordsAndData, aes(x=Easting, y=Northing), size=1 ) + 
  coord_fixed() +
  labs( title = 'Chinese Porcelain',
        subtitle =  'Site 7',
        fill = 'Count',
        caption= paste ('Poisson GAM. Deviance explained: ', 
                        round(gam.sum$dev.expl,2),'. ',  
                        scales::pvalue(gam.sum$s.pv,
                                       accuracy = 0.001, # Number to round to
                                       decimal.mark = ".", # The character to be used 
                                       # to indicate the numeric decimal point
                                       add_p = TRUE) # Add "p=" before the value?
        )) +
  theme(plot.caption = element_text(face = "italic", size=12),
        legend.text = element_text(size = 12),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(.5, 'cm'),
        axis.text = element_text(size=10)) 

p7



# 10.9 Pearlware

g1 <- gam(Pearlware ~ s(Easting, Northing, k=100 ), 
          family=  poisson(link=log),  data=coordsAndData, method="REML")
gam.sum <- summary(g1)
AIC(g1)
gam.check(g1, pch=21, cex=2, bg='dark grey',  rep  =500, lwd=1)


gridPts<- makeGrid(gridSpacing=5, tooFar=25, coordsAndData$Easting, 
                   coordsAndData$Northing  ) 
pred <- predict(g1, gridPts, type="link",se.fit=FALSE)
pred1 <- data.frame(gridPts, Predicted = pred)


theme_set(theme_classic(base_size = 18))


p8 <- ggplot(data = pred1 , aes(x = Easting, y = Northing )) +
  geom_raster(aes (fill = exp(Predicted))) + 
  scale_fill_viridis(option= 'plasma',  trans='log' ,alpha=.66, 
                     labels = scales::label_number(accuracy=.01),
                     breaks = exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), 
                                      length.out=11))) +
  #scale_fill_gradientn(colours = grey.colors(100, start=.9, end=.1)) +
  geom_contour(aes(z= exp(Predicted)),
               breaks = round(exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), length.out=11)),1),
               color= 'black', 
               linewidth=.5, show.legend=T) +
  #geom_contour_filled(aes(z= exp(Predicted)),
  #               breaks = round(exp(seq(min(pred1$Predicted), 
  #                               max(pred1$Predicted), length.out=11)),1),
  #             color= 'black', linewidth=.5, show.legend=T) +
  #scale_fill_grey(start=.99, end=.4) +
  #scale_fill_viridis(option= 'plasma', discrete= T, alpha =.66) +  
  geom_point(data= coordsAndData, aes(x=Easting, y=Northing), size=1 ) + 
  coord_fixed() +
  labs( title = 'Pearlware',
        subtitle =  'Site 7',
        fill = 'Count',
        caption= paste ('Poisson GAM. Deviance explained: ', 
                        round(gam.sum$dev.expl,2),'. ',  
                        scales::pvalue(gam.sum$s.pv,
                                       accuracy = 0.001, # Number to round to
                                       decimal.mark = ".", # The character to be used 
                                       # to indicate the numeric decimal point
                                       add_p = TRUE) # Add "p=" before the value?
        )) +
  theme(plot.caption = element_text(face = "italic", size=12),
        legend.text = element_text(size = 12),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(.5, 'cm'),
        axis.text = element_text(size=10)) 

p8

# 10.10 Late Thin-Bodied Wares

g1 <- gam(LateThinBodied ~ s(Easting, Northing, k=120 ), 
          family=  poisson(link=log),  data=coordsAndData, method="REML")
gam.sum <- summary(g1)
AIC(g1)
gam.check(g1, pch=21, cex=2, bg='dark grey',  rep  =500, lwd=1)


gridPts<- makeGrid(gridSpacing=5, tooFar=25, coordsAndData$Easting, 
                   coordsAndData$Northing  ) 
pred <- predict(g1, gridPts, type="link",se.fit=FALSE)
pred1 <- data.frame(gridPts, Predicted = pred)


theme_set(theme_classic(base_size = 18))

p9 <- ggplot(data = pred1 , aes(x = Easting, y = Northing )) +
  geom_raster(aes (fill = exp(Predicted))) + 
  scale_fill_viridis(option= 'plasma',  trans='log' ,alpha=.66, 
                     labels = scales::label_number(accuracy=.01),
                     breaks = exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), 
                                      length.out=11))) +
  #scale_fill_gradientn(colours = grey.colors(100, start=.9, end=.1)) +
  geom_contour(aes(z= exp(Predicted)),
               breaks = round(exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), length.out=11)),1),
               color= 'black', 
               linewidth=.5, show.legend=T) +
  #geom_contour_filled(aes(z= exp(Predicted)),
  #               breaks = round(exp(seq(min(pred1$Predicted), 
  #                               max(pred1$Predicted), length.out=11)),1),
  #             color= 'black', linewidth=.5, show.legend=T) +
  #scale_fill_grey(start=.99, end=.4) +
  #scale_fill_viridis(option= 'plasma', discrete= T, alpha =.66) +  
  geom_point(data= coordsAndData, aes(x=Easting, y=Northing), size=1 ) + 
  coord_fixed() +
  labs( title = 'Late Thin-Bodied Wares',
        subtitle =  'Site 7',
        fill = 'Count',
        caption= paste ('Poisson GAM. Deviance explained: ', 
                        round(gam.sum$dev.expl,2),'. ',  
                        scales::pvalue(gam.sum$s.pv,
                                       accuracy = 0.001, # Number to round to
                                       decimal.mark = ".", # The character to be used 
                                       # to indicate the numeric decimal point
                                       add_p = TRUE) # Add "p=" before the value?
        )) +
  geom_text(x= 11498970, y=3891250, label='C3', color='black', size=8, 
            fontface='italic' ) +
  geom_text(x= 11498980, y=3891200, label='C4', color='black', size=8, 
            fontface='italic' ) +
  geom_text(x= 11499050, y=3891210, label='Borrow Pit', color='black', size=8, 
            fontface='italic' ) +
  theme(plot.caption = element_text(face = "italic", size=12),
        legend.text = element_text(size = 12),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(.5, 'cm'),
        axis.text = element_text(size=10)) 

p9




# 10.11 Put 3 Late thin-bodied wares together in a plot using cowplot

Figure3 <- plot_grid(p9, p6, p7, p8,
                    # labels = c('1.', '2.', '3.', '4.'),
                    label_size =24,
                    nrow=1, ncol=4,
                    align="hv") 
Figure3

ggsave( Figure3, file = 'Figure3.png',  
        dpi=600, width=10, height=6, scale=2.5) 


# 10.12 Put  early and late thin-bodied wares together in a plot using cowplot

Figure4 <- plot_grid(p5, p9,
                     #labels = c('1.', '2.'),
                     label_size =24,
                     align="h",
                     nrow=1, ncol=2) 
Figure4

ggsave( Figure4, file = 'Figure4.png',  
        dpi=600, width=10, height=6, scale=1.5) 




#### 11.  prep the data for CA -- get rid of quads with no ceramics ####

coordsAndData1 <- coordsAndData %>% 
  select (ProjectID, QuadratID, Northing, Easting,
   Astbury.Type ,      
   #British.Stoneware,
   #Coarse.Earthenware.unidentified,
    Creamware,
    Pearlware,
  # Redware,
  # Refined.Earthenware.unidentifiable,
    Whieldon.type.Ware,
  # Delftware.Dutch.British,
    Porcelain.Chinese,
  # Slipware.North.Midlands.Staffordshire,
  #  Westerwald.Rhenish,
    White.Salt.Glaze,
  # Tin.Enameled.unidentified,
    Jackfield.Type,
    Nottingham,
    Staffordshire.Mottled.Glaze,
    Delft
  # Native.American,
  # American.Stoneware,
  # British.Brown.Fulham.Type,
  # totalCount
  )
nonZeroIndex <- rowSums(coordsAndData1[,-1:-4]) > 0
coordsAndData1 <- coordsAndData1[nonZeroIndex,]




####  12. define a function to compute a variogram #### 
# using chi-square and Euclidean distances for the assemblages.
vGram <- function(coords, counts, from, to, lagDist){
  #  coords <- as.matrix(coordsAndData[,3:4])
  #  counts <- as.matrix(coordsAndData[,5:20])
  #  from <-  0
  #  to <- 150
  #  lagDist <- 10
  coords <- as.matrix(coords)
  counts <- as.matrix(counts)
  nQuads <- nrow(counts)
  #postMat <- matrix(0, nrow=nQuads, ncol = nVars)  
  # compute distances from one point to others
  propCounts <- prop.table(counts,1)
  propMeans <- colSums(counts)/sum(counts)
  eucDist <- geoDist <- chi2Dist <- matrix(0, nrow=nQuads, ncol = nQuads)
  for (i in 1:nQuads){
    geoDist[,i] <- apply(coords, 1, function(x)
      {sqrt(sum((x-coords[i,])^2))})
    eucDist[,i] <- apply(propCounts, 1, function(x)
      {sqrt(sum(((x-propCounts[i,])^2)))})
    chi2Dist[,i] <- apply(propCounts, 1, function(x)
    {sqrt(sum(((x-propCounts[i,])^2)/propMeans))})
  }  
  geoDist <- geoDist[upper.tri(geoDist)]
  eucDist <- eucDist[upper.tri(eucDist)]
  chi2Dist <- chi2Dist[upper.tri(chi2Dist)]
  binLimits <- seq(from, to, lagDist)
  meanGeoDist <- rep(0,length(binLimits)-1)
  meanEucDist <- meanChi2Dist <- meanGeoDist
  for (i in 1:length(binLimits)-1){
       pairs <-   (geoDist > binLimits[i]) & (geoDist <= binLimits[i+1])
       meanGeoDist[i] <- mean(geoDist[pairs])
       meanEucDist[i] <- mean(eucDist[pairs])
       meanChi2Dist[i] <- mean(chi2Dist[pairs])
       }
  return(data.frame( meanGeoDist= meanGeoDist, 
                     meanEucDist = meanEucDist, 
                     meanChi2Dist = meanChi2Dist))
  }


#### 13. Compute the variograms ####
varioGram <- vGram (coordsAndData1[,3:4], coordsAndData1[,5:ncol(coordsAndData1)],
                      from =0, to=150, lagDist=10 ) 

 theme_set(theme_classic(base_size = 18))
p10 <- ggplot(data=varioGram , aes(meanGeoDist, y=meanChi2Dist)) +
  # geom_bar(stat="identity", fill="grey") +
  geom_line(col= "grey", size=2) +
  geom_point(shape=21, size=5, colour="black", fill="grey") +
  labs( title="Variogram, Thin-Bodied Wares ",
        subtitle = 'Site 7',
        x="Mean Distance (feet)", y='Mean Chi-square Distance' )   
p10

theme_set(theme_classic(base_size = 18))
p11 <- ggplot(data=varioGram , aes(meanGeoDist, y=meanEucDist)) +
  # geom_bar(stat="identity", fill="grey") +
  geom_line(col= "grey", size=2) +
  geom_point(shape=21, size=5, colour="black", fill="grey") +
  labs( title = "Variogram, Thin-Bodied Wares ",
        subtitle = 'Site 7',
        x="Mean Distance (feet)", y='Mean Euclidean Distance' )   
p11


ggsave( p10, file = 'Figure6.png',  
        dpi=600, width=10, height=6, scale=1) 




#### 14.  Define a function to the the EB Dirichlet-multinomial smooth #### 
dmnSmooth <- function(ID, coords, counts, radius){
# For each spatial unit (STP or quad) estimate a Dirichlet 
# prior based on units with a specified radius and a Dirichlet
# posterior for the unit. 
#  radius <- 40
#  ID <- coordsAndData[,2]
#  coords <- as.matrix(coordsAndData[,3:4])
#  counts <- as.matrix(coordsAndData[,5:10])
  coords <- as.matrix(coords)
  counts <- as.matrix(counts)
  nVars <- ncol(counts)
  nQuads <- nrow(counts)
  postMat <- matrix(0, nrow=nQuads, ncol = nVars)  
  # compute distances from one point to others
for (i in 1:nQuads){
  distP <- apply(coords, 1, function(x){sqrt(sum((x-coords[i,])^2))}) 
  # get the counts for the neighborhood
  nbrhdMat <- counts[(distP <= (radius*1.01)),]
  # pull out cols with non-zero col sums
  nonZeroCols <- (colSums(nbrhdMat) > 0)
  nbrhdMat1 <- nbrhdMat[,nonZeroCols]
  # estimate the Dirichlet prior
  fit1 <- dmn(k=1, count = nbrhdMat1 , verbose=F)
  fit1
  prior1 <- fit1@fit$Estimate
  # add back the cols with the 0 sums
  prior <- rep(0,nVars)
  prior[nonZeroCols] <- prior1
  postMat[i,] <- prior + counts[i,]
}
colnames(postMat) <- colnames(counts)
rownames(postMat) <- ID
return(list(coords=coords,postMat=postMat ))}

#### 14.1 Call the DNM functionn ####

sm50 <- dmnSmooth( ID = coordsAndData1[,2],
                   coords = coordsAndData1[,3:4],
                   counts = coordsAndData1[,5:ncol(coordsAndData1)],
                   radius= 50 )

#### 14.2 Do the CA and plots #### 

# 14.3 Randomized in inertias for plotting 

##### Define a function to compute the actual proportion of inertia
# accounted for by CA dimensions and the expected proportion, based on 
# random permutation of the elements within each row of the data matrix.
# Both the mean and 95% confidence interval are computed for the randomizations.
get_Randomized_Inertias <- function(n_times, data){
  # Arguments: n_times: The number of randomizations.
  #            data:    A data frame or matrix that contains ONLY counts.
  # Function to permute elements within a row
  permute_Row <- function(row) {return(sample(row))}
  # Function to permute elements within all rows 
  permute_Matrix_Rows <-function(data){
    repeat{permuted_matrix <- t(apply(data, 1, permute_Row))
    # Make sure no colsums are zeros
    if(sum(colSums(permuted_matrix)==0)==0){break}}
    return(permuted_matrix)}
  # Function to compute % inertias
  get_Inertias <- function(data){
    p <- data/sum(data)
    # row and column marginal sums
    row_masses <- rowSums(p)
    col_masses <- colSums(p)
    # expected values
    e <- row_masses %o% col_masses
    # residual matrix
    i <- (p-e)/e
    z <- i*sqrt(e) 
    # SVD
    svd_result <- svd(z)
    # Extract the singular values as % inertias
    round(svd_result$d^2/sum(svd_result$d^2),3)
  }
  # call the functions
  random_inertias <- replicate(n = n_times, 
                               expr = get_Inertias(data = permute_Matrix_Rows(
                                 data=data)))
  # get the means and CLs
  means_and_cls <- data.frame(t(apply(random_inertias, 1, function(x) 
    c(mean = mean(x), quantile(x, c(0.025, 0.975))))))
  colnames(means_and_cls) <- c('mean', 'lcl', 'ucl')
  means_and_cls$actual <- get_Inertias(data=data)
  means_and_cls$Dimension <- 1:nrow(means_and_cls)
  return(means_and_cls[-nrow(means_and_cls),])
}
##### End of function

# run the function

means_and_cls <- get_Randomized_Inertias(n_times=1000,prop.table(sm50$postMat,1))

# plot the results

theme_set(theme_classic(base_size = 18))  
p10 <- ggplot(data=means_and_cls, aes(x = Dimension, y=actual)) +
  scale_x_continuous(n.breaks = nrow(means_and_cls)) +
  geom_line(aes(y=mean), lty=2, col='black', linewidth=1) +
  geom_ribbon(aes(x= Dimension, ymin=lcl, ymax=ucl), 
              col='gray', alpha=.1) +
  geom_line(aes(y=actual), col= 'grey', linewidth=2) +
  geom_point(shape=21, size=5, colour="black", fill="grey") +
  labs( title="CA: Thin-Bodied Sherds",
        subtitle = '',
        x="Dimension", y='Proportion of Inertia')
p10

ggsave( p10, file = 'Figure7.png',  
        dpi=600, width=10, height=6, scale=1) 


ca1 <- ca(prop.table(sm50$postMat,1))

# Put the results in data frames 
# We convert inertia to percent inertia 
inertia <- data.frame('Inertia' = prop.table(ca1$sv^2))
# We only take the first five CA dimensions for row and col scores
rowScores <- data.frame(ca1$rowcoord[,1:5], 
                        unit =ca1$rownames)
colScores <- data.frame(ca1$colcoord[,1:5], 
                        type =ca1$colnames)


# 14.21  scale rowcoords by SVs

scaledRowScores <-  ca1$rowcoord[,1:5] * matrix(ca1$sv[1:5], 
                                         nrow= nrow(ca1$rowcoord), 
                                         ncol =5, byrow=T  )
scaledRowScores <- data.frame (unit=rownames(scaledRowScores), scaledRowScores)


##### 14.4 Now we plot the row and col scores on CA Dim 1 and Dim2 ####

# We can uncomment the call to geom_text_repel(). But the result is messy.
set.seed(42)
p11 <- ggplot(scaledRowScores, aes(x=Dim1,y=Dim2)) +
  coord_fixed() +
  geom_point(shape=21, size=5, colour="black", alpha=.75, fill='grey') +
  geom_hline(yintercept = 0, linetype='dashed', color='grey') +
  geom_vline(xintercept = 0, linetype='dashed', color='grey') +
  #geom_text_repel(aes(label= unit), cex = 4, max.overlaps=50) +
  labs(title = "Quadrat Scores",
       subtitle = 'Site 7',
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep='')) 
p11


# Plot the column  (ware type) scores on CA Dim 1 and Dim 2
p12 <- ggplot(colScores, aes(x = Dim1,y = Dim2)) +
  #coord_fixed() +
  geom_point(shape=21, size=5, colour="black", fill="yellow", alpha=.75) +
  geom_text_repel(aes(label=type), cex= 5, force=2) +
  geom_hline(yintercept = 0, linetype='dashed', color='grey') +
  geom_vline(xintercept = 0, linetype='dashed', color='grey') +
  labs(title = 'Ware Type Scores',
       subtitle = 'Site 7',
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep=''))  
p12

Figure8 <- plot_grid(p11, p12,
                     #labels = c('1.', '2.'),
                     label_size =24,
                     align="h",
                     nrow=1, ncol=2) 
Figure8

ggsave( Figure8, file = 'Figure8.png',  
        dpi=600, width=10, height=4, scale=1.5) 

##### 14.5 KDEs  for phasing ####
# replicate scores by assemblage sample size 
dim1ForHist<- data.frame(dim1 = rep(scaledRowScores$Dim1,
                                    rowSums(coordsAndData1[,5:ncol(coordsAndData1)])),
                         dim2 = rep(scaledRowScores$Dim2,
                                    rowSums(coordsAndData1[,5:ncol(coordsAndData1)])),
                         dim3 = rep(scaledRowScores$Dim3,
                                    rowSums(coordsAndData1[,5:ncol(coordsAndData1)])))

# 2-d weighted KDE

p13 <- ggplot(dim1ForHist, aes(x = dim1, y = dim2)) +
  coord_fixed() +
  geom_point(shape=21, size=5, colour="black", fill="lightblue", alpha=.50) + 
  geom_density_2d(color = 'black', size=1, adjust = 1,  alpha=.5, bins=11 ) +
  labs(title="Site 7", x="Dimension 1", y="Dimension 2") 
  #geom_vline(xintercept=c(-1.3, .2), colour = "blue", linetype = "dashed",
  #           size=1)
p13 



# 1-d Weighted Histogram

p14 <- ggplot(dim1ForHist, aes(x = dim1)) +
  #coord_fixed() +
  geom_histogram(aes(y= after_stat(density)), fill="grey", color="#e9ecef", binwidth =.05)+ 
  geom_density (color = 'black', size=1, adjust = 1, bw='nrd0')  +
  labs(title="Frequency Distribution of Quadrat Scores",
       subtitle = 'Site 7',
       x="CA Dimension 1", 
       y= 'Density') +
geom_vline(xintercept=c(-.6, .1, .4), colour = "black", linetype = "dashed",
           size=1) +
  geom_text(x= -.9, y= 2, label="1", size=10) +
  geom_text(x= -.25, y= 2, label="2", size=10) +
  geom_text(x= .25, y= 2, label="3", size=10) +
  geom_text(x= .5, y= 2, label="4", size=10)
p14


ggsave( p14, file = 'Figure9a.png',  
        dpi=600, width=10, height=6, scale=1.25) 


#### 14.6 Assign the Phases based on CA scores ####

scaledRowScores1 <- scaledRowScores %>% 
  mutate (Phase = case_when(Dim1 < -.6 ~ '1',
                           (Dim1 >= - .6 ) & (Dim1 < .1) ~ '2',
                           (Dim1 >= .1) & (Dim1 < .4) ~ '3',
                           (Dim1 >= .4) ~ '4'))
                           
          


##### 14.7 Map the CA Phases ####


scaledRowScores1WCoords <- inner_join (scaledRowScores1 , coordsAndData1, 
                                by= c('unit'= 'QuadratID') )


theme_set(theme_classic(base_size = 18))
p15 <- ggplot(data= scaledRowScores1WCoords, aes(x=Easting, y=Northing, fill= Phase)) +
  geom_point(size=5, shape=22, alpha= .66)+ 
  scale_fill_viridis_d(option='magma') +
  geom_text_repel( aes(label=Phase), size=5) +
  coord_fixed() +
  labs( title = 'CA Phases',
        subtitle = 'Site 7',
        caption= '')
p15
ggsave( p15, file = 'Figure9b.png',  
        dpi=600, width=10, height=8, scale=1.25) 



#### 14.8 Do a battleship plot based on CA Dim1 ####

# First define a  function to sort the rows and cols of a matrix based on the
# orders from two arguments (e.g. Row scores and Col scores from CA)
# arguments:  the name of the variable that contains the row scores. 
#             the name of the variable that contains the col scores. 
#             the name of the dataframe that contains the counts of ware types 
#               in units
# returns:    the sorted dataframe 
sortData<- function(unitScores,typeScores,unitData){
  sortedData<-unitData[order(unitScores, decreasing=T),]
  sortedData<-sortedData[,c(1,order(typeScores)+1)]
  return(sortedData)
}

unitData <- data.frame(QuadratID = rownames(sm50$postMat),sm50$postMat )

unitData_Sorted  <- sortData(scaledRowScores$Dim1, colScores$Dim1, 
                                unitData)
Mat<-as.matrix(unitData_Sorted[,-1])
rownames(Mat)<- NULL
propMat <- prop.table(Mat,1)
# Do the plot

# Figure 10 
battleship.plot(propMat,
                mar=c(2,2,15,2),
                cex.labels=2,
                yaxlab= '',
                #main = 'Seriation by CA Dimension 1',
                xlab='Ware Type',
                col='grey')


# put the Dim 1 row coords in a matrix
scaledRowScoresDim1Mat <- matrix(scaledRowScores$Dim1, nrow=nrow(scaledRowScores),
                                 ncol = nrow(ca1$colcoord), byrow = F)
# put the Dim-1 col coords in a matrix                                                        
colScoresDim1Mat <- matrix(ca1$colcoord[,1], nrow=nrow(scaledRowScores),
                           ncol = nrow(ca1$colcoord), byrow = T)
# compute the assemblage variances
assemblageVar <- rowSums(((colScoresDim1Mat - scaledRowScoresDim1Mat)^2)*
                           prop.table(sm50$postMat,1))
# compute the assemblage ranges based on uniform distribution 

dim1AssemVar <- data.frame(scaledRowScores= scaledRowScores$Dim1,
                           assemVar = assemblageVar,
                           assemRange = sqrt(12*assemblageVar))


theme_set(theme_classic(base_size = 18))
p16 <- ggplot(data=dim1AssemVar , aes(scaledRowScores, y=scaledRowScores)) +
  coord_fixed() +
  geom_errorbar(aes(ymin= scaledRowScores - (assemRange/2), 
                    ymax= scaledRowScores + (assemRange/2), 
                    width=0)) +
  geom_point(shape=21, size=5, color="black", fill="grey", alpha=.75) +
  labs( title="CA, Thin-Bodied Wares",
        subtitle = 'Site 7',
        x="Dimension 1", y='Range' ) +
  theme(legend.position="none")+
  geom_vline(xintercept=c(-.6, .1, .4), colour = "black", linetype = "dashed",
                                           size=1)
p16


theme_set(theme_classic(base_size = 18))
p17 <- ggplot(data=dim1AssemVar , aes(scaledRowScores, y= sqrt(assemVar))) +
  geom_point(shape=21, size=5, color="black", fill="grey", alpha=.75) +
  labs( title="Time Averaging",
        subtitle = 'Site 7',
        x="Dimension 1", y='Assemblage Standard Deviation.') +
  geom_vline(xintercept=c(-.6, .1, .4), colour = "black", linetype = "dashed",
             size=1) +
  geom_text(x= -.9, y= .5, label="1", size=10) +
  geom_text(x= -.25, y= .5, label="2", size=10) +
  geom_text(x= .25, y= .5, label="3", size=10) +
  geom_text(x= .5, y= .5, label="4", size=10)
p17



#### 15. GAMs for Thick-Bodied wares ####

# 15.1 Coarse Earthenware
g1 <- gam( Coarse.Earthenware.unidentified        
          ~ s(Easting, Northing, k=100 ), 
          family=  poisson(link=log),  data=coordsAndData, method="REML")
gam.sum <- summary(g1)
gam.sum
AIC(g1)
gam.check(g1, pch=21, cex=2, bg='dark grey',  rep  =500, lwd=1)


gridPts<- makeGrid(gridSpacing=5, tooFar=25, coordsAndData$Easting, 
                   coordsAndData$Northing  ) 
pred <- predict(g1, gridPts, type="link",se.fit=FALSE)
pred1 <- data.frame(gridPts, Predicted = pred)


theme_set(theme_classic(base_size = 18))


p18 <- ggplot(data = pred1 , aes(x = Easting, y = Northing )) +
  geom_raster(aes (fill = exp(Predicted))) + 
  scale_fill_viridis(option= 'plasma',  trans='log' ,alpha=.66, 
                     labels = scales::label_number(accuracy=.01),
                     breaks = exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), 
                                      length.out=11))) +
  #scale_fill_gradientn(colours = grey.colors(100, start=.9, end=.1)) +
  geom_contour(aes(z= exp(Predicted)),
               breaks = round(exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), length.out=11)),1),
               color= 'black', 
               linewidth=.5, show.legend=T) +
  #geom_contour_filled(aes(z= exp(Predicted)),
  #               breaks = round(exp(seq(min(pred1$Predicted), 
  #                               max(pred1$Predicted), length.out=11)),1),
  #             color= 'black', linewidth=.5, show.legend=T) +
  #scale_fill_grey(start=.99, end=.4) +
  #scale_fill_viridis(option= 'plasma', discrete= T, alpha =.66) +  
  geom_point(data= coordsAndData, aes(x=Easting, y=Northing), size=1 ) + 
  coord_fixed() +
  labs( title = 'Coarse Earthenware, Unidentified',
        subtitle =  'Site 7',
        fill = 'Count',
        caption= paste ('Poisson GAM. Deviance explained: ', 
                        round(gam.sum$dev.expl,2),'. ',  
                        scales::pvalue(gam.sum$s.pv,
                                       accuracy = 0.001, # Number to round to
                                       decimal.mark = ".", # The character to be used 
                                       # to indicate the numeric decimal point
                                       add_p = TRUE) # Add "p=" before the value?
        )) +
  theme(plot.caption = element_text(face = "italic", size=12),
        legend.text = element_text(size = 12),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(.5, 'cm'),
        axis.text = element_text(size=10)) 

p18


# 15.2 Redware
g1 <- gam( Redware        
           ~ s(Easting, Northing, k=100 ), 
           family=  poisson(link=log),  data=coordsAndData, method="REML")
gam.sum <- summary(g1)
gam.sum
AIC(g1)
gam.check(g1, pch=21, cex=2, bg='dark grey',  rep  =500, lwd=1)


gridPts<- makeGrid(gridSpacing=5, tooFar=25, coordsAndData$Easting, 
                   coordsAndData$Northing  ) 
pred <- predict(g1, gridPts, type="link",se.fit=FALSE)
pred1 <- data.frame(gridPts, Predicted = pred)


theme_set(theme_classic(base_size = 18))


p19 <- ggplot(data = pred1 , aes(x = Easting, y = Northing )) +
  geom_raster(aes (fill = exp(Predicted))) + 
  scale_fill_viridis(option= 'plasma',  trans='log' ,alpha=.66, 
                     labels = scales::label_number(accuracy=.01),
                     breaks = exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), 
                                      length.out=11))) +
  #scale_fill_gradientn(colours = grey.colors(100, start=.9, end=.1)) +
  geom_contour(aes(z= exp(Predicted)),
               breaks = round(exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), length.out=11)),1),
               color= 'black', 
               linewidth=.5, show.legend=T) +
  #geom_contour_filled(aes(z= exp(Predicted)),
  #               breaks = round(exp(seq(min(pred1$Predicted), 
  #                               max(pred1$Predicted), length.out=11)),1),
  #             color= 'black', linewidth=.5, show.legend=T) +
  #scale_fill_grey(start=.99, end=.4) +
  #scale_fill_viridis(option= 'plasma', discrete= T, alpha =.66) +  
  geom_point(data= coordsAndData, aes(x=Easting, y=Northing), size=1 ) + 
  coord_fixed() +
  labs( title = 'Redware',
        subtitle =  'Site 7',
        fill = 'Count',
        caption= paste ('Poisson GAM. Deviance explained: ', 
                        round(gam.sum$dev.expl,2),'. ',  
                        scales::pvalue(gam.sum$s.pv,
                                       accuracy = 0.001, # Number to round to
                                       decimal.mark = ".", # The character to be used 
                                       # to indicate the numeric decimal point
                                       add_p = TRUE) # Add "p=" before the value?
        )) +
  theme(plot.caption = element_text(face = "italic", size=12),
        legend.text = element_text(size = 12),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(.5, 'cm'),
        axis.text = element_text(size=10)) 

p19


# 15.3 Slipware.North.Midlands.Staffordshire 
g1 <- gam( Slipware.North.Midlands.Staffordshire         
           ~ s(Easting, Northing, k=100 ), 
           family=  poisson(link=log),  data=coordsAndData, method="REML")
gam.sum <- summary(g1)
gam.sum
AIC(g1)
gam.check(g1, pch=21, cex=2, bg='dark grey',  rep  =500, lwd=1)


gridPts<- makeGrid(gridSpacing=5, tooFar=25, coordsAndData$Easting, 
                   coordsAndData$Northing  ) 
pred <- predict(g1, gridPts, type="link",se.fit=FALSE)
pred1 <- data.frame(gridPts, Predicted = pred)


theme_set(theme_classic(base_size = 18))


p20 <- ggplot(data = pred1 , aes(x = Easting, y = Northing )) +
  geom_raster(aes (fill = exp(Predicted))) + 
  scale_fill_viridis(option= 'plasma',  trans='log' ,alpha=.66, 
                     labels = scales::label_number(accuracy=.01),
                     breaks = exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), 
                                      length.out=11))) +
  #scale_fill_gradientn(colours = grey.colors(100, start=.9, end=.1)) +
  geom_contour(aes(z= exp(Predicted)),
               breaks = round(exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), length.out=11)),1),
               color= 'black', 
               linewidth=.5, show.legend=T) +
  #geom_contour_filled(aes(z= exp(Predicted)),
  #               breaks = round(exp(seq(min(pred1$Predicted), 
  #                               max(pred1$Predicted), length.out=11)),1),
  #             color= 'black', linewidth=.5, show.legend=T) +
  #scale_fill_grey(start=.99, end=.4) +
  #scale_fill_viridis(option= 'plasma', discrete= T, alpha =.66) +  
  geom_point(data= coordsAndData, aes(x=Easting, y=Northing), size=1 ) + 
  coord_fixed() +
  labs( title = 'Slipware, North Midlands',
        subtitle =  'Site 7',
        fill = 'Count',
        caption= paste ('Poisson GAM. Deviance explained: ', 
                        round(gam.sum$dev.expl,2),'. ',  
                        scales::pvalue(gam.sum$s.pv,
                                       accuracy = 0.001, # Number to round to
                                       decimal.mark = ".", # The character to be used 
                                       # to indicate the numeric decimal point
                                       add_p = TRUE) # Add "p=" before the value?
        )) +
  theme(plot.caption = element_text(face = "italic", size=12),
        legend.text = element_text(size = 12),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(.5, 'cm'),
        axis.text = element_text(size=10)) 

p20





# 15.4  British.Brown.Stoneware  
g1 <- gam(  British.Brown.Stoneware           
           ~ s(Easting, Northing, k=100 ), 
           family=  poisson(link=log),  data=coordsAndData, method="REML")
gam.sum <- summary(g1)
gam.sum
AIC(g1)
gam.check(g1, pch=21, cex=2, bg='dark grey',  rep  =500, lwd=1)


gridPts<- makeGrid(gridSpacing=5, tooFar=25, coordsAndData$Easting, 
                   coordsAndData$Northing  ) 
pred <- predict(g1, gridPts, type="link",se.fit=FALSE)
pred1 <- data.frame(gridPts, Predicted = pred)


theme_set(theme_classic(base_size = 18))


p21 <- ggplot(data = pred1 , aes(x = Easting, y = Northing )) +
  geom_raster(aes (fill = exp(Predicted))) + 
  scale_fill_viridis(option= 'plasma',  trans='log' ,alpha=.66, 
                     labels = scales::label_number(accuracy=.01),
                     breaks = exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), 
                                      length.out=11))) +
  #scale_fill_gradientn(colours = grey.colors(100, start=.9, end=.1)) +
  geom_contour(aes(z= exp(Predicted)),
               breaks = round(exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), length.out=11)),1),
               color= 'black', 
               linewidth=.5, show.legend=T) +
  #geom_contour_filled(aes(z= exp(Predicted)),
  #               breaks = round(exp(seq(min(pred1$Predicted), 
  #                               max(pred1$Predicted), length.out=11)),1),
  #             color= 'black', linewidth=.5, show.legend=T) +
  #scale_fill_grey(start=.99, end=.4) +
  #scale_fill_viridis(option= 'plasma', discrete= T, alpha =.66) +  
  geom_point(data= coordsAndData, aes(x=Easting, y=Northing), size=1 ) + 
  coord_fixed() +
  labs( title = 'British Brown Stoneware  ',
        subtitle =  'Site 7',
        fill = 'Count',
        caption= paste ('Poisson GAM. Deviance explained: ', 
                        round(gam.sum$dev.expl,2),'. ',  
                        scales::pvalue(gam.sum$s.pv,
                                       accuracy = 0.001, # Number to round to
                                       decimal.mark = ".", # The character to be used 
                                       # to indicate the numeric decimal point
                                       add_p = TRUE) # Add "p=" before the value?
        )) +
  theme(plot.caption = element_text(face = "italic", size=12),
        legend.text = element_text(size = 12),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(.5, 'cm'),
        axis.text = element_text(size=10)) 

p21



# 15.5  Westerwald.Rhenish  
g1 <- gam(  Westerwald.Rhenish            
            ~ s(Easting, Northing, k=100 ), 
            family=  poisson(link=log),  data=coordsAndData, method="REML")
gam.sum <- summary(g1)
gam.sum
AIC(g1)
gam.check(g1, pch=21, cex=2, bg='dark grey',  rep  =500, lwd=1)


gridPts<- makeGrid(gridSpacing=5, tooFar=25, coordsAndData$Easting, 
                   coordsAndData$Northing  ) 
pred <- predict(g1, gridPts, type="link",se.fit=FALSE)
pred1 <- data.frame(gridPts, Predicted = pred)


theme_set(theme_classic(base_size = 18))


p22 <- ggplot(data = pred1 , aes(x = Easting, y = Northing )) +
  geom_raster(aes (fill = exp(Predicted))) + 
  scale_fill_viridis(option= 'plasma',  trans='log' ,alpha=.66, 
                     labels = scales::label_number(accuracy=.01),
                     breaks = exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), 
                                      length.out=11))) +
  #scale_fill_gradientn(colours = grey.colors(100, start=.9, end=.1)) +
  geom_contour(aes(z= exp(Predicted)),
               breaks = round(exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), length.out=11)),1),
               color= 'black', 
               linewidth=.5, show.legend=T) +
  #geom_contour_filled(aes(z= exp(Predicted)),
  #               breaks = round(exp(seq(min(pred1$Predicted), 
  #                               max(pred1$Predicted), length.out=11)),1),
  #             color= 'black', linewidth=.5, show.legend=T) +
  #scale_fill_grey(start=.99, end=.4) +
  #scale_fill_viridis(option= 'plasma', discrete= T, alpha =.66) +  
  geom_point(data= coordsAndData, aes(x=Easting, y=Northing), size=1 ) + 
  coord_fixed() +
  labs( title = 'Westerwald Stoneware ',
        subtitle =  'Site 7',
        fill = 'Count',
        caption= paste ('Poisson GAM. Deviance explained: ', 
                        round(gam.sum$dev.expl,2),'. ',  
                        scales::pvalue(gam.sum$s.pv,
                                       accuracy = 0.001, # Number to round to
                                       decimal.mark = ".", # The character to be used 
                                       # to indicate the numeric decimal point
                                       add_p = TRUE) # Add "p=" before the value?
        )) +
  theme(plot.caption = element_text(face = "italic", size=12),
        legend.text = element_text(size = 12),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(.5, 'cm'),
        axis.text = element_text(size=10)) 

p22


# 15.6   Thick-Bodied Wares

coordsAndData <-coordsAndData %>%   
   mutate(ThickBodiedWares = British.Brown.Stoneware +
                             Coarse.Earthenware.unidentified + 
                             Redware + 
                             Slipware.North.Midlands.Staffordshire +
                             Westerwald.Rhenish +
                             American.Stoneware+
                              German.Stoneware)
                              
                                
g1 <- gam(ThickBodiedWares           
            ~ s(Easting, Northing, k=100 ), 
            family=  poisson(link=log),  data=coordsAndData, method="REML")
gam.sum <- summary(g1)
gam.sum
AIC(g1)
gam.check(g1, pch=21, cex=2, bg='dark grey',  rep  =500, lwd=1)


gridPts<- makeGrid(gridSpacing=5, tooFar=25, coordsAndData$Easting, 
                   coordsAndData$Northing  ) 
pred <- predict(g1, gridPts, type="link",se.fit=FALSE)
pred1 <- data.frame(gridPts, Predicted = pred)


theme_set(theme_classic(base_size = 18))


p23<- ggplot(data = pred1 , aes(x = Easting, y = Northing )) +
  geom_raster(aes (fill = exp(Predicted))) + 
  scale_fill_viridis(option= 'plasma',  trans='log' ,alpha=.66, 
                     labels = scales::label_number(accuracy=.01),
                     breaks = exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), 
                                      length.out=11))) +
  #scale_fill_gradientn(colours = grey.colors(100, start=.9, end=.1)) +
  geom_contour(aes(z= exp(Predicted)),
               breaks = round(exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), length.out=11)),1),
               color= 'black', 
               linewidth=.5, show.legend=T) +
  #geom_contour_filled(aes(z= exp(Predicted)),
  #               breaks = round(exp(seq(min(pred1$Predicted), 
  #                               max(pred1$Predicted), length.out=11)),1),
  #             color= 'black', linewidth=.5, show.legend=T) +
  #scale_fill_grey(start=.99, end=.4) +
  #scale_fill_viridis(option= 'plasma', discrete= T, alpha =.66) +  
  geom_point(data= coordsAndData, aes(x=Easting, y=Northing), size=1 ) + 
  coord_fixed() +
  labs( title = 'Thick-Bodied Wares',
        subtitle =  'Site 7',
        fill = 'Count',
        caption= paste ('Poisson GAM. Deviance explained: ', 
                        round(gam.sum$dev.expl,2),'. ',  
                        scales::pvalue(gam.sum$s.pv,
                                       accuracy = 0.001, # Number to round to
                                       decimal.mark = ".", # The character to be used 
                                       # to indicate the numeric decimal point
                                       add_p = TRUE) # Add "p=" before the value?
        )) +
  theme(plot.caption = element_text(face = "italic", size=12),
        legend.text = element_text(size = 12),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(.5, 'cm'),
        axis.text = element_text(size=10)) +
  geom_text(x= 11499000, y=3891260, label='C5', color='black', size=8, 
            fontface='italic' ) +
  geom_text(x= 11499010, y=3891150, label='C6', color='black', size=8, 
            fontface='italic' ) +
  geom_text(x= 11499000, y=3891080, label='C7', color='black', size=8, 
            fontface='italic' )
p23

Figure11 <- plot_grid(p23, p18, p19, p21,
                     #labels = c('1.', '2.'),
                     label_size =24,
                     align="h",
                     nrow=1, ncol=4) 
Figure11

ggsave( Figure11, file = 'Figure11.png',  
        dpi=600, width=10, height=6, scale=2.5) 

# 15.7 GAM for Thin Bodied


g1 <- gam( ThinBodied         
           ~ s(Easting, Northing, k=100 ), 
           family=  poisson(link=log),  data=coordsAndData, method="REML")
gam.sum <- summary(g1)
gam.sum
AIC(g1)
gam.check(g1, pch=21, cex=2, bg='dark grey',  rep  =500, lwd=1)


gridPts<- makeGrid(gridSpacing=5, tooFar=25, coordsAndData$Easting, 
                   coordsAndData$Northing  ) 
pred <- predict(g1, gridPts, type="link",se.fit=FALSE)
pred1 <- data.frame(gridPts, Predicted = pred)


theme_set(theme_classic(base_size = 18))


p22 <- ggplot(data = pred1 , aes(x = Easting, y = Northing )) +
  geom_raster(aes (fill = exp(Predicted))) + 
  scale_fill_viridis(option= 'plasma',  trans='log' ,alpha=.66, 
                     labels = scales::label_number(accuracy=.01),
                     breaks = exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), 
                                      length.out=11))) +
  #scale_fill_gradientn(colours = grey.colors(100, start=.9, end=.1)) +
  geom_contour(aes(z= exp(Predicted)),
               breaks = round(exp(seq(min(pred1$Predicted), 
                                      max(pred1$Predicted), length.out=11)),1),
               color= 'black', 
               linewidth=.5, show.legend=T) +
  #geom_contour_filled(aes(z= exp(Predicted)),
  #               breaks = round(exp(seq(min(pred1$Predicted), 
  #                               max(pred1$Predicted), length.out=11)),1),
  #             color= 'black', linewidth=.5, show.legend=T) +
  #scale_fill_grey(start=.99, end=.4) +
  #scale_fill_viridis(option= 'plasma', discrete= T, alpha =.66) +  
  geom_point(data= coordsAndData, aes(x=Easting, y=Northing), size=1 ) + 
  coord_fixed() +
  labs( title = 'Thin-Bodied Wares',
        subtitle =  'Site 7',
        fill = 'Count',
        caption= paste ('Poisson GAM. Deviance explained: ', 
                        round(gam.sum$dev.expl,2),'. ',  
                        scales::pvalue(gam.sum$s.pv,
                                       accuracy = 0.001, # Number to round to
                                       decimal.mark = ".", # The character to be used 
                                       # to indicate the numeric decimal point
                                       add_p = TRUE) # Add "p=" before the value?
        )) +
  theme(plot.caption = element_text(face = "italic", size=12),
        legend.text = element_text(size = 12),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(.5, 'cm'),
        axis.text = element_text(size=10)) 

p22




Figure12 <- plot_grid(p22, p21,
                      #labels = c('1.', '2.'),
                      label_size =24,
                      align="h",
                      nrow=1, ncol=2) 
Figure12

ggsave( Figure12, file = 'Figure12.png',  
        dpi=600, width=10, height=6, scale=1.5) 



#### 16.  prep the data for CA of thick-bodied -- get rid of quads with no ceramics ####

coordsAndData2 <- coordsAndData %>% 
  select (ProjectID, QuadratID, Northing, Easting,
          #Astbury.Type ,      
          British.Brown.Stoneware,
          Coarse.Earthenware.unidentified,
          #Creamware,
          #Pearlware,
          Redware,
          # Refined.Earthenware.unidentifiable,
          # Whieldon.type.Ware,
          # Delftware.Dutch.British,
          # Porcelain.Chinese,
          Slipware.North.Midlands.Staffordshire,
          Westerwald.Rhenish,
          #White.Salt.Glaze,
          #Tin.Enameled.unidentified,
          #Jackfield.Type,
          #Nottingham,
          #Staffordshire.Mottled.Glaze,
          #Delft
          #Native.American,
          #German.Stoneware, #This is an outlier.
          American.Stoneware
          # totalCount
  )
nonZeroIndex <- rowSums(coordsAndData2[,-1:-4]) > 0
coordsAndData2 <- coordsAndData2[nonZeroIndex,]

colSums(coordsAndData2[,-1:-4])


#### 17.  Compute the variograms #####
varioGram <- vGram (coordsAndData2[,3:4], coordsAndData2[,5:ncol(coordsAndData2)],
                    from =0, to=150, lagDist=10 ) 

theme_set(theme_classic(base_size = 18))
p23 <- ggplot(data=varioGram , aes(meanGeoDist, y=meanChi2Dist)) +
  # geom_bar(stat="identity", fill="grey") +
  geom_line(col= "grey", size=2) +
  geom_point(shape=21, size=5, colour="black", fill="grey") +
  labs( title="Variogram, Thick-Bodied Wares ",
        subtitle = 'Site 7',
        x="Mean Distance (feet)", y='Mean Chi-square Distance' )   
p23

theme_set(theme_classic(base_size = 18))
p24 <- ggplot(data=varioGram , aes(meanGeoDist, y=meanEucDist)) +
  # geom_bar(stat="identity", fill="grey") +
  geom_line(col= "grey", size=2) +
  geom_point(shape=21, size=5, colour="black", fill="grey") +
  labs( title = "Variogram, Thick-Bodied Wares ",
        subtitle = 'Site 7',
        x="Mean Distance (feet)", y='Mean Euclidean Distance' )   
p24


ggsave( p23, file = 'Figure13.png',  
        dpi=600, width=8, height=6, scale=1) 


sm90 <- dmnSmooth( ID = coordsAndData2[,2],
                   coords = coordsAndData2[,3:4],
                   counts = coordsAndData2[,5:ncol(coordsAndData2)],
                   radius= 90 )


means_and_cls <- get_Randomized_Inertias(n_times=1000,prop.table(sm90$postMat,1))

# plot the results

theme_set(theme_classic(base_size = 18))  
p25 <- ggplot(data=means_and_cls, aes(x = Dimension, y=actual)) +
  scale_x_continuous(n.breaks = nrow(means_and_cls)) +
  geom_line(aes(y=mean), lty=2) +
  geom_ribbon(aes(x= Dimension, ymin=lcl, ymax=ucl), 
              col='gray', alpha=.1) +
  geom_line(aes(y=actual), col= "black", linewidth=1) +
  geom_point(shape=21, size=5, colour="black", fill="grey") +
  labs( title="CA: Actual vs. Randomized Inertias",
        subtitle = '',
        x="Dimension", y='Proportion of Inertia')
        
p25

ggsave( p25, file = 'Figure14.png',  
        dpi=600, width=8, height=6, scale=1) 



ca2 <- ca(sm90$postMat)

test <- plot(ca2)

# Put the results in data frames  
# We convert inertia to percent inertia 
inertia <- data.frame('Inertia' = prop.table(ca2$sv^2))
# We only take the first five CA dimensions for row and col scores
rowScores2 <- data.frame(ca2$rowcoord[,1:4], 
                        unit =ca2$rownames)
colScores2 <- data.frame(ca2$colcoord[,1:4], 
                        type =ca2$colnames)


# 14.21  scale rowcoords by SVs

scaledRowScores2 <-  ca2$rowcoord[,1:5] * matrix(ca2$sv[1:5], 
                                                nrow= nrow(ca2$rowcoord), 
                                                ncol =5, byrow=T  )
scaledRowScores2 <- data.frame (unit=rownames(scaledRowScores2), scaledRowScores2)




# plot the dim1 scores of the two analysis

scaledRowScoresBoth <- inner_join(scaledRowScores1, scaledRowScores2, by='unit' )

p26 <- ggplot(scaledRowScoresBoth, aes(x=Dim1.x, y=Dim2.y)) +
  #coord_fixed() +
  geom_point(shape=21, size=5, colour="black", alpha=.75, fill='grey') +
  geom_hline(yintercept = 0, linetype='dashed', color='grey') +
  geom_vline(xintercept = 0, linetype='dashed', color='grey') +
  #geom_text_repel(aes(label= unit), cex = 4, max.overlaps=50) +
  labs(title = "Quadrat Scores",
       subtitle = 'Site 7',
       x = "Dimension 1, Thin Wares", 
       y=  "Dimension 2, Thick Wares") 

p26


ggsave( p26, file = 'Figure16.png',  
        dpi=600, width=8, height=6, scale=1) 


cor.test(scaledRowScoresBoth$Dim1.x, scaledRowScoresBoth$Dim2.y)


# Now we plot the row (assemblage) scores on CA Dim 1 and Dim2
# We can uncomment the call to geom_text_repel(). But the result is messy.
set.seed(42)
p27 <- ggplot(scaledRowScores2, aes(x=Dim1,y=Dim2)) +
  #coord_fixed() +
  geom_point(shape=21, size=5, colour="black", alpha=.75, fill='grey') +
  geom_hline(yintercept = 0, linetype='dashed', color='grey') +
  geom_vline(xintercept = 0, linetype='dashed', color='grey') +
  #geom_text_repel(aes(label= unit), cex = 4, max.overlaps=50) +
  labs(title = "Quadrat Scores",
       subtitle = 'Site 7',
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep='')) 
p27

# Plot the column  (ware type) scores on CA Dim 1 and Dim 2
p28 <-  ggplot(colScores2, aes(x = Dim1,y = Dim2)) +
  #coord_fixed() +
  geom_point(shape=21, size=5, colour="black", fill="yellow", alpha=.75) +
  geom_text_repel(aes(label=type), cex= 5, force=2) +
  geom_hline(yintercept = 0, linetype='dashed', color='grey') +
  geom_vline(xintercept = 0, linetype='dashed', color='grey') +
  labs(title = 'Ware Type Scores',
       subtitle = 'Site 7',
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep=''))  
p28


Figure13 <- plot_grid(p27, p28,
                     #labels = c('1.', '2.'),
                     label_size =24,
                     align="h",
                     nrow=1, ncol=2) 
Figure13

ggsave( Figure13, file = 'Figure13.png',  
        dpi=600, width=10, height=4, scale=1.5) 


##### 14.5 KDEs  for phasing ####
# replicate scores by assemblage sample size 
dim1ForHist<- data.frame(dim1 = rep(scaledRowScores2$Dim1,
                                    rowSums(coordsAndData2[,5:ncol(coordsAndData2)])),
                         dim2 = rep(scaledRowScores2$Dim2,
                                    rowSums(coordsAndData2[,5:ncol(coordsAndData2)])),
                         dim3 = rep(scaledRowScores2$Dim3,
                                    rowSums(coordsAndData2[,5:ncol(coordsAndData2)])))

# 2-d weighted KDE

p29 <- ggplot(dim1ForHist, aes(x = dim1, y = dim2)) +
  coord_fixed() +
  geom_point(shape=21, size=5, colour="black", fill="lightblue", alpha=.50) + 
  geom_density_2d(color = 'black', size=1, adjust = 1,  alpha=.5, bins=11 ) +
  labs(title="Site 7", x="Dimension 1", y="Dimension 2") 
#geom_vline(xintercept=c(-1.3, .2), colour = "blue", linetype = "dashed",
#           size=1)
p29 



# 1-d Weighted Histogram

p30 <- ggplot(dim1ForHist, aes(x = dim1)) +
  #coord_fixed() +
  geom_histogram(aes(y= after_stat(density)), fill="grey", color="#e9ecef", binwidth =.05)+ 
  geom_density (color = 'black', size=1, adjust = 1, bw='nrd0')  +
  labs(title="Frequency Distribution of Quadrat Scores",
       subtitle = 'Site 7',
       x="CA Dimension 1", 
       y= 'Density') +
  geom_vline(xintercept=c(0), colour = "black", linetype = "dashed",
             size=1) +
  geom_text(x= -.25, y= 2, label="1", size=10) +
  geom_text(x= .25, y= 2, label="2", size=10) 
p30


ggsave( p30, file = 'Figure14.png',  
        dpi=600, width=8, height=6, scale=1) 



#### 14.6 Assign the Phases based on CA scores ####

scaledRowScores2 <- scaledRowScores2 %>% 
  mutate (Group = case_when(Dim1  < 0 ~ '1',
                            Dim1 >= 0 ~ '2'))
  
#  mutate (Group = case_when(Dim1 < -.4 ~ '1',
#                            (Dim1 >= -.4) & (Dim1 < .15) ~ '2',
#                            Dim1 > .15  ~  '3'))


##### 14.7 Map the CA Phases ####

scaledRowScores2WCoords <- inner_join (scaledRowScores2 , coordsAndData2, 
                                       by= c('unit'= 'QuadratID') )




theme_set(theme_classic(base_size = 18))
p31 <- ggplot(data= scaledRowScores2WCoords, aes(x=Easting, y=Northing, 
                                                 fill= Group)) +
  geom_point(size=5, shape=22, alpha= .66)+ 
  scale_fill_viridis_d(option='magma') +
  geom_text_repel( aes(label=Group), size=5) +
  coord_fixed() +
  labs( title = 'CA Groups',
        subtitle = 'Site 7',
        caption= '')
p31


ggsave( p31, file = 'Figure15.png',  
        dpi=600, width=10, height=8, scale=1)




unitData <- data.frame(QuadratID = rownames(sm90$postMat),sm90$postMat )

unitData_Sorted  <- sortData(scaledRowScores2$Dim1, colScores2$Dim1, 
                             unitData)
Mat<-as.matrix(unitData_Sorted[,-1])
rownames(Mat)<- NULL
propMat <- prop.table(Mat,1)
# Do the plot

# Figure 10 
battleship.plot(propMat,
                mar=c(2,2,15,2),
                cex.labels=2,
                yaxlab= '',
                #main = 'Seriation by CA Dimension 1',
                xlab='Ware Type',
                col='grey')


# put the Dim 1 row coords in a matrix
scaledRowScores2Dim1Mat <- matrix(scaledRowScores2$Dim1, nrow=nrow(scaledRowScores2),
                                 ncol = nrow(ca2$colcoord), byrow = F)
# put the Dim-1 col coords in a matrix                                                        
colScores2Dim1Mat <- matrix(ca2$colcoord[,1], nrow=nrow(scaledRowScores2),
                           ncol = nrow(ca2$colcoord), byrow = T)
# compute the assemblage variances
assemblageVar2 <- rowSums(((colScores2Dim1Mat - scaledRowScores2Dim1Mat)^2)*
                           prop.table(sm90$postMat,1))
# compute the assemblage ranges based on uniform distribution 

dim1AssemVar2 <- data.frame(scaledRowScores= scaledRowScores2$Dim1,
                           assemVar = assemblageVar2,
                           assemRange = sqrt(12*assemblageVar2))


theme_set(theme_classic(base_size = 18))
p10 <- ggplot(data=dim1AssemVar2 , aes(x=scaledRowScores, y=scaledRowScores)) +
  coord_fixed() +
  geom_errorbar(aes(ymin= scaledRowScores - (assemRange/2), 
                    ymax= scaledRowScores + (assemRange/2), 
                    width=0)) +
  geom_point(shape=21, size=5, color="black", fill="grey", alpha=.75) +
  labs( title="CA, Thick-Bodied Wares",
        subtitle = 'Site 7',
        x="Dimension 1", y='Range' ) +
  theme(legend.position="none")+
  geom_vline(xintercept=c(-.6, .1, .4), colour = "black", linetype = "dashed",
             size=1)
p10


theme_set(theme_classic(base_size = 18))
p10 <- ggplot(data=dim1AssemVar2 , aes(scaledRowScores, y= sqrt(assemVar))) +
  geom_point(shape=21, size=5, color="black", fill="grey", alpha=.75) +
  labs( title="Time Averaging",
        subtitle = 'Site 7',
        x="Dimension 1", y='Assemblage Standard Deviation.') +
  geom_vline(xintercept=c(-.6, .1, .4), colour = "black", linetype = "dashed",
             size=1) +
  geom_text(x= -.9, y= .5, label="1", size=10) +
  geom_text(x= -.25, y= .5, label="2", size=10) +
  geom_text(x= .25, y= .5, label="3", size=10) +
  geom_text(x= .5, y= .5, label="4", size=10)
p10

















#### 16.  prep the data for CA of thin AND thick-bodied  ####

coordsAndData3 <- coordsAndData %>% 
  select (ProjectID, QuadratID, Northing, Easting,
#          Astbury.Type ,      
          British.Brown.Stoneware,
          Coarse.Earthenware.unidentified,
          Creamware,
          Pearlware,
          Redware,
          # Refined.Earthenware.unidentifiable,
          Whieldon.type.Ware,
          # Delftware.Dutch.British,
          Porcelain.Chinese,
          Slipware.North.Midlands.Staffordshire,
          Westerwald.Rhenish,
          White.Salt.Glaze,
          #Tin.Enameled.unidentified,
          Jackfield.Type,
         Nottingham,
          Staffordshire.Mottled.Glaze,
          Delft,
          #Native.American,
          American.Stoneware
          # totalCount
  )
nonZeroIndex <- rowSums(coordsAndData3[,-1:-4]) > 0
coordsAndData3 <- coordsAndData3[nonZeroIndex,]

colSums(coordsAndData3[,-1:-4])
rowSums(coordsAndData3[,-1:-4])

#### 17.  Compute the variograms #####
varioGram <- vGram (coordsAndData3[,3:4], coordsAndData3[,5:ncol(coordsAndData3)],
                    from =0, to=150, lagDist=10 ) 

theme_set(theme_classic(base_size = 18))
p26 <- ggplot(data=varioGram , aes(meanGeoDist, y=meanChi2Dist)) +
  # geom_bar(stat="identity", fill="grey") +
  geom_line(col= "grey", size=2) +
  geom_point(shape=21, size=5, colour="black", fill="grey") +
  labs( title="Variogram, Thick-Bodied Wares ",
        subtitle = 'Site 7',
        x="Mean Distance (feet)", y='Mean Chi-square Distance' )   
p26

theme_set(theme_classic(base_size = 18))
p27 <- ggplot(data=varioGram , aes(meanGeoDist, y=meanEucDist)) +
  # geom_bar(stat="identity", fill="grey") +
  geom_line(col= "grey", size=2) +
  geom_point(shape=21, size=5, colour="black", fill="grey") +
  labs( title = "Variogram, Thick-Bodied Wares ",
        subtitle = 'Site 7',
        x="Mean Distance (feet)", y='Mean Euclidean Distance' )   
p27


#ggsave( p26, file = 'Figure13.png',  
#        dpi=600, width=10, height=6, scale=1) 


sm50 <- dmnSmooth( ID = coordsAndData3[,2],
                   coords = coordsAndData3[,3:4],
                   counts = coordsAndData3[,5:ncol(coordsAndData3)],
                   radius= 50 )

ca3 <- ca(sm50$postMat)

test <- plot(ca3)

# Put the results in data frames  
# We convert inertia to percent inertia 
inertia <- data.frame('Inertia' = prop.table(ca3$sv^2))
# We only take the first five CA dimensions for row and col scores
rowScores3 <- data.frame(ca3$rowcoord[,1:4], 
                         unit =ca3$rownames)
colScores3 <- data.frame(ca3$colcoord[,1:4], 
                         type =ca3$colnames)

# Now we check the scree plot -- we use a scatter plot here, not a bar plot as 
# with our earlier example. Either works!

# Apply the broken.stick function to the inertia dataframe
bs <- broken.stick(nrow(inertia))

# Plot the proportion of inertia

theme_set(theme_classic(base_size = 18))
p28<- ggplot(data=inertia , aes(x= 1:length(Inertia), y=Inertia)) +
  # scale_x_continuous(n.breaks = 8) +
  # geom_bar(stat="identity", fill="grey") +
  geom_line(col= "grey", size=2) +
  geom_point(shape=21, size=5, colour="black", fill="grey") +
  geom_line(aes(y = bs[,2], x= bs[,1]), color = "black", linetype = "dashed", 
            linewidth=1) +
  labs( title="CA, Thin and Thick-Bodied Wares",
        subtitle = 'Site 7',
        x="Dimension", y='Porportion of Inertia' ) 
p28


#ggsave( p28, file = 'Figure13.png',  
#        dpi=600, width=10, height=6, scale=1) 


# plot the dim1 scores of the two analysis

ggplot(rowScores3, aes(x=Dim1, y=Dim2)) +
  #coord_fixed() +
  geom_point(shape=21, size=5, colour="black", alpha=.75, fill='grey') +
  geom_hline(yintercept = 0, linetype='dashed', color='grey') +
  geom_vline(xintercept = 0, linetype='dashed', color='grey') +
  #geom_text_repel(aes(label= unit), cex = 4, max.overlaps=50) +
  labs(title = "Quadrat Scores",
       subtitle = 'Site 7',
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep='')) 




# Now we plot the row (assemblage) scores on CA Dim 1 and Dim2
# We can uncomment the call to geom_text_repel(). But the result is messy.
set.seed(42)
ggplot(rowScores3, aes(x=Dim1,y=Dim2)) +
  #coord_fixed() +
  geom_point(shape=21, size=5, colour="black", alpha=.75, fill='grey') +
  geom_hline(yintercept = 0, linetype='dashed', color='grey') +
  geom_vline(xintercept = 0, linetype='dashed', color='grey') +
  #geom_text_repel(aes(label= unit), cex = 4, max.overlaps=50) +
  labs(title = "Quadrat Scores",
       subtitle = 'Site 7',
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep='')) 


# Plot the column  (ware type) scores on CA Dim 1 and Dim 2
ggplot(colScores3, aes(x = Dim1,y = Dim2)) +
  #coord_fixed() +
  geom_point(shape=21, size=5, colour="black", fill="yellow", alpha=.75) +
  geom_text_repel(aes(label=type), cex= 5, force=2) +
  geom_hline(yintercept = 0, linetype='dashed', color='grey') +
  geom_vline(xintercept = 0, linetype='dashed', color='grey') +
  labs(title = 'Ware Type Scores',
       subtitle = 'Site 7',
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep=''))  


##### 14.5 KDEs  for phasing ####
# replicate scores by assemblage sample size 
dim1ForHist<- data.frame(dim1 = rep(rowScores3$Dim1,
                                    rowSums(coordsAndData3[,5:ncol(coordsAndData3)])),
                         dim2 = rep(rowScores3$Dim2,
                                    rowSums(coordsAndData3[,5:ncol(coordsAndData3)])),
                         dim3 = rep(rowScores3$Dim3,
                                    rowSums(coordsAndData3[,5:ncol(coordsAndData3)])))

# 2-d weighted KDE

p13 <- ggplot(dim1ForHist, aes(x = dim1, y = dim2)) +
  coord_fixed() +
  geom_point(shape=21, size=5, colour="black", fill="lightblue", alpha=.50) + 
  geom_density_2d(color = 'black', size=.5, adjust = 1,  alpha=1, 
                  binwidth= .01 ) +
  labs(title="Site 7", x="Dimension 1", y="Dimension 2") 
#geom_vline(xintercept=c(-1.3, .2), colour = "blue", linetype = "dashed",
#           size=1)
p13 



# 1-d Weighted Histogram

p14 <- ggplot(dim1ForHist, aes(x = dim1)) +
  #coord_fixed() +
  geom_histogram(aes(y=..density..), fill="grey", color="#e9ecef", binwidth =.1)+ 
  geom_density (color = 'black', size=1, adjust = 1)  +
  labs(title="Frequency Distribution of Quadrat Scores",
       subtitle = 'Site 7',
       x="CA Dimension 1", 
       y= 'Density') +
  geom_vline(xintercept=c(-1.4, .1, .6 ), colour = "black", linetype = "dashed",
             size=1) +
  geom_text(x=-2, y=.8, label="1", size=10) +
  geom_text(x= -.6, y=.8, label="2", size=10) +
  geom_text(x= .4, y=.8, label="3", size=10) +
  geom_text(x= .9, y=.8, label="4", size=10)
p14


ggsave( p14, file = 'Figure8.png',  
        dpi=600, width=10, height=6, scale=1.25) 


#### 14.6 Assign the Phases based on CA scores ####

rowScores1 <- rowScores %>% 
  mutate (Phase = case_when(Dim1 < -1.4 ~ '1',
                            (Dim1 > - 1.4 ) & (Dim1 < .1) ~ '2',
                            (Dim1 > .1)  & (Dim1 < .6) ~ '3',
                            Dim1 > .6  ~ '4')) 



##### 14.7 Map the CA Phases ####


rowScores1WCoords <- inner_join (rowScores1, coordsAndData1, 
                                 by= c('unit'= 'QuadratID') )


theme_set(theme_classic(base_size = 18))
p15 <- ggplot(data= rowScores1WCoords, aes(x=Easting, y=Northing, fill= Phase)) +
  geom_point(size=5, shape=22, alpha= .66)+ 
  scale_fill_viridis_d(option='magma') +
  geom_text_repel( aes(label=Phase), size=5) +
  coord_fixed() +
  labs( title = 'CA Phases',
        subtitle = 'Site 7',
        caption= '')
p15
ggsave( p15, file = 'Figure9.png',  
        dpi=600, width=10, height=8, scale=1.25) 



#### 14.8 Do a battleship plot based on CA Dim1 ####

# First define a  function to sort the rows and cols of a matrix based on the
# orders from two arguments (e.g. Row scores and Col scores from CA)
# arguments:  the name of the variable that contains the row scores. 
#             the name of the variable that contains the col scores. 
#             the name of the dataframe that contains the counts of ware types 
#               in units
# returns:    the sorted dataframe 
sortData<- function(unitScores,typeScores,unitData){
  sortedData<-unitData[order(unitScores, decreasing=T),]
  sortedData<-sortedData[,c(1,order(typeScores)+1)]
  return(sortedData)
}

unitData <- data.frame(QuadratID = rownames(sm50$postMat),sm50$postMat )

unitData_Sorted  <- sortData(rowScores3$Dim1, colScores3$Dim1, 
                             unitData)
Mat<-as.matrix(unitData_Sorted[,-1])
rownames(Mat)<- NULL
propMat <- prop.table(Mat,1)
# Do the plot

# Figure 10 

png('bsPlot.png', width = 10, height = 8, units= 'in',
    pointsize= 14, res=600) # open the PNG 
battleship.plot(propMat,
                mar=c(2,2,15,2),
                cex.labels=1,
                yaxlab= '',
                maxxspan = .75,
                #main = 'Seriation by CA Dimension 1',
                xlab='Ware Type',
                col='grey')
dev.off()  # Close the PNG 




