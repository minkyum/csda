library(sf)
library(terra)
library(RColorBrewer)


siteDomain <- shapefile('/projectnb/modislc/users/mkmoon/cdsa/plsp/shp/ABoVE_reference_grid_v2_1527/data/ABoVE_Study_Domain/ABoVE_Study_Domain.shp')
siteDomain <- crop(siteDomain,extent(-3398275,-1423682.7,3062705,4562705))


filesM <- list.files('/projectnb/modislc/users/sjstone/woody_encroach/data/maxar',pattern=glob2rx('*M2AS*.NTF'),full.names=T,recursive=T)
filesP <- list.files('/projectnb/modislc/users/sjstone/woody_encroach/data/maxar',pattern=glob2rx('*P2AS*.NTF'),full.names=T,recursive=T)

print(length(filesM))

baseImg <- rast('/projectnb/planet/PLSP/Img_cliped/Healy_NEON/base_image.tif')
# baseImg <- raster('/projectnb/planet/PLSP/Img_cliped/Toolik_Field_Station_NEON/base_image.tif')

mycol <- colorRampPalette(brewer.pal(11,'Spectral'))
mycol <- mycol(19)


setwd('/projectnb/modislc/users/mkmoon/cdsa/figures/')
png(filename='maxar_avail_p2as.png',width=11,height=9,units='in',res=300)

plot(siteDomain)
text(-3098275,4462705,'Pan; n = 134',cex=2)
datList <- matrix(1,length(filesP),3)
for(i in 1:length(filesP)){
  datList[i,1] <- i
  
  img1 <- rast(filesP[i],lyrs=1)
  # print(paste0(i,'; ',res(img1)[1],'; ',nbands(img1)))
  
  # log <- try(intersect(img1,baseImg),silent=T)
  # if (inherits(log, 'try-error')) { datList[i,2] <- 0}
  
  info <- gdal_metadata(filesP[i])
  datList[i,3] <- as.numeric(substr(unlist(strsplit(info[pmatch('NITF_IDATIM',info)],'='))[2],1,8))

  e <- extent(img1)
  p <- as(e, 'SpatialPolygons')
  crs(p) <- crs(img1)
  p <- spTransform(p,crs(siteDomain))
  
  plot(p,add=T,col=mycol[as.numeric(substr(datList[i,3],1,4))-2003],border=F)  
  print(i)
}

plot(raster(matrix(seq(1,22,length.out=100),10,10)),
     legend.only=T,
     col=mycol,
     zlim=c(1,19),
     legend.width=1.5,
     legend.shrink=0.6,
     horiz=F,
     smallplot=c(0.01,0.04,0.2,0.8),
     axis.args=list(at=c(1,7,12,17),
                    labels=c(2004,2010,2015,2020),
                    cex.axis=1.5,font=1)

)
dev.off()



sum(datList[,2])

datSub_Toolik <- datList[datList[,2]==1,]


##
baseImg <- raster('/projectnb/planet/PLSP/Img_cliped/Healy_NEON/base_image.tif')
par(mfrow=c(4,5))
for(i in 1:20){
  img1 <- raster(files[datSub_Healy[i,1]],band=1)
  img1 <- intersect(img1,baseImg)
  plot(img1,main=datSub_Healy[i,3])
}

baseImg <- raster('/projectnb/planet/PLSP/Img_cliped/Toolik_Field_Station_NEON/base_image.tif')
par(mfrow=c(4,5))
for(i in 1:20){
  img1 <- raster(files[datSub_Toolik[i,1]],band=1)
  img1 <- intersect(img1,baseImg)
  plot(img1,main=datSub_Toolik[i,3])
}

