### ##########################################################################################
### Script for classification of satellite data
### 
### based on a script by: Dr. Ned Norning, Dr. Martin Wegmann
### first version approx. in 2011
###
### www.remote-sensing-biodiversity.org
##############################################################################################

##############################################################################################
###	DOCUMENTATION
#####################################################################################
# The script reads a Shapefile holding the samples (defined by: attName).

# Raster image that contains environmental variables (continuous, categorical) (defined by: satImage). 

# For each sample the data values for that pixel are determied and these data are used to run 
# the model. 
#####################################################################################

###########################
###						###
###	Set variables				###
###						###
###########################

#-----------------------------------------------------------------------------------------------
# within the CIP pool install.packages() need to be done prior the library() function
# the following command is just a new defined command by you which checks if the package is installed and if not, it installs the package automatically
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }

loadandinstall("maptools")
loadandinstall("sp")
loadandinstall("randomForest")
loadandinstall("raster")
loadandinstall("rgdal")
loadandinstall("mgcv")


#-----------------------------------------------------------------------------------------------

####### Start Classification ########


###########################
###				###
###	Set variables and settings	###
###				###
###########################

# install QGIS, load the satellite data and define a cropping area and training data within this region
# think about a validation set

#  set main folder
#_____________________________________________________________________________

# set your working directory
# assign path to variable "workingdir"
workingdir <- ("/...../script/") # optional

# set working directory using the variable name
setwd(workingdir) # optional

#### path to the shape training data using the relative path
path_shapefile <- 'vector_data/path/to/file'
shapefile <- 'shapefile_without_suffix'


# Read the Shapefile
#using the link defined above
vec <- readOGR("path_shapefile, shapefile


# import data - several choices:
# import the whole stack of bands
satImage <- brick("name_of_satellite_image")


# in case you have just one file with various bands:
# raster1 <- raster("raster_data/crop_p224p63_b1.tif")
# raster2 <- raster("raster_data/crop_p224p63_b2.tif")
# raster3 <- raster("raster_data/crop_p224p63_b3.tif")
# raster4 <- raster("raster_data/crop_p224p63_b4.tif")

# or if you just want to import some bands in a big stack of bands:
# raster1 <- raster("raster_data/crop_p224p63.tif",band=1)
# raster2 <- raster("raster_data/crop_p224p63.tif",band=2)
# raster3 <- raster("raster_data/crop_p224p63.tif",band=3)
# raster4 <- raster("raster_data/crop_p224p63.tif",band=4)
# otherwise skip the "band=.." part and just add the correct file name for each band.

# stack the bands together (layer stacking)
# if more than 4 raster are imported, add another file name
# satImage <- stack(raster1,raster2, raster3,raster4)

#################
#### import finished ###
#################


### Number of samples per land cover class - number up to you and depending on study area size
numsamps <- 100

### Name of the attribute that holds the integer land cover type identifyer (very important to consider when doing the training data sets)
attName <- 'id'

### Name and path of the output GeoTiff image - just a variable for later usage
outImage <- 'results/classif_result.tif'


#################
#### settings set - finished ###
#################



#-----------------------------------------------------------------------------------------------

###################################
###				###
###	Start processing	###
###				###
###################################

# Create vector of unique land cover attribute values

allAtt <- slot(vec, "data")
tabAtt <-table(allAtt[[attName]])
uniqueAtt <-as.numeric(names(tabAtt))

### Create input data from a Shapefile with training data and the image ("generation points for Regions of Interest")
for (x in 1:length(uniqueAtt)) {
  # Create a mask for cover type 1 (an integer attribute)
  class_data <- vec[vec[[attName]]==uniqueAtt[x],]
  # Get random points for the class
  classpts <- spsample(class_data, type="random", n=numsamps) 
  #Append data for all but the first run
  if (x == 1) { 
    xy <- classpts
  } else {
    xy <- rbind(xy, classpts)		 
  }
}


# # # for testing if you want
# summary(xy)
# plot(xy)
# image(satImage)
# plot(xy, add=TRUE)



### plot the random generated points on one of the rasters - for visual checking only
pdf("results/randompoints.pdf")
plot(satImage,2)
points(xy)
dev.off()
# !!! resulting image NOT in the plot window but in a pdf outside your R session - see settings on top about location of results

##############
#
# Get class number for each sample point

temp <- over(x=xy,y=vec)
summary(temp)

#############
#
# Create the responce input for randomForest

response <- factor(temp[[attName]]) 

#############

trainvals <- cbind(response, extract(satImage, xy)) #what is behind certain values (1-water and the value for each band)

# check for consistency
head(trainvals)

#############
#
# Run Random Forest
# think about different methods (SVM, BRT, GLM, ...)
# compare the results


print("Starting to calculate random forest object") 
randfor <- randomForest(as.factor(response) ~. , #glm, car, gam, any statistics you want..
		    data=trainvals, 		#a tree gives all breaking point (how to split table) ##library(tree), cmd: #tree()
		    na.action=na.omit, 		# if pixels with no values exist
		    confusion=T)		# first info how good your classification performed

randfor #to look at results (confusion matrix: 100 good, all class A points are within class A)

# ### further settings which can be defined - see: randomForest manual
# model_1 <-randomForest(x=input,
#                        ntree=Treesize, 
#                        nodesize=Nodesize,  
#                        importance=TRUE, 
#                        proximity=FALSE, 
#                        mtry=Feature_OOB, 
#                       confusion=TRUE,
#                        do.trace=TRUE,
#                        norm.votes=TRUE)
# 
# ### other method could be applied and compared like glm(), gam(), SVM, tree etc.

#############
#
# Start predictions

print("Starting predictions")
predict(satImage, randfor, filename=outImage, progress='text', format='GTiff', datatype='INT1U', type='response', overwrite=TRUE)


####### End of Classification ########
