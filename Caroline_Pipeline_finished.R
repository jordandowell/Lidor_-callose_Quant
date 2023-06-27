##### EBI imager
##### 
##### 
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("digest")

BiocManager::install("EBImage",force = T)
library("EBImage")

#imager and tidyr are on CRAN
library(imager)
library(tidyr)



#get a list of images in the input images folder
#chage patter to match the ending of the file CASE SENSITIVE
InputImage<-list.files(path = "InputImages/",pattern = ".JPG")

#create data frame to store information
ALL_LabeledDots<-data.frame()
i<-1
#iterate over files
for (i in 1:length(InputImage)) {
  print(paste(InputImage[i], "is starting"))
 
  img <- readImage(paste0("InputImages/",InputImage[i]), resize = NULL, rotate = NULL)
  # fit a color map (only provided parameter is a color similarity cutoff)
  pdf(paste0("OutputImages/Clustering_",InputImage[i],".pdf"))
   recolorize_obj <- recolorize2(img, cutoff = 20)
dev.off()

  #calculate green leaf index
  GREEN_LEAF_INDEX<-(2*recolorize_obj$centers[,2]-recolorize_obj$centers[,1]-recolorize_obj$centers[,3])/(2*recolorize_obj$centers[,1]+recolorize_obj$centers[,2]+recolorize_obj$centers[,3])
  #find max leaf index
  LayersToUse<-which.max(GREEN_LEAF_INDEX)
  pdf(paste0("OutputImages/Clustering_SingleLayers_LayerUsed_",LayersToUse,"_",InputImage[i],".pdf"))
  layout(matrix(1:10, nrow = 2, byrow = TRUE))
  par(mar = c(0, 0, 2, 0))
  # split images based on colorizing 
  layers <- splitByColor(recolorize_obj, plot_method = "colormask")
dev.off()
  

#select the layer that has the highest Green leaf index 
Mask_1  <-as.cimg(layers[[LayersToUse]])
 
#blur image to connect blobs
ExGreen <- isoblur(Mask_1, .2)

#provide a label 
ExGreen <- bwlabel(ExGreen)


#create a data set to print alongside the data
dots_bw <- getFrame(ExGreen, 1)
labelled_dots <- bwlabel(dots_bw)
df <- as.data.frame(cbind(computeFeatures.moment(labelled_dots),
                          computeFeatures.shape(labelled_dots)))

#add file name to the dataframe
LabeledDots<-cbind(paste0("InputImages/",InputImage[i],"_LayerUsed_",LayersToUse),df)
colnames(LabeledDots)[1]<- "FileName"

LabeledDots<-separate_wider_delim(LabeledDots,cols = "FileName", names_sep = "_",delim = "_")

#produce Image specific data sets

write.csv(LabeledDots, paste0("OutputTables/Labeled_LayerUsed_",LayersToUse,"_",InputImage[i],".csv"))



ALL_LabeledDots<-rbind(ALL_LabeledDots,LabeledDots)
}

#ignore errors
#Error in exists(cacheKey, where = .rs.WorkingDataEnv, inherits = FALSE) : 
#invalid first argument
#Error in assign(cacheKey, frame, .rs.CachedDataEnv) : 
 # attempt to use zero-length variable name

#add what your column names are for you separators 
#
#
write.csv(ALL_LabeledDots, "OutputTables/FULL_imageDATA.csv")

