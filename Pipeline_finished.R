##### EBI imager
##### 
##### 
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("EBImage")
library("EBImage")

#imager and tidyr are on CRAN
library(imager)
library(tidyr)
#threshold needs to be changed relative to the lighting of the image
#change threshold percentage as necessary
#reducing the percentage will include more pixels, 
#increasing the percentage will reduce the number of pixels included
#90%worked well for all the test images
THRESH<- "90%"


#get a list of images in the input images folder
InputImage<-list.files(path = "InputImages/",pattern = ".jpg")

#create dataframe to store information
ALL_LabeledDots<-data.frame()

#iterate over files
for (i in 1:length(InputImage)) {
  print(paste(InputImage[1], "is starting"))
  


#load image
image <- load.image(paste0("InputImages/",InputImage[i]))

#threshold image based on intensity
ExGreen <- threshold(image, thr=THRESH)
#blur image to connect blobs
ExGreen <- isoblur(ExGreen, .1)

#connect pixels give that deposition is likely round
ExGreen <- opening(ExGreen, makeBrush(5, shape='disc'))
#fill in shapes
ExGreen <- fillHull(ExGreen)
#provide a label 
ExGreen <- bwlabel(ExGreen)
#create an overlay image where the deposits are outlined in black
Objects_labeled <- paintObjects(ExGreen, image, col=c("black","yellow"), opac=c(1,.2), thick=TRUE)
#output image
pdf(paste0("OutputImages/Labeled_",InputImage[i],".pdf"))
plot(Objects_labeled)
dev.off()
#create a dataset to print alongside the data
dots_bw <- getFrame(ExGreen, 1)
labelled_dots <- bwlabel(dots_bw)
df <- as.data.frame(cbind(computeFeatures.moment(labelled_dots),
                          computeFeatures.shape(labelled_dots)))

#add file name to the dataframe
LabeledDots<-cbind(paste0("InputImages/",InputImage[i]),df)
colnames(LabeledDots)[1]<- "FileName"

LabeledDots<-separate_wider_delim(LabeledDots,cols = "FileName", names_sep = "_",delim = "_")

#produce Image specifc data sets

write.csv(LabeledDots, paste0("OutputTables/Labeled_",InputImage[i],".csv"))



ALL_LabeledDots<-rbind(ALL_LabeledDots,LabeledDots)
}



#add what your column names are for you separators 
#
#
write.csv(ALL_LabeledDots, "OutputTables/CalloseDeposits.csv")

