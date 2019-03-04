## Demo script for perennial ryegrass single leaf images
## This script will demonstrate:
#1) seperating foreground (leaf) from background
#2) measure severity
#3) counting pustules
#4) can be used for stem and crown rust
## Date: 2.26.19
## Author: Garett Heineck

## Required packages - may need installation first
## PLEASE UPDATE R (if you have not done so recently)
library(tidyverse)
library(readxl)
library(dplyr)
library(jpeg)
library(EBImage) #needs to be downloaded from (https://www.bioconductor.org/packages/release/bioc/html/EBImage.html)***
library(randomForest)
library(stringr)
library(ggplot2)
library(cowplot)
##############
##############
##############


##############
##############
##############
## Required file paths
#parent directory folder nomed "grass.leaf_rust.detection_2.26.19"***
#To run this you need a folder named "grass.leaf_rust.detection_2.26.19"
img_dir_leaf.demo.2.26.19<- "/Users/heine237/Documents/GitHub/grass.leaf_rust.detection_2.26.19" #NOTE: change to your own file path***
#************************#
## Creating folders to store all image output
#NOTE: the folder "original_img" is where the images you want to process need to be***
folders <- c("training data_2.26.19","results", "original_img", "S1_crop_img", "S2_foreground_classify", "S3_foreground_EBImage", "S4_CR_classify",  "S5_CR_EBImage", "S6_SR_classify",  "S7_SR_EBImage") #adding in the correct folder list*** 
for (i in 1:length(folders))  { 
  dir.create(paste(img_dir_leaf.demo.2.26.19,folders[i], sep="/")) 
}
#NOTE: you may see Warning messages, that is ok***
#make sure the "original_img" folder has images in it***
##############
##############
##############


##############
##############
##############
## Read in field data sheet.
## The field data contains plant information about each image captured in the field.
## You can open the excel spreadsheet to read the column descriptions
## Field data also gives us median visual ratings for crown rust.
## We can compare these visual scores to our computer generated ratings.
#************************#
#read in field data***
#we will use this later in the image analysis process***
leaf_field.dat<-  read_excel(paste(img_dir_leaf.demo.2.26.19, "results", "leaf.demo_field.data_2.26.19.xlsx", sep = "/"), skip = 7, na = ".")
summary(leaf_field.dat)
##############
##############
##############

##############
##############
##############
## This step crops the original images.
## Cropping your images is important to reduce processing time. 
#************************#
#file paths for the original images***
folder_original_img<- paste(img_dir_leaf.demo.2.26.19, "original_img",sep = "/")
#file path for the cropped images***
folder_crop_img<-  (paste(img_dir_leaf.demo.2.26.19,"S1_crop_img",sep = "/"))
#************************#
#this is one way to determine how much of an image to remove***
#look at your image and guess what proportion should be removed***
#5% is take from the short length***
#5% is taken from the long length***
Top.Bot<- 0.05
Lef.Rig<- 0.05
#************************#
paths_original_img<- list.files(path=folder_original_img,full.names = TRUE) 
names_original_img<- list.files(path=folder_original_img,full.names = FALSE) 
for (i in 1:length(paths_original_img)){
  temp=readJPEG(paths_original_img[i])
  temp1=temp[1:dim(temp)[1], 1:dim(temp)[2],] # standardizing the rotation of each image***
  tempdim<- round(dim(temp1))
  top<- round(tempdim[1] * Top.Bot)
  bottom<- tempdim[1] - (tempdim[1] * Top.Bot) 
  left<- round(tempdim[2] * Lef.Rig)
  right<- round(tempdim[2] - (tempdim[2] * Lef.Rig))
  temp2<-temp1[top:bottom, left:right,]
  order<- leaf_field.dat$plant_num[i] # this adds the field ID names to each image***
  writeJPEG(temp2, paste(folder_crop_img, "/", order, "_", names_original_img[i], sep = ""), quality = 1)
}
##############
##############
##############


##############
##############
##############
## We now need to load the training data.
## Information on how to create training data can be found in the TRAINING DATA HELP GUIDE.
## Collectively the training mixes are called a palette in the training palette folder.
## The palette has many mixes, each help in predicting different features within the image.
#************************#
palette_directory_leaf<- paste(img_dir_leaf.demo.2.26.19, "training data_2.26.19",sep = "/") #file path where mixes are saved***
#************************#
mixes_names<- list.files(path=palette_directory_leaf,pattern="*.csv",full.names = FALSE) #name directory for what is in the palette folder***
mixes_path<- list.files(path=palette_directory_leaf, pattern="*.csv", full.names = TRUE) #path directory for what is in the palette folder***
training.palette_leaf<- data.frame()
#this for() loop will systematically re arrange and condense each mix file in the training palette folder***
#the reason I am doing this is to allow the script to update itself upon adding additional mixes***
for (i in 1:length(mixes_path)){
  temp_mix<- read.csv(mixes_path[i])
  temp_mix$band<- NA
  temp_mix$band[1:which(temp_mix$Label == "Red")] <- "Red"
  temp_mix$band[(which(temp_mix$Label == "Red")+1):which(temp_mix$Label == "Green")] <- "Green"
  temp_mix$band[(which(temp_mix$Label == "Green")+1):which(temp_mix$Label == "Blue")] <- "Blue"
  temp<- split(temp_mix, temp_mix$band)
  temp2<- do.call("cbind", split(temp_mix, temp_mix$band))
  image<- temp2$Blue.Label[1]
  mix<- mixes_names[i]
  temp3<- data.frame(mix, image, x=temp2[5]$Blue.X, y=temp2[6]$Blue.Y, red=temp2[18]$Red.Mean, green=temp2[11]$Green.Mean, blue=temp2[4]$Blue.Mean)
  training.palette_leaf<- rbind(training.palette_leaf, temp3) 
}
summary(training.palette_leaf) #summarizing the training palette***
count(training.palette_leaf, mix) %>% View #counting observations in each mix of the training palette*** 
##############
##############
##############


##############
##############
##############
## We will now make the random forest models to detect different features in the cropped images.
## A different random forest model will be needed for each feature.
## Here were are detecting three features: 
# 1) the foreground (biological related pixels) from the background
# 2) crown rust from healthy leaf tissue
# 3) stem rust from healthy leaf tissue
#************************#
#model to seperate foreground (biological related pixels)***
palette_selection_bio<- filter(training.palette_leaf, !grepl("CR_", mix) & !grepl("SR_", mix))
palette_selection_bio$classification<- c(rep(1, len=1200),rep(0, len=1201)) #selecting the mixes (1=foreground)***
palette_selection_bio %>% group_by(mix) %>% summarise(avg=mean(classification)) 
rfm_bio_leaf.demo_2.26.19<- randomForest(classification~(red+green+blue),data=palette_selection_bio, ntree=100,mtry = 1,importance=TRUE)
print(rfm_bio_leaf.demo_2.26.19)
plot(rfm_bio_leaf.demo_2.26.19) #ntree is set to 100, that looks about right***
importance(rfm_bio_leaf.demo_2.26.19) #green and blue bands are the most important***
#************************#
#model for crown rust***
palette_selection_CR<- training.palette_leaf
palette_selection_CR$classification<- c(rep(0, len=1200), rep(1, len=700),rep(0, len=1201), rep(0, len=346)) 
palette_selection_CR %>% group_by(mix) %>% summarise(avg=mean(classification))  #check to make sure CR (crown rust related mixes) have a 1***
rfm_CR_leaf.demo_2.26.19<- randomForest(classification~(red+green+blue),data=palette_selection_CR, ntree=100,mtry = 1,importance=TRUE)
print(rfm_CR_leaf.demo_2.26.19)
plot(rfm_CR_leaf.demo_2.26.19)
importance(rfm_CR_leaf.demo_2.26.19) #red and blue bands are the most important***
#************************#
#model for stem rust***
palette_selection_SR<- training.palette_leaf
palette_selection_SR$classification<- c(rep(0, len=1200), rep(0, len=700),rep(0, len=1201), rep(1, len=346))
palette_selection_SR %>% group_by(mix) %>% summarise(avg=mean(classification))  #check to make sure SR (stem rust related mixes) have a 1***
rfm_SR_leaf.demo_2.26.19<- randomForest(classification~(red+green+blue),data=palette_selection_SR, ntree=100,mtry = 1,importance=TRUE)
print(rfm_SR_leaf.demo_2.26.19)
plot(rfm_SR_leaf.demo_2.26.19)
importance(rfm_SR_leaf.demo_2.26.19) #red and blue bands are the most important***
##############
##############
##############

##############
##############
##############
## Running the image processing loop.
## This is a really large loop that is broken up into 6 sections.
#1) seperating foreground from background
#2) seperating crown rust from healthy tissue
#3) conducting morphological operations for crown rust quantification with EBImage
#4) seperating stem rust from healthy tissue
#5) conducting morphological operations for stem rust quantification with EBImage
#6) writing summary statisitics
#************************#
#each path is for an image***
folder_cropped_leaf.demo_2.26.19<-  (paste(img_dir_leaf.demo.2.26.19,"S1_crop_img",sep = "/"))
folder_classify_leaf.demo_2.26.19<- (paste(img_dir_leaf.demo.2.26.19,"S2_foreground_classify",sep = "/"))
folder_EBImage_leaf.demo_2.26.19<-  (paste(img_dir_leaf.demo.2.26.19,"S3_foreground_EBImage",sep = "/"))
folder_CR_classify_leaf.demo_2.26.19<-  (paste(img_dir_leaf.demo.2.26.19,"S4_CR_classify",sep = "/"))
folder_CR_EBImage_leaf.demo_2.26.19<-  (paste(img_dir_leaf.demo.2.26.19,"S5_CR_EBImage",sep = "/"))
folder_SR_classify_leaf.demo_2.26.19<-  (paste(img_dir_leaf.demo.2.26.19,"S6_SR_classify",sep = "/"))
folder_SR_EBImage_leaf.demo_2.26.19<-  (paste(img_dir_leaf.demo.2.26.19,"S7_SR_EBImage",sep = "/"))
#************************#
#check to make sure all the cropped image show up***
paths_cropped_leaf<- list.files(path=folder_cropped_leaf.demo_2.26.19,full.names = TRUE)
names_cropped_leaf<- list.files(path=folder_cropped_leaf.demo_2.26.19,full.names = FALSE) 
#create a data frome to collect numeric output from the analysis***
img.stats_leaf.demo_2.26.19<- data.frame()

for (i in 1:length(paths_cropped_leaf)) {
  img.01<- readJPEG(paths_cropped_leaf[i])
  coor<- as.data.frame(as.table(img.01[,,1]))[1:2]
  red<- 255*as.data.frame(as.table(img.01[,,1]))[3]
  green<- 255*as.data.frame(as.table(img.01[,,2]))[3]
  blue<- 255*as.data.frame(as.table(img.01[,,3]))[3]
  img.dat.01<- cbind(coor, red, green, blue)
  colnames(img.dat.01)<- c("y","x","red","green","blue")
  img.dat.01$classify<- predict(rfm_bio_leaf.demo_2.26.19, img.dat.01)
  img.dat.01$thresh<- ifelse(img.dat.01$classify>0.80, 1,0)  #Set threshold to 80%***
  img.02<- matrix(img.dat.01$thresh, nrow=nrow(img.01), ncol=ncol(img.01))
  writeJPEG(img.02, paste(folder_classify_leaf.demo_2.26.19, "/", names_cropped_leaf[i], sep = ""), quality = 1)
  paths_classify_leaf.01<- list.files(path=folder_classify_leaf.demo_2.26.19,full.names = TRUE)
  morph_op.01<- readImage(paths_classify_leaf.01[i])
  overlay.01<-  readImage(paths_cropped_leaf[i])
  kernal.01<- makeBrush(3, shape='box') #kernal sizes between 3 and 9 seem to work best***
  image_dilate.01<- thresh(dilate(morph_op.01, kernal.01), w=100, h=100, offset= 0.001) #here dilate is used, sometimes 'erode' works better***
  img.03 = stackObjects(image_dilate.01, overlay.01, combine = T, bg.col='black')
  writeImage(img.03, paste(folder_EBImage_leaf.demo_2.26.19, "/", names_cropped_leaf[i] ,sep=""), quality = 100)
  leaf.featr.img.03<- bwlabel(image_dilate.01)
  leaf.featr<- data.frame(computeFeatures.shape(leaf.featr.img.03))
  
  paths_EBImage_leaf<-list.files(path=folder_EBImage_leaf.demo_2.26.19,full.names = TRUE) #starting crown rust***
  img.04<- readJPEG(paths_EBImage_leaf[i])
  coor<- as.data.frame(as.table(img.04[,,1]))[1:2]
  red<- 255*as.data.frame(as.table(img.04[,,1]))[3]
  green<- 255*as.data.frame(as.table(img.04[,,2]))[3]
  blue<- 255*as.data.frame(as.table(img.04[,,3]))[3]
  img.dat.02<- cbind(coor, red, green, blue)
  colnames(img.dat.02)<- c("y","x","red","green","blue")
  img.dat.02$order<- seq(1:length(img.dat.02$x))
  img.dat.02$exclude<- img.dat.02$red+img.dat.02$blue+img.dat.02$green
  img.dat.02_rgb<- filter(img.dat.02, exclude > 0)
  img.dat.02_black<- filter(img.dat.02, exclude == 0)
  img.dat.02_rgb$classify<- predict(rfm_CR_leaf.demo_2.26.19, img.dat.02_rgb)
  img.dat.02_black$classify<- rep(0, times=length(img.dat.02_black$red))
  img_combine.CR<- rbind(img.dat.02_rgb,img.dat.02_black)
  img_combine.CR<- arrange(img_combine.CR, order)
  img_combine.CR$thresh<- ifelse(img_combine.CR$classify>0.80, 1,0) #Set threshold to 80%***
  img.05<- matrix(img_combine.CR$thresh, nrow=nrow(img.04), ncol=ncol(img.04))
  writeJPEG(img.05, paste(folder_CR_classify_leaf.demo_2.26.19, "/", names_cropped_leaf[i] ,sep=""), quality= 1)
  
  paths_classify_CR<- list.files(path=folder_CR_classify_leaf.demo_2.26.19,full.names = TRUE)
  morph_op.02<- readImage(paths_classify_CR[i])
  overlay.02<-  readImage(paths_cropped_leaf[i])
  img.06<- thresh(morph_op.02, w=5, h=5, offset=0.01) #thresholding with a small 5x5 pixel window***
  img.06<- dilate(img.06, makeBrush(3, shape='disc')) #here I am using a dilating operation expanding a neighborhood of pixels with a circular structure***
  img.06 = watershed(distmap(img.06), 1) #idnetifying peaks and valleys in the greyscale image, becasue pustules are small a small radius of 1 is used***
  display(img.06)
  writeImage(colorLabels(img.06), paste(folder_CR_EBImage_leaf.demo_2.26.19, "/", names_cropped_leaf[i], sep=""), quality = 100)
  CR.featr<- data.frame(computeFeatures.shape(img.06, overlay.02))
  
  paths_EBImage_leaf<-list.files(path=folder_EBImage_leaf.demo_2.26.19,full.names = TRUE) #starting stem rust***
  img.07<- readJPEG(paths_EBImage_leaf[i])
  coor<- as.data.frame(as.table(img.07[,,1]))[1:2]
  red<- 255*as.data.frame(as.table(img.07[,,1]))[3]
  green<- 255*as.data.frame(as.table(img.07[,,2]))[3]
  blue<- 255*as.data.frame(as.table(img.07[,,3]))[3]
  img.dat.03<- cbind(coor, red, green, blue)
  colnames(img.dat.03)<- c("y","x","red","green","blue")
  img.dat.03$order<- seq(1:length(img.dat.02$x))
  img.dat.03$exclude<- img.dat.03$red+img.dat.03$blue+img.dat.03$green
  img.dat.03_rgb<- filter(img.dat.03, exclude > 0)
  img.dat.03_black<- filter(img.dat.03, exclude == 0)
  img.dat.03_rgb$classify<- predict(rfm_SR_leaf.demo_2.26.19, img.dat.03_rgb)
  img.dat.03_black$classify<- rep(0, times=length(img.dat.03_black$red))
  img_combine.SR<- rbind(img.dat.03_rgb,img.dat.03_black)
  img_combine.SR<- arrange(img_combine.SR, order)
  img_combine.SR$thresh<- ifelse(img_combine.SR$classify>0.60, 1,0) #Set threshold to 60%***
  img.08<- matrix(img_combine.SR$thresh, nrow=nrow(img.04), ncol=ncol(img.04))
  writeJPEG(img.08, paste(folder_SR_classify_leaf.demo_2.26.19, "/", names_cropped_leaf[i] ,sep=""), quality= 1)
  
  paths_classify_SR<- list.files(path=folder_SR_classify_leaf.demo_2.26.19,full.names = TRUE)
  morph_op.03<- readImage(paths_classify_SR[i])
  overlay.03<-  readImage(paths_cropped_leaf[i])
  img.09<- erode(morph_op.03, makeBrush(3, shape='disc')) #generally, stem rust pustules are larger than crown rust so a eroding operation works well to filter noise***
  img.09<- dilate(img.09, makeBrush(5, shape='disc')) #dilating whatever is left over seems to work, brrush size 5 or 7 works (this will affect average pustule size)***
  img.09<- fillHull(img.09)
  img.09 = watershed(distmap(img.09), 1) #a larger radius could be used if pustule were larger***
  writeImage(colorLabels(img.09), paste(folder_SR_EBImage_leaf.demo_2.26.19, "/", names_cropped_leaf[i], sep=""), quality = 100)
  SR.featr<- data.frame(computeFeatures.shape(img.09, overlay.03))
  
  
  write.stats<- data.frame(img.ID=              str_sub(names_cropped_leaf[i]), #unique image ID***
                           comp.sum.img=        length(img.dat.01$thresh), #total pixels***
                           comp.sum.RF.bio=     sum(img.dat.01$thresh),  #total biological pixels***
                           comp.sum.EBI.bio=    sum(leaf.featr$s.area), #sum of biological pixels after EBImage processing***
                           comp.sum.RF.CR=      sum(img_combine.CR$thresh), #sum all crown rust pixels***
                           comp.pustct.EBI.CR=  length(CR.featr$s.area), #average crown rust pustule count***
                           comp.avgpust.EBI.CR= mean(CR.featr$s.area), #average crown rust pusutle size***
                           comp.sum.EBI.CR=     sum(CR.featr$s.area), #total crown rust pustule area (pixels)***
                           comp.prop.EBI.CR=    (sum(CR.featr$s.area)/sum(leaf.featr$s.area)), #proportion of leaf area infected with crown rust***
                           comp.sum.RF.SR=      sum(img_combine.SR$thresh), 
                           comp.prop.RF.SR=     (sum(img_combine.SR$thresh)/sum(leaf.featr$s.area)), 
                           comp.pustct.EBI.SR=  length(SR.featr$s.area), 
                           comp.avgpust.EBI.SR= mean(SR.featr$s.area), 
                           comp.sum.EBI.SR=     sum(SR.featr$s.area), 
                           comp.prop.EBI.SR=    (sum(SR.featr$s.area)/sum(leaf.featr$s.area))) 
  
  img.stats_leaf.demo_2.26.19<-rbind(img.stats_leaf.demo_2.26.19, write.stats) 
}
#writing the output statistics to the parent directory folder***
write.csv(img.stats_leaf.demo_2.26.19, paste(img_dir_leaf.demo.2.26.19, "results","img.stats_leaf.demo_2.26.19.csv", sep = "/"))
##############
##############
##############

##############
##############
##############
## Now we can check to see how our computer and visual scores match up.
## We will use the random forest, EBImage output, and the orinal field data file (found in results folder) to determine success of the proccess.
## Here are some things to note about how computer ratings differ from visual ratings on the Cobb scale:
#1) The Cobb scale is a 0-100 scale, but is NOT percent severity.
#2) You must multiply Cobb severity by 0.37 to get to percent severity.
#3) The last column in "img.stats_plant.demo_2.25.19" is the most conservative estimate of crown rust (comp.prop.EBI.CR).
#4) This is a proportion not a percent so multiply by 100 to standardize against visual ratings.
#5) Mean pusutle area will change based on the morphological operations used so make sure you are consistant within an experiment.
stats_leaf.demo_2.26.19<- read.csv(paste(img_dir_leaf.demo.2.26.19, "results", "img.stats_leaf.demo_2.26.19.csv", sep = "/"), na.strings = ".")

leaf.demo_2.26.19.output<- cbind(leaf_field.dat,stats_leaf.demo_2.26.19) %>%
  mutate(visual.percent.CR = ratr.med.CR.sev * 0.37) %>%
  mutate(visual.percent.SR = ratr.med.SR.sev * 0.37) %>%
  mutate(computer.percent.CR = comp.prop.EBI.CR * 100) %>%
  mutate(computer.percent.SR = comp.prop.EBI.SR * 100)
summary(leaf.demo_2.26.19.output)

## Plotting data

#crown rust severity
ggplot(leaf.demo_2.26.19.output, aes(x = visual.percent.CR, y = computer.percent.CR))+
  labs(x = "Visual crown rust severity", y= "Computer crown rust severity")+
  geom_point()

#stem rust severity
ggplot(leaf.demo_2.26.19.output, aes(x = visual.percent.SR, y = computer.percent.SR))+
  labs(x = "Visual stem rust severity", y= "Computer stem rust severity")+
  geom_point()

#crown rust putule count
ggplot(leaf.demo_2.26.19.output, aes(x = manual.pust.ct.CR, y = comp.pustct.EBI.CR))+
  labs(x = "Manual crown rust pustule count", y= "Computer crown rust pustule count")+
  geom_point()
#the computer is nearly perfect for crown rust***

#stem rust pustule count 
ggplot(leaf.demo_2.26.19.output, aes(x = manual.pust.ct.SR, y = comp.pustct.EBI.SR))+
  labs(x = "Manual stem rust pustule count", y= "Computer stem rust pustule count")+
  geom_point()
#it looks like the computer was a little off for stem rust***







