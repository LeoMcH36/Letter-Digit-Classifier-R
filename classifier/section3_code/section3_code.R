#section 3
setwd("C:/assignment2_40225145")

#library to create tables
#https://www.rdocumentation.org/packages/gt
library(gt)

#library for fread function
#https://www.rdocumentation.org/packages/data.table
library(data.table)

#set seed
set.seed(42)


#section 3.1

#https://www.rdocumentation.org/packages/RColorBrewer
#library to get colours for graphs
library(RColorBrewer)

#library to get graphs
#https://www.rdocumentation.org/packages/ggplot2
library(ggplot2)

#histogram for number of pixels in an image
nr_pix <- fread("section2_features/40225145_features.csv",select = c(3),skip = 1)
nr_pix

pixels <- unlist(nr_pix)

nr_pixHis <- ggplot(as.data.frame(pixels), aes(x = pixels)) +
  geom_histogram(bins = 30,fill = "#dba4c7") +
  ggtitle("Number of pixels")

nr_pixHis


#histogram for  height of image in pixels
height <- fread("section2_features/40225145_features.csv",select = c(8),skip = 1)
height
imgHeight <- unlist(height)
height_His <- ggplot(as.data.frame(imgHeight), aes(x = imgHeight)) +
  geom_histogram(bins = 10,fill = "#f77f39") + 
  ggtitle("Height of symbol in pixels")

height_His

#histogram for  columns with 3 or more pixels in them
col3p <- fread("section2_features/40225145_features.csv",select = c(7),skip = 1)
col3p
imgcol3p <- unlist(col3p)
col3p_His <- ggplot(as.data.frame(imgcol3p), aes(x = imgcol3p)) +
  geom_histogram(bins = 15,fill = "#5fb7ed") +
  ggtitle("Number of columns with 3 or more pixels")

col3p_His


#section 3.2

pixels_digit <- fread("section2_features/40225145_features.csv",select = c(3),skip = 57,nrows= 56)
pixels_digit <- unlist(pixels_digit)

#probability that a randomly selected digit image has a pixel count > 20
mu <- mean(pixels_digit)
mu
sd <- sd(pixels_digit)
sd

prb_greaterThan_20 <- 1- pnorm(20,mu,sd)
prb_greaterThan_20



#section 3.3


#values for tables

#stats for letters

#letter averages
pixels_Letters <- fread("section2_features/40225145_features.csv",select = c(3),skip = 1,nrows= 56)
#pixels_Letters
mu_Letters_px <- mean(unlist(pixels_Letters))
#number of pixels avg
mu_Letters_px


rows2_Letters <- fread("section2_features/40225145_features.csv",select = c(4),skip = 1,nrows= 56)
#rows2_Letters
mu_Letters_rows2 <- mean(unlist(rows2_Letters))
#number of  rows with > pixels avg
mu_Letters_rows2

cols2_Letters <- fread("section2_features/40225145_features.csv",select = c(5),skip = 1,nrows= 56)
#cols2_Letters
mu_Letters_cols2 <- mean(unlist(cols2_Letters))
#number of cols with >  2 pixels avg
mu_Letters_cols2


rows3_Letters <- fread("section2_features/40225145_features.csv",select = c(6),skip = 1,nrows= 56)
#rows3_Letters
mu_Letters_rows3 <- mean(unlist(rows3_Letters))
#number of  rows with > 3 pixels avg
mu_Letters_rows3


cols3_Letters <- fread("section2_features/40225145_features.csv",select = c(7),skip = 1,nrows= 56)
#cols3_Letters
mu_Letters_cols3 <- mean(unlist(cols3_Letters))
#number of  cols with > 3 pixels avg
mu_Letters_cols3


height_Letters <- fread("section2_features/40225145_features.csv",select = c(8),skip = 1,nrows= 56)
#height_Letters
mu_Letters_height <- mean(unlist(height_Letters))
#height avg
mu_Letters_height


width_Letters <- fread("section2_features/40225145_features.csv",select = c(9),skip = 1,nrows= 56)
#width_Letters
mu_Letters_width <- mean(unlist(width_Letters))
#width avg
mu_Letters_width


left2_Letters <- fread("section2_features/40225145_features.csv",select = c(10),skip = 1,nrows= 56)
#left2_Letters
mu_Letters_left2 <- mean(unlist(left2_Letters))
#number of left2 tiles avg
mu_Letters_left2

right2_Letters <- fread("section2_features/40225145_features.csv",select = c(11),skip = 1,nrows= 56)
#right2_Letters
mu_Letters_right2 <- mean(unlist(right2_Letters))
#number of right2 tiles avg
mu_Letters_right2

verticalness_Letters <- fread("section2_features/40225145_features.csv",select = c(12),skip = 1,nrows= 56)
#verticalness_Letters
mu_Letters_verticalness <- mean(unlist(verticalness_Letters))
#verticalness avg
mu_Letters_verticalness

top2_Letters <- fread("section2_features/40225145_features.csv",select = c(13),skip = 1,nrows= 56)
#top2_Letters
mu_Letters_top2 <- mean(unlist(top2_Letters))
#number of top2 tiles avg
mu_Letters_top2

bottom2_Letters <- fread("section2_features/40225145_features.csv",select = c(14),skip = 1,nrows= 56)
#bottom2_Letters
mu_Letters_bottom2 <- mean(unlist(bottom2_Letters))
#number of bottom2 tiles avg
mu_Letters_bottom2

horizontalness_Letters <- fread("section2_features/40225145_features.csv",select = c(15),skip = 1,nrows= 56)
#horizontalness_Letters
mu_Letters_horizontalness <- mean(unlist(horizontalness_Letters))
#number of images with rows with > pixels avg
mu_Letters_horizontalness

curvedness_Letters <- fread("section2_features/40225145_features.csv",select = c(16),skip = 1,nrows= 56)
#curvedness_Letters
mu_Letters_curvedness <- mean(unlist(curvedness_Letters))
#number of images with rows with > pixels avg
mu_Letters_curvedness


#letter standard deviations
pixels_Letters <- fread("section2_features/40225145_features.csv",select = c(3),skip = 1,nrows= 56)
#pixels_Letters
sd_Letters_px <- sd(unlist(pixels_Letters))
#sd of pixels 
sd_Letters_px


rows2_Letters <- fread("section2_features/40225145_features.csv",select = c(4),skip = 1,nrows= 56)
#rows2_Letters
sd_Letters_rows2 <- sd(unlist(rows2_Letters))
#sd of  rows with > pixels 
sd_Letters_rows2

cols2_Letters <- fread("section2_features/40225145_features.csv",select = c(5),skip = 1,nrows= 56)
#cols2_Letters
sd_Letters_cols2 <- sd(unlist(cols2_Letters))
#sd of cols with >  2 pixels 
sd_Letters_cols2


rows3_Letters <- fread("section2_features/40225145_features.csv",select = c(6),skip = 1,nrows= 56)
#rows3_Letters
sd_Letters_rows3 <- sd(unlist(rows3_Letters))
#sd of  rows with > 3 pixels 
sd_Letters_rows3


cols3_Letters <- fread("section2_features/40225145_features.csv",select = c(7),skip = 1,nrows= 56)
#cols3_Letters
sd_Letters_cols3 <- sd(unlist(cols3_Letters))
#sd of  cols with > 3 pixels 
sd_Letters_cols3


height_Letters <- fread("section2_features/40225145_features.csv",select = c(8),skip = 1,nrows= 56)
#height_Letters
sd_Letters_height <- sd(unlist(height_Letters))
#height sd
sd_Letters_height


width_Letters <- fread("section2_features/40225145_features.csv",select = c(9),skip = 1,nrows= 56)
#width_Letters
sd_Letters_width <- sd(unlist(width_Letters))
#width sd
sd_Letters_width


left2_Letters <- fread("section2_features/40225145_features.csv",select = c(10),skip = 1,nrows= 56)
#left2_Letters
sd_Letters_left2 <- sd(unlist(left2_Letters))
#number of left2 tiles sd
sd_Letters_left2

right2_Letters <- fread("section2_features/40225145_features.csv",select = c(11),skip = 1,nrows= 56)
#right2_Letters
sd_Letters_right2 <- sd(unlist(right2_Letters))
#number of right2 tiles sd
sd_Letters_right2

verticalness_Letters <- fread("section2_features/40225145_features.csv",select = c(12),skip = 1,nrows= 56)
#verticalness_Letters
sd_Letters_verticalness <- sd(unlist(verticalness_Letters))
#verticalness sd
sd_Letters_verticalness

top2_Letters <- fread("section2_features/40225145_features.csv",select = c(13),skip = 1,nrows= 56)
#top2_Letters
sd_Letters_top2 <- sd(unlist(top2_Letters))
#sd of top2 tiles 
sd_Letters_top2

bottom2_Letters <- fread("section2_features/40225145_features.csv",select = c(14),skip = 1,nrows= 56)
#bottom2_Letters
sd_Letters_bottom2 <- sd(unlist(bottom2_Letters))
#sd of bottom2 tiles 
sd_Letters_bottom2

horizontalness_Letters <- fread("section2_features/40225145_features.csv",select = c(15),skip = 1,nrows= 56)
#horizontalness_Letters
sd_Letters_horizontalness <- sd(unlist(horizontalness_Letters))
#sd of images with rows with > pixels 
sd_Letters_horizontalness

curvedness_Letters <- fread("section2_features/40225145_features.csv",select = c(16),skip = 1,nrows= 56)
#curvedness_Letters
sd_Letters_curvedness <- sd(unlist(curvedness_Letters))
#sd of images with rows with > pixels 
sd_Letters_curvedness



#letter variance
pixels_Letters <- fread("section2_features/40225145_features.csv",select = c(3),skip = 1,nrows= 56)
#pixels_Letters
var_Letters_px <- var(unlist(pixels_Letters))
#var of pixels 
var_Letters_px


rows2_Letters <- fread("section2_features/40225145_features.csv",select = c(4),skip = 1,nrows= 56)
#rows2_Letters
var_Letters_rows2 <- var(unlist(rows2_Letters))
#var of  rows with > pixels 
var_Letters_rows2

cols2_Letters <- fread("section2_features/40225145_features.csv",select = c(5),skip = 1,nrows= 56)
#cols2_Letters
var_Letters_cols2 <- var(unlist(cols2_Letters))
#var of cols with >  2 pixels 
var_Letters_cols2


rows3_Letters <- fread("section2_features/40225145_features.csv",select = c(6),skip = 1,nrows= 56)
#rows3_Letters
var_Letters_rows3 <- var(unlist(rows3_Letters))
#var of  rows with > 3 pixels 
var_Letters_rows3


cols3_Letters <- fread("section2_features/40225145_features.csv",select = c(7),skip = 1,nrows= 56)
#cols3_Letters
var_Letters_cols3 <- var(unlist(cols3_Letters))
#var of  cols with > 3 pixels 
var_Letters_cols3


height_Letters <- fread("section2_features/40225145_features.csv",select = c(8),skip = 1,nrows= 56)
#height_Letters
var_Letters_height <- var(unlist(height_Letters))
#height var
var_Letters_height


width_Letters <- fread("section2_features/40225145_features.csv",select = c(9),skip = 1,nrows= 56)
#width_Letters
var_Letters_width <- var(unlist(width_Letters))
#width var
var_Letters_width


left2_Letters <- fread("section2_features/40225145_features.csv",select = c(10),skip = 1,nrows= 56)
#left2_Letters
var_Letters_left2 <- var(unlist(left2_Letters))
#number of left2 tiles var
var_Letters_left2

right2_Letters <- fread("section2_features/40225145_features.csv",select = c(11),skip = 1,nrows= 56)
#right2_Letters
var_Letters_right2 <- var(unlist(right2_Letters))
#number of right2 tiles var
var_Letters_right2

verticalness_Letters <- fread("section2_features/40225145_features.csv",select = c(12),skip = 1,nrows= 56)
#verticalness_Letters
var_Letters_verticalness <- var(unlist(verticalness_Letters))
#verticalness var
var_Letters_verticalness

top2_Letters <- fread("section2_features/40225145_features.csv",select = c(13),skip = 1,nrows= 56)
#top2_Letters
var_Letters_top2 <- var(unlist(top2_Letters))
#var of top2 tiles 
var_Letters_top2

bottom2_Letters <- fread("section2_features/40225145_features.csv",select = c(14),skip = 1,nrows= 56)
#bottom2_Letters
var_Letters_bottom2 <- var(unlist(bottom2_Letters))
#var of bottom2 tiles 
var_Letters_bottom2

horizontalness_Letters <- fread("section2_features/40225145_features.csv",select = c(15),skip = 1,nrows= 56)
#horizontalness_Letters
var_Letters_horizontalness <- var(unlist(horizontalness_Letters))
#var of images with rows with > pixels 
var_Letters_horizontalness

curvedness_Letters <- fread("section2_features/40225145_features.csv",select = c(16),skip = 1,nrows= 56)
#curvedness_Letters
var_Letters_curvedness <- var(unlist(curvedness_Letters))
#var of images with rows with > pixels 
var_Letters_curvedness

#stats for numbers

#number average
pixels_Numbers <- fread("section2_features/40225145_features.csv",select = c(3),skip = 57,nrows= 56)
#pixels_Numbers
mu_Numbers_px <- mean(unlist(pixels_Numbers))
#number of pixels avg
mu_Numbers_px


rows2_Numbers <- fread("section2_features/40225145_features.csv",select = c(4),skip = 57,nrows= 56)
#rows2_Numbers
mu_Numbers_rows2 <- mean(unlist(rows2_Numbers))
#number of  rows with > pixels avg
mu_Numbers_rows2

cols2_Numbers <- fread("section2_features/40225145_features.csv",select = c(5),skip = 57,nrows= 56)
#cols2_Numbers
mu_Numbers_cols2 <- mean(unlist(cols2_Numbers))
#number of cols with >  2 pixels avg
mu_Numbers_cols2


rows3_Numbers <- fread("section2_features/40225145_features.csv",select = c(6),skip = 57,nrows= 56)
#rows3_Numbers
mu_Numbers_rows3 <- mean(unlist(rows3_Numbers))
#number of  rows with > 3 pixels avg
mu_Numbers_rows3


cols3_Numbers <- fread("section2_features/40225145_features.csv",select = c(7),skip = 57,nrows= 56)
#cols3_Numbers
mu_Numbers_cols3 <- mean(unlist(cols3_Numbers))
#number of  cols with > 3 pixels avg
mu_Numbers_cols3


height_Numbers <- fread("section2_features/40225145_features.csv",select = c(8),skip = 57,nrows= 56)
#height_Numbers
mu_Numbers_height <- mean(unlist(height_Numbers))
#height avg
mu_Numbers_height


width_Numbers <- fread("section2_features/40225145_features.csv",select = c(9),skip = 57,nrows= 56)
#width_Numbers
mu_Numbers_width <- mean(unlist(width_Numbers))
#width avg
mu_Numbers_width


left2_Numbers <- fread("section2_features/40225145_features.csv",select = c(10),skip = 57,nrows= 56)
#left2_Numbers
mu_Numbers_left2 <- mean(unlist(left2_Numbers))
#number of left2 tiles avg
mu_Numbers_left2

right2_Numbers <- fread("section2_features/40225145_features.csv",select = c(11),skip = 57,nrows= 56)
#right2_Numbers
mu_Numbers_right2 <- mean(unlist(right2_Numbers))
#number of right2 tiles avg
mu_Numbers_right2

verticalness_Numbers <- fread("section2_features/40225145_features.csv",select = c(12),skip = 57,nrows= 56)
#verticalness_Numbers
mu_Numbers_verticalness <- mean(unlist(verticalness_Numbers))
#verticalness avg
mu_Numbers_verticalness

top2_Numbers <- fread("section2_features/40225145_features.csv",select = c(13),skip = 57,nrows= 56)
#top2_Numbers
mu_Numbers_top2 <- mean(unlist(top2_Numbers))
#number of top2 tiles avg
mu_Numbers_top2

bottom2_Numbers <- fread("section2_features/40225145_features.csv",select = c(14),skip = 57,nrows= 56)
#bottom2_Numbers
mu_Numbers_bottom2 <- mean(unlist(bottom2_Numbers))
#number of bottom2 tiles avg
mu_Numbers_bottom2

horizontalness_Numbers <- fread("section2_features/40225145_features.csv",select = c(15),skip = 57,nrows= 56)
#horizontalness_Numbers
mu_Numbers_horizontalness <- mean(unlist(horizontalness_Numbers))
#number of images with rows with > pixels avg
mu_Numbers_horizontalness

curvedness_Numbers <- fread("section2_features/40225145_features.csv",select = c(16),skip = 57,nrows= 56)
#curvedness_Numbers
mu_Numbers_curvedness <- mean(unlist(curvedness_Numbers))
#number of images with rows with > pixels avg
mu_Numbers_curvedness


#number standard deviations
pixels_Numbers <- fread("section2_features/40225145_features.csv",select = c(3),skip = 57,nrows= 56)
#pixels_Numbers
sd_Numbers_px <- sd(unlist(pixels_Numbers))
#sd of pixels 
sd_Numbers_px


rows2_Numbers <- fread("section2_features/40225145_features.csv",select = c(4),skip = 57,nrows= 56)
#rows2_Numbers
sd_Numbers_rows2 <- sd(unlist(rows2_Numbers))
#sd of  rows with > pixels 
sd_Numbers_rows2

cols2_Numbers <- fread("section2_features/40225145_features.csv",select = c(5),skip = 57,nrows= 56)
#cols2_Numbers
sd_Numbers_cols2 <- sd(unlist(cols2_Numbers))
#sd of cols with >  2 pixels 
sd_Numbers_cols2


rows3_Numbers <- fread("section2_features/40225145_features.csv",select = c(6),skip = 57,nrows= 56)
#rows3_Numbers
sd_Numbers_rows3 <- sd(unlist(rows3_Numbers))
#sd of  rows with > 3 pixels 
sd_Numbers_rows3


cols3_Numbers <- fread("section2_features/40225145_features.csv",select = c(7),skip = 57,nrows= 56)
#cols3_Numbers
sd_Numbers_cols3 <- sd(unlist(cols3_Numbers))
#sd of  cols with > 3 pixels 
sd_Numbers_cols3


height_Numbers <- fread("section2_features/40225145_features.csv",select = c(8),skip = 57,nrows= 56)
#height_Numbers
sd_Numbers_height <- sd(unlist(height_Numbers))
#height sd
sd_Numbers_height


width_Numbers <- fread("section2_features/40225145_features.csv",select = c(9),skip = 57,nrows= 56)
#width_Numbers
sd_Numbers_width <- sd(unlist(width_Numbers))
#width sd
sd_Numbers_width


left2_Numbers <- fread("section2_features/40225145_features.csv",select = c(10),skip = 57,nrows= 56)
#left2_Numbers
sd_Numbers_left2 <- sd(unlist(left2_Numbers))
#number of left2 tiles sd
sd_Numbers_left2

right2_Numbers <- fread("section2_features/40225145_features.csv",select = c(11),skip = 57,nrows= 56)
#right2_Numbers
sd_Numbers_right2 <- sd(unlist(right2_Numbers))
#number of right2 tiles sd
sd_Numbers_right2

verticalness_Numbers <- fread("section2_features/40225145_features.csv",select = c(12),skip = 57,nrows= 56)
#verticalness_Numbers
sd_Numbers_verticalness <- sd(unlist(verticalness_Numbers))
#verticalness sd
sd_Numbers_verticalness

top2_Numbers <- fread("section2_features/40225145_features.csv",select = c(13),skip = 57,nrows= 56)
#top2_Numbers
sd_Numbers_top2 <- sd(unlist(top2_Numbers))
#sd of top2 tiles 
sd_Numbers_top2

bottom2_Numbers <- fread("section2_features/40225145_features.csv",select = c(14),skip = 57,nrows= 56)
#bottom2_Numbers
sd_Numbers_bottom2 <- sd(unlist(bottom2_Numbers))
#sd of bottom2 tiles 
sd_Numbers_bottom2

horizontalness_Numbers <- fread("section2_features/40225145_features.csv",select = c(15),skip = 57,nrows= 56)
#horizontalness_Numbers
sd_Numbers_horizontalness <- sd(unlist(horizontalness_Numbers))
#sd of images with rows with > pixels 
sd_Numbers_horizontalness

curvedness_Numbers <- fread("section2_features/40225145_features.csv",select = c(16),skip = 57,nrows= 56)
#curvedness_Numbers
sd_Numbers_curvedness <- sd(unlist(curvedness_Numbers))
#sd of images with rows with > pixels 
sd_Numbers_curvedness



#number variance
pixels_Numbers <- fread("section2_features/40225145_features.csv",select = c(3),skip = 57,nrows= 56)
#pixels_Numbers
var_Numbers_px <- var(unlist(pixels_Numbers))
#var of pixels 
var_Numbers_px


rows2_Numbers <- fread("section2_features/40225145_features.csv",select = c(4),skip = 57,nrows= 56)
#rows2_Numbers
var_Numbers_rows2 <- var(unlist(rows2_Numbers))
#var of  rows with > pixels 
var_Numbers_rows2

cols2_Numbers <- fread("section2_features/40225145_features.csv",select = c(5),skip = 57,nrows= 56)
#cols2_Numbers
var_Numbers_cols2 <- var(unlist(cols2_Numbers))
#var of cols with >  2 pixels 
var_Numbers_cols2


rows3_Numbers <- fread("section2_features/40225145_features.csv",select = c(6),skip = 57,nrows= 56)
#rows3_Numbers
var_Numbers_rows3 <- var(unlist(rows3_Numbers))
#var of  rows with > 3 pixels 
var_Numbers_rows3


cols3_Numbers <- fread("section2_features/40225145_features.csv",select = c(7),skip = 57,nrows= 56)
#cols3_Numbers
var_Numbers_cols3 <- var(unlist(cols3_Numbers))
#var of  cols with > 3 pixels 
var_Numbers_cols3


height_Numbers <- fread("section2_features/40225145_features.csv",select = c(8),skip = 57,nrows= 56)
#height_Numbers
var_Numbers_height <- var(unlist(height_Numbers))
#height var
var_Numbers_height


width_Numbers <- fread("section2_features/40225145_features.csv",select = c(9),skip = 57,nrows= 56)
#width_Numbers
var_Numbers_width <- var(unlist(width_Numbers))
#width var
var_Numbers_width


left2_Numbers <- fread("section2_features/40225145_features.csv",select = c(10),skip = 57,nrows= 56)
#left2_Numbers
var_Numbers_left2 <- var(unlist(left2_Numbers))
#number of left2 tiles var
var_Numbers_left2

right2_Numbers <- fread("section2_features/40225145_features.csv",select = c(11),skip = 57,nrows= 56)
#right2_Numbers
var_Numbers_right2 <- var(unlist(right2_Numbers))
#number of right2 tiles var
var_Numbers_right2

verticalness_Numbers <- fread("section2_features/40225145_features.csv",select = c(12),skip = 57,nrows= 56)
#verticalness_Numbers
var_Numbers_verticalness <- var(unlist(verticalness_Numbers))
#verticalness var
var_Numbers_verticalness

top2_Numbers <- fread("section2_features/40225145_features.csv",select = c(13),skip = 57,nrows= 56)
#top2_Numbers
var_Numbers_top2 <- var(unlist(top2_Numbers))
#var of top2 tiles 
var_Numbers_top2

bottom2_Numbers <- fread("section2_features/40225145_features.csv",select = c(14),skip = 57,nrows= 56)
#bottom2_Numbers
var_Numbers_bottom2 <- var(unlist(bottom2_Numbers))
#var of bottom2 tiles 
var_Numbers_bottom2

horizontalness_Numbers <- fread("section2_features/40225145_features.csv",select = c(15),skip = 57,nrows= 56)
#horizontalness_Numbers
var_Numbers_horizontalness <- var(unlist(horizontalness_Numbers))
#var of images with rows with > pixels 
var_Numbers_horizontalness

curvedness_Numbers <- fread("section2_features/40225145_features.csv",select = c(16),skip = 57,nrows= 56)
#curvedness_Numbers
var_Numbers_curvedness <- var(unlist(curvedness_Numbers))
#var of images with rows with > pixels 
var_Numbers_curvedness


#stats for total

#total average
pixels_Total <- fread("section2_features/40225145_features.csv",select = c(3),skip = 1,nrows= 168)
#pixels_Total
mu_Total_px <- mean(unlist(pixels_Total))
#number of pixels avg
mu_Total_px


rows2_Total <- fread("section2_features/40225145_features.csv",select = c(4),skip = 1,nrows= 168)
#rows2_Total
mu_Total_rows2 <- mean(unlist(rows2_Total))
#number of  rows with > pixels avg
mu_Total_rows2

cols2_Total <- fread("section2_features/40225145_features.csv",select = c(5),skip = 1,nrows= 168)
#cols2_Total
mu_Total_cols2 <- mean(unlist(cols2_Total))
#number of cols with >  2 pixels avg
mu_Total_cols2


rows3_Total <- fread("section2_features/40225145_features.csv",select = c(6),skip = 1,nrows= 168)
#rows3_Total
mu_Total_rows3 <- mean(unlist(rows3_Total))
#number of  rows with > 3 pixels avg
mu_Total_rows3


cols3_Numbers <- fread("section2_features/40225145_features.csv",select = c(7),skip = 1,nrows= 168)
#cols3_Numbers
mu_Total_cols3 <- mean(unlist(cols3_Numbers))
#number of  cols with > 3 pixels avg
mu_Total_cols3


height_Total <- fread("section2_features/40225145_features.csv",select = c(8),skip = 1,nrows= 168)
#height_Total
mu_Total_height <- mean(unlist(height_Total))
#height avg
mu_Total_height


width_Total <- fread("section2_features/40225145_features.csv",select = c(9),skip = 1,nrows= 168)
#width_Total
mu_Total_width <- mean(unlist(width_Total))
#width avg
mu_Total_width


left2_Total <- fread("section2_features/40225145_features.csv",select = c(10),skip = 1,nrows= 168)
#left2_Total
mu_Total_left2 <- mean(unlist(left2_Total))
#number of left2 tiles avg
mu_Total_left2

right2_Total <- fread("section2_features/40225145_features.csv",select = c(11),skip = 1,nrows= 168)
#right2_Total
mu_Total_right2 <- mean(unlist(right2_Total))
#number of right2 tiles avg
mu_Total_right2

verticalness_Total <- fread("section2_features/40225145_features.csv",select = c(12),skip = 1,nrows= 168)
#verticalness_Total
mu_Total_verticalness <- mean(unlist(verticalness_Total))
#verticalness avg
mu_Numbers_verticalness

top2_Total <- fread("section2_features/40225145_features.csv",select = c(13),skip = 1,nrows= 168)
#top2_Total
mu_Total_top2 <- mean(unlist(top2_Total))
#number of top2 tiles avg
mu_Total_top2

bottom2_Total <- fread("section2_features/40225145_features.csv",select = c(14),skip = 1,nrows= 168)
#bottom2_Total
mu_Total_bottom2 <- mean(unlist(bottom2_Total))
#number of bottom2 tiles avg
mu_Total_bottom2

horizontalness_Total <- fread("section2_features/40225145_features.csv",select = c(15),skip = 1,nrows= 168)
#horizontalness_Total
mu_Total_horizontalness <- mean(unlist(horizontalness_Total))
#number of images with rows with > pixels avg
mu_Total_horizontalness

curvedness_Total <- fread("section2_features/40225145_features.csv",select = c(16),skip = 1,nrows= 168)
#curvedness_Total
mu_Total_curvedness <- mean(unlist(curvedness_Total))
#number of images with rows with > pixels avg
mu_Total_curvedness


#number standard deviations
pixels_Total <- fread("section2_features/40225145_features.csv",select = c(3),skip = 1,nrows= 168)
#pixels_Total
sd_Total_px <- sd(unlist(pixels_Total))
#sd of pixels 
sd_Numbers_px


rows2_Total <- fread("section2_features/40225145_features.csv",select = c(4),skip = 1,nrows= 168)
#rows2_Total
sd_Total_rows2 <- sd(unlist(rows2_Total))
#sd of  rows with > pixels 
sd_Total_rows2

cols2_Total <- fread("section2_features/40225145_features.csv",select = c(5),skip = 1,nrows= 168)
#cols2_Total
sd_Total_cols2 <- sd(unlist(cols2_Total))
#sd of cols with >  2 pixels 
sd_Total_cols2


rows3_Total <- fread("section2_features/40225145_features.csv",select = c(6),skip = 1,nrows= 168)
#rows3_Total
sd_Total_rows3 <- sd(unlist(rows3_Total))
#sd of  rows with > 3 pixels 
sd_Total_rows3


cols3_Total <- fread("section2_features/40225145_features.csv",select = c(7),skip = 1,nrows= 168)
#cols3_Total
sd_Total_cols3 <- sd(unlist(cols3_Total))
#sd of  cols with > 3 pixels 
sd_Total_cols3


height_Total <- fread("section2_features/40225145_features.csv",select = c(8),skip = 1,nrows= 168)
#height_Total
sd_Total_height <- sd(unlist(height_Total))
#height sd
sd_Total_height


width_Total <- fread("section2_features/40225145_features.csv",select = c(9),skip = 1,nrows= 168)
#width_Total
sd_Total_width <- sd(unlist(width_Total))
#width sd
sd_Total_width


left2_Total <- fread("section2_features/40225145_features.csv",select = c(10),skip = 1,nrows= 168)
#left2_Total
sd_Total_left2 <- sd(unlist(left2_Total))
#number of left2 tiles sd
sd_Total_left2

right2_Total <- fread("section2_features/40225145_features.csv",select = c(11),skip = 1,nrows= 168)
#right2_Total
sd_Total_right2 <- sd(unlist(right2_Total))
#number of right2 tiles sd
sd_Total_right2

verticalness_Total <- fread("section2_features/40225145_features.csv",select = c(12),skip = 1,nrows= 168)
#verticalness_Total
sd_Total_verticalness <- sd(unlist(verticalness_Total))
#verticalness sd
sd_Total_verticalness

top2_Total <- fread("section2_features/40225145_features.csv",select = c(13),skip = 1,nrows= 168)
#top2_Total
sd_Total_top2 <- sd(unlist(top2_Total))
#sd of top2 tiles 
sd_Total_top2

bottom2_Total <- fread("section2_features/40225145_features.csv",select = c(14),skip = 1,nrows= 168)
#bottom2_Total
sd_Total_bottom2 <- sd(unlist(bottom2_Total))
#sd of bottom2 tiles 
sd_Total_bottom2

horizontalness_Total <- fread("section2_features/40225145_features.csv",select = c(15),skip = 1,nrows= 168)
#horizontalness_Total
sd_Total_horizontalness <- sd(unlist(horizontalness_Total))
#sd of images with rows with > pixels 
sd_Total_horizontalness

curvedness_Total <- fread("section2_features/40225145_features.csv",select = c(16),skip = 1,nrows= 168)
#curvedness_Total
sd_Total_curvedness <- sd(unlist(curvedness_Total))
#sd of images with rows with > pixels 
sd_Total_curvedness



#number variance
pixels_Total <- fread("section2_features/40225145_features.csv",select = c(3),skip = 1,nrows= 168)
#pixels_Total
var_Total_px <- var(unlist(pixels_Total))
#var of pixels 
var_Total_px


rows2_Total <- fread("section2_features/40225145_features.csv",select = c(4),skip = 1,nrows= 168)
#rows2_Total
var_Total_rows2 <- var(unlist(rows2_Total))
#var of  rows with > pixels 
var_Total_rows2

cols2_Total <- fread("section2_features/40225145_features.csv",select = c(5),skip = 1,nrows= 168)
#cols2_Total
var_Total_cols2 <- var(unlist(cols2_Total))
#var of cols with >  2 pixels 
var_Total_cols2


rows3_Total <- fread("section2_features/40225145_features.csv",select = c(6),skip = 1,nrows= 168)
#rows3_Total
var_Total_rows3 <- var(unlist(rows3_Total))
#var of  rows with > 3 pixels 
var_Total_rows3


cols3_Total <- fread("section2_features/40225145_features.csv",select = c(7),skip = 1,nrows= 168)
#cols3_Total
var_Total_cols3 <- var(unlist(cols3_Total))
#var of  cols with > 3 pixels 
var_Numbers_cols3


height_Total <- fread("section2_features/40225145_features.csv",select = c(8),skip = 1,nrows= 168)
#height_Total
var_Total_height <- var(unlist(height_Total))
#height var
var_Total_height


width_Total <- fread("section2_features/40225145_features.csv",select = c(9),skip = 1,nrows= 168)
#width_Total
var_Total_width <- var(unlist(width_Total))
#width var
var_Total_width


left2_Total <- fread("section2_features/40225145_features.csv",select = c(10),skip = 1,nrows= 168)
#left2_Total
var_Total_left2 <- var(unlist(left2_Total))
#number of left2 tiles var
var_Total_left2

right2_Total <- fread("section2_features/40225145_features.csv",select = c(11),skip = 1,nrows= 168)
#right2_Total
var_Total_right2 <- var(unlist(right2_Total))
#number of right2 tiles var
var_Total_right2

verticalness_Total <- fread("section2_features/40225145_features.csv",select = c(12),skip = 1,nrows= 168)
#verticalness_Total
var_Total_verticalness <- var(unlist(verticalness_Total))
#verticalness var
var_Total_verticalness

top2_Total <- fread("section2_features/40225145_features.csv",select = c(13),skip = 1,nrows= 168)
#top2_Total
var_Total_top2 <- var(unlist(top2_Total))
#var of top2 tiles 
var_Total_top2

bottom2_Total <- fread("section2_features/40225145_features.csv",select = c(14),skip = 1,nrows= 168)
#bottom2_Total
var_Total_bottom2 <- var(unlist(bottom2_Total))
#var of bottom2 tiles 
var_Total_bottom2

horizontalness_Total <- fread("section2_features/40225145_features.csv",select = c(15),skip = 1,nrows= 168)
#horizontalness_Total
var_Total_horizontalness <- var(unlist(horizontalness_Total))
#var of images with rows with > pixels 
var_Numbers_horizontalness

curvedness_Total <- fread("section2_features/40225145_features.csv",select = c(16),skip = 1,nrows= 168)
#curvedness_Total
var_Total_curvedness <- var(unlist(curvedness_Total))
#var of images with rows with > pixels 
var_Total_curvedness


#table for averages
LettersAvg <- c(mu_Letters_px,mu_Letters_rows2,mu_Letters_cols2,mu_Letters_rows3,mu_Letters_cols3,mu_Letters_height,mu_Letters_width,mu_Letters_left2,mu_Letters_right2,mu_Letters_verticalness,
                mu_Letters_top2,mu_Letters_bottom2,mu_Letters_horizontalness,mu_Letters_curvedness)
NumbersAvg <- c(mu_Numbers_px,mu_Numbers_rows2,mu_Numbers_cols2,mu_Numbers_rows3,mu_Numbers_cols3,mu_Numbers_height,mu_Numbers_width,mu_Numbers_left2,mu_Numbers_right2,mu_Numbers_verticalness,
                mu_Numbers_top2,mu_Numbers_bottom2,mu_Numbers_horizontalness,mu_Numbers_curvedness)
TotalAvg <- c(mu_Total_px,mu_Total_rows2,mu_Total_cols2,mu_Total_rows3,mu_Total_cols3,mu_Total_height,mu_Total_width,mu_Total_left2,mu_Total_right2,mu_Total_verticalness,
              mu_Total_top2,mu_Total_bottom2,mu_Total_horizontalness,mu_Total_curvedness)
difference <- (( NumbersAvg/LettersAvg ) * 100)


difference <- difference - 100

#create colour pallet
diffColor1 <- brewer.pal(6, "Blues")

difference <- abs(difference)

names <- c("nr_pix","rows_with_2","cols_with_2","rows_with_3p","cols_with_3p","height","width","left2tile","right2tile","verticalness","top2tile","bottom2tile","horizontalness","curvedness")
resultsAvg <- data.frame("Feature" = names,"Letters" = LettersAvg,"Digits" = NumbersAvg,"All" = TotalAvg,"Percent difference of letters and digits" = difference)

resultsAvg <- resultsAvg %>%
  gt() %>%
  tab_header(
    title = md("Average of each feature by set")
    
  )
#https://gt.rstudio.com/reference/data_color.html
data_color(
  resultsAvg,
  "Percent.difference.of.letters.and.digits",
  colors = scales::col_numeric(
    palette = diffColor1,
    domain = c(0:100) 
  )
  
)

#table for standard deviation

diffColor2 <- brewer.pal(9, "Reds")

Letters_SD <- c(sd_Letters_px,sd_Letters_rows2,sd_Letters_cols2,sd_Letters_rows3,sd_Letters_cols3,sd_Letters_height,sd_Letters_width,sd_Letters_left2,sd_Letters_right2,sd_Letters_verticalness,
                sd_Letters_top2,sd_Letters_bottom2,sd_Letters_horizontalness,sd_Letters_curvedness)
Numbers_SD <- c(sd_Numbers_px,sd_Numbers_rows2,sd_Numbers_cols2,sd_Numbers_rows3,sd_Numbers_cols3,sd_Numbers_height,sd_Numbers_width,sd_Numbers_left2,sd_Numbers_right2,sd_Numbers_verticalness,
                sd_Numbers_top2,sd_Numbers_bottom2,sd_Numbers_horizontalness,sd_Numbers_curvedness)
Total_SD <- c(sd_Total_px,sd_Total_rows2,sd_Total_cols2,sd_Total_rows3,sd_Total_cols3,sd_Total_height,sd_Total_width,sd_Total_left2,sd_Total_right2,sd_Total_verticalness,
              sd_Total_top2,sd_Total_bottom2,sd_Total_horizontalness,sd_Total_curvedness)



difference2 <- (( Numbers_SD/Letters_SD ) * 100)

difference2
difference2 <- difference2 - 100
difference2


difference2 <- abs(difference2)
results_SD <- data.frame("Class" = names,"Letters" = Letters_SD,"Digits" = Numbers_SD,"All" = Total_SD,"Percent difference of letters and digits" = difference2)
results_SD
results_SD <- results_SD %>%
  gt() %>%
  tab_header(
    title = md("Standard deviation of each feature by set")
    
  )

data_color(
  results_SD,
  "Percent.difference.of.letters.and.digits",
  colors = scales::col_numeric(
    palette = diffColor2,
    domain = c(0:100) 
  )
  
)

#table for variation

diffColor3 <- brewer.pal(9, "Greens")
Letters_var <- c(var_Letters_px,var_Letters_rows2,var_Letters_cols2,var_Letters_rows3,var_Letters_cols3,var_Letters_height,var_Letters_width,var_Letters_left2,var_Letters_right2,var_Letters_verticalness,
                 var_Letters_top2,var_Letters_bottom2,var_Letters_horizontalness,var_Letters_curvedness)
Numbers_var <- c(var_Numbers_px,var_Numbers_rows2,var_Numbers_cols2,var_Numbers_rows3,var_Numbers_cols3,var_Numbers_height,var_Numbers_width,var_Numbers_left2,var_Numbers_right2,var_Numbers_verticalness,
                 var_Numbers_top2,var_Numbers_bottom2,var_Numbers_horizontalness,var_Numbers_curvedness)
Total_var <- c(var_Total_px,var_Total_rows2,var_Total_cols2,var_Total_rows3,var_Total_cols3,var_Total_height,var_Total_width,var_Total_left2,var_Total_right2,var_Total_verticalness,
               var_Total_top2,var_Total_bottom2,var_Total_horizontalness,var_Total_curvedness)



difference3 <- (( Numbers_var/Letters_var ) * 100)


difference3 <- difference3 - 100



difference3 <- abs(difference3)
results_var <- data.frame("Class" = names,"Letters" = Letters_var,"Digits" = Numbers_var,"All" = Total_var,"Percent difference of letters and digits" = difference3)

results_var <- results_var %>%
  gt() %>%
  tab_header(
    title = md("Variance of each feature by set")
    
  )

data_color(
  results_var,
  "Percent.difference.of.letters.and.digits",
  colors = scales::col_numeric(
    palette = diffColor3,
    domain = c(0:250) 
  )
  
)



#section 3.4



#scatter graph of height and verticalness


features_df <- as.data.frame("section2_features/40225145_features.csv")


features_df <- read.csv("section2_features/40225145_features.csv")
features_df$Colour="#f0a12b" #mango orange for maths symbols
letters <- c('a','b','c','d','e','f','g')
nums <- c('one','two','three','four','five','six','seven')

# Set new column values to appropriate colours
features_df$Colour[features_df$label %in% letters == TRUE]="#3f90bf" #blue for letters
features_df$Colour[features_df$label %in% nums == TRUE]="#3ab55b" # green for digits

# Plot all points at once, using newly generated colours

height_vert_scatterplot <- ggplot(features_df, aes(x=height, y=verticalness))  + geom_point(color=features_df$Colour) + geom_smooth(method=lm, se=FALSE,color="#d14b4b") + labs(title="Height and verticalness of every symbol",x = "Height in pixels") 

#dependant variables


#calculating the correclation of using every value
height_vert_scatterplot
correlation <- cor.test(features_df$height,features_df$verticalness)
correlation


#hypothesis using a sample

#creating empty vectors to store samples
sampled_Heights <- numeric()
sampled_verts <- numeric()

#sampling ~10%
for(i in 1:16)
{


#selecting random unique rows
 rnum <-  sample(2:168,1)

 #add new values to vectors
 sampled_Heights <- append(sampled_Heights,features_df[rnum,8],after = length(sampled_Heights))
 sampled_verts <- append(sampled_verts,features_df[rnum,12],after = length(sampled_verts))
}

sampled_Heights
sampled_verts

#getting p value and correlation
correlation <- cor.test(sampled_Heights,sampled_verts)
correlation





#section 3.5



 features_df <- read.csv("section2_features/40225145_features.csv")
 features_df$Colour="#ffffff00" #set maths symbols as invisible
 letters <- c('a','b','c','d','e','f','g')
 nums <- c('one','two','three','four','five','six','seven')
 
 # Set new column values to appropriate colours
 features_df$Colour[features_df$label %in% letters == TRUE]="#3f90bf" #blue for letters
 features_df$Colour[features_df$label %in% nums == TRUE]="#3ab55b" # green for digits

# Plot scatter graphs
 
curvedness_scatterplot <- ggplot(features_df, aes(x=index, y=curvedness))  + geom_point(color=features_df$Colour,size = 3) + geom_smooth(method=NULL, se=FALSE,color="#d14b4b") + labs(title="Curvedness of Letters and digits",x = "symbol index") 
curvedness_scatterplot

curvedness_scatterplot2 <- ggplot(features_df, aes(x=index, y=curvedness))  + geom_point(color=features_df$Colour,size = 28,shape="square") + geom_smooth(method=NULL, se=FALSE,color="#d14b4b") + labs(title="Curvedness of Letters and digits",x = "symbol index") 
curvedness_scatterplot2

row3p_scatterplot <- ggplot(features_df, aes(x=index, y=rows_with_3p))  + geom_point(color=features_df$Colour,size = 3) + geom_smooth(method=NULL, se=FALSE,color="#d14b4b") + labs(title="Number of rows with 3 or more pixels for Letters and digits",x = "symbol index") 
row3p_scatterplot

row3p_scatterplot2 <- ggplot(features_df, aes(x=index, y=rows_with_3p))  + geom_point(color=features_df$Colour,size = 28,shape="square") + geom_smooth(method=NULL, se=FALSE,color="#d14b4b") + labs(title="Number of rows with 3 or more pixels for Letters and digits",x = "symbol index") 
row3p_scatterplot2

col2_scatterplot <- ggplot(features_df, aes(x=index, y=cols_with_2 ))  + geom_point(color=features_df$Colour,size = 3) + geom_smooth(method=NULL, se=FALSE,color="#d14b4b") + labs(title="Number of columns with 2 or less pixels for Letters and digits",x = "symbol index") 
col2_scatterplot

col2_scatterplot2 <- ggplot(features_df, aes(x=index, y=cols_with_2 ))  + geom_point(color=features_df$Colour,size = 28,shape="square") + geom_smooth(method=NULL, se=FALSE,color="#d14b4b") + labs(title="Number of columns with 2 or less pixels for Letters and digits",x = "symbol index") 
col2_scatterplot2
 
 

#hypothesis test for cols2
 
#Data for boxplot

#sample from letters
cols2_sample_L <- cols2_Letters[sample(nrow(cols2_Letters), 10), ]

#sample from numbers
cols2_sample_N <- cols2_Numbers[sample(nrow(cols2_Numbers), 10), ]

#put samples in one vector
cols2_samp <- c(cols2_sample_L,cols2_sample_N)

#set column names
names(cols2_samp) <- c("Letters","Digits")


 # Boxplot 
 boxplot(cols2_samp, main="Data on columns with <= 2 pixels from sample",
          ylab=" amount of columns ")
 
 #get data for table
 
 L_Values <- c(mean(unlist(cols2_sample_L)),sd(unlist(cols2_sample_L)),length(unlist(cols2_sample_L)))
 L_Values
 
 N_Values <- c(mean(unlist(cols2_sample_N)),sd(unlist(cols2_sample_N)),length(unlist(cols2_sample_N)))
 
 
 measure <- c("xbar","SD","n")
 
 samples_df <- data.frame(measure,L_Values,N_Values)
 names(samples_df) <- c("Measure","Letters","Numbers")
 
#create table
 gt(samples_df)

 #run t test 
 t.test(cols2_sample_L,cols2_sample_N)
 
 
 
 #hypothesis test for rows3p
 
 #Data for boxplot
 
 #sample from letters
 rows3_sample_L <- rows3_Letters[sample(nrow(rows3_Letters), 10), ]
 #sample from numbers
 rows3_sample_N <- rows3_Numbers[sample(nrow(rows3_Numbers), 10), ]
 
 rows3_samp <- c(rows3_sample_L,rows3_sample_N)
 names(rows3_samp) <- c("Letters","Digits")
 
 
 # Boxplot 
 boxplot(rows3_samp, main="Data on rows >=3 pixels from sample",
         ylab=" amount of rows ")

 #get data for table
 L_Values2 <- c(mean(unlist(rows3_sample_L)),sd(unlist(rows3_sample_L)),length(unlist(rows3_sample_L)))
 
 N_Values2 <- c(mean(unlist(rows3_sample_N)),sd(unlist(rows3_sample_N)),length(unlist(rows3_sample_N)))
 
 measure <- c("xbar","SD","n")
 
 samples2_df <- data.frame(measure,L_Values2,N_Values2)
 
 names(samples2_df) <- c("Measure","Letters","Numbers")

 #create table
 gt(samples2_df)
 
 #run t test
 t.test(rows3_sample_L,rows3_sample_N)
 
 
 #hypothesis test for rows3p
 
 #data for boxplot
 #sample from letters
 curvedness_sample_L <- curvedness_Letters[sample(nrow(curvedness_Letters), 12), ]
 #sample from numbers
 curvedness_sample_N <- curvedness_Numbers[sample(nrow(curvedness_Numbers), 12), ]
 
 curvedness_samp <- c(curvedness_sample_L,curvedness_sample_N)
 
 names(curvedness_samp) <- c("Letters","Digits")
 
 # Boxplot 
 boxplot(curvedness_samp, main="Data on curvedness from sample",
         ylab=" curvedness value ")
 
 #data for table
 
 L_Values3 <- c(mean(unlist(curvedness_sample_L)),sd(unlist(curvedness_sample_L)),length(unlist(curvedness_sample_L)))
 
 N_Values3 <- c(mean(unlist(curvedness_sample_N)),sd(unlist(curvedness_sample_N)),length(unlist(curvedness_sample_N)))
 
 measure <- c("xbar","SD","n")

  samples3_df <- data.frame(measure,L_Values3,N_Values3)

   names(samples3_df) <- c("Measure","Letters","Numbers")
 
#create table
 gt(samples3_df)

 #run t test 
 t.test(curvedness_sample_L,curvedness_sample_N)
 
 
 # randomization test as we couldnt reject the null hypothesis
 
 
 X <- c(curvedness_Letters)
 Y <- c(curvedness_Numbers)
 
 # Observed difference in means:
 obsdiff = mean(unlist(X))-mean(unlist(Y))
 obsdiff
 
 #number of simulations
 nsims = 100000

 
 A <- c(X,Y)
 A<- unlist(A)
 class(A)# put all values in a single vector

 nulldiffs <- rep(0,nsims) # initialise with 0s
 
 for (ni in 1:nsims) {
   randomA = sample(A) # randomly reorder.
   randomX = randomA[1:length(unlist((X)))]
   randomY = randomA[(length(unlist(X))+1):length(A)]
   nulldiffs[ni] <- mean(unlist(randomX)) - mean(unlist(randomY))
   
 } 
#calculate pvalue
 
 
 diffSum <- 0
  for (i in nulldiffs)
  {
    if(abs(i) > abs(obsdiff))
    {
      diffSum = diffSum + i
      diffSum
    }
  }
 pvalue = diffSum/nsims
 pvalue # significant at 0.05 level
 
 
 print("Program Ended")
