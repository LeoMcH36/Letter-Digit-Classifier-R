setwd("C:/assignment2_40225145")

#vector to store symbols
symbols <- c("a","b","c","d","e","f","g","one","two","three","four","five","six","seven","less","greater","equal","lessequal","greaterequal","notequal","approxequal")

#vector to store indexes
indexes <- c(1,2,3,4,5,6,7,8)

#variables for file path
firstHalf <- "section1_images/40225145_"
secondHalf <- "_"
extension1 <- ".PGM"
extension2 <- ".csv"

#load libraries outside of loop
#load reader library for n.readLines function
#https://www.rdocumentation.org/packages/reader
library(reader)

#load mgsub library for mgsub function
#https://www.rdocumentation.org/packages/mgsub
library(mgsub)

for(items1 in symbols)
{
  for(items2 in indexes)
  {
    
    
    
    #Import PGM image as a matrix and assign to variable skipping first 3 lines
    myData <- as.matrix(c(n.readLines(paste(firstHalf , items1 , secondHalf ,0, items2 , extension1 , sep = ""),n = 628,  skip = 3)))
    
    
    
    #change all 255's to 0 and all 0's to 1 to represent white and black spaces
    Img <- mgsub(myData,c("255","0"),c("0","1"))
    
    #convert these to numbers
    ImgNum <- as.numeric(Img)
    
    #make matrix 25x25
    mat <- matrix(ImgNum,ncol=25)
    
    #however matrix is flipped and has been turned 90 degrees clockwise
    
    #fixing this
    
    #flip matrix by reversing it
    #https://stackoverflow.com/questions/19820712/flip-the-matrix
    matrixrev <-mat[,c(25:1),drop = FALSE]
    
    #turn matrix 90 anti-clockwise
    #https://stackoverflow.com/questions/16496210/rotate-a-matrix-in-r-by-90-degrees-clockwise
    matrixrev <- apply(t(matrixrev),2,rev)
    
    #place matrix into a data frame
    df <- data.frame(matrixrev)
    
    #create csv file
    cat(NULL,file= paste(firstHalf , items1 , secondHalf ,0, items2 , extension2 , sep = ""))
    
    #write the csv file as a matrix without row or column names
    write.table(df,paste(firstHalf , items1 , secondHalf ,0, items2 , extension2 , sep = ""),row.names = FALSE, col.names = FALSE, sep = ",")
    
    
  }
}

print("Program complete")
