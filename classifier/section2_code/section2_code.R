setwd("C:/assignment2_40225145")

#create csv file
cat("label,index,nr_pix,rows_with_2 ,cols_with_2 ,rows_with_3p,cols_with_3p,height,width,left2tile,right2tile,verticalness,top2tile,bottom2tile,horizontalness,curvedness",file= "C:/assignment2_40225145/section2_features/40225145_featurestest.csv")

#vector to store symbols
symbols <- c("a","b","c","d","e","f","g","one","two","three","four","five","six","seven","less","greater","equal","lessequal","greaterequal","notequal","approxequal")

#vector to store indexes
indexes <- c(1,2,3,4,5,6,7,8)

#variables for file path
firstHalf <- "section1_images/40225145_"
secondHalf <- "_"
extension <- ".csv"

for(items1 in symbols)
{
  for(items2 in indexes)
  {

  myData <- (read.csv(paste(firstHalf , items1 , secondHalf ,0, items2 , extension , sep = ""),header = FALSE))

 


#the label for the symbol e.g "a"
label <- items1

#the index for the symbol e.g 1
index <- items2

#the commented numbers denote feature index i.e the 1 below is for nr_pix

#1

#The number of black pixels in the image
nr_pix <- length(which(myData == 1))

#nr_pix
nr_pix

#insert into data frame
df <- data.frame(myData)

#2

#set variables
rows_with_2 <- 0
count <- 1
numBlackpx <- 0

#search through matrix
for(ir in 1:25)
{
  for(ic in 1:25)
  {
    #if pixel is black
  if(df[ir,ic] == 1)
  {
    #increase pixel count
    numBlackpx = numBlackpx + 1
    
  }
  #if at end of row
  if (count %% 25 == 0)
  {
    #if row contained exactly two black pixels
    if(numBlackpx == 2)
    {
      #increase row count
      rows_with_2 = rows_with_2 + 1
      
      
    }
    #reset black pixel count
    numBlackpx = 0
    
  }
  #count amount of pixels
  count = count + 1
  }
}
#rows with exactly 2px
rows_with_2

#3

#I decided the best method was to treat the columns like rows so i could use the same method styles as above using the t() method
#transpose matrix in order to calculate values per column
dfTrans <- t(df)



#reset variables
count <- 1
cols_with_2 <- 0
numBlackpx <- 0

#code works like #2
for(ir in 1:25)
{
  for(ic in 1:25)
  {
    if(dfTrans[ir,ic] == 1)
    {
      numBlackpx = numBlackpx + 1
      
    }
    if (count %% 25 == 0)
    {
      
      if(numBlackpx == 2)
      {
        #increase column count
        cols_with_2 = cols_with_2 + 1
        
        
      }
      numBlackpx = 0
      
    }
    count = count + 1
  }
}
#columns with exactly 2px
cols_with_2



#4

#reset variables
numBlackpx <- 0
count <- 1
rows_with_3p <- 0

#code works the same 
for(ir in 1:25)
{
  for(ic in 1:25)
  {
    if(df[ir,ic] == 1)
    {
      numBlackpx = numBlackpx + 1
      
    }
    if (count %% 25 == 0)
    {
      #if row contained more than 2 black pixels
      if(numBlackpx > 2)
      {
        #increase row count
        rows_with_3p = rows_with_3p + 1
        
        
      }
      numBlackpx = 0
      
    }
    count = count + 1
  }
}
#rows with 3 or more px
rows_with_3p


#5

#reset variables
cols_with_3p <- 0
numBlackpx <- 0

#code works the same 
for(ir in 1:25)
{
  for(ic in 1:25)
  {
    #use transposed matrix
    if(dfTrans[ir,ic] == 1)
    {
      numBlackpx = numBlackpx + 1
      
    }
    if (count %% 25 == 0)
    {
      #if row had more than 2 pixels
      if(numBlackpx > 2)
      {
        #increase column count
        cols_with_3p = cols_with_3p + 1
        
        
      }
      numBlackpx = 0
      
    }
    count = count + 1
  }
}
#columns with 3 or more pixels
cols_with_3p

#6

#reset variables
count <- 1
rowNum <- 1
height <- 0
topRow <- 0
bottomRow <- 0
firstPx <- FALSE
height <- 0

for(ir in 1:25)
{
  for(ic in 1:25)
  {
    #if black pixel is the first black pixel in the matrix
    if(df[ir,ic] == 1 && firstPx == FALSE )
    {
      #change to true to stop the top row value from being change again
      firstPx = TRUE
      #store row number of current row as top row
      topRow = rowNum
    }
    
    if(df[ir,ic] == 1 && firstPx == TRUE )
    {
      #last pixel counted's row number will be stored as bottom row
      bottomRow = rowNum
    }
    if (count %% 25 == 0)
    {
      rowNum = rowNum + 1
      
      
    }
    count = count + 1
  }
}
#+1 is because we start from the middle of each pixel
height = ((bottomRow - topRow) + 1 )
#height 
height

#7

#works the same as #6 but using the transpose matrix

#reset variables
count <- 1
rowNum <- 1
topRow <- 0
bottomRow <- 0
firstPx <- FALSE
width <- 0


for(ir in 1:25)
{
  for(ic in 1:25)
  {
    #use transposed matrix
    if(dfTrans[ir,ic] == 1 && firstPx == FALSE )
    {
      firstPx = TRUE
      topRow = rowNum
    }
    if(dfTrans[ir,ic] == 1 && firstPx == TRUE )
    {
      bottomRow = rowNum
    }
    if (count %% 25 == 0)
    {
      rowNum = rowNum + 1
      
      
    }
    count = count + 1
  }
}

#+1 is because we start from the middle of each pixel
width = ((bottomRow - topRow)+ 1 )

#width 
width


#8

#reset variables
rowNum <- 1
left2tile <- 0

for(ir in 1:25)
{
  for(ic in 1:25)
  {
    #if pixel is black and not on the final row and not in the final column
    if((is.element(1,df[ir,ic])) && rowNum != 25 && ic != 25)
    {
      
      #if the pixel next to it is  white
      if(df[ir,(ic + 1)] == 0)
      {
        
        #if the one below it is  black
        if(df[(ir + 1),ic] == 1) 
        {
          
          #if the one next to the one below it is white
          if((df[(ir + 1),(ic + 1)] == 0 ))
          {
            
            left2tile = left2tile + 1
            
          }
          
        }
      }
      
    }
    if (count %% 25 == 0)
    {
      rowNum = rowNum + 1
      
      
      
    }
    count = count + 1
  }
}
#amount of left 2-tiles
left2tile

#9
#code similar to #8 but checks are different
#reset variables
rowNum <- 1
right2tile <- 0

for(ir in 1:25)
{
  for(ic in 1:25)
  {
    #if pixel is white and not on the final row and not in the final column
    if((df[ir,ic] == 0) && rowNum != 25 && ic != 25)
    {
      
      #if the pixel next to it is  black
      if(df[ir,(ic + 1)] == 1)
      {
        
        #if the one below it is  white
        if(df[(ir + 1),ic] == 0) 
        {
          
          #if the one next to the one below it is black
          if((df[(ir + 1),(ic + 1)] == 1 ))
          {
            
            right2tile = right2tile + 1
            
          }
          
        }
      }
      
    }
    if (count %% 25 == 0)
    {
      rowNum = rowNum + 1
      
      
      
    }
    count = count + 1
  }
}
#amount of right 2-tiles
right2tile

#10

verticalness <- ((left2tile + right2tile)/(nr_pix))
#verticalness


verticalness 



#11 

#reset variables
rowNum <- 0
top2tile <- 0
for(ir in 1:25)
{
  for(ic in 1:25)
  {
    #if pixel is black and not on the final row and not in the final column
    if((is.element(1,df[ir,ic] )) && rowNum != 25 && ic != 25)
    {
      
      #if the pixel next to it is also black
      if(df[ir,(ic + 1)] == 1)
      {
      
        #if the one below it is  white
        if(df[(ir + 1),ic] == 0) 
        {
          
          #if the one next to the one below it is white
          if((df[(ir + 1),(ic + 1)] == 0 ))
          {
            
            top2tile = top2tile + 1
            
          }
          
        }
      }
      
    }
    if (count %% 25 == 0)
    {
      rowNum = rowNum + 1
      
     
      
    }
    count = count + 1
  }
}
#amount of top 2-tiles
top2tile

#12

#reset variables
rowNum <- 1
bottom2tile <- 0

for(ir in 1:25)
{
  for(ic in 1:25)
  {
    #if pixel is white and not on the final row and not in the final column
    if((df[ir,ic] == 0) && rowNum != 25 && ic != 25)
    {
     
      #if the pixel next to it is also white
      if(df[ir,(ic + 1)] == 0)
      {
       
        #if the one below it is  black, using == 1 was causing it to crash
        if(is.element(1, df[(ir + 1),ic]))
        {
         
          #if the one next to the one below it is black
          if((df[(ir + 1),(ic + 1)] == 1 ))
          {
           
            bottom2tile = bottom2tile + 1
            
          }
          
        }
      }
      
    }
    if (count %% 25 == 0)
    {
      rowNum = rowNum + 1
      
      
      
    }
    count = count + 1
  }
}
#amount of bottom 2-tiles
bottom2tile


#13



horizontalness <- ((top2tile + bottom2tile)/(nr_pix))
#horizantalness
horizontalness

#14

#i chose to calculate curvedness as my original feature

#reset variables
rowNum <- 1
diagnol3tile <- 0

for(ir in 1:25)
{
  for(ic in 1:25)
  {
    #if pixel is black and not on the second final or final row and not in the second final or final column
    if((is.element(1,df[ir,ic] )) && rowNum < 23 && ic < 23)
    {
      
      
        
        
          
          #if two rows below and  2 to the right is black
          if( df[(ir + 2),(ic + 2)] == 1 ) 
          {
            
            diagnol3tile = diagnol3tile + 1
            
          }
          
        
      
      
    }
    #for diagonal lines in the other direction
    
    #if pixel is not on the second final or final row and not in the second final or final column
    else if( rowNum < 23 && ic < 23)
    {
      
      #if the 2 pixels right is black
      if( df[ir,(ic + 2)] == 1 )
      {
        
        
          
          #if two rows below it is black
          if(df[(ir + 2),ic] == 1) 
          {
            
            diagnol3tile = diagnol3tile + 1
            
          }
          
        
      }
      
    }
    if (count %% 25 == 0)
    {
      rowNum = rowNum + 1
      
      
      
    }
    count = count + 1
  }
}

#diagonal 3tiles
diagnol3tile

#caluclate curvedness
curvedness <- ((diagnol3tile)/(nr_pix))

#curvedness
curvedness

#store features in a list

features <- list(label,index,as.numeric(nr_pix),rows_with_2,cols_with_2,rows_with_3p,cols_with_3p,height,width,left2tile,right2tile,verticalness,top2tile,bottom2tile,horizontalness,curvedness)


#write features to a table and append, so it gets added to the bottom  of the table without overwriting the others

write.table(features,"section2_features/40225145_featurestest.csv",row.names = FALSE,  sep = "," ,col.names=FALSE, append = TRUE,quote = FALSE)


  }
}


print("Program complete")






