
setwd("C:/assignment2_40225145")
set.seed(42)

#section 4.1

features_df <- read.csv("section2_features/40225145_features.csv")

#pvalue approach
fit <- lm(horizontalness ~ nr_pix + rows_with_2 + cols_with_2 + rows_with_3p + cols_with_3p + height + width + left2tile + right2tile + verticalness + top2tile + bottom2tile, data=features_df)
summary(fit)

#remove rows_with_2 pvalue = 0.17064   

fit <- lm(horizontalness ~ nr_pix +  cols_with_2 + rows_with_3p + cols_with_3p + height + width + left2tile + right2tile + verticalness + top2tile + bottom2tile, data=features_df)
summary(fit)

#remove width pvalue =  0.20762   
fit <- lm(horizontalness ~ nr_pix +  cols_with_2 + rows_with_3p + cols_with_3p + height +  left2tile + right2tile + verticalness + top2tile + bottom2tile, data=features_df)
summary(fit)


#store summary in variable
fit_df <- summary.lm(fit)
fit_df

#user enters the label and index 
lbl <- readline( "Please Enter Label") 
ind <-readline( "Please Enter index")

#get values for the image that the entered
nr_pix_Value <- features_df[features_df$label == lbl & features_df$index ==ind,3]

cols_with_2_Value  <- features_df[features_df$label == lbl & features_df$index ==ind,5]

rows_with_3p_Value <- features_df[features_df$label == lbl & features_df$index ==ind,6]

cols_with_3p_Value <- features_df[features_df$label == lbl & features_df$index ==ind,7]

height_Value <- features_df[features_df$label == lbl & features_df$index ==ind,8]

left2tile_Value <- features_df[features_df$label == lbl & features_df$index ==ind,10]

right2tile_Value <- features_df[features_df$label == lbl & features_df$index ==ind,11]

verticalness_Value <- features_df[features_df$label == lbl & features_df$index ==ind,12]

top2tile_Value <- features_df[features_df$label == lbl & features_df$index ==ind,13]

bottom2tile_Value <- features_df[features_df$label == lbl & features_df$index ==ind,14]

#horizontal = 0.8
fit

#y=b0 + b1x + b2x ...+bnx
horizontal <- fit_df$coefficients[1, 1] + (fit_df$coefficients[2, 1] * nr_pix_Value ) +(fit_df$coefficients[3, 1]*cols_with_2_Value)+(fit_df$coefficients[4, 1]*rows_with_3p_Value)+(fit_df$coefficients[5, 1]*cols_with_3p_Value)+(fit_df$coefficients[6, 1]*height_Value)+(fit_df$coefficients[7, 1]*left2tile_Value)+(fit_df$coefficients[8, 1]*right2tile_Value)+(fit_df$coefficients[9, 1]*verticalness_Value)+(fit_df$coefficients[10, 1]*top2tile_Value)+(fit_df$coefficients[11, 1]*bottom2tile_Value)

#the horizontal value taken from the features csv file
actualHorizontal <- features_df[features_df$label == lbl & features_df$index ==ind,15]

#function to output result
result <- function(horizontal){
  

#stops negative values from being outputted, as they are actually 0
  if (horizontal < 0)
  {
    horizontal <-0
}
else
{
  paste("Predicted horizontal: ",horizontal)
}
  
}

#exception handle if the user enters an invalid image
tryCatch(result(horizontal), error=function(result) {
  message(paste("invalid image"))
  message(paste("List Of valid labels: "),c("a ","b ","c ","d ","e ","f ","g ","one ","two ","three ","four ","five ","six ","seven ","less ","greater ","equal ","lessequal ","greaterequal ","notequal ","approxequal "))
  message(paste("List Of valid indexes: "), c("1 ","2 ","3 ","4 ","5 ","6 ","7 ","8 "))
   # Choose a return value in case of error
  return(NA)
})
paste("Actual horizontal: ",actualHorizontal)



#section 4.2



#library for fread function
#https://www.rdocumentation.org/packages/data.table
library(data.table)
#create set of letters and digits to set factors for
letters <- c('a','b','c','d','e','f','g')
nums <- c('one','two','three','four','five','six','seven')
#get all letters and digits
features_L_D <- fread("section2_features/40225145_features.csv",nrows= 112)
features_L_D

#create factor column
#instansiate dummy.letter value
features_L_D$dummy.letter <-0
features_L_D$dummy.letter


# Set new column values to appropriate values
features_L_D$dummy.letter[features_L_D$label %in% letters == TRUE] <- 1
features_L_D$dummy.letter
#create random feature column for the stupid model
#random feature
features_L_D$Rand.Feature = sample(0:100,nrow(features_L_D),rep=TRUE)

# training and test datasets


# randomly shuffle rows:
features_shuffled <- features_L_D[sample(nrow(features_L_D)),]


# first 80% will be training data:
training_data = features_shuffled[1:90,]
test_data = features_shuffled[91:113,]


#library to get graphs
#https://www.rdocumentation.org/packages/ggplot2
library(ggplot2)

# plot a histogram:
plt <- ggplot(training_data, aes(x=rows_with_3p, fill=as.factor(dummy.letter))) +
  geom_histogram(binwidth=.2, alpha=.5, position='identity')
plt 


#create linear model
glmfit<-glm(dummy.letter ~ rows_with_3p + cols_with_2 + curvedness, 
            data = training_data, 
            family = 'binomial') 
summary(glmfit)

#select 3 features for classifier
newdata = fread("section2_features/40225145_features.csv",select=c(6,5,16),nrows= 113) 

#calculate probaility values
predicted = predict(glmfit, newdata, type="response")

predicted

#using test data,to make predictions
test_data[["predicted_val"]] = predict(glmfit, test_data, type="response")
test_data[["predicted_class"]] = 0
test_data[["predicted_class"]][test_data[["predicted_val"]] > 0.5] = 1

correct_items = test_data[["predicted_class"]] == test_data[["dummy.letter"]] 

test_data[["predicted_class"]]
# proportion correct:
nrow(test_data[correct_items,])/nrow(test_data)

# proportion incorrect:
nrow(test_data[!correct_items,])/nrow(test_data)


#reapeat same steps for stupid model


glmfit<-glm(dummy.letter ~ Rand.Feature, 
            data = training_data, 
            family = 'binomial') 
summary(glmfit)


training_data[["predicted_val"]] = predict(glmfit, training_data, type="response")
training_data[["predicted_class"]] = 0
training_data[["predicted_class"]][training_data[["predicted_val"]] > 0.5] = 1

correct_items = training_data[["predicted_class"]] == training_data[["dummy.letter"]] 

# proportion correct:
nrow(training_data[correct_items,])/nrow(training_data)

# proportion incorrect:
nrow(training_data[!correct_items,])/nrow(training_data)



test_data[["predicted_val"]] = predict(glmfit, test_data, type="response")
test_data[["predicted_class"]] = 0
test_data[["predicted_class"]][test_data[["predicted_val"]] > 0.5] = 1

correct_items = test_data[["predicted_class"]] == test_data[["dummy.letter"]] 

# proportion correct:
nrow(test_data[correct_items,])/nrow(test_data)

# proportion incorrect:
nrow(test_data[!correct_items,])/nrow(test_data)


# The caret package for R can automate the crossvalidation procedure 
#https://www.rdocumentation.org/packages/caret 
library(caret)

# define training control
 train_control <- trainControl(method="cv", number=5)

 
#make sure dummy.letter is a factor and not numeric
features_L_D$dummy.letter <- as.factor(features_L_D$dummy.letter)

# train the model
model <- train(dummy.letter ~ rows_with_3p + cols_with_2 + curvedness, data=features_L_D, trControl=train_control, method="glm",family="binomial" )
#  summarize results
print(model)




# train the model
model2 <- train(dummy.letter ~ Rand.Feature, data=features_L_D, trControl=train_control, method="glm",family="binomial" )
#  summarize results
print(model2)


#section 4.3

#87% accuracy
x <- 0.87 * 112
x #~97
#my model will on average guess 97 correctly
#my model had 87% accuracy
#random model accuracy was 43%

binomDis <- pbinom(97,112,0.43)
#probability of getting less than 97
binomDis
#probability of getting 97 or greater
#pvalue
probOfcorrect <- (1 - binomDis)

#the probability of the random model
#getting 77 correct is..
probOfcorrect
print("Program Ended")
 
