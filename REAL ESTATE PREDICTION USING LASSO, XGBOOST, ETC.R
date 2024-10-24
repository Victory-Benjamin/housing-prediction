install.packages("corrplot")
install.packages("ggrepel")
install.packages("viridis")
install.packages("gridExtra")
install.packages("purrr")
install.packages("glue")
install.packages("randomForest")
install.packages("patchwork")



library("ggrepel")
library(corrplot)             
library(tidyverse)
library(scales)
library(wesanderson)
library(psych)
library(RColorBrewer)
library(knitr)
library(here)
library(gridExtra)
library(purrr)
library(glue)
library(randomForest)
library(patchwork)


train <- read_csv("train..csv")
test <- read_csv("test..csv")

view(train)

str(head(train))

# Save the 'Id' column from the test set to a separate vector
test_label <- train$Id

# Remove the 'Id' column from both the test and train datasets
train$Id <- NULL
test$Id <- NULL

all <- bind_rows(train, test)
dim(all)

ggplot(data= all[!is.na(all$SalePrice), ], aes(x = SalePrice))+
  geom_histogram(fill= "blue", binwidth = 10000)+
  scale_x_continuous(breaks = seq(0,800000, by= 100000), labels = comma)+
  labs(title = "Distribution of Sale Prices",
       x = "Sale Price",
       y = "Count")

#summary(all$SalePrice)
#dim(na.omit(all))


numericVars <- all%>%
  select(where(is.numeric))
numericVars

cat("there are", length(numericVars), "numeric variables")

cor_numVar <- numericVars %>%
  cor(use = "pairwise.complete.obs", method = "pearson") #correlations of all numeric variables
cor_numVar


#sort on decreasing correlations with SalePrice
cor_sorted <- cor_numVar[ "SalePrice", ] %>%
  .[abs(.) > 0.5]  %>%
  sort( decreasing=TRUE)
cor_sorted  

# call names() on the sorted corr. matrix to get a vector of names that i can use to subset the corr. matrix
CorHigh <- names(cor_sorted)
CorHigh

#subset the correlation matrix
cor_numVar <- cor_numVar[CorHigh, CorHigh]
cor_numVar

# plot the correlation graph
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

ggplot(data = all[!is.na(all$SalePrice), ],
       aes(x= factor(OverallQual), y= SalePrice))+
  geom_boxplot(colour= "blue")+
  scale_y_continuous(breaks= seq(0, 800000, 100000), labels = comma)




#creating a plot using the viridis package  
library(viridis)
  
ggplot(data = all[!is.na(all$SalePrice), ], 
         aes(x = GrLivArea, y = SalePrice, colour = factor(OverallQual))) +
    
    geom_point() +
    
    geom_smooth(method = "lm", se = FALSE, colour = "black", aes(group = 1)) +
    
    scale_y_continuous(breaks = seq(0, 800000, 100000), labels = comma) +
    
    labs(title = "Relationship between GrLivArea and Price of a Building") +
    
    geom_text_repel(aes(label = ifelse(GrLivArea > 4500, rownames(all), ""))) +
    
    scale_colour_viridis_d()



#creating a plot using a random palette
custom_palette <- c("#D73027", "#FC8D59", "#FEE090", "#E0F3F8", "#91BFDB", 
                    "#4575B4", "#1F78B4", "#33A02C", "#FB9A99", "#E31A1C")

ggplot(data = all[!is.na(all$SalePrice), ], 
       aes(x = GrLivArea, y = SalePrice, colour = factor(OverallQual))) +
  
  geom_point() +
  
  geom_smooth(method = "lm", se = FALSE, colour = "black", aes(group = 1)) +
  
  scale_y_continuous(breaks = seq(0, 800000, 100000), labels = scales::comma) +
  
  labs(title = "Relationship between GrLivArea and Price of a Building") +
  
  geom_text_repel(aes(label = ifelse(GrLivArea > 4500, rownames(all), ""))) +
  
  scale_colour_manual(values = custom_palette)

colNA <- all %>%
  select(where( ~any(is.na(.)))) %>%
  summarize(across(everything(), ~sum(is.na(.))))
cat("there are", length(colNA), "columns with missing values")

colNA_long <- colNA %>%
  
  pivot_longer(names_to = "colums with NA",
             values_to = "number of NA's",
             cols = everything()) %>%
              arrange(desc( `number of NA's`))

print(colNA_long)


#HANDLING MISSING VALUES,GROUPING RELATED VARIABLES, CONVERTING CLEAR ORDINAL VALUES TO ORDINAL NUMERIC VALUES, AND CONVERTING NON-ORDINAL VALUES TO FACTORS
#pool variables
#all$PoolQC[is.na(all$PoolQC)] <- "none"
#               #OR for converting "NA" to "none"

table(all$PoolQC, useNA = "ifany")


qualities <- c("none"= 0, "Po" = 1,  "Fa"= 2, "TA"= 3, "Gd" = 4, "Ex" = 5)

all <- all%>%
  mutate(PoolQC = ifelse(is.na(PoolQC), "none",PoolQC),
         PoolQC = recode(PoolQC, !!!qualities),
         PoolQC = as.numeric(PoolQC))




pool_variable <-  all %>%
  filter(if_all(c(contains("Pool"),OverallQual), ~ !is.na(.)))%>%
  filter(PoolArea > 0) %>%
  select(contains("Pool"),OverallQual)
#arrange(desc(OverallQual))
pool_variable

table(all$PoolArea)
table(all$PoolQC)

ggplot(data = pool_variable, aes(y= PoolQC, x= PoolArea))+
   geom_point()


cor_pool <- pool_variable %>%
cor(use = "pairwise.complete.obs", method = "spearman")
cor_pool


mcar_poolqc <- pool_variable %>%
  filter(PoolArea >0, PoolQC == 0)
mcar_poolqc

#filling MCAR poolQC with the overall quality for that variable
all <- all %>%
  mutate(PoolQC = ifelse(PoolArea >0 & PoolQC == 0,round(OverallQual/2),PoolQC ))

#miscellenous variable
miscellenous_feature <- all %>%
  filter(!is.na(MiscFeature)) %>%
  select(MiscFeature)
miscellenous_feature

table(all$MiscFeature)
miscellenous_feature <- all %>%
  filter(!is.na(SalePrice)) %>%
  select(MiscFeature, SalePrice)
miscellenous_feature

all <- all %>%
  mutate(MiscFeature = ifelse(is.na(MiscFeature), "none", MiscFeature)) %>%
  mutate(MiscFeature = factor(MiscFeature))  

ggplot(data = miscellenous_feature, aes(y= SalePrice, x = MiscFeature))+
  stat_summary(fun = median, geom = "bar", fill = "green")+
  geom_label(stat = "count", aes(label = ..count.. , y= ..count..))+
  scale_y_continuous(breaks = seq(0,800000, by = 100000), labels = scales::comma)



#Alley variable
table(all$Alley)

all <- all %>%
  mutate(Alley = ifelse(is.na(Alley), "none", Alley))  %>%
  mutate(Alley= factor(Alley))

alley_graph <- all %>%
  filter(!is.na(SalePrice))

 
ggplot(data = alley_graph, aes(y = SalePrice, x = Alley))+
  stat_summary(fun = median, geom = "bar", fill = "green") +
  scale_y_continuous(breaks = seq(0, 200000, by = 50000), labels = scales::comma)+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))


#fence variable
all <- all %>%
  mutate(Fence = ifelse(is.na(Fence), "no fence", Fence)) %>%
  mutate(Fence = factor(Fence))
  

fence_graph <- all%>%
  filter(!is.na(SalePrice))

ggplot(data = fence_graph, aes(x= Fence, y = SalePrice))+
  stat_summary(fun = median, geom = "bar", fill = "green")+
  geom_label(stat = "count",  aes(label = ..count.., y=..count..))+
  scale_y_continuous(breaks = seq(0, 200000, by = 50000), label = scales::comma)


ggplot(data = fence_graph, aes(x = Fence, y = SalePrice, colour = OverallQual))+
  geom_boxplot()+
  scale_y_continuous(label = scales::comma)



# Plot with improvements
ggplot(data = fence_graph, aes(x = Fence, y = SalePrice, color = factor(OverallQual))) +
  geom_boxplot()+
  geom_jitter(alpha = 0.6, width = 0.15, size = 1.5)+  # Add jittered points for actual data
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = scales::comma) +  # More detailed y-axis
  labs(
    title = "Distribution of Sale Prices by Fence Type and Overall Quality",
    x = "Fence Type",  # Improved x-axis label
    y = "Sale Price (USD)",  # Improved y-axis label
    color = "Overall Quality"
  ) +
  theme_minimal() +  # Minimal theme for a cleaner look
  theme(
    legend.position = "top",  # Move the legend to the top for better visibility
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  # Add median labels on top of each boxplot
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 0)), 
               vjust = -1.5, size = 3, color = "black")
 
  

sum_fence <- all %>%
  group_by(Fence) %>%
  summarise(
    median_price = median(SalePrice), 
    count = n()
    )
sum_fence

#FIREPLACE VARIABLES
#check that NA's equall number of variable in fireplaces=0
variable_count <- all %>%
  filter(Fireplaces==0) %>%
  summarise(Fireplaces_0 = n(),
            missing_firequ = sum(is.na(FireplaceQu)))
variable_count #since the number of NA and the number of fireplaces that are zero is equal. i can safely input "no fire place" in the NA's



all <- all %>%
  mutate(FireplaceQu = ifelse(is.na(FireplaceQu), "none", FireplaceQu))%>%
  mutate(FireplaceQu = factor(FireplaceQu),
         Fireplaces = factor(Fireplaces)) %>%
  mutate(FireplaceQu = as.numeric(recode(FireplaceQu, !!!qualities)))
  

table(all$FireplaceQu)

#lot variables
lot_variable <- all %>%
  select(contains("Lot"))
view(lot_variable)

any(is.na(lot_variable))

sum(is.na(all$LotFrontage))


all <- all %>%
  mutate(Neighborhood = factor(Neighborhood)) %>%
  mutate(LotFrontage =  ifelse(is.na(LotFrontage),
                               median(LotFrontage, na.rm = TRUE), 
                               LotFrontage) )
         

lot_frontage_graph <- all %>%
  filter(!is.na(SalePrice)) %>%
  group_by(Neighborhood) %>%
  ungroup()
lot_frontage_graph

ggplot(data = lot_frontage_graph, aes(x = Neighborhood, y = LotFrontage))+
  stat_summary(fun = median, geom = "bar", fill = "navy")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 


all <- all %>%
  mutate(LotShape = as.numeric(recode(LotShape, 'IR3'= 0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))


ggplot(data = all[!is.na(all$SalePrice), ], aes(x = as.factor(LotConfig), y = SalePrice))+
  stat_summary(fun = median, geom = "bar", fill = "pink")+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))


#garage_variable
garage_variables <- all %>%
  select(contains("garage"))
garage_variables

table(garage_variables$GarageType)
table(garage_variables$GarageFinish)
table(garage_variables$GarageCars)
table(garage_variables$GarageQual)
table(garage_variables$GarageCond)

# Count NAs in each column of the garage_variables dataset
garage_variables %>%
  summarize_all( ~ sum(is.na(.)))

all <- all %>%
  mutate(GarageYrBlt = ifelse(is.na(GarageYrBlt), YearBuilt, GarageYrBlt))


#calculates how many rows in the dataset all have missing (NA) values simultaneously 
#for all four columns: GarageType, GarageFinish, GarageCond, and GarageQual.
length(which(is.na(garage_variables$GarageType) & is.na(garage_variables$GarageFinish)
             & is.na(garage_variables$GarageCond) & is.na(garage_variables$GarageQual) 
             & is.na(garage_variables$GarageYrBlt)))



#Find the 2 additional NAs
missin_garage <- all%>%
  mutate(row_id = row_number()) %>%
  filter(!is.na(GarageType) & is.na(GarageFinish)) %>%
  select(row_id, contains("Garage"))

missin_garage %>% kable() 


                #OR
# #use this code to find the two additional NA's
# kable(all[!is.na(all$GarageType) & is.na(all$GarageFinish), c('GarageCars', 
#                                                                 'GarageArea', 
#                                                                 'GarageType', 
#                                                                 'GarageCond', 
#                                                                 'GarageQual',
#                                                                 'GarageFinish')])
#   # Count NAs in each column of the garage_variables dataset

all %>%
  select(contains("Garage")) %>%
  summarise_all(~sum(is.na(.)))


get_mode <- function(x){
  freq_x <- table(x)
  mode_value <- names(freq_x)[freq_x == max(freq_x)]
  return(mode_value)
}


#Imputing modes.
all <- all %>%
  mutate(GarageFinish = ifelse(row_number() == 2127, get_mode(GarageFinish), GarageFinish),
         GarageQual = ifelse(row_number() == 2127, get_mode(GarageQual), GarageQual),
         GarageCond = ifelse(row_number() == 2127, get_mode(GarageCond), GarageCond))


#summary of NA's
all %>%
select(contains("Garage")) %>%
summarise_all(~sum(is.na(.)))


#display "fixed" house
all %>%
  slice(2127) %>%
  select(contains("Garage"))

all %>%
  slice(2577) %>%
  select(contains("Garage"))

all <- all %>%
  mutate(GarageType = ifelse(row_number() == 2577, NA, GarageType),
         GarageCars = ifelse(row_number() == 2577, 0, GarageCars),
         GarageArea = ifelse(row_number() == 2577, 0, GarageArea))

all %>%
  summarise_all(~sum(is.na(.))) %>%
  select(contains("Garage"))

length(which(is.na(all$GarageType) 
             & is.na(all$GarageFinish)
             & is.na(all$GarageQual)
             & is.na(all$GarageCond)))  

all <- all %>%
  mutate(GarageType = ifelse(is.na(GarageType), "no Garage", GarageType),
         GarageType = as.factor(GarageType)) 
  
table(all$GarageType)
levels(all$GarageType)


finish <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)

all <- all %>%
  mutate(GarageFinish = ifelse(is.na(GarageFinish), "None", GarageFinish)) %>%
  mutate(GarageFinish = as.numeric(recode(GarageFinish, !!!finish)))

table(all$GarageFinish)

table(all$GarageQual)
all <- all %>%
  mutate(GarageQual = ifelse(is.na(GarageQual), "none", GarageQual)) %>%
  mutate(GarageQual = as.numeric(recode(GarageQual, !!!qualities)))

table(all$GarageQual)


table(all$GarageCond)
all <- all %>%
  mutate(GarageCond = ifelse(is.na(GarageCond), "none",GarageCond)) %>%
  mutate(GarageCond = as.numeric(recode(GarageCond, !!!qualities)))

table(all$GarageCond)


#fixing basement variables

all %>%
  select(contains("Bsmt")) %>% #there are 11 variables related to basement 
  summarise_all(~sum(is.na(.)))# number of na's in each column


#find rows were variables have na's simutaneously
all %>%
  filter(is.na(BsmtQual) & is.na(BsmtCond) & is.na(BsmtExposure) & is.na(BsmtFinType1) &
         is.na(BsmtFinType2)) %>%
  nrow()

all %>%
  mutate(row_id = row_number()) %>%
  filter(!is.na(BsmtFinType1) & 
           (is.na(BsmtFinType2) | 
           is.na(BsmtExposure) | 
           is.na(BsmtCond) | 
           is.na(BsmtQual))) %>%
  select(row_id,BsmtFinType1, BsmtFinType2, BsmtExposure, BsmtCond, BsmtQual )
  

  

 call_mode <- function(x){
   freq_x <- table(x)
   mode_value <- names(freq_x)[freq_x == max(freq_x)]
   return(mode_value)
 }
  
call_mode(all$BsmtFinType2)
#replace missing values with mode
all <- all %>%
  mutate(row_id = row_number()) %>%
  mutate(BsmtFinType2 = ifelse(row_number()==333, call_mode(BsmtFinType2),
                               BsmtFinType2),
         BsmtExposure = ifelse(row_number() %in% c(949, 1488, 2349), 
                               call_mode(BsmtExposure), BsmtExposure),
         BsmtCond = ifelse(row_number() %in% c(2041, 2186, 2525),
                           call_mode(BsmtCond), BsmtCond),
         BsmtQual = ifelse(row_number() %in% c(2218 , 2219), call_mode(BsmtQual), BsmtQual)) 




#factorizing/one_hot coding for variables
all <- all %>%
  mutate(BsmtQual = ifelse(is.na(BsmtQual), "none", BsmtQual),
         BsmtQual = as.numeric(recode(BsmtQual, !!!qualities)))
  
  
all <- all %>%
  mutate(BsmtCond = ifelse(is.na(BsmtCond), "none", BsmtCond),
         BsmtCond = as.numeric(recode(BsmtCond, !!! qualities)))



Exposure <- c("None" = 0, "No"=1, "Mn"=2, "Av"=3, "Gd"=4)
Exposure

all <- all %>%
  mutate(BsmtExposure = ifelse(is.na(BsmtExposure), "None", BsmtExposure),
         BsmtExposure = as.numeric(recode(BsmtExposure, !!! Exposure)))


FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)

all <- all %>%
  mutate(BsmtFinType1 = ifelse(is.na(BsmtFinType1), "None", BsmtFinType1),
         BsmtFinType1 = as.numeric(recode(BsmtFinType1, !!! FinType)))



all <- all %>%
  mutate(BsmtFinType2 = ifelse(is.na(BsmtFinType2), "None", BsmtFinType2),
         BsmtFinType2 = as.numeric(recode(BsmtFinType2, !!! FinType)))


# all %>%
#   mutate(row_id = row_number()) %>%
#   filter(if_any(contains("Bsmt"), is.na)) %>%
#   select(row_id, BsmtQual, contains("Bsmt")) 
#   
# 


#handling the other basement variables with 1 or 2 NA's
all <- all %>%
  mutate(BsmtFinSF1 = ifelse(is.na(BsmtFinSF1), 0, BsmtFinSF1),
         BsmtFinSF2 = ifelse(is.na(BsmtFinSF2), 0, BsmtFinSF2),
         BsmtUnfSF = ifelse(is.na(BsmtUnfSF), 0, BsmtUnfSF),
         TotalBsmtSF = ifelse(is.na(TotalBsmtSF), 0, TotalBsmtSF),
         BsmtFullBath = ifelse(is.na(BsmtFullBath), 0, BsmtFullBath),
         BsmtHalfBath = ifelse(is.na(BsmtHalfBath), 0, BsmtHalfBath))



#fixing NA's in mansory variables
all %>%
  select(contains("Mas")) %>%
  summarise_all(~sum(is.na(.)))

all %>%
  mutate(row_id = row_number()) %>%
  filter(is.na(MasVnrType) & is.na(MasVnrArea)) %>%
  select(row_id, contains("Mas")) %>%
  view() %>%
  nrow()


all %>%
  mutate(row_id = row_number()) %>%
  filter(is.na(MasVnrType) & !is.na(MasVnrArea)) %>%
  select(row_id, MasVnrType,MasVnrArea)



call_mode <- function(x){
  freq_x <- table(x)
  mode_value <- names(freq_x)[freq_x== max(freq_x)]
  return(mode_value)
}

call_second_mode <- function(x) {
  freq_table <- sort(table(x), decreasing = TRUE)
  
  # Check if there are at least two different values
  if (length(freq_table) > 1) {
    return(names(freq_table)[2])  # Second most frequent value
  } else {
    return(names(freq_table)[1])  # If no second mode, return the most frequent value
  }
}



all <- all %>%
  mutate(MasVnrType = ifelse(row_id == 2611, call_second_mode(MasVnrType),
                             MasVnrType)) 
all %>%
  slice(2611) %>%
  select(row_id, contains("Mas")) 



all <- all %>%
  mutate(MasVnrType = ifelse(is.na(MasVnrType), "None",MasVnrType))


#check the ordinality of the MasVnrType

all %>%
  filter(!is.na(SalePrice)) %>%
  group_by(MasVnrType) %>%
  summarise(median_price = median(SalePrice), count = n()) %>%
  arrange(median_price)

Masonry <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)

all <- all %>%
  mutate(MasVnrType = as.numeric(recode(MasVnrType, !!! Masonry)))

all <- all %>%
  mutate(MasVnrArea = ifelse(is.na(MasVnrArea), 0, MasVnrArea))


#handling NA's in MSzoning variable
table(all$MSZoning, useNA = "ifany")

all <- all %>%
  mutate(MSZoning = ifelse(is.na(MSZoning), call_mode(MSZoning), MSZoning),
         MSZoning = as.factor(MSZoning))
sum(table(all$MSZoning))  

#handling NA's in kitchen variables
all %>%
  filter(!is.na(KitchenAbvGr) & is.na(KitchenQual)) %>%
  select(row_id, contains("kitchen"))

all <- all %>%
  mutate(KitchenQual = ifelse(row_id == 1556, call_mode(KitchenQual), KitchenQual),
         KitchenQual = as.numeric(recode(KitchenQual, !!! qualities))) 
  

all %>%
  slice(1556) %>%
  select(row_id, contains("kitchen"))


#handling NA's in utilities variable
all %>%
  filter(Utilities == "NoSeWa" | is.na(Utilities)) %>%
  select(row_id, 1:9)
  

all <- all %>%
  select(-Utilities)

#handling the functional variable



all <- all %>%
  mutate(Functional = ifelse(is.na(Functional), call_mode(Functional), Functional),
         Functional = as.numeric(recode(Functional, 'Sal'=0, 'Sev'=1, 
                                        'Maj2'=2, 'Maj1'=3, 'Mod'=4, 
                                        'Min2'=5, 'Min1'=6, 'Typ'=7)))



#handling exterior
all %>%
  summarise_all(~sum(is.na(.))) %>%
  select(contains("Exter"))


all <- all %>%
  mutate(Exterior1st = ifelse(is.na(Exterior1st), call_mode(Exterior1st), Exterior1st),
         Exterior2nd = ifelse(is.na(Exterior2nd), call_mode(Exterior2nd), Exterior2nd),
         Exterior2nd = as.factor(Exterior2nd),
         Exterior1st = as.factor(Exterior1st))


all <- all%>%
  mutate(ExterQual = as.numeric(recode(ExterQual, !!! qualities)),
         ExterCond = as.numeric(recode(ExterCond, !!! qualities)))



#dealing with NA's inelectrical variables
all <- all%>%
  mutate(Electrical = ifelse(is.na(Electrical), call_mode(Electrical), Electrical),
         Electrical = as.factor(Electrical))



#dealing with NA's in sales condition and sales type
all <- all %>%
  mutate(SaleType = ifelse(is.na(SaleType), call_mode(SaleType), SaleType),
         SaleType = as.factor(SaleType))

all <- all %>%
  mutate(SaleCondition = as.factor(SaleCondition))



# checking the all dataset for NA
all_with_na <- all %>%
  select(-SalePrice) %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "column",
               values_to = "NA_count") %>%
  filter(NA_count > 0)
all_with_na
  
  

Charcol <- all %>%
  select(where(is.character))%>%
  colnames()
Charcol

cat("there are", length(Charcol), "remaining character variables")



#HANDLING THE OTHER CHARACTER VARIABLES WITHOUT NA
# handling the founation variable

all <- all %>%
  mutate(Foundation = as.factor(Foundation))


# handling heating variables
table(all$Heating)
all <- all %>%
  mutate(Heating = as.factor(Heating),
         HeatingQC = recode(HeatingQC, !!! qualities),
         CentralAir = recode(CentralAir, "N" = 0, "Y" = 1))



#handling roof variables
table(all$RoofStyle)
all <- all %>%
  mutate(RoofStyle = as.factor(RoofStyle),
         RoofMatl = as.factor(RoofMatl))



# handling the land variables
table(all$LandContour)
table(all$LandSlope)
all <- all %>%
  mutate(LandContour = as.factor(LandContour),
         LandSlope = recode(LandSlope, "Sev" = 0, "Mod" = 1, "Gtl" = 2))


# handling dwelling variables
table(all$BldgType)
# lets check for ordinality of the variable using a graph
ggplot(data = all[!is.na(all$SalePrice), ],
       aes(x = BldgType, y = SalePrice))+
  stat_summary(fun = median, geom = "bar", fill = "blue")+
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), 
                     labels = scales::comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

# the graph does not show ordinallity, so we convert to factor
all <- all %>%
  mutate(BldgType = as.factor(BldgType))


table(all$HouseStyle)  
#no ordinality, convert to factor
all <- all%>%
  mutate(HouseStyle = as.factor(HouseStyle))


# handling neighborhood variables
table(all$Neighborhood)
levels(all$Neighborhood)

table(all$Condition1)
levels(all$Condition1)
all <- all %>%
  mutate(Condition1 = as.factor(Condition1))

table(all$Condition2)
levels(all$Condition2)
all <- all %>%
  mutate(Condition2 = as.factor(Condition2))


# handling road acess variables
table(all$Street)
# check ordinality with graph 
ggplot(data = all[!is.na(all$SalePrice), ], 
       aes(x = Street, y = SalePrice)) +
  stat_summary(fun = median, geom = "bar", fill = "blue")+
  scale_y_continuous(labels = scales::comma)+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

# graph shows variable is ordinal, so label encoding
all <- all %>%
  mutate(Street = recode(Street, "Grvl" = 0, "Pave" = 1))


table(all$PavedDrive)
# check for ordinality of variable
ggplot(data = all[!is.na(all$SalePrice), ], 
       aes(x = PavedDrive, y = SalePrice))+
  stat_summary(fun = median, geom = "bar", fill = "blue")+
  scale_y_continuous(labels = scales::comma)+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

# graph shows variable is ordinal, label encoding
all <- all %>%
  mutate(PavedDrive = recode(PavedDrive, "N" = 0, "P" = 1, "Y" = 2))

# check to be sure there is no NA in my dataset
any(is.na(all[!is.na(all$SalePrice), ]))

# At this point all the NA's in the data has been handled,
#and all variables changed to either numeric or categorical.
#However, there are 3 variables recorded as numeric, which should actually 
# be categorical: MSSubClass, Year and Month Sold. i will convert them to factors.

table(all$YrSold)
#I wil convert YrSold into a factor before modeling, but as I need 
#the numeric version of YrSold to create an Age variable, I am not doing that yet.
table(all$MoSold)
all <- all %>%
  mutate(MoSold = as.factor(MoSold))

ys <- ggplot(data =  all[!is.na(all$SalePrice), ], 
       aes(x = YrSold, y = SalePrice))+
  stat_summary(fun = median, geom = "bar", fill = "blue")+
  scale_y_continuous(breaks = seq(0, 800000, by = 25000), 
                     labels = scales::comma)+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice


ms <- ggplot(data = all[!is.na(all$SalePrice), ], 
             aes(x = MoSold, y = SalePrice))+
  stat_summary(fun = median, geom = "bar", fill = "blue")+
  scale_y_continuous(breaks = seq(0, 800000, by = 25000), 
                     labels = scales::comma)+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red")
#dashed line is median SalePrice


grid.arrange(ys, ms, widths=c(1,2))



table(all$MSSubClass)
all <- all %>%
  mutate(MSSubClass = as.factor(MSSubClass),
         MSSubClass = recode(MSSubClass, '20'='1 story 1946+', 
                     '30'='1 story 1945-', 
                     '40'='1 story unf attic', '45'='1,5 story unf', 
                     '50'='1,5 story fin', '60'='2 story 1946+', 
                     '70'='2 story 1945-', '75'='2,5 story all ages',
                     '80'='split/multi level', '85'='split foyer',
                     '90'='duplex all style/age', '120'='1 story PUD 1946+', 
                     '150'='1,5 story PUD all', '160'='2 story PUD 1946+',
                     '180'='PUD multilevel', '190'='2 family conversion'))


levels(all$MSSubClass)
# check for numeric variables
numericVars <- which(sapply(all, is.numeric))
factorVars <- which(sapply(all, is.factor))

glue("there are {length(numericVars)} numeric variables 
     and {length(factorVars)} factor variables")

                # OR

# numi <- all %>%
#   select(where(is.numeric))
# 
# dumi <- all %>%
#   select(where(is.factor))
# 
# glue("there are {length(numi)} numeric variables 
#      and {length(dumi)} factor variables")



# check the correlation between the numeric variables
all_numVar <- all %>%
  select(where(is.numeric))
all_numVar

cor_numVar <- cor(all_numVar, use = "pairwise.complete.obs", method = "pearson") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- cor_numVar[ "SalePrice", ] %>%
  .[abs(.) > 0.5]  %>%
  sort( decreasing=TRUE)
cor_sorted  

# call names() on the sorted corr. matrix to get a vector of names that i can use to subset the corr. matrix
CorHigh <- names(cor_sorted)
CorHigh

#subset the correlation matrix
cor_numVar <- cor_numVar[CorHigh, CorHigh]
cor_numVar
# plot the correlation matrix
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)


# Finding variable importance with a quick random forest
# select only rows without NA
#all <- all[complete.cases(all), ]

# 1. Set the seed for reproducibility
set.seed(2018)
quick_RF <- randomForest(x=all[1:1460,-79], y=all$SalePrice[1:1460], ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]
ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + 
  geom_bar(stat = 'identity') + labs(x = 'Variables', 
                                     y= '% increase MSE if variable is randomly permuted') +
coord_flip() + 
theme(legend.position="none")

# 
# set.seed(2018)
# quick_RF <- randomForest(x = all[1:1460, -79],
#                          y = all$SalePrice[1:1460], ntree = 100, importance = TRUE)
# impt_quick_RF <- importance(quick_RF)
# 
# df_RF <- data.frame(variables = row.names(impt_quick_RF), MSE = impt_quick_RF[, 1])
# df_RF <- df_RF[order(df_RF$MSE, decreasing = TRUE), ]
# df_RF
# 
# ggplot(data = df_RF[1:20,], aes(x=reorder(variables, MSE), y = MSE, fill = MSE))+
#          geom_bar(stat = "identity")+
#        labs(x = "Variables", y = "% increase MSE if variable is randomly permuted")+
#   coord_flip()+
#   theme_minimal()
# 
# 
#
# Visualizing the GrLivArea and other square feet measurements
s1 <- ggplot(all, aes(x = GrLivArea))+
        geom_density()+
        labs(x = 'Square feet living area')

s2 <- ggplot(all, aes(x = as.factor(x = TotRmsAbvGrd)))+
        geom_bar(stat = "count", fill = "blue")+
        labs(x = 'Rooms above Ground')

s3 <- ggplot(data = all, aes(x = `1stFlrSF`))+
        geom_density()+
        labs(x = 'Square feet first floor')
        
s4 <- ggplot(data = all, aes(x = `2ndFlrSF` ))+
        geom_density()+
        labs(x = "Square feet second floor")

s5 <- ggplot(data = all, aes(x = TotalBsmtSF ))+
        geom_density()+
        labs(x = "Square feet basement")

s6 <- ggplot(data = all[all$LotArea < 100000, ], aes(x = TotalBsmtSF ))+
        geom_density()+
        labs(x = "Square feet lot")

s7 <- ggplot(data = all, aes(x = LotFrontage ))+
        geom_density()+
        labs(x = "Linear feet lot frontage")

s8 <- ggplot(data = all, aes(x = LowQualFinSF ))+
        geom_histogram()+
        labs(x = "Low quality square feet 1st & 2nd")

(s1|s2)/(s3|s5)/(s4|s8)/(s6|s7)


# GrLivArea seemed to be just the total of square feet 1st and 2nd floor
all %>%
  filter(LowQualFinSF > 0) %>%
  select(row_id, GrLivArea,  LowQualFinSF, `1stFlrSF`, `2ndFlrSF`)
  

# visualizing the second most important variable- neigbourhood which is also categorical
#the median SalePrice by Neighorhood
n1 <- ggplot(data =  all[!is.na(all$SalePrice), ],
       aes(x = Neighborhood, y = SalePrice))+
    stat_summary(fun = median, geom = "bar", fill = "blue")+
    geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3)+
    scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)+
    geom_hline(yintercept=163000, linetype="dashed", color = "red") + #dashed line is median 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


# frequency distribution of the neigbourhood data
ggplot(data = all, aes(x = Neighborhood))+
  geom_bar(stat = "count", fill = "blue")+
  geom_label(stat = "count", aes(label = ..count.., y= ..count..))+
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  

# visualizing the 3rd most important variable
q1 <- ggplot(data = all, aes(x = as.factor(OverallQual)))+
    geom_bar(stat = "count")
  
q2 <- ggplot(data = all, aes(x = as.factor(ExterQual)))+
    geom_bar(stat = "count")

q3 <- ggplot(data = all, aes(x = as.factor(BsmtQual)))+
    geom_bar(stat = "count")

q4 <- ggplot(data = all, aes(x = as.factor(KitchenQual)))+
   geom_bar(stat = "count")

q5 <- ggplot(data = all, aes(x = as.factor(GarageQual)))+
    geom_bar(stat = "count")

q6 <- ggplot(data = all, aes(x = as.factor(FireplaceQu)))+
    geom_bar(stat = "count")

q7 <- ggplot(data = all, aes(x = as.factor(PoolQC)))+
    geom_bar(stat = "count")


(q1|q2)/(q3|q4)/(q5|q6|q7)



# visualizing the 4th most important variable which is also categorical
ggplot(data = all[!is.na(all$SalePrice), ], aes(x = MSSubClass , y = SalePrice))+
  stat_summary(fun = median, geom = "bar", fill = "blue")+
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)+
  geom_label(stat = "count", aes(label = ..count.., y= ..count..), size = 3)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median
  
  
# visualizing the frequency distribution
ggplot(data = all, aes(x = MSSubClass))+
  geom_bar(stat = "count", fill = "blue")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_label(stat = "count", aes(label = ..count.., y= ..count..), size = 3)
# visualizing the frequency distribution of the garage variable
sum(table(all$GarageCars))
table(all$GarageYrBlt)

all <- all %>%
  mutate(GarageYrBlt = ifelse(row_id == 2593, 2007, GarageYrBlt))
  

# ggplot(data = all[!is.na(all$GarageCars == 0), ], aes(x = GarageYrBlt))+
#   geom_histogram(stat = "count")

g1 <- ggplot(data = all, aes(x = GarageYrBlt))+
      geom_bar(stat = "count")

g2 <- ggplot(data = all, aes(x = GarageType))+
      geom_bar(stat = "count")

g3 <- ggplot(data = all, aes(x = GarageFinish))+
      geom_bar(stat = "count")

g4 <- ggplot(data = all, aes(x = as.factor(GarageCars)))+
      geom_bar(stat = "count")

g5 <- ggplot(data = all, aes(x = GarageArea))+
      geom_density()

g6 <- ggplot(data = all, aes(x = as.factor(GarageQual)))+
      geom_bar(stat = "count")

g7 <- ggplot(data = all, aes(x = as.factor(GarageCond)))+
      geom_bar(stat = "count")

(g1|g2)/(g3|g4)/(g5|g6|g7)

#visualizing the basement variables
all%>%
select(contains("bsmt"))

b1 <- ggplot(data = all, aes(x = as.factor(BsmtQual)))+
    geom_bar(stat = "count")+
    labs(x = "Height of the basement")

b2 <- ggplot(data = all, aes(x = BsmtCond))+
        geom_bar(stat = "count")+
        labs(x = "Rating of general condition")

b3 <- ggplot(data = all, aes(x = BsmtExposure))+
        geom_bar(stat = "count")+
        labs(x = "Walkout or garden level walls")

b4 <- ggplot(data = all, aes(x = as.factor(BsmtFinType1)))+
        geom_bar(stat = "count")+
        labs(x = "Rating of Type 1 finished area")

b5 <- ggplot(data = all, aes(x = BsmtFinSF1))+
        geom_density()+
        labs(x = "Type 1 finished square feet")

b6 <- ggplot(data = all, aes(x = BsmtFinType2))+
        geom_bar(stat = "count")+
        labs(x = "Rating of Type 2 finished area")

b7 <- ggplot(data = all, aes(x = BsmtFinSF2))+
        geom_density()+
        labs(x = "Type 2 finished square feet")

b8 <- ggplot(data = all, aes(x = BsmtUnfSF))+
        geom_histogram()+
        labs(x = "Unfinished square feet")

(b1|b2)/(b3|b4)/(b5|b6)/(b7|b8)



# checking the correlation between this three variables - BsmtUnfSF,  BsmtFinSF2, BsmtFinSF1 and TotalBsmtSF 
cor_check <- all %>%
  mutate(finished_unfinished = BsmtUnfSF + BsmtFinSF2 + BsmtFinSF1)
cor_check

correx <- cor(all$TotalBsmtSF, cor_check$finished_unfinished)
correx


# feature engineering
all %>%
  select(contains("Bath"))

# of all the bathroom varaiables, only fullbath seems to have a correlation with salesprice (0.5).
#i will combine these variables so they become one strong predictor

all <- all %>%
  mutate(Totbathrooms = BsmtFullBath + (BsmtHalfBath * 0.5) + FullBath + (HalfBath * 0.5))
  
tb1 <- ggplot(data = all[!is.na(all$SalePrice),], aes(x = Totbathrooms, 
                                               y = SalePrice))+
          geom_point()+
          geom_smooth(method = "lm", se = FALSE, colour = "blue")

cor(all$SalePrice, all$Totbathrooms, use = "complete.obs")

tb2 <- ggplot(data = all, aes(x = as.factor(Totbathrooms)))+
          geom_bar()

(tb1|tb2)


#adding new variables ; 'Remod' and 'Age'
all <- all %>%
  mutate(Remod = ifelse(YearBuilt == YearRemodAdd, 0, 1), #0 =No Remodeling, 1=Remodeling
        Age =  YrSold - YearRemodAdd)

all %>%
  filter(Age == -2)%>%
  select(row_id, Age)

all%>%
slice(2550)%>%
  select(Age, YearRemodAdd, YearBuilt, YrSold)

ggplot(data = all[!is.na(all$SalePrice), ], aes(x = Age, y = SalePrice))+
  geom_point(colour = "blue")+
  geom_smooth(method = "lm", se=FALSE, colour = "black")

cor(all$SalePrice, all$Age, use = "complete.obs")# older houses cost less


# visualizing Remod and saleprice
ggplot(data = all[!is.na(all$SalePrice),], aes(x = as.factor(Remod), y = SalePrice))+
  stat_summary(fun = median, geom = "bar", fill = "blue")+
  geom_label(stat = "count", aes(label = ..count.., y= ..count..), size = 6)+
  scale_y_continuous(breaks = seq(0, 800000, by = 500000), labels = scales::comma)+
  theme_classic()+
  geom_hline(yintercept=163000, linetype="dashed") #dashed line is median SalePrice



#creating a new variable - IsNew(houses that has not been remolded)
all <- all%>%
  mutate(IsNew = ifelse(YearBuilt == YrSold, 1, 0))
  
table(all$IsNew)

#graph shows houses sold in the same year they were built cost more
ggplot(data = all[!is.na(all$SalePrice),], aes(x = as.factor(IsNew), y = SalePrice))+
  stat_summary(fun = median, geom = "bar", fill = "blue")+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3)+
  scale_y_continuous(breaks = seq(0, 800000, by = 500000), labels = scales::comma)+
  theme_classic()+
  geom_hline(yintercept=163000, linetype="dashed") #dashed line is median SalePrice
  
all$YrSold <-  as.factor(all$YrSold)



str(all$Neighborhood)

nb1 <- ggplot(data = all[!is.na(all$SalePrice), ],
              aes(x = reorder(Neighborhood, SalePrice, FUN = median),  y = SalePrice))+
  stat_summary(fun = median, geom = "bar", fill = "blue", colour = "green")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "visualization of neigbourhood and saleprice",
       x = "Neighbourhood",
       y = "Sale price")+
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3 )+
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median
nb1


nb2 <- ggplot(data = all[!is.na(all$SalePrice),], 
              aes(x = reorder(Neighborhood, SalePrice, FUN = mean), y = SalePrice))+
  stat_summary(fun = mean, geom = "bar", fill = "black", colour = "white")+
  labs(thttp://127.0.0.1:36237/graphics/plot_zoom_png?width=1334&height=627itle = "visualization of neigbourhood and saleprice",
       x = "Neighbourhood",
       y = "Sale price")+
  theme(axis.text = element_text(angle = 45, hjust = 1))+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3)+
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)+
  geom_hline(yintercept=180921, linetype="dashed", color = "red") #dashed line is median
nb2

(nb1|nb2)


mean(all$SalePrice, na.rm = TRUE)
# this boxplot explains how outliers can affect the mean of a distribution
ggplot(data = all, aes(x = Neighborhood, y= SalePrice))+
  geom_boxplot()+
  theme(axis.text = element_text(angle = 45, hjust = 1))
table(all$Neighborhood)



#create a new column that differentiates rich, middle, and power neigbourhood. AKA binning
all <- all %>%
  mutate(NeighRich = case_when(
    Neighborhood %in% c("MeadowV","IDOTRR","BrDale") ~ 0,
    Neighborhood %in% c("StoneBr", "NoRidge", "NridgHt") ~ 2,
    TRUE ~ 1
  ))

table(all$NeighRich, useNA = "ifany")



# create a predictor that adds up the living space above and below ground.
ggplot(data = all[!is.na(all$SalePrice), ], aes(x = GrLivArea, y = SalePrice)) +
  geom_point(colour = "blue")+
  geom_smooth(method = "lm", se= FALSE, colour = "black")+
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)] > 4500, rownames(all), " ")))



all <- all %>%
  mutate(TotalSqFeet = GrLivArea + TotalBsmtSF)

ggplot(data = all[!is.na(all$SalePrice), ], aes(x = TotalSqFeet, y = SalePrice)) +
  geom_point(colour = "blue")+
  geom_smooth(method = "lm", se= FALSE, colour = "black")+
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)] > 4500, rownames(all), " ")))
  

