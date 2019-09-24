#Library
library(caret)
library(plyr)
library(dplyr)
library(vtreat)
library(e1071)
library(rbokeh)
library(MLmetrics)
library(reshape2)

#Set wd
setwd("/Users/jlee/Desktop/Data_Scientist_Exercise_Jim_Lee")
options(scipen=999)

#Read in the csv files
clinical <- read.csv('clinical.csv') #Clinical Data
clinical$Radiation <- as.factor(clinical$Radiation)
genomics <- read.csv('genomics.csv') #Genomics Data

#Transpose the genomics file and aggregate around the patient ID (as to have all info in one row)
genomicsTransp <- dcast(genomics, ID ~ Gene, value.var = "Gene")
genomicsTransp[-1] <- lapply(genomicsTransp[-1], function(x) as.factor(as.numeric(!is.na(x))))

#Join the data together
clinicalEnriched <- clinical %>% 
  left_join(genomicsTransp, by = "ID")

#############################
##Exploratory Data Analysis##
#############################

#Given the genomics dataset has detailed information, num.mutations and num.mutated.genes are redundant
#The patient ID is not useful as a feature to build a model on

#High Level Overview of Clinical Data
summary(clinicalEnriched)

#Some noteable observations
#92/190 Entries of Tumor Size are NULL!
summary(clinicalEnriched$Tumor.Size)

#96/190 Entries of M (distant metastasis) are NULL!
summary(clinicalEnriched$M)

#"Right" Upper Lobe mispelled on two entries
summary(clinicalEnriched$Primary.Site)

#IB (1B) mispelled on one entry
summary(clinicalEnriched$Stage)

#There are 6 patients who are not in the genomic dataset (NAs on join indicate as such)
summary(clinicalEnriched$AKT1)

#Age seems to be random here, but it does seem notable that the age range is only from 56-84
table(clinicalEnriched$Age,clinicalEnriched$Outcome)

#Interesting trend that individuals who recieved radiation tend to survive more often
table(clinicalEnriched$Radiation,clinicalEnriched$Outcome)

#Tumor grade seems incorrect as there is not a tumor stage of 1 (description is 1-4) and 9 likely indicates 
#that it does not exist or an incorrectly labeled 1. Given this ambiguous nature of the grade, this may be 
#something subject to clean up or feature removal
table(clinicalEnriched$Grade,clinicalEnriched$Outcome)
clinicalEnriched$Grade[clinicalEnriched$Grade == 9] <- NA #Removing all 9 values

#####################
##Clean and Analyze##
#####################

#Define Variables for vtreat
informativeVars <- c("Survival.Months", "Age", "Grade", "Num.Primaries", "T", "N", "M",
                     "Radiation","Stage", "Primary.Site", " Histology", "Tumor.Size",
                     "AKT1", "ALK_Col1", "ALK_Col2", "APC", "ATM_Col1", "ATM_Col2", "BRAF", "CCND2",  
                     "CDKN2A", "CTNNB1", "DNMT3A", "EGFR", "ERBB3", "ERBB4", "ESR1", "FBXW7",    
                     "FGFR1", "FGFR3", "FLT4", "FOXL2", "GNAS", "HNF1A", "KRAS_Col1", "KRAS_Col2",
                     "MAP2K2", "MET", "MLH_Col2", "MSH2", "MSH6", "NF_Col1", "NF_Col2", "NF_Col3", 
                     "NF_Col5", "NOTCH1", "NTRK1")
y1Target        <- "Outcome"
y1Success       <- "Alive"

#Apply vtreat
plan <- designTreatmentsC(clinicalEnriched,
                          informativeVars,
                          y1Target,
                          y1Success)

treatedData <- prepare(plan, clinicalEnriched)

#Create Training and Test Datasets
set.seed(1234)
splitPercent <- round(nrow(treatedData) %*% .8) #Divide the data into 20% and 80%
totalRecords <- 1:nrow(treatedData)
idx <- sample(totalRecords, splitPercent)
trainDat <- treatedData[idx,]
testDat  <- treatedData[-idx,]

#Create a model (Logistic Regression)
fit <- train(as.factor(Outcome) ~ ., 
              data = trainDat,
              method="glm", family="binomial")
fit

####################
## Assess/Predict ##
####################

# Predict, Confusion Matrix, & Accuracy -- For Training Set: "100%"
preds <- predict(fit, trainDat)                 
confusionMatrix(preds, trainDat$Outcome)

# Predict, Confusion Matrix, & Accuracy -- For Test Set: 86.8%
newPreds        <- predict(fit, testDat)      
confusionMatrix(newPreds, testDat$Outcome)

#Based on the results, we can determine that the model created is clearly overfit in that the 
#accuracy of predictions from the model applied to the training set are less than that of the test set
#Still, a nearly 90% accuracy on a model is not an entirely poor result and could be used to estimate
#probabilities of a patient's mortality. To test for one year new samples for one year, I would 
#approach by setting the survival months to 12. Some of the other features may also require assumptions,
#particularly with tumor growth rate and stage that could be a source of error. Afterwards by applying 
#the same treatment plan with vtreat, one can use this model to predict results.

###############
## Questions ##
###############

#1. This data, like our real data, may be messy, incomplete, and/or sparsely documented. 
#Please walk us through how you cleaned up this dataset. Do you see any interesting trends?

#In terms of clean up, I mainly applied the package vtreat which allowed me to deal with the NULL and UNK 
#values that were missing in order to build the model. Vtreat creates extra dummy variables in additional columns 
#(including NULL and UNK values) that help when creating a supervised learning model. 

#There are a few examples of misspelled words that I noticed through EDA but those ultimately impacted a small
#amount of the dataset (e.g. one or two rows). Some of the values needed to be converted to factors (e.g. Radiology)
#Additionally, there are a few missing patients from the genomics dataset. 

#Regarding interesting trends, 18% of individuals who did not recieve radiation are alive and whereas
#27% of individuals who did recieve radiation are alive.

#2. Tell us how you decided which features to use for your model. 
#Are there any new features it might be productive to engineer from the current features?

#Given the gene details are fully described in the genomics dataframe, we can omit information from 
#num.mutations and num.mutated.genes. Additionally, patient ID was not neccessary as a feature after joining the datasets.

#Otherwise, I tried to include as much information in the model as I could sice important 
#in generating the overall model. The genomics file was reformatted and transposed to enrich the clinical dataset. 
#The main reason for this was to aggregate the column details to one row and determine if certain mutations in a specific
#gene may be linked to a patient's survival.

#3. Which algorithm did you use for the prediction and why?

#For the purposes of this analysis, I used a Logistic Regression to generate a model on the categorical results.
#This is generally a simple modelling approach ideal for predicting cases that are binary. When applied to a feature 
#set with unknown outcomes) the results appear in the form of a probability which can help determine which patients are most at risk
#or the status could be determined by defining a cutoff threshold for the results. 

#4. How did you assess the predictive model’s quality? Summarize your findings.

#I used a train-test split of 80/20 from the original dataset. This means that my model was created from analyzing 
#the features of 152 random entries and than tested against a seperate partition of another 38 random entries all 
#from the original set. This is done for the purposes of determining an overfit model in the training set.

#From observation, my model was fairly overfit when comparing the accuracy results. The accuracy from applying the model against 
#the test set was 86.8% (nearly 90%). But, the model could benefit from additional analysis of some of the features as the
#fit with the training model was "perfect".

#5. Next steps? What might you do with more time or access to additional data or expertise?

#Given the small size of this dataset (190 entries) it was difficult to partition the data and perform other
#analysis including creating a validation set and applying a k-folds approach to sampling. Adding more diverse information
#would help make this model more accurate. In one case, from what I observed in the age distribution (while it may be sparse
#at younger ages) it would be advantageous to collect a bigger diversity of patient ages. Also other biological
#factors like gender, weight or race may also be features that could play into survival rates.

#In terms of expertise, there is some conceptual information that might help. One notable example being Tumor Stage.
#I notice that there are references to stage 1a/1b vs 1 marked differently. I am not sure if those are subdivisions
#of the stages or completely different stages. 

#With more time, I would re-evaluate the features to remove any features to the model that may improve the accuracy
#I could also explore other modeling approaches for accuracy comparison, including k-NN, Decision Trees and Random Forest models.
#Lastly, I would have also attempted this study with Python packages and Jupyter Notebook but for some reason, my computer cuts off
#coding when generating a PDF extract

#######################
##Comments / Feedback##
#######################

#Given this analysis, some other thoughts to change the modeling have been made apparent. In creating a one year model - 
#rather than using Survival.Months as a feature, one could improve upon my work by updating the outcome to account for
#only 12 month survival by adjusting for any patients alive after 12 months in Survival.Months and creating a model strictly
#the revised outcome result.