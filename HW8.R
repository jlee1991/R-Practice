#HW8 
#Part 1: Build a PowerPoint using week 3 data, change all of the graphs variables and save the PowerPoint.  Add another slide.  Turn in the pptx.
#Note: It did not make particular sense to stick with the histograms so I changed the graphs to Pie Charts

# libraries
library(officer)
library(ggplot2)
library(flextable)

# 2 decimal places
options(digits=2)

# set wd
setwd("/cloud/project/lessons/9_Apr3_APIs_ReportingAutomation/wk9_data")

# Identify the file in the directory
searchPattern <- 'Wk3'
pth <- list.files(pattern = searchPattern, 
                  full.names = T)
pth <- pth[grep('*.csv', pth)]
df  <- read.csv(pth)

pptx <- read_pptx('exampleTemplate.pptx')

# All ppt inputs
author <- 'Jim Lee, the (Data) Scientist Guy'
theme  <- '1_Office Theme'

# Let's start with a title slide
titleTxt <- paste(Sys.Date(), 'Patient Review')
pptx <- add_slide(pptx, 
                  layout = "Title Slide", 
                  master = theme)

pptx <- ph_with_text(pptx, type = "ctrTitle", str = titleTxt ) 
pptx <- ph_with_text(pptx, type = "subTitle",  str = author)


# Add another slide "Two Content"
pptx <- add_slide(pptx, 
                  layout = "Two Content", 
                  master = theme)

# Add a slide title
pptx <- ph_with_text(pptx, 
                     type = "title", 
                     str  = 'Race' )

# Make a simple visual and add it to the first body section
test <- lapply(df[, -1], function(x) as.data.frame(table(df$race)))
df_race <- test$race
bp <- ggplot(df_race, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0)
pptx <- ph_with_gg(pptx, value = bp, index = 1)

# 2nd box add table of info
Unknown <- as.matrix(table(df$race))[1,1]
AfricanAmerican  <- as.matrix(table(df$race))[2,1]
Asian <- as.matrix(table(df$race))[3,1]
Caucasian  <- as.matrix(table(df$race))[4,1]
Hispanic <- as.matrix(table(df$race))[5,1] 
Other <- as.matrix(table(df$race))[6,1] 

highlevelDF <- data.frame(Unk = Unknown,
                          AfricanAm = AfricanAmerican,
                          Asian = Asian,
                          Caucasian = Caucasian,
                          Hispanic = Hispanic,
                          Other = Other)

# Flextable
ft   <- flextable(data = highlevelDF)
pptx <- ph_with_flextable(pptx, value = ft, type = "body", index = 2)

# Add another slide
pptx <- add_slide(pptx, 
                  layout = "Comparison", 
                  master = theme)
# Add the title
pptx <- ph_with_text(pptx, 
                     type = "title", 
                     str = 'Admission Details' )

# Left side sub title
pptx <- ph_with_text(pptx, 
                     type  = "body", 
                     str   = 'Admission Source',
                     index = 1)

# Right side sub title
pptx <- ph_with_text(pptx, 
                     type  = "body", 
                     str   = 'Medical Specialty',
                     index = 5)

# Left side content
jpeg('tmpAdmissionSource.jpg')
test2 <- lapply(df[, -1], function(x) as.data.frame(table(df$admission_source_id)))
df_admission_source <- test2$admission_source_id
ggplot(df_admission_source, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0)
dev.off()
pptx <- ph_with_img(pptx, 
                    type   = "body",
                    src    = 'tmpAdmissionSource.jpg',
                    height = 4.5,
                    width  = 4,
                    index  = 4)

# Right side content
jpeg('tmpMedicalSpecialty.jpg')
test3 <- lapply(df[, -1], function(x) as.data.frame(table(df$medical_specialty)))
df_medical_specialty <- test3$medical_specialty
ggplot(df_admission_source, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0)
dev.off()
pptx <- ph_with_img(pptx, 
                    type   = "body",
                    src    = 'tmpMedicalSpecialty.jpg',
                    height = 4.5,
                    width  = 4,
                    index  = 2)

# Add another slide
pptx <- add_slide(pptx, 
                  layout = "Two Content", 
                  master = theme)

# Add a slide title
pptx <- ph_with_text(pptx, 
                     type = "title", 
                     str  = 'Discharge Reason' )

jpeg('tmpDischargeDisposition.jpg')
test4 <- lapply(df[, -1], function(x) as.data.frame(table(df$discharge_disposition_id)))
df_discharge_disposition_id <- test4$discharge_disposition_id
ggplot(df_discharge_disposition_id, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0)
dev.off()
pptx <- ph_with_img(pptx, 
                    type   = "body",
                    src    = 'tmpDischargeDisposition.jpg',
                    height = 4.5,
                    width  = 4,
                    index  = 2)

fileName <- paste0('diabetesReadout_',searchPattern,'.pptx')
print(pptx, target = fileName)
