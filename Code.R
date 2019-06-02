data<- read.csv("E:\\BA assingment\\Project 1\\SchoolHist.csv", header = T, stringsAsFactors = F)
uni1 <- data
avg.HH<-read.csv(file ="E:\\BA assingment\\Project 1\\Household average income.csv", header = T, stringsAsFactors = F)

#Merging AVG.Household data set
uni<- merge(x=uni1, y=avg.HH,by ="State",all=T)
summary(uni)
uni

#INSTATE _ CONNECTICUT
#1.InState Y, Full time, 2015, Applicant
iNgpa1 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa1 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa1]=mean(uni$GPA[iYgpa1])
summary(uni$GPA)
#2.InState Y, Full time, 2015, Prospect
iNgpa2 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa2 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa2] = mean(uni$GPA[iYgpa2])
summary(uni$GPA)
#3.InState Y, Full time, 2015, Suspect
iNgpa3 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa3 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa3] = mean(uni$GPA[iYgpa3])
summary(uni$GPA)
#4.InState Y, Full time, 2016, Applicant
iNgpa4 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa4 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa4] = mean(uni$GPA[iYgpa4])
summary(uni$GPA)
#5.InState Y, Full time, 2016, Prospect
iNgpa5 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa5 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa5]= mean(uni$GPA[iYgpa5])
summary(uni$GPA)
#6.InState Y, Full time, 2016, Suspect
iNgpa6 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa6 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa6] = mean(uni$GPA[iYgpa6])
summary(uni$GPA)
#7.InState Y, Full time, 2017, Applicant
iNgpa7 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa7 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa7] = mean(uni$GPA[iYgpa7])
summary(uni$GPA)
#8.InState Y, Full time, 2017, Prospect
iNgpa8 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa8 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa8] = mean(uni$GPA[iYgpa8])
summary(uni$GPA)
#9.InState Y, Full time, 2017, Suspect(59330 out of 99268)
iNgpa9 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa9 <- which(uni$InState == "Y" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa9] = mean(uni$GPA[iYgpa9])
summary(uni$GPA)
#10.InState Y, Part time, 2015, Applicant(no values)
iNgpa10 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa10 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa10] = mean(uni$GPA[iYgpa10])
summary(uni$GPA)
#11.InState Y, Part time, 2015, Prospect
iNgpa11 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa11 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa11] = mean(uni$GPA[iYgpa11])
summary(uni$GPA)
#12.InState Y, Part time, 2015, Suspect
iNgpa12 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa12 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa12] = mean(uni$GPA[iYgpa12])
summary(uni$GPA)
#13.InState Y, Part time, 2016, Applicant(no values)
iNgpa13 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa13 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa13] = mean(uni$GPA[iYgpa13])
summary(uni$GPA)
#14.InState Y, Part time, 2016, Prospect(no value)
iNgpa14 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa14 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa14] = mean(uni$GPA[iYgpa14])
summary(uni$GPA)
#15.InState Y, Part time, 2016, Suspect
iNgpa15 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa15 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa15] = mean(uni$GPA[iYgpa15])
summary(uni$GPA)
#16.InState Y, Part time, 2017, Applicant(no values)
iNgpa16 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa16 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa16] = mean(uni$GPA[iYgpa16])
summary(uni$GPA)
#17.InState Y, Part time, 2017, Prospect
iNgpa17 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa17 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa17] = mean(uni$GPA[iYgpa17])
summary(uni$GPA)
#18.InState Y, Part time, 2017, Suspect
iNgpa18 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa18 <- which(uni$InState == "Y" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa18] = mean(uni$GPA[iYgpa18])
summary(uni$GPA)
#Checking for NA's in InState DATA
check1 <- subset(uni, uni$InState == "Y")
summary(check1$GPA)

#NEW YORK 
#1.State NY, Full time, 2015, Applicant
iNgpa1 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa1 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa1]=mean(uni$GPA[iYgpa1])
summary(uni$GPA)
#2.State NY, Full time, 2015, Prospect
iNgpa2 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa2 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa2] = mean(uni$GPA[iYgpa2])
summary(uni$GPA)
#3.State NY, Full time, 2015, Suspect
iNgpa3 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa3 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa3] = mean(uni$GPA[iYgpa3])
summary(uni$GPA)
#4.State NY, Full time, 2016, Applicant
iNgpa4 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa4 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa4] = mean(uni$GPA[iYgpa4])
summary(uni$GPA)
#5.State NY, Full time, 2016, Prospect
iNgpa5 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa5 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa5]= mean(uni$GPA[iYgpa5])
summary(uni$GPA)
#6.State NY, Full time, 2016, Suspect
iNgpa6 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa6 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa6] = mean(uni$GPA[iYgpa6])
summary(uni$GPA)
#7.State NY, Full time, 2017, Applicant
iNgpa7 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa7 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa7] = mean(uni$GPA[iYgpa7])
summary(uni$GPA)
#8.State NY, Full time, 2017, Prospect
iNgpa8 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa8 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa8] = mean(uni$GPA[iYgpa8])
summary(uni$GPA)
#9.State NY, Full time, 2017, Suspect(59330 out of 99268)
iNgpa9 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa9 <- which(uni$State == "NY" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa9] = mean(uni$GPA[iYgpa9])
summary(uni$GPA)
#10.State NY, Part time, 2015, Applicant(no values)
iNgpa10 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa10 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa10] = mean(uni$GPA[iYgpa10])
summary(uni$GPA)
#11.State NY, Part time, 2015, Prospect
iNgpa11 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa11 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa11] = mean(uni$GPA[iYgpa11])
summary(uni$GPA)
#12.State NY, Part time, 2015, Suspect
iNgpa12 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa12 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa12] = mean(uni$GPA[iYgpa12])
summary(uni$GPA)
#13.State NY, Part time, 2016, Applicant(no values)
iNgpa13 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa13 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa13] = mean(uni$GPA[iYgpa13])
summary(uni$GPA)
#14.State NY, Part time, 2016, Prospect(no value)
iNgpa14 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa14 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa14] = mean(uni$GPA[iYgpa14])
summary(uni$GPA)
#15.State NY, Part time, 2016, Suspect
iNgpa15 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa15 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa15] = mean(uni$GPA[iYgpa15])
summary(uni$GPA)
#16.State NY, Part time, 2017, Applicant(no values)
iNgpa16 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa16 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa16] = mean(uni$GPA[iYgpa16])
summary(uni$GPA)
#17.State NY, Part time, 2017, Prospect
iNgpa17 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa17 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa17] = mean(uni$GPA[iYgpa17])
summary(uni$GPA)
#18.State NY, Part time, 2017, Suspect
iNgpa18 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa18 <- which(uni$State == "NY" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa18] = mean(uni$GPA[iYgpa18])
summary(uni$GPA)
#Checking for NA's in NY DATA
check2 <- subset(uni, uni$State == "NY")
summary(check2$GPA)
summary(uni$GPA)

#NEW JERSEY 
#1.State NJ, Full time, 2015, Applicant
iNgpa1 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa1 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa1]=mean(uni$GPA[iYgpa1])
summary(uni$GPA)
#2.State NJ, Full time, 2015, Prospect
iNgpa2 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa2 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa2] = mean(uni$GPA[iYgpa2])
summary(uni$GPA)
#3.State NJ, Full time, 2015, Suspect
iNgpa3 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa3 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa3] = mean(uni$GPA[iYgpa3])
summary(uni$GPA)
#4.State NJ, Full time, 2016, Applicant
iNgpa4 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa4 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa4] = mean(uni$GPA[iYgpa4])
summary(uni$GPA)
#5.State NJ, Full time, 2016, Prospect
iNgpa5 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa5 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa5]= mean(uni$GPA[iYgpa5])
summary(uni$GPA)
#6.State NJ, Full time, 2016, Suspect
iNgpa6 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa6 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa6] = mean(uni$GPA[iYgpa6])
summary(uni$GPA)
#7.State NJ, Full time, 2017, Applicant
iNgpa7 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa7 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa7] = mean(uni$GPA[iYgpa7])
summary(uni$GPA)
#8.State NJ, Full time, 2017, Prospect
iNgpa8 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa8 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa8] = mean(uni$GPA[iYgpa8])
summary(uni$GPA)
#9.State NJ, Full time, 2017, Suspect(59330 out of 99268)
iNgpa9 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa9 <- which(uni$State == "NJ" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa9] = mean(uni$GPA[iYgpa9])
summary(uni$GPA)
#10.State NJ, Part time, 2015, Applicant(no values)
iNgpa10 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa10 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa10] = mean(uni$GPA[iYgpa10])
summary(uni$GPA)
#11.State NJ, Part time, 2015, Prospect
iNgpa11 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa11 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa11] = mean(uni$GPA[iYgpa11])
summary(uni$GPA)
#12.State NJ, Part time, 2015, Suspect
iNgpa12 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa12 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa12] = mean(uni$GPA[iYgpa12])
summary(uni$GPA)
#13.State NJ, Part time, 2016, Applicant(no values)
iNgpa13 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa13 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa13] = mean(uni$GPA[iYgpa13])
summary(uni$GPA)
#14.State NJ, Part time, 2016, Prospect(no value)
iNgpa14 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa14 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa14] = mean(uni$GPA[iYgpa14])
summary(uni$GPA)
#15.State NJ, Part time, 2016, Suspect
iNgpa15 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa15 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa15] = mean(uni$GPA[iYgpa15])
summary(uni$GPA)
#16.State NJ, Part time, 2017, Applicant(no values)
iNgpa16 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa16 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa16] = mean(uni$GPA[iYgpa16])
summary(uni$GPA)
#17.State NJ, Part time, 2017, Prospect
iNgpa17 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa17 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa17] = mean(uni$GPA[iYgpa17])
summary(uni$GPA)
#18.State NJ, Part time, 2017, Suspect
iNgpa18 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa18 <- which(uni$State == "NJ" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa18] = mean(uni$GPA[iYgpa18])
summary(uni$GPA)
#Checking for NA's in NJ DATA
check3 <- subset(uni, uni$State == "NJ")
summary(check3$GPA)

#MA
#1.State MA, Full time, 2015, Applicant
iNgpa1 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa1 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa1]=mean(uni$GPA[iYgpa1])
summary(uni$GPA)
#2.State MA, Full time, 2015, Prospect
iNgpa2 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa2 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa2] = mean(uni$GPA[iYgpa2])
summary(uni$GPA)
#3.State MA, Full time, 2015, Suspect
iNgpa3 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa3 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa3] = mean(uni$GPA[iYgpa3])
summary(uni$GPA)
#4.State MA, Full time, 2016, Applicant
iNgpa4 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa4 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa4] = mean(uni$GPA[iYgpa4])
summary(uni$GPA)
#5.State MA, Full time, 2016, Prospect
iNgpa5 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa5 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa5]= mean(uni$GPA[iYgpa5])
summary(uni$GPA)
#6.State MA, Full time, 2016, Suspect
iNgpa6 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa6 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa6] = mean(uni$GPA[iYgpa6])
summary(uni$GPA)
#7.State MA, Full time, 2017, Applicant
iNgpa7 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa7 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa7] = mean(uni$GPA[iYgpa7])
summary(uni$GPA)
#8.State MA, Full time, 2017, Prospect
iNgpa8 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa8 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa8] = mean(uni$GPA[iYgpa8])
summary(uni$GPA)
#9.State MA, Full time, 2017, Suspect(59330 out of 99268)
iNgpa9 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa9 <- which(uni$State == "MA" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa9] = mean(uni$GPA[iYgpa9])
summary(uni$GPA)
#10.State MA, Part time, 2015, Applicant(no values)
iNgpa10 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa10 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa10] = mean(uni$GPA[iYgpa10])
summary(uni$GPA)
#11.State MA, Part time, 2015, Prospect
iNgpa11 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa11 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa11] = mean(uni$GPA[iYgpa11])
summary(uni$GPA)
#12.State MA, Part time, 2015, Suspect
iNgpa12 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa12 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa12] = mean(uni$GPA[iYgpa12])
summary(uni$GPA)
#13.State MA, Part time, 2016, Applicant(no values)
iNgpa13 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa13 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa13] = mean(uni$GPA[iYgpa13])
summary(uni$GPA)
#14.State MA, Part time, 2016, Prospect(no value)
iNgpa14 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa14 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa14] = mean(uni$GPA[iYgpa14])
summary(uni$GPA)
#15.State MA, Part time, 2016, Suspect
iNgpa15 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa15 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa15] = mean(uni$GPA[iYgpa15])
summary(uni$GPA)
#16.State MA, Part time, 2017, Applicant(no values)
iNgpa16 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa16 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa16] = mean(uni$GPA[iYgpa16])
summary(uni$GPA)
#17.State MA, Part time, 2017, Prospect
iNgpa17 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa17 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa17] = mean(uni$GPA[iYgpa17])
summary(uni$GPA)
#18.State MA, Part time, 2017, Suspect
iNgpa18 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa18 <- which(uni$State == "MA" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa18] = mean(uni$GPA[iYgpa18])
summary(uni$GPA)
#Checking for NA's in MA DATA
check4 <- subset(uni, uni$State == "MA")
summary(check4$GPA)

#ILLINOINS
#1.State IL, Full time, 2015, Applicant
iNgpa1 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa1 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa1]=mean(uni$GPA[iYgpa1])
summary(uni$GPA)
#2.State IL, Full time, 2015, Prospect
iNgpa2 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa2 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa2] = mean(uni$GPA[iYgpa2])
summary(uni$GPA)
#3.State IL, Full time, 2015, Suspect
iNgpa3 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa3 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa3] = mean(uni$GPA[iYgpa3])
summary(uni$GPA)
#4.State IL, Full time, 2016, Applicant
iNgpa4 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa4 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa4] = mean(uni$GPA[iYgpa4])
summary(uni$GPA)
#5.State IL, Full time, 2016, Prospect
iNgpa5 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa5 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa5]= mean(uni$GPA[iYgpa5])
summary(uni$GPA)
#6.State IL, Full time, 2016, Suspect
iNgpa6 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa6 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa6] = mean(uni$GPA[iYgpa6])
summary(uni$GPA)
#7.State IL, Full time, 2017, Applicant
iNgpa7 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa7 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa7] = mean(uni$GPA[iYgpa7])
summary(uni$GPA)
#8.State IL, Full time, 2017, Prospect
iNgpa8 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa8 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa8] = mean(uni$GPA[iYgpa8])
summary(uni$GPA)
#9.State IL, Full time, 2017, Suspect(59330 out of 99268)
iNgpa9 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa9 <- which(uni$State == "IL" & uni$StudentType == "Full-time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa9] = mean(uni$GPA[iYgpa9])
summary(uni$GPA)
#10.State IL, Part time, 2015, Applicant(no values)
iNgpa10 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa10 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa10] = mean(uni$GPA[iYgpa10])
summary(uni$GPA)
#11.State IL, Part time, 2015, Prospect
iNgpa11 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa11 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa11] = mean(uni$GPA[iYgpa11])
summary(uni$GPA)
#12.State IL, Part time, 2015, Suspect
iNgpa12 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa12 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2015fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa12] = mean(uni$GPA[iYgpa12])
summary(uni$GPA)
#13.State IL, Part time, 2016, Applicant(no values)
iNgpa13 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa13 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa13] = mean(uni$GPA[iYgpa13])
summary(uni$GPA)
#14.State IL, Part time, 2016, Prospect(no value)
iNgpa14 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa14 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa14] = mean(uni$GPA[iYgpa14])
summary(uni$GPA)
#15.State IL, Part time, 2016, Suspect
iNgpa15 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa15 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2016fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa15] = mean(uni$GPA[iYgpa15])
summary(uni$GPA)
#16.State IL, Part time, 2017, Applicant(no values)
iNgpa16 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & is.na(uni$GPA))
iYgpa16 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "APPLICANT" & !is.na(uni$GPA))
uni$GPA[iNgpa16] = mean(uni$GPA[iYgpa16])
summary(uni$GPA)
#17.State IL, Part time, 2017, Prospect
iNgpa17 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & is.na(uni$GPA))
iYgpa17 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "PROSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa17] = mean(uni$GPA[iYgpa17])
summary(uni$GPA)
#18.State IL, Part time, 2017, Suspect
iNgpa18 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & is.na(uni$GPA))
iYgpa18 <- which(uni$State == "IL" & uni$StudentType == "Part-Time" & uni$Term == "2017fall" & uni$Status.1 == "SUSPECT" & !is.na(uni$GPA))
uni$GPA[iNgpa18] = mean(uni$GPA[iYgpa18])
summary(uni$GPA)
#Checking for NA's in IL DATA
check4 <- subset(uni, uni$State == "IL")
summary(check4$GPA)
summary(uni$GPA)

##Female, White
FNgpa <- which(uni$Gender == "Female" & uni$Race == "White" & is.na(uni$GPA))
FYgpa <- which(uni$Gender == "Female" & uni$Race == "White" & !is.na(uni$GPA))
uni$GPA[FNgpa]=mean(uni$GPA[FYgpa])
summary(uni$GPA)

#Female, Black/African American
FNgpa2 <- which(uni$Gender == "Female" & uni$Race == "Black/African American" & is.na(uni$GPA))
FYgpa2 <- which(uni$Gender == "Female" & uni$Race == "Black/African American" & !is.na(uni$GPA))
uni$GPA[FNgpa2]=mean(uni$GPA[FYgpa2])
summary(uni$GPA)

#Female, Hispanic/Latino
FNgpa3 <- which(uni$Gender == "Female" & uni$Race == "Hispanic/Latino" & is.na(uni$GPA))
FYgpa3 <- which(uni$Gender == "Female" & uni$Race == "Hispanic/Latino" & !is.na(uni$GPA))
uni$GPA[FNgpa3]=mean(uni$GPA[FYgpa3])
summary(uni$GPA)

#Female, American Indian/Alaskan Native
FNgpa4 <- which(uni$Gender == "Female" & uni$Race == "American Indian/Alaskan Native" & is.na(uni$GPA))
FYgpa4 <- which(uni$Gender == "Female" & uni$Race == "American Indian/Alaskan Native" & !is.na(uni$GPA))
uni$GPA[FNgpa4]=mean(uni$GPA[FYgpa4])
summary(uni$GPA)

#Female, White
FNgpa <- which(uni$Gender == "Female" & uni$Race == "White" & is.na(uni$GPA))
FYgpa <- which(uni$Gender == "Female" & uni$Race == "White" & !is.na(uni$GPA))
uni$GPA[FNgpa]=mean(uni$GPA[FYgpa])
summary(uni$GPA)

#Female, Black/African American
FNgpa2 <- which(uni$Gender == "Female" & uni$Race == "Black/African American" & is.na(uni$GPA))
FYgpa2 <- which(uni$Gender == "Female" & uni$Race == "Black/African American" & !is.na(uni$GPA))
uni$GPA[FNgpa2]=mean(uni$GPA[FYgpa2])
summary(uni$GPA)

#Female, Hispanic/Latino
FNgpa3 <- which(uni$Gender == "Female" & uni$Race == "Hispanic/Latino" & is.na(uni$GPA))
FYgpa3 <- which(uni$Gender == "Female" & uni$Race == "Hispanic/Latino" & !is.na(uni$GPA))
uni$GPA[FNgpa3]=mean(uni$GPA[FYgpa3])
summary(uni$GPA)

#Female, American Indian/Alaskan Native
FNgpa4 <- which(uni$Gender == "Female" & uni$Race == "American Indian/Alaskan Native" & is.na(uni$GPA))
FYgpa4 <- which(uni$Gender == "Female" & uni$Race == "American Indian/Alaskan Native" & !is.na(uni$GPA))
uni$GPA[FNgpa4]=mean(uni$GPA[FYgpa4])
summary(uni$GPA)

#Male, White
MNgpa1 <- which(uni$Gender == "Male" & uni$Race == "White" & is.na(uni$GPA))
MYgpa1 <- which(uni$Gender == "Male" & uni$Race == "White" & !is.na(uni$GPA))
uni$GPA[MNgpa1]=mean(uni$GPA[MYgpa1])
summary(uni$GPA)

#Male, Black/African American
MNgpa2 <- which(uni$Gender == "Male" & uni$Race == "Black/African American" & is.na(uni$GPA))
MYgpa2 <- which(uni$Gender == "Male" & uni$Race == "Black/African American" & !is.na(uni$GPA))
uni$GPA[MNgpa2]=mean(uni$GPA[MYgpa2])
summary(uni$GPA)

#Male, Hispanic/Latino
MNgpa3 <- which(uni$Gender == "Male" & uni$Race == "Hispanic/Latino" & is.na(uni$GPA))
MYgpa3 <- which(uni$Gender == "Male" & uni$Race == "Hispanic/Latino" & !is.na(uni$GPA))
uni$GPA[MNgpa3]=mean(uni$GPA[MYgpa3])
summary(uni$GPA)

#Male, American Indian/Alaskan Native
MNgpa4 <- which(uni$Gender == "Male" & uni$Race == "American Indian/Alaskan Native" & is.na(uni$GPA))
MYgpa4 <- which(uni$Gender == "Male" & uni$Race == "American Indian/Alaskan Native" & !is.na(uni$GPA))
uni$GPA[MNgpa4]=mean(uni$GPA[MYgpa4])
summary(uni$GPA)

View(uni)

##Level of Income
low<-which(uni$HouseholdIncome<0.8*uni$Household.average.income)
uni["class"] <- NA
uni$class[low]="LOW"
unique(uni$class)

High<-which(uni$HouseholdIncome>1.2*uni$Household.average.income)
uni$class[High]="HIGH"
unique(uni$class)

Mid<-which(uni$HouseholdIncome<=1.2*uni$Household.average.income & uni$HouseholdIncome>=0.8*uni$Household.average.income)
uni$class[Mid]="MID"
unique(uni$class)

##Persona Modelling
#Applicant,FT,GPA,Class
APP<-subset(uni,uni$Status.1=="APPLICANT")
APP.FT<-subset(APP,APP$StudentType=="Full-time")
APP.FT.GPA<-subset(APP.FT,APP.FT$GPA>=3)
APP.FT.GPA.class1<-subset(APP.FT.GPA,APP.FT.GPA$class=="LOW")
nrow(APP.FT.GPA.class1)

APP.FT.GPA.class2<-subset(APP.FT.GPA,APP.FT.GPA$class=="MID")
nrow(APP.FT.GPA.class2)

APP.FT.GPA.class3<-subset(APP.FT.GPA,APP.FT.GPA$class=="HIGH")
nrow(APP.FT.GPA.class3)

#Prospect,FT,GPA,Class
Pro<-subset(uni,uni$Status.1=="PROSPECT")
Pro.FT<-subset(Pro,Pro$StudentType=="Full-time")
Pro.FT.GPA<-subset(Pro.FT,Pro.FT$GPA>=3)
Pro.FT.GPA.class1<-subset(Pro.FT.GPA,Pro.FT.GPA$class=="LOW")
nrow(Pro.FT.GPA.class1)

Pro.FT.GPA.class2<-subset(Pro.FT.GPA,Pro.FT.GPA$class=="MID")
nrow(Pro.FT.GPA.class2)

Pro.FT.GPA.class3<-subset(Pro.FT.GPA,Pro.FT.GPA$class=="HIGH")
nrow(Pro.FT.GPA.class3)

#Suspect,FT,GPA,Class
Sus<-subset(uni,uni$Status.1=="SUSPECT")
Sus.FT<-subset(Sus,Sus$StudentType=="Full-time")
Sus.FT.GPA<-subset(Sus.FT,Sus.FT$GPA>=3)
Sus.FT.GPA.class1<-subset(Sus.FT.GPA,Sus.FT.GPA$class=="LOW")
nrow(Sus.FT.GPA.class1)

Sus.FT.GPA.class2<-subset(Sus.FT.GPA,Sus.FT.GPA$class=="MID")
nrow(Sus.FT.GPA.class2)

Sus.FT.GPA.class3<-subset(Sus.FT.GPA,Sus.FT.GPA$class=="HIGH")
nrow(Sus.FT.GPA.class3)

#Part time,Distance
summary(uni$DistancetoCampus_miles)
PT<-subset(uni,uni$StudentType=="Part-Time")
nrow(PT)
PT.near<-subset(PT,PT$DistancetoCampus_miles<=62.32)
nrow(PT.near)

PT.far<-subset(PT,PT$DistancetoCampus_miles>62.32)
nrow(PT.far)

#Outstate,Female,Full time
OS<-subset(uni,uni$InState=="N")
OS.F<-subset(OS,OS$Gender=="Female")
OS.F.FT <- subset(OS.F,OS.F$StudentType == "Full-time")
nrow(OS.F.FT)

#undergrad students
prog <- which(!uni$Program == "")
prog.1 <- uni[prog,]
unique(UG.1$Program)
UG <- which(!prog.1$Program == "NON.UGAS" & !prog.1$Program == "NON.PNURS" & !prog.1$Program == "NON.PDHYG" & !prog.1$Program == "NON.BSCST.CPES" &
              !prog.1$Program == "NON.PCHRO"& !prog.1$Program == "NON.PNAMD" &!prog.1$Program == "NON.BSCST.REF"&
              !prog.1$Program == "NON.PREBUSN" &!prog.1$Program == "NON.PREPSYC"&
              !prog.1$Program == "NON.PREFM" &!prog.1$Program == "NON.PRECPIA" &!prog.1$Program == "NON.BSCST.INTLC" &!prog.1$Program == "NON.PRESCI" &
              !prog.1$Program == "NON.PREENGR" &!prog.1$Program == "NON.PREGENST" &!prog.1$Program == "NON.PREDSN" &!prog.1$Program == "NON.PCHR" &
              !prog.1$Program == "NON.PRECS" &!prog.1$Program == "NON.PREHUMN" &!prog.1$Program == "NON.PREDSN" &!prog.1$Program == "NON.PREHUSV" &
              !prog.1$Program == "NON.PREMUSC" &!prog.1$Program == "NON.SPCUG" &!prog.1$Program == "NON.BSCST.ENGR" &!prog.1$Program == "NON.PREMATH" &
              !prog.1$Program == "NON.BSCST.BUSN" & !prog.1$Program == "ND.NATMED" &!prog.1$Program == "MS.ELED.ELCT" &!prog.1$Program == "AA.BUAD" &
              !prog.1$Program == "AS.DHYG" &!prog.1$Program == "AS.GENST" &!prog.1$Program == "AA.FM")
UGG <- prog.1[UG,]
nrow(UGG)
unique(UGG$Program)
