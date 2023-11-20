#create project
#top right-> new project, browse, 
#put it in a folder on desktop or some known path with the project
#function read.table() with same name as in the 

StudDataset<-read.table("StudDataset.CSV", header=T, sep=",")



class(StudDataset)
ncol(StudDataset)
nrow(StudDataset)

df<-StudDataset[ , -c(1,ncol(StudDataset))]

#general summary
 str(df)
 
 #change name of coumns
 names(df) <- c("age", "height", "country", "city", "roommate", "travel", "km_travel", "stat_knowledge", "hours_stud", "use_project", "use_R", "R_confidence", "course_exc")

 #clean the data

 df$height

 #position of a specific data
 which(df$height<100) 
 df$height[which(df$height<100)]<-181
 
 df$height
 
 
 #country
 df$country
 class( df$country)
 df$country[ df$country=="ITALY"]<- "Italy"
 df$country[ df$country=="italy"]<- "Italy"
 df$country[ df$country=="Italy"]<- "Italy"
 df$country[ df$country=="SPAIN"]<- "Spain" 
 
 #transform factor in dataframe
 df$country<-as.factor(df$country)
 
 #roommate
 df$roommate[df$roommate>10]<-NA
 
 # columns from 6 to 10
 df[,6:10]
 
 # km_travel
 class( df$km_travel)
 as.numeric( df$km_travel)
 
 
 #create a vector with positions of incorrect files
 idx<-which(is.na(as.numeric( df$km_travel)))
 df$km_travel[df$km_travel=="600 metri"]<-0.6
 
 
 ####da appunti di pedro riguardare!!!
 
 
 df[df=="600 metri"] <- "0.6"
 df[df=="2km"] <- "2"
 df[df=="2.0"] <- "2"
 df[df=="1 km"] <- "1"
 df[df==">30km"] <- "35"
 df[df=="2 km"] <- "2"
 df[df=="2,5"] <- "2.5"
 df[df=="1,8"] <- "1.8"
 
 
 df$km_travel <- as.numeric(df$`km_travel`)
 
 
 as.factor(df$`stat_knowledge`)
 df$`stat_knowledge` <- factor(df$`stat_knowledge`,
                                         levels=c(1,2,3,4,5))
 df$`stat_knowledge`
 
 df$`hours_stud` <- as.numeric(df$`hours_stud`)
 
 df$`use_project`
 df$`use_R`
 
 ifelse(df$`use_project`=="Yes",TRUE,FALSE)
 ifelse(df$`use_R`=="Yes",TRUE,FALSE)
 
 df$`use_project` <- ifelse(df$`use_project`=="Yes",TRUE,FALSE)
 df$`use_R` <- ifelse(df$`use_R`=="Yes",TRUE,FALSE)
 
 
 class(df$`use_R`)
 class(df$`course_exc`)
 class(df$`stat_knowledge`)
 
 df$travel <- as.factor(df$travel)
 
 summary(df)
 
 table(df$country)
 table(df$country)/nrow(df)
 round(table(df$country)/nrow(df),2)
 round(table(df$country)/nrow(df),2)*100
 
 table(df$travel)
 
 mean(df$`km_travel`[df$travel=="bike"])
 mean(df$`km_travel`[df$travel=="train"])
 mean(df$`km_travel`[df$travel=="walk"])
 
 aggregate(df$`km_travel`,by=list(travel=df$travel),
           FUN=mean)
 
