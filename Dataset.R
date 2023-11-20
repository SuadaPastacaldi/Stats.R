# #Another homework  - not done in class
# ##ADD a VARIABLE
# #Suppose we want to create another variable starting from country
# #which is UE non UE
# ifelse((stud.df$country=="Italy" | stud.df$country=="Germany" |
#           stud.df$country=="Croatia" | stud.df$country=="Spain"), T, F)
# stud.df$eu <- ifelse((stud.df$country=="Italy" | stud.df$country=="Germany" |
#                         stud.df$country=="Croatia" | stud.df$country=="Spain"), TRUE, FALSE)
#
# cbind(stud.df$country, stud.df$eu)
#
# as.factor(stud.df$eu)


### Lecture 7 - module 2

df <- read.table("StudDataset.csv", header=T, sep=',', stringsAsFactors = T)
names(df) <- c("chron.info" ,"age", "height", "country", "city", "roommate", "travel", "km_travel", "stat_knowledge", "hours_stud", "use_project", "use_R", "R_confidence", "course.exc")

str(df)
df$country

##Frequency distribution
table(df$use_R)
table(df$roommate)
table(df$travel)
table(df$country)

#Grouped frequency distribution
#we take the variable km_travel and consider
range(df$km_travel)

cut(df$km_travel, breaks = c(0.5, 2, 8, 20,50, 180), labels = c("very close", "close",
                                                                "medium", "far", "very far"))

km_cat <- cut(df$km_travel, breaks = c(0.5, 2, 8, 20, 50, 180), labels = c("very close", "close",
                                                                           "medium", "far", "very far"))
table(km_cat)

ifelse(km_cat=="very close", "(0.5-2]", ifelse(km_cat=="close",  "(2-8]", ifelse(km_cat=="medium", "(8-20]",
                                                                                 ifelse(km_cat=="far", "(20-50]", ifelse(km_cat=="very far",  "(50-180]", NA)))))

km_cat_num <- ifelse(km_cat=="very close", "(0.5-2]", ifelse(km_cat=="close",  "(2-8]", ifelse(km_cat=="medium", "(8-20]",
                                                                                               ifelse(km_cat=="far", "(20-50]", ifelse(km_cat=="very far",  "(50-180]", NA)))))


#to add this variable to the dataset
df<- data.frame(df, km_cat, km_cat_num)
str(df)

#Bivariate frequency distribution

table(df$travel, df$km_cat)
table(df$use_R, df$stat_knowledge)


#Bar chart:
barplot(table(df$travel))
barplot(table(df$km_cat))
#########
######
# The most basic barplot you can do:
barplot(table(df$travel))

# COLORS
barplot(table(df$travel), col="red" )
barplot(table(df$travel), col="#E41E9C" )

# Specific color for each bar? Use a well known palette
library(RColorBrewer)
coul <- brewer.pal(5, "Set2")
coul <- brewer.pal(5, "Set1")

barplot(table(df$travel), col=c(1,2,3,4,5) )
barplot(table(df$travel), col=coul )

# Change border color
barplot(table(df$travel), border="#69b3a2", col="white" )

# axis label
barplot(table(df$travel), xlab="categories",
        ylab="values",  main="My title", ylim=c(0,40))

#horizontal barplot
barplot(table(df$travel), col="#69b3a2",  horiz=T)

barplot(table(df$travel), col="#69b3a2",  horiz=T, las=1)

# The las argument allows to change the orientation of the axis labels:
# 0: always parallel to the axis
# 1: always horizontal
# 2: always perpendicular to the axis
# 3: always vertical.
# This is specially helpful for horizontal bar chart.

# Changing names
barplot(table(df$travel), names.arg=c("group1","group2","group3","group4"), col="orange")

# Customize the labels:
# font.axis: font: 1: normal, 2: bold, 3: italic, 4: bold italic
# col.axis: color
# cex.axis: size
# Customize axis title:
#
# font.lab
# col.lab
# cex.lab


# Customize labels (left)
barplot(table(df$travel),
        names.arg=c("group1","group2","group3","group4"),
        font.axis=2,
        col.axis="orange",
        cex.axis=1.5
)

# Customize title (right)
barplot(table(df$travel),
        xlab="category",
        font.lab=2,
        col.lab="orange",
        cex.lab=2
)

# # If your group names are long, you need to: rotate them to avoid overlapping.
# # This is done with las
# # increase bottom margin size using the mar parameter of the par() function.
# # Four values are provided: bottom, left, top, right respectively.
# # Note: prefer a horizontal barplot in this case.


#####
########


#pie chart (not so much used)
pie(table(df$travel), main="Pie chart of Travel")

##Stacked barchart
f_tab1<-table(df$eu, df$use_R)/length(df$eu)
rownames(f_tab1)<- c('eu', 'noneu')
colnames(f_tab1)<- c('R.yes', 'R.no')

barplot(f_tab1, main = "Mutiple bar chart", xlab = "use R", names.arg =c('Yes', 'No'),
        ylab = "Frequency", col = c("red", "blue"), ylim=c(0,0.6),xlim=c(1,10),border=NA,  beside = TRUE,
        legend.text = TRUE, width=rep(1,4), args.legend = list(x = 9.3, y=0.3,
                                                               bty="n", border=F))
abline(h=0)


f_tab2<-table(df$km_cat, df$travel)/length(df$travel)

barplot(f_tab2, main = "Mutiple bar chart", xlab = "use R",
        ylab = "Frequency", col =brewer.pal(5, "Set2") , border=NA,  beside = TRUE,
        legend.text = TRUE, width=rep(1,7), args.legend = list(x = 9.3, y=0.36,
                                                               inset = c( 0, 0), bty="n", border=F))
abline(h=0)





#histogram
hist(df$age)
hist(df$height)

# With the breaks argument we can specify the number of cells we want in the histogram.
# However, this number is taken by R just a suggestion as it calculates the best number of cells,
# keeping this suggestion in mind.
hist(df$height, breaks = 15)




s <- c(19, 23, 26, 29, 33)
hist(df$age, breaks=s, xlim=c(19,33), right=F, col='lavenderblush',
     xlab="age", main='title', freq=F)

hist(df$age, breaks=s, xlim=c(19,33), right=F, xaxt='n', col='lavenderblush',
     yaxt='n', xlab="age", ylab = '', main='title', freq=F)
axis(1, tick=T, at = c(19, 23, 26, 29, 33))
axis(2, tick=T, at = c(0.00, 0.02, 0.04, 0.06, 0.07, 0.1, 0.15, 0.17))


#right = F (default) the right value of breaks is not included
#if right=T is is included
a <- hist(df$age, breaks=s, xlim=c(19,33), right=T, col=c(2,3,4, 5),
          xlab="age", main='title', freq=F)


abline(h=0.05, col="gray", lty=2, lwd=1.5)

plot('', xlim=c(18,34), ylim=c(0,.15))
abline(h=0.05, col="gray", lty=2, lwd=1.5)
abline(h=0.10, col="gray", lty=2, lwd=1.5)
abline(h=0.15, col="gray", lty=2, lwd=1.5)
plot(a, add=T)
text(a$mids,a$density,labels=round(a$density,2), adj=c(0.5, -0.5))

text(a$mids,a$density,labels=round(a$density,2), adj=c(0.5, -0.5))

str(a)

###Lists



#mean variance
mean(df$age)
mean(df$age^2)-mean(df$age)^2

median(df$age)
quantile(df$age)

quantile(df$age, p=0.4)


##boxplots
boxplot(df$age)

agelastyear <- round(rnorm(25, 23, 3))
a <- boxplot(df$age, agelastyear, main='boxplot of age', ann=T, names=c('age this year','age last year'))




mean(df$age*df$height)-mean(df$age)*mean(df$height)
cov(df$age,df$height)
cor(df$age,df$height)

cor(df$hours_stud, df$height, use="complete.obs")

(mean(df$age*df$height)-mean(df$age)*mean(df$height))/sqrt((mean(df$age^2)-mean(df$age)^2)*(mean(df$height^2)-mean(df$height)^2))

plot(df$age, df$height)

####
####
###Scatterplot
data(trees)
tdf <- trees

cor(tdf$Height, tdf$Volume)
cor(tdf$Height, tdf$Girth)


plot(tdf$Height, tdf$Volume)
plot(tdf$Height, tdf$Volume, xlab = "height", ylab="volume")
plot(tdf$Height, tdf$Volume, xlab = "height", ylab="volume", main="Tree", col="red")


plot(tdf$Height, tdf$Volume, xlab = "height", ylab="volume", main="Tree", col="red", xaxt='n', yaxt='n')
axis(1, tick=T, at = c(60,70,80))
axis(2, tick=T, at = c(20,40, 60, 80), font=2)



