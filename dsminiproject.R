#install.packages('readxl') 
if (!require("readxl")) install.packages("readxl")
library(readxl)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
#install.packages('dplyr')
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
#install.packages("ggplot2")
#install.packages("hexbin")
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
#install.packages("tidyverse")
library(readxl)
library(dplyr)
library(ggplot2)
library(hexbin)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("scales")) install.packages("scales")
library(scales)
if (!require("corrplot")) install.packages("corrplot")
library(corrplot)
library(tidyverse)
install.packages("caret")
library(caret)
install.packages("pROC")
library(pROC)
install.packages("Metrics")
library("Metrics")
if (!require("rpart")) install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
if (!require("rattle")) install.packages("rattle")
library(rattle)
install.packages("ROCR")
library(ROCR)
install.packages("tinytex")
tinytex::install_tinytex()




data_path1="D:/social_mental/data1.xlsx"
data1=read_excel(data_path1)
View(data1)
summary(data1)
times_used=data1$`Frequency of Social Media Interaction`
summary(times_used)

#check for null values in times_used 
null_values <-is.na(times_used)
num_null_values <- sum(null_values)
num_null_values  #0 null values

#data1 has 4 categorical values converting them into numbers 
category_mapping=c("Frequently"=4,"Very Often"=3,"Occasionally"=2,"Rarely"=1)
freq=category_mapping[times_used] 



#binding the newly created columns to the data1
data1_final <- cbind(data1,freq)
View(data1_final)

#-------------------------------------------------------------------------------------------------------------------------dataset-2
#frequency
data_path2="D:/social_mental/data2.xlsx"
data2=read_excel(data_path2)
View(data2)

k=data2$`How much time did you spend in Social media during the Lockdown?`
View(k)
#text to numbers
freq_mapping=c("Less than 2 hours"=1,"2-3 hours"=2,"3-6 hours"=4,"6-8 hours"=7)
freq=freq_mapping[k]
View(freq)
data2_final <- cbind(data2,freq)
View(data2_final)
#age
age_check=c("18-21 years old"=19,"22-30 years old"=26,"41-60 years old"=50,"31-40 years old"=35)
l=data2$`Age Range`
Age <-age_check[l]
data2_final <- cbind(data2_final,Age)
temp <- data.frame(data2_final$`How much time did you spend in Social media during the Lockdown?`,data2_final$freq)
View(temp)



#--------------------------------------------------------------------------------------------------------------------------dataset-3
data_3_path="D:/social_mental/data3.csv"
fre_mapping=c("Between 2 and 3 hours"=2,"Between 3 and 4 hours"=3,"More than 5 hours"=5,"Less than an Hour"=1,"Between 4 and 5 hours"=4,"Between 1 and 2 hours"=1) 
data3=read.csv(data_3_path) 
k<-data3$X8..What.is.the.average.time.you.spend.on.social.media.every.day.
#k
#p=data3$X9..How.often.do.you.find.yourself.using.Social.media.without.a.specific.purpose.
#data3_final <- subset(data3_final,select=-freq)
freq=fre_mapping[k]
data3_final <- cbind(data3,freq)
View(data3_final)
o=data3$X1..What.is.your.age.
colnames(data3_final)
data3_final$Age=data3$X1..What.is.your.age.
#gender
Gender=data3$X2..Gender
data3_final$Gender=data3$X2..Gender
write.csv(data3_final,file="D:/social_mental/data3_final.csv",row.names = FALSE)


#-------------------------------------------------------------------------------------------------creating for age,gender,frequency watched
gaf1=data.frame(data1_final$Age,data1_final$Gender,data1_final$freq)
View(gaf1)
names(gaf1)<- c('age', 'gender', 'freq')
gaf2=data.frame(data2_final$Age,data2_final$Gender,data2_final$freq)
View(gaf2)
names(gaf2)<- c('age', 'gender', 'freq')
gaf3=data.frame(data3_final$Age,data3_final$Gender,data3_final$freq)
names(gaf3)<- c('age', 'gender', 'freq')
View(gaf3)
GAF <- rbind(gaf1,gaf2,gaf3)
View(GAF)
GAF_filtered <- GAF[GAF$freq == x, ]
#saving to local device
write.csv(GAF_filtered,file="D:/social_mental/my_dataset.csv",row.names = FALSE)
#print(c)
x <- max(GAF_filtered$freq)
cat("maX frequency:", x, "\n")

View(GAF_filtered)
# Filter the dataset to get only the rows with the highest frequency count
highest_freq_data <- subset(GAF_filtered, freq == x)
View(highest_freq_data)

# Count the number of males and females with the highest frequency count
num_males <- sum(highest_freq_data$gender == "Male")
num_females <- sum(highest_freq_data$gender == "Female")

# Print the number of males and females with the highest frequency count
print(paste("Number of males with the highest frequency count:", num_males))
print(paste("Number of females with the highest frequency count:", num_females))
print(x)
h_f_c<-data.frame(num_males,num_females)
View(h_f_c)
write.csv(h_f_c,file="D:/social_mental/hfc.csv",row.names = FALSE)



#--------------------------------------------------------------------------------------------------count of usage of social media 
app_use=data2$`What Social Media Platforms do you use?`
View(app_use)
app_usage=data.frame(app_use)
View(app_usage)
app_usage <- app_usage$app_use
#View(app_usage)
# Define the values and initialize the count vector
arr <- c("Facebook", "Whatsapp", "Instagram", "Snapchat", "Telegram", "Discord", "YouTube")
count_vector <- rep(0, length(arr))
names(count_vector) <- arr
#--------------------------------------------------------------------------------------counting no of app users
# Iterate over each value in the DataFrame "df"
for (i in app_usage) {
  # Iterate over each value in the array "arr"
  apps_used <- unlist(strsplit(as.character(i), ", "))
  for (j in 1:length(arr)) {
    # Check if the app is present in the value "i"
    if (arr[j] %in% apps_used) {
      # Increment the count for the corresponding app
      count_vector[j] <- count_vector[j] + 1
    }
  }
}

# Print the counts
for (app in names(count_vector)) {
  print(paste(app, ":", count_vector[app]))
}

app_data=data.frame(count_vector)
View(app_data)
write.csv(app_data,file="D:/social_mental/app_user_count.csv",row.names = TRUE)


#--------------------------------------------------------------------------------------------------------------gender app_usage
data_app_gender <- data.frame(Gender = data2$Gender, app_usage = app_usage)
View(data_app_gender)
arr <- c("Facebook", "Whatsapp", "Instagram", "Snapchat", "Telegram", "Discord", "YouTube")
genders <- unique(data_app_gender$Gender)
count_matrix <- matrix(0, nrow = length(genders), ncol = length(arr), dimnames = list(genders, arr))

# Iterate over each value in the data frame "data_app_gender"
for (i in 1:nrow(data_app_gender)) {
  apps_used <- unlist(strsplit(as.character(data_app_gender$app_usage[i]), ", "))
  gender <- data_app_gender$Gender[i]
  for (j in 1:length(arr)) {
    if (arr[j] %in% apps_used) {
      count_matrix[gender, arr[j]] <- count_matrix[gender, arr[j]] + 1
    }
  }
}

# Print the counts for each gender and platform
print(count_matrix["Male",])
print(count_matrix["Female",])
print(count_matrix)
gender_app_freq=data.frame(count_matrix)
View(gender_app_freq)
write.csv(gender_app_freq,file="D:/social_mental/gender_app_freq.csv",row.names = TRUE)

  #-------------------------------------------------------------------------------------------------------------------------------------Age freq 
age_freq1=cbind(data1_final$Age,data1_final$freq)
age_freq2=cbind(data2_final$Age,data2_final$freq)
age_freq3=cbind(data3_final$Age,data3_final$freq)
age_freq=rbind(age_freq1,age_freq2,age_freq3)
View(age_freq)
colnames(age_freq)<-c("Age","freq")
write.csv(age_freq,file="D:/social_mental/age_freq.csv",row.names = TRUE)
#---------------------------------------------------------------------------------------------------What is the overall impact of social media usage on mental health
a_g_m_1=data.frame(data1$Age,data1$Gender,data1$`Impact on Mental Health (Score)`)
View(a_g_m_1)
assign_label <- function(value){
  if(value<temp_1){
    return ("negative")
    
  }
  else{
    return ("positive")
  }
}
temp_1 <- mean(a_g_m_1$data1..Impact.on.Mental.Health..Score..)#------------------------------------------calculated mean value for the data and then evaluated dataset1
temp_1 
a_g_m_1$mental_health=sapply(a_g_m_1$data1..Impact.on.Mental.Health..Score..,assign_label)
View(a_g_m_1)
a_g_m_1_final <- a_g_m_1[,c("data1.Age","data1.Gender","mental_health")]
colnames(a_g_m_1_final) <-c("age","gender","mental_health")
View(a_g_m_1_final)

a_g_m_2=data.frame(data2_final$Age,data2_final$Gender,data2$`Have you faced a Mental health Issue?`)
View(a_g_m_2)
assign_label2 <- function(value){
  if(value=="Yes"){
    return ("positive")
  }
  else if(value=="No"){
    return ("negative")
  }
  else{
    return("NA")
  }
}
a_g_m_2$mental_health<- sapply(data2$`Have you faced a Mental health Issue?`, assign_label2)
View(a_g_m_2)
#remove na values 
a_g_m_2_final <-na.omit(a_g_m_2)
View(a_g_m_2_final)
a_g_m_2_final<- a_g_m_2[a_g_m_2$mental_health%in% c("positive", "negative"), ]
a_g_m_2_final <- a_g_m_2_final[,c("data2_final.Age","data2_final.Gender","mental_health")]
colnames(a_g_m_2_final) <- c("age", "gender", "mental_health")
View(a_g_m_2_final)

a_g_m_3=data.frame(data3$X1..What.is.your.age.,data3$X2..Gender,data3$X18..How.often.do.you.feel.depressed.or.down.)
View(a_g_m_3)
assign_label3 <- function(value){
  if(value>temp_3){
    return("positive")
  }
  else{
    return("negative")
  }
}
temp_3 = mean(data3$X18..How.often.do.you.feel.depressed.or.down.)#------------------------------------------------------------------------calculated mean value for the dataset3
temp_3
colnames(a_g_m_3)<-c("age","gender","mental_health")
a_g_m_3$mental_health=sapply(a_g_m_3$mental_health,assign_label3)
View(a_g_m_3)
a_g_m_3_final=data.frame(a_g_m_3$age,a_g_m_3$gender,a_g_m_3$mental_health)
View(a_g_m_3_final)
colnames(a_g_m_3_final) <- c("age","gender","mental_health")
data3_final$mental_health=a_g_m_3_final$mental_health
View(data3_final)
Age_gender_mental <- rbind(a_g_m_1_final,a_g_m_2_final,a_g_m_3_final) 
View(Age_gender_mental)
#-----------------------------------------------------------------------------------------------------taking male and female values
Age_gender_mental_final <- Age_gender_mental[Age_gender_mental$gender %in% c("Male", "Female"), ]
View(Age_gender_mental_final) 
summary(Age_gender_mental)
unique(Age_gender_mental)
write.csv(Age_gender_mental_final,file="D:/social_mental/Age_gender_mental.csv",row.names = FALSE)
unique_values<- unique(Age_gender_mental_final$mental_health)
print(length(unique_values))
unique_values
count_mental_health <- table(Age_gender_mental$mental_health)
count_mental_health
count_male_mental_health <- table(subset(Age_gender_mental, gender == "Male")$mental_health)
print("Counts of Mental Health for Males:")
print(count_male_mental_health)
cc<-data.frame(count_male_mental_health)
View(cc)
count_female_mental_health <- table(subset(Age_gender_mental, gender == "Female")$mental_health)
cc1<-data.frame(count_female_mental_health)
View(cc1)
merged_counts <- cbind(male_positive = count_male_mental_health["positive"],
                       male_negative = count_male_mental_health["negative"],
                       female_positive = count_female_mental_health["positive"],
                       female_negative = count_female_mental_health["negative"])
View(merged_counts)
gender_counts <- data.frame(
  gender = c("Male", "Female"),
  positive = c(count_male_mental_health["positive"], count_female_mental_health["positive"]),
  negative = c(count_male_mental_health["negative"], count_female_mental_health["negative"])
)
View(gender_counts)
write.csv(gender_counts,file="D:/social_mental/gender_counts.csv",row.names = FALSE)

ggplot(Age_gender_mental_final, aes(x = age, fill = mental_health)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("positive" = "blue", "negative" = "red")) +
  labs(title = "Density Plot of Mental Health by Age", x = "Age", y = "Density", fill = "Mental Health")


ggplot(Age_gender_mental_final, aes(x = age, fill = mental_health)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("positive" = "blue", "negative" = "red")) +
  labs(title = "Density Plot of Mental Health by Age", x = "Age", y = "Density", fill = "Mental Health") +
  facet_wrap(~ gender)
#--------------------------------------------------------------------------------------gender proportion
gender_proportions <- Age_gender_mental_final %>%
  group_by(gender) %>%
  summarise(prop_positive = mean(mental_health == "positive"))

# Print proportions by gender
print(gender_proportions)
g_p<-data.frame(gender_proportions)
View(g_p)
write.csv(g_p,file="D:/social_mental/g_p.csv",row.names = FALSE)
# Plot pie chart
pie(g_p$prop_positive, labels = paste0(g_p$gender, ": ", round(g_p$proppositive * 100, 2), "%"), col = c("lightblue", "lightgreen"), main = "Proportion of Positive Mental Health by Gender")
#------------------------------------------------------------------------------------------------------age proportions
age_proportions <- Age_gender_mental_final %>%
  group_by(age) %>%
  summarise(prop_positive = mean(mental_health == "positive"))
#mean of all the values based 

print(age_proportions)
age_prop<-data.frame(age_proportions)
View(age_prop)
write.csv(age_prop,file="D:/social_mental/age_prop.csv",row.names = FALSE)
# Define age ranges
age_ranges <- cut(age_proportions$age, breaks = c(0, 20, 30, 40, 50, 60, max(age_proportions$age)), labels = c("0-20", "21-30", "31-40", "41-50", "51-60", "61+"), include.lowest = TRUE)

# Aggregate data by age range
age_range_proportions <- aggregate(prop_positive ~ age_range, 
                                   data = data.frame(age_range = age_ranges, prop_positive = age_proportions$prop_positive), 
                                   FUN = mean)

unique_levels <- unique(age_prop$prop_positive)

# Generate a color palette with sufficient colors
color_palette <- rainbow(length(unique_levels))

# Create the density plot with the correct color mapping
#ggplot(age_prop, aes(x = age, fill = factor(prop_positive))) +
 # geom_density(alpha = 0.5) +
  #scale_fill_manual(values = color_palette) +
  #labs(title = "Density Plot of Age by Prop Positive",
  #     y = "Density",
    #   fill = "Prop Positive") +
 # theme_minimal()

#-------------------------------------------------------------------------------------------------------------------------------------------Age gender relationship 
pd=data.frame(data3_final$X3..Relationship.Status,data3_final$Age,data3_final$Gender,data3_final$X7..What.social.media.platforms.do.you.commonly.use.)
View(pd)
colnames(pd)=c("relationshipStatus","Age","Gender","App")
write.csv(pd,file="D:/social_mental/age_gender_relation_app.csv",row.names = FALSE)
summary(pd)
unique(pd$Gender)
unique(pd$relationshipStatus)   #---------------------------------------------------------------------------------------------------------------relationship status
pd <- pd[pd$Gender %in% c("Male", "Female", "Trans"), ]
#-----------------------------------------------------------------------------------single
filtered_pd <- pd[pd$relationshipStatus %in% c("Single"), ]
val<-mean(filtered_pd$Age)
View(filtered_pd)

data <- subset(pd,relationshipStatus=="Single")
apps <- strsplit(trimws(data$App),",")
View(data)
apps <- unlist(apps)
app_counts <- table(apps)
app_counts
sorted_counts_single <- sort(app_counts,decreasing = TRUE)
print(sorted_counts_single)
n_s<-nrow(filtered_pd)
sorted_counts_single<-sorted_counts_single/n_s
print(sorted_counts_single)
write.csv(sorted_counts_single,file="D:/social_mental/single_app.csv",row.names = FALSE)

#--------------------------------------------------------In a relationship
filtered_pd_relation <- pd[pd$relationshipStatus %in% c("In a relationship"), ]
val_relationship<-mean(filtered_pd_relation$Age)
val_relationship

data_relationship <- subset(pd,relationshipStatus=="In a relationship")
apps <- strsplit(trimws(data_relationship$App),",")
View(apps)
apps_relation <- unlist(apps)
app_counts_relation <- table(apps_relation )
app_counts_relation
sorted_counts_relation <- sort(app_counts_relation,decreasing = TRUE)
n_r<-nrow(filtered_pd_relation)
sorted_counts_relation <-sorted_counts_relation/n_r
write.csv(sorted_counts_relation,file="D:/social_mental/relation_app.csv",row.names = FALSE)




#------------------------------------------------------------------Married
filtered_pd_marrige <- pd[pd$relationshipStatus %in% c("Married"), ]
View(filtered_pd_marrige)
val_marrige<-mean(filtered_pd_marrige$Age)
val_marrige
data_marrige <- subset(pd,relationshipStatus=="Married")
apps_marrige <- strsplit(trimws(data_marrige$App),",")
View(apps_marrige)
apps_marrige<- unlist(apps_marrige)
app_counts_marrige <- table(apps_marrige)
sorted_counts_marrige <- sort(app_counts_marrige,decreasing = TRUE)
n_m<-nrow(filtered_pd_marrige)
sorted_counts_marrige<-sorted_counts_marrige/n_m
print(sorted_counts_marrige)
write.csv(sorted_counts_marrige,file="D:/social_mental/married_app.csv",row.names = FALSE)


#-------------------------------------------------------------------Divorced
filtered_pd_Divorced <- pd[pd$relationshipStatus %in% c("Divorced"), ]
View(filtered_pd_Divorced)
val_Divorced<- mean(filtered_pd_Divorced$Age)
val_Divorced
data_Divorced <- subset(pd,relationshipStatus=="Divorced")
apps_Divorced <- strsplit(trimws(data_Divorced$App),",")
apps_Divorced<- unlist(apps_Divorced)
app_counts_Divorced <- table(apps_Divorced)
sorted_counts_Divorced <- sort(app_counts_Divorced,decreasing = TRUE)
print(sorted_counts_Divorced)
num_val<-nrow(filtered_pd_Divorced)
sorted_counts_Divorced <- sorted_counts_Divorced / num_val
df_sorted_counts_Divorced <- as.data.frame(sorted_counts_Divorced)
print(num_val)
View(df_sorted_counts_Divorced)
write.csv(df_sorted_counts_Divorced,file="D:/social_mental/divorced_app.csv",row.names = FALSE)

#--------------------------------------------------------------------------------------------------------------relationship status mental health 
Age_gen_re_men=data.frame(data3$X1..What.is.your.age.,data3$X3..Relationship.Status,data3$X2..Gender,data3$X18..How.often.do.you.feel.depressed.or.down.)
View(Age_gen_re_men)
colnames(Age_gen_re_men) = c("age","rstatus","Gender","mental_health")
Age_gen_re_men$mental_health <- sapply(Age_gen_re_men$mental_health,assign_label3)
write.csv(Age_gen_re_men,file="D:/social_mental/Age_gen_re_men.csv",row.names = FALSE)
single<-subset(Age_gen_re_men,rstatus=="Single")
count_single<-nrow(single)
count_single
relation<-subset(Age_gen_re_men,rstatus=="Single")
count_single<-nrow(single)
count_single
relation <- subset(Age_gen_re_men,rstatus=="In a relationship")
count_relation <- nrow(relation)
count_relation
filtered_data <- subset(Age_gen_re_men, rstatus == "Single" & mental_health== "positive")
View(filtered_data)
num_single_posiitve<- nrow(filtered_data)
sr=num_single_posiitve/count_single#------------------------------------------------------------------------------------------singles ratio
filtered_data_relation <- subset(Age_gen_re_men, rstatus == "In a relationship" & mental_health== "positive")
View(filtered_data_relation)
num_relation_posiitve<- nrow(filtered_data_relation)
rr=num_relation_posiitve/count_relation#-------------------------------------------------------------------------------------relation ratio
filtered_data_married <-subset(Age_gen_re_men,rstatus=="Married")
count_married <- nrow(filtered_data_married)
print(count_married)
filtered_data_married <- subset(Age_gen_re_men,rstatus=="Married" & mental_health=="positive")
mr=nrow(filtered_data_married)/count_married#-----------------------------------------------------------------------------------Married
filtered_divorced <- subset(Age_gen_re_men,rstatus=="Divorced")
count_divorced <- subset(Age_gen_re_men,rstatus=="Divorced" & mental_health=="positive")
d_count<-nrow(filtered_divorced)
d_count
dr=nrow(count_divorced)/nrow(filtered_divorced)#--------------------------------------------------------------------------------Divorced
ratios_df <-data.frame(
  "Relationship status"= c("Single", "In a relationship", "Married", "Divorced"),
  "Ratio"=c(sr,rr,mr,dr)
)
View(ratios_df)
write.csv(ratios_df,file="D:/social_mental/ratios_df.csv",row.names = FALSE)

#------------------------------------------------------------------------------------------------------------------------------freq
freq_married=subset(data3_final,X3..Relationship.Status=="Married")
View(freq_married)
mean_freq_married=mean(freq_married$freq)
mean_freq_married
View(data3_final)
colnames(data3_final)<-c("Timestamp","Age","Gender","relation","Occupation","Organisation","usage","Apps","freq_range","social_media_purpose","busy_distraction","restless","distraction","worries","concentrate","compare","general","seek_validation","feel_depressed","intrest","sleep_issues","freq","mental_health")

# 1. Create the new data frame (freq_re) with renamed columns
freq_re <- data.frame(relation = data3_final$relation, freq = data3_final$freq)

# 2. View the data frame (optional)
View(freq_re)

# 3. Calculate summary statistics by relation
relation_summary <- freq_re %>%
  group_by(relation) %>%
  summarize(
    Mean_Freq = mean(freq),  # Use 'freq' from the renamed column
    Highest_Freq = max(freq)  # Use 'freq' from the renamed column
  )

# 4. Print the results (now Mean_Freq is defined)
print(relation_summary)
category_counts<-table(freq_re)
m_div<- nrow(subset(freq_re,relation=="Divorced" & freq==max(data3_final$freq)))
m_div<-m_div/d_count
m_div
m_sin<- nrow(subset(freq_re,relation=="Single" & freq==max(data3_final$freq)))
m_sin<-m_sin/count_single
m_sin
m_rel<-nrow(subset(freq_re,relation=="In a relationship" & freq==max(data3_final$freq)))
m_rel<-m_rel/count_relation
m_rel
m_married<- nrow(subset(freq_re,relation=="Married" & freq==max(data3_final$freq)))
m_married<-m_married/count_married
m_married
r_fre<-data.frame(relation=c("Married","Divorced","In a relation","Single"),ratio=c(m_married,m_div,m_rel,m_sin))
r_fre
write.csv(r_fre,file="D:/social_mental/ratios_freq.csv",row.names = FALSE)

print(max(data3_final$freq))

#----------------------------------------------------------------------------------------------------------------------------------building a model
colnames(data3_final)
data3_final <- data3_final[, !colnames(data3_final) %in% "X1..What.is.your.age."]
data3_final <- data3_final[, !colnames(data3_final) %in% "X2..Gender"]
View(data3_final)
colnames(data3_final)<-c("Timestamp","relation","Occupation","Organisation","usage","Apps","freq_range","social_media_purpose","busy_distraction","restless","distraction","worries","concentrate","compare","general","seek_validation","feel_depressed","intrest","sleep_issues","freq","Age","Gender","mental_health")
main_data<-data.frame(data3_final$Age,data3_final$Gender,data3_final$relation,data3_final$Apps,data3_final$distraction,data3_final$concentrate,data3_final$freq,data3_final$mental_health)
valid_genders <- c("Male", "Female", "Trans")
main_data <- main_data %>%filter(Gender %in% valid_genders)
View(main_data)
colnames(main_data) <- c("Age", "Gender", "Relation", "Apps", "Distraction", "Concentrate", "Frequency", "Mental_Health")
main_data$num_apps<-sapply(strsplit(main_data$Apps,","),length)
cor_matrix <- cor(main_data[, c("Age", "Distraction", "Concentrate", "Frequency", "num_apps")]) 

# View the correlation matrix
print(cor_matrix)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust")


main_data <- na.omit(main_data)
main_data$Gender <- factor(main_data$Gender, levels = c("Male", "Female","Trans"), labels = c(1, 0,2))
main_data$Relation<-factor(main_data$Relation,levels = c("In a relationship","Single","Married","Divorced"),labels = c(1,2,3,4))
main_data$Mental_Health<-factor(main_data$Mental_Health,levels=c("positive","negative"),labels = c(1,0))
View(main_data)
X<-main_data[,c("Age","Gender","Relation","Distraction","Concentrate","Frequency","num_apps")]
Y<-as.numeric(main_data$Mental_Health)-1
str(main_data$Mental_Health)  #giv einternal structure of the mental health
model <- glm(Mental_Health ~ Age + Gender + Relation + Distraction + Concentrate + Frequency + num_apps, data = main_data, family = binomial)
summary(model$coefficients)
model$residuals
model$rank


# Create confusion matrix
conf_matrix <- table(predict(model, type = "response") > 0.5, Y)
print("Confusion Matrix:")
print(conf_matrix)



#---------------------------------------------------------------train_test_data
set.seed(123)
train_index <- createDataPartition(main_data$Mental_Health, p = 0.8, list = FALSE)
train_data <- main_data[train_index, ]
test_data <- main_data[-train_index, ]

Y_train <- as.numeric(train_data$Mental_Health) - 1
Y_test <- as.numeric(test_data$Mental_Health) - 1


formula <- as.formula("Mental_Health ~ Age + Gender + Relation + Distraction + Concentrate + Frequency + num_apps")
model <- glm(formula, data = train_data, family = binomial)
predicted_values <- predict(model, newdata = test_data, type = "response")
predicted_values
predicted_classes <- ifelse(predicted_values > 0.5, 1, 0)
predicted_classes
accuracy <- mean(predicted_classes == Y_test)
print(paste("Accuracy:", accuracy))

#----------------------------------------------------------------------------------------------------------confusion matrix
conf_matrix <- confusionMatrix(factor(predicted_classes, levels = c(0, 1)), factor(Y_test, levels = c(0, 1)))
conf_matrix

precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
misclassification_rate <- sum(conf_matrix[1, 2], conf_matrix[2, 1]) / sum(conf_matrix)
print(paste("Misclassification Rate:", round(misclassification_rate, 4)))



#------------------------------------------------------------------metrics
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))

Y_test
predicted_values
roc_curve <- roc(Y_test, predicted_values)
print(roc_curve)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "red")
legend("topright", legend = c("ROC Curve", "Diagonal Reference"), col = c("blue", "red"), lty = 1:2, cex = 0.8)
#auc_score <- auc(roc_curv)
#print(paste("AUC-ROC:", auc_score))

#log_likelihood <- logLik(model$finalModel)
#print(paste("Log-Likelihood:", log_likelihood))
cross_entropy <- -mean(ifelse(Y_test == 1, log(predicted_values), log(1 - predicted_values)))
print(paste("Cross-Entropy Loss:", cross_entropy))
#  cor(y_actual,y_predict)^2
#}
#LR_R = RSQUARE(as.numeric(test_data),as.numeric(predicted_values)) 

#------------precision recall curve
install.packages("PRROC")
library(PRROC)

precision_recall <- pr.curve(scores.class0 = predicted_values, weights.class0 = Y_test, curve = TRUE)
plot(precision_recall, main = "Precision-Recall Curve", col = "blue", lwd = 2)
abline(h = precision_recall$auc.integral, col = "red", lty = 2)
legend("bottomleft", legend = c("Precision-Recall Curve", "AUC-PR"), col = c("blue", "red"), lty = 1:2, cex = 0.8)


# Extract coefficients
coefficients_summary <- summary(model)$coefficients

# Extract residuals
residuals_summary <- summary(model)$residuals
residuals_summary
# Extract the call used to create the model
call_summary <- summary(model)$call

# Print coefficients summary
print("Coefficients Summary:")
print(coefficients_summary)

# Print residuals summary
print("Residuals Summary:")
print(residuals_summary)

# Print call summary
print("Call Summary:")
print(call_summary)

# Predict values
# Assuming 'new_data' is the data you want to make predictions on
predictions <- predict(model, newdata = new_data)

# Print predicted values
print("Predicted Values:")
print(predictions)

df_resid <- model$df.residual

# Print residual degrees of freedom
#number of observations minus the number of parameters estimated in the model.
print(paste("Residual Degrees of Freedom:", df_resid))

#install.packages("WVPlots")

#library(WVPlots)

#main_data$Mental_Health_labels <-factor(main_data$Mental_Health, labels = c("negative", "positive"))
#DoubleDensityPlot(train_data,"Distraction","Concentrate",title = "plot")
#DoubleHistogramPlot(train_data,"Distraction","Concentrate",title = "plot")

precision <- conf_matrix$byClass["Pos Pred Value"]

# Calculate Prevalence
prevalence <- sum(Y_test == 1) / length(Y_test)
enrichment <- precision / prevalence
enrichment
#If the enrichment is greater than 1, it indicates that the model is better than random at identifying positive cases, while if it's less than 1, it indicates the model is worse than random.
prevalence
coefficients(model)


predicted_numeric<-as.numeric(predicted_values)
Y_test_glm_numeric<-as.numeric(Y_test)
# Calculate RMSE
rmse <- sqrt(mean((predicted_numeric - Y_test_glm_numeric)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

rootsquareerror<-sqrt(rmse)
rootsquareerror

# Calculate Mean Squared Error (MSE)
mse <- mean((predicted_numeric - Y_test_glm_numeric)^2)
cat("Mean Squared Error (MSE):", mse, "\n")

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(test_pred_numeric - Y_test_numeric))
cat("Mean Absolute Error (MAE):", mae, "\n")

recall <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Recall:", recall))
print(paste("Specificity:", specificity))
conf_matrix



#--------------------------------------------------------------------------------------------------decision tree
formula <- Mental_Health ~ Age + Gender + Relation + Distraction + Concentrate + Frequency
fit <- rpart(formula,data=main_data,cp=0.01)
print(fit)
summary(fit$variable.importance)

train_index <- createDataPartition(main_data$Mental_Health, p = 0.8, list = FALSE)
train_data <- main_data[train_index, ]
test_data <- main_data[-train_index, ]

Y_train <- as.numeric(train_data$Mental_Health) - 1
Y_test <- as.numeric(test_data$Mental_Health) - 1

train_pred <- predict(fit, newdata = train_data, type = "class")

# Predicting on testing data
test_pred <- predict(fit, newdata = test_data, type = "class")
test_pred



train_accuracy <- mean(train_pred == Y_train)
cat("Training Accuracy:", train_accuracy, "\n")

# Calculate accuracy for testing data
test_accuracy <- mean(test_pred == Y_test)
cat("Testing Accuracy:", test_accuracy, "\n")
confusion_test <- table(Actual = Y_test, Predicted = test_pred)
confusion_test
precision_test <- confusion_test[2, 2] / sum(confusion_test[, 2])
recall_test <- confusion_test[2, 2] / sum(confusion_test[2, ])
f1_score_test <- 2 * precision_test * recall_test / (precision_test + recall_test)
cat("Testing Precision:", precision_test, "\n")
cat("Testing Recall:", recall_test, "\n")
cat("Testing F1 Score:", f1_score_test, "\n")
prevalence <- sum(Y_test == 1) / length(Y_test)
enrichment <- precision_test / prevalence
enrichment

predictions <- predict(fit, test_data, type = "prob")
#---------------------------------------------------------------------------------------check
#prediction_obj <- prediction(predictions[,2], Y_test)

# Create ROC curve
roc_curve <- performance(prediction_obj, "tpr", "fpr")

# Plot ROC curve
plot(roc_curve, main = "ROC Curve for Decision Tree Model", col = "blue", lwd = 2, 
     xlab = "False Positive Rate (1 - Specificity)", ylab = "True Positive Rate (Sensitivity)",
     xlim = c(0, 1), ylim = c(0, 1))
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)
legend("bottomright", legend = c("ROC Curve", "Random Classifier"), 
       col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 2))


misclassification_rate <- sum(confusion_test[1, 2], confusion_test[2, 1]) / sum(confusion_test)
print(paste("Misclassification Rate:", round(misclassification_rate, 4)))

library(rpart)
library(rpart.plot)

# Fit the decision tree
formula <- Mental_Health ~ Age + Gender + Relation + Distraction + Concentrate + Frequency
fit <- rpart(formula, data = main_data, cp = 0.01)

# Plot the decision tree
rpart.plot(fit, type = 4, extra = 101, under = TRUE, cex = 0.8, box.palette = "auto")
summary(fit)

# Convert factors to numeric
test_pred_numeric <- as.numeric(as.character(test_pred))
Y_test_numeric <- as.numeric(as.character(Y_test))

# Calculate RMSE
rmse <- sqrt(mean((test_pred_numeric - Y_test_numeric)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

rootsquareerror<-sqrt(rmse)
rootsquareerror

# Calculate Mean Squared Error (MSE)
mse <- mean((test_pred_numeric - Y_test_numeric)^2)
cat("Mean Squared Error (MSE):", mse, "\n")

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(test_pred_numeric - Y_test_numeric))
cat("Mean Absolute Error (MAE):", mae, "\n")

# Plot predicted vs actual values
#plot(Y_test_numeric, test_pred_numeric,
 #    main = "Actual vs Predicted Values",
#     xlab = "Actual Values",
#     ylab = "Predicted Values",
#     col = "blue",
#     pch = 19)

# Add a diagonal line for reference
#abline(0, 1, col = "red")

# Add legend
#legend("topleft", legend = "Diagonal Line", col = "red", lty = 1)
install.packages("PRROC")
library(PRROC)
cross_entropy <- -mean(ifelse(Y_test == 1, log(predictions), log(1 - predictions)))
print(paste("Cross-Entropy Loss:", cross_entropy))
pr <- pr.curve(scores.class0 = predictions, weights.class0 = Y_test, curve = TRUE)

# Plot the Precision-Recall curve
plot(pr, main = "Precision-Recall Curve", col = "blue", lwd = 2)
abline(h = pr$auc.integral, col = "red", lty = 2)
legend("bottomleft", legend = c("Precision-Recall Curve", "AUC-PR"), col = c("blue", "red"), lty = 1:2, cex = 0.8)

# Check for missing values
if (any(is.na(predictions)) || any(is.na(Y_test))) {
  stop("Missing values found in predictions or Y_test.")
}

# Check data types
if (!is.numeric(predictions) || !is.numeric(Y_test)) {
  stop("Predictions and Y_test must be numeric vectors.")
}

# Check if Y_test contains only binary labels
if (!all(Y_test %in% c(0, 1))) {
  stop("Y_test must contain only binary labels (0s and 1s).")
}

# Print predictions and Y_test for debugging
print(head(predictions))
print(head(Y_test))

# Generate the Precision-Recall curve using the PRROC package
pr <- pr.curve(scores.class0 = predictions, weights.class0 = Y_test, curve = TRUE)

# Plot the Precision-Recall curve
plot(pr, main = "Precision-Recall Curve", col = "blue", lwd = 2)
abline(h = pr$auc.integral, col = "red", lty = 2)
legend("bottomleft", legend = c("Precision-Recall Curve", "AUC-PR"), col = c("blue", "red"), lty = 1:2, cex = 0.8)



#------------------------------------------------------------------------------------------------------------------------------------hybrid-model
library(rpart)
library(caret)
set.seed(123)
train_index <- createDataPartition(main_data$Mental_Health, p = 0.8, list = FALSE)
train_data <- main_data[train_index, ]
test_data <- main_data[-train_index, ]

formula_logistic <- Mental_Health ~ Age + Gender + Relation + Distraction + Concentrate + Frequency + num_apps
formula_tree <- Mental_Health ~ Age + Gender + Relation + Distraction + Concentrate + Frequency

# Fit logistic regression model
model_logistic <- glm(formula_logistic, data = train_data, family = binomial)

# Fit decision tree model
fit_tree <- rpart(formula_tree, data = train_data, cp = 0.01)

# Predict probabilities using logistic regression model
predicted_probabilities_logistic <- predict(model_logistic, newdata = test_data, type = "response")

# Predict classes using decision tree model
predicted_classes_tree <- predict(fit_tree, newdata = test_data, type = "class")

predicted_classes_tree <- as.numeric(as.character(predicted_classes_tree))

# Combine predictions
hybrid_predictions <- ifelse(predicted_probabilities_logistic > 0.5, 1, 0) * predicted_classes_tree
accuracy_hybrid <- mean(hybrid_predictions == Y_test)
print(paste("Accuracy of Hybrid Model:", accuracy_hybrid))
levels(test_data$Mental_Health)
levels(hybrid_predictions)

hybrid_predictions_factor <- factor(hybrid_predictions, levels = levels(test_data$Mental_Health))

# Create confusion matrix
conf_matrix <- confusionMatrix(hybrid_predictions_factor, test_data$Mental_Health)

# Extract evaluation metrics
accuracy_hybrid <- conf_matrix$overall['Accuracy']
precision_hybrid <- conf_matrix$byClass['Pos Pred Value']
recall_hybrid <- conf_matrix$byClass['Sensitivity']
f1_score_hybrid <- 2 * precision_hybrid * recall_hybrid / (precision_hybrid + recall_hybrid)

# Print evaluation metrics
print(paste("Accuracy of Hybrid Model:", accuracy_hybrid))
print(paste("Precision of Hybrid Model:", precision_hybrid))
print(paste("Recall of Hybrid Model:", recall_hybrid))
print(paste("F1 Score of Hybrid Model:", f1_score_hybrid))
conf_matrix






#----------------------------------------------------------------------------------------num_apps,frequencynum_
num_freq<-data.frame(main_data$num_apps,main_data$Frequency,main_data$Concentrate,main_data$Distraction)
View(num_freq)
colnames(num_freq) <- c("num_apps","freq","concentrate","distraction")
write.csv(num_freq,file="D:/social_mental/num_freq_conc_dis.csv",row.names = FALSE)
