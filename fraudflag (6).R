# Script that processes CSV files of suspicious new participants
# written by Linfei (linfei.li@gmail.com)
# in collaboration with Kuangfu and Claudia

# all lines that require RA input are tagged with #UPDATE
# On windows, Ctrl+Enter runs code on current line.
# Ctrl+Shift+Enter runs entire script

# loading in required libraries

require(data.table)
require(dplyr)
require(stringr)
require(lubridate)

# loading in data 1/3/2021

# First, set your working directory here. This is the folder that you'd want all your output to be downloaded to.
# You can check what your current directory is by typing getwd() into the Console
# the CSV files in the fread function can be downloaded directly from SONA/Qualtrics

setwd("C:/Users/Linfei Li/Documents/CDR/participant fraud") #UPDATE
sona=fread("1_4 course_report_0 (29).csv")[,1:5] #UPDATE
qualtrics=fread("1_4 Virtual Lab - New Participant Intake form_January 4, 2021_18.32.csv") #UPDATE

#upload flagged participants as CSV
#must remove "headings" for each type of flag in CSV
flagged_participants=fread("flagged participants (1).csv", header = TRUE) #UPDATE
flagged_participants=flagged_participants[which(flagged_participants$date!=""),]

##############################################################################################

# Minor data cleaning

#removing observations that are survey previews or additional text descriptions in first 2 rows
qualtrics=qualtrics[-c(1:2,which(qualtrics$Status=="Survey Preview")),] 


#renaming columns
print("hi")
names(qualtrics)[23] <- "First Name"
names(qualtrics)[24] <- "Last Name"
names(qualtrics)[25] <- "email1"

names(flagged_participants)[5] <- "First Name"
names(flagged_participants)[6] <- "Last Name"


#getting startdate into date format

(init_date=str_split(as.character(qualtrics$StartDate[1])," ",simplify=T)[1])
separator="/"
if(grepl("-",init_date,fixed=T)){
  separator="-"
}
date_split=str_split(init_date,separator,simplify=T)
year="y"
if(4 %in% sapply(date_split,nchar)){ 
  year="Y" #4 digit year
}
if(nchar(date_split[1])<=2){ #first char is month; mdy format
  qualtrics$StartDate=as.POSIXct(qualtrics$StartDate,
                                 format=paste("%m",separator,"%d",separator,"%",year," %H:%M",sep=""))
  qualtrics$EndDate=as.POSIXct(qualtrics$EndDate,
                               format=paste("%m",separator,"%d",separator,"%",year," %H:%M",sep=""))
} else { #ymd format
  qualtrics$StartDate=as.POSIXct(qualtrics$StartDate,
                                 format=paste("%",year,separator,"%m",separator,"%d %H:%M",sep=""))
  qualtrics$EndDate=as.POSIXct(qualtrics$EndDate,
                               format=paste("%",year,separator,"%m",separator,"%d %H:%M",sep=""))
}
qualtrics$StartDate=as_datetime(substr(as.character(qualtrics$StartDate),1,nchar(as.character(qualtrics$StartDate))-3),
                                format="%Y-%m-%d %H:%M")
qualtrics$EndDate=as_datetime(substr(as.character(qualtrics$EndDate),1,nchar(as.character(qualtrics$EndDate))-3),
                              format="%Y-%m-%d %H:%M")

if(sum(sapply(qualtrics$StartDate,is.na))>0){
  warning("Date format incorrect")
  print(rownames(qualtrics[which(is.na(qualtrics$StartDate))]))
}

flagged_participants$date=as.Date(flagged_participants$date, format = "%m/%d/%Y")

#trimming space after emails
qualtrics$email1=str_trim(qualtrics$email1)
qualtrics$edu_email2=str_trim(qualtrics$edu_email2)
sona$Email=str_trim(sona$Email)

#converting fraud scores from character to numeric
qualtrics$Q_RecaptchaScore=as.numeric(qualtrics$Q_RecaptchaScore)
qualtrics$Q_RelevantIDDuplicateScore=as.numeric(qualtrics$Q_RelevantIDDuplicateScore)
qualtrics$Q_RelevantIDFraudScore=as.numeric(qualtrics$Q_RelevantIDFraudScore)
qualtrics$Q_RelevantIDLastStartDate=as.POSIXct(qualtrics$Q_RelevantIDLastStartDate,format="%m/%d/%y %H:%M")

#creating refer text
# this concatenates all the refer text into 1 column called refer_text
qualtrics$refer_text=str_trim(paste(qualtrics$refer_10_TEXT,qualtrics$refer_11_TEXT,
                                    qualtrics$refer_16_TEXT,qualtrics$refer_18_TEXT,
                                    qualtrics$refer_7_TEXT))

#creating rowID
# this is used to uniquely identify each submission FOR THIS SESSION
# rowIDs are NOT going to be consistent across different ("updated") qualtrics datasets
qualtrics$rowID=1:nrow(qualtrics)

#creating Flagged
qualtrics$flagged=0

#creating Reason for Discuss
# use this to sort through the processed CSVs for upload/discuss
qualtrics$reason="N/A"

#creating email. If provided .edu email at any point as student, this will be copied. Else NA
qualtrics$email_fin=NA
#updating email fin for students with proper emails on any try
qualtrics$email_fin=ifelse(qualtrics$student!="Not student" & (endsWith(qualtrics$email1,".edu")|endsWith(qualtrics$edu_email2,".edu")),
                           ifelse(endsWith(qualtrics$email1,".edu"),qualtrics$email1,qualtrics$edu_email2),
                           qualtrics$email_fin)
#updating email fin for nonstudents
qualtrics$email_fin=ifelse(qualtrics$student=="Not student",
                           qualtrics$email1,
                           qualtrics$email_fin)

###############################################################################################
# Function definitions for CDR New Participant Processing
###############################################################################################

#helper function to concatenate lowercase first.last name for arbitrary df
# returns vector of strings "first.last"
# inputs: 
#     df = subsetted qualtrics df OR full sona df oR flagged_participants df
concat_names=function(df){
  first=tolower(str_trim(df$`First Name`))
  last=tolower(str_trim(df$`Last Name`))
  
  return(paste(first,".",last,sep=""))
}

#returns df of qualtrics data in specified date range, inclusive of provided dates
# will print warning message if empty dataframe is selected
# inputs:
#     qualtrics = full qualtrics dataframe
#     date_begin, date_end = string of form "YYYY-MM-DD HH:MM:SS"; H {0-23} M {0,59} S {0,59}
get_dated_df=function(qualtrics,date_begin,date_end){
  
  df=qualtrics %>%
    filter((StartDate>=as_datetime(date_begin) & StartDate<=as_datetime(date_end)) | 
             (EndDate>=as_datetime(date_begin) & EndDate<=as_datetime(date_end))) #12/23 end date fix for surveys that last 2+ days
  
  if(nrow(df)==0){
    warning("Warning! No participants were recorded in date range.")
  }
  
  return(df)
}

#function that returns dataframe of under-18 new signups
# flags them, and puts reason as "under 18"
# input: 
#     df = subset of qualtrics data of only the relevant dates
get_under_18=function(df){
  under18=df %>%
    filter(age=="No")
  
  if(nrow(under18)==0){
    return(under18)
  }
  
  under18$reason="under 18"
  under18$flagged=1
  return(under18)
}

#function that checks for duplicate IPs
# takes all qualtrics IPs on {date}, and 
#  returns a dataframe of all duplicates in qualtrics sorted by IP and date
#  and reason = "duplicate IP" OR "duplicate IP - student"
# currently, this does not take chronology into account. 
#  IE, if I use 4/1/2020 as a date, it will return 10/6 results as duplicates as well.
# inputs: 
#     qualtrics = full qualtrics data
#     date_begin, date_end = string "YYYY-MM-DD HH:MM:SS", e.g.: "2020-04-01 23:59:00"
get_duplicateIP=function(qualtrics,date_begin,date_end){
  
  today=get_dated_df(qualtrics,date_begin,date_end) #qualtrics data of todays date
  other=qualtrics[-today$rowID,] #all other qualtrics data
  
  #finding duplicates
  test=today[which(today$IPAddress %in% other$IPAddress),]
  test2=other[which(other$IPAddress %in% today$IPAddress),]
  today_ip=today$IPAddress[which(duplicated(today$IPAddress))]
  test3=today[which(today$IPAddress %in% today_ip),]
  
  ids=unique(c(test$rowID,test2$rowID,test3$rowID))
  
  df=qualtrics[which(qualtrics$rowID %in% ids),]
  
  if(nrow(df)==0){
    return(df)
  }
  
  df$reason=ifelse(df$student!="Not student","duplicate IP - student","duplicate IP")
  return(df[order(df$IPAddress,df$StartDate), ])
}

#returns df of everyone whose IP_block score is greater than/= 1
# reason is updated to "IP block" when df is entered in discuss
# inputs: 
#     df = subset of qualtrics data of only the relevant dates
IPblock=function(df){
  block=df %>%
    filter(IP_block>=1)
  
  if(nrow(block)==0){
    return(block)
  }
  
  block$reason=ifelse(block$IP_block==1,"IP block=1",
                      ifelse(block$IP_block==2,"IP block=2",
                             block$reason))
  
  return(block)
}

#returns qualtrics df of participants that made a weird joke
# updates reason to be "weird joke" in discuss df
# inputs:
#     df = subset of qualtrics data of only the relevant dates
#     ids = list of rowIDs corresponding to weird jokes
get_jokes=function(df,ids){
  if(is.null(ids)){
    return(df[-c(1:nrow(df)),])
  }
  jokes=df[which(df$rowID %in% ids),]
  jokes$reason="weird joke"
  
  return(jokes)
}

#checks qualtrics df against flagged participants df for matches in first.last and IP address
# returns a df of both IP and name matches (so if a new participant matches both name and IP, they will appear twice in returned df)
# "reason" is tagged as "X in flagged" and "flagged" is set to 1
# inputs:
#      flagged_participants = df of flagged participants from Drive; cleaned 
#      df = subset of qualtrics data of only the relevant dates
get_new_flags=function(flagged_participants,df){
  flagnames=concat_names(flagged_participants)
  qualnames=concat_names(df) 
  qualIP=df$IPAddress
  flagIP=flagged_participants$`IP Address`
  
  name_id=df$rowID[which(qualnames %in% flagnames)]
  IP_id=df$rowID[which(qualIP%in%flagIP)] #array of qualtrics rowIDs whose IPs show up in flagged
  
  name=df[which(df$rowID%in%name_id),] #df of new flags
  IP=df[which(df$rowID%in%IP_id),]
  
  dat=rbind(name,IP)
  
  if(nrow(dat)==0){
    return(dat)
  }
  
  if(nrow(IP)>0){
    IP$reason="IP in flagged"
    IP$flagged=1
  }
  if(nrow(name)>0){
    name$reason="name in flagged"
    name$flagged=1
  }
  
  return(rbind(name,IP))
}

#updates .edu emails in qualtrics$email_fin provided at second opportunity if no strange options are selected (not sign as student/no .edu email)
# if "not sign as student" and/or "my school does not have .edu address" is selected, df$reason is updated accordingly
#  note that if both options are selected, the participant will show up twice in output
# if a .edu email is provided at second chance, but "not sign" and/or "school has no .edu" is selected
#  reason is updated to "weird Other: gave...."
# inputs: 
#     df = subset of qualtrics data of only the relevant dates
student_email=function(df1){
  
  bad_email2=df1 %>%
    filter(student=="Other student") %>%
    filter(!endsWith(email1,".edu")) %>% #did not use .edu address at first
    filter((!endsWith(edu_email2, ".edu") & edu_email_reason=="" & not_sign_as_student=="")) #did not provide a .edu address at second chance and signed as student. 
  
  if(nrow(bad_email2)>0){
    bad_email2$reason="Other student who provided non-.edu email at second chance w/o good reason"
  }
  
  #ID as student, but email not end in .edu
  nonedu_email=df1 %>%
    filter(student=="Other student") %>%
    filter(!(endsWith(email1,".edu")|endsWith(edu_email2,".edu"))) %>% 
    filter(edu_email_reason!="")
  
  if(nrow(nonedu_email>0)){
    nonedu_email$reason="my school has a non-edu email"
  }
  
  #ID as student, but not want to sign as student
  # note that someone can both say they don't have .edu and not want to sign as student
  # in which case they would show up twice in df
  log_this=df1 %>%
    filter(student=="Other student") %>%
    filter(not_sign_as_student!="") 
  
  if(nrow(log_this>0)){
    log_this$reason="not want to sign as student - Other"
  }
  
  #people who gave edu email, but selected strange choice (not want to sign and/or school doesn't have .edu)
  weird=df1%>%filter(endsWith(edu_email2, ".edu") & (edu_email_reason!="" | not_sign_as_student!=""))
  
  if(nrow(weird>0)){
    weird$reason="weird Other: gave .edu email2, but not sign as student and/or claimed non-edu email"
  }
  
  if(nrow(log_this)+nrow(nonedu_email)+nrow(bad_email2)==0){
    return(log_this)
  }
  
  dat=rbind(log_this,nonedu_email,weird,bad_email2) 
  return(dat) 
}

#updates uchicago.edu and chicagobooth.edu emails in qualtrics$email_fin provided at second opportunity if no strange options are selected (not sign as student/no .edu email)
# if "not sign as student" and/or "my school does not have .edu address" is selected, 
#  df$reason is updated as "LIES" and the flag is set to 1
#   note that if both options are selected, the participant will show up twice in output
# if a .edu email is provided at second chance, but "not sign" and/or "school has no .edu" is selected
#  reason is updated to "weird UChicago: gave...."
#  and the flag is set to 1
# inputs: 
#     df = subset of qualtrics data of only the relevant dates
uchicago_email=function(df1){
  
  bad_email2=df1 %>%
    filter(student=="UChicago student") %>%
    filter(!(endsWith(email1,"uchicago.edu")|endsWith(email1,"chicagobooth.edu"))) %>% 
    filter((!(endsWith(edu_email2,"uchicago.edu")|endsWith(edu_email2,"chicagobooth.edu"))
            & edu_email_reason=="" & not_sign_as_student=="")) #did not provide a .edu address at second chance and signed as student. 
  
  if(nrow(bad_email2)>0){
    bad_email2$reason="UChicago student who provided non-Uchicago email at second chance w/o good reason"
  }
  #ID as student, but email not end in .edu
  nonedu_email=df1 %>%
    filter(student=="UChicago student") %>%
    filter(!(endsWith(email1,"uchicago.edu")|endsWith(email1,"chicagobooth.edu")|
               endsWith(edu_email2,"uchicago.edu")|endsWith(edu_email2,"chicagobooth.edu"))) %>%
    filter(edu_email_reason!="")
  
  if(nrow(nonedu_email)>0){
    nonedu_email$reason="LIES: my school has a non-edu email - UChicago"
    nonedu_email$flagged=1
  }
  
  #ID as student, but not want to sign as student
  # note that someone can both say they don't have .edy and not want to sign as student
  # in which case they would show up twice in df
  log_this=df1 %>%
    filter(student=="UChicago student") %>%
    filter(not_sign_as_student!="") 
  
  if(nrow(log_this)>0){
    log_this$reason="not sign as student - UChicago"
    log_this$flagged=1
  }
  
  #people who gave edu email, but selected strange choice (not want to sign and/or school doesn't have .edu)
  weird=df1%>%filter((endsWith(edu_email2,"uchicago.edu")|endsWith(edu_email2,"chicagobooth.edu"))
                     & (edu_email_reason!="" | not_sign_as_student!=""))
  
  if(nrow(weird)>0){
    weird$reason="weird UChicago: gave .edu email2, but not sign as student and/or claimed non-edu email"
    weird$flagged=0
  }
  
  if(nrow(log_this)+nrow(nonedu_email)+nrow(bad_email2)==0){
    return(log_this)
  }
  
  dat=rbind(log_this,nonedu_email,weird,bad_email2)
  return(dat[order(dat$rowID),]) 
}

#returns df of participants whose IPcountry and stated country do not match OR
# participants who have an IPCountry but no self-reported country
# tags reason as "country IP mismatch"
# inputs: 
#     df = subset of qualtrics data of only the relevant dates
get_country_mismatch=function(df){
  mismatched=df%>%
    filter(!startsWith(country,IP_country) | (country=="" & IP_country!=""))
  
  if(nrow(mismatched)==0){
    return(mismatched)
  }
  
  mismatched$reason="country IP mismatch"
  
  return(mismatched)  
}

#checks firstname.lastname and emails of qualtrics against SONA
# returns df from qualtrics of IDs that already are present in SONA
# reason is updated to "sona duplicate name" or "sona duplicate email"
# inputs:
#     df = subset of qualtrics data of only the relevant dates
get_sona_duplicates=function(df){
  
  copy=df
  copy$fullname=concat_names(df)
  
  sonanames=concat_names(sona)
  sonaemail=sona$Email 
  
  #getting rowIDs that correspond to duplicates in SONA
  name_id=copy$rowID[which(copy$fullname %in% sonanames)]
  #check both orig and "updated" email
  email_id=copy$rowID[which(df$email1 %in% sonaemail | df$email_fin %in% sonaemail)] 
  
  name=df[which(df$rowID%in%name_id),]
  
  email=df[which(df$rowID%in%email_id),]
  
  dat=rbind(name,email)
  
  if(nrow(dat)==0){
    return(dat)
  }
  
  if(nrow(email)!=0){
    email$reason="sona duplicate email"
  }
  if(nrow(name)!=0){
    name$reason="sona duplicate name"
  }
  return(rbind(name,email))
}

get_fraud=function(df){
  
  #also add if fraud scores are blank (recaptcha, duplicate, fraud); flag as fraud score blank
  blank=df %>%
    filter(is.na(Q_RecaptchaScore) | is.na(Q_RelevantIDDuplicateScore) | is.na(Q_RelevantIDFraudScore))
  
  fraud=df %>%
    filter(Q_RecaptchaScore<=0.5 | Q_RelevantIDDuplicate=="true" 
           | Q_RelevantIDDuplicateScore>=75 | Q_RelevantIDFraudScore>=30)
  
  startdate=df %>%
    filter(!is.na(Q_RelevantIDLastStartDate)) 
  
  dat=rbind(fraud,blank,startdate)
  
  if(nrow(dat)==0){
    return(dat)
  }
  
  if(nrow(blank!=0)){
    blank$reason="fraud score blank"
  }
  if(nrow(startdate!=0)){
    startdate$reason="last startdate"
  }
  if(nrow(fraud!=0)){
    fraud$reason=ifelse(fraud$student=="Not student",
                        "fraud score threshold",
                        "fraud score threshold - student")
  }
  return(rbind(fraud,blank,startdate))
}

###############################################################################################
# Running code
###############################################################################################

#input date range
#YYYY-MM-DD HH:MM:SS" HH=00-23, MM=00-59
date_begin="2020-12-18 22:31:00" #UPDATE
date_end="2021-01-04 23:59:00"   #UPDATE

# step 3

df=get_dated_df(qualtrics,date_begin,date_end)

# step 4

# 4a
under18=get_under_18(df) #flag and do not upload

# 4e
logstudents=student_email(df) #put in discuss
loguchicago=uchicago_email(df) #put in discuss

# 4biii
ip_block=IPblock(df) #put in discuss, do not upload

# 4c
newflags=get_new_flags(flagged_participants, df)

# 4d
#eyeball check
joke_test=df[which(df$age=="Yes"),c("rowID","joke")] #exclude under 18
View(joke_test)
weird_jokeID=c(6669,6677,6690,6704,6709,6711,6730,6731,6732) #UPDATE: if jokes are weird, put its rowID in the "c()"
# e.g. c(5702,5676,3048)

weirdjoke=get_jokes(df,weird_jokeID)

# 4f
country_mismatch=get_country_mismatch(df) #put in discuss, do not upload

# 4g
sona_duplicate=get_sona_duplicates(df) #discuss; upload at discretion

# 4h
fraud=get_fraud(df) # do not upload. discuss if student. 

# 4bi
#note that all these dfs are sorted by IP
duplicateIP=get_duplicateIP(qualtrics,date_begin,date_end)
dupIPstudent=duplicateIP[which(duplicateIP$reason=="duplicate IP - student")] 
dupIPother=duplicateIP[which(duplicateIP$reason=="duplicate IP")]

# putting CSVs together

# Discuss
discuss=rbind(ip_block,logstudents, dupIPother, #only nonstudents
              loguchicago,sona_duplicate, weirdjoke,
              fraud[which(fraud$reason!="fraud score threshold"),]) #students or blank
discuss=discuss[order(discuss$StartDate, discuss$IPAddress, discuss$rowID, discuss$reason),] 

discussfin=discuss
#create indicator of 1 if participant is in current period
discussfin$current=ifelse(discussfin$StartDate>=as_datetime(date_begin) & discussfin$StartDate<=as_datetime(date_end),1,0)

discussfin=subset(discussfin,select=-c(Status,Progress,Finished,RecordedDate,ResponseId,RecipientLastName,
                                       RecipientFirstName,RecipientEmail,ExternalReference,DistributionChannel,
                                       UserLanguage,refer_18_TEXT,refer_16_TEXT,refer_7_TEXT,refer_10_TEXT,refer_11_TEXT))
write.csv(discussfin,paste(as.Date(date_end),"_to_discuss.csv",sep=""))

# do not upload
do_not_upload=rbind(under18,ip_block,country_mismatch,fraud[which(fraud$reason=="fraud score threshold"),],newflags)
do_not_upload=do_not_upload[order(do_not_upload$StartDate, do_not_upload$rowID, do_not_upload$reason),]

do_not_upload=subset(do_not_upload,select=-c(Status,Progress,Finished,RecordedDate,ResponseId,RecipientLastName,
                                             RecipientFirstName,RecipientEmail,ExternalReference,DistributionChannel,
                                             UserLanguage,refer_18_TEXT,refer_16_TEXT,refer_7_TEXT,refer_10_TEXT,refer_11_TEXT))
write.csv(do_not_upload,paste(as.Date(date_end),"_do_not_upload.csv",sep=""))

#start to look for uploadables here
discuss_id=unique(discuss$rowID)
dont_upload_id=unique(do_not_upload$rowID)

`%notin%` <- Negate(`%in%`)

unsure=discuss[which(discuss$rowID[which(discuss$StartDate>=as_datetime(date_begin)&discuss$StartDate<=as_datetime(date_end))] 
                     %notin% dont_upload_id),] #in current discuss, but NOT don't upload
dupIPstudent=dupIPstudent[which(dupIPstudent$StartDate>=as_datetime(date_begin) & 
                                  dupIPstudent$StartDate<=as_datetime(date_end)),] #filtering for only students in current period 
okaystudents=dupIPstudent[which(dupIPstudent$rowID %notin% discuss$rowID 
                                | dupIPstudent$rowID %notin% do_not_upload$rowID),] #add in the dupIPstudents whose ONLY issue is dupIP

upload_check=df[-which(df$rowID %in% dont_upload_id),] #remove do not upload from uploads
upload_check=upload_check[-which(upload_check$rowID %in% unsure$rowID),] #remove participants with discuss items from upload, replace with df of reasons
upload_check=rbind(upload_check,unsure,okaystudents)
upload_check=upload_check[order(upload_check$StartDate, upload_check$rowID, upload_check$reason),]

upload_check=subset(upload_check,select=-c(Status,Progress,Finished,RecordedDate,ResponseId,RecipientLastName,
                                           RecipientFirstName,RecipientEmail,ExternalReference,DistributionChannel,
                                           UserLanguage,refer_18_TEXT,refer_16_TEXT,refer_7_TEXT,refer_10_TEXT,refer_11_TEXT))
write.csv(upload_check,paste(as.Date(date_end),"_upload_check.csv",sep=""))

