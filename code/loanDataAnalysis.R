library(dplyr)
library(tidyr)
library(gridExtra)
library(ggplot2)
library(lubridate)


# this is script to generate
# univariate data vizs R scripts for columns
# with in a dataframe
univariateViz <- function(filePath,nCols){
  data <- read.csv(filePath)
  dfName <- gsub("*\\\\","",filePath)
  dfName <- gsub(".csv","","dfName")
  print(paste("dfName: ",dfName,sep=""))
  cols <- colnames(data)
  for(k in 1:length(cols)){
    cols[k] <- gsub("\\.","_",cols[k])
  }
  colnames(data) <- cols
  colnames(data)

  # evaluate each column
  i = 0
  lines <- c()
  for(col in cols){
    print(col)
  }

  for(col in cols){
    line <- paste("p",i," <- ggplot(data) ",sep="")
    subLine <- "ggplot(data) "
    print(col)
    # remove NAs, null values
    t <-  data[[col]]
    t <- t[!is.na(t)]
    t <- t[!is.null(t)]

    # check if is discrete values that have small no of unique values
    uni_val <- length(unique(t))
    print(paste(col,"  ","UNIQUE values: ",uni_val))

    if(uni_val<8){
      # use bar chart
      line <- paste(line,"+ geom_bar(aes(x=", col,",fill=",col,"),stat=\"count\")",sep="")

      subLine <- paste(subLine,"+ geom_bar(aes(x=", col,",fill=",col,"),stat=\"count\")",sep="")

      lines <- append(lines,line)

      # print(noquote(line))
      # print(noquote(subLine))
      eval(parse(text=line))

      i <- i + 1
    }else if(is.numeric(t)){
      # use histogram
      line <- paste(line,"+ geom_histogram(aes(x=", col,"))",sep="")
      subLine <- paste(subLine,"+ geom_histogram(aes(x=", col,"))",sep="")
      lines <- append(lines,line)
      # print(noquote(line))
      # print(noquote(subLine))
      eval(parse(text=line))

      i <- i + 1

    }

    line <- ""
  }

  gridLine <- r"(grid.arrange()"
  for(j in 1:i-1){
    gridLine <- paste(gridLine,"p",j,", ",sep="");
  }
  gridLine <- paste(gridLine,"ncol = ", nCols,")",sep="");
  print(noquote(lines))
  print(noquote(gridLine))
  eval(parse(text=gridLine))
}

ggplot(loan_data) +
  geom_bar(aes(x=education,fill=education),stat="count") +
  facet_grid(terms~.)

# this is an exploratory project for loan payment data
# data source: Kaggle(https://www.kaggle.com/datasets/zhijinzhai/loandata)
library(tidyr)
library(dplyr)
# load the data
loan_data <- read.csv(r"(D:\Download\LoanData\Loan payments data.csv)")
# check basic info & structure
colnames(loan_data)
head(loan_data)
glimpse(loan_data)

# Goal: to derive insights and identify pattern in loan payments overdue

# Problems with the data
# 1. Lack of demographic data, only age, education and gender info are available
# 2. Only data in Sep to Nov in 2016, can't derive any seasonal difference or trends from it
# 3. Also the time-effectiveness of the data is questionable as this analysis is carried out in Feb 2024
# 4. The data is relatively small, with only 500 rows of data
# 5. From the similarity of loan capital, it seems that this data only represents a fraction of loan actions


# data cleaning
# remove duplicates
sum(duplicated(loan_data$Loan_ID))

# check for NAs
for(col in colnames(loan_data)){
  print(paste(col,' NA:',sum(is.na(loan_data[col]))))
}
# there are 300 NAs in past_due_days, primarily because those who paid on time will
print(sum(loan_data$loan_status=='PAIDOFF'))
# 300 records of paying loan before due, matched with the NAs

# check for NULLs
for(col in colnames(loan_data)){
  print(paste(col,' NA:',sum(is.null(loan_data[col]))))
}
# no NULL values found


# see basic uni-variate things
mean(loan_data$Principal) # average principal involved: 943.2
sd(loan_data$Principal) # standard deviation in principal: 115.2403
summary(loan_data$Principal)
# insight: the principal involved are close to 1000

loan_data %>%
  group_by(loan_status) %>%
  summarize(count=n())
# insight: 300 paidoff, 100 collection, 100 paid of after collection

loan_data %>%
  group_by(terms) %>%
  summarize(count=n())
# insight: 272 30-day terms, 207 15-day terms, 21 7-day terms
# insight: most loan are made in 15 or 30 days term

# visualize univariately
univariateViz(r"(D:\Download\LoanData\Loan payments data.csv)",2)


# insight: College, high School or below are extremely large, with bachelor lower, and master or above extremely small
# insight: the effective date is centered around 11 Sep 2016
# insight: age is centered on 28-30

# insight: number of loan borrowers is way larger than number of female borrowers

ggplot(loan_data) +
  geom_bar(aes(x=Gender,fill=Gender),stat="count") +
  facet_grid(loan_status~.)
# insight: remale ratio is higher in paidoff group




# check for relationships between 2 variables
ggplot(loan_data) +
  geom_freqpoly(aes(x=age,color=loan_status))
# Null Hypothesis1: No significant difference in age distribution between different loan_status


ggplot(loan_data) +
  geom_bar(aes(x=education),stat='count',fill='#ab231b') +
  facet_grid(loan_status ~ .)
# insight: the distribution of PAIDOFF AND PaidOff after collection are similar,
# with college education being the dominant, and high school or below and bechalor comes after
# whilst master or above being extremely rare in all group
# but in COLLECTION group, high school or below education level becomes the dominant

colnames(loan_data)

ggplot(loan_data) +
  geom_bar(aes(x=Gender),stat='count',fill='#ab231b') +
  facet_grid(loan_status ~ .)
# insight: males take up more portion of Collection and Paid After Collection

ggplot(loan_data) +
  geom_freqpoly(aes(x=age,color=loan_status),stat='count',size=1)
# insight: distribution of age is somewhat similar but there is no spike at around 35 in COLLECTION group

mean(loan_data$past_due_days[!is.na(loan_data$past_due_days)])
sd(loan_data$past_due_days[!is.na(loan_data$past_due_days)])
# insight: past due days are on average 36.01 days
# 29.3 standard deviations, quite large
# consider the fact that terms are mostly 15, 30 days
# the past due days are quite long

ggplot(loan_data)+
  geom_freqpoly(aes(x=past_due_days,color=loan_status),size=1) +
  facet_grid(terms ~.)
# insight : if people pay defaulted loan, some pay it quickly (within 10 days of default),
# others do not pay it until compulsory collection
# note that there is a small increase at 20 + for loan of 30-day term

loan_data_grp_paidoff_date <- loan_data[loan_data$paid_off_date!="",] %>%
  group_by(paid_off_date) %>%
  summarize(totalAmount=sum(Principal))

loan_data$day <- wday(as.Date(loan_data$paid_off_date,"%m/%d/%Y"),week_start = 1)

colnames(loan_data)
loan_data_day <- loan_data %>%
                    group_by(day) %>%
                    summarize(avgPrincipal=mean(Principal),
                              devPrincipal=sd(Principal),
                              totalPrincipal=sum(Principal))




loan_data_date <- loan_data %>%
                    group_by(paid_off_date,day) %>%
                    summarize(totalAmount=sum(Principal))

ggplot(loan_data_date[loan_data_date$paid_off_date!="",]) +
  geom_col(aes(x=paid_off_date,y=totalAmount)) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()

loan_data_date %>% arrange(desc(totalAmount))
# top 10 dates that received abnormal paid back values
# 10Oct(Tue), 25Sep(Mon), 11Oct(Fri), 26Sep(Tue), 24Sep(Sun),
# 13Oct(Fri), 8Oct(Sun), 13Oct(Fri), 9Oct(Mon), 12OCt(Thu)

ggplot(loan_data) +
  geom_histogram(aes(x=as.Date(paid_off_date,"%m/%d/%Y"),fill='#f57600',alpha=0.2),stat='count') +
  geom_histogram(aes(x=as.Date(effective_date,"%m/%d/%Y"),fill='#8babf1',alpha=0.2),stat='count') +
  scale_fill_discrete(labels=c('Paid Off Date', 'Effective Date')) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(loan_data) +
  geom_histogram(aes(x=as.Date(paid_off_date,"%m/%d/%Y"),alpha=0.2),stat='count') +
   theme(axis.text.x = element_text(angle = 90)) +
   facet_grid(education ~ .)



ggplot(loan_data) +
  geom_histogram(aes(x=as.Date(effective_date,"%m/%d/%Y"),fill='#8babf1',alpha=0.2),stat='count') +
  theme(axis.text.x = element_text(angle = 90))

loan_data_15 <- loan_data[loan_data$terms==15,]
loan_data_15 %>%
  group_by(education) %>%
  summarize(co)


loan_data_30 <- loan_data[loan_data$terms==30,]


ggplot(loan_data) +
  geom_bar(aes(x=loan_status,fill=loan_status),stat="count") +
  facet_grid(terms~.)

# percentage of loan_status in each terms
terms <- c(7,15,30)
for(t in terms){
  r1 <- round(sum(loan_data$terms==t & loan_data$loan_status=="COLLECTION")/sum(loan_data$terms==t),2)
  r2 <- round(sum(loan_data$terms==t & loan_data$loan_status=="COLLECTION_PAIDOFF")/sum(loan_data$terms==t),2)
  r3 <- round(sum(loan_data$terms==t & loan_data$loan_status=="PAIDOFF")/sum(loan_data$terms==t),2)
  print(noquote(paste("The group of term ",t," consist of ",r1*100,"% COLLECTION, ", r2*100,"% COLLECTION_PAIDOFF, ",r3*100,"% PAIDOFF")))
}
# insight: the ratio of paidoff decreases as the term increases
# hypothesis: longer terms, higher probability of overdue



terms <- c(7,15,30)
for(t in terms){
  r1 <- round(sum(loan_data$terms==t & loan_data$Gender=="male")/sum(loan_data$terms==t),2)
  r2 <- round(sum(loan_data$terms==t & loan_data$Gender=="female")/sum(loan_data$terms==t),2)
  print(noquote(paste("The group of term ",t," consist of ",r1*100,"% Male, ", r2*100,"% Female")))
}



terms <- c(7,15,30)
for(t in terms){
  edu_level <- unique(loan_data$education)
  line <- paste("The group of term ",t," consist of ")
  for(edu in edu_level){
    r <- round(sum(loan_data$terms==t & loan_data$education==edu)/sum(loan_data$terms==t),2)

    line <- paste(line,r*100,"% ",edu,", ",sep="")
  }
  print(noquote(line))
}

loan_data$effective_day_week <- wday(as.Date(loan_data$effective_date,"%m/%d/%Y"),week_start = 1)

days <- sort(unique(loan_data$effective_day_week))
for(d in days){
  r1 <- round(sum(loan_data$effective_day_week==d & loan_data$loan_status=="PAIDOFF")/sum(loan_data$effective_day_week==d),2)
  r2 <- round(sum(loan_data$effective_day_week==d & loan_data$loan_status=="COLLECTION_PAIDOFF")/sum(loan_data$effective_day_week==d),2)
  r3 <- round(sum(loan_data$effective_day_week==d & loan_data$loan_status=="COLLECTION")/sum(loan_data$effective_day_week==d),2)
  print(noquote(paste("In loans borrowed on ",d," consist of ",r1*100,"% COLLECTION, ", r2*100,"% COLLECTION_PAIDOFF, ",r3*100,"% PAIDOFF")))

}
ggplot(loan_data) +
  geom_bar(aes(x=effective_day_week,fill=loan_status),stat="count")

ggplot(loan_data) +
  geom_bar(aes(x=effective_day_week,fill=education),stat="count")

# insight: people borrow less on Tue,Wed,Thu,Fri
# poeple borrow the most on Sun and Mon
# but poeple who borrowed on Tue,Wed,Thu will almost never overdue their loan




