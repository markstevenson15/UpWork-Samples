install.packages("mailR",repos='http://cran.us.r-project.org')
install.packages("RODBC",repos='http://cran.us.r-project.org')
install.packages("lubridate",repos='http://cran.us.r-project.org')
install.packages("zoo",repos='http://cran.us.r-project.org')
install.packages("sqldf",repos='http://cran.us.r-project.org')
install.packages("xlsx",repos='http://cran.us.r-project.org')
install.packages("rJava")


library.funct<-function(){
  library(RODBC)
  library(lubridate)
  library(zoo)
  library(sqldf)
  library(xlsx)
  library(rJava) #Use 32 Bit system architecture for R, and Java. . 
                 #Download required version from https://www.java.com/en/download/manual.jsp
                 #If necessary, manually set the directory of Java location using Sys.setenv(). Example
                 #Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') 
}

idx.Function<-function(i,j){
  
  if(j=="<90" & (i==y[1])){
    k<-1
  }
  
  if(j=="BETWEEN 90 AND 119" & (i==y[1])){
    k<-2
  }
  
  if(j=="BETWEEN 120 AND 180" & (i==y[1])){
    k<-3
  }
  
  if(j==">180" & (i==y[1])){
    k<-4
  }
  
  if(j=="<90" & (i==y[2])){
    k<-5
  }
  
  if(j=="BETWEEN 90 AND 119" & (i==y[2])){
    k<-6
  }
  
  if(j=="BETWEEN 120 AND 180" & (i==y[2])){
    k<-7
  }
  
  if(j==">180" & (i==y[2])){
    k<-8
  }
  
  if(j=="<90" & (i==y[3])){
    k<-9
  }
  
  if(j=="BETWEEN 90 AND 119" & (i==y[3])){
    k<-10
  }
  
  if(j=="BETWEEN 120 AND 180" & (i==y[3])){
    k<-11
  }
  
  if(j==">180" & (i==y[3])){
    k<-12
  }
  
  if(j=="<90" & (i==y[4])){
    k<-13
  }
  
  if(j=="BETWEEN 90 AND 119" & (i==y[4])){
    k<-14
  }
  
  if(j=="BETWEEN 120 AND 180" & (i==y[4])){
    k<-15
  }
  
  if(j==">180" & (i==y[4])){
    k<-16
  }
  
  return(k)
}


Level2.qry.Function<-function(i,j){
  qry<-paste0('SELECT COUNT(ID) FROM Query1 WHERE FindingDate BETWEEN #',as.Date(i,origin="1970-01-01"),'# AND #',as.Date(i,origin="1970-03-31"),'# AND
              (Date() - FindingDate) ',j,' ')
  return(((c(j,as.numeric(sqlQuery(channel,qry)),c(i,i+91.25)))))
}

Level3.qry.Function<-function(i,j){
  qry<-paste0('SELECT COUNT(ID) FROM Query2 WHERE FindingDate BETWEEN #',as.Date(i,origin="1970-01-01"),'# AND #',as.Date(i,origin="1970-03-31"),'# AND
              (Date() - FindingDate) ',j,' ')
  return(((c(j,as.numeric(sqlQuery(channel,qry)),c(i,i+91.25)))))
}


df1.funct<-function(){
  df1<-data.frame(matrix(nrow=16,ncol=4))
  for(j in c("<90","BETWEEN 90 AND 119","BETWEEN 120 AND 180",">180")){
    for(i in ((as.Date(y,origin="1970-01-01")))){
      k<-idx.Function(i,j)
      df1[k,]<-c(Level2.qry.Function(i,j))
    }
  }
  
  df1[,3:4]<-lapply((lapply(df1[,3:4],function(q) as.numeric(q))),function(t) as.Date(t,origin="1970-01-01"))
  df1$'Date Range'<-paste(df1[,3]," to ", df1[,4])
  df1$Quarter[1:4]<-'Q1'
  df1$Quarter[5:8]<-'Q2'
  df1$Quarter[9:12]<-'Q3'
  df1$Quarter[13:16]<-'Q4'
  df1<-df1[,-4,drop=TRUE]
  df1<-df1[,-3,drop=TRUE]
  colnames(df1)=c("Days Old","# Findings","Date Range","Quarter")
  return(df1)
}

df2.funct<-function(){
  df2<-data.frame(matrix(nrow=16,ncol=4))
  for(j in c("<90","BETWEEN 90 AND 119","BETWEEN 120 AND 180",">180")){
    for(i in ((as.Date(y,origin="1970-01-01")))){
      k<-idx.Function(i,j)
      df2[k,]<-c(Level3.qry.Function(i,j))
    }
  }
  df2[,3:4]<-lapply((lapply(df2[,3:4],function(q) as.numeric(q))),function(t) as.Date(t,origin="1970-01-01"))
  df2$'Date Range'<-paste(df2[,3]," to ", df2[,4])
  df2$Quarter[1:4]<-'Q1'
  df2$Quarter[5:8]<-'Q2'
  df2$Quarter[9:12]<-'Q3'
  df2$Quarter[13:16]<-'Q4'
  df2<-df2[,-4,drop=TRUE]
  df2<-df2[,-3,drop=TRUE]
  colnames(df2)=c("Days Old","# Findings","Date Range","Quarter")
  return(df2)
}

df3.funct<-function(){
  qry2<-sqlQuery(channel,'SELECT "Model Name",COUNT(ID) as "# Findings" FROM Query3 GROUP BY "Model Name"')
  return(qry2)
}


df4.funct<-function(){
  qry3<-sqlQuery(channel,'SELECT "Model Name",Date() as TodaysDate,"Planned Remediation Date" as "Due", "Level" FROM Query3')
  qry3$Due<-as.Date(as.yearmon(as.yearqtr(qry3$Due)),frac=3)
  qry3$'Days Remaining'<-qry3$Due-as.Date(qry3$TodaysDate)
  qry3$Group<-as.character(cut(as.numeric(qry3$'Days Remaining'),breaks=c(-Inf,0,30,60,90,120,180,Inf)))
  qry3$Group[qry3$Group=="(-Inf,0]"]<-"Overdue"
  qry3$Group[qry3$Group=="(30,60]"]<-"Between 30 and 60"
  qry3$Group[qry3$Group=="(60,90]"]<-"Between 60 and 90"
  qry3$Group[qry3$Group=="(90,120]"]<-"Between 90 and 120"
  qry3$Group[qry3$Group=="(120,180]"]<-"Between 120 and 180"
  qry3$Group[qry3$Group=="(180, Inf]"]<-"Over 180"
  return(qry3)
}


df5.funct<-function(){
  qry4<-sqlQuery(channel,"SELECT * FROM Query4")
  return(qry4)
}

df6.funct<-function(){
  df.Summary.1<-data.frame(matrix(nrow=2,ncol=2))
  colnames(df.Summary.1)<-c('High/level 2','Medium/level 3')
  rownames(df.Summary.1)<-c('Due within 90 days','Due within 180 days')
  df.Summary.1[1,1]<-as.numeric(sqldf("SELECT COUNT('Model Name') FROM df4 a WHERE a.'Days Remaining' < 90 AND a.'Level' LIKE '%High%'"))
  df.Summary.1[1,2]<-as.numeric(sqldf("SELECT COUNT('Model Name') FROM df4 a WHERE a.'Days Remaining' < 90 AND a.'Level' LIKE '%Med%'"))
  df.Summary.1[2,1]<-as.numeric(sqldf("SELECT COUNT('Model Name') FROM df4 a WHERE a.'Days Remaining' BETWEEN 91 AND 180 AND a.'Level' LIKE '%High%'"))
  df.Summary.1[2,2]<-as.numeric(sqldf("SELECT COUNT('Model Name') FROM df4 a WHERE a.'Days Remaining' BETWEEN 91 AND 180 AND a.'Level' LIKE '%Medium%'"))	
  df.Summary.1[is.na(df.Summary.1)] <- 0
  return(df.Summary.1)
}

df7.funct<-function(){
  df.Summary.2<-data.frame(matrix(nrow=3,ncol=2))
  colnames(df.Summary.2)<-c('High/level 2','Medium/level 3')
  rownames(df.Summary.2)<-c('<90 days','>=90 but <120 days','>=120 days')
  
  df.Summary.2[1,1]<-as.numeric(sqldf("SELECT SUM(a.'# Findings') FROM df1 a WHERE a.'Days Old' LIKE '<90'"))
  df.Summary.2[2,1]<-as.numeric(sqldf("SELECT SUM(a.'# Findings') FROM df1 a WHERE a.'Days Old' LIKE 'BETWEEN 90 AND 119'"))
  df.Summary.2[3,1]<-as.numeric(sqldf("SELECT SUM(a.'# Findings') FROM df1 a WHERE a.'Days Old' IN ('BETWEEN 120 AND 180','>180')"))
  
  df.Summary.2[1,2]<-as.numeric(sqldf("SELECT SUM(a.'# Findings') FROM df2 a WHERE a.'Days Old' LIKE '<90'"))
  df.Summary.2[2,2]<-as.numeric(sqldf("SELECT SUM(a.'# Findings') FROM df2 a WHERE a.'Days Old' LIKE 'BETWEEN 90 AND 119'"))
  df.Summary.2[3,2]<-as.numeric(sqldf("SELECT SUM(a.'# Findings') FROM df2 a WHERE a.'Days Old' IN ('BETWEEN 120 AND 180','>180')"))
  return(df.Summary.2)
}

#Update output path
output.funct<-function(){
  write.xlsx(df1,file=paste("C:\\Users\\mark\\Documents\\Findings as of ",Sys.Date(),".xlsx"),sheetName="L2FindingsByAgeQtr")
  write.xlsx(df2,file=paste("C:\\Users\\mark\\Documents\\Findings as of ",Sys.Date(),".xlsx"),sheetName="L3FindingsByAgeQtr",append=TRUE)
  write.xlsx(df3,file=paste("C:\\Users\\mark\\Documents\\Findings as of ",Sys.Date(),".xlsx"),sheetName="FindingsByModel",append=TRUE)
  write.xlsx(df4,file=paste("C:\\Users\\mark\\Documents\\Findings as of ",Sys.Date(),".xlsx"),sheetName="FindingsDueDate",append=TRUE)
  write.xlsx(df5,file=paste("C:\\Users\\mark\\Documents\\Findings as of ",Sys.Date(),".xlsx"),sheetName="FindingsClosed",append=TRUE)
  write.xlsx(df6.Summary.1,file=paste("C:\\Users\\mark\\Documents\\Findings as of ",Sys.Date(),".xlsx"),sheetName="FindingsSummary1",append=TRUE)
  write.xlsx(df7.Summary.2,file=paste("C:\\Users\\mark\\Documents\\Findings as of ",Sys.Date(),".xlsx"),sheetName="FindingsSummary2",append=TRUE)
}

#reminder<-function(){
#  qry3$'Days Remaining'<-as.numeric(qry3$'Days Remaining')
#  under15<-sqldf("SELECT * FROM qry3 a WHERE a.'Days Remaining' < 15")
#  
#  send.mail(from = "mstevenson@wintrust.com",
#            to = c("Recipient 1 <markstevenson15@gmail.com>"),
#            subject = "Remediations Due within 2 Weeks",
#            body = paste("There are",nrow(under15),"remediations due within 2 weeks. See attached file for details."),
#            smtp = list(host.name = "ASPMX.L.GOOGLE.COM", port = 25),
#            authenticate = FALSE,
#            send = TRUE,
#            attach.files=c("C:\\Users\\mstevens\\Documents\\16709 finding.csv")
#  )
#}

save(idx.Function,file=paste0(.libPaths(),"/idxFunction.Rdata")[1])
save(Level2.qry.Function,file=paste0(.libPaths(),"/L2qryFunction.Rdata")[1])
save(Level3.qry.Function,file=paste0(.libPaths(),"/L3qryFunction.Rdata")[1])
save(df1.funct,file=paste0(.libPaths(),"/df1Function.Rdata")[1])
save(df2.funct,file=paste0(.libPaths(),"/df2Function.Rdata")[1])
save(df3.funct,file=paste0(.libPaths(),"/df3Function.Rdata")[1])
save(df4.funct,file=paste0(.libPaths(),"/df4Function.Rdata")[1])
save(df5.funct,file=paste0(.libPaths(),"/df5Function.Rdata")[1])
save(df6.funct,file=paste0(.libPaths(),"/df6Function.Rdata")[1])
save(df7.funct,file=paste0(.libPaths(),"/df7Function.Rdata")[1])
save(output.funct,file=paste0(.libPaths(),"/outputFunction.Rdata")[1])
save(reminder,file=paste0(.libPaths(),"/reminder.Rdata")[1])
save(library.funct,file=paste0(.libPaths(),"/libfunct.Rdata")[1])



#This section was meant to run as a standalone program after prior had been run once. 

for(i in c("/L2qryFunction.Rdata",
           "/L3qryFunction.Rdata",
           "/df1Function.Rdata",
           "/df2Function.Rdata",
           "/df3Function.Rdata",
           "/df4Function.Rdata",
           "/df5Function.Rdata",
           "/df6Function.Rdata",
           "/df7Function.Rdata",
           "/idxFunction.Rdata",
           "/outputFunction.Rdata",
           "/libfunct.Rdata")){
  load(paste0(.libPaths()[1],i))
}


library.funct()

channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/Mark/Downloads/FindingDB1.accdb") #Update path where Database is located.
x<-seq((Sys.Date()-years(1)),Sys.Date(),1)
y<-as.numeric(quantile(as.numeric(x)))[1:4]


df1<-df1.funct()
df2<-df2.funct()
df3<-df3.funct()
df4<-df4.funct()
df5<-df5.funct()
df6.Summary.1<-df6.funct()
df7.Summary.2<-df7.funct()
output.funct()

#if(min(na.omit(qry3$'Days Remaining'))<15){
#reminder()
#}










