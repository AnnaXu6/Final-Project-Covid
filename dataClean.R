library(data.table)
dat<-fread('epidemiology.csv')

##map location_key to country name according to
###https://laendercode.net/en/2-letter-list.html 
##CN China
##US United States
##JP Japan
##DE Germany
##GB United Kingdom
##IN India

countries<-setNames(
c('CN','US','JP','DE','GB','IN'),
c('China','United States','Japan','Germany','United Kingdom','India'))

dat<-dat[location_key %in% countries]

cols.want<-c('date','location_key',
            grep('(onfirmed|deceased|tested)',names(dat),value=T))
			
	
dat<-dat[,..cols.want]

save(dat,file='dataFiltered.RData')

