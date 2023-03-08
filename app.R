######
library(shiny)
library(shinyWidgets)
library(DT)
library(fontawesome)
library(plotly)
library(shinycssloaders)
library(flextable)
library(DT)
library(htmltools)
######

######
library(data.table)
library(ggplot2)

load('dataFiltered.RData')

dat[,Year:=year(date)]
dat[,Month:=month(date)]
dat[,Week:=week(date)]
dat[,Month:=paste(Year,sprintf('%02d',Month),sep="-")]
dat[,Week:=paste(Year,sprintf('%02d',Week),sep="-")]
dat[,Day:=date]

countries<-setNames(
c('CN','US','JP','DE','GB','IN'),
c('China','United States','Japan','Germany','United Kingdom','India'))

countries<-as.data.table(countries,keep=T)

names(countries)<-c('country','location_key')



ui<-navbarPage(
  id = "zongPage",
  collapsible = TRUE,
  inverse = TRUE,
  windowTitle = "References database",
  position = "fixed-top",
  
  ### css
  tags$head(
    tags$style(HTML("

	ul#navBar2 {
    float: right !important;
    }
	 
    body {padding-top: 60px!important;}
	 
	.shiny-output-error { visibility: hidden; }
	
	.shiny-output-error:before { visibility: hidden; }
    ")),
   ),
	
               
  tabPanel("About",
  fluidRow(
  column(width=10,offset=1,
  HTML(
  '<p>The source data is <em>the epidemiology.csv</em> file, obtained from 
  <a href="https://github.com/GoogleCloudPlatform/covid-19-open-data">covid-19-open-data</a>, 
  with 12,525,825 rows and 10 columns, each with the following column names:<p>
  
  <ul>
    <li>date</li>
    <li>location_key</li>
    <li>new_confirmed</li>
    <li>new_deceased</li>
    <li>new_recovered</li>
    <li>new_tested</li>
    <li>cumulative_confirmed</li>
    <li>cumulative_deceased</li>
    <li>cumulative_recovered</li>
    <li>cumulative_tested</li>
  </ul>
  
  <p>The <code>date</code> range for this data is 2019-12-31 to 2022-12-30.</p>
  <p><code>location_key</code> is the alphabetical abbreviation of the country or region, the abbreviation of the country is in two-letter form, 
  according to the <a href="https://laendercode.net/en/2-letter-list.html">https://laendercode.net/en/2-letter-list.html</a> website, 
  the two-letter abbreviation can be transformed into the country name.</p>
  <p>For simplicity, we have selected only 6 representative countries for this analysis, they are</p>'),

 uiOutput('sixCs'),
 
 tags$br(),
 
  HTML('
  <p>Furthermore, we only use <u>confirmed</u>, <u>deceased</u> and <u>tested</u> data.
  The first two of these are relatively complete, while <u>tested</u> data are only available for a few countries and certain time periods.</p>
 '),
 
 HTML('Data manipulation was performed by <a href="https://r-datatable.com/">data.table</a> and 
 visualization was done by <a href="https://ggplot2.tidyverse.org/">ggplot2</a> and <a href="https://plotly.com/r/">plotly</a>.')
  
  ))),  ####end tab1.1(information)
  
  tabPanel("visualization",
  fluidRow(
  column(3,
  awesomeCheckboxGroup(
       inputId = 'whichCountry',
       label = 'Select coutries:', 
        choices = c('China(CN)'='CN',
                    'United States(US)'='US',
                    'Japan(JP)'='JP',
                    'Germany(DE)'='DE',
                    'United Kingdom(GB)'='GB',
                    'India(IN)'='IN'),
       selected = c('CN','DE')
    )
  ),
  
  column(4,
  
   radioGroupButtons(
   inputId = "whichStat",
   label = NULL,
   choices = c('Confirmed','Deceased','Tested'),
   selected='Deceased',
   individual = TRUE,
   checkIcon = list(
      yes = tags$i(class = "fa fa-circle", 
    style = "color: steelblue"),
   no = tags$i(class = "fa fa-circle-o", 
    style = "color: steelblue"))
   ),

   radioGroupButtons(
     inputId = "whichFirm",
     label = NULL,
     choices = c('New','Cumulative'),
     individual = TRUE,
     checkIcon = list(
        yes = tags$i(class = "fa fa-circle", 
      style = "color: steelblue"),
     no = tags$i(class = "fa fa-circle-o", 
      style = "color: steelblue"))
   ),
  
  radioGroupButtons(
   inputId = "peroidBy",
   label = NULL,
   choices = c('Month','Week','Day'),
   justified = TRUE,
   selected='Week',
   checkIcon = list(
    yes = icon("ok", 
    lib = "glyphicon"))
  ),
  
  switchInput(
   inputId = "wantlog",
   label = "Y log scale", 
    labelWidth = "80px"
  )
  )
  ), ####end config row;
  
  fluidRow(
  column(12,withSpinner(plotlyOutput('TSPlot')))
  
  )
  ), ####end tab1.1(information)
  
  tabPanel("Data Query",
  dateInput('whichDay',label='Select one day',min='2019-12-31',max='2022-12-31',value='2022-02-02'),
  DTOutput('p3table')
  )
)


server<-function(input,output,session)
{

output$sixCs<-renderUI({
flextable(countries[,2:1]) %>% 
autofit() %>%
theme_vanilla() %>%
htmltools_value()
})


output$p3table<-renderDT({
oneday<-dat[date==as.Date(input$whichDay),]
oneday<-oneday[,2:8]
oneday<-countries[oneday,on=.(location_key)][,!'location_key']



sketch <- htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
	  th(rowspan = 2, 'Country'),
      th(colspan = 3, 'New',style='text-align:center'),
      th(colspan = 3, 'Cumulative',style='text-align:center')
    ),
    tr(
      lapply(rep(c('Confirmed', 'Deceased','Tested'), 2), th)
    )
  )
))


datatable(oneday,rownames=F,container = sketch,options=list(dom='t',  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#CCFFFF', 'color': '#34282C'});",
    "}"))) %>%
formatRound(names(oneday)[-1],digits=0)


})


output$TSPlot<-renderPlotly({

#### time series
col1<-paste(tolower(input$whichFirm),tolower(input$whichStat),sep="_")
col2<-input$peroidBy
cons<-input$whichCountry
colss<-c(col1,col2,'location_key')
plot.data<-dat[location_key %in% cons,..colss]
names(plot.data)<-c('STAT','SPAN','REGION')
plot.data<-plot.data[,.(STAT=sum(STAT)),.(SPAN,REGION)]

p1<-ggplot(plot.data,aes(x=SPAN,y=STAT,colour=REGION,group=REGION))+
    geom_point(size=0.8)+
    geom_line()+
    theme_bw()+
    #guides(x=guide_axis(angle=60))+
    labs(x=paste0('Time Period (',input$peroidBy,")"),
	y=paste0('Count (',input$whichFirm,")"),
	title=paste(input$whichFirm,input$whichStat,'at',input$peroidBy,'Level')
     )

if(input$peroidBy =='Week')
{
bs<-plot.data[,unique(grep('(01|05|10|15|25|20|30|35|40|45)$',SPAN,value=T))]
p1<-p1+scale_x_discrete(breaks=bs)
}

if(input$peroidBy =='Day')
{
bs<-plot.data[,unique(grep('(01|10|20)$',SPAN,value=T))]
p1<-p1+scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m-%d",expand=expansion(add=10))
}


if(input$wantlog) p1<-p1+scale_y_continuous(trans='log1p')

ggplotly(p1,tooltip=c('x','y','color'),height=450) %>%
layout(xaxis=list(tickangle=-60))

})


}


shinyApp(ui,server)
