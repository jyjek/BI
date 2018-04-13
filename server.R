library(bigrquery)
library(dplyr)
library(DT)
library(highcharter)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(dplyr)
library(lubridate)
library(shinyBS)
library(shinyWidgets)
library(highcharter)
library(stringr)
library(DBI)
library(glue)
library(ISOweek)
project <- "rozetka-com-ua"
shinyServer <- shinyServer(function(input, output, session) {
  runjs({'var el2 = document.querySelector(".skin-blue");
    el2.className = "skin-blue sidebar-mini";
    var clicker = document.querySelector(".sidebar-toggle");
    clicker.id = "switchState";
    '})
  onclick('switchState', runjs({'
    var title = document.querySelector(".logo")
    if (title.style.visibility == "hidden") {
    title.style.visibility = "visible";
    } else {
    title.style.visibility = "hidden";
    } '}))
  
  output$sidebarUserPanel <- renderUI({
    
      sidebarUserPanel(# input$userName,
        "admin",
        subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
        image="https://www.parvisdental.co.uk/wp-content/uploads/2016/09/no-profile-image.jpg"
      )
  })
  
  val<-reactiveValues()
  val$num<-1

  output$cat= renderUI({ 
    numericInput("cat","Input category:",value = "")
  })
  
 output$sk= renderUI({

    numericInput("SKU","Input SKU:",value = "")
  })
 
 output$ab= renderUI({
     validate(
       need(input$SKU!="", 'Choose SKU')
     )
   actionButton("ok","Start processing",class='btn-info')
 })
 
  observeEvent(input$ok,{
  val$num<-1
   if( val$num!=0){
    sql<-sprintf("select b.id,b.title,c.sum_30,a.data, exact_count_distinct(user_webstore_id) as users 
              from
                 (SELECT integer(session_interaction_product_id) as id, integer (user_webstore_id) as user_webstore_id,date as data
                 FROM (TABLE_DATE_RANGE([rozetka-com-ua:UserFeed.UserFeed_],
                 DATE_ADD(CURRENT_TIMESTAMP(), -28, 'DAY'),
                 CURRENT_TIMESTAMP()))       ) a 
                 join
                 (SELECT id,title
                 FROM [rozetka-com-ua:rozetka_products.products] 
                 where  id in (%s)
                 ) b 
                 on a.id = b.id
                 
                 left join 
                 (SELECT product_id, date(created) as data ,count(product_quantity) as sum_30
                 FROM [rozetka-com-ua:CommonViews.orders_products] 
                 where  created> DATE_ADD(CURRENT_DATE(), -28, 'DAY') 
                 and order_status = 'complete' and product_status ='staffed'  
                 group by 1,2) c
                 on b.id=c.product_id and c.data=a.data                     
                 group by 1,2,3,4
                 order by a.data",input$SKU)
   
    q<-query_exec(sql, project = project,max_pages = Inf)
    q<-q%>%
      dplyr::mutate_all(funs(replace(.,is.na(.),0)))%>%
      dplyr::mutate(a_data=lubridate::ymd(a_data))%>%
      magrittr::set_colnames(c("id","title","sales","date","views"))
    val$date<-q
    }
    val$num<-0
  })
  
  output$row<-renderDataTable({
    validate(
      need(val$date, '')
    )
    datatable(  val$date,escape=F,rownames=F,
              options=list(scrollX = TRUE,columnDefs = list(list(className = 'dt-center', targets = "_all"))),selection="none"
    )
    
    })
  output$value1 <- renderPrint({  val$num }) 
  
  output$plot1<-renderHighchart({
    
    validate(
      need(val$date, '')
    )
    
    dat<-val$date
    if(input$wk==F){
    highchart() %>%
      hc_add_series(dat,"spline", hcaes(x = date, y = sales),name = "Продажи",color="green",yAxis=0)%>%
      hc_add_series(dat,"spline", hcaes(x = date, y = views),name = "Views",color="blue",yAxis=1)%>%
      hc_xAxis(type = 'datetime',title=list(text="Дата"))%>%
     # hc_yAxis(title=list(text="Продажи"))%>%
      hc_yAxis_multiples(list(
        title=list(text="Sales"),
        top = "0%",
        height = "50%",
        opposite=FALSE),
        list(
          title=list(text="Views"),
          top = "51%",
          height = "50%",
          opposite=T))%>%
      hc_title(text = dat$title[1])%>%
      hc_add_theme(hc_theme_smpl())
    }else{
      
      n1<-ISOweekday(as.Date(max(dat$date)))
      if(n1==7){dat$week <- as.numeric( format(dat$date-1, "%U"))}else{dat$week <- as.numeric( format(dat$date-(n1+1), "%U"))}
      
      dat1<-dat%>%
        group_by(week)%>%
        summarise(sls=sum(sales),
                  vws=sum(views))
      
      highchart() %>% 
        hc_add_series(dat1,"column",hcaes(x=week,y=vws),name="Views",yAxis = 0)%>%
        hc_add_series(dat1,"column",hcaes(x=week,y=sls),name="Sales",yAxis = 1)%>%
        hc_xAxis(categories=dat1$week) %>%
        hc_xAxis(title=list(text="Week"))%>%
        hc_yAxis_multiples(list(
          title=list(text="Views"),
          top = "0%",
          height = "50%",
          opposite=FALSE),
          list(
            title=list(text="Sales"),
            top = "51%",
            height = "50%",
            opposite=T))%>%
        #hc_chart(zoomType = "x") %>% 
        hc_add_theme(hc_theme_smpl())
      }
    
  })
  
  output$view7 <- renderValueBox({
    validate(need(val$date, ''))
    dat<-val$date%>%
      arrange(date)
    
    valueBox(
      glue('{dat%>%arrange(date)%>%tail(7)%>%
        summarise(sum(views))%>%as.numeric()} ({round(dat%>%arrange(desc(date))%>%slice(1:7)%>%summarise(sum(views))/dat%>%arrange(desc(date))%>%slice(8:14)%>%summarise(sum(views))*100-100,1)}%)'),"Weekly views", icon = icon("eye"),
      color = ifelse(41>40 ,"red","blue")
    )
  })
  
  output$view28 <- renderValueBox({
    validate(need(val$date, ''))
    dat<-val$date%>%
      arrange(date)
    
    valueBox(
      dat%>%arrange(date)%>%tail(28)%>%
        summarise(sum(views))%>%as.numeric(),"28 days views", icon = icon("eye"),
      color = ifelse(41>40 ,"red","blue")
    )
  })
  
  output$sales28 <- renderValueBox({
    validate(need(val$date, ''))
    dat<-val$date%>%
      arrange(date)
    
    valueBox(
      dat%>%arrange(date)%>%tail(28)%>%
        summarise(sum(sales))%>%as.numeric()  ,"28 days sales", icon = icon("cart-plus"),
      color = ifelse(41>40 ,"red","blue")
    )
  })
  output$sales7 <- renderValueBox({
    validate(need(val$date, ''))
    dat<-val$date%>%
      arrange(date)
    
    valueBox(
      glue('{dat%>%arrange(date)%>%tail(7)%>%
        summarise(sum(sales))%>%as.numeric()} ({round(dat%>%arrange(desc(date))%>%slice(1:7)%>%summarise(sum(sales))/dat%>%arrange(desc(date))%>%slice(8:14)%>%summarise(sum(sales))*100,1)-100} %)'),"Weekly sales", icon = icon("cart-plus"),
      color = ifelse(41>40 ,"red","blue")
    )
  })

  
  
  })