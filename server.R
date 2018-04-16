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
library(viridis) 
library(plotly)
library(sunburstR)
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
  
  output$sidebarUserPanel <-  renderUI({
    
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
     validate(need(input$SKU!="", 'Choose SKU'))
   actionButton("ok","Start processing",class='btn-info')
 })
 
 output$ab1= renderUI({
   validate(
     need(input$cat!="", 'Choose category')
   )
   actionButton("ok1","Start processing",class='btn-info')
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
                 (SELECT   merchandises.product.id as product_id	, date(created) as data ,count( merchandises.quantity ) as sum_30
                    FROM [rozetka-com-ua:rozetka_orders.orders]
                    where  created> DATE_ADD(CURRENT_DATE(), -28, 'DAY') 
                    and  status = 'complete' and merchandises.status ='staffed'
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
# Category ####
  
  observeEvent(input$ok1,{
    val$num1<-1
    if( val$num1!=0){
      sql1<-sprintf("select a.*,b.parent_id,b.producer_title,b.title
          from
          (SELECT   merchandises.product.id,merchandises.quantity,merchandises.cost_with_discount,merchandises.coupon.title,
                  date(created) as date 
          FROM flatten([rozetka-com-ua:rozetka_orders.orders] ,merchandises)
              where  created> DATE_ADD(CURRENT_DATE(), -28, 'DAY') 
                    and  status = 'complete' and merchandises.status ='staffed' ) a 
           join(
           SELECT id,title,parent_id,producer_title FROM [rozetka-com-ua:rozetka_products.products] 
           where parent_id=%s
           ) b on a.merchandises.product.id=b.id",input$cat)
      
      q<-query_exec(sql1, project = project,max_pages = Inf)
      val$cat<-q
    }
    val$num1<-0
  })
  
  output$category<- DT::renderDataTable({
    val$cat 
  },
    server=FALSE,
   # validate(need(!is.null(val$cat), ''))
   escape=F,rownames=F,class = 'cell-border compact',
                colnames = c("SKU","Кількість","Вартість","Промо-код","Дата","Category ID","Виробник","Назва"),
                extensions = c('Scroller','Buttons'),
                options=list( dom = 'Bfrtip',buttons = c('csv', 'excel'),scrollX = TRUE,columnDefs = list(list(className = 'dt-center', targets = "_all"))),selection="none"
    
    
  )
  
  output$plotly<-renderPlotly({
    validate(need(!is.null(val$cat), ''))
  val$cat%>%
              magrittr::set_colnames(c("id","qnt","cost","coup","date","parent","title_p","title"))%>%
              mutate(date=ymd(date))%>%
              group_by(title_p)%>%
              summarise(sums=sum(cost),
                        qnt=sum(qnt))%>%
    plot_ly(labels = ~title_p, values = ~sums)%>%
              add_pie(hole = 0.6) %>%
      layout(title = 'Доля бренду у категорії',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  output$tree<-renderHighchart({
    validate(need(!is.null(val$cat), ''))
    hchart( val$cat%>%
              magrittr::set_colnames(c("id","qnt","cost","coup","date","parent","title_p","title"))%>%
              mutate(date=ymd(date))%>%
              group_by(title_p)%>%
              summarise(sums=sum(cost),
                        qnt=sum(qnt)), "treemap", hcaes(x = title_p, value = sums/10**3, color = qnt))%>%
      hc_tooltip(pointFormat = "<b>{point.title_p}</b>:<br>
                Об'єм продажів: {point.value:,.0f} тис.грн<br>
                Оборотність: {point.qnt:,.0f} шт.")%>%
      hc_title(text = "Структура продажів")%>% 
      hc_colorAxis(stops = list_parse2(data.frame(q = 0:10/10,
                                                  c = substring(viridis(10 + 1), 0, 7),
                                                  stringsAsFactors = FALSE)))
    
  })
  
  output$proc <- renderValueBox({
    validate(need(!is.null(val$cat), ''))
    r<-val$cat%>%
      magrittr::set_colnames(c("id","qnt","cost","coup","date","parent","title_p","title"))%>%
      mutate(date=ymd(date),
             mt=if_else(!is.na(coup),1,0))%>%
      group_by(mt)%>%
      summarise(sums=sum(cost),
                qnt=sum(qnt))%>%.$sums
    
    valueBox( if(length(r)>1) {round(r[2]/r[1]*100,1)}else{0}  ,"Відсоток акційних продажів", icon = icon("percent"),
      color = ifelse(41>40 ,"red","blue")
    )
  })
  
  output$subs<-renderSunburst({
    validate(need(!is.null(val$cat), ''))
    val$cat%>%
      magrittr::set_colnames(c("id","qnt","cost","coup","date","parent","title_p","title"))%>%
      mutate(date=ymd(date))%>%
      group_by(title,title_p)%>%
      summarise(cost=sum(cost))%>%
      select(title_p,title,cost)%>%ungroup()%>%
      mutate(title=gsub("-"," ",title),
             nm=glue('{title_p}-{title}'))%>%
      select(nm,cost)%>%
      sunburst(width="100%",height = "100%", legend = FALSE)
    
  })
  
  
  })