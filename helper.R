
dat=q

r=q%>%
  magrittr::set_colnames(c("id","qnt","cost","coup","date","parent","title_p","title"))%>%
  mutate(date=ymd(date),
         mt=if_else(!is.na(coup),1,0))%>%
  group_by(mt)%>%
  summarise(sums=sum(cost),
            qnt=sum(qnt))%>%.$sums
r[2]/r[1]*100

r=dat%>%
  magrittr::set_colnames(c("id","qnt","cost","coup","date","parent","title_p","title"))%>%
  mutate(date=ymd(date))%>%
  group_by(title_p)%>%
  summarise(sm=sum(cost),
            mn=sum(cost)/sum(qnt))


library(plotly)
 plot_ly(q1, labels = ~title_p, values = ~sums, type = 'pie') %>%
  layout(title = 'Доля бренду у категорії',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

 
library(viridis) 

 hchart( q1, "treemap", hcaes(x = title_p, value = sums/10**3, color = qnt))%>%
   hc_tooltip(pointFormat = "<b>{point.title_p}</b>:<br>
                Об'єм продажів: {point.value:,.0f} тис.грн<br>
                Оборотність: {point.qnt:,.0f} шт.")%>%
   hc_title(text = "Структура продажів")%>% 
   hc_colorAxis(stops = list_parse2(data.frame(q = 0:10/10,
                                               c = substring(viridis(10 + 1), 0, 7),
                                               stringsAsFactors = FALSE)))
 
 
 
 dat1%>%
   group_by(title,title_p)%>%
   summarise(cost=sum(cost))%>%
   select(title_p,title,cost)%>%ungroup()%>%
   mutate(title=gsub("-"," ",title),
          nm=glue('{title_p}-{title}'))%>%
   select(nm,cost)%>%
   sunburst()

 cts[which(cts$parent_title=="Иконы"),1]
 cts%>%arrange(parent_title)%>%select(parent_title)%>%unique()
 
 
 x=84397778
 x1=85514
 x2=1568
 to_norm<-function(x){
   if(x<10**3){
     x<-x
     y="грн"}
   if(x>10**3 & x<10**6){
     x<-round(x/10**3,2)
     y="тис. грн"}
   if(x>10**6){
     x<-round(x/10**6,2)
     y="млн. грн"}
   res<-list(a=x,b=y)
   return(res)
 }
to_norm(x2) 
  

library(echarts4r)

df <- data.frame(
  parent = c("earth", "earth", "earth", "mars", "mars"), 
  child = c("forest", "ocean", "iceberg", "elon", "curiosity"),
  value = ceiling(rnorm(5, 10, 2))
)

df %>% 
  e_charts() %>% 
  e_sunburst(parent, child, value) %>% 
  e_title("Sunburst")


dt1<-dat1%>%
  group_by(title_p,title)%>%
  summarise(sm=sum(qnt*cost))

dt1 %>% 
  e_charts() %>% 
  e_treemap(title_p,sm) %>% 
  e_title("Graph")


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


dat %>% 
  e_charts(date) %>% 
  e_line(views) %>%
  e_line(sales, name = "Sales", x.index = 1) %>% # second y axis 
  e_tooltip(trigger = "axis")%>%
  e_datazoom(type = "slider")

USArrests %>% 
  dplyr::mutate(
    State = row.names(.),
    Rape = -Rape
  ) %>% 
  e_charts(State) %>% 
  e_area(Murder) %>%
  e_bar(Rape, name = "Sick basterd", x.index = 1) %>% # second y axis 
  e_mark_line("Sick basterd", data = list(type = "average")) %>% 
  e_mark_point("Murder", data = list(type = "min"))%>%
  e_tooltip(trigger = "axis")

USArrests %>%
  dplyr::mutate(State = row.names(.)) %>%
  e_charts(Assault) %>%
  e_line(Murder) %>%
  e_line(UrbanPop,  y.index = 1) %>%  # second y axis
  e_tooltip(trigger = "axis")



USArrests %>%
  e_charts(Assault) %>%
  e_line(Murder, smooth = TRUE) %>%
  e_line(Rape, y.index = 1) %>% # add secondary axis
  e_y_axis(index = 1, show = T)
USArrests %>%
  head(10) %>%
  dplyr::mutate(State = row.names(.)) %>%
  e_charts(State) %>%
  e_area(Murder) %>%
  e_x_axis(axisLabel = list(interval = 0, rotate = 45))   


data.frame(x=LETTERS[1:5],y=1:5,
               z=6:10)%>%
  e_charts(x)%>%
  e_line(y)%>%
  e_line(z, y.index = 1)



df=data.frame(x=LETTERS[1:5],y=1:5,
           z=6:10)
highchart() %>% 
  hc_add_series(df,"line",hcaes(x=x,y=y),yAxis = 0)%>%
  hc_add_series(df,"line",hcaes(x=x,y=z),yAxis = 1)%>%
   hc_yAxis_multiples(list(
    top = "0%",
    height = "50%",
    opposite=FALSE),
    list(top = "51%",
      height = "50%",
      opposite=T))

data.frame(x = LETTERS[1:5],
           y=1:5,
           z = 6:10,
           w = rnorm(5, 4, 1),
           e = rnorm(5, 5, 2)
) %>%
  e_charts(x) %>%
  e_bar(y, stack = "stack") %>%
  e_bar(z, stack = "stack") %>%
  e_bar(w , stack = "grp2") %>%
  e_bar(e, stack = "grp2")

data.frame(x=LETTERS[1:5],
           y=1:5,
           z=6:10
) %>%
  e_charts(x) %>%
  e_line(y, gridIndex=0) %>%
  e_line(z, gridIndex=1)


data.frame(x = LETTERS[1:5],
           y=1:5,
           z = 6:10,
           w = rnorm(5, 4, 1),
           e = rnorm(5, 5, 2)
) %>%
  e_charts(x) %>%
  e_bar(y, stack = "stack") %>% # defaults to y.index = 0
  e_bar(z, stack = "stack") %>% # defaults to y.index = 0
  e_bar(w , stack = "grp2", y.index = 1) %>% # secondary axis + stack
  e_bar(e, stack = "grp2", y.index = 1)


dat%>%
  arrange(date)%>%tail(7)%>%
  summarise(sum(views))%>%as.numeric()
