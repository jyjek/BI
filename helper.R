
dat=q

q%>%
  magrittr::set_colnames(c("id","qnt","cost","coup","date","parent","title_p","title"))%>%
  mutate(date=ymd(date),
         mt=if_else(!is.na(coup),1,0))%>%
  group_by(mt)%>%
  summarise(sums=sum(cost),
            qnt=sum(qnt))%>%.[which(.$mt==1),"sums"]/sum(.$sums)


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
 