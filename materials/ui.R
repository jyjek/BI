library(shiny)
library(shinymaterial)
library(shinyjs)
library(DT)
library(shinyBS)
library(dplyr)
library(lubridate)
library(shinyWidgets)
library(glue)
library(highcharter)
library(plotly)
library(sunburstR)

options(shiny.sanitize.errors = FALSE)
ui <- material_page(
  title = "",
  nav_bar_fixed = TRUE,
  include_fonts = T,
  nav_bar_color = "teal lighten-1",
  material_side_nav(
    image_source = "side_nav.jpeg",
    material_row(
      material_column(
        width = 7,
        material_radio_button(
          "adjType",
          label = "",
          choices = c("Percent Change" = "close_p", "Price in $USD" = "close")
        )
      ),
      material_column(
        width = 5,
        material_radio_button(
          "day_back",
          label = "",
          choices = c("1 Week" = 7, "1 Month" = 30, "1 Year" = 365)
        )
      )
    ),
    tags$br(),
    material_row(
      material_column(
        offset = 1,
        width = 5,
        material_switch("BTC", 
                        label = "Bitcoin",
                        initial_value = TRUE,
                        color = my_col$BTC)
      ),
      material_column(
        width = 5,
        material_switch("BCH",
                        label = "Bitcoin Cash", 
                        initial_value = TRUE,
                        color = my_col$BCH)
      )
    ),
    material_row(
      material_column(
        offset = 1,
        width = 5,
        material_switch("ETH",
                        label = "Ethereum", 
                        initial_value = TRUE,
                        color = my_col$ETH)
      ),
      material_column(
        width = 5,
        material_switch("LTC", 
                        label = "Litecoin",
                        initial_value = TRUE,
                        color = my_col$LTC)
      )
    ),
    tags$br(),
    tags$br(),
    tags$br(),
    material_row(
      material_column(
        width = 10,
        offset = 1,
        HTML("<a href='https://github.com/ericrayanderson/shinymaterial_crypto/' target='_blank'> app code <i class='material-icons'>open_in_new</i></a>")
      )
    ),
    material_row(
      material_column(
        width = 10,
        offset = 1,
        HTML("<a href='https://ericrayanderson.github.io/shinymaterial/' target='_blank'> shinymaterial package <i class='material-icons'>open_in_new</i></a>")
      )
    )
  ),
  tags$br(),
  tags$div(id = 'wholeApp', style = "opacity:0",
           material_row(
             material_column(width=2),
             uiOutput("coinCards")
           ),
           material_row(
             material_column(
               width = 8,
               offset = 2,
               material_card(
                 depth = 3,
                 ggvisOutput("crypto"),
                 uiOutput('hide_gear')
               )
             )
           )
  )
)



