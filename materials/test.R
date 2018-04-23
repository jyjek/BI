library(shiny)
library(shinymaterial)

# Wrap shinymaterial apps in material_page
ui <- material_page(
  title = "title",
  nav_bar_fixed = F,
  # Place side-nav in the beginning of the UI
  material_side_nav(
    fixed = F,
    # Place side-nav tabs within side-nav
    material_side_nav_tabs(
      side_nav_tabs = c(
        "Example Side-Nav Tab 1" = "example_side_nav_tab_1",
        "Example Side-Nav Tab 2" = "example_side_nav_tab_2"
      ),
      icons = c("cast", "insert_chart")
    )
  ),
  # Define side-nav tab content
  material_side_nav_tab_content(
    side_nav_tab_id = "example_side_nav_tab_1",
    
    material_row(
      material_column(
        width = 3,
        material_number_box("SKU", "Input SKU:",initial_value=0,min_value = 2,max_value = 10**8,
                            color = "red"),
        material_switch(input_id="wk", label= "", off_label = "Daily", on_label = "Weekly",
                        initial_value = FALSE, color = NULL)
        
      )),
    material_row(
      material_column(
        width = 3,
        material_card(
          title = "Weekly views",depth=2,icon("cart-plus","fa-2x"),
          h3(glue::glue("500"))
         
        )
      ),
      material_column(
        width = 3,
        material_card(tags$style(".card-content {background-color:#C2FDD4}"),icon("eye","fa-2x"),
          title = "Weekly views",depth=2,
          h3(glue::glue("500"))
          
        )
          )
      )
  
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "example_side_nav_tab_2",
    tags$h1("Second Side-Nav Tab Content")
  )
)

server <- function(input, output) {
  
}
shinyApp(ui = ui, server = server)