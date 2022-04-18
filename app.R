library(shiny)
library(reactable)
library(magrittr)
library(ggplot2)
library(safetyData)
library(Tplyr)
library(dplyr)
library(purrr)
library(rlang)
adsl <- adam_adsl %>%
  mutate(
    TRT01A = ordered(TRT01A, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
  )

tab <- tplyr_table(adsl, TRT01A) %>%
  add_layer(
    group_count(SEX, by = "Sex n (%)")
  ) %>%
  add_layer(
    group_desc(AGE, by = "Age (years)")
  ) %>%
  add_layer(
    group_count(RACE, by = "Race n (%)")
  )

b_tab <- build(tab, metadata = TRUE) %>%
  apply_row_masks() %>%
  select(row_id, starts_with("row"), starts_with("var")) %>%
  relocate(row_id, row_label1, row_label2, var1_Placebo, `var1_Xanomeline Low Dose`, `var1_Xanomeline High Dose`)


get_metadata_filters <- function(tab, row, col) {
  req(row(), col())
  tmp <- tab$metadata %>%
    filter(row_id == row()) %>%
    select(col()) %>%
    extract2(1) %>%
    extract2(1)
  
  tmp
}

ui <- fillPage(
  column(8,
         reactableOutput("demoTab")
  ),
  
  column(4,
         plotOutput("AEBySubGroup"),
         plotOutput("LabsBySubGroup")
  )
)



server <- function(input, output) {
  
  row <- reactive(b_tab[input$row$index,1]$row_id)
  col <- reactive(input$col$column)
  
  output$demoTab <- renderReactable(
    reactable(
      select(b_tab, -row_id, -starts_with("ord")),
      sortable = FALSE,
      onClick = JS("function(rowInfo, colInfo) {
                      if (window.Shiny) {
                        Shiny.setInputValue('row', { index: rowInfo.index + 1 })
                        Shiny.setInputValue('col', { column: colInfo.id })
                        }
                    }"),
      height = 450,
      defaultPageSize = 11,
      columns = list(
        row_label1 = colDef(name = ""),
        row_label2 = colDef(name = ""),
        var1_Placebo = colDef(name = "Placebo"),
        `var1_Xanomeline Low Dose` = colDef(name = "Xano Low"),
        `var1_Xanomeline High Dose` = colDef(name = "Xano High")
      )
    )
  )
  
  meta_filters <- reactive({
    req(row, col)
    get_metadata_filters(tab, row, col)
  })
  
  f_usubjid <- reactive({
    req(meta_filters)
    tmp <- tab$target %>%
      filter(!!!meta_filters()$filters) %>%
      extract2("USUBJID")
    
    tmp
  })
  
  f_tab_ae <- reactive({
    req(f_usubjid())
    adam_adae %>%
      filter(USUBJID %in% f_usubjid(), AEBODSYS %in% c("GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
                                                       "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
                                                       "CARDIAC DISORDERS",
                                                       "NERVOUS SYSTEM DISORDERS"))
  })
  
  f_tab_labs <- reactive({
    req(f_usubjid())
    adam_adlbc %>%
      filter(USUBJID %in% f_usubjid(), PARAM %in% c("Protein (g/L)", "Albumin (g/L)"))
  })
  
  output$AEBySubGroup <- renderPlot({
    req(f_tab_ae())
    f_tab_ae() %>%
      ggplot(aes(x = AEBODSYS, fill = AEBODSYS)) +
      geom_bar() +
      theme(
        legend.position = "right",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()
      ) +
      labs(
        title = "Selected AEs by subgroup",
        y = "Frequency of AEs",
        x = "Body System"
      )
  })
  
  output$LabsBySubGroup <- renderPlot({
    req(f_tab_labs())
    f_tab_labs() %>%
      ggplot(aes(y = AVAL, x = ADY, group = PARAM, color = PARAM)) +
      geom_smooth()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
