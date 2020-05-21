library(shiny)
library(DT)

dat <- iris[c(1,2,3,51,52,53,101,102,103), c(5,1,2,3,4)]

ui <- fluidPage(
  DTOutput("table")
)

server <- function(input, output){
  output[["table"]] <- renderDT({
    dtable <- datatable(dat, rownames = FALSE, 
                        options = list(
                          rowsGroup = list(0) # merge cells of column 1
                        ))
    path <- "yousuf/dataTables.rowsGroup.js" # folder containing dataTables.rowsGroup.js
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
  })
}

shinyApp(ui, server)