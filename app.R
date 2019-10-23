library(shiny)
library(leaflet)
library(DT)
library(rgdal)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # in ui
  fileInput(inputId = "filemap",
            label = "Upload map. Choose shapefile",
            multiple = TRUE,
            accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
  mainPanel(
    leafletOutput(outputId = "map")
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # in server()
    map <- reactive({
    # shpdf is a data.frame with the name, size, type and datapath
    # of the uploaded files
    shpdf <- input$filemap
    # The files are uploaded with names
    # 0.dbf, 1.prj, 2.shp, 3.xml, 4.shx
    # (path/names are in column datapath)
    # We need to rename the files with the actual names:
    # fe_2007_39_county.dbf, etc.
    # (these are in column name)
    
    # Name of the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])
    
    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    
    # Now we read the shapefile with readOGR() of rgdal package
    # passing the name of the file with .shp extension.
    
    # We use the function grep() to search the pattern "*.shp$"
    # within each element of the character vector shpdf$name.
    # grep(pattern="*.shp$", shpdf$name)
    # ($ at the end denote files that finish with .shp,
    # not only that contain .shp)
    map <- readOGR(paste(tempdirname,
                         shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                         sep = "/"
    ))
    map
  })


  output$map <- renderLeaflet({
    if (is.null(data()) | is.null(map())) {
      return(NULL)
    }
  map <- map()
  leaflet(map)%>%addTiles()%>%addPolylines(data = map)
  
})
  output$table <- renderDT(map@data)
}

# Run the application 
shinyApp(ui = ui, server = server)

