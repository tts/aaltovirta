sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(
    menuItem("Scatterplot by School", tabName = "scatterplot"),
    menuItem("Data by School", tabName = "table"),
    menuItem("Compare items by School", tabName = "barchart"),
    menuItem("Field means by School", tabName = "heatmap"),
    conditionalPanel(
      condition = "input.tabs == 'scatterplot'",
      selectInput(inputId = "school",
                  label = "School", 
                  choices = c("All Schools", schools),
                  multiple = FALSE,
                  selected = "All Schools"),
      selectInput("xc", "x axis", as.list(metrics), selected = "Altmetric"),
      selectInput("yc", "y axis", as.list(metrics), selected = "Mendeley"),
      selectInput("scale", "x axis scale", as.list(scales), selected = "linear"),
      tags$div(class="form-group shiny-input-container",
               HTML("<p>Altmetrics ", datadate, " by Altmetric</p>
                    <p>Citations ", citedate, " by WoS</p>
                    <p>Citations, tweets, and Mendeley readers also as of ", datadate2017, "</p>
                    <p>OA fulltext link via <a ref='https://oadoi.org/'>oaDOI</a>", oadate, ". Shown as a <span style='color:gold'><b>golden</b></span> stroke in scatterplot. Links in data</p>")
        )
      ),
    conditionalPanel(
      condition = "input.tabs == 'barchart'",
      selectizeInput(inputId = 'items', 
                     label = 'Items', 
                     choices = NULL, 
                     options = list(maxItems = 5,
                                    placeholder = 'Select max 5'))),
    conditionalPanel(
      condition = "input.tabs == 'table'",
      tags$div(class="form-group shiny-input-container", 
               HTML("<p><i>Citations2017</i>, <i>Tweets2017</i> and <i>Mendeley2017</i> are values as of May 2017. 
                    Compared to last year, 2016, decline is shown as an <span style='color:#fc8d59'><b>orange</b> cell</span>,
                    no change as <span style='color:#ffffbf'><b>yellow</b></span>, and growth as <span style='color:#91cf60'><b>green</b></span>.</p>
                    <p>In practice, decline is only possible due to missing data.</p>
                    <p><i>CitesSeries</i>, <i>TweetsSeries</i> and <i>MendeleySeries</i> represent these two data points as a sparkline.</p>
                    <p>With <b>Column visibility</b>, hide columns by clicking.</p>
                    <p><i>Title</i> is abbreviated to save space. Hover over it to see the whole string in tooltip.</p>
                    <p>First three columns remain fixed in horizontal scrolling.</p>")
      )),
    id = "tabs"
  )
)


body <- dashboardBody(

  tabItems(
    
    tabItem("scatterplot",
            fluidRow(
              column(
                width = 8,
                box(title = "Scatterplot by School",
                    status = "success",
                    solidHeader = TRUE,
                    width = "100%",
                    height = "700px",
                    ggvisOutput("gv"))
              ),
              column(
                width = 4,
                valueBoxOutput("nrofpubl", width = "100%"),
                valueBoxOutput("nrofitemswithmetrics", width = "100%"),
                valueBoxOutput("nrofoadoi", width = "100%"),
                valueBoxOutput("maxaltmetrics", width = "100%"),
                valueBoxOutput("maxmendeley", width = "100%"),
                valueBoxOutput("maxtwitter", width = "100%")
            ))
    ),
    
    tabItem("table",
            fluidRow(
              column(width = 12,
                     height = "600px",
                     sparklineOutput("spark"),
                     DT::dataTableOutput("datatable", 
                                         width = "100%",
                                         height = "600px"))
              )
    ),
    
    tabItem("barchart",
            fluidRow(
              column(width =  12,
                     height = "600px",
                     showOutput("chart", "nvd3"),
                     HTML('<style>.rChart {width: 100%; height: 400px}</style>'))
              )),
    
    tabItem("heatmap",
            fluidRow(
              column(width = 12,
                     plotlyOutput("fields")
                     )
            )
    )
    
  ))


dashboardPage(
  dashboardHeader(title = "(Alt)metrics of Aalto University publications in VIRTA",
                  titleWidth = "500"),
  sidebar,
  body,
  skin = "black"
)

