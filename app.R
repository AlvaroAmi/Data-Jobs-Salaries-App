
#PART 3
library(ggplot2)
library(shiny)

load("RDataDeliverable2.RData")

ui <- fluidPage(
  tags$head( #Put internet image as background using HTML
    tags$style(
      HTML(
        "
        body {
          background-image: url('https://wallpaperswide.com/download/light_background-wallpaper-3840x2400.jpg');
          background-size: cover;
          background-position: center;
          background-repeat: no-repeat;
        }
        "
      )
    )
  ),
  tags$h1("Data Jobs Salary Distribution"),
  tags$p("Possible experience level values: ", tags$strong(paste(unique(file$experience_level), collapse = ", "))),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("companySizes", "Company Size", choices = sort(unique(file$company_size)), selected = unique(file$company_size)),
      sliderInput("remoteRatioRange", "Remote Ratio Range", min = 0, max = 100, value = c(0, 100)),
      textInput("experienceLevelInput", "Search by Experience Level", placeholder = "Enter experience levels separated by commas"),
      actionButton("updateButton", "UPDATE")
    ),
    mainPanel(
      plotOutput("salaryPlot")
    )
  ),
  tags$a(href= "https://www.kaggle.com/datasets/lorenzovzquez/data-jobs-salaries/", "Link to the used dataset")
)

server <- function(input, output) {
  
  buttonClicked <- reactiveVal(FALSE) #Check if the button has been clicked at least once
  
  filteredData <- eventReactive(input$updateButton | !buttonClicked(), { #Filter data based in remote ratio range
    buttonClicked(TRUE) 
    temp <- file[file$company_size %in% input$companySizes & 
                   file$remote_ratio >= input$remoteRatioRange[1] & 
                   file$remote_ratio <= input$remoteRatioRange[2], ]
    
    if (!is.null(input$experienceLevelInput) && input$experienceLevelInput != "") {
      levels_to_search <- strsplit(input$experienceLevelInput, ",")[[1]] # Separate the experience levels by "," (no spaces)
      temp <- temp[temp$experience_level %in% trimws(levels_to_search, "both"), ] #Filter the data to include the selected experience levels
    }
    
    temp
  })
  
  output$salaryPlot <- renderPlot({
    ggplot(filteredData(), aes(x = salary_in_usd)) +
      geom_histogram(binwidth = 10000, fill = "skyblue", color = "black", boundary = 0) +
      labs(title = "Salary Distribution", x = "Salary (USD)", y = "Frequency")
  })
  
}

shinyApp(ui = ui, server = server)
