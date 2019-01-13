library(jsonlite)
library(curl)
library(data.table)
library(dplyr)
library(DT)

ui <- navbarPage(
  "Package Function Conflicts",
  tabPanel(
    "Package Descriptions and Conflicts",
    fluidPage(
      titlePanel("Find Function Name Conflicts in R Packages"),
      h5(
        "This application finds package function description and function name conflicts 
     between two packages using the R documentation API. Specify package name and click 
     on", tags$i("Find Package Version"), "to get a list of possible package versions. Choose a version and 
     click", tags$i("Show Package Functions"), "to find package metrics and list of functions with description for each package. Click",
        tags$i("Show Function Conflicts"), "to get a list and description of functions with same name between two packages.
     "
      ),
      tags$head(
        tags$style(HTML("
                    body {
                    background-color: #34495E;
                    color: #ffffff; 
                    }
                    "))
      ),
      tags$style(
        type = "text/css",
        ".shiny-output-error { visibility: hidden; }",
        ".shiny-output-error:before { visibility: hidden; }"
      ),
      hr(),
      fluidRow(
        column(
          5,
          fluidRow(column(6, h5("Specify First Package"))),
          fluidRow(
            column(
              6,
              textInput("package1", label = NULL, value = "dplyr"),
              actionButton(offset = 0.2, "package1_info", "Find Package Version", style = "color: #ffffff;background-color: #212F3D;")
            ),
            column(
              4,
              fluidRow(
                column(
                  8,
                  selectInput("packageVersion1", label = NULL, choices = "")
                ),
                column(
                  7,
                  actionButton("package_information", "Show Package Functions", style = "color: #ffffff;background-color: #212F3D;")
                )
              )
            )
          )
        ),
        column(
          5, offset = 2,
          fluidRow(column(6, h5("Specify Second Package"))),
          fluidRow(
            column(
              6,
              textInput("package2", label = NULL, value = "plyr"),
              actionButton("package2_info", "Find Package Version", style = "color: #ffffff;background-color: #212F3D;")
            ),
            column(
              4,
              fluidRow(
                column(
                  8,
                  selectInput("packageVersion2", label = NULL, choices = "")
                ),
                column(
                  7,
                  actionButton("package_information2", "Show Package Functions", style = "color: #ffffff;background-color: #212F3D;")
                )
              )
            )
          )
        )
      ),
      HTML("<br/>"),
      hr(),
      fluidRow(
        column(
          2, offset = 5,
          actionButton("package_intersection", "Show Function Conflicts", style = "color: #ffffff;background-color: #212F3D;")
        )
      ),
      HTML("<br/>"),
      column(
        2,
        h4("Metrics"),
        tableOutput("package1_metrics"),
        hr(),
        hr(),
        h4("Functions"),
        DT::dataTableOutput("table_info_package1", width = 325)
      ),
      column(
        2, offset = 3,
        h4("Function Conflicts"),
        tableOutput("table_intersect")
      ),
      column(
        2, offset = 3,
        h4("Metrics"),
        tableOutput("package2_metrics"),
        hr(),
        hr(),
        h4("Functions"),
        DT::dataTableOutput("table_info_package2", width = 325)
      )
    )
  ),
  tabPanel(
    "References",
    verbatimTextOutput("Reference")
  )
)