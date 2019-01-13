server <- function(input, output, session) {
  observeEvent(input$package1_info, {
    withProgress(message = "Application loading", value = 0, {
      incProgress(0.7, detail = "Loading versions")
      url <- paste("http://rdocumentation.org/api/packages/", input$package1, sep = "")
      vers <- fromJSON(txt = url)
      version1 <- vers$versions$version
      incProgress(0.3, detail = "Finishing")
      updateSelectInput(session, "packageVersion1", choices = version1, selected = "0.7.3")
    })
  })

  observeEvent(input$package2_info, {
    withProgress(message = "Application loading", value = 0, {
      incProgress(0.7, detail = "Loading versions")
      url <- paste("http://rdocumentation.org/api/packages/", input$package2, sep = "")
      vers2 <- fromJSON(txt = url)
      version2 <- vers2$versions$version
      incProgress(0.3, detail = "Finishing")
      updateSelectInput(session, "packageVersion2", choices = version2, selected = "1.8.4")
    })
  })

  package_information1 <- eventReactive(input$package_information, {
    withProgress(message = "Application loading", value = 0, {
      url <- paste("https://rdocumentation.org/api/packages/", input$package1, "/versions/", input$packageVersion1, sep = "")
      incProgress(0.7, detail = "Loading")
      dat <- fromJSON(txt = url)
      incProgress(0.3, detail = "Finishing...")
      if (is.null(dat$topics$name) | is.null(dat$topics$title)) {
        table <- data.frame("No functions in this version of the package")
        colnames(table) <- (paste(input$package1, "Functions", sep = " "))
        return(table)
      }
      else {
        packages1 <- data.frame(dat$topics$name, dat$topics$title)
        packages1 <- packages1 %>% arrange(dat$topics$name)
        colnames(packages1) <- c(paste(input$package1, "Functions", sep = " "), "Description")
        return(packages1)
      }
    })
  })

  package_information2 <- eventReactive(input$package_information2, {
    withProgress(message = "Application loading", value = 0, {
      url <- paste("https://rdocumentation.org/api/packages/", input$package2, "/versions/", input$packageVersion2, sep = "")
      incProgress(0.7, detail = "Loading")
      dat <- fromJSON(txt = url)
      incProgress(0.3, detail = "Finishing...")
      if (is.null(dat$topics$name) | is.null(dat$topics$title)) {
        table <- data.frame("No functions in this version of the package")
        colnames(table) <- (paste(input$package2, "Functions", sep = " "))
        return(table)
      }
      else {
        packages2 <- data.frame(dat$topics$name, dat$topics$title)
        packages2 <- packages2 %>% arrange(dat$topics$name)
        colnames(packages2) <- c(paste(input$package2, "Functions", sep = " "), "Description")
        return(packages2)
      }
    })
  })

  package_metrics <- eventReactive(input$package_information, {
    url <- paste("https://rdocumentation.org/api/packages/", input$package1, "/versions/", input$packageVersion1, sep = "")
    dat <- fromJSON(txt = url)

    metrics <- data.frame(
      dat$package_name, dat$version, dat$title, dat$description,
      dat$release_date, dat$license, dat$maintainer$name, dat$maintainer$email
    )
    colnames(metrics) <- c("Name", "Version", "Title", "Description", "Release Date", "License", "Maintainer", "Contact")
    metrics_data <- data.frame(t(metrics))
    colnames(metrics_data) <- "Package Metrics"

    if (nrow(metrics_data) == 0) {
      metrics <- data.frame("No package metrics")
      colnames(metrics) <- "Metrics"
      return(metrics)
    }
    else {
      metrics_data <- setDT(data.frame(metrics_data), keep.rownames = TRUE)[]
      colnames(metrics_data) <- c("Metrics", "Description")
      return(data.frame(metrics_data))
    }
  })

  package_metrics2 <- eventReactive(input$package_information2, {
    url <- paste("https://rdocumentation.org/api/packages/", input$package2, "/versions/", input$packageVersion2, sep = "")
    dat <- fromJSON(txt = url)

    metrics <- data.frame(
      dat$package_name, dat$version, dat$title, dat$description,
      dat$release_date, dat$license, dat$maintainer$name, dat$maintainer$email
    )
    colnames(metrics) <- c("Name", "Version", "Title", "Description", "Release Date", "License", "Maintainer", "Contact")
    metrics_data <- data.frame(t(metrics))
    colnames(metrics_data) <- "Package Metrics"

    if (nrow(metrics_data) == 0) {
      metrics <- data.frame("No package metrics")
      colnames(metrics) <- "Metrics"
      return(metrics)
    }
    else {
      metrics_data <- setDT(data.frame(metrics_data), keep.rownames = TRUE)[]
      colnames(metrics_data) <- c("Metrics", "Description")
      return(data.frame(metrics_data))
    }
  })

  output$package1_metrics <- renderTable({
    return(package_metrics())
  })

  output$package2_metrics <- renderTable({
    return(package_metrics2())
  })

  output$table_info_package1 <- DT::renderDataTable({
    table <- package_information1()
    table <- datatable(table, rownames = FALSE) %>% formatStyle(c(paste(input$package1, "Functions", sep = " "), "Description"), backgroundColor = "#34495E")
    table
  }, options = list(scrollX = TRUE))

  output$table_info_package2 <- DT::renderDataTable({
    table <- package_information2()
    table <- datatable(table, rownames = FALSE) %>% formatStyle(c(paste(input$package2, "Functions", sep = " "), "Description"), backgroundColor = "#34495E")
    table
  }, options = list(scrollX = TRUE))

  package_inter <- eventReactive(input$package_intersection, {
    withProgress(message = "Application loading", value = 0, {
      incProgress(0.3, detail = "Loading")
      url <- paste("https://rdocumentation.org/api/packages/", input$package1, "/versions/", input$packageVersion1, sep = "")
      dat <- fromJSON(txt = url)
      packages1 <- data.frame(dat$topics$name)
      title1 <- data.frame(dat$topics$title)

      data_package1 <- cbind(packages1, title1)

      url2 <- paste("https://rdocumentation.org/api/packages/", input$package2, "/versions/", input$packageVersion2, sep = "")
      dat2 <- fromJSON(txt = url2)
      packages2 <- data.frame(dat2$topics$name)
      title2 <- data.frame(dat2$topics$title)

      data_package2 <- cbind(packages2, title2)

      inter <- intersect(packages1$dat.topics.name, packages2$dat2.topics.name)
      inter <- data.frame(inter)

      incProgress(0.7, detail = "Finishing...")
      if (nrow(inter) == 0) {
        inter <- data.frame("No function conflicts")
        colnames(inter) <- "Conflicts"
        return(inter)
      }
      else {
        inter <- data.frame(inter)
        colnames(inter) <- "dat.topics.name"
        inter$dat.topics.name <- as.character(inter$dat.topics.name)
        data_package1$dat.topics.name <- as.character(data_package1$dat.topics.name)
        data_package1$dat.topics.title <- as.character(data_package1$dat.topics.title)
        inter2 <- inter %>% inner_join(data_package1)
        data_package2 <- data_package2 %>% plyr::rename(replace = c("dat2.topics.name" = "dat.topics.name"))
        data_package2$dat.topics.name <- as.character(data_package2$dat.topics.name)
        data_package2$dat2.topics.title <- as.character(data_package2$dat2.topics.title)
        inter2 <- inter2 %>% inner_join(data_package2)
        colnames(inter2) <- c("Conflicts", paste(input$package1, "Description", sep = " "), paste(input$package2, "Description", sep = " "))
        inter2 <- inter2 %>% arrange(Conflicts)
        return(inter2)
      }
    })
  })

  output$table_intersect <- renderTable({
    table <- package_inter()
    return(table)
  })

  output$Reference <- renderPrint({
    sessionInfo()
  })
}