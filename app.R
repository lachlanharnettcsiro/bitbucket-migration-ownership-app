library(shiny)
library(DT)
library(bslib)
library(blastula)
library(dplyr)
library(shinyjs)
library(openxlsx)



email_creds <- creds_anonymous(host = "smtp-relay.csiro.au", port = 25, use_ssl = FALSE)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

create_mailto <- function(
    to,
    subject,
    body
) {
  paste0(
    "mailto:", to,
    "?subject=", utils::URLencode(subject, reserved = TRUE),
    "&body=", utils::URLencode(body, reserved = TRUE)
  )
}

build_text_email <- function(params) {
  
  paste(
    "Hi Bitbucket Migration Team,",
    "",
    "The following repository IDs have been claimed:",
    "",
    render_text_table(params$BB_IDs, params$BB_Repos),
    "",
    sprintf(
      "The user has provided %s as the contact for these repositories.",
      params$user %||% "<not provided>"
    ),
    "",
    "The following details have been included in the claim:",
    "",
    params$message %||% "No additional message provided.",
    "",
    sep ="\n"
  )
}



render_text_table <- function(ids, repos) {
  if (length(ids) == 0) {
    return("No repository IDs were selected.")
  }
  
  table_output <-knitr::kable(
    data.frame(BB_ID = ids, BB_Repo = repos),
    format = "simple"
  )
  
  paste(table_output, collapse = "\n")
}


#' Sends email for bitbucket repo claims
#'
#' @description
#' @param from Who is making the claim
#' @param params Params for template
send_email <- function(from, email_params) {
  
  email <- render_connect_email(
    input = "bitbucket_repository_claim_email.Rmd",
    render_options = list(params = email_params),
    connect_footer = FALSE
  )
  
  smtp_send(
    email = email,
    to = "schelp@csiro.au",
    from = from,
    subject = "Bitbucket Repositories Migration Claim",
    credentials = email_creds
  )
}

ui <- page_fluid(
    useShinyjs(),
  
    theme = bs_theme(version = 5),
  
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
  
  
    # Application title
    br(),
    card(class = "title-container",
      titlePanel("Bitbucket Migration - Repository Ownership", "Bitbucket Migration Ownership"),
    ),
    
    layout_columns(
      card(
        card_header("Project Information"),
          card_body(
            p(strong(
              "
              This application streamlines the process of identifying and claiming orphaned Bitbucket repositories as part of the migration to GitHub. Confirming ownership is an essential step to ensure a smooth and accurate transition.
              "
            )),
            p(
              "
              To claim a repository, simply follow the instructions in the right-hand panel. Once completed, a ticket will be submitted to Scientific Computing to update the records accordingly.
              "
            ),
            p(
              "The information displayed is based on a review of exported Bitbucket data and other available sources related to repositories and identities. Its purpose is to link each repository to the correct individual or research unit. Please note that this report is generated automatically, so occasional inconsistencies or errors may occur.",
              br(),
              br(),
              "Thank you for supporting the migration to GitHub."
            ),
          )
      ),
      
      if (file.exists("shiny-server/.deployment")) {
        tagList(
          card(
            card_header("How to Claim Repositories"),
            card_body(
              tags$ol(
                tags$li("Select the rows for the repositories you want to claim from the table."),
                tags$li(
                  "Enter the details of the ownership changes in the field provided below, including:",
                  tags$ul(
                    tags$li("New owner's ident (if available) or preferred first and last name"),
                    tags$li("Research unit")
                  )
                ),
                tags$li("Click", strong("Generate Email"), " to generate an email to send to the Scientific Computing Help team")
              ),
                div(class = "center-inputs",
                    textInput("userEmail", NULL, placeholder = "Enter a contact email for the repos"),
                    textAreaInput("userMessage", NULL, placeholder = "Enter associated information"),
                    actionButton("claim", "Generate Email"))
              )
            )
          )
        
      } else {
        tagList(
          card(
            card_header("How to Claim Repositories"),
            card_body(
              tags$ol(
                tags$li("Select the rows for the repositories you want to claim from the table."),
                tags$li(
                  "Enter the details of the ownership changes in the field provided below, including:",
                  tags$ul(
                    tags$li("New owner's ident (if available) or preferred first and last name"),
                    tags$li("Research unit")
                  )
                ),
                tags$li("Click ", strong("Submit Ticket"), " to send your request to the Scientific Computing Help team.")
              ),
              div(class = "center-inputs",
                  textInput("userEmail", NULL, placeholder = "Enter a contact email for the repos"),
                  textAreaInput("userMessage", NULL, placeholder = "Enter associated information"),
                  actionButton("claim", "Submit Ticket")
              )
            )
        )
        )
      }
    ),
    
    card(
      class = "info-table",
      height = "800px",
      card_header(
        "Table of Bitbucket Projects",
        class = "d-flex justify-content-between align-items-center",
        div(
          tags$span(
            style = "margin-right: 8px; cursor: pointer;",
            `data-bs-toggle` = "tooltip",
            `data-bs-placement` = "left",
            title = "Select Download Excel for a copy of your search results",
            icon("circle-info")
          ),
          downloadButton("downloadExcel", "Download Excel", class = "btn-sm")
        )
      ),
      card_body(
        min_height = "700px",
        dataTableOutput("infoTable")
      )
    )
)


server <- function(input, output, session) {
  
  # Initialise Bootstrap tooltips
  runjs("
        var tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]'))
        vartooltipList = tooltipTriggerList.map(function (tooltipTriggerE1) {
        return new bootstrap.Tooltip(tooltipTriggerE1)
        })
        ")
  
    # Autofills the email field with session$user
    if (!file.exists("shiny-server/.deployment")) {
      observe({
        updateTextInput(session, "userEmail", value = session$user)
        shinyjs::disable("userEmail")
        
      })
    }
  
    session$allowReconnect(TRUE)
  
    # Getting 403 Forbidden?
    data <- read.csv("https://bbmigration-001.it.csiro.au/index_bu.csv", sep=";")
    # data <- read.csv("index_bu.csv")
    
    # Rename 4th column to "Possible_RU
    colnames(data)[4] <- "Possible_RU"
    
    data_original <- data
        
    data$Repo_Info_Link <- ifelse(
      grepl("^https?://", data$Repo_Info_Link),  
      sprintf("<a href='%s' target='_blank'>%s</a>", 
              data$Repo_Info_Link,
              data$Repo_Info_Link),
      data$Repo_Info_Link                         
    )
    
    
    output$infoTable <- renderDataTable({
      datatable(data, selection = "multiple", escape = FALSE, rownames = FALSE, filter = "top")
    }, server = FALSE)
    
    
    # Header style for Excel export
    headerStyle <- createStyle(
      fontColour = "#FFFFFF",
      fgFill = "#366092",
      textDecoration = "bold",
      halign = "center",
      valign = "center",
      wrapText = TRUE
    )
    
    # Download handler for filtered data
    output$downloadExcel <- downloadHandler(
      filename = function() {
        paste0("bitbucket_migration_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        # Get filtered rows from data table
        filtered_data <- data_original
        
        if (!is.null(input$infoTable_rows_all)) {
          filtered_data <- data_original[input$infoTable_rows_all, ]
        }
        
        # Remove the last column (notes)
        filtered_data <- filtered_data[, -ncol(filtered_data)]
        
        # Create workbook and add data
        wb <- createWorkbook()
        addWorksheet(wb, "Data")
        writeData(wb, "Data", filtered_data)
        
        # Apply header style to all columns
        addStyle(wb, "Data", headerStyle, rows = 1, cols = 1:ncol(filtered_data))
        
        # Adjust column widths automatically
        setColWidths(wb, "Data", cols = 1:ncol(filtered_data), widths = "auto")
        
        # Save workbook
        saveWorkbook(wb, file)
        
      }
    )
    
    
    if (exists("session")) {
      usr <- session$user
    } else {
      usr <- NULL
    }
      
    if (!is.null(usr) && nzchar(usr)) {
      logged_in <- usr
    } else {
      logged_in <- "Unknown User"
    }
  
    
    observeEvent(input$claim, {
      selected_rows <- input$infoTable_rows_selected 
      
      if (length(selected_rows) == 0) {
        showNotification("Please select at least one repository to claim.", type = "error")
        return()
      }
      
      ids_claimed <- data[selected_rows, "BB_ID"]
      repos_claimed <- data[selected_rows, "BB_Repo"]
      
      from_addr <- input$userEmail
      
      email_params <- list(
        BB_IDs = ids_claimed,
        BB_Repos = repos_claimed,
        message = input$userMessage,
        user = from_addr,
        logged_in_user = logged_in
      )
      
      
      if (is.null(from_addr) || from_addr == "") {
        showNotification("Please enter an email address.", type = "error")
        return()
      }
      
      render_only <- file.exists("shiny-server/.deployment")
      
      if (!render_only) {
        send_email(
          from = from_addr,
          email_params = email_params
        )
        
        showNotification("Claim submitted! An email has been sent to the SC Help Team.", type = "message")
      
      
      } else {
        
        text_body <- build_text_email(email_params)
        
        mailto <- create_mailto(
          to = "schelp@csiro.au",
          subject = "Bitbucket Repositories Migration Claim",
          body = text_body
        )
        
        mailto <- paste(mailto, collapse = "")
        
        if (nchar(mailto) > 1800) {
          showNotification(
            "Email is too long to open automatically. Please copy it instead.",
            type = "warning"
          )
        }
        
        runjs(sprintf("
          var link = document.createElement('a');
          link.href = '%s';
          link.style.display = 'none';
          document.body.appendChild(link);
          link.click();
          document.body.removeChild(link);
        ", mailto))
      }
      
      # Reset the input fields
      updateTextInput(session, "userMessage", value = "")
      
      dataTableProxy("infoTable") %>% selectRows(NULL)
      
    })
}

shinyApp(ui = ui, server = server)
