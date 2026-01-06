library(shiny)
library(DT)
library(bslib)
library(blastula)
library(dplyr)
library(shinyjs)



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
    render_text_table(params$BB_IDs),
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
    sprintf(
      "The claim was submitted by %s.",
      params$logged_in_user %||% "<unknown>"
    ),
    sep = "\n"
  )
}



render_text_table <- function(ids) {
  if (length(ids) == 0) {
    return("No repository IDs were selected.")
  }
  
  knitr::kable(
    data.frame(BB_ID = ids),
    format = "simple"
  )
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
    # to = schelp@csiro.au
    to = "huy029@csiro.au",
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
            p(
              "
              This application helps users identify and claim orphaned Bitbucket repositories as part of the migration to GitHub. Confirming ownership is a critical step to ensure a smooth transition. To claim specific repositories, follow the instructions on the right-hand panel. A ticket will then be submitted to Scientific Computing to help update the records.
              "
            ),
            p(
              "
              The information shown is based on a review of exported Bitbucket repository data, Active Directory, and other available sources related to repositories and identities. Its purpose is to link repository ownership to an individual or business unit. 
              As this report is generated automatically, some inconsistencies or errors may occur in the final results.
              "
            ),
            p(
              "
              This effort is part of the Bitbucket migration project, which involves moving all current on-premise Bitbucket data to GitHub. Thank you for supporting this migration.
              "
            ),
          )
      ),
      
      if (file.exists("shiny-server/.deployment")) {
        tagList(
          card(
            card_header("Generate Email"),
            card_body(
              p("To claim specific repositories listed in the table, select the relevant rows, enter a contact email and associated information in the below fields, then click the generate email. "),
              div(class = "center-inputs",
                  textInput("userEmail", NULL, placeholder = "Enter a contact email for the repos"),
                  textAreaInput("userMessage", NULL, placeholder = "Enter associated information"),
                  actionButton("claim", "Generate Email")
              )
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
                tags$li("Enter the details of the ownership changes in the field provided below."),
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
      card_header("Table of Bitbucket Projects"),
      card_body(
        dataTableOutput("infoTable")
      )
    )
)


server <- function(input, output, session) {
  
    # Autofills the email field with session$user
    if (!file.exists("shiny-server/.deployment")) {
      observe({
        updateTextInput(session, "userEmail", value = session$user)
        shinyjs::disable("userEmail")
        
      })
    }

    # Getting 403 Forbidden?
    data <- read.csv("https://bbmigration-001.it.csiro.au/index_bu.csv", sep=";")
    # data <- read.csv("index_bu.csv")
  
    data$Repo_Info_Link <- ifelse(
      grepl("^https?://", data$Repo_Info_Link),  
      sprintf("<a href='%s' target='_blank'>%s</a>", 
              data$Repo_Info_Link, 
              data$Repo_Info_Link),
      data$Repo_Info_Link                         
    )
    
    
    output$infoTable <- renderDataTable({
      datatable(data, selection = "multiple", escape = FALSE, rownames = FALSE, filter = "top")
    })
    
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
      
      from_addr <- input$userEmail
      
      email_params <- list(
        BB_IDs = ids_claimed,
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
        
        showNotification("Claim submitted! An email has been sent to the migration team.", type = "message")
      
      
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
      
    })
}

shinyApp(ui = ui, server = server)
