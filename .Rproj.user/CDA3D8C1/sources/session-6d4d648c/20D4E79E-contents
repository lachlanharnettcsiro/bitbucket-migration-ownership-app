library(shiny)
library(DT)
library(bslib)
library(blastula)
library(dplyr)
library(bsicons)


email_creds <- creds_anonymous(host = "smtp-relay.csiro.au", port = 25, use_ssl = FALSE)

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
    to = "har9b0@csiro.au",
    from = from,
    subject = "Bitbucket Repositories Migration Claim",
    credentials = email_creds
  )
}

ui <- page_fluid(
    theme = bs_theme(version = 5),
  
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
  
  
    # Application title
    br(),
    card(class = "title-container",
      titlePanel("Bitbucket Migration - Repository Ownership (WIP)", "Bitbucket Migration Ownership"),
    ),
    
    layout_columns(
      card(
        card_header("Project Information"),
          card_body(
            p(
              "
              The following data is generated from a review of exported Bitbucket repository data, Active Directory and other available information on repositories and identities. The generated data is an attempt to link repository ownership to an individual or business unit.
              "
            ),
            p(
              "
              As the report generation is automated, there will be inconsistencies or errors made in the final determinations.
              "
            ),
            p(
              "
              This is part of the Bitbucket migration project, which is moving all of the current on-prem Bitbucket data to GitHub. Thank you for supporting the migration of repositories from Bitbucket to GitHub. Establishing ownership is a crucial part of this process.
              "
            ),
          )
      ),
      card(
        card_header("Claim a Repository"),
        card_body(
              p("To claim specific repositories listed in the table, select the relevant rows, enter an email address and associated information in the below fields, then click the submit ticket button. "),
              div(class = "center-inputs",
                textInput("userEmail", NULL, placeholder = "Enter a contact email for the repos"),
                textAreaInput("userMessage", NULL, placeholder = "Enter associated information"),
                actionButton("claim", "Submit Ticket")
              )
        )
      ),
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
      
      send_email(
        from = from_addr,
        email_params = email_params
      )
      
      showNotification("Claim submitted! An email has been sent to the migration team.", type = "message")
      
        
    })
}

shinyApp(ui = ui, server = server)
