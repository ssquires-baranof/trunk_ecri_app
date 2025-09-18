library(shinyjs)
library(dplyr)
library(shinyWidgets)
library(bs4Dash)
library(reactable)
library(fresh)
library(highcharter)

library(shinyauthr)



options(spinner.color="#13213D", spinner.color.background="#efefef", spinner.size=2)
options(digits=10)




theme <- create_theme(
  
  bs4dash_status(
    primary = "#efefef",
    secondary = "#13213D",
    warning = "#efefef"
  ),
  
  bs4dash_sidebar_light (
    hover_bg = "#efefef",
    hover_color = "#13213D"
  ),
  
  bs4dash_sidebar_dark(
    hover_bg = "#13213D",
    hover_color = "#efefef"
  ))



shinyUI(dashboardPage(
  freshTheme = theme,
  fullscreen = TRUE,
  help = FALSE,
  title = "ECRI",
  header = dashboardHeader(
    skin='dark',
    controlbarIcon = icon("wand-sparkles", lib= "font-awesome"),
    title =  dashboardBrand(
      title = "_",
      color = "primary",
      image = "https://cdn.trunkspacestorage.com/wp-content/uploads/2024/06/21-300x76.png",
      
    ),
    useShinyjs(),
    #use_login(),
    rightUi = tags$li(class = "dropdown", shinyauthr::logoutUI(id = "logout"))
  ),
  
  sidebar = dashboardSidebar(
    skin = "light",
    elevation = 3,
    sidebarMenu(
      sidebarHeader("ECRI"),
      menuItem(
        "Summary",
        tabName = "summary",
        icon = icon("mountain", lib= "font-awesome")
      ),
      menuItem(
        "Inputs",
        tabName = "inputs",
        icon = icon("mountain-city", lib= "font-awesome")
      ),
      menuItem(
        "Unit Mix",
        tabName = "unit_mix",
        icon = icon("flask", lib= "font-awesome")
      ),
      menuItem(
        "Rent Roll",
        tabName = "rent_roll",
        icon = icon("person-hiking", lib= "font-awesome")
      ),
      menuItem(
        "ECRI",
        tabName = "ecri",
        icon = icon("person-hiking", lib= "font-awesome")
      )
      
    )
  ),
  controlbar =  dashboardControlbar(
    id = "controlbar",
    collapsed = T,
    fileInput("rr", "Upload Rent Role", accept = c(".csv")),
    fileInput("occ", "Upload Occupancy", accept = c(".xlsx")),
    fileInput("vt", "Upload ECRI List", accept = c(".csv")),
    
  ),
  body = dashboardBody(
    shinyauthr::loginUI("login"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$script(
        "$(document).on('shiny:inputchanged', function(event) {
          if (event.name != 'changed') {
            Shiny.setInputValue('changed', event.name);
          }
        });"
      )#,
      #includeHTML(("google-analytics.html"))
    ),
    tags$style(type = "text/css", 
               ".content-wrapper { background-color: #ffffff !important;}
               .sidebar, .sidebar-light .brand-link, .navbar-white {background-color: #efefef !important;}
               .sidebar-light .brand-link .bg-primary{border-bottom: none!important;}
               div.small-box.bg-primary, .bg-primary {margin: 0; border: none}
               .card {background-color: #efefef !important; border: 2px, solid, #075481; box-shadow: none !important; border-radius: 6px}
               .card-header{border-radius: 0px; background-color: #075481; color: #ffffff;}
               .card-title {font-family:'freight-sans-pro', font-weight: 600; sans-serif; letter-spacing:.1em; font-size:.9rem!important;}
               .brand-image {border-radius: 0 !important;box-shadow: none !important;}
               .control-sidebar-slide-open .control-sidebar, .control-sidebar-slide-open .control-sidebar::before 
               {padding: 10px !important;background-color: #efefef !important;} 
              .ReactTable {background-color: #13213D !important;}
              .bg-primary .nav-link {color: #fff;  font-family: 'freight-sans-pro', sans-serif; font-weight: 600; margin: 10px}
              .bg-primary .nav-link.active, .bg-primary .nav-link:hover, .bg-primary .nav-link:focus {background-color: #ffffff !important; color: #13213D !important; transition: 0.5s ease;}
              .bg-primary, .bg-primary a {color: #fff !important}
              "
    ),
    #useShinyjs(),
    
    
    tabItems(
      tabItem(
        tabName = "summary",
        fluidRow(
          column(12, 
                 
                 tabBox(
                   width = 12, collapsible = FALSE,
                   maximizable=FALSE,
                   background = "primary",
                   tabPanel(title = "ECRI Premium Buckets",
                            reactableOutput("ecri_premium_buckets")),
                   tabPanel(title = "ECRI Length of Stay",
                            reactableOutput("los_ecri")),
                   tabPanel(title = "Rent",
                            valueBox( 
                              width = 12,
                              value = "Current In-Place Rate:", 
                              subtitle = htmlOutput("current_in_place_rate_psf"),
                              color = "info",
                              icon = icon("coins", lib= "font-awesome")),
                              valueBox( 
                                width = 12,
                                value = "Projected In-Place Rate:", 
                                subtitle = htmlOutput("projected_in_place_rate_psf"),
                                color = "info",
                                icon = icon("calendar", lib= "font-awesome")
                            ))
                   #tabPanel(
                    # title = "Sales Chart"
                   #)
                 )
          )
        )
      ),
      tabItem(
        tabName = "inputs",
        fluidRow(
          column(12, 
                 
                 tabBox(
                   width = 12, collapsible = FALSE,
                   maximizable=FALSE,
                   background = "primary",
                   tabPanel(title = "Premium Buckets",
                            reactableOutput("premium_buckets")),
                   tabPanel(title = "Rent Rate Percentiles",
                            reactableOutput("rr_percentiles")),
                   tabPanel(title = "SF Buckets",
                            reactableOutput("sf_buckets"))
                 )
          )
        )
      ),
      tabItem(
        tabName = "rent_roll",
        fluidRow(
          column(12, 
                 
                 tabBox(
                   width = 12, collapsible = FALSE,
                   maximizable=FALSE,
                   background = "primary",
                   tabPanel(title = "Rent Roll Inputs",
                            reactableOutput("rr")),
                   tabPanel(title = "Top Quartile",
                            reactableOutput("unit_type_rr")),
                   tabPanel(title = "Move In Rate",
                            reactableOutput("move_in_rate"))
                 )
          )
        )
      ),
      tabItem(
        tabName = "unit_mix",
        fluidRow(
          column(12, 
                 
                 tabBox(
                   width = 12, collapsible = FALSE,
                   maximizable=FALSE,
                   background = "primary",
                   tabPanel(title = "Unit Mix",
                            reactableOutput("occ"))
                 )
          )
        )
      ),
      tabItem(
        tabName = "ecri",
        fluidRow(
          column(12, 
                 
                 tabBox(
                   width = 12, collapsible = FALSE,
                   maximizable=FALSE,
                   background = "primary",
                   tabPanel(title = "ECRI",
                            reactableOutput("rr_joined_filter"))
                 )
          )
        )
      )
      
      
      
      
      
      
    ) #items
  ) #body
) #page
) # ui