#----START GLOBAL SECTION----

#packages
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(data.table)
library(tidyverse)
library(dplyr)
library(magrittr)
library(lubridate)

#remove all variables from current environment#
rm(list = ls())

#set working directory; all file structure is relative to working directory.
setwd("/Users/nischal/Documents/Nischal-EIMS-Vaccine/EIMS/VaccineRequest/")#EIMS server

#distro hub reports path
distropath <- "/Users/nischal/Documents/Nischal-EIMS-Vaccine/Vaccine/VaccineMgt/data/distro/" #EIMS server

#help desk information
help_desk_phone <- "304-553-7846"
help_desk_phone_hours <- "Monday to Friday, 8 AM to 4 PM"
help_desk_email <- "EIMSHelpDesk@wv.gov"
help_desk_email_hours <- "Every Day, 8 AM to 4 PM"

#non-conditional drop-down lists
counties <- list("Barbour","Berkeley","Boone","Braxton","Brooke","Cabell","Calhoun","Clay","Doddridge","Fayette","Gilmer",
                 "Grant","Greenbrier","Hampshire","Hancock","Hardy","Harrison","Jackson","Jefferson","Kanawha","Lewis","Lincoln",
                 "Logan","Marion","Marshall","Mason","McDowell","Mercer","Mineral","Mingo","Monongalia","Monroe","Morgan",
                 "Nicholas","Ohio","Pendleton","Pleasants","Pocahontas","Preston","Putnam","Raleigh","Randolph","Ritchie",
                 "Roane","Summers","Taylor","Tucker","Tyler","Upshur","Wayne", "Webster","Wetzel","Wirt","Wood","Wyoming")
inventory <-  list("Inv1") #list("Pfizer", "Pfizer Pediatric", "Moderna", "Janssen") #pfizer only on 1/21/22
distrohubs <- list("02 - Hub2") #removed Cabell and Greenbrier on 8/9/21; removed Monongalia and Berkeley on 3/2/2022
pickupdays <- list("TUES", "WED","THURS")

#vaccine shipping and ordering units
inv1_container <- 75 #units per container
inv4_container <- 10
inv2_container <- 10 #units
inv3_container <- 20 #units
order_minimum <- 1 #minimum order for pick up
inv1_quantities_in_unit <-6 #quantities per unit
inv4_quantities_in_unit <-10
inv2_quantities_in_unit <-10 #changed 12/6/21
inv3_quantities_in_unit <-5

#image file for background; needs to be in www folder
background_image <- "WVU-Pattern_Thin_124-Coated.jpg"#official WVU background option

#function to add star to mandatory field labels
labelMandatory <- function(label){
  tagList(label, span("*", class="mandatory_star"))
} #end function

#css styles
#color schemes from official WVU branding: https://universityrelations.wvu.edu/brand-guide
css <- HTML("
  /*default color of all text; backup background color if background image file fails*/
  body {color:#002855; background-color: #EAAA00; font-family: Iowan Times New Roman, Times New Roman, Times, serif; font-size: 16px;}
  
  /*horizontal line customization*/
  hr {border: 1.5px solid #BFB8AF; border-radius: 1px; width: 90%; background: #BFB8AF;box-shadow: 3px 4px 12px 1px #002855;}
  
  /*header font properties to override body font*/
  h1 {font-family: Helvetica Neue, Helvetica, sans-serif;}
  h2 {font-family: Helvetica Neue, Helvetica, sans-serif;}
  h3 {font-family: Helvetica Neue, Helvetica, sans-serif;}
  h4 {font-family: Helvetica Neue, Helvetica, sans-serif;}
  h5 {font-family: Helvetica Neue, Helvetica, sans-serif;}
  h6 {font-family: Helvetica Neue, Helvetica, sans-serif;}
  
  /*navigation bar color*/
  .navbar {background-color: #968C83;}
  
  /*top tabs color and font color*/
  .navbar-nav li a{background-color: #EAAA00; color:#968C83 ;font-variant: small-caps;font-weight: 500; font-family: Helvetica Neue, Helvetica, sans-serif;}
  
  /*top tabs color and font when mouse hover and/or selected (active)*/
  .navbar-nav li a:hover, .navbar-nav> li[class=active]> a {background-color: #ED8B00 !important; color:white !important ;
    font-weight: bold; text-shadow: 2px 2px 5px #002855;font-variant: small-caps;font-family: Helvetica Neue, Helvetica, sans-serif;}
  
  /*navigation bar title (brand) properties*/
  .navbar-default .navbar-brand{color:white !important; font-weight: bold; text-shadow: 2px 2px 5px #002855; font-family: Helvetica Neue, Helvetica, sans-serif;}
  
  /*tabset panel (tab in a tab) color and font*/
  .tabbable > .nav > li > a {background-color: #EAAA00; color:white; font-variant: small-caps;font-weight: 500; font-family: Helvetica Neue, Helvetica, sans-serif;} 
  
  /*tabset panel (tab in a tab) color and font when mouse hover and/or selected*/
  .tabbable li a:hover, .tabbable > .nav > li[class=active] > a {background-color: #ED8B00; color:white;font-weight: bold;
    text-shadow: 2px 2px 5px #002855;font-variant: small-caps;font-family: Helvetica Neue, Helvetica, sans-serif;}
  
  /*image border size, color, and shadow*/
  img {border:2px solid #002855; box-shadow: 3px 4px 12px 1px #002855; width: 90%; height: auto;}
  
  /*select menu customization*/
  .selectize-input {font-family: Helvetica Neue, Helvetica, sans-serif; font-size: 12pt; box-shadow: 3px 4px 12px 1px #002855;
    font-weight: bold; border-radius: 0px}
  .selectize-dropdown {font-family: Helvetica Neue, Helvetica, sans-serif;font-size: 10pt;}
  .control-label {background-color: #002855; color: white; font-family: Helvetica Neue, Helvetica, sans-serif;font-size: 14pt;
    border-top-left-radius: 10px; border-top-right-radius: 10px; text-align: center; padding: 4px; width:100%; margin: 0px;
    box-shadow: 3px 0px 12px 1px #002855;}
 
  /*text/number input field customization*/
  .form-control {font-family: Helvetica Neue, Helvetica, sans-serif; font-size: 12pt; box-shadow: 3px 4px 12px 1px #002855;
    font-weight: bold; border-radius: 0px; text-align: center;}
    
  /* required field asterisk */
  .mandatory_star {color: red;}
  
  /*submit button customization - blue */
  .submitbutton {box-shadow: 3px 4px 12px 1px #002855; background:linear-gradient(to bottom, #005EB8 20%, #002855 100%); background-color:#002855;
	  border-radius:3px; border:2px solid #002855; display:inline-block; cursor:pointer; color:white; font-family:Helvetica Neue, Helvetica, sans-serif;
	  font-size:17px; font-weight:bold; padding:10px 20px;}
	.submitbutton:hover {background:linear-gradient(to bottom, #378de5 20%, #79bbff 100%); background-color:#378de5;}
  .submitbutton:active {position:relative;top:1px;}
  
  /*cancel or remove button customization - red */
  .cancelOrRemoveButton {box-shadow: 3px 4px 12px 1px #002855; background:linear-gradient(to bottom, #CD4C08 20%, #BE3A34 100%); background-color:#BE3A34;
	  border-radius:3px; border:2px solid #BE3A34; display:inline-block; cursor:pointer; color:white; font-family:Helvetica Neue, Helvetica, sans-serif;
	  font-size:17px; font-weight:bold; padding:10px 20px;}
	.cancelOrRemoveButton:hover {background:linear-gradient(to bottom, #e6a3b4 20%, #e6c1c4 100%); background-color:#e6a3b4;}
  .cancelOrRemoveButton:active {position:relative;top:1px;}
  
  /*add button customization - green */
  .addButton {box-shadow: 3px 4px 12px 1px #002855; background:linear-gradient(to bottom, #9ABEAA 20%, #0D5257 100%); background-color:#0D5257;
	  border-radius:3px; border:2px solid #0D5257; display:inline-block; cursor:pointer; color:white; font-family:Helvetica Neue, Helvetica, sans-serif;
	  font-size:17px; font-weight:bold; padding:10px 20px;}
	.addButton:hover {background:linear-gradient(to bottom, #bff5ee 20%, #69b5ab 100%); background-color:#bff5ee;}
  .addButton:active {position:relative;top:1px;}
   
   /*change or edit button customization - yellow */
  .changeButton {box-shadow: 3px 4px 12px 1px #002855; background:linear-gradient(to bottom, #FDDA24 20%, #9F7D23 100%); background-color:#9F7D23;
	  border-radius:3px; border:2px solid #9F7D23; display:inline-block; cursor:pointer; color:white; font-family:Helvetica Neue, Helvetica, sans-serif;
	  font-size:17px; font-weight:bold; padding:10px 20px;}
	.changeButton:hover {background:linear-gradient(to bottom, #BFB8AF 20%, #EAAA00 100%); background-color:#bff5ee;}
  .changeButton:active {position:relative;top:1px;}
  
   /*refresh button customization - grays */
  .refreshButton {box-shadow: 3px 4px 12px 1px #002855; background:linear-gradient(to bottom, #BFB8AF 20%, #6E6259 100%); background-color:#6E6259;
	  border-radius:3px; border:2px solid #6E6259; display:inline-block; cursor:pointer; color:white; font-family:Helvetica Neue, Helvetica, sans-serif;
	  font-size:17px; font-weight:bold; padding:10px 20px;}
	.refreshButton:hover {background:linear-gradient(to bottom, #968C83 20%, #968C83 100%); background-color:#bff5ee;}
  .refreshButton:active {position:relative;top:1px;}
  
  /* error or warning message */
  .shiny-text-output {color: #bf1f13;font-weight: bold; font-family: Helvetica Neue, Helvetica, sans-serif; font-size: 10px;}
  
  /* Edit Pop-up (Modal) customization */
  .modal-lg {width: 1200px;}
  
  /* datatable customization */
  .datatables .odd {background-color: #BFB8AF !important;}
  .datatables .even {background-color: #968C83;}
  .datatables thead th {background-color: #A2AAAD; color: #333F48; font-weight: bold; font-size: 16px;
    font-family: Helvetica Neue, Helvetica, sans-serif; border: 1px solid #333F48; text-shadow: 2px 2px 5px white;}
  .datatables tbody td {font-family: Helvetica Neue, Helvetica, sans-serif;color: #333F48;}
  
  ")#end HTML

#------END GLOBAL SECTION------

#-----START UI SECTION--------
ui <- shinyUI(fluidPage(
  #color scheme / theme; set styles defined in global 
  tags$head(tags$style(css)),
  #background pattern of all tabs
  setBackgroundImage(src=(background_image)), 
  
  navbarPage("INVENTORY REQUEST PORTAL", #set up page framework
             tabPanel("EIMS Log-In Page", #set up Log in page
                      mainPanel(width=12, #setting width allows for auto-scaling based on browser window
                                fluidRow(
                                  column(3,align="center",
                                         #EIMS logo, created in Canva and Paint; needs to be in a www folder
                                         tags$img(src="EIMS_Logo.png")
                                  ),#end column
                                  
                                  #greeting to user
                                  column(6,align="center",
                                         h3(strong("Welcome to the EIMS Inventory Request Portal!"), style="text-shadow: 2px 2px 5px #002855; font-size:30px;"), 
                                         h4(strong("Please follow the steps below to log in.")),
                                         br(),
                                         #uiOutput("training"),
                                         h4(strong(em("NOTICE: All COVID-19 inventory are now available for direct shipment through WVSIIS.")), style="color:red;"), 
                                         h4("You can contact WVSIIS via their help desk phone number ((877)408-8930) or their website (", a("click here", href="https://www.wvimm.org/wvsiis/"), ")."),
                                         h4("Hub 4 and Hub 5 are no longer operational. EIMS only supports requests from Hub 2 for Inv1 until the inventory is depleted."),
                                         p(),
                                  ), #end column
                                  column(1,
                                  ),#end column
                                  column(2,align="center",
                                         #JIATF logo; needs to be in a www folder
                                         tags$img(src="JIATF_logo.png")
                                  )#end column
                                  
                                ), #end fluidRow
                                p(),
                                fluidRow(
                                  column(2), #end column
                                  column(4, align="center",
                                         textInput("EIMSProviderPIN", 
                                                   label=labelMandatory("WV Provider PIN"), 
                                                   placeholder="enter PIN")
                                  ),#end column
                                  column(4,
                                         strong("Enter the six-digit WV Provider PIN or WVA+6-digit WV Provider PIN associated with the location for which you are ordering.")
                                  )#end column
                                ),# end fluidRow
                                p(),
                                fluidRow(
                                  column(2), #end column
                                  column(4, align="center",
                                         textInput("EIMSEmail", 
                                                   label=labelMandatory("Email Address"), placeholder="name@place.com")
                                  ),#end column
                                  column(4,
                                         strong("Enter your registration email address.")
                                  )#end column
                                ),# end fluidRow
                                p(),
                                p(),
                                br(),
                                fluidRow(
                                  column(2), #end column
                                  column(8, align="center",
                                         h3(strong("By clicking", em("Enter Inventory Request Portal"), ", I certify that the email address I have 
                                 entered is my own and that I am authorized to order COVID-19 inventory for the listed Medical Provider Location PIN."))
                                  ),#end column
                                  
                                ),#end fluidRow
                                p(),
                                textOutput("validation_messages_login"),
                                p(),
                                fluidRow(align="center",
                                         actionButton(inputId = "enterEIMS", label = "Enter Inventory Request Portal", class="submitbutton")
                                ),#end fluid row
                                p(),
                                hr()
                      ),#end Main Panel
                      #FOOTER
                      uiOutput("footer")
             ),#end tabPanel
             id="home"
  )#end navbarPage
)#end fluidPage
)#end shinyUI

#---------END UI SECTION-------

#----START SERVER SECTION-----  

server = function(input, output, session) {
  
  #dynamic operational and distribution weeks
  #Start Date in yyyy-mm-dd
  start_date <- as.Date("2021-05-28")
  #days between today and start date
  days_gone_by <- Sys.Date() - start_date
  #weeks since start, this week plus next 5 weeks
  weeks_gone_by <- as.numeric(floor(days_gone_by/7))+6
  #set nulls and formats for week loop
  i <- NULL
  week_list <- NULL
  week_list <- as.list(week_list) #list needed for selectInput()'s choices format
  week_number_list <- NULL
  week_number_list <- as.list(week_number_list)
  distribution_week_list <- NULL #distribution week for vaccinator interface
  distribution_week_list <- as.list(distribution_week_list)
  #create list of weeks with start and end dates and of the week numbers
  for(i in 1:weeks_gone_by){
    weekstartdate = start_date + (7*(i-1)) #starts on a Friday
    distributionweekstartdate = weekstartdate + 3 #starts on a Monday
    weekenddate = weekstartdate + 6
    distributionweekenddate = distributionweekstartdate + 6
    week_list <- rbind(week_list,  paste("Week", i, ":", format(weekstartdate, format = "%m/%d/%y"),"to", 
                                         format(weekenddate, format = "%m/%d/%y")))
    week_number_list <-rbind(week_number_list, i)
    distribution_week_list <- rbind(distribution_week_list, paste(format(distributionweekstartdate, format = "%m/%d/%y"),"to", 
                                                                  format(distributionweekenddate, format = "%m/%d/%y")))
  }
  
  currentoperationalweek <- as.numeric(floor(days_gone_by/7))+1 
  
  #link to video
  url <- a("If you have questions, please click here to watch a how-to video or contact the Help Desk.", 
           href="https://www.youtube.com/watch?v=rsJsZkh1MGU&list=PLyeBU1TbsA_2A2HcNE1Q18Vjmx0nWCw9Y&index=2")
  output$training <- renderUI({
    tagList(url)
  })
  
  #page footer
  footer_0<- renderUI({
    div(
      br(),
      br(),
      br(),
      fluidRow(
        column(2, 
        ),#end column
        column(8, align="center",
               #strong("EIMS Help Desk", style = "font-size:9px;"),
               #br(),
               #strong("Call us at", help_desk_phone,"from", help_desk_phone_hours, style = "font-size:8px;"),
               #br(),
               #strong("or email us at", help_desk_email, style = "font-size:8px;")
        ), #end column
        column(2, align="center",
               h6(em(strong("EIMS IS POWERED BY")), style = "font-size:7px;"),
               #DataDrivenWV logo; needs to be in a www folder
               tags$img(src="datadrivenwv.jpg"),
               p()
        )#end column
      )#end fluidrow
    )#end div
  })
  
  output$footer <-footer_0
  output$footer1 <-footer_0
  output$footer2 <-footer_0
  output$footer3 <-footer_0
  output$footer4 <-footer_0
  output$footer5 <-footer_0
  
  #read in registered user list, providers and location code
  registered_users <- fread("data/EIMS_Enrollment.csv")
  InventoryProviders <- fread("data/WV_Provider_Enrollment.csv")
  ProviderLocationCodes <- fread("data/Provider_Location_Codes.csv") #note that this file has had some format changes from original to be more friendly to this process
  
  #validations for login
  validation_login <- reactive({
    validate(
      #check PIN is in registration list
      need(nrow(registered_users[which(tolower(registered_users$`What is your Provider Location PIN?`)==tolower(input$EIMSProviderPIN))])>=1, 
           "The PIN you entered does not match any user profiles."),
      #validate that PIN-email combo exists in registration
      need(nrow(registered_users[which(tolower(registered_users$`What is your Provider Location PIN?`)==tolower(input$EIMSProviderPIN) & 
                                         tolower(registered_users$`User's Work Email`)==tolower(input$EIMSEmail))])==1, 
           "There is a PIN-email mismatch. Please check that these both match what was entered in registration. 
           If you have questions, you may contact the Help Desk")
    )
  })
  
  ####validation messages in login box  
  output$validation_messages_login <- renderText(
    paste(validation_login()$run)
  ) 
  
  #for the status of the log in to keep things from loading
  USER <- reactiveValues(LoggedIn=FALSE)
  
  ##save values and allow access to request portal tabs
  observeEvent(input$enterEIMS, {
    ####validations to stop access
    validation_login()
    
    ####set values
    WVSIISUserWVProviderPINEntered <- toupper(input$EIMSProviderPIN)
    #just the 6-digits of the PIN
    WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
    WVSIISUserName <- registered_users$`User's Name (First and Last)`[which(tolower(registered_users$`What is your Provider Location PIN?`)==tolower(input$EIMSProviderPIN) & tolower(registered_users$`User's Work Email`)==tolower(input$EIMSEmail))]
    
    ###if a request file doesn't exist for the PIN, create one.
    if(file.exists(paste("data/Requests/", WVSIISUserWVProviderPIN,".csv", sep=""))==FALSE){
      createrequest <- setNames(data.frame(matrix(ncol = 20, nrow = 0)),  
                                c("RequestStatus", "OperationalWeek","WVProviderPIN", "EntityName", "EntityType",	"EntityGroup",
                                  "County", "TargetPopulation", "InventoryType", "Series","RequestedUnits",	"POCName", "POCPhone", 
                                  "POCEmail", "NotesToApprover", "NotesFromApprover", "DeliveryMethod",	"DistributionHub", 
                                  "ShippingAddress", "PickUpDay"))
      
      fwrite(createrequest,paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
    }
    
    ####set LoggedIn to TRUE
    USER$LoggedIn <- TRUE 
    
    ####append request tabs - load order matters
    appendTab(inputId="home", tab=tabPanel("WV Provider PIN Profile", uiOutput("tab5")))
    appendTab(inputId="home", tab=tabPanel("New Inventory Request", uiOutput("tab1")))
    appendTab(inputId="home", tab=tabPanel("Change Requests", uiOutput("tab2")))
    appendTab(inputId="home", tab=tabPanel("Distribution Summary", uiOutput("tab3")))
    appendTab(inputId="home", tab=tabPanel("Approved Representatives", uiOutput("tab4")))
    
    ####remove login tab
    removeTab(inputId="home",target="EIMS Log-In Page")
  }) #end observeEvent
  
  observe({
    if(USER$LoggedIn==TRUE){
      
      ####set values
      WVSIISUserWVProviderPINEntered <- toupper(input$EIMSProviderPIN)
      #just the 6-digits of the PIN
      WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
      WVSIISUserName <- registered_users$`User's Name (First and Last)`[which(tolower(registered_users$`What is your Provider Location PIN?`)==tolower(input$EIMSProviderPIN) & tolower(registered_users$`User's Work Email`)==tolower(input$EIMSEmail))]
      
      ####preload PIN-dependent data
      
      #WVSIIS User Data for PIN
      RequestPrepopulation <- InventoryProviders%>%
        mutate("WVProviderPIN" = substr(loc_vfc_pin, nchar(loc_vfc_pin)-6+1, nchar(loc_vfc_pin)))%>% #last six digits to remove WVAs
        rename("OrgNumber" = "org_iis_id")%>%
        mutate("ShippingAddress"= ifelse(loc_ship_street1!= "", paste(InventoryProviders$loc_ship_street1, InventoryProviders$loc_ship_city, 
                                                                      paste(InventoryProviders$loc_ship_state, InventoryProviders$loc_ship_zip, sep=" "), sep=", ")),"")%>% #if no address, blank, else combine address elements
        rename("POCEmail" = "loc_primary_email")%>%
        rename("POCPhone" = "loc_primary_phone")%>%
        rename("OrgName" = "org_name")%>%
        rename("Entity" = "loc_name")%>%
        rename("County" = "loc_ship_county")%>%
        mutate("POCName" = paste(InventoryProviders$loc_primary_fname, InventoryProviders$loc_primary_mi, InventoryProviders$loc_primary_lname, sep=" "))%>%
        mutate("loc_type_combined" = case_when(loc_type==28 ~ loc_type_text, loc_type!=28 ~ loc_type))%>%
        left_join(., ProviderLocationCodes, by = c("loc_type_combined"="loc_type"))%>%
        filter(as.numeric(WVProviderPIN)==as.numeric(WVSIISUserWVProviderPIN))%>%
        select("OrgNumber","OrgName", "WVProviderPIN", "Entity", "POCName", "POCPhone", "POCEmail", "ShippingAddress","County", 
               "loc_type_combined", "EntityType")
      
      #Entity group definitions for RequestPrepopulation$loc_type_combined
      EntityGroup <-switch(RequestPrepopulation$loc_type_combined,
                           "1"= "JIATF", #Commercial vaccination service provider; includes commercial EMTs/ambulance services
                           "2"= "DCR", #Corrections/detention health services
                           "3"= "CommunityClinics", #Health center - community (non-Federally Qualified Health Center/non-Rural Health Clinic); includes PAAC
                           "4"= "CommunityClinics", #Health center - migrant or refugee
                           "5"= "CommunityClinics", #Health center - occupational
                           "6"= "CommunityClinics", #Health center - STD/HIV clinic
                           "7"= "CommunityClinics", #Health center - student; mostly college/university health centers
                           "8"= "JIATF", #Home health care provider
                           "9"= "JIATF", #Hospital
                           "10"= "JIATF", #Indian Health Service
                           "11"= "JIATF", #Tribal health
                           "12"= "JIATF", #Medical practice - family medicine
                           "13"= "JIATF", #Medical practice - pediatrics
                           "14"= "JIATF", #Medical practice - internal medicine
                           "15"= "JIATF", #Medical practice - OB/GYN
                           "16"= "JIATF", #Medical practice - other specialty
                           "17"= "Pharmacy", #Pharmacy - chain
                           "18"= "Pharmacy", #Pharmacy - independent
                           "19"= "CommunityClinics", #Public health provider - public health clinic; primarily local health departments
                           "20"= "CommunityClinics", #Public health provider - Federally Qualified Health Center; FQHCs and FQHC PK-12 Clinics
                           "21"= "CommunityClinics", #Public health provider - Rural Health Clinic
                           "22"= "LTC", #Long-term care - nursing home, skilled nursing facility, federally certified
                           "23"= "LTC", #Long-term care - nursing home, skilled nursing facility, non-federally certified
                           "24"= "LTC", #Long-term care - assisted living
                           "25"= "LTC", #Long-term care - intellectual or developmental disability
                           "26"= "LTC", #Long-term care - combination (e.g., assisted living and nursing home in same facility)
                           "27"= "JIATF", #Urgent care; non-emergency rooms
                           "28"= "JIATF" #Other
      )# end EntityGroup
      
      ###create object for request data for PIN
      request_data <- fread(paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep="")) 
      
      #weekly blackout period for request processing
      blackout_trigger <- switch(wday(Sys.Date()),
                                 "1" = FALSE, #Sunday
                                 "2" = FALSE, #Monday
                                 "3" = FALSE, #Tuesday
                                 "4" = TRUE, #Wednesday
                                 "5" = TRUE, #Thursday
                                 "6" = FALSE, #Friday
                                 "7" = FALSE, #Saturday
      )#end blackout_trigger
      
      #Distribution Week list based on blackout trigger
      if(blackout_trigger==TRUE){
        week_choices <- as.list(tail(distribution_week_list, n=4))
      }else{
        week_choices <- as.list(tail(distribution_week_list, n=5))
      }
      #----Render Tabs----#
      
      ##Tab 1 - New Request
      output$tab1 <- renderUI({
        fluidPage(
          
          #color scheme / theme; set styles defined in global 
          tags$head(tags$style(css)),
          
          #background pattern of all tabs
          setBackgroundImage(src=(background_image)), 
          
          mainPanel(width=12, #setting width allows for auto-scaling based on browser window
                    fluidRow(
                      column(3,align="center",
                             #EIMS logo, created in Canva and Paint; needs to be in a www folder
                             tags$img(src="EIMS_Logo.png")
                      ), #end column
                      #greeting to user
                      column(6,align="center",
                             h3(strong("Welcome, ", WVSIISUserName,"!"), style="text-shadow: 2px 2px 5px #002855; font-size:30px;"), 
                             h4(strong("Please follow the steps below to request inventory for", 
                                       RequestPrepopulation$Entity, "(WV Provider PIN:", paste(WVSIISUserWVProviderPINEntered,").", sep=""))),
                             p(),
                             em("*", style="color:red;"), 
                             em(" denotes a mandatory field")
                      ), #end column
                      column(1,
                      ),#end column
                      column(2,align="center",
                             #JIATF logo; needs to be in a www folder
                             tags$img(src="JIATF_logo.png")
                      )#end column
                    ), #end fluidRow
                    hr(),
                    fluidRow(
                      column(4, align="center",
                             selectInput("inventoryfinder",
                                         label = labelMandatory("Inventory Finder Status"),
                                         choices = c("choose status", "No", "Yes", "Not Applicable"))
                      ), #end column
                      column(6,
                             strong("Have you reported your inventory to Inventory Finder in the last week?"),
                             br(),
                             textOutput("inventory_finder_warning")
                      )#end column
                    ),#end fluidRow
                    hr(),
                    conditionalPanel(
                      condition="input.inventoryfinder == 'Yes' || input.inventoryfinder =='Not Applicable'",
                      fluidRow(
                        column(4, align="center",
                               selectInput("week1",
                                           label = labelMandatory("Distribution Week"),
                                           choices = c("choose week",week_choices))
                        ), #end column
                        column(6,
                               strong("Select the distribution week for this request. You may place requests for all listed weeks in advance. 
                                     The cut-off for requesting for next week is 11:59 PM on Tuesday of this week.")
                        )#end column
                        
                      ), #end fluidRow
                      hr(),
                      fluidRow(
                        column(4, align="center",
                               selectInput("inventory1",
                                           label = labelMandatory("Inventory Type"),
                                           choices = c("choose inventory type", inventory))
                        ),#end column
                        column(6,
                               strong("Select the type of inventory you would like to request. Note that you can only select one type per request.") 
                        )#end column
                      ),# end fluidRow
                      fluidRow(
                        column(4, align="center",
                               conditionalPanel(
                                 condition="input.inventory1=='Inv1'",
                                 numericInput("RequestedUnitsInv1Prime",
                                              label=labelMandatory("Prime"), 
                                              min=0, step=1, value=0)
                               ), #end conditionalPanel
                               conditionalPanel(
                                 condition="input.inventory1=='Inv2'",
                                 numericInput("RequestedUnitsInv2Prime",
                                              label=labelMandatory("Prime"), 
                                              min=0, step=1, value=0)
                               ), #end conditionalPanel
                               conditionalPanel(
                                 condition="input.inventory1=='Inv3'",
                                 numericInput("RequestedUnitsinv3",
                                              label=labelMandatory("Requested Units"), 
                                              min=0, step=1, value=0)
                               ), #end conditionalPanel
                               conditionalPanel(
                                 condition="input.inventory1=='Inv4'",
                                 numericInput("RequestedUnitsInv4Prime",
                                              label=labelMandatory("Prime"), 
                                              min=0, step=1, value=0)
                               ), #end conditionalPanel
                        ),#end column
                        column(6,
                               conditionalPanel(
                                 condition="input.inventory1!='Inv3'",
                                 strong("Enter the total number of units being requested between the Prime (primary inventory) and Boost (booster) fields.
                                         If unsure of how many will be used for each purpose, estimate to your best ability.")
                               ),#end conditional panel
                               conditionalPanel(
                                 condition="input.inventory1=='Inv3'",
                                 strong("Enter the total number of units being requested.")
                               )#end conditional panel
                        )#end column
                      ),# end fluidRow
                      fluidRow(
                        column(4, align="center",
                               conditionalPanel(
                                 condition="input.inventory1=='Inv1'",
                                 numericInput("RequestedUnitsInv1Boost",
                                              label=labelMandatory("Boost"), 
                                              min=0, step=1, value=0) 
                               ),#end conditional panel
                               conditionalPanel(
                                 condition="input.inventory1=='Inv2'",
                                 numericInput("RequestedUnitsInv2Boost",
                                              label=labelMandatory("Boost"), 
                                              min=0, step=1, value=0) 
                               ),#end conditional panel
                               conditionalPanel(
                                 condition="input.inventory1=='Inv3'",
                                 paste("")
                               ),#end conditional Panel
                               conditionalPanel(
                                 condition="input.inventory1=='Inv4'",
                                 numericInput("RequestedUnitsInv4Boost",
                                              label=labelMandatory("Boost"), 
                                              min=0, step=1, value=0) 
                               ),#end conditional panel
                        ), #end column
                        column(6,
                               conditionalPanel(
                                 condition="input.inventory1=='Inv1'",
                                 textOutput("requested_units_inv1")%>%tagAppendAttributes(style='color:#002855; font-size:18px;font-weight:bold'),
                                 textOutput("requested_quantities_inv1")%>%tagAppendAttributes(style='color:#002855; font-size:18px;font-weight:bold')
                               ),#end conditionalPanel
                               conditionalPanel(
                                 condition="input.inventory1=='Inv2'",
                                 textOutput("requested_units_inv2")%>%tagAppendAttributes(style='color:#002855; font-size:18px;font-weight:bold'),
                                 textOutput("requested_quantities_inv2")%>%tagAppendAttributes(style='color:#002855; font-size:18px;font-weight:bold')
                               ),#end conditionalPanel
                               conditionalPanel(
                                 condition="input.inventory1=='Inv3'",
                                 textOutput("requested_quantities_inv3")%>%tagAppendAttributes(style='color:#002855; font-size:18px;font-weight:bold')
                               ),#end conditionalPanel
                               conditionalPanel(
                                 condition="input.inventory1=='Inv4'",
                                 textOutput("requested_units_inv4")%>%tagAppendAttributes(style='color:#002855; font-size:18px;font-weight:bold'),
                                 textOutput("requested_quantities_inv4")%>%tagAppendAttributes(style='color:#002855; font-size:18px;font-weight:bold')
                               ),#end conditionalPanel
                               br(),
                               #textOutput("vaccine_delivery_message"), #increments for delivery message
                               textOutput("minimum_unit_warning"),
                        ) #end column
                      ), #end fluidRow
                      hr(),
                      #fluidRow(
                      #column(4, align="center",
                      #selectInput("distribution1",
                      #label = labelMandatory("Distribution Option"),
                      #choices = NULL)
                      #),#end column
                      #column(6,
                      #strong("Select the distribution option."),
                      #br(),
                      #conditionalPanel(
                      #condition="input.distribution1=='Hub Pick-Up'",
                      #textOutput("approved_pickup")
                      #),#end conditional panel
                      #conditionalPanel(
                      #condition="input.distribution1=='Direct Shipment'",
                      #textOutput("validation_address_message1"),
                      #textOutput("validation_address_message2")%>%tagAppendAttributes(style='color:black; font-size:12px;font-weight:bold'),
                      #textOutput("validation_address_message3")
                      #),#end conditional panel
                      #)#end column
                      #),# end fluidRow
                      #conditionalPanel(
                      #condition="input.distribution1=='Hub Pick-Up'",
                      fluidRow(
                        column(4, align="center",
                               selectInput("distrohubs1",
                                           label = labelMandatory("Distribution Hub"),
                                           choices = c("choose pick-up hub", distrohubs))
                        ),#end column
                        column(6,
                               strong("Select the distribution hub where you would like to pick up your order."),
                               br(),
                               textOutput("approved_pickup")
                        )#end column
                      ), #end fluidRow
                      fluidRow(
                        column(4, align="center",
                               
                               selectInput("pickupday1",
                                           label = labelMandatory("Pick-Up Day"),
                                           choices = c("choose pick-up day",pickupdays)),
                        ),#end column
                        column(6,
                               strong("Select the day of pick up for this request."),
                               br(),
                        )#end column
                      ),# end fluidRow
                      #),#end conditionalPanel
                      hr(),
                      fluidRow(
                        column(4, align="center",
                               textInput("TargetPopulation1", 
                                         label=labelMandatory("Target Population"), 
                                         placeholder="please give brief description")
                        ),#end column
                        column(6,
                               strong("What population(s) do you expect to receive these inventory?"),
                               br(),
                               textOutput("validation_target_message")
                        )#end column
                      ),# end fluidRow
                      hr(),
                      fluidRow(
                        column(4, align="center",
                               textInput("Notes1", label="Notes", placeholder="this is optional")
                        ),#end column
                        column(6,
                               strong("Do you have any notes or questions for the approver?")
                        )#end column
                      ),# end fluidRow
                      hr(),
                      fluidRow(
                        column(4, align="center",
                               textOutput("validation_messages"),
                               p(),
                               actionButton(inputId = "request", label = "Submit Request", class="submitbutton"),
                               p()
                        )#end column
                      )#end fluidRow
                    )#end condintionalpanel for Vaccine Finder question
          ),#end main panel
          #FOOTER
          uiOutput("footer1")
        )#end fluid page
      })#end render UI
      
      ##Tab 2 - Change Request
      output$tab2 <- renderUI({
        fluidPage(
          mainPanel(width=12, #setting width allows for auto-scaling based on browser window
                    #header with name of user, PIN, and location associated with PIN
                    h3(strong(paste("Welcome, ", WVSIISUserName,"!")), style="text-shadow: 2px 2px 5px #002855; font-size:30px;", align="center"), 
                    
                    p(),
                    br(),
                    tabsetPanel(
                      tabPanel(paste("Requests for Distribution Week", distribution_week_list[match(currentoperationalweek, week_number_list)]),
                               #requests that can be canceled or rescheduled
                               h4(strong(paste("The following requests for", 
                                               RequestPrepopulation$Entity, "(WV Provider PIN:", WVSIISUserWVProviderPINEntered,")
                                          can be canceled or rescheduled. ")), align="center"),
                               br(),
                               #output with buttons and datatable
                               uiOutput("MainBody_currentweek")
                      ),# end tabPanel
                      tabPanel("Future Requests",
                               #requests that can be canceled or rescheduled
                               h4(strong(paste("The following requests for", 
                                               RequestPrepopulation$Entity, "(WV Provider PIN:", WVSIISUserWVProviderPINEntered,")
                                          can be canceled, rescheduled, or changed. ")), align="center"),
                               br(),
                               #conditional panel message
                               conditionalPanel(
                                 condition="blackout_trigger == 'TRUE'",
                                 h4(strong(paste("On Wednesdays and Thursdays, you cannot edit the requests for the upcoming week
                                                      because we are compiling requests and putting together distribution plans. Return Friday to modify 
                                                      those requests in the Requests for Distribution Week tab or call your Stakeholder LNO or 
                                                      call the Help Desk.")), align="center"),
                               ), #end conditional panel
                               #output with buttons and datatable
                               uiOutput("MainBody_futureweeks"),
                               hr()
                      )#end tabPanel
                    )#end tabsetPanel
          ),#end main panel
          #FOOTER
          uiOutput("footer2")
        )#end fluid page
      })#end render UI
      
      ##Tab 3 - Distribution Summary
      output$tab3 <- renderUI({
        fluidPage(
          mainPanel(
            width=12, #setting width allows for auto-scaling based on browser window
            h3(strong(paste("Welcome, ", WVSIISUserName,"!")), style="text-shadow: 2px 2px 5px #002855; font-size:30px;", align="center"), 
            h4(strong(paste("The following is the request history for", 
                            RequestPrepopulation$Entity, "(WV Provider PIN:", WVSIISUserWVProviderPINEntered,").")), align="center"),
            br(),
            fluidRow(
              column(4,
                     selectInput("distroweek",
                                 "Select Distribution Week",
                                 choices=c("choose week", distribution_week_list[(nrow(week_list)-9):(nrow(week_list)-5)])
                     )#end selectInput
              ) #end column
            ),#end fluid row
            br(),
            fluidRow(downloadButton("distrohistory_csv", "Download in CSV", class="submitbutton"),
                     actionButton(inputId="refresh2", label="Refresh", icon = icon('refresh'), class="refreshButton"),
                     helpText("Note that only hub pick-ups are displayed. Direct shipment information is held by DHHR.")
            ),#end fluidRow
            br(),
            br(),
            DTOutput("distrohistory")
          ),#end main panel
          #FOOTER
          uiOutput("footer3")
        )#end fluid page
      })#end render UI
      
      ##Tab 4 - Approved Representatives
      output$tab4 <- renderUI({
        fluidPage(
          mainPanel(width=12, #setting width allows for auto-scaling based on browser window
                    #header with name of user, PIN, and location associated with PIN
                    h3(strong(paste("Welcome, ", WVSIISUserName,"!")), style="text-shadow: 2px 2px 5px #002855; font-size:30px;", align="center"), 
                    h4(strong(paste("Please add, remove, or update any individuals approved for picking up inventory orders below for", 
                                    RequestPrepopulation$Entity, "(WV Provider PIN:", WVSIISUserWVProviderPINEntered,").")), align="center"),
                    p(),
                    br(),
                    #output with buttons and datatable
                    uiOutput("MainBody_approved")
          ),#end main panel
          #FOOTER
          uiOutput("footer4")
        )#end fluid page
      })#end render UI
      
      ##Tab 5 - PIN Profile    
      output$tab5 <- renderUI({
        fluidPage(
          mainPanel(width=12, #setting width allows for auto-scaling based on browser window
                    #header with name of user, PIN, and location associated with PIN
                    h3(strong(paste("Welcome, ", WVSIISUserName,"!")), style="text-shadow: 2px 2px 5px #002855; font-size:30px;", align="center"), 
                    h4(strong(paste("Below is profile information for your WV Provider PIN. Please contact the Help Desk if any of this is incorrect or needs updated.")), align="center"),
                    h4(strong(paste("Account Information for WV Provider PIN #:", WVSIISUserWVProviderPINEntered)), align="center"),
                    fluidRow(column(4,
                                    #strong("Organization Name:",style="font-size:15px;"), 
                                    #br(),
                                    #em(paste(RequestPrepopulation$OrgName)),
                                    #p(),
                                    strong("Entity Name:",style="font-size:15px;"),
                                    br(),
                                    em(paste(RequestPrepopulation$Entity)),
                                    p(),
                                    strong("Entity Type:",style="font-size:15px;"),
                                    br(),
                                    em(paste(RequestPrepopulation$EntityType))
                    ),#end column
                    column(4,
                           strong("Shipping Address:",style="font-size:15px;"),
                           br(),
                           em(paste(RequestPrepopulation$ShippingAddress)),
                           p(), 
                           strong("County:",style="font-size:15px;"),
                           br(),
                           em(paste(RequestPrepopulation$County))
                    ),#end column
                    column(4,
                           strong("Contact Name:",style="font-size:15px;"),
                           br(),
                           em(paste(RequestPrepopulation$POCName)),
                           p(),
                           strong("Contact Phone:",style="font-size:15px;"),
                           br(),
                           em(paste(RequestPrepopulation$POCPhone)),
                           p(),
                           strong("Contact E-Mail:",style="font-size:15px;"),
                           br(),
                           em(paste(RequestPrepopulation$POCEmail))
                    )#end column
                    ) #end fluidRow,
          ),#end main panel
          #FOOTER
          uiOutput("footer5")
        )#end fluid page
      })#end render UI
      
      
      #-----Start Approved Representatives-----
      
      ##create renderUI for table
      output$MainBody_approved<-renderUI({
        fluidPage(
          hr(),
          br(),
          h4("All changes are automatically saved. Select row to edit/delete."),
          ####in-line buttons for Add, Edit, Delete, Download
          actionButton(inputId = "Add_row_head",label = "Add", class="addButton"), 
          actionButton(inputId = "mod_row_head",label = "Edit", class="changeButton"), 
          actionButton(inputId = "Del_row_head",label = "Delete", class="cancelOrRemoveButton"), 
          actionButton(inputId="refresh1", label="Refresh", icon = icon('refresh'), class="refreshButton"),
          downloadButton("approved_csv", "Download CSV", class="submitbutton"),
          br(),
          br(),
          column(6,
                 dataTableOutput("approved_table"),
                 tags$script("$(document).on('click', '#approved_table button', function () {
                   Shiny.onInputChange('lastClickId',this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });")
          )#end column
        ) #end fluid page 
      })# end renderUI
      
      ##render the datatable for renderUI
      output$approved_table<-renderDataTable({
        #just the 6-digits of the PIN
        WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
        ### read in data
        approved_all <- fread("data/Approved_Representatives.csv")
        DT <-approved_all%>% filter(WVProviderPIN==as.numeric(WVSIISUserWVProviderPIN)) #only show records for PIN
        datatable(DT, selection = 'single', escape=F, rownames=FALSE, options = list(dom = 't',autoWidth = TRUE)) #allows for only selecting one row at a time
      }) #end renderDataTable
      
      #refresh button
      observeEvent(input$refresh1,{
        #update data and table
        output$approved_table<-renderDataTable({
          #just the 6-digits of the PIN
          WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
          ### read in data
          approved_all <- fread("data/Approved_Representatives.csv")
          DT <-approved_all%>% filter(WVProviderPIN==as.numeric(WVSIISUserWVProviderPIN)) #only show records for PIN
          datatable(DT, selection = 'single', escape=F, rownames=FALSE, options = list(dom = 't',autoWidth = TRUE)) #allows for only selecting one row at a time
        }) #end renderDataTable
      })#end observe event
      
      ###This is the pop up board for input a new row
      observeEvent(input$Add_row_head, {
        showModal(modalDialog(title = "Add a new approved representative",
                              textInput(paste0("Name_add", input$Add_row_head), label="Approved Representative Name:", placeholder="..."),
                              textInput(paste0("Phone_add", input$Add_row_head), label="Approved Representative Phone Number:", placeholder="###-###-####"),
                              br(),
                              textOutput("validation_messages1"),
                              br(),
                              actionButton("go", "Add New Person", class="addButton"),
                              modalButton("Cancel"),
                              easyClose = TRUE, footer = NULL, fade=TRUE
        )#end showModal
        )#end modalDialog
      }) #end observeEvent
      
      ###validation message for dialog box and to stop submission of change if error
      validation1 <- reactive({
        validate(
          #check that name starts with a letter
          need(grep("^[a-zA-Z].*",input[[paste0("Name_add", input$Add_row_head)]], value = FALSE), "Please enter name of new person."),
          #validate that phone number starts with a number
          need(grep("^[0-9].*",input[[paste0("Phone_add", input$Add_row_head)]], value = FALSE), "Please enter phone number of new person.")
        )
      })#end validation1
      ####validation messages in modal dialog box  
      output$validation_messages1 <- renderText(
        paste(validation1()$run)) 
      
      ### Add a new row to DT  
      observeEvent(input$go, {
        ####validations to stop row add
        validation1()
        #just the 6-digits of the PIN
        WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
        ####add row if validations are fulfilled
        new_row=data.frame(
          'WVProviderPIN'=WVSIISUserWVProviderPIN,
          'PickUpFullName'=input[[paste0("Name_add", input$Add_row_head)]],
          'PickUpPhoneNumber'=input[[paste0("Phone_add", input$Add_row_head)]]
        )#end data.frame
        #remove any punctuations that aren't periods, commas, parentheses from name
        new_row$PickUpFullName <- gsub("[^[:alnum:]\\.\\,\\(\\)\\[:space:]]", "", new_row$PickUpFullName)
        ####add new record to dataframe
        new_row <- t(apply(new_row,2,as.character))
        ####remove dialog box
        removeModal()
        ####write change to file
        fwrite(new_row, "data/Approved_Representatives.csv", append=TRUE, row.names=FALSE)
        #update table
        output$approved_table<-renderDataTable({
          ### read in data
          approved_all <- fread("data/Approved_Representatives.csv")
          DT <-approved_all%>% filter(WVProviderPIN==as.numeric(WVSIISUserWVProviderPIN)) #only show records for PIN
          datatable(DT, selection = 'single', escape=F, rownames=FALSE, options = list(dom = 't',autoWidth = TRUE)) #allows for only selecting one row at a time
        }) #end renderDataTable
        ####confirm change
        showModal(modalDialog(title="Your changes have been saved!", 
                              modalButton("OK"),
                              easyClose = TRUE, footer = NULL, fade=TRUE
        ) #end modalDialog
        )#end showModal
      }) #end observeEvent
      
      ## delete selected rows part
      ### this is warning message for deleting
      observeEvent(input$Del_row_head,{
        #just the 6-digits of the PIN
        WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
        ### read in data
        approved_all <- fread("data/Approved_Representatives.csv")
        approved_row_index <-cbind(approved_all,data.frame(as.numeric(rownames(approved_all))))
        names(approved_row_index)[4] <- "rn" #first empty column
        approved_row_index_filtered <-approved_row_index[approved_row_index$WVProviderPIN==as.numeric(WVSIISUserWVProviderPIN)]
        showModal(
          if(length(input$approved_table_rows_selected)>=1 ){
            modalDialog(
              title = "Warning",
              paste("Are you sure you want to delete the record for", 
                    approved_all$PickUpFullName[approved_row_index_filtered$rn[input$approved_table_rows_selected]],"?" ), #name of selected record
              footer = tagList(
                modalButton("Cancel"),
                actionButton("ok", "Yes")
              ), easyClose = TRUE)
          }else{
            modalDialog(
              title = "Warning",
              paste("Please select row that you want to delete." ),easyClose = TRUE)#end modalDialog
          }#end else
        ) #end showModal
      }) #end observeEvent
      
      ### If user say OK, then delete the selected rows
      observeEvent(input$ok, {
        #just the 6-digits of the PIN
        WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
        ####refresh approved_all, reactive, index
        approved_all <- fread("data/Approved_Representatives.csv")
        approved_row_index <-cbind(approved_all,data.frame(as.numeric(rownames(approved_all))))
        names(approved_row_index)[4] <- "rn" #first empty column
        approved_row_index_filtered <-approved_row_index[approved_row_index$WVProviderPIN==as.numeric(WVSIISUserWVProviderPIN)]
        ####remove row
        approved_all=approved_all[-approved_row_index_filtered$rn[input$approved_table_rows_selected]]
        ####remove dialog box
        removeModal()
        ####write to file
        fwrite(approved_all, "data/Approved_Representatives.csv")
        #update table
        output$approved_table<-renderDataTable({
          approved_all <- fread("data/Approved_Representatives.csv")
          DT <-approved_all%>% filter(WVProviderPIN==as.numeric(WVSIISUserWVProviderPIN)) #only show records for PIN
          datatable(DT, selection = 'single', escape=F, rownames=FALSE, options = list(dom = 't',autoWidth = TRUE)) #allows for only selecting one row at a time
        }) #end renderDataTable
        ####confirm change
        showModal(modalDialog(title="Your changes have been saved!", 
                              modalButton("OK"),
                              easyClose = TRUE, footer = NULL, fade=TRUE
        ) #end modalDialog
        )#end showModal
      })
      
      ### edit button
      observeEvent(input$mod_row_head,{
        showModal(
          if(length(input$approved_table_rows_selected)>=1 ){
            modalDialog(
              fluidPage(
                h3(strong("Modification of Record"),align="center"),
                hr(),
                dataTableOutput('row_modif'),
                actionButton("save_changes","Save changes", class="submitbutton"),
                tags$script(HTML("$(document).on('click', '#save_changes', function () {
                             var list_value=[]
                             for (i = 0; i < $( '.new_input' ).length; i++)
                             {
                             list_value.push($( '.new_input' )[i].value)
                             }
                             Shiny.onInputChange('newValue', list_value) });")) ), size="l" )
          }else{
            modalDialog(
              title = "Warning",
              paste("Please select the row that you want to edit" ),easyClose = TRUE) #end modalDialog
          }#end else
        ) #end showModal
      }) #end observeEvent
      
      ### modify part - format of  inputs
      output$row_modif<-renderDataTable({
        #just the 6-digits of the PIN
        WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
        selected_row=input$approved_table_rows_selected
        #use updated values
        approved_all <- fread("data/Approved_Representatives.csv")
        approved_row_index <-cbind(approved_all,data.frame(as.numeric(rownames(approved_all))))
        names(approved_row_index)[4] <- "rn" #first empty column
        approved_row_index_filtered <-approved_row_index[approved_row_index$WVProviderPIN==as.numeric(WVSIISUserWVProviderPIN)]
        old_row=approved_all[approved_row_index_filtered$rn[input$approved_table_rows_selected]]
        row_change=list()
        ####find format of existing columns
        for (i in colnames(old_row))
        {
          if (is.numeric(approved_all[[i]]))
          {
            row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"','  type="number" id=new_',i,' ><br>')
          } 
          else if(is.Date(approved_all[[i]])){
            row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="date" id=new_  ',i,'  ><br>') 
          }
          else 
            row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="textarea"  id=new_',i,'><br>')
        }
        row_change=as.data.table(row_change)
        setnames(row_change,colnames(old_row))
        DT=row_change[,1:3] #only fields we want to allow to edit in pop up window
        DT 
      },
      escape=F,options=list(dom='t',ordering=F,scrollX = TRUE),selection="none") #end renderDataTable
      
      ### This is to replace the modified row to existing row
      observeEvent(input$newValue,{
        #just the 6-digits of the PIN
        WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
        #use updated values
        approved_all <- fread("data/Approved_Representatives.csv")
        approved_row_index <-cbind(approved_all,data.frame(as.numeric(rownames(approved_all))))
        names(approved_row_index)[4] <- "rn" #first empty column
        approved_row_index_filtered <-approved_row_index[approved_row_index$WVProviderPIN==as.numeric(WVSIISUserWVProviderPIN)]
        newValue=lapply(input$newValue, function(col) {
          if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
            as.numeric(as.character(col))
          } else {
            col
          }
        })
        DF=data.frame(lapply(newValue, function(x) t(data.frame(x))))
        colnames(DF)=colnames(approved_all)
        #remove any punctuations that aren't periods, commas, parentheses from name
        DF$PickUpFullName <- gsub("[^[:alnum:]\\.\\,\\(\\)\\[:space:]]", "", DF$PickUpFullName)
        approved_all[approved_row_index_filtered$rn[input$approved_table_rows_selected]]<-DF
        ####write to file
        fwrite(approved_all, "data/Approved_Representatives.csv")
        #update table
        output$approved_table<-renderDataTable({
          approved_all <- fread("data/Approved_Representatives.csv")
          DT <-approved_all%>% filter(WVProviderPIN==as.numeric(WVSIISUserWVProviderPIN)) #only show records for PIN
          datatable(DT, selection = 'single', escape=F, rownames=FALSE, options = list(dom = 't',autoWidth = TRUE)) #allows for only selecting one row at a time
        }) #end renderDataTable
        ####confirm change
        showModal(modalDialog(title="Your changes have been saved!", 
                              modalButton("OK"),
                              easyClose = TRUE, footer = NULL, fade=TRUE
        ) #end modalDialog
        )#end showModal
      }) #end observeEvent
      
      ### download the table in csv
      output$approved_csv<- downloadHandler(
        filename = function() {
          #just the 6-digits of the PIN
          WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
          paste("Approved_Representatives_", WVSIISUserWVProviderPIN,"_",Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          #just the 6-digits of the PIN
          WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
          approved_all <- fread("data/Approved_Representatives.csv")
          filtereddata <- approved_all %>% filter(WVProviderPIN==as.numeric(WVSIISUserWVProviderPIN))
          write.csv(filtereddata, file, row.names = F)
        }
      )           
      #-----End Approved Representatives-----
      
      #----Start New Vaccine Request----
      
      ##validations for data inputs and messages before Submit Request is allowed
      
      ###Did the entity report to Vaccine Finder in the past week?
      observeEvent(input$inventoryfinder,{
        if(input$inventoryfinder=="No"){
          output$inventory_finder_warning <-renderText({paste("Please report inventory into Inventory Finder, then select 'Yes'. 
          Select 'Not Applicable' if this requirement does not apply to you and/or have not ordered inventory before.
          Contact the Help Desk if you have questions.")})
        } else {
          output$inventory_finder_warning <-renderText({paste("")})
        }
      })#end observeEvent
      
      ###How many vials were requested for Janssen one-dose? Is it enough for delivery?
      observeEvent(c(input$inventory1, input$RequestedUnitsinv3),{
        inventory <- input$inventory1
        #check for blanks and nulls
        if(is.numeric(input$RequestedUnitsinv3)==FALSE){
          inv3units=0
        }else{
          inv3units=input$RequestedUnitsinv3
        }
        
        #display sum of vials as doses
        output$requested_quantities_inv3 <- renderText({paste("Total Quantities Requested:",(inv3_quantities_in_unit*inv3units),"(",inv3_quantities_in_unit,"quantities per unit)")})
        
        ####Is it minimum or more?
        if(inventory=="Inv3" && inv3units <order_minimum){
          #display minimum vial message
          output$minimum_unit_warning <- renderText({
            paste("There is a minimum of", order_minimum,"total units per inventory type.")})
        } else {
          output$minimum_unit_warning <- renderText({paste("")})
        }
        
        #delivery minimum message and distribution options
        #excess_janssen <- janssen_container - (input$RequestedVialsjanssen%%janssen_container)
        #vaccine_delivery <- paste("Note: Must order Janssen in increments of",janssen_container,"for direct shipment option. 
        #Add",excess_janssen,"more vial(s) for this option.")
        #if((vaccines=="Janssen" && input$RequestedVialsjanssen<janssen_container) | 
        # (vaccines=="Janssen" && input$RequestedVialsjanssen%%janssen_container != 0)){
        choices <- list("choose distribution option", "Hub Pick-Up")
        #note about increments of vials after Vials field in UI
        #output$vaccine_delivery_message <- renderText(paste(vaccine_delivery))
        #} else {
        #choices <-list("choose distribution option","Hub Pick-Up","Direct Shipment")
        output$inventory_delivery_message <- renderText(paste(""))
        #}
        updateSelectInput(session, "distribution1", choices = choices)
        
      })#end observeEvent
      
      ###How many vials were requested for Pfizer two-dose? Is it enough for delivery?
      observeEvent(c(input$inventory1, input$RequestedUnitsInv1Prime, input$RequestedUnitsInv1Boost), {
        inventory <- input$inventory1
        #check for blanks and zeroes
        if(is.numeric(input$RequestedUnitsInv1Prime)==FALSE){
          inv1prime=0
        }else{
          inv1prime=input$RequestedUnitsInv1Prime
        }
        if(is.numeric(input$RequestedUnitsInv1Boost)==FALSE){
          inv1boost=0
        }else{
          inv1boost=input$RequestedUnitsInv1Boost
        }
        
        #display sum of what is in the fields
        TotalRequestedUnitsP <- inv1prime + inv1boost
        output$requested_units_inv1 <- renderText({paste("Total Units Requested:",TotalRequestedUnitsP)})
        #display sum of vials as doses
        output$requested_quantities_inv1 <- renderText({paste("Total Quantities Requested:",(inv1_quantities_in_unit*TotalRequestedUnitsP),"(",inv1_quantities_in_unit,"quantities per unit)")})
        
        ####Does it add to minimum or more?
        if(inventory=="Inv1" && TotalRequestedUnitsP <order_minimum){
          #display minimum vial message
          output$minimum_unit_warning <- renderText({
            paste("There is a minimum of", order_minimum,"total units per inventory type.")})
        } else {
          output$minimum_unit_warning <- renderText({paste("")})
        }
        
        #delivery minimum message and distribution options
        #excess_pfizer <- pfizer_container - (TotalRequestedVialsP%%pfizer_container)
        #vaccine_delivery <- paste("Note: Must order Pfizer in increments of",pfizer_container,"for direct shipment option. 
        #Add",excess_pfizer,"more vial(s) for this option.")
        #if((vaccines=="Pfizer" && TotalRequestedVialsP<pfizer_container) | 
        #(vaccines=="Pfizer" && TotalRequestedVialsP%%pfizer_container != 0)){
        choices <- list("choose distribution option", "Hub Pick-Up")
        #note about increments of vials after Vials field in UI
        #output$vaccine_delivery_message <- renderText(paste(vaccine_delivery))
        #} else {
        #choices <-list("choose distribution option","Hub Pick-Up","Direct Shipment")
        output$inventory_delivery_message <- renderText(paste(""))
        #}
        updateSelectInput(session, "distribution1", choices = choices)
        
      }) #end observe event
      
      ###How many vials were requested for Pfizer Pediatric two-dose? Is it enough for delivery?
      observeEvent(c(input$inventory1, input$RequestedUnitsInv4Prime, input$RequestedUnitsInv4Boost), {
        inventory <- input$inventory1
        #check for blanks and zeroes
        if(is.numeric(input$RequestedUnitsInv4Prime)==FALSE){
          inv4prime=0
        }else{
          inv4prime=input$RequestedUnitsInv4Prime
        }
        if(is.numeric(input$RequestedUnitsInv4Boost)==FALSE){
          inv4boost=0
        }else{
          inv4boost=input$RequestedUnitsInv4Boost
        }
        
        #display sum of what is in the fields
        TotalRequestedUnitsPP <- inv4prime + inv4boost
        output$requested_units_inv4 <- renderText({paste("Total Units Requested:",TotalRequestedUnitsPP)})
        #display sum of vials as doses
        output$requested_quantities_inv4 <- renderText({paste("Total Quantities Requested:",(inv4_quantities_in_unit*TotalRequestedUnitsPP),"(",inv4_quantities_in_unit,"quantities per unit)")})
        
        ####Does it add to minimum or more?
        if(inventory=="Inv4" && TotalRequestedUnitsPP <order_minimum){
          #display minimum vial message
          output$minimum_unit_warning <- renderText({
            paste("There is a minimum of", order_minimum,"total units per inventory type.")})
        } else {
          output$minimum_unit_warning <- renderText({paste("")})
        }
        
        #delivery minimum message and distribution options
        #excess_pfizer_pediatric <- pfizer_pediatric_container - (TotalRequestedVialsPP%%pfizer_pediatric_container)
        #vaccine_delivery <- paste("Note: Must order Pfizer Pediatric in increments of",pfizer_pediatric_container,"for direct shipment option. 
        #Add",excess_pfizer_pediatric,"more vial(s) for this option.")
        #if((vaccines=="Pfizer Pediatric" && TotalRequestedVialsPP<pfizer_pediatric_container) | 
        #(vaccines=="Pfizer Pediatric" && TotalRequestedVialsPP%%pfizer_pediatric_container != 0)){
        choices <- list("choose distribution option", "Hub Pick-Up")
        #note about increments of vials after Vials field in UI
        #output$vaccine_delivery_message <- renderText(paste(vaccine_delivery))
        #} else {
        #choices <-list("choose distribution option","Hub Pick-Up","Direct Shipment")
        output$inventory_delivery_message <- renderText(paste(""))
        #}
        updateSelectInput(session, "distribution1", choices = choices)
        
      }) #end observe event
      
      ###delivery option based on vaccine and number of vials for Moderna
      observeEvent(c(input$inventory1, input$RequestedUnitsInv2Prime, input$RequestedUnitsInv2Boost), {
        inventory <- input$inventory1
        #adjust for blanks and nulls
        if(is.numeric(input$RequestedUnitsInv2Prime)==FALSE){
          inv2prime=0
        }else{
          inv2prime=input$RequestedUnitsInv2Prime
        }
        if(is.numeric(input$RequestedUnitsInv2Boost)==FALSE){
          inv2boost=0
        }else{
          inv2boost=input$RequestedUnitsInv2Boost
        }
        
        #display sum of what is in the fields
        TotalRequestedUnitsM <- inv2prime + inv2boost
        output$requested_units_inv2 <- renderText({paste("Total Units Requested:",TotalRequestedUnitsM)})
        #display sum of vials as doses
        output$requested_quantities_inv2 <- renderText({paste("Total Quantities Requested:",(inv2_quantities_in_unit*TotalRequestedUnitsM),"(",inv2_quantities_in_unit,"quantities per unit)")})
        
        ####Does it add to minimum or more?
        if(inventory=="Inv2" && TotalRequestedUnitsM <order_minimum){
          #display minimum vial message
          output$minimum_unit_warning <- renderText({
            paste("There is a minimum of", order_minimum,"total units per inventory type.")})
        } else {
          output$minimum_unit_warning <- renderText({paste("")})
        }
        
        #delivery minimum message and distribution options
        #excess_moderna <- moderna_container - (TotalRequestedVialsM%%moderna_container)
        #vaccine_delivery <- paste("Note: Must order Moderna in increments of",moderna_container,"for direct shipment option. 
        #Add",excess_moderna,"more vial(s) for this option.")
        #if((vaccines=="Moderna" && TotalRequestedVialsM<moderna_container) | 
        #(vaccines=="Moderna" && TotalRequestedVialsM%%moderna_container != 0)){
        choices <- list("choose distribution option", "Hub Pick-Up")
        #note about increments of vials after Vials field in UI
        #output$vaccine_delivery_message <- renderText(paste(vaccine_delivery))
        #} else {
        #choices <-list("choose distribution option","Hub Pick-Up","Direct Shipment")
        output$inventory_delivery_message <- renderText(paste(""))
        #}
        updateSelectInput(session, "distribution1", choices = choices)
      })   #end observeEvent
      
      
      ###If Hub Pick-Up is selected for Delivery Method, is at least 1 person an approved representative for pick up?
      #observeEvent(input$distribution1, {
      observe({
        #just the 6-digits of the PIN
        WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
        ####who is approved
        approved_all <- fread("data/Approved_Representatives.csv")
        approved_by_PIN <-approved_all%>% 
          filter(WVProviderPIN==as.numeric(WVSIISUserWVProviderPIN))%>%
          select("PickUpFullName")
        ####if they are picking up, is there someone on the list
        if(nrow(approved_by_PIN)==0){
          #if(input$distribution1=="Hub Pick-Up" && length(approved_by_PIN)==0){ 
          message=1
        }else{
          if(nrow(approved_by_PIN)>0){
            #if(input$distribution1=="Hub Pick-Up" && length(approved_by_PIN)>0){
            message=2
          }else{
            message=3
          } #end else
        }#end else
        ####message about approved representatives
        approvedmessage <- switch(message, "1" = "There are currently no individuals listed as approved representatives 
                                    for picking up inventory for this WV Provider PIN. 
                                    This needs to be corrected before you submit your request. 
                                    You can add them in the Approved Representatives tab, 
                                    or you can contact the Help Desk for assistance.",
                                  "2" = paste("The following individuals are approved representatives for this WV Provider PIN:",
                                              sapply(approved_by_PIN,paste,collapse=", "), ".  If one of these individuals won't be available, 
                                            please update in the Approved Representatives tab."),
                                  "3" = "")
        output$approved_pickup <- renderText({paste(approvedmessage)})
      })
      
      ####validation messages in UI for shipping address  
      #output$validation_address_message1 <- renderText({"The following address is on file for this location for deliveries. If this is not the address where this 
      #needs to be shipped, please update your profile with DHHR or call the Help Desk before 
      #placing the request:"})
      #output$validation_address_message2 <- renderText({paste(RequestPrepopulation$ShippingAddress)})
      #output$validation_address_message3 <- renderText({"Please note that changes cannot be made to direct shipment requests after the Tuesday at 11:59 PM prior to shipment."})
      
      ###validation checks for submit click    
      validation <- reactive({
        validate(
          #validate that Prime and Boost vials are a whole number
          need(input$RequestedUnitsInv1Prime%%1==0 ||
                 input$RequestedUnitsInv2Prime%%1==0 ||
                 input$RequestedUnitsinv3%%1==0 ||
                 input$RequestedUnitsInv4Prime%%1==0 ||
                 input$RequestedUnitsInv1Boost%%1==0 ||
                 input$RequestedUnitsInv4Boost%%1==0 ||
                 input$RequestedUnitsInv2Boost%%1==0
               ,"Only whole units are to be ordered."),
          #validate that at least the minimum vials have been ordered
          need(input$RequestedUnitsInv1Prime + input$RequestedUnitsInv1Boost >= order_minimum ||
                 input$RequestedUnitsInv2Prime + input$RequestedUnitsInv2Boost >= order_minimum ||
                 input$RequestedUnitsInv4Prime + input$RequestedUnitsInv4Boost >= order_minimum ||
                 input$RequestedUnitsinv3 >= order_minimum
               ,"You must enter at least the minimum order amount."),
          #validate that week was selected, brand was selected, distribution type
          #need(input$week1 != "choose week" &&
          #input$vaccines1 != "choose vaccine brand" &&
          #input$distribution1 != "choose distribution option"
          #, "You are missing a required field."),
          #validate that week was selected, brand was selected
          need(input$week1 != "choose week" &&
                 input$inventory1 != "choose inventory type"
               , "You are missing a required field."),
          #validate that distribution hub and pick up day are selected if Hub Pick-Up
          #if(input$distribution1=="Hub Pick-Up"){
          need(input$distrohubs1 != "choose pick-up hub" &&
                 input$pickupday1 != "choose pick-up day"
               , "You are missing a required field.")
          #}
          ,
          #Does account have all the profile information?
          need(is.null(RequestPrepopulation$Entity)== FALSE &&
                 is.null(RequestPrepopulation$EntityType)== FALSE &&
                 is.null(RequestPrepopulation$County)== FALSE &&
                 is.null(RequestPrepopulation$WVProviderPIN)== FALSE &&
                 is.null(RequestPrepopulation$POCName)== FALSE &&
                 is.null(RequestPrepopulation$POCPhone)== FALSE &&
                 is.null(RequestPrepopulation$POCEmail)== FALSE
               ,"There is necessary information missing from your location's WV SIIS profile to place a inventory request. Contact Help Desk for assistance.")
        )#end validate
      })#end validation
      
      ####validation messages in UI  
      output$validation_messages <- renderText(
        paste(validation()$run)) 
      
      ###target population validation
      validation_target <- reactive({
        validate(
          ###Was Target Population answered?
          need(input$TargetPopulation1 != "", "Please include a target population. If unsure, use 'Local residents'.")
        )#end validate
      })#end validation target
      
      ####validation messages in UI  
      output$validation_target_message <- renderText(
        paste(validation_target()$run)) 
      
      ### Add a new row(s) to PIN requests
      observeEvent(input$request, {
        ####validations to stop row add
        validation()
        validation_target()
        #just the 6-digits of the PIN
        WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
        #### distribution week to operational week
        operationalweek <- week_number_list[match(input$week1, distribution_week_list)] #find equivalent of operational week to distribution week,
        ####logic for brand and number of vials
        primeunits <- switch(input$inventory1,
                             "Inv1" = input$RequestedUnitsInv1Prime,
                             "Inv4" = input$RequestedUnitsInv4Prime,
                             "Inv2"= input$RequestedUnitsInv2Prime,
                             "Inv3" = input$RequestedUnitsinv3)
        boostunits <- switch(input$inventory1,
                             "Inv1" = input$RequestedUnitsInv1Boost,
                             "Inv4" = input$RequestedUnitsInv4Boost,
                             "Inv2"= input$RequestedUnitsInv2Boost,
                             "Inv3" = NULL)
        
        ####add row if validations are fulfilled for gub pick up Prime Vials if applicable
        #if(primevials!=0 && is.null(primevials)==FALSE && input$distribution1=="Hub Pick-Up"){
        if(primeunits!=0 && is.null(primeunits)==FALSE){
          new_row=data.frame(
            "RequestStatus"="Pending",
            "OperationalWeek"= as.character(operationalweek),
            "WVProviderPIN"=WVSIISUserWVProviderPIN,
            "EntityName"=RequestPrepopulation$Entity,
            "EntityType"=RequestPrepopulation$EntityType,
            "EntityGroup"=EntityGroup,
            "County"=RequestPrepopulation$County,
            "TargetPopulation"=input$TargetPopulation1,
            "InventoryType"=input$inventory1,
            "Series"="Prime", #different from prime and boost record  updates
            "RequestedUnits"=primeunits,#different from prime and boost record  updates
            "POCName"=RequestPrepopulation$POCName,
            "POCPhone"=RequestPrepopulation$POCPhone,
            "POCEmail"=RequestPrepopulation$POCEmail,
            "NotesToApprover"=input$Notes1,
            "NotesFromApprover"="",
            #"DeliveryMethod"=input$distribution1,
            "DeliveryMethod"="Hub Pick-Up",
            "DistributionHub"=input$distrohubs1,
            "ShippingAddress"="",
            "PickUpDay"=input$pickupday1
          )#end data.frame
          ####add new record to dataframe
          new_row <- t(apply(new_row,2,as.character))
          request_data<-rbind(request_data,new_row, fill=TRUE)
          ####write change to file
          fwrite(new_row, paste("data/Requests/",as.character(WVSIISUserWVProviderPIN),".csv", sep=""), append=TRUE, row.names = FALSE) 
        } #end primevials row add
        
        ####add row if validations are fulfilled for hub pick up Boost Vials if applicable
        #if(boostvials!=0 && is.null(boostvials)==FALSE && input$distribution1=="Hub Pick-Up"){
        if(boostunits!=0 && is.null(boostunits)==FALSE){
          new_row=data.frame(
            "RequestStatus"="Pending",
            "OperationalWeek"= as.character(operationalweek),
            "WVProviderPIN"=WVSIISUserWVProviderPIN,
            "EntityName"=RequestPrepopulation$Entity,
            "EntityType"=RequestPrepopulation$EntityType,
            "EntityGroup"=EntityGroup,
            "County"=RequestPrepopulation$County,
            "TargetPopulation"=input$TargetPopulation1,
            "InventoryType"=input$inventory1,
            "Series"="Boost", #different from prime and boost record  updates
            "RequestedUnits"=boostunits,#different from prime and boost record  updates
            "POCName"=RequestPrepopulation$POCName,
            "POCPhone"=RequestPrepopulation$POCPhone,
            "POCEmail"=RequestPrepopulation$POCEmail,
            "NotesToApprover"=input$Notes1,
            "NotesFromApprover"="",
            #"DeliveryMethod"=input$distribution1,
            "DeliveryMethod"="Hub Pick-Up",
            "DistributionHub"=input$distrohubs1,
            "ShippingAddress"="",
            "PickUpDay"=input$pickupday1
          )#end data.frame
          ####add new record to dataframe
          new_row <- t(apply(new_row,2,as.character))
          request_data<-rbind(request_data,new_row, fill=TRUE)
          ####write change to file
          fwrite(new_row, paste("data/Requests/",as.character(WVSIISUserWVProviderPIN),".csv", sep=""), append=TRUE, row.names = FALSE) 
        } #end boostvials row add
        
        ####add rows if validations are fulfilled for direct shipment
        #if(input$distribution1=="Direct Shipment"){
        #new_row=data.frame(
        #"RequestStatus"="Pending",
        #"OperationalWeek"= as.character(operationalweek),
        #"WVProviderPIN"=WVSIISUserWVProviderPIN,
        #"EntityName"=RequestPrepopulation$Entity,
        #"EntityType"=RequestPrepopulation$EntityType,
        #"EntityGroup"=EntityGroup,
        #"County"=RequestPrepopulation$County,
        #"TargetPopulation"=input$TargetPopulation1,
        #"VaccineBrand"=input$vaccines1,
        #"Series"="Prime", #direct shipments are listed as prime, but actual breakdown is in the NotesFromApprover field
        #"RequestedVials"=ifelse(is.null(boostvials)==FALSE && is.null(primevials)==FALSE, primevials+boostvials, primevials),
        #"POCName"=RequestPrepopulation$POCName,
        #"POCPhone"=RequestPrepopulation$POCPhone,
        #"POCEmail"=RequestPrepopulation$POCEmail,
        #"NotesToApprover"=input$Notes1,
        #"NotesFromApprover"=paste("prime vials:",primevials,"; boost vials:",boostvials),
        #"DeliveryMethod"=input$distribution1,
        #"DistributionHub"="Direct Shipment",
        #"ShippingAddress"=RequestPrepopulation$ShippingAddress,
        #"PickUpDay"=""
        #)#end data.frame
        ####add new record to dataframe
        #new_row <- t(apply(new_row,2,as.character))
        #new_row$TargetPopulation <- as.character(new_row$TargetPopulation)
        #new_row$NotesToApprover <- as.character(new_row$NotesToApprover)
        #request_data<-rbind(request_data,new_row, fill=TRUE)
        ####write change to file
        #fwrite(new_row, paste("data/Requests/",as.character(WVSIISUserWVProviderPIN),".csv", sep=""), append=TRUE, row.names = FALSE) 
        #} #end direct ship row add
        
        ####reset inputs
        updateSelectInput(session, "week1", selected = "choose week")
        updateSelectInput(session, "inventory1", selected = "choose inventory type")
        updateNumericInput(session, "RequestedUnitsInv1Prime", value = 0)
        updateNumericInput(session, "RequestedUnitsInv1Boost", value = 0)
        updateNumericInput(session, "RequestedUnitsInv4Prime", value = 0)
        updateNumericInput(session, "RequestedUnitsInv4Boost", value = 0)
        updateNumericInput(session, "RequestedUnitsInv2Prime", value = 0)
        updateNumericInput(session, "RequestedUnitsInv2Boost", value = 0)
        updateNumericInput(session, "RequestedUnitsinv3", value = 0)
        #updateSelectInput(session, "distribution1", choices=NULL)
        updateSelectInput(session, "distrohubs1", selected="choose pick-up hub")
        updateSelectInput(session, "pickupday1", selected="choose pick-up day")
        updateTextInput(session, "TargetPopulation1", value = "")
        updateTextInput(session, "Notes1", value = "")
        
        ####confirm changes
        showModal(modalDialog(title="Your request has been submitted!", 
                              modalButton("OK"),
                              easyClose = TRUE, footer = NULL, fade=TRUE
        ) #end modalDialog
        )#end showModal
      }) #end observeEvent
      
      #-----End New Vaccine Request---- 
      
      #-----Start Distribution History-----
      
      ###Get distro data by week number of files using input$distroweek
      observe({
        if(input$distroweek!="choose week" && is.null(input$distroweek)==FALSE){
          selected_week_number <- week_number_list[match(input$distroweek, distribution_week_list)]
          file_finder <- list.files(path = distropath, pattern=paste0("^.*(_Week_", selected_week_number,").*$"), 
                                    all.files=FALSE, full.names=TRUE) #names of files for the selected week
          if(length(file_finder)==0){
            #if no data for week
            distrobyPIN<-setNames(data.frame(matrix(ncol = 10, nrow = 0)),
                                  c("WVProviderPIN","WorkOrder","InventoryType","Series","RequestedUnits",
                                    "AllottedUnits","DeliveredUnits","DistributionHub","PickUpDay", "RequestStatus"))
          }else{
            distrobyweek <- NULL #set placeholder for file; reset each time to catch any updates
            for (data in file_finder){ #for all information in the files for files identified in file_finder
              temphold <-read.csv(data, header=TRUE) #create a temp file
              distrobyweek <-unique(rbind(distrobyweek, temphold)) #add temp to request file and filter for unique entries
              rm(temphold) #remove temp
            }#end for
            ###filter for display and file
            #just the 6-digits of the PIN
            WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
            distrobyPIN <- distrobyweek%>%
              filter(WVProviderPIN==WVSIISUserWVProviderPIN)%>%
              select("WVProviderPIN","WorkOrder","InventoryType","Series","RequestedUnits","AllottedUnits","DeliveredUnits","DistributionHub","PickUpDay", "RequestStatus")
          }#end else
          ### DT output for request history
          output$distrohistory <- renderDT(datatable(distrobyPIN, rownames=FALSE))
          
          ### download csv for request history
          output$distrohistory_csv<- downloadHandler(
            filename = function() {
              #just the 6-digits of the PIN
              WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
              paste("Distribution_History_",WVSIISUserWVProviderPIN, ".csv", sep="")
            },
            content = function(file){
              write.csv(distrobyPIN, file,row.names=FALSE)
            }
          ) #end downloadHandler
        }#end if
      })#end observe
      
      #refresh button
      observeEvent(input$refresh2,{
        #redo everything
        if(input$distroweek!="choose week" && is.null(input$distroweek)==FALSE){
          selected_week_number <- week_number_list[match(input$distroweek, distribution_week_list)]
          file_finder <- list.files(path = distropath, pattern=paste0("^.*(_Week_", selected_week_number,").*$"), 
                                    all.files=FALSE, full.names=TRUE) #names of files for the selected week
          if(length(file_finder)==0){
            #if no data for week
            distrobyPIN<-setNames(data.frame(matrix(ncol = 10, nrow = 0)),
                                  c("WVProviderPIN","WorkOrder","InventoryType","Series","RequestedUnits",
                                    "AllottedUnits","DeliveredUnits","DistributionHub","PickUpDay", "RequestStatus"))
          }else{
            distrobyweek <- NULL #set placeholder for file; reset each time to catch any updates
            for (data in file_finder){ #for all information in the files for files identified in file_finder
              temphold <-read.csv(data, header=TRUE) #create a temp file
              distrobyweek <-unique(rbind(distrobyweek, temphold)) #add temp to request file and filter for unique entries
              rm(temphold) #remove temp
            }#end for
            ###filter for display and file
            #just the 6-digits of the PIN
            WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
            distrobyPIN <- distrobyweek%>%
              filter(WVProviderPIN==WVSIISUserWVProviderPIN)%>%
              select("WVProviderPIN","WorkOrder","InventoryType","Series","RequestedUnits","AllottedUnits","DeliveredUnits","DistributionHub","PickUpDay", "RequestStatus")
          }#end else
          ### DT output for request history
          output$distrohistory <- renderDT(datatable(distrobyPIN, rownames=FALSE))
          
          ### download csv for request history
          output$distrohistory_csv<- downloadHandler(
            filename = function() {
              #just the 6-digits of the PIN
              WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
              paste("Distribution_History_",WVSIISUserWVProviderPIN, ".csv", sep="")
            },
            content = function(file){
              write.csv(distrobyPIN, file,row.names=FALSE)
            }
          ) #end downloadHandler
        }#end if
      })#end observeEvent
      
      #-----End Distribution History-----
      
      #-----Start Change Order-----
      
      ###Current Operational Week
      #allows rescheduling and cancellations
      #data from hub and direct shipment distro reports
      
      ####find this operational week's distro files and combine them; note that direct ship requests do not show up in distro folder
      file_finder_change <- list.files(path = distropath, pattern=paste0("^.*(_Week_", currentoperationalweek,").*$"), 
                                       all.files=FALSE, full.names=TRUE) #names of files for the current operational week
      if(length(file_finder_change)==0){
        #if no data for week
        distroweek<-setNames(data.frame(matrix(ncol = 10, nrow = 0)),
                             c("WVProviderPIN","WorkOrder","RequestStatus","InventoryType","Series","RequestedUnits","AllottedUnits",
                               "DeliveredUnits","DistributionHub","PickUpDay"))
      }else{ 
        currentdistro <- NULL #set placeholder for file; reset each time to catch any updates
        for (data in file_finder_change){ #for all information in the files for files identified in file_finder
          temphold <-read.csv(data, header=TRUE) #create a temp file
          currentdistro <-rbind(currentdistro, temphold) #add temp to request file
          rm(temphold) #remove temp
        }#end for
        #### filter to PIN and select fields to show
        #just the 6-digits of the PIN
        WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
        distroweek <- currentdistro%>%
          filter(WVProviderPIN == WVSIISUserWVProviderPIN)%>% #for PIN
          filter(RequestStatus != "Canceled")%>% #keep uncanceled, skips over null request status
          filter(as.numeric(DeliveredUnits)==0 | is.na(DeliveredUnits)==TRUE)%>% #keep unfulfilled orders
          select("WVProviderPIN","WorkOrder","RequestStatus","InventoryType","Series","RequestedUnits","AllottedUnits",
                 "DeliveredUnits","DistributionHub","PickUpDay")
      }#end else
      
      ##create renderUI for table
      output$MainBody_currentweek<-renderUI({
        fluidPage(
          hr(),
          br(),
          h4("All changes are automatically saved. Select row first to cancel or reschedule."),
          ####in-line buttons for Edit, Cancel, Download
          actionButton(inputId = "currentweekcancel_row_head",label = "Cancel Request", class="cancelOrRemoveButton"),
          actionButton(inputId = "currentweekmod_row_head",label = "Reschedule", class="changeButton"),
          actionButton(inputId="currentweek_refresh", label="Refresh", icon = icon('refresh'), class="refreshButton"),
          downloadButton("canceled_rescheduled_csv", "Download CSV", class="submitbutton"),
          br(),
          br(),
          column(6,
                 dataTableOutput("currentweek_table"),
                 tags$script("$(document).on('click', '#currentweek_table button', function () {
                   Shiny.onInputChange('lastClickId',this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });")
          )#end column
        ) #end fluid page 
      })# end renderUI
      
      ##render the datatable for renderUI
      output$currentweek_table<-renderDataTable({
        ####find this operational week's distro files and combine them
        file_finder_change <- list.files(path = distropath, pattern=paste0("^.*(_Week_", currentoperationalweek,").*$"), 
                                         all.files=FALSE, full.names=TRUE) #names of files for the current operational week
        if(length(file_finder_change)==0){
          #if no data for week
          distroweek<-setNames(data.frame(matrix(ncol = 10, nrow = 0)),
                               c("WVProviderPIN","WorkOrder","RequestStatus","InventoryType","Series","RequestedUnits","AllottedUnits",
                                 "DeliveredUnits","DistributionHub","PickUpDay"))
        }else{ 
          currentdistro <- NULL #set placeholder for file; reset each time to catch any updates
          for (data in file_finder_change){ #for all information in the files for files identified in file_finder
            temphold <-read.csv(data, header=TRUE) #create a temp file
            currentdistro <-rbind(currentdistro, temphold) #add temp to request file
            rm(temphold) #remove temp
          }#end for
          #### filter to PIN and select fields to show
          #just the 6-digits of the PIN
          WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
          distroweek <- currentdistro%>%
            filter(WVProviderPIN == WVSIISUserWVProviderPIN)%>% #for PIN
            filter(RequestStatus != "Canceled")%>% #keep uncanceled, skips over null request status
            filter(as.numeric(DeliveredUnits)==0 | is.na(DeliveredUnits)==TRUE)%>% #keep unfulfilled orders
            select("WVProviderPIN","WorkOrder","RequestStatus","InventoryType","Series","RequestedUnits","AllottedUnits",
                   "DeliveredUnits","DistributionHub","PickUpDay")
        }#end else
        datatable(distroweek, selection = 'single', escape=F, rownames=FALSE, options = list(dom = 't',autoWidth = TRUE)) #allows for only selecting one row at a time
      }) #end renderDataTable 
      
      #refresh button
      observeEvent(input$currentweek_refresh,{
        #update data
        ####find this operational week's distro files and combine them
        file_finder_change <- list.files(path = distropath, pattern=paste0("^.*(_Week_", currentoperationalweek,").*$"), 
                                         all.files=FALSE, full.names=TRUE) #names of files for the current operational week
        if(length(file_finder_change)==0){
          #if no data for week
          distroweek<-setNames(data.frame(matrix(ncol = 10, nrow = 0)),
                               c("WVProviderPIN","WorkOrder","RequestStatus","InventoryType","Series","RequestedUnits","AllottedUnits",
                                 "DeliveredUnits","DistributionHub","PickUpDay"))
        }else{ 
          
          currentdistro <- NULL #set placeholder for file; reset each time to catch any updates
          for (data in file_finder_change){ #for all information in the files for files identified in file_finder
            temphold <-read.csv(data, header=TRUE) #create a temp file
            currentdistro <-rbind(currentdistro, temphold) #add temp to request file
            rm(temphold) #remove temp
          }#end for
          
          #### filter to PIN and select fields to show
          #just the 6-digits of the PIN
          WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
          distroweek <- currentdistro%>%
            filter(WVProviderPIN==WVSIISUserWVProviderPIN)%>% #for PIN
            filter(RequestStatus!="Canceled")%>% #keep uncanceled, skips over null request stauts
            filter(as.numeric(DeliveredUnits)==0 | is.na(DeliveredUnits)==TRUE)%>% #keep unfulfilled orders
            select("WVProviderPIN","WorkOrder","RequestStatus","InventoryType","Series","RequestedUnits","AllottedUnits",
                   "DeliveredUnits","DistributionHub","PickUpDay")
        }#end else
        #update table
        output$currentweek_table<-renderDataTable({
          datatable(distroweek, selection = 'single', escape=F, rownames=FALSE, options = list(dom = 't',autoWidth = TRUE)) #allows for only selecting one row at a time
        }) #end renderDataTable 
      })#end observeEvent
      
      ### cancel selected rows part
      ### this is warning message for canceling
      observeEvent(input$currentweekcancel_row_head,{
        #update data
        ####find this operational week's distro files and combine them
        file_finder_change <- list.files(path = distropath, pattern=paste0("^.*(_Week_", currentoperationalweek,").*$"), 
                                         all.files=FALSE, full.names=TRUE) #names of files for the current operational week
        if(length(file_finder_change)==0){
          #if no data for week
          distroweek<-setNames(data.frame(matrix(ncol = 10, nrow = 0)),
                               c("WVProviderPIN","WorkOrder","RequestStatus","InventoryType","Series","RequestedUnits","AllottedUnits",
                                 "DeliveredUnits","DistributionHub","PickUpDay"))
        }else{ 
          
          currentdistro <- NULL #set placeholder for file; reset each time to catch any updates
          for (data in file_finder_change){ #for all information in the files for files identified in file_finder
            temphold <-read.csv(data, header=TRUE) #create a temp file
            currentdistro <-rbind(currentdistro, temphold) #add temp to request file
            rm(temphold) #remove temp
          }#end for
          
          #### filter to PIN and select fields to show
          #just the 6-digits of the PIN
          WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
          distroweek <- currentdistro%>%
            filter(WVProviderPIN==WVSIISUserWVProviderPIN)%>% #for PIN
            filter(RequestStatus!="Canceled")%>% #keep uncanceled, skips over null request stauts
            filter(as.numeric(DeliveredUnits)==0 | is.na(DeliveredUnits)==TRUE)%>% #keep unfulfilled orders
            select("WVProviderPIN","WorkOrder","RequestStatus","InventoryType","Series","RequestedUnits","AllottedUnits",
                   "DeliveredUnits","DistributionHub","PickUpDay")
        }#end else
        showModal(
          if(length(input$currentweek_table_rows_selected)>=1 ){
            modalDialog(
              title = "Warning",
              paste("Are you sure you want to cancel your request for", 
                    distroweek$RequestedUnits[input$currentweek_table_rows_selected], "units of",
                    distroweek$InventoryType[input$currentweek_table_rows_selected],"?"),
              footer = tagList(
                modalButton("Changed My Mind"),
                actionButton("cancel_week", "Yes, Cancel Request")
              ), easyClose = TRUE)
          }else{
            modalDialog(
              title = "Warning",
              paste("Please select row that you want to cancel." ),easyClose = TRUE)#end modalDialog
          }#end else
        ) #end showModal
      }) #end observeEvent
      
      ### If user says "Yes, Cancel", then create cancel status for selected row
      observeEvent(input$cancel_week, {
        #current row in DT that is being cancelled
        DT_cancel_row <- input$currentweek_table_rows_selected
        #update data
        ####find this operational week's distro files and combine them
        file_finder_change <- list.files(path = distropath, pattern=paste0("^.*(_Week_", currentoperationalweek,").*$"), 
                                         all.files=FALSE, full.names=TRUE) #names of files for the current operational week
        if(length(file_finder_change)==0){
          #if no data for week
          distroweek<-setNames(data.frame(matrix(ncol = 10, nrow = 0)),
                               c("WVProviderPIN","WorkOrder","RequestStatus","InventoryType","Series","RequestedUnits","AllottedUnits",
                                 "DeliveredUnits","DistributionHub","PickUpDay"))
        }else{ 
          
          currentdistro <- NULL #set placeholder for file; reset each time to catch any updates
          for (data in file_finder_change){ #for all information in the files for files identified in file_finder
            temphold <-read.csv(data, header=TRUE) #create a temp file
            currentdistro <-rbind(currentdistro, temphold) #add temp to request file
            rm(temphold) #remove temp
          }#end for
          
          #### filter to PIN and select fields to show
          #just the 6-digits of the PIN
          WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
          distroweek <- currentdistro%>%
            filter(WVProviderPIN==WVSIISUserWVProviderPIN)%>% #for PIN
            filter(RequestStatus!="Canceled")%>% #keep uncanceled, skips over null request stauts
            filter(as.numeric(DeliveredUnits)==0 | is.na(DeliveredUnits)==TRUE)%>% #keep unfulfilled orders
            select("WVProviderPIN","WorkOrder","RequestStatus","InventoryType","Series","RequestedUnits","AllottedUnits",
                   "DeliveredUnits","DistributionHub","PickUpDay")
        }#end else
        #find where this row is in the Hub_0#_Week_## files by work order
        WO_splitter <- unlist(strsplit(distroweek$WorkOrder[DT_cancel_row],"-"))
        hubnumber<-WO_splitter[1]
        weeknumber<-WO_splitter[2]
        cancel_request <- fread(paste(distropath,"Hub_",hubnumber,"_Week_",weeknumber,".csv",sep=""))
        cancel_row <-which(cancel_request$WorkOrder==distroweek$WorkOrder[DT_cancel_row])
        #update fields for that row
        cancel_request$AllottedUnits[cancel_row] <-"0"
        cancel_request$AllottedQuantities[cancel_row] <-"0"
        #cancel_request$RequestedVials[cancel_row] <-"0"
        #cancel_request$RequestedDoses[cancel_row] <-"0"
        cancel_request$RequestStatus[cancel_row] <- "Canceled"
        cancel_request$PickUpDay[cancel_row] <-""
        
        #close dialog box  
        removeModal()#for message
        
        #update file
        fwrite(cancel_request, paste(distropath,"Hub_",hubnumber,"_Week_",weeknumber,".csv",sep=""))
        #refresh cancel_reschedule
        ####find this operational week's distro files and combine them
        file_finder_change <- list.files(path = distropath, pattern=paste0("^.*(_Week_", currentoperationalweek,").*$"), 
                                         all.files=FALSE, full.names=TRUE) #names of files for the current operational week
        
        currentdistro <- NULL #set placeholder for file; reset each time to catch any updates
        for (data in file_finder_change){ #for all information in the files for files identified in file_finder
          temphold <-read.csv(data, header=TRUE) #create a temp file
          currentdistro <-rbind(currentdistro, temphold) #add temp to request file
          rm(temphold) #remove temp
        }#end for
        
        #### filter to PIN and select fields to show
        #just the 6-digits of the PIN
        WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
        distroweek <- currentdistro%>%
          filter(WVProviderPIN==WVSIISUserWVProviderPIN)%>% #for PIN
          filter(RequestStatus!="Canceled")%>% #keep uncanceled, skips over null request stauts
          filter(as.numeric(DeliveredUnits)==0 | is.na(DeliveredUnits)==TRUE)%>% #keep unfulfilled orders
          select("WVProviderPIN","WorkOrder","RequestStatus","InventoryType","Series","RequestedUnits","AllottedUnits",
                 "DeliveredUnits","DistributionHub","PickUpDay")
        ##render the datatable for renderUI
        output$currentweek_table<-renderDataTable({
          datatable(distroweek, selection = 'single', escape=F, rownames=FALSE, options = list(dom = 't',autoWidth = TRUE)) #allows for only selecting one row at a time
        }) #end renderDataTable 
        #give confirmation dialog box  
        ####confirm change
        showModal(modalDialog(title="Your changes have been saved!", 
                              modalButton("OK"),
                              easyClose = TRUE, footer = NULL, fade=TRUE
        ) #end modalDialog
        )#end showModal
      })#end observeEvent
      
      #### Change schedule day
      ### reschedule button
      observeEvent(input$currentweekmod_row_head,{
        showModal(
          if(length(input$currentweek_table_rows_selected)>=1 ){
            modalDialog(
              fluidPage(
                h3(strong("Reschedule for This Week"),align="center"),
                hr(),
                uiOutput('currentweekrow_modif'),
                p(),
                textOutput("validation_reschedule"),
                p(),
                actionButton("save_changes1","Save changes", class="submitbutton"),
                tags$script("$(document).on('click', '#save_changes1 button', function () {
                   Shiny.onInputChange('lastClickId',this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });")
              )#end fluid page
            )#end modal dialog
          }else{
            modalDialog(
              title = "Warning",
              paste("Please select the row that you want to edit" ),easyClose = TRUE) #end modalDialog
          }#end else
        ) #end showModal
      }) #end observeEvent
      
      observeEvent(input$currentweekmod_row_head,{
        #don't allow updates if it is too late
        available_days <- switch(wday(Sys.Date()),
                                 #normal week
                                 "1" = c("choose day", "TUES", "WED", "THURS"), #Sunday
                                 "2" = c("choose day", "TUES", "WED", "THURS"), #Monday
                                 "3" = c("choose day", "WED", "THURS"), #Tuesday
                                 "4" = c("choose day", "THURS"), #Wednesday
                                 "5" = c("cannot reschedule"), #Thursday
                                 "6" = c("choose day", "TUES", "WED", "THURS"), #Friday
                                 "7" = c("choose day", "TUES", "WED", "THURS") #Saturday
                                 #week where Friday is allowed
                                 #"1" = c("choose day", "TUES", "WED", "THURS", "FRI"), #Sunday
                                 #"2" = c("choose day", "TUES", "WED", "THURS", "FRI"), #Monday
                                 #"3" = c("choose day", "WED", "THURS", "FRI"), #Tuesday
                                 #"4" = c("choose day", "THURS", "FRI"), #Wednesday
                                 #"5" = c("choose day", "FRI"), #Thursday
                                 #"6" = c("choose day", "TUES", "WED","THURS", "FRI"), #Friday
                                 #"7" = c("choose day", "TUES", "WED","THURS", "FRI") #Saturday
        )#end available_days
        output$currentweekrow_modif<-renderUI({
          selectInput("currentweekreschedule",
                      "Select new pick-up day",
                      choices=available_days)
        })#end renderUI
      })#end ObserveEvent
      
      validation_rescheduleday <- reactive({
        validate(
          #validate that Requested vials are a whole number
          need(input$currentweekreschedule != "cannot reschedule", 
               "You may cancel this order and place another one for a different week; however, it is too late to reschedule for this distribution week.")
        )#endvalidate
      })#end reactive
      
      ####validation messages in modal  
      output$validation_reschedule <- renderText(
        paste(validation_rescheduleday()$run))
      
      ### This is to replace the modified row to existing row
      observeEvent(input$save_changes1,{
        #validate it isn't a Thursday reschedule attempt
        validation_rescheduleday()
        #close dialog box  
        removeModal()#for message
        selected_row=input$currentweek_table_rows_selected
        #update data
        ####find this operational week's distro files and combine them
        file_finder_change <- list.files(path = distropath, pattern=paste0("^.*(_Week_", currentoperationalweek,").*$"), 
                                         all.files=FALSE, full.names=TRUE) #names of files for the current operational week
        if(length(file_finder_change)==0){
          #if no data for week
          distroweek<-setNames(data.frame(matrix(ncol = 10, nrow = 0)),
                               c("WVProviderPIN","WorkOrder","RequestStatus","InventoryType","Series","RequestedUnits","AllottedUnits",
                                 "DeliveredUnits","DistributionHub","PickUpDay"))
        }else{ 
          
          currentdistro <- NULL #set placeholder for file; reset each time to catch any updates
          for (data in file_finder_change){ #for all information in the files for files identified in file_finder
            temphold <-read.csv(data, header=TRUE) #create a temp file
            currentdistro <-rbind(currentdistro, temphold) #add temp to request file
            rm(temphold) #remove temp
          }#end for
          
          #### filter to PIN and select fields to show
          #just the 6-digits of the PIN
          WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
          distroweek <- currentdistro%>%
            filter(WVProviderPIN==WVSIISUserWVProviderPIN)%>% #for PIN
            filter(RequestStatus!="Canceled")%>% #keep uncanceled, skips over null request stauts
            filter(as.numeric(DeliveredUnits)==0 | is.na(DeliveredUnits)==TRUE)%>% #keep unfulfilled orders
            select("WVProviderPIN","WorkOrder","RequestStatus","InventoryType","Series","RequestedUnits","AllottedUnits",
                   "DeliveredUnits","DistributionHub","PickUpDay")
        }#end else
        
        #find where this row is in the Hub_0#_Week_## files by work order
        WO_splitter <- unlist(strsplit(distroweek$WorkOrder[selected_row],"-"))
        hubnumber<-WO_splitter[1]
        weeknumber<-WO_splitter[2]
        reschedule_request <- fread(paste(distropath,"Hub_",hubnumber,"_Week_",weeknumber,".csv",sep=""))
        old_row <-which(reschedule_request$WorkOrder==distroweek$WorkOrder[selected_row])
        reschedule_request$PickUpDay[old_row] <- input$currentweekreschedule
        fwrite(reschedule_request, paste(distropath,"Hub_",hubnumber,"_Week_",weeknumber,".csv",sep=""))
        #refresh cancel_reschedule
        ####find this operational week's distro files and combine them
        file_finder_change <- list.files(path = distropath, pattern=paste0("^.*(_Week_", currentoperationalweek,").*$"), 
                                         all.files=FALSE, full.names=TRUE) #names of files for the current operational week
        currentdistro <- NULL #set placeholder for file; reset each time to catch any updates
        for (data in file_finder_change){ #for all information in the files for files identified in file_finder
          temphold <-read.csv(data, header=TRUE) #create a temp file
          currentdistro <-rbind(currentdistro, temphold) #add temp to request file
          rm(temphold) #remove temp
        }#end for
        #### filter to PIN and select fields to show
        #just the 6-digits of the PIN
        WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
        distroweek <- currentdistro%>%
          filter(WVProviderPIN==WVSIISUserWVProviderPIN)%>% #for PIN
          filter(RequestStatus!="Canceled")%>% #keep uncanceled, skips over null request stauts
          filter(as.numeric(DeliveredUnits)==0 | is.na(DeliveredUnits)==TRUE)%>% #keep unfulfilled orders
          select("WVProviderPIN","WorkOrder","RequestStatus","InventoryType","Series","RequestedUnits","AllottedUnits",
                 "DeliveredUnits","DistributionHub","PickUpDay")
        ##render the datatable for renderUI
        output$currentweek_table<-renderDataTable({
          datatable(distroweek, selection = 'single', escape=F, rownames=FALSE, options = list(dom = 't',autoWidth = TRUE)) #allows for only selecting one row at a time
        }) #end renderDataTable 
        #give confirmation dialog box  
        ####confirm change
        showModal(modalDialog(title="Your changes have been saved!", 
                              modalButton("OK"),
                              easyClose = TRUE, footer = NULL, fade=TRUE
        ) #end modalDialog
        )#end showModal
      })#end observeEvent
      
      ### download the table in csv
      output$canceled_rescheduled_csv<- downloadHandler(
        filename = function() {
          #just the 6-digits of the PIN
          WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
          paste("Current_Week_Requests_", WVSIISUserWVProviderPIN,"_",Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          #update data
          ####find this operational week's distro files and combine them
          file_finder_change <- list.files(path = distropath, pattern=paste0("^.*(_Week_", currentoperationalweek,").*$"), 
                                           all.files=FALSE, full.names=TRUE) #names of files for the current operational week
          if(length(file_finder_change)==0){
            #if no data for week
            distroweek<-setNames(data.frame(matrix(ncol = 10, nrow = 0)),
                                 c("WVProviderPIN","WorkOrder","RequestStatus","InventoryType","Series","RequestedUnits","AllottedUnits",
                                   "DeliveredUnits","DistributionHub","PickUpDay"))
          }else{ 
            
            currentdistro <- NULL #set placeholder for file; reset each time to catch any updates
            for (data in file_finder_change){ #for all information in the files for files identified in file_finder
              temphold <-read.csv(data, header=TRUE) #create a temp file
              currentdistro <-rbind(currentdistro, temphold) #add temp to request file
              rm(temphold) #remove temp
            }#end for
            
            #### filter to PIN and select fields to show
            #just the 6-digits of the PIN
            WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
            distroweek <- currentdistro%>%
              filter(WVProviderPIN==WVSIISUserWVProviderPIN)%>% #for PIN
              filter(RequestStatus!="Canceled")%>% #keep uncanceled, skips over null request stauts
              filter(as.numeric(DeliveredUnits)==0 | is.na(DeliveredUnits)==TRUE)%>% #keep unfulfilled orders
              select("WVProviderPIN","WorkOrder","RequestStatus","InventoryType","Series","RequestedUnits","AllottedUnits",
                     "DeliveredUnits","DistributionHub","PickUpDay")
          }#end else
          write.csv(distroweek, file, row.names = F)
        }
      )     
      
      ###FUTURE REQUESTS
      #allows all options one can find in the New Request Tab for all weeks besides current planning week
      #from request report for PIN
      
      #just the 6-digits of the PIN
      WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
      ####read in request data file 
      allPIN <-fread(paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
      #add index
      allPIN_indexed <-cbind(allPIN,data.frame(as.numeric(rownames(allPIN))))
      names(allPIN_indexed)[21] <- "rn" #first  empty column
      
      #filter for future weeks and select fields you want to show
      filtered_indexed <- allPIN_indexed %>%
        filter(as.numeric(OperationalWeek)>currentoperationalweek)%>% #find all weeks past this week
        filter(RequestStatus!="Canceled")%>%#remove canceled
        mutate("DistributionWeek" = (distribution_week_list[match(OperationalWeek, week_number_list)]))
      
      #select fields you want to show
      filtered_display <- filtered_indexed %>% #add distribution week column
        #select("RequestStatus", "DistributionWeek", "WVProviderPIN","TargetPopulation","InventoryBrand","Series","RequestedVials","DeliveryMethod",
        #"DistributionHub","PickUpDay", "ShippingAddress","NotesToApprover","NotesFromApprover")
        select("RequestStatus", "DistributionWeek", "WVProviderPIN","TargetPopulation","InventoryType","Series","RequestedUnits","DeliveryMethod",
               "DistributionHub","PickUpDay","NotesToApprover")
      
      ##create renderUI for table
      output$MainBody_futureweeks<-renderUI({
        fluidPage(
          hr(),
          #conditional panel for blackout period message
          br(),
          h4("All changes are automatically saved. Select row first to cancel or edit."),
          ####in-line buttons for Edit, Cancel, Download
          actionButton(inputId = "futureweekscancel_row_head",label = "Cancel Request", class="cancelOrRemoveButton"),
          actionButton(inputId = "futureweeksmod_row_head",label = "Change Request", class="changeButton"),
          actionButton(inputId="futureweek_refresh", label="Refresh", icon = icon('refresh'), class="refreshButton"),
          downloadButton("futureweeks_csv", "Download CSV", class="submitbutton"),
          br(),
          br(),
          column(6,
                 dataTableOutput("futureweeks_table"),
                 tags$script("$(document).on('click', '#futureweeks_table button', function () {
                   Shiny.onInputChange('lastClickId',this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });")
          )#end column
        ) #end fluid page 
      })# end renderUI
      
      ##render the datatable for renderUI
      output$futureweeks_table<-renderDataTable({
        datatable(filtered_display, selection = 'single', escape=F, rownames=FALSE, options = list(dom = 't',autoWidth = TRUE, pageLength = 100)) #allows for only selecting one row at a time
      }) #end renderDataTable 
      
      #refresh button
      observeEvent(input$futureweek_refresh,{
        #just the 6-digits of the PIN
        WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
        #update data
        ####read in request data file 
        allPIN <-fread(paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
        
        #add index
        allPIN_indexed <-cbind(allPIN,data.frame(as.numeric(rownames(allPIN))))
        names(allPIN_indexed)[21] <- "rn" #first  empty column
        
        #filter for future weeks and select fields you want to show
        filtered_indexed <- allPIN_indexed %>%
          filter(as.numeric(OperationalWeek)>currentoperationalweek)%>% #find all weeks past this week
          filter(RequestStatus!="Canceled")%>%#remove canceled
          mutate("DistributionWeek" = (distribution_week_list[match(OperationalWeek, week_number_list)]))
        
        #select fields you want to show
        filtered_display <- filtered_indexed %>% #add distribution week column
          #select("RequestStatus", "DistributionWeek", "WVProviderPIN","TargetPopulation","VaccineBrand","Series","RequestedVials","DeliveryMethod",
          #"DistributionHub","PickUpDay", "ShippingAddress","NotesToApprover","NotesFromApprover")
          select("RequestStatus", "DistributionWeek", "WVProviderPIN","TargetPopulation","InventoryType","Series","RequestedUnits","DeliveryMethod",
                 "DistributionHub","PickUpDay","NotesToApprover")
        #update table
        output$futureweeks_table<-renderDataTable({
          datatable(filtered_display, selection = 'single', escape=F, rownames=FALSE, options = list(dom = 't',autoWidth = TRUE, pageLength = 100)) #allows for only selecting one row at a time
        }) #end renderDataTable 
      })
      
      
      ### cancel selected rows part
      ### this is warning message for canceling
      observeEvent(input$futureweekscancel_row_head,{
        #just the 6-digits of the PIN
        WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
        #update data
        ####read in request data file 
        allPIN <-fread(paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
        
        #add index
        allPIN_indexed <-cbind(allPIN,data.frame(as.numeric(rownames(allPIN))))
        names(allPIN_indexed)[21] <- "rn" #first  empty column
        
        #filter for future weeks and select fields you want to show
        filtered_indexed <- allPIN_indexed %>%
          filter(as.numeric(OperationalWeek)>currentoperationalweek)%>% #find all weeks past this week
          filter(RequestStatus!="Canceled")%>%#remove canceled
          mutate("DistributionWeek" = (distribution_week_list[match(OperationalWeek, week_number_list)]))
        
        #select fields you want to show
        filtered_display <- filtered_indexed %>% #add distribution week column
          #select("RequestStatus", "DistributionWeek", "WVProviderPIN","TargetPopulation","VaccineBrand","Series","RequestedVials","DeliveryMethod",
          #"DistributionHub","PickUpDay", "ShippingAddress","NotesToApprover","NotesFromApprover")
          select("RequestStatus", "DistributionWeek", "WVProviderPIN","TargetPopulation","InventoryType","Series","RequestedUnits","DeliveryMethod",
                 "DistributionHub","PickUpDay","NotesToApprover")
        
        showModal(
          #if row selected
          if(length(input$futureweeks_table_rows_selected)>=1 ){
            #if selected row is for the upcoming week AND it is Wednesday or Thursday
            if(blackout_trigger==TRUE && 
               match(filtered_display$DistributionWeek[input$futureweeks_table_rows_selected],distribution_week_list)==(currentoperationalweek+1)){
              modalDialog(
                title = "Blackout Period for Upcoming Week Changes",
                #paste("Hub pick-up requests for the upcoming distribution week cannot be edited or canceled on the Wednesday and Thursday prior
                #as we are putting our distribution plan together. Call the Help Desk if you need to implement changes that cannot
                #wait until Friday. Note that changes to requests for direct shipment must be resolved by end-of-business of the 
                #Wednesday prior to prevent shipment." )
                paste("Hub pick-up requests for the upcoming distribution week cannot be edited or canceled on the Wednesday and Thursday prior
                      as we are putting our distribution plan together. Call the Help Desk if you need to implement changes that cannot
                      wait until Friday." )
                ,easyClose = TRUE)#end modalDialog
            }else{
              modalDialog(
                title = "Cancel Request",
                paste("Are you sure you want to cancel your request for", 
                      filtered_display$RequestedUnits[input$futureweeks_table_rows_selected], "units of",
                      filtered_display$InventoryType[input$futureweeks_table_rows_selected],"?"),
                footer = tagList(
                  modalButton("Changed My Mind"),
                  actionButton("cancel_futureweeks", "Yes, Cancel Request")
                ), easyClose = TRUE)
            } #end if/else
          }else{
            modalDialog(
              title = "Warning",
              paste("Please select row that you want to cancel." ),easyClose = TRUE)#end modalDialog
          }#end if/else
        ) #end showModal
      }) #end observeEvent
      
      ## If user says "Yes, Cancel", then create cancel status for selected row
      observeEvent(input$cancel_futureweeks, {
        #just the 6-digits of the PIN
        WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
        #close dialog box  
        removeModal()#for message
        #update data
        ####read in request data file 
        allPIN <-fread(paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
        
        #add index
        allPIN_indexed <-cbind(allPIN,data.frame(as.numeric(rownames(allPIN))))
        names(allPIN_indexed)[21] <- "rn" #first  empty column
        
        #filter for future weeks and select fields you want to show
        filtered_indexed <- allPIN_indexed %>%
          filter(as.numeric(OperationalWeek)>currentoperationalweek)%>% #find all weeks past this week
          filter(RequestStatus!="Canceled")%>%#remove canceled
          mutate("DistributionWeek" = (distribution_week_list[match(OperationalWeek, week_number_list)]))
        
        #old row being canceled
        cancel_row <- as.data.frame(filtered_indexed$rn)[as.numeric(unlist(input$futureweeks_table_rows_selected)),1]
        #update fields for that row
        allPIN$RequestedUnits[cancel_row] <-"0"
        allPIN$TargetPopulation[cancel_row] <-"Canceled"
        allPIN$RequestStatus[cancel_row] <- "Canceled"
        allPIN$PickUpDay[cancel_row] <-""
        allPIN$ShippingAddress[cancel_row] <-""
        #update file
        fwrite(allPIN, paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
        #refresh data
        ####read in request data file 
        allPIN <-fread(paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
        
        #add index
        allPIN_indexed <-cbind(allPIN,data.frame(as.numeric(rownames(allPIN))))
        names(allPIN_indexed)[21] <- "rn" #first  empty column
        
        #filter for future weeks and select fields you want to show
        filtered_indexed <- allPIN_indexed %>%
          filter(as.numeric(OperationalWeek)>currentoperationalweek)%>% #find all weeks past this week
          filter(RequestStatus!="Canceled")%>%#remove canceled
          mutate("DistributionWeek" = (distribution_week_list[match(OperationalWeek, week_number_list)]))
        
        #select fields you want to show
        filtered_display <- filtered_indexed %>% #add distribution week column
          #select("RequestStatus", "DistributionWeek", "WVProviderPIN","TargetPopulation","VaccineBrand","Series","RequestedVials","DeliveryMethod",
          #"DistributionHub","PickUpDay", "ShippingAddress","NotesToApprover","NotesFromApprover")
          select("RequestStatus", "DistributionWeek", "WVProviderPIN","TargetPopulation","InventoryType","Series","RequestedUnits","DeliveryMethod",
                 "DistributionHub","PickUpDay","NotesToApprover")
        
        #refresh table
        output$futureweeks_table<-renderDataTable({
          datatable(filtered_display, selection = 'single', escape=F, rownames=FALSE, options = list(dom = 't',autoWidth = TRUE, pageLength = 100)) #allows for only selecting one row at a time
        })#end render data table
        #give confirmation dialog box  
        ####confirm change
        showModal(modalDialog(title="Your changes have been saved!", 
                              modalButton("OK"),
                              easyClose = TRUE, footer = NULL, fade=TRUE
        ) #end modalDialog
        )#end showModal
      })#end observe event
      
      ###allow changes to the records
      observeEvent(input$futureweeksmod_row_head,{
        #just the 6-digits of the PIN
        WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
        #update data
        ####read in request data file 
        allPIN <-fread(paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
        
        #add index
        allPIN_indexed <-cbind(allPIN,data.frame(as.numeric(rownames(allPIN))))
        names(allPIN_indexed)[21] <- "rn" #first  empty column
        
        #filter for future weeks and select fields you want to show
        filtered_indexed <- allPIN_indexed %>%
          filter(as.numeric(OperationalWeek)>currentoperationalweek)%>% #find all weeks past this week
          filter(RequestStatus!="Canceled")%>%#remove canceled
          mutate("DistributionWeek" = (distribution_week_list[match(OperationalWeek, week_number_list)]))
        
        #select fields you want to show
        filtered_display <- filtered_indexed %>% #add distribution week column
          #select("RequestStatus", "DistributionWeek", "WVProviderPIN","TargetPopulation","VaccineBrand","Series","RequestedVials","DeliveryMethod",
          #"DistributionHub","PickUpDay", "ShippingAddress","NotesToApprover","NotesFromApprover")
          select("RequestStatus", "DistributionWeek", "WVProviderPIN","TargetPopulation","InventoryType","Series","RequestedUnits","DeliveryMethod",
                 "DistributionHub","PickUpDay","NotesToApprover")
        
        old_row=allPIN[filtered_indexed$rn[input$futureweeks_table_rows_selected]]
        #if(old_row$DeliveryMethod=="Direct Shipment"){
        #split out prime and boost values
        #dsprimevials <-strsplit(old_row$NotesFromApprover," ")[[1]][3] #third string element
        #dsboostervials <-strsplit(old_row$NotesFromApprover," ")[[1]][7] #seventh string element
        #}#end if
        showModal(
          if(length(input$futureweeks_table_rows_selected)>=1 ){
            #if selected row is for the upcoming week AND it is Wednesday or Thursday
            if(blackout_trigger==TRUE && 
               match(filtered_display$DistributionWeek[input$futureweeks_table_rows_selected],distribution_week_list)==(currentoperationalweek+1)){
              modalDialog(
                title = "Blackout Period for Upcoming Week Changes",
                #paste("Hub pick-up requests for the upcoming distribution week cannot be edited or canceled on the Wednesday and Thursday prior
                #as we are putting our distribution plan together. Call the Help Desk if you need to implement changes that cannot
                #wait until Friday. Note that changes to requests for direct shipment must be resolved by end-of-business of the 
                #Wednesday prior." )
                paste("Hub pick-up requests for the upcoming distribution week cannot be edited or canceled on the Wednesday and Thursday prior
                      as we are putting our distribution plan together. Call the Help Desk if you need to implement changes that cannot
                      wait until Friday." )
                ,easyClose = TRUE)#end modalDialog
            } else #for changing a hub-pickup row
              #if(old_row$DeliveryMethod=="Hub Pick-Up"){
              modalDialog(
                fluidPage(
                  h3(strong("Modification of Record"),align="center"),
                  hr(),
                  fluidRow(
                    column(6, align="center",
                           selectInput(paste0("inventory2",input$futureweeksmod_row_head),
                                       label = labelMandatory("Inventory Type"),
                                       choices = c("choose inventory type", inventory),
                                       selected=old_row$InventoryType)
                    ),#end column
                    column(6,
                           strong("Select the type of inventory you would like to request. Note that you can only select one type per request.") 
                    )#end column
                  ),# end fluidRow
                  fluidRow(
                    column(6, align="center",
                           selectInput(paste0("series2",input$futureweeksmod_row_head),
                                       label = labelMandatory("Series"),
                                       choices = c("choose series", "Prime","Boost"),
                                       selected=old_row$Series)
                    ),#end column
                    column(6,
                           strong("Select whether these units are estimated to be used for the primary (Prime) or booster (Boost) 
                            vaccination just for this request. Inv3 is only available as Prime.") 
                    )#end column
                  ),# end fluidRow
                  fluidRow(
                    column(6, align="center",
                           numericInput(paste0("RequestedUnits2",input$futureweeksmod_row_head),
                                        label=labelMandatory("Requested Units"), 
                                        min=0, step=1, value=old_row$RequestedUnits)
                    ),#end column
                    column(6,
                           strong("Enter the total number of units being requested.")
                    )#end column
                  ),# end fluidRow
                  fluidRow(
                    column(6, align="center",
                           selectInput(paste0("distrohubs2",input$futureweeksmod_row_head),
                                       label = labelMandatory("Distribution Hub"),
                                       choices = c("choose pick-up hub", distrohubs),
                                       selected=old_row$DistributionHub)
                    ),#end column
                    column(6,
                           strong("Select the distribution hub where you would like to pick up your order.")
                    )#end column
                  ), #end fluidRow
                  fluidRow(
                    column(6, align="center",
                           
                           selectInput(paste0("pickupday2",input$futureweeksmod_row_head),
                                       label = labelMandatory("Pick-Up Day"),
                                       choices = c(pickupdays),
                                       selected=old_row$PickUpDay)
                    ),#end column
                    column(6,
                           strong("Select the day of pick up for this request."),
                           br(),
                    )#end column
                  ),# end fluidRow
                  hr(),
                  fluidRow(
                    column(6, align="center",
                           textInput(paste0("TargetPopulation2",input$futureweeksmod_row_head), 
                                     label=labelMandatory("Target Population"), 
                                     value=old_row$TargetPopulation)
                    ),#end column
                    column(6,
                           strong("What population(s) do you expect to receive these inventory?")
                    )#end column
                  ),#end fluid row
                  fluidRow(
                    column(6, align="center",
                           textInput(paste0("Notes2",input$futureweeksmod_row_head), label="Notes", value=paste(old_row$NotesToApprover))
                    ),#end column
                    column(6,
                           strong("Do you have any notes or questions for the approver?")
                    )#end column
                  ),# end fluidRow
                  hr(),
                  fluidRow(
                    column(6, align="center",
                           textOutput("validation_change_messages"), 
                           p(),
                           textOutput("validation_inv3_message")
                    )#end column
                  ),#end fluid row
                  actionButton("save_changes2","Save changes", class="submitbutton"),
                  #tags$script("$(document).on('click', '#save_changes2 button', function () {
                  #Shiny.onInputChange('lastClickId',this.id);
                  #Shiny.onInputChange('lastClick', Math.random()) });")
                )#end fluid page
              )#end modal dialog
            #} else { #for changing a direct shipment row
            #modalDialog(
            #fluidPage(
            #h3(strong("Changing a Direct Shipment Request"),align="center"),
            # hr(),
            #paste("Direct shipment requests can be changed by increasing/decreasing the request by a shippable quantity or by
            #changing the request to a hub pick-up. Once a request is changed from direct shipment to hub pick-up, it cannot be
            #changed back to a direct shipment request. A new request would have to be created."),
            #p(),
            #paste("What would you like to do?"),
            #p(),
            #selectInput(paste0("directshipchoice",input$futureweeksmod_row_head),
            #label = "Choose Request Change",
            #choices = c("choose change action","Change Direct Shipment Quantity","Change Request to a Hub Pick-Up")),
            #p(),
            #actionButton("direct_ship_edit","Edit Direct Shipment Request", class="submitbutton")
            #)#end fluid page
            #)#end modal dialog
            #} #end if/else if/else for pop up based on row selected row week or delivery method
          }else{
            modalDialog(
              title = "Warning",
              paste("Please select the row that you want to edit" ),easyClose = TRUE) #end modalDialog
          }#end else
        ) #end showModal
      }) #end observeEvent
      
      ###second direct ship modal
      #observeEvent(input$direct_ship_edit, {
      #just the 6-digits of the PIN
      #WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
      #update data
      ####read in request data file 
      #allPIN <-fread(paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
      
      #add index
      #allPIN_indexed <-cbind(allPIN,data.frame(as.numeric(rownames(allPIN))))
      #names(allPIN_indexed)[21] <- "rn" #first  empty column
      
      #filter for future weeks and select fields you want to show
      #filtered_indexed <- allPIN_indexed %>%
      #filter(as.numeric(OperationalWeek)>currentoperationalweek)%>% #find all weeks past this week
      #filter(RequestStatus!="Canceled")%>%#remove canceled
      #mutate("DistributionWeek" = (distribution_week_list[match(OperationalWeek, week_number_list)]))
      
      #select fields you want to show
      #filtered_display <- filtered_indexed %>% #add distribution week column
      #select("RequestStatus", "DistributionWeek", "WVProviderPIN","TargetPopulation","VaccineBrand","Series","RequestedVials", "DeliveryMethod",
      #"DistributionHub","PickUpDay", "ShippingAddress","NotesToApprover","NotesFromApprover")
      
      #old_row=allPIN[filtered_indexed$rn[input$futureweeks_table_rows_selected]]
      #if(old_row$DeliveryMethod=="Direct Shipment"){
      #split out prime and boost values
      #dsprimevials <-strsplit(old_row$NotesFromApprover," ")[[1]][3] #third string element
      #dsboostervials <-strsplit(old_row$NotesFromApprover," ")[[1]][7] #seventh string element
      #}#end if
      
      #showModal(
      
      #modal for changing direct shipment quantity
      #if(input[[paste0("directshipchoice",input$futureweeksmod_row_head)]]=="Change Direct Shipment Quantity"){
      #modalDialog(
      #fluidPage(
      #hr(),
      #h3(strong("Modification of Direct Shipment Record"),align="center"),
      #hr(),
      #fluidRow(
      #column(6, align="center",
      #selectInput(paste0("vaccines4",input$futureweeksmod_row_head),
      #label = labelMandatory("Vaccine Brand"),
      #choices = c("choose vaccine brand", "Moderna"), #this will need updated after other brands can ship
      #selected=old_row$VaccineBrand)
      #),#end column
      #column(6,
      #strong("Select the brand of vaccine you would like to request. Note that you can only select one brand per request.") 
      #)#end column
      #),# end fluidRow
      #fluidRow(
      #column(6, align="center",
      #numericInput(paste0("PrimeVials4",input$futureweeksmod_row_head),
      #label=labelMandatory("Prime Vials"), 
      #min=0, step=1, value=as.numeric(dsprimevials))
      #),#end column
      #column(6,
      #strong("Enter the total number of prime vials being requested.")
      #)#end column
      #),# end fluidRow
      #fluidRow(
      #column(6, align="center",
      #numericInput(paste0("BoosterVials4",input$futureweeksmod_row_head),
      #label=labelMandatory("Booster Vials"), 
      #min=0, step=1, value=as.numeric(dsboostervials))
      #),#end column
      #column(6,
      #strong("Enter the total number of booster vials being requested.")
      #)#end column
      #),# end fluidRow
      #fluidRow(
      #column(6, align="center",
      #textInput(paste0("TargetPopulation4",input$futureweeksmod_row_head), 
      #label=labelMandatory("Target Population"), 
      #value=old_row$TargetPopulation)
      #),#end column
      #column(6,
      #strong("What population(s) do you expect to receive these vaccines?")
      #)#end column
      #),#end fluid row
      #fluidRow(
      #column(6, align="center",
      #textInput(paste0("Notes4",input$futureweeksmod_row_head), label="Notes", value=paste(old_row$NotesToApprover))
      #),#end column
      #column(6,
      #strong("Do you have any notes or questions for the approver?")
      #)#end column
      #),# end fluidRow
      #hr(),
      #fluidRow(
      #column(6, align="center",
      #textOutput("validation_change4_messages"), 
      #)#end column
      #),#end fluid row
      #actionButton("save_changes4","Save changes", class="submitbutton")
      #)#end fluid page
      #)#end modalDialog
      #} else
      
      #modal for changing direct shipment to hub pick up
      #if(input[[paste0("directshipchoice",input$futureweeksmod_row_head)]]=="Change Request to a Hub Pick-Up"){
      #modalDialog(
      #fluidPage(
      #hr(),           
      #h3(strong("Modification of Record - Direct Shipment to Hub Pick-Up"),align="center"),
      #hr(),
      #fluidRow(
      #column(6, align="center",
      #selectInput(paste0("vaccines3",input$futureweeksmod_row_head),
      #label = labelMandatory("Vaccine Brand"),
      #choices = c("choose vaccine brand", vaccines),
      #selected=old_row$VaccineBrand)
      #),#end column
      #column(6,
      #strong("Select the brand of vaccine you would like to request. Note that you can only select one brand per request.") 
      #)#end column
      #),# end fluidRow
      #fluidRow(
      #column(6, align="center",
      #numericInput(paste0("PrimeVials3",input$futureweeksmod_row_head),
      #label=labelMandatory("Prime Vials"), 
      # min=0, step=1, value=as.numeric(dsprimevials))
      #),#end column
      #column(6,
      #strong("Enter the total number of prime vials being requested.")
      #)#end column
      #),# end fluidRow
      #fluidRow(
      #column(6, align="center",
      #numericInput(paste0("BoosterVials3",input$futureweeksmod_row_head),
      #label=labelMandatory("Booster Vials"), 
      #min=0, step=1, value=as.numeric(dsboostervials))
      #),#end column
      #column(6,
      #strong("Enter the total number of booster vials being requested.")
      #)#end column
      #),# end fluidRow
      #fluidRow(
      #column(6, align="center",
      #selectInput(paste0("distrohubs3",input$futureweeksmod_row_head),
      #label = labelMandatory("Distribution Hub"),
      #choices = c("choose pick-up hub", distrohubs))
      #),#end column
      #column(6,
      #strong("Select the distribution hub where you would like to pick up your order.")
      #)#end column
      #), #end fluidRow
      #fluidRow(
      #column(6, align="center",
      
      #selectInput(paste0("pickupday3",input$futureweeksmod_row_head),
      #label = labelMandatory("Pick-Up Day"),
      #choices = c(pickupdays),
      #selected=old_row$PickUpDay)
      #),#end column
      #column(6,
      #strong("Select the day of pick up for this request."),
      #br(),
      #)#end column
      #),# end fluidRow
      #fluidRow(
      #column(6, align="center",
      #textInput(paste0("TargetPopulation3",input$futureweeksmod_row_head), 
      #label=labelMandatory("Target Population"), 
      #value=old_row$TargetPopulation)
      #),#end column
      #column(6,
      #strong("What population(s) do you expect to receive these vaccines?")
      #)#end column
      #),#end fluid row
      #fluidRow(
      #column(6, align="center",
      #textInput(paste0("Notes3",input$futureweeksmod_row_head), label="Notes", value=paste(old_row$NotesToApprover))
      #),#end column
      #column(6,
      #strong("Do you have any notes or questions for the approver?")
      #)#end column
      #),# end fluidRow
      #hr(),
      #fluidRow(
      #column(6, align="center",
      #textOutput("validation_change3_messages"), 
      #p(),
      #textOutput("validation_janssen3_message")
      #)#end column
      #),#end fluid row
      #actionButton("save_changes3","Save changes", class="submitbutton")
      #)#end fluid page
      #)#end modalDialog
      #}#end if
      
      #)#end showModal
      #}) #end observeEvent
      
      ###validation checks for submit click for hub pick up
      validation_future <- reactive({
        validate(
          #validate that Requested vials are a whole number
          need(input[[paste0("RequestedUnits2",input$futureweeksmod_row_head)]]%%1==0,
               "Only whole units are to be requested."),
          #validate that vials are >0
          need(input[[paste0("RequestedUnits2",input$futureweeksmod_row_head)]]>0,
               "Zero units indicates you want to cancel. Please click 'Dismiss' and use the 'Cancel Request' button."),
          #need all necessary fields filled out
          need(input[[paste0("inventory2",input$futureweeksmod_row_head)]]!="choose inventory type" &&
                 input[[paste0("series2",input$futureweeksmod_row_head)]]!= "choose series" &&
                 input[[paste0("RequestedUnits2",input$futureweeksmod_row_head)]]!=0 &&
                 input[[paste0("distrohubs2",input$futureweeksmod_row_head)]]!= "choose pick-up hub" &&
                 input[[paste0("pickupday2",input$futureweeksmod_row_head)]]!= "choose pick-up day" &&
                 input[[paste0("TargetPopulation2",input$futureweeksmod_row_head)]]!= "",
               "A mandatory field is missing. Please fill it out and try again.")
        )#end validate
      })#end validation
      
      ####validation messages in modal for to hub pick up
      output$validation_change_messages <- renderText(
        paste(validation_future()$run))
      
      ####janssen validation check before submit click for hub pick up
      validation_inv3 <- reactive({
        if(input[[paste0("inventory2",input$futureweeksmod_row_head)]]=="Inv3" &&
           input[[paste0("series2",input$futureweeksmod_row_head)]]>="Boost"){
          validate(need(input[[paste0("inventory2",input$futureweeksmod_row_head)]]=="Inv3" &&
                          input[[paste0("series2",input$futureweeksmod_row_head)]]=="Prime",
                        "Inv3 can only be requested as Prime series."))
        }
      })#end reactive
      
      ####validation messages in modal for janssen hub pick up
      output$validation_inv3_message <- renderText(
        paste(validation_inv3()$run))
      
      ## If user says Submit Changes then modify record
      observeEvent(input$save_changes2, {
        #validation
        validation_future()
        validation_inv3()
        
        #close dialog box  
        removeModal()#for message
        #just the 6-digits of the PIN
        WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
        #update data
        ####read in request data file 
        allPIN <-fread(paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
        
        #add index
        allPIN_indexed <-cbind(allPIN,data.frame(as.numeric(rownames(allPIN))))
        names(allPIN_indexed)[21] <- "rn" #first  empty column
        
        #filter for future weeks and select fields you want to show
        filtered_indexed <- allPIN_indexed %>%
          filter(as.numeric(OperationalWeek)>currentoperationalweek)%>% #find all weeks past this week
          filter(RequestStatus!="Canceled")%>%#remove canceled
          mutate("DistributionWeek" = (distribution_week_list[match(OperationalWeek, week_number_list)]))
        
        #old row being modified
        change_row <- filtered_indexed$rn[input$futureweeks_table_rows_selected]
        #update fields for that row
        allPIN$InventoryType[change_row] <- input[[paste0("inventory2",input$futureweeksmod_row_head)]]
        allPIN$Series[change_row] <-input[[paste0("series2",input$futureweeksmod_row_head)]]
        allPIN$RequestedUnits[change_row] <-input[[paste0("RequestedUnits2",input$futureweeksmod_row_head)]]
        allPIN$DistributionHub[change_row] <- input[[paste0("distrohubs2",input$futureweeksmod_row_head)]]
        allPIN$PickUpDay[change_row] <-input[[paste0("pickupday2",input$futureweeksmod_row_head)]]
        allPIN$TargetPopulation[change_row] <-input[[paste0("TargetPopulation2",input$futureweeksmod_row_head)]]
        allPIN$NotesToApprover[change_row] <-input[[paste0("Notes2",input$futureweeksmod_row_head)]]
        
        #just the 6-digits of the PIN
        WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
        #update file
        fwrite(allPIN, paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
        #refresh data
        ####read in request data file 
        allPIN <-fread(paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
        
        #add index
        allPIN_indexed <-cbind(allPIN,data.frame(as.numeric(rownames(allPIN))))
        names(allPIN_indexed)[21] <- "rn" #first  empty column
        
        #filter for future weeks and select fields you want to show
        filtered_indexed <- allPIN_indexed %>%
          filter(as.numeric(OperationalWeek)>currentoperationalweek)%>% #find all weeks past this week
          filter(RequestStatus!="Canceled")%>%#remove canceled
          mutate("DistributionWeek" = (distribution_week_list[match(OperationalWeek, week_number_list)]))
        
        #select fields you want to show
        filtered_display <- filtered_indexed %>% #add distribution week column
          #select("RequestStatus", "DistributionWeek", "WVProviderPIN","TargetPopulation","VaccineBrand","Series","RequestedVials","DeliveryMethod",
          #"DistributionHub","PickUpDay", "ShippingAddress","NotesToApprover","NotesFromApprover")
          select("RequestStatus", "DistributionWeek", "WVProviderPIN","TargetPopulation","InventoryType","Series","RequestedUnits","DeliveryMethod",
                 "DistributionHub","PickUpDay","NotesToApprover")
        #refresh table
        output$futureweeks_table<-renderDataTable({
          datatable(filtered_display, selection = 'single', escape=F, rownames=FALSE, options = list(dom = 't',autoWidth = TRUE, pageLength = 100)) #allows for only selecting one row at a time
        })#end render data table
        #give confirmation dialog box  
        ####confirm change
        showModal(modalDialog(title="Your changes have been saved!", 
                              modalButton("OK"),
                              easyClose = TRUE, footer = NULL, fade=TRUE
        ) #end modalDialog
        )#end showModal
      })#end observe event
      
      ###validation checks for submit click for direct shipment converted to hub pick up
      #validation_future3 <- reactive({
      #validate(
      #validate that prime vials are a whole number
      #need(input[[paste0("PrimeVials3",input$futureweeksmod_row_head)]]%%1==0,
      #"Only whole vials are to be requested."),
      #validate that booster vials are a whole number
      #need(input[[paste0("BoosterVials3",input$futureweeksmod_row_head)]]%%1==0,
      #"Only whole vials are to be requested."),
      #validate that total vials are >0
      #need((input[[paste0("PrimeVials3",input$futureweeksmod_row_head)]]+input[[paste0("BoosterVials3",input$futureweeksmod_row_head)]])>0,
      #"Zero vials indicates you want to cancel. Please click 'Dismiss' and use the 'Cancel Request' button."),
      #need all necessary fields filled out
      #need(input[[paste0("inventory3",input$futureweeksmod_row_head)]]!="choose inventory brand" &&
      #input[[paste0("distrohubs3",input$futureweeksmod_row_head)]]!= "choose pick-up hub" &&
      #input[[paste0("pickupday3",input$futureweeksmod_row_head)]]!= "choose pick-up day" &&
      #input[[paste0("TargetPopulation3",input$futureweeksmod_row_head)]]!= "",
      #"A mandatory field is missing. Please fill it out and try again.")
      #)#end validate
      #})#end validation
      
      ####validation messages in modal for direct ship converted to hub pick up
      # output$validation_change3_messages <- renderText(
      #paste(validation_future3()$run))
      
      ####janssen validation check before submit click for direct ship converted to hub pick up
      #validation_janssen3 <- reactive({
      #if(input[[paste0("vaccines3",input$futureweeksmod_row_head)]]=="Janssen" &&
      #input[[paste0("BoosterVials3",input$futureweeksmod_row_head)]]>0){
      #validate(need(input[[paste0("vaccines3",input$futureweeksmod_row_head)]]=="Janssen" &&
      #input[[paste0("BoosterVials3",input$futureweeksmod_row_head)]]==0,
      #"Janssen can only be requested as Prime series. Please set Boost to 0."))
      #}
      #})#end reactive
      
      ####validation messages in modal for janssen direct ship converted to hub pick up
      #output$validation_janssen3_message <- renderText(
      #paste(validation_janssen3()$run))
      
      ## If user says Submit Changes then modify record for direct ship converted to hub pick up
      #observeEvent(input$save_changes3, {
      #validation
      #validation_future3()
      #validation_janssen3()
      
      #close dialog box  
      #removeModal()#for message
      #just the 6-digits of the PIN
      #WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
      #update data
      ####read in request data file 
      #allPIN <-fread(paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
      
      #add index
      #allPIN_indexed <-cbind(allPIN,data.frame(as.numeric(rownames(allPIN))))
      #names(allPIN_indexed)[21] <- "rn" #first  empty column
      
      #filter for future weeks and select fields you want to show
      #filtered_indexed <- allPIN_indexed %>%
      #filter(as.numeric(OperationalWeek)>currentoperationalweek)%>% #find all weeks past this week
      #filter(RequestStatus!="Canceled")%>%#remove canceled
      #mutate("DistributionWeek" = (distribution_week_list[match(OperationalWeek, week_number_list)]))
      
      #old_row=allPIN[filtered_indexed$rn[input$futureweeks_table_rows_selected]]
      #if(old_row$DeliveryMethod=="Direct Shipment"){
      #split out prime and boost values
      #dsprimevials <-strsplit(old_row$NotesFromApprover," ")[[1]][3] #third string element
      #dsboostervials <-strsplit(old_row$NotesFromApprover," ")[[1]][7] #seventh string element
      #}#end if
      
      #old row being canceled
      #cancel_row <- as.data.frame(filtered_indexed$rn)[as.numeric(unlist(input$futureweeks_table_rows_selected)),1]
      #update fields for that row
      #allPIN$RequestedVials[cancel_row] <-"0"
      #allPIN$TargetPopulation[cancel_row] <-"Canceled"
      #allPIN$RequestStatus[cancel_row] <- "Canceled"
      #allPIN$PickUpDay[cancel_row] <-""
      #allPIN$ShippingAddress[cancel_row] <-""
      #update file
      #fwrite(allPIN, paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
      
      #add new row(s) for hub pick up
      ####add row if validations are fulfilled for hub pick up Prime Vials if applicable
      #if(as.numeric(input[[paste0("PrimeVials3",input$futureweeksmod_row_head)]])!=0){
      #new_row=data.frame(
      #"RequestStatus"="Pending",
      #"OperationalWeek"= old_row$OperationalWeek,
      #"WVProviderPIN"=WVSIISUserWVProviderPIN,
      #"EntityName"=RequestPrepopulation$Entity,
      #"EntityType"=RequestPrepopulation$EntityType,
      #"EntityGroup"=EntityGroup,
      #"County"=RequestPrepopulation$County,
      #"TargetPopulation"=input[[paste0("TargetPopulation3",input$futureweeksmod_row_head)]],
      #"VaccineBrand"=input[[paste0("vaccines3",input$futureweeksmod_row_head)]],
      #"Series"="Prime", #different from prime and boost record  updates
      #"RequestedVials"=input[[paste0("PrimeVials3",input$futureweeksmod_row_head)]],#different from prime and boost record  updates
      #"POCName"=RequestPrepopulation$POCName,
      # "POCPhone"=RequestPrepopulation$POCPhone,
      #"POCEmail"=RequestPrepopulation$POCEmail,
      #"NotesToApprover"=input[[paste0("Notes3",input$futureweeksmod_row_head)]],
      # "NotesFromApprover"="",
      # "DeliveryMethod"="Hub Pick-Up",
      #"DistributionHub"=input[[paste0("distrohubs3",input$futureweeksmod_row_head)]],
      # "ShippingAddress"="",
      # "PickUpDay"=input[[paste0("pickupday3",input$futureweeksmod_row_head)]]
      #)#end data.frame
      ####add new record to dataframe
      #new_row <- t(apply(new_row,2,as.character))
      #request_data<-rbind(request_data,new_row, fill=TRUE)
      ####write change to file
      #fwrite(new_row, paste("data/Requests/",as.character(WVSIISUserWVProviderPIN),".csv", sep=""), append=TRUE, row.names = FALSE) 
      #} #end primevials row add
      
      ####add row if validations are fulfilled for hub pick up Boost Vials if applicable
      #if(as.numeric(input[[paste0("BoosterVials3",input$futureweeksmod_row_head)]])!=0){
      #new_row=data.frame(
      #"RequestStatus"="Pending",
      #"OperationalWeek"= old_row$OperationalWeek,
      # "WVProviderPIN"=WVSIISUserWVProviderPIN,
      #"EntityName"=RequestPrepopulation$Entity,
      # "EntityType"=RequestPrepopulation$EntityType,
      # "EntityGroup"=EntityGroup,
      # "County"=RequestPrepopulation$County,
      # "TargetPopulation"=input[[paste0("TargetPopulation3",input$futureweeksmod_row_head)]],
      # "VaccineBrand"=input[[paste0("vaccines3",input$futureweeksmod_row_head)]],
      # "Series"="Boost", #different from prime and boost record  updates
      # "RequestedVials"=input[[paste0("BoosterVials3",input$futureweeksmod_row_head)]],#different from prime and boost record  updates
      # "POCName"=RequestPrepopulation$POCName,
      # "POCPhone"=RequestPrepopulation$POCPhone,
      # "POCEmail"=RequestPrepopulation$POCEmail,
      # "NotesToApprover"=input[[paste0("Notes3",input$futureweeksmod_row_head)]],
      # "NotesFromApprover"="",
      # "DeliveryMethod"="Hub Pick-Up",
      #  "DistributionHub"=input[[paste0("distrohubs3",input$futureweeksmod_row_head)]],
      #  "ShippingAddress"="",
      #  "PickUpDay"=input[[paste0("pickupday3",input$futureweeksmod_row_head)]]
      # )#end data.frame
      ####add new record to dataframe
      # new_row <- t(apply(new_row,2,as.character))
      #request_data<-rbind(request_data,new_row, fill=TRUE)
      ####write change to file
      #fwrite(new_row, paste("data/Requests/",as.character(WVSIISUserWVProviderPIN),".csv", sep=""), append=TRUE, row.names = FALSE) 
      #} #end boostvials row add
      
      #refresh data
      ####read in request data file 
      #allPIN <-fread(paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
      
      #add index
      #allPIN_indexed <-cbind(allPIN,data.frame(as.numeric(rownames(allPIN))))
      #names(allPIN_indexed)[21] <- "rn" #first  empty column
      
      #filter for future weeks and select fields you want to show
      #filtered_indexed <- allPIN_indexed %>%
      #filter(as.numeric(OperationalWeek)>currentoperationalweek)%>% #find all weeks past this week
      #filter(RequestStatus!="Canceled")%>%#remove canceled
      #mutate("DistributionWeek" = (distribution_week_list[match(OperationalWeek, week_number_list)]))
      
      #select fields you want to show
      #filtered_display <- filtered_indexed %>% #add distribution week column
      #select("RequestStatus", "DistributionWeek", "WVProviderPIN","TargetPopulation","VaccineBrand","Series","RequestedVials","DeliveryMethod",
      #"DistributionHub","PickUpDay", "ShippingAddress","NotesToApprover","NotesFromApprover")
      #refresh table
      #output$futureweeks_table<-renderDataTable({
      #datatable(filtered_display, selection = 'single', escape=F, rownames=FALSE, options = list(dom = 't',autoWidth = TRUE, pageLength = 100)) #allows for only selecting one row at a time
      #})#end render data table
      ####confirm change
      #showModal(modalDialog(title="Your changes have been saved!", 
      #modalButton("OK"),
      #easyClose = TRUE, footer = NULL, fade=TRUE
      #) #end modalDialog
      #)#end showModal
      #})#end observe event
      
      ###validation checks for submit click for direct shipment edit
      #validation_future4 <- reactive({
      #validate(
      #validate that prime vials are a whole number
      #need(input[[paste0("PrimeVials4",input$futureweeksmod_row_head)]]%%1==0 || input[[paste0("PrimeVials4",input$futureweeksmod_row_head)]]==0,
      #"Only whole vials are to be requested."),
      #validate that booster vials are a whole number
      #need(input[[paste0("BoosterVials4",input$futureweeksmod_row_head)]]%%1==0 || input[[paste0("BoosterVials4",input$futureweeksmod_row_head)]]==0,
      #"Only whole vials are to be requested."),
      #validate that total vials are >0
      # need((input[[paste0("PrimeVials4",input$futureweeksmod_row_head)]]+input[[paste0("BoosterVials4",input$futureweeksmod_row_head)]])>0,
      #"Zero vials indicates you want to cancel. Please click 'Dismiss' and use the 'Cancel Request' button."),
      #need all necessary fields filled out
      #need(input[[paste0("vaccines4",input$futureweeksmod_row_head)]]!="choose vaccine brand" &&
      # input[[paste0("TargetPopulation4",input$futureweeksmod_row_head)]]!= "",
      # "A mandatory field is missing. Please fill it out and try again."),
      #validate that sum of vials is a shippable quantity - Moderna
      #if(input[[paste0("vaccines4",input$futureweeksmod_row_head)]]=="Moderna"){
      # need((input[[paste0("PrimeVials4",input$futureweeksmod_row_head)]]+input[[paste0("BoosterVials4",input$futureweeksmod_row_head)]])%%moderna_container==0,
      #  paste("Moderna containers are shipped in quantities of",moderna_container,"vials. Please adjust your total request."))},
      #validate that sum of vials is a shippable quantity - Pfizer
      #if(input[[paste0("vaccines4",input$futureweeksmod_row_head)]]=="Pfizer"){
      #need((input[[paste0("PrimeVials4",input$futureweeksmod_row_head)]]+input[[paste0("BoosterVials4",input$futureweeksmod_row_head)]])%%pfizer_container==0,
      #paste("Pfizer containers are shipped in quantities of",pfizer_container,"vials. Please adjust your total request."))},
      #validate that sum of vials is a shippable quantity - Janssen
      #if(input[[paste0("vaccines4",input$futureweeksmod_row_head)]]=="Janssen"){
      # need((input[[paste0("PrimeVials4",input$futureweeksmod_row_head)]]+input[[paste0("BoosterVials4",input$futureweeksmod_row_head)]])%%janssen_container==0,
      #paste("Janssen containers are shipped in quantities of",janssen_container,"vials. Please adjust your total request."))},
      #validate that no Janssen boosters were requested
      #if(input[[paste0("vaccines4",input$futureweeksmod_row_head)]]=="Janssen" &&
      #input[[paste0("BoostVials4",input$futureweeksmod_row_head)]]>0){
      #need(input[[paste0("vaccines4",input$futureweeksmod_row_head)]]=="Janssen" &&
      #  input[[paste0("BoostVials4",input$futureweeksmod_row_head)]]==0,
      # "Janssen can only be requested as Prime series. Please set Boost to 0.")}
      #)#end validate
      # })#end validation
      
      ####validation messages in modal for direct ship edit
      #output$validation_change4_messages <- renderText(
      #paste(validation_future4()$run))
      
      ## If user says Submit Changes then modify record
      #observeEvent(input$save_changes4, {
      #validation
      #validation_future4()
      
      #close dialog box  
      #removeModal()#for message
      #just the 6-digits of the PIN
      #WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
      #update data
      ####read in request data file 
      #allPIN <-fread(paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
      
      #add index
      #allPIN_indexed <-cbind(allPIN,data.frame(as.numeric(rownames(allPIN))))
      #names(allPIN_indexed)[21] <- "rn" #first  empty column
      
      #filter for future weeks and select fields you want to show
      #filtered_indexed <- allPIN_indexed %>%
      #filter(as.numeric(OperationalWeek)>currentoperationalweek)%>% #find all weeks past this week
      #filter(RequestStatus!="Canceled")%>%#remove canceled
      #mutate("DistributionWeek" = (distribution_week_list[match(OperationalWeek, week_number_list)]))
      
      #old row being modified
      #change_row <- as.data.frame(filtered_indexed$rn)[as.numeric(unlist(input$futureweeks_table_rows_selected)),1]
      
      #update fields for that row
      #allPIN$VaccineBrand[change_row] <- input[[paste0("vaccines4",input$futureweeksmod_row_head)]]
      #allPIN$RequestedVials[change_row] <- as.numeric(input[[paste0("PrimeVials4",input$futureweeksmod_row_head)]]) + as.numeric(input[[paste0("BoosterVials4",input$futureweeksmod_row_head)]])
      #allPIN$TargetPopulation[change_row] <- input[[paste0("TargetPopulation4",input$futureweeksmod_row_head)]]
      #allPIN$NotesToApprover[change_row] <- input[[paste0("Notes4",input$futureweeksmod_row_head)]]
      #allPIN$NotesFromApprover[change_row] <- paste("prime vials:",input[[paste0("PrimeVials4",input$futureweeksmod_row_head)]],"; boost vials:",input[[paste0("BoosterVials4",input$futureweeksmod_row_head)]])
      
      #just the 6-digits of the PIN
      #WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
      #update file
      #fwrite(allPIN, paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
      #refresh data
      ####read in request data file 
      #allPIN <-fread(paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
      
      #add index
      #allPIN_indexed <-cbind(allPIN,data.frame(as.numeric(rownames(allPIN))))
      #names(allPIN_indexed)[21] <- "rn" #first  empty column
      
      #filter for future weeks and select fields you want to show
      #filtered_indexed <- allPIN_indexed %>%
      #filter(as.numeric(OperationalWeek)>currentoperationalweek)%>% #find all weeks past this week
      #filter(RequestStatus!="Canceled")%>%#remove canceled
      #mutate("DistributionWeek" = (distribution_week_list[match(OperationalWeek, week_number_list)]))
      
      #select fields you want to show
      #filtered_display <- filtered_indexed %>% #add distribution week column
      #select("RequestStatus", "DistributionWeek", "WVProviderPIN","TargetPopulation","VaccineBrand","Series","RequestedVials","DeliveryMethod",
      # "DistributionHub","PickUpDay", "ShippingAddress","NotesToApprover","NotesFromApprover")
      #refresh table
      #output$futureweeks_table<-renderDataTable({
      #datatable(filtered_display, selection = 'single', escape=F, rownames=FALSE, options = list(dom = 't',autoWidth = TRUE, pageLength = 100)) #allows for only selecting one row at a time
      #})#end render data table
      #give confirmation dialog box  
      ####confirm change
      #showModal(modalDialog(title="Your changes have been saved!", 
      # modalButton("OK"),
      # easyClose = TRUE, footer = NULL, fade=TRUE
      #) #end modalDialog
      #)#end showModal
      #})#end observe event
      
      ### download the table in csv
      output$futureweeks_csv<- downloadHandler(
        filename = function() {
          #just the 6-digits of the PIN
          WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
          paste("Future_Weeks_Requests_", WVSIISUserWVProviderPIN,"_",Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          #just the 6-digits of the PIN
          WVSIISUserWVProviderPIN <- substr(input$EIMSProviderPIN, nchar(input$EIMSProviderPIN)-6+1, nchar(input$EIMSProviderPIN))
          #update data
          ####read in request data file 
          allPIN <-fread(paste("data/Requests/",WVSIISUserWVProviderPIN,".csv", sep=""))
          
          #add index
          allPIN_indexed <-cbind(allPIN,data.frame(as.numeric(rownames(allPIN))))
          names(allPIN_indexed)[21] <- "rn" #first  empty column
          
          #filter for future weeks and select fields you want to show
          filtered_indexed <- allPIN_indexed %>%
            filter(as.numeric(OperationalWeek)>currentoperationalweek)%>% #find all weeks past this week
            filter(RequestStatus!="Canceled")%>%#remove canceled
            mutate("DistributionWeek" = (distribution_week_list[match(OperationalWeek, week_number_list)]))
          
          #select fields you want to show
          filtered_display <- filtered_indexed %>% #add distribution week column
            #select("RequestStatus", "DistributionWeek", "WVProviderPIN","TargetPopulation","VaccineBrand","Series","RequestedVials","DeliveryMethod",
            #"DistributionHub","PickUpDay", "ShippingAddress","NotesToApprover","NotesFromApprover")
            select("RequestStatus", "DistributionWeek", "WVProviderPIN","TargetPopulation","InventoryType","Series","RequestedUnits","DeliveryMethod",
                   "DistributionHub","PickUpDay","NotesToApprover")
          df <- apply(filtered_display,2,as.character)
          write.csv(df, file, row.names = F)
        }
      )    
      
      #-----End Change Order -----
      
    }#end loggedin =TRUE
  }) #end observe
}#end server function

#-------END SERVER SECTION-----

#----RUN THE APP-----
shinyApp(ui, server)