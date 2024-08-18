# Load packages
library(shiny)
library(bs4Dash)
library(waiter)
library(firebase)
library(lubridate)
library(dplyr)
library(shinyjs)
library(shinyWidgets)
library(glue)
library(bslib)
library(jsonlite)
library(lubridate)
library(typedjs)
library(stringr)
library(tm)
library(reticulate)
library(httr)
library(jsonlite)
library(sever)
library(wordcloud2)
library(data.table)
library(purrr)
library(plyr)
library(pool)
library(dbplyr)

OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")
DETAIL_KEY <- Sys.getenv("DETAIL_KEY") 

USER_TABLE= "utilisateur_airb"

waiting_104 <- tagList(
  html = waiter::bs4_spinner(),
  div(style="color:black;",tags$br(),tags$br(), h3("Loading data, please wait..."))
)

source_python("airbnb_functions.py")


# Define pool handler by pool on global level
pool <- pool::dbPool(odbc::odbc(),
                     Driver = "postgresql",
                     Server =ifelse(Sys.getenv("ENVIR")=="dev", Sys.getenv('PGIP'), paste0("/cloudsql/",Sys.getenv('cloud_sql_con_name')) ),
                     Database = Sys.getenv("dbname") ,
                     UID = Sys.getenv("user"),
                     PWD = Sys.getenv("password"),
                     sslmode="require",
                     Port = 5432)

onStop(function() {
  pool::poolClose(pool)
})


checkCounter <- function(pool, email){
  
  tbl(pool,USER_TABLE ) %>% filter(email==email)%>% pull(query_count)
  
}

addOneQuery = function(pool, email){
  
  q <- glue_sql("
        UPDATE {DBI::SQL(USER_TABLE)} 
        SET query_count = query_count + 1
        WHERE email = {email};", .con=pool)
  
  dbExecute(pool,q )
  
  
}

ui = dashboardPage(
  dark = F,
  help = F,
  fullscreen = F,
  scrollToTop = TRUE,
  header = dashboardHeader(border = F, 
                           title = div(style='padding: 10px;',
                                       img( class="ui avatar image", src="https://img.icons8.com/?size=512&id=UCgJoZGoeBg1&format=png"),
                                       span('Aireview')
                           ),
                           fixed = TRUE, 
                           tags$div(
                             class = "navbar-custom-menu",
                             style ="width: 80%;margin-top: 10px;",
                             tags$ul(
                               class = "nav navbar-nav",
                               style = "margin-left: auto; margin-right: auto; width: 50%;",
                               tags$li(
                                 style = "width: 100%;",
                                 div( class="ui icon input",style="width:100%;padding-bottom: 15px;",
                                      tags$input( type="text", id="searchInput",value="",  placeholder="https://www.airbnb.fr/rooms/51749338",style="width:100%;border-radius:20px;",
                                                  tags$i( class="inverted circular red search link icon", id = "searchIcon")
                                      )
                                 )
                               )
                             )
                           ), 
                           
                           rightUi = tagList(
                             uiOutput("user"), 
                             
                           )
  ),
  sidebar = dashboardSidebar(disable = T, skin = "light"),
  body = dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.css"),
      
    ),
    useFirebase(), 
    useShinyjs(),
    use_waiter(), 
    useSever(), 
    includeCSS("www/style.css"),
    firebaseUIContainer(), 
    fluidRow(
      column(width = 12, 
             uiOutput("P8",fill = T,inline = T, width="100%")
      )
    )
    
    
  ),
  controlbar = NULL,
  footer = tags$footer( class="main-footer fixed-bottom" ,
                        div( class="float-right d-none d-sm-inline",
                             style="width:12%;",
                             uiOutput('progressbare')
                             
                        ),
                        tags$a( target="_blank",
                                div( class="disabled item", "Contact: miskowski85@hotmail.fr") ) ),
  title = "Aireview"
)



server = function(input, output, session) {
  
  
  sever(html = tagList(
    h1("Whoops!"),
    p("Your have been disconnected"),
    reload_button("REFRESH", class = "warning")
  ), bg_image =paste0(
    "https://images.pexels.com/photos/4827/",
    "nature-forest-trees-fog.jpeg?auto=compress",
    "&cs=tinysrgb&dpr=2&h=750&w=1260"
  )
  , color = "white")
  
  
  f <- FirebaseUI$
    new(persistence = "local")$ # instantiate
    set_providers( # define providers
      google = TRUE
    )$
    launch() # launch
  
  
  
  
  
  # Utilisation de shinyjs pour capturer les événements
  observe({
    # Capture de l'événement de la touche "Entrée"
    runjs('
      $("#searchInput").keypress(function(e) {
        if(e.which == 13) {
          Shiny.setInputValue("search", $("#searchInput").val(), {priority: "event"});
        }
      });
    ')
    
    # Capture de l'événement du clic sur l'icône
    runjs('
      $("#searchIcon").click(function() {
        Shiny.setInputValue("search", $("#searchInput").val(), {priority: "event"});
      });
    ')
  })
  
  
  query_count <- reactiveVal(0)
  
  
  # Observer pour gérer l'authentification et les permissions
  observe({
    f$req_sign_in() # Requiert une connexion
    current_user <- f$get_signed_in()
    
    user_photo_url <- current_user$response$photoURL
    user_email <- current_user$response$email
    last_login_date <- Sys.Date()
    
    # Construire et exécuter la requête d'insertion/mise à jour de l'utilisateur
    user_upsert_query <- glue_sql("INSERT INTO {DBI::SQL(USER_TABLE)} (nom, email, last_login_at)
    VALUES ({current_user$response$displayName},{user_email}, {last_login_date})
    ON CONFLICT (email)
    DO UPDATE SET last_login_at = EXCLUDED.last_login_at;", .con = pool)
    
    
    tryCatch({
      dbExecute(pool, user_upsert_query)
    }, error = function(e) {
      print(e$message)
    })
    
    
    query_count(checkCounter(pool, user_email))
    
    output$progressbare <- renderUI({
      
      f$req_sign_in() # Requiert une connexion
      shinyWidgets::progressBar(id = "prg",value = as.integer( query_count()),range_value = c(0,10),total = 10,title = "Limit" ,  striped = T, status = "success",size = "xs")
      
    })
    
    
    
    
  })
  
  
  
  
  
  
  # user menu ---------------------------------------------------------------
  
  com<- reactiveValues()
  description <- reactiveValues(r=NULL)
  propostions <- reactiveValues(r=NULL)
  
  output$user <- renderUI({
    
    f$req_sign_in() 
    
    user <- f$get_signed_in()
    
    div(style='padding: 10px;',
        img( class="ui avatar image", src=user$response$photoURL),
        span(user$response$displayName)
    )
    
  })
  
  
  
  observeEvent(input$signout, {
    f$sign_out()
  })
  
  
  #---------------------------
  
  
  xres <- reactiveValues(x=NULL)
  revsres <- reactiveValues(revs=NULL)
  
  observeEvent(input$search,{
    f$req_sign_in() 
    text <- input$search
    
    regex <- "\\bhttps://[^\\s/$.?#].[^\\s]*airbnb\\.\\w{2,}\\/(rooms|lux\\/list)\\/\\d+\\b"
    matchea <-  str_detect(string = text,pattern =  regex) 
    
    
    if(matchea){
      waiter_show(html = waiting_104, color = "transparent")
      
      
      tryCatch({
        
        
        adr <- strsplit(input$search, "\\?")[[1]][1]
        
        room_id <- gsub(".*/rooms/(\\d+)*", "\\1",adr)
        
        listing_details <- content(GET(glue('https://www.airbnb.de/api/v2/pdp_listing_details/{room_id}?_format=for_rooms_show&_p3_impression_id=p3_1587291065_1e%2FBlC2IefkrfTQe&adults=1&check_in={Sys.Date()+2}&check_out={Sys.Date()+7}&key={DETAIL_KEY}')))
        
        listingId=room_id
        descriptif= listing_details[["pdp_listing_detail"]][["sectioned_description"]][["description"]]
        host_pic=listing_details[["pdp_listing_detail"]][["primary_host"]][["profile_pic_path"]]
        titre=listing_details[["pdp_listing_detail"]][["name"]]
        imageUrl = listing_details[["pdp_listing_detail"]][["photos"]][[1]][["large"]]
        revs <- get_all_reviews(listingId)
        
        reviewCount  <- length(revs) 
        
        
        guestSatisfactionOverall <- listing_details[["pdp_listing_detail"]][["reviews_module"]][["localized_overall_rating"]] 
        
        
        xres$x <-list(
          listingId=room_id, 
          descriptif=descriptif, 
          host_pic=host_pic,
          titre=titre, 
          reviewCount=reviewCount, 
          guestSatisfactionOverall=guestSatisfactionOverall, 
          imageUrl= imageUrl, 
          listing_details=listing_details
        ) 
        
        
        revsres$revs <- revs[order(sapply(revs, function(x) as.POSIXct(x$created_at)),  decreasing = F)]
        
        
      }, error = function(e) {
        print("An error occurred:")
        print(e)
        
        waiter_hide()
        xres$x <- NULL
        revsres$revs <- NULL
      })
      
      
      waiter_hide()
    }else{
      
      xres$x <- NULL
      revsres$revs <- NULL
      
      showNotification(ui = "Enter a valid Airbnb url", type = "error")
      
      
    }
    
  })
  
  
  
  
  observeEvent(input$selected,{
    
    f$req_sign_in() 
    req(revsres$revs)
    user <- f$get_signed_in()
    email <- user$response$email
    cq=as.integer(checkCounter(pool, email))
    
    
    
    
    
    
    
    
    if(cq<=10){
      
      ii<-input$selected
      
      
      shinyjs::runjs(glue('document.getElementById("reps<<ii>>").innerHTML = "<div class=\'ui active inline mini loader\'></div>";', .open = "<<", .close = ">>"))
      
      filtered_list <- Filter(function(x) x[["id_str"]] == as.character(input$selected), revsres$revs)
      
      guest <- filtered_list[[1]][["author"]][["first_name"]]
      host <- filtered_list[[1]][["recipient"]][["first_name"]]
      comment <- filtered_list[[1]][["comments"]]
      lang=toupper( filtered_list[[1]][["language"]])
      
      interactions <- xres$x$listing_details[["pdp_listing_detail"]][["sectioned_description"]][["interaction"]]
      house_rules <- xres$x$listing_details[["pdp_listing_detail"]][["sectioned_description"]][["house_rules"]]
      access <- xres$x$listing_details[["pdp_listing_detail"]][["sectioned_description"]][["access"]]
      notes <- xres$x$listing_details[["pdp_listing_detail"]][["sectioned_description"]][["notes"]]
      descr <- xres$x$listing_details[["pdp_listing_detail"]][["sectioned_description"]][["description"]]
      
      prompt <- glue(
        "Vous êtes <<host>>, un hôte expérimenté sur la plateforme Airbnb. Vous venez de recevoir l'avis suivant de la part de <<guest>> :

        <<comment>>
        
        Instructions :
        1. Cet avis est rédigé en <<lang>> (code de langue).
        2. Répondez de manière professionnelle et chaleureuse dans la même langue.
        3. Remerciez <<guest>> pour son feedback.
        4. Abordez spécifiquement les points positifs mentionnés.
        5. Si des problèmes sont soulevés, proposez des solutions concrètes.
        6. Invitez <<guest>> à revenir pour un futur séjour.
        7. Limitez votre réponse à 3-4 phrases concises.
        
        Commencez directement votre réponse sans répéter ces instructions.",
        .open = "<<", .close = ">>"
      )
      
      
      response <<- POST(
        url = "https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = paste("Bearer", OPENAI_API_KEY)),
        content_type_json(),
        encode = "json",
        body = list(
          model = "gpt-3.5-turbo",
          temperature = 1,
          messages = list(list(
            role = "user",
            content = prompt
          ))
        )
      )
      
      
      reponsechat <- httr::content(response)$choices[[1]]$message$content
      
      
      com[[input$selected]] <- reponsechat
      addOneQuery(pool, email)
      
      query_count(cq+1)
      
    }else{
      
      showNotification(ui = "you've reached the usage limit", type = "error")
      
    }
    
    
    
    
  })
  
  
  
  output$P8 <- renderUI({
    
    f$req_sign_in() 
    #req(revsres$revs)
    
    fluidPage(
      
      br(), 
      fluidRow(
        
        column(4,
               
               uiOutput("main_summary")
               
        ),
        
        column(8,
               
               if(!is.null(xres$x)){
                 
                 bs4Dash::tabsetPanel(
                   
                   side = "left",
                   id = "tabcard2",
                   type = "tabs",
                   selected = "Reviews",
                   
                   tabPanel(
                     "Reviews",
                     
                     bs4Dash::box(title = NULL,footer = NULL, width = 12,closable = F, collapsible = F, height = "65vh",elevation = 0, id = "de", 
                                  
                                  output$plots <- renderUI({
                                    f$req_sign_in() # Requiert une connexion
                                    
                                    plot_output_list <- lapply(revsres$revs, function(rv){
                                      
                                      plotname <- paste0("o-", rv[["id_str"]])
                                      uiOutput(plotname)
                                    })
                                    
                                    tagList(plot_output_list)
                                  }) 
                                  
                     )
                     
                     
                   ),
                   
                   tabPanel(
                     "Wordcloud", 
                     uiOutput("wc")
                   ), 
                   
                   tabPanel(
                     "Recommander", 
                     uiOutput("reco")
                   )
                   
                   
                 )
               }else(
                 div(
                   br(), 
                   
                   img( src="www/undraw_house.svg",style="width:350px;height:66vh;margin-left: 50px;")
                 )
               )
               
               
               
               
               
        )
        
        
      )
      
      
      
    )
    
    
  })
  
  
  observeEvent(revsres$revs, ignoreNULL = T, {
    
    f$req_sign_in() 
    req( revsres)
    req(xres)
    st <-lapply(revsres$revs, function(rv){
      
      r=rv[["id_str"]]
      
      com[[r]] <- rv[["response"]]
      
    })
    
    
    
    lapply(revsres$revs, function(rv){
      
      r=rv[["id_str"]]
      
      output[[paste0("o-",r)]]<- renderUI({
        f$req_sign_in() # Requiert une connexion
        
        div(class = "ui comments",
            div(class = "comment",
                a(class = "avatar",
                  img(src =rv[["author"]][["picture_url"]])
                ),
                div(class = "content",
                    a(class = "author", rv[["author"]][["first_name"]]),
                    div(class = "metadata",
                        div(class = "date",as_date( lubridate::as_datetime(rv[["created_at"]]))),
                        div(class = "rating",
                            tags$i(class = "star yellow icon"),
                            rv[["rating"]]
                        )
                    ),
                    div(class = "text  direct-chat-text",
                        rv[["comments"]]
                    ), 
                    
                    div(class = "comments",
                        div(class = "comment",
                            a(class = "avatar",
                              img(src = rv[["recipient"]][["picture_url"]])
                            ),
                            div(class = "content",
                                a(class = "author",rv[["recipient"]][["first_name"]]),
                                
                                div(class = "text direct-chat-text",id=paste0("reps", rv[["id_str"]]), 
                                    typed( ifelse(nchar( rv[["response"]])==0, com[[r]],rv[["response"]] )  ,contentType = 'HTML')),
                                div(class = "actions",
                                    #a(HTML('<i class="paper plane icon"></i>'), "Generate Reply") 
                                    if(nchar( rv[["response"]])==0){
                                      actionButton(paste0("myDiv", rv[["id_str"]]), label = "Generate response", icon("paper-plane"),size = "xxs",outline = T,status = "primary")
                                    }
                                    
                                    
                                )
                            )
                        )
                    )
                    
                )
            )
            , 
            # HTML(glue('<script>  <<st>></script>',.open="<<", .close=">>")),
            HTML(glue('<script> 
                   document.getElementById("myDiv<<r>>").addEventListener("click", () => Shiny.setInputValue("selected", "<<r>>", {priority:"event"}));
                 </script>', .open = "<<", .close = ">>")
            )
            
        )
        
        
      })
      
    })
    
    
    output$main_summary <- renderUI({
      
      f$req_sign_in() 
      if(!is.null(xres$x)){
        
        socialBox(title =userBlock(
          image = xres$x[[3]],
          title = xres$x[[4]],
          subtitle = div(style="padding-top:8px;",
                         HTML(paste0('<i class="comments icon"></i> ',xres$x[["reviewCount"]] , " Reviews . ", '<i class="star yellow icon"></i>', xres$x[["guestSatisfactionOverall"]] ) )
                         
                         
                         
                         
          )
        ),  footer = NULL, width = 12, collapsible = F, collapsed = F, closable = F,elevation = 0, 
        
        carousel(
          id = "mycarousel",indicators = T,
          width = 12,
          carouselItem(
            tags$img(style="border-radius:8px; background-size: cover;
    background-position: center;
    width: 100%;
    height: 100%;",src = xres$x$listing_details[["pdp_listing_detail"]][["photos"]][[1]][["large"]])
          ),
          carouselItem(
            tags$img(style="border-radius:8px; background-size: cover;
    background-position: center;
    width: 100%;
    height: 100%;",src =  xres$x$listing_details[["pdp_listing_detail"]][["photos"]][[2]][["large"]])
          ),
          carouselItem(
            tags$img(style="border-radius:8px; background-size: cover;
    background-position: center;
    width: 100%;
    height: 100%;", src = xres$x$listing_details[["pdp_listing_detail"]][["photos"]][[3]][["large"]])
          ), 
          carouselItem(
            tags$img(style="border-radius:8px; background-size: cover;
    background-position: center;
    width: 100%;
    height: 100%;", src =  xres$x$listing_details[["pdp_listing_detail"]][["photos"]][[4]][["large"]])
          )
          
        )
        
        
        )
        
        
        
        
        
      }else{
        
        NULL
        
      }
      
      
      
    })
    
    
    
    
    
    
    output$typc <- renderUI({
      
      f$req_sign_in() 
      
      div(class = "text direct-chat-text" ,
          typed( elementId = "des",  description$r  ,contentType = 'HTML'))
    })
    
    
    output$wc <- renderUI({
      
      f$req_sign_in() 
      
      fluidPage(
        fluidRow(
          column(4,
                 br(), 
                 box(title = "Settings",
                     icon = icon("cogs"),width = 12, #height = "65vh",
                     footer =NULL, # actionButton("val",label = "Generate", width = "100%"), 
                     maximizable = F, closable = F, collapsible = F, 
                     
                     tagList(
                       
                       
                       sliderInput("freq",ticks = F,
                                   "Minimum Frequency:",
                                   min = 1,  max = 50, value =1),
                       sliderInput(inputId = "rotate", ticks = F, label = "rotate Ratio", min = 0, max = 1, value = 1),
                       sliderInput(inputId = "minrotate", ticks = F, label = "min rotate", min = -90, max = 90, value = -1),
                       sliderInput(inputId = "maxrotate", ticks = F, label = "max Ratio", min = -90, max = 90, value = -1),
                       pickerInput(inputId = "font",label = "font fmaily", choices = names(pdfFonts()),multiple = F,width = "100%", 
                                   options = list(
                                     `live-search` = TRUE,  size = 3)
                       ),
                       numericInput("size",step = 0.1,
                                    "Font size:",
                                    min = 0.5,  max = 3, value =1),
                       
                       radioGroupButtons(
                         inputId = "rcolor",
                         label = NULL,
                         choices = c("random-dark", "random-light" ),
                         individual = F,justified = T, 
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-circle", 
                                        style = "color: steelblue"),
                           no = tags$i(class = "fa fa-circle-o", 
                                       style = "color: steelblue"))
                       ),
                       colorPickr(
                         inputId = "id1",preview=F, hue=T, opacity=T,selected = "#E1E1E1",  
                         label = NULL,inline =F, useAsButton=F,width = "100%", 
                       ),
                       textInput(inputId = "stopw",label = "Ignore words :",placeholder = "Hello the and")
                       
                       
                       
                       
                     )
                     
                 )
                 
          ), 
          column(8, 
                 br(),
                 wordcloud2Output("wordc",width = "100%",height = "100%")
                 
          )
          
        )
      )
      
      
      
    })
    
    
    
    output$wordc <- renderWordcloud2({
      
      f$req_sign_in() 
      req(revsres$revs)
      
      suppressWarnings({
        docs <- Corpus(VectorSource( lapply(revsres$revs, FUN = function(x) x[["comments"]] )%>%unlist()))
        docs <- tm_map(docs, removeWords, stopwords("en"))
        dtm <- TermDocumentMatrix(docs) 
        matrix <- as.matrix(dtm) 
        words <- sort(rowSums(matrix),decreasing=TRUE) 
        df <- data.frame(word = names(words),freq=words)
      })
      
      df %>% filter(freq>=input$freq, !word%in% strsplit(input$stopw," ")[[1]] )-> df
      
      
      
      wordcloud2(data=df, backgroundColor = input$id1,fontFamily = input$font, 
                 rotateRatio=input$rotate, minRotation = input$minrotate,
                 maxRotation = input$maxrotate, color = input$rcolor, size = input$size
      )
      
    })
    
    
    #})
    
    
    
    
    
    
    output$typr <- renderUI({
      f$req_sign_in() 
      
      div(class = "text direct-chat-text" ,
          typed( elementId = "dese",strings =  HTML(as.character(propostions$r) ) ,contentType = 'html'))
    })
    
    
    
    observeEvent(input$actionr,{
      
      f$req_sign_in() 
      req(revsres$revs)
      
      user <- f$get_signed_in()
      email <- user$response$email
      
      cq=as.integer(checkCounter(pool, email))
      if(cq<=10){
        
        shinyjs::runjs(glue('document.getElementById("dese").innerHTML = "<div class=\'ui active inline mini loader\'></div>";', .open = "<<", .close = ">>"))
        
        xf <-as.integer(input$nrc)
        
        rvp <- laply(1:length(revsres$revs), function(x) revsres$revs[[x]][["comments"]] ) %>% unlist()
        rvp <- sample( rvp, min(150, length(rvp) ) )  %>% paste0(collapse = "<br> Guest : ")
        
        
        prompt <-glue("I'm an airbnb host and I've received these reviews: are there things I can improve to satisfy my guests? if so, you list ONLY the {xf} most important ones as html list with some Emojis back to line after each item of the list (ie \n ) , if there's nothing to improve, you congratulate me and don't give any recommendations. here are the reviews :{rvp} ")
        
        
        response <- POST(
          url = "https://api.openai.com/v1/chat/completions",
          add_headers(Authorization = paste("Bearer", OPENAI_API_KEY)),
          content_type_json(),
          encode = "json",
          body = list(
            model = "gpt-3.5-turbo",
            temperature = 1,
            messages = list(list(
              role = "user",
              content = prompt
            ))
          )
        )
        
        #propostions$r <- prompt
        propostions$r <-  httr::content(response)$choices[[1]]$message$content
        
        
        addOneQuery(pool, email)
        
        query_count(cq+1)
        
      }else{
        
        showNotification(ui = "you've reached the usage limit", type = "error")
        
      }
      
    })
    
    
    output$titl <- renderUI({
      
      f$req_sign_in()
      
      tagList(
        tags$p("Generate top ", style = "display: inline; margin: 0;"),
        
        tags$input(
          id = "nrc",
          type = "number",
          value = 3,
          min = 1,
          max = 7,
          step = 1,
          style = "display: inline; width: 50px; border: none; border-bottom: 1px solid #ccc; font-size: inherit; margin-left: 5px; margin-right: 5px;"
        ),
        
        tags$p(" recommendations to improve guest experience: ", style = "display: inline; margin: 0;"),
        
        actionButton(
          "actionr",
          outline = T,
          label = NULL,
          icon =icon("paper-plane"),
          status = "primary",
          style = "display: inline; margin-left: 5px; vertical-align: middle;"
        )
      )
    })
    
    output$reco <- renderUI({
      
      
      f$req_sign_in() 
      div(
        br(),
        br(),
        tags$div(class = "ui comments",
                 tags$div(class = "comment",
                          tags$a(class = "avatar",
                                 tags$img(src = "https://img.icons8.com/?size=512&id=UCgJoZGoeBg1&format=png")
                          ),
                          tags$div(class = "content",
                                   
                                   tags$div(class = "text", 
                                            uiOutput('typr')
                                   ),
                                   tags$div(class = "actions",
                                            uiOutput("titl")
                                   )
                          )
                 )
        )
        
      ) 
      
      
    })
    
    
    
    
    
    
  })
  
  
  
  
  
}




addResourcePath("www", "www/")

shinyApp(ui = ui, server = server, options = list(port = as.integer(Sys.getenv('PORT')), host = "0.0.0.0"))