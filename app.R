library(shiny)
library(ggplot2)
library(shinythemes)



load(file = "territoire.RData")

###########################################################################
ui = fluidPage(
  theme = shinytheme("cerulean"),
  headerPanel(img(src='logo_daaf.png', align = "center"), windowTitle = "Mémento Agricole de la Martinique"),
  navlistPanel("Thématique",fluid = TRUE,
  
  tabPanel("SAU",
           
           h3("Répartition de la SAU Martiniquaise"),
          fluidRow(
            column(width = 4,
           selectInput("annee",
                       label = "Choisissez l'année :",
                       choices = list("2017","2016","2015","2014","2013","2012","2011","2010"),
                       selected = "2017"
           ),
           tableOutput("table"),
           downloadButton("do1", "Télécharger les données")),
           column(width = 8,
           plotOutput(outputId = "camembert")
           )
           )
          ),
  tabPanel("Banane export",
           h3("La Banane Export en Martinique"),
         #  sliderInput("perban", "Sélectionnez la période :", min = 2003, max = 2017, value = 2017),
             tableOutput("tabban"),
             downloadButton("do2", "Télécharger les données"),
         br(),
         br(),
            selectInput("parban",
                        label = "Visualisez :",
                        choices = list("production", "SAU", "rendement", "valeur", "prix"),
                        selected = "production"),
             plotOutput(outputId = "camban")
  ),
  tabPanel("Canne à sucre",
           h3("La canne, le sucre et le rhum en Martinique"),
           sliderInput("periode", label = h4("Choisissez la période (de 1977 à 2017)"), min = 1977, max = 2017, value = c(2010,2017)),
           br(),
           h4("La canne"),
           tableOutput("tabcan"),
           selectInput("parcan",
                       label = "Visualisez :",
                       choices = list("surface", "production", "rendement", "valeur", "prix"),
                       selected = "production"
                       ),

           plotOutput(outputId = "camcan"),
  
  h4("le sucre"),
  tableOutput("tabsucre"),
  selectInput("parsucre",
              label = "Visualisez :",
              choices = list("production", "teneur en sucre", "coefficient de paiement"),
              selected = "production"
  ),
  plotOutput("camsucre"),
  h4("le rhum"),
  tableOutput("tabrhum"),
  selectInput("parrhum",
              label = "Visualisez :",
              choices = list("rhum agricole", "rhum industriel"),
              selected = "rhum agricole"
  ),
  plotOutput("camrhum"),
  downloadButton("do3", "Télécharger les données")
           ),
  tabPanel("Fruits et légumes",
           h3("Les fruits et légumes"),
           h4("Evolution des quantités (tonnes) :"),
           tableOutput("tabfr1"),
           br(),
           selectInput("parfr1",
                       label = "Evolution par catégorie :",
                       choices = list("ananas", "banane créole", "autres fruits", "légumes", "tubercules"),
                       selected = "ananas"
           ),
           plotOutput("camfr1"),
           h4("Evolution des prix (euro/kg) :"),
           tableOutput("tabfr2"),
           br(),
           selectInput("parfr2",
              label = "Evolution des prix par catégorie :",
              choices = list("ananas", "banane créole", "autres fruits", "légumes", "tubercules"),
              selected = "ananas"
                       ),
           plotOutput("camfr2"),
           downloadButton("do4", "Télécharger les données")
           
           ),
  tabPanel("Elevage",
           h3("L'Elevage en Martinique"),
           selectInput("parel",
           label = "Types d'animaux :",
           choices = list("bovins", "ovins", "caprins", "porcins", "volailles", "lapins"),
           selected = "bovins"
                       ),
          h4("Le cheptel (nombre d'animaux)"),
          tableOutput("tabel1"),
          br(),
          plotOutput("camel1"),
          downloadButton("do5", "Télécharger les données"),
          h4("La production de viande annuelle (tonnes eq. carcasse)"),
          tableOutput("tabel2"),
          br(),
          plotOutput("camel2"),
          downloadButton("do6", "Télécharger les données")
           )
  )          
 
)

##################### FIN ui ######################################################
# Define server logic required to draw a histogram
server <- function(input, output) {

  output$camembert <- renderPlot(
    {
      
      SAU_an <- SAU[SAU$année==input$annee,]
      ggplot(data = SAU_an) + geom_col(mapping = aes(x = cultures, y = SAU ))
    }
  )
  
  
  
  output$table = renderTable(
    {
      SAU_n <- SAU[SAU$année==input$annee,-c(1)]
      
    }
  )
  output$do1 <- downloadHandler(
    filename = "SAU.csv",
    content = function(file){
      write.csv2(SAU, file)
    }
  )
  output$tabban = renderTable(
    {
     banane[,-c(4,6)]
    }
  )
  output$do2 <- downloadHandler(
    filename = "banane.csv",
    content = function(file){
      write.csv2(banane, file)
    }
  )
  output$camban <- renderPlot({
    parban <- input$parban
    if(parban=="production"){
    ggplot(data = banane) + geom_col(mapping = aes(x = annee, y = production))
    }
    else if(parban == "SAU"){
      ggplot(data = banane) + geom_col(mapping = aes(x = annee, y = sau))
    }
    else if (parban == "rendement"){
      ggplot(data = banane) + geom_line(mapping = aes(x = annee, y = rdt))
    }
    else if (parban == "valeur"){
      ggplot(data = banane) + geom_col(mapping = aes(x = annee, y = valeur))
    }
    else {
      ggplot(data = banane) + geom_line(mapping = aes(x = annee, y = prix))
    }
  }  
  )
  output$do3<- downloadHandler(
    filename = "canne.csv",
    content = function(file){
      write.csv2(canne, file)
    }
  )
  output$tabcan <- renderTable({
    canne[canne$anneee>=input$periode[1] & canne$anneee<=input$periode[2],c(1,2,5,6)]
  })
  output$camcan <- renderPlot({
    parcan <- input$parcan
    if(parcan =="production"){
      ggplot(data = canne[canne$anneee>=input$periode[1] & canne$anneee<=input$periode[2],]) + geom_col(mapping = aes(x = anneee, y = total))
    }
    else if (parcan == "coefficient de paiement"){
      ggplot(data = canne[canne$anneee>=input$periode[1] & canne$anneee<=input$periode[2],]) + geom_line(mapping = aes(x = anneee, y = rendement))
    }
    else if (parcan == "valeur"){
      ggplot(data = canne[canne$anneee>=input$periode[1] & canne$anneee<=input$periode[2],]) + geom_col(mapping = aes(x = anneee, y = valeur))
    }
    else {
      ggplot(data = canne[canne$anneee>=input$periode[1] & canne$anneee<=input$periode[2],]) + geom_line(mapping = aes(x = anneee, y = prix))
    }
  }  
  ) 
output$tabsucre <- renderTable(
  {
    canne[canne$anneee>=input$periode[1] & canne$anneee<=input$periode[2],c(1,3,7,8)]
  }
)  
output$camsucre <- renderPlot({
  parsucre <- input$parsucre
  if(parsucre =="production"){
    ggplot(data = canne[canne$anneee>=input$periode[1] & canne$anneee<=input$periode[2],]) + geom_col(mapping = aes(x = anneee, y = sucre))
  }
  else if(parsucre == "teneur en sucre"){
    ggplot(data = canne[canne$anneee>=input$periode[1] & canne$anneee<=input$periode[2],]) + geom_line(mapping = aes(x = anneee, y = saccharose))
  }
  else{
    ggplot(data = canne[canne$anneee>=input$periode[1] & canne$anneee<=input$periode[2],]) + geom_line(mapping = aes(x = anneee, y = `coef paiement`))
  }
}  
)
output$camrhum <- renderPlot(
  {
    parrhum <- input$parrhum
    if(parrhum == "rhum agricole"){
    ggplot(data = canne[canne$anneee>=input$periode[1] & canne$anneee<=input$periode[2],]) + geom_col(mapping = aes(x = anneee, y = `rhum agricole`))
    }
    else
    {
      ggplot(data = canne[canne$anneee>=input$periode[1] & canne$anneee<=input$periode[2],]) + geom_col(mapping = aes(x = anneee, y = `rhum industriel`))
    }
      }
)
output$tabrhum <- renderTable(
  {
    canne[canne$anneee>=input$periode[1] & canne$anneee<=input$periode[2],c(1,10,11)]
  }
)
output$do4<- downloadHandler(
  filename = "fruits.csv",
  content = function(file){
    write.csv2(fruits, file)
  }
)
output$tabfr1 <- renderTable(
  {
data.frame("année" = c("2012", "2013", "2014", "2015", "2016", "2017"),as.data.frame.array(xtabs(quantite~annee+produit, data=fruits)))

  }
)
output$tabfr2 <- renderTable(
  {
data.frame("année" = c("2012", "2013", "2014", "2015", "2016", "2017"),as.data.frame.array(xtabs(prix~annee+produit, data=fruits)))
  }
)

output$camfr1 <- renderPlot(
  {
    parfr1 <- input$parfr1
    if(parfr1 == "ananas"){
      ggplot(data = fruits[fruits$produit=="ananas",]) + geom_col(mapping = aes(x = annee, y = quantite))
    }
    else if(parfr1 == "banane créole"){
      ggplot(data = fruits[fruits$produit=="banane créole",]) + geom_col(mapping = aes(x = annee, y = quantite))
    }
    else if(parfr1== "autres fruits"){
      ggplot(data = fruits[fruits$produit=="autres fruits",]) + geom_col(mapping = aes(x = annee, y = quantite))
    }
    else if(parfr1 == "légumes"){
      ggplot(data = fruits[fruits$produit=="légumes",]) + geom_col(mapping = aes(x = annee, y = quantite))
    }
    else{
      ggplot(data = fruits[fruits$produit=="tubercules",]) + geom_col(mapping = aes(x = annee, y = quantite))
    }
    
  }
)
output$camfr2 <- renderPlot(
  {
    parfr2 <- input$parfr2
    if(parfr2 == "ananas"){
      ggplot(data = fruits[fruits$produit=="ananas",]) + geom_line(mapping = aes(x = annee, y = prix))
    }
    else if(parfr2 == "banane créole"){
      ggplot(data = fruits[fruits$produit=="banane créole",]) + geom_line(mapping = aes(x = annee, y = prix))
    }
    else if(parfr2== "autres fruits"){
      ggplot(data = fruits[fruits$produit=="autres fruits",]) + geom_line(mapping = aes(x = annee, y = prix))
    }
    else if(parfr2 == "légumes"){
      ggplot(data = fruits[fruits$produit=="légumes",]) + geom_line(mapping = aes(x = annee, y = prix))
    }
    else{
      ggplot(data = fruits[fruits$produit=="tubercules",]) + geom_line(mapping = aes(x = annee, y = prix))
    }
    
  }
)
output$do5 <- downloadHandler(
  filename = "cheptel.csv",
  content = function(file){
    write.csv2(cheptel, file)
  }
)
output$do6 <- downloadHandler(
  filename = "abattages.csv",
  content = function(file){
    write.csv2(abattages, file)
  }
)
output$tabel1 <- renderTable(
  {
    data.frame("année" = c("2010","2011", "2012", "2013", "2014", "2015", "2016", "2017"),as.data.frame.array(xtabs(effectif~annee+cheptel, data=cheptel)))
  }
)
output$tabel2 <- renderTable(
  {
    data.frame("année" = c("2010","2011", "2012", "2013", "2014", "2015", "2016", "2017"),as.data.frame.array(xtabs(abattage~annee+cheptel, data=abattages)))
  }
)
# "bovins", "ovins", "caprins", "porcins", "volailles", "lapins"
output$camel1 <- renderPlot(
  {
    parel <- input$parel
    if(parel == "bovins"){
      ggplot(data = cheptel[cheptel$cheptel=="bovins",]) + geom_line(mapping = aes(x = annee, y = effectif))
    }
    else if(parel == "porcins")
    {
      ggplot(data = cheptel[cheptel$cheptel=="porcins",]) + geom_line(mapping = aes(x = annee, y = effectif))
    }
    else if(parel == "volailles")
    {
      ggplot(data = cheptel[cheptel$cheptel=="volailles",]) + geom_line(mapping = aes(x = annee, y = effectif))
    }
    else if(parel == "lapins")
    {
      ggplot(data = cheptel[cheptel$cheptel=="lapines mères",]) + geom_line(mapping = aes(x = annee, y = effectif))
    }
    else if(parel == "ovins")
    {
      ggplot(data = cheptel[cheptel$cheptel=="ovins",]) + geom_line(mapping = aes(x = annee, y = effectif))
    }
    else
    {
      ggplot(data = cheptel[cheptel$cheptel=="caprins",]) + geom_line(mapping = aes(x = annee, y = effectif))
    }
  }
)
output$camel2 <- renderPlot(
  {
    parel <- input$parel
    if(parel == "bovins"){
      ggplot(data = abattages[abattages$cheptel=="bovins",]) + geom_col(mapping = aes(x = annee, y = abattage))
    }
    else if(parel == "porcins")
    {
      ggplot(data = abattages[abattages$cheptel=="porcins",]) + geom_col(mapping = aes(x = annee, y = abattage))
    }
    else if(parel == "volailles")
    {
      ggplot(data = abattages[abattages$cheptel=="volailles",]) + geom_col(mapping = aes(x = annee, y = abattage))
    }
    else if(parel == "lapins")
    {
      ggplot(data = abattages[abattages$cheptel=="lapins",]) + geom_col(mapping = aes(x = annee, y = abattage))
    }
    else
    {
      ggplot(data = abattages[abattages$cheptel=="ovins-caprins",]) + geom_col(mapping = aes(x = annee, y = abattage))
    }
    
    
    
  }
)
#################### FIN server ##############################
}
# Run the application 
shinyApp(ui = ui, server = server)