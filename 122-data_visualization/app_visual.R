#stats_2_data_visulaization
setwd('C:/Users/THINK_SUBJECT22/Documents/Z_rest/AAA_stat_tutor/shiny_example_via_git/shiny-examples/121-random_noise/data')
d <- read.csv('beers_smiles.csv')
#packages:
library(shiny)
d <- data.frame(seq(100), seq(100))
colnames(d) = c('beers', 'smiles')
#define the names of the IV and the DV::
IV = colnames(d)[1]
DV = colnames(d)[2]
#this is the correlation:
cor(d, method = 'pearson')
#this should be equivalent to the regression: (glm)
fit = lm(smiles ~ beers, data = d)
summary(fit) # show results

B_text = 'B:'
B = round(lsfit(d[IV], d[DV])[[1]][2], 3)
beta_text = 'Beta:'
beta =round(B * (sd(d[[IV]])/sd(d[[DV]])), 3)
cor_text = 'Correlation:'
correlation = round(cor(d, method = 'pearson')[2], 3)
  
#changing the value adds random noise to the data (to the nr smiles)
value = 100
d[DV] <- d[DV] + rnorm(100, sd = value)
par(mfrow = c(1,2), mar = c(1,1,1,1), oma = c(1,1,1,10)) #note: bottom, left, top and right . notes oma = outer margin region, mar = margins for ech specific plot
hist(d[[DV]], breaks = 3)
plot(d, xpd = NA)
text(correlation, y = 8, x =16, xpd = NA)
text(B, y = 5, x =16, xpd = NA)
text(beta, y = 2, x =16, xpd = NA)
text(cor_text, y = 9, x =16, xpd = NA)
text(B_text, y = 6, x =16, xpd = NA)
text('beta_text', y = 1, x =150, xpd = NA)

#code above here is the same as the plot below, just here to test changes


#todo::
#make variables more abstract
#make the layout prettier

#add sd of y and x: and show the clculation of B.
#show the table output from R code (as an image?)
#--> make a slider to add randomness + get this into a shiny app:
#make new folder in the git repository + add the example of adding randomness.

  
#################shiny ap here:
ui <- fluidPage(
  # App title ----
  titlePanel("Visualizing random noise"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "noise",
                  label = "Amount of noise (defined as the standard deviation of the distribution from which the random number are picked)",
                  min = 0,
                  max = 100,
                  value = 0)
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
    )
  )
)

#note: boins has become 'noise'

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    d <- data.frame(seq(100), seq(100))
    colnames(d) = c('beers', 'smiles')
    #define the names of the IV and the DV::
    IV = colnames(d)[1]
    DV = colnames(d)[2]
    #adding random noise dependent on the slider value
    value = input$noise
    d[DV] <- d[DV] + rnorm(100, sd = value)
    #this should be equivalent to the regression: (glm)
    fit = lm(smiles ~ beers, data = d)
    B_text = 'B:'
    B = round(lsfit(d[IV], d[DV])[[1]][2], 3)
    beta_text = 'Beta:'
    beta =round(B * (sd(d[[IV]])/sd(d[[DV]])), 3)
    cor_text = 'Correlation:'
    correlation = round(cor(d, method = 'pearson')[2], 3)
    
    #determine cooridnates for the tekst
    step = (range(d[DV])[1]*-1+range(d[DV])[2]) /20 
    x1 = 160
    y1 = max(d[DV])-(step*2) 
    x11 = 160
    y11 = max(d[DV])-(step*3)
    
    x2 =160 
    y2 = max(d[DV]) - (step * 5)
    x22 = 160
    y22 = max(d[DV])- (step * 6)
    
    x3 = 160
    y3 = max(d[DV])- (step * 8)
    x33 = 160
    y33 = max(d[DV])- (step * 9)
    
    
    #changing the value adds random noise to the data (to the nr smiles)
    value = 0
    d[DV] <- d[DV] + rnorm(100, sd = value)
    par(mfrow = c(1,2), mar = c(1,1,1,1), oma = c(1,1,1,10)) #note: bottom, left, top and right . notes oma = outer margin region, mar = margins for ech specific plot
    hist(d[[DV]], breaks = 20)
    plot(d, xpd = TRUE)
    #placing the text on the right:
    text(correlation, y = y1, x =x1, xpd = NA)
    text(B, y = y2, x =x2, xpd = NA)
    text(beta, y = y3, x =x3, xpd = NA)
    text(cor_text, y = y11, x =x11, xpd = NA)
    text(B_text, y = y22, x =x22, xpd = NA)
    text(beta_text, y = y33, x =x33, xpd = NA)
    })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

