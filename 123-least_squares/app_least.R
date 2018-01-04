#least_squares_animation
#setwd('C:/Users/THINK_SUBJECT22/Documents/statistics')
#d <- read.csv('beers_smiles.csv')
#packages:
#install.packages('Rgb')
library('Rgb')
d <- data.frame( x = seq(11)-1,  y= c(1,3,4,3, 5, 6, 8, 8, 9, 8, 7))
colnames(d) = c('beers', 'smiles')
#define the names of the IV and the DV::
IV = colnames(d)[1]
DV = colnames(d)[2]

#define the names of the IV and the DV::
IV = colnames(d)[1]
DV = colnames(d)[2]
#variables:
point_color = rgb(0.2,0,1, 1)
line_color = rgb(1,0.2,0.2, 0.6)
line_width = 3
x1 = 5
y1 = 12
x2 = 5
y2 = 14
text_size = 1.6
value_adjust = 100 
y_lim = c(-5, 15)
  #y_lim = c(0, max(d[DV])+1) #this limit for variable data sets
 
#parameters
par(mfrow = c(1,1), mar = c(1,1,1,1), oma = c(1,10,1,1)) #note: bottom, left, top and right . notes oma = outer margin region, mar = margins for ech specific plot
#simple dot plot:
plot(d, ylim = y_lim, type = 'n', 
     xlab = '#beers', 
     lab = '#smiles')
#the regression line:
abline(lsfit(d[IV], d[DV]), 
       col = line_color, 
       lwd = line_width) #look inside the function + see what the code does
#plot a vertical line from each point up or down: this should connect to the glm line.
intercept = lsfit(d[IV], d[DV])[[1]][1]
B = lsfit(d[IV], d[DV])[[1]][2]
for (i in seq(1:length(d[,1])+1)){
  #value = intercept+(B*(i-1))
  segments(d[IV][i,], d[DV][i,], d[IV][i,],value/value_adjust, 
           col = line_color, 
           lwd = line_width)
}

mean_smiles <- mean(d[[DV]])
#play with the intercept and B
#value = 1 #this has to be the value that goes into the slider
value_intercept = 0
new_intercept = (lsfit(d[IV], d[DV])[[1]][1]) + value_intercept
new_B = (lsfit(d[IV], d[DV])[[1]][2]) + value/value_adjust
old_midpoint = intercept+((length(d[DV][,1])/2)*B)
new_midpoint = new_intercept+((length(d[DV][,1])/2)*new_B)
new_intercept = intercept + (old_midpoint - new_midpoint)

#new regression line + segments to plot:
abline(new_intercept, new_B, 
       col = line_color,
       lwd = line_width)
total_diff_score = c()
for (i in seq(1:length(d[,1])+1)){
  value = new_intercept+(new_B*(i-1))
  segments(d[IV][i,], d[DV][i,], d[IV][i,],value, 
           col = line_color, 
           lwd = line_width)
  difference= (d[DV][i,]**2) - (value**2)
  total_diff_score = c(total_diff_score, difference)
  print (i)
}
total_diff_score_complete = sum(total_diff_score)

#after this change the new_intercept to be the old? make sure the order is correct
intercept = new_intercept
B = new_B
#add the points, do this last to overlap the endpoints of the lines:
points(d, cex = 2, col = point_color, pch = 16)
#add labeling::
text('Y_hat = a + B(xy) * x(i)',
     x = x1, y = y1, 
     xpd = NA,
     cex = text_size)

#code above here is the same as the plot below, just here to test changes
#################shiny ap here:
ui <- fluidPage(
  # App title ----
  titlePanel("Illustration of least squares method"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "B",
                  label = "change the slope of the regression line, each step = 0.01",
                  min = -100,
                  max = 100,
                  value = 0)
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      verbatimTextOutput("summary")
    )
  )
)


#note: bins has become 'B'
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    value = input$B
    #parameters
    par(mfrow = c(1,1), mar = c(1,1,1,1), oma = c(6,1,1,1)) #note: bottom, left, top and right . notes oma = outer margin region, mar = margins for ech specific plot
    #simple dot plot:
    plot(d, type = 'n', 
         xlab = '#beers', 
         ylab = '#smiles', 
         ylim = y_lim,
         xpd = NA)
    
    value_intercept = 0 #for now zero, add second slider to change the intercept
    new_intercept = (lsfit(d[IV], d[DV])[[1]][1]) + value_intercept
    new_B = (lsfit(d[IV], d[DV])[[1]][2]) + value/value_adjust
    old_midpoint = intercept+((length(d[DV][,1])/2)*B)
    new_midpoint = new_intercept+((length(d[DV][,1])/2)*new_B)
    new_intercept = intercept + (old_midpoint - new_midpoint)
    
    #regression line + segments to plot:
    abline(new_intercept, new_B, 
           col = line_color,
           lwd = line_width)
    total_diff_score = c()
    for (i in seq(1:length(d[,1])+1)){
      value = new_intercept+(new_B*(i-1))
      segments(d[IV][i,], d[DV][i,], d[IV][i,],value, 
               col = line_color, 
               lwd = line_width)
      difference= (d[DV][i,]**2) - (value**2)
      total_diff_score = c(total_diff_score, difference)
      print (i)
    }
    total_diff_score_complete = sum(total_diff_score)
    
    #after this change the new_intercept to be the old? make sure the order is correct
    #intercept = new_intercept
    #B = new_B
    #add the points, do this last to overlap the endpoints of the lines:
    points(d, cex = 2, col = point_color, 
           pch = 16, 
           ylim = y_lim
           )
    #add labeling::
    text('Y_hat = a + B(xy) * x(i)',
         x = x1, y = y1, 
         xpd = NA,
         cex = text_size)
    
    #text(total_diff_score_complete,
     #    x = x2, y = y2, 
      #   xpd = NA,
       #  cex = text_size)
    
    })
  output$summary <- renderPrint({
    'the starting point of the slider (zero) is the calculated B value that minimized the total sum of all the vertical line segments'
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)