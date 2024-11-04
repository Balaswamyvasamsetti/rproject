# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)
library(pROC)

# Load the dataset
heart_data <- read.csv("C:\\lab\\Rlab\\heart.csv")

# Convert categorical variables to factors
heart_data <- heart_data %>%
  mutate(target = factor(target),
         sex = factor(sex),
         cp = factor(cp),
         fbs = factor(fbs),
         restecg = factor(restecg),
         slope = factor(slope),
         ca = factor(ca),
         thal = factor(thal),
         exang = factor(exang))  

# Define UI
ui <- fluidPage(
  titlePanel("Heart Disease Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "Age:", value = 50, min = 0),
      selectInput("sex", "Sex:", choices = c("Male" = 1, "Female" = 0)),
      selectInput("cp", "Chest Pain Type:", choices = 0:3),
      numericInput("trestbps", "Resting Blood Pressure:", value = 120),
      numericInput("chol", "Cholesterol:", value = 200),
      selectInput("fbs", "Fasting Blood Sugar > 120 mg/dl:", choices = c("No" = 0, "Yes" = 1)),
      numericInput("thalach", "Maximum Heart Rate Achieved:", value = 150),
      selectInput("exang", "Exercise Induced Angina:", choices = c("No" = 0, "Yes" = 1)),
      numericInput("oldpeak", "Old Peak:", value = 1.0),
      selectInput("slope", "Slope of the Peak Exercise ST Segment:", choices = 0:2),
      selectInput("ca", "Number of Major Vessels (0-4):", choices = 0:4),
      selectInput("thal", "Thalassemia:", choices = c("Normal" = 1, "Fixed Defect" = 2, "Reversible Defect" = 3)),
      actionButton("predict", "Predict Heart Disease")
    ),
    mainPanel(
      verbatimTextOutput("result"),
      plotOutput("histogram")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Create a logistic regression model
  model <- glm(target ~ age + sex + cp + trestbps + chol + fbs + thalach + exang + oldpeak + slope + ca + thal, 
               data = heart_data, 
               family = binomial)
  
  # Prediction logic
  observeEvent(input$predict, {
    new_data <- data.frame(
      age = input$age,
      sex = factor(input$sex), 
      cp = as.factor(input$cp),
      trestbps = input$trestbps,
      chol = input$chol,
      fbs = factor(input$fbs),
      thalach = input$thalach,
      exang = factor(input$exang), 
      oldpeak = input$oldpeak,
      slope = factor(input$slope), 
      ca = factor(input$ca), 
      thal = factor(input$thal)
    )
    
    # Predict the probability
    prediction <- predict(model, newdata = new_data, type = "response")
    
    # Output result
    output$result <- renderPrint({
      if (prediction > 0.5) {
        "Heart disease predicted: Yes"
      } else {
        "Heart disease predicted: No"
      }
    })
  })
  
  # Enhanced Histogram of Age based on Heart Disease Prediction
  output$histogram <- renderPlot({
    ggplot(heart_data, aes(x = age, fill = target)) +
      geom_histogram(binwidth = 5, position = "dodge", color = 'black', alpha = 0.8) +
      labs(
        title = 'Age Distribution by Heart Disease Status',
        x = 'Age (years)',
        y = 'Frequency'
      ) +
      scale_fill_manual(
        values = c("0" = "#4CAF50", "1" = "#F44336"), 
        name = "Heart Disease Status",
        labels = c("0" = "No Disease", "1" = "Disease")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "top",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        panel.grid.major = element_line(color = "grey85"),
        panel.grid.minor = element_blank()
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
