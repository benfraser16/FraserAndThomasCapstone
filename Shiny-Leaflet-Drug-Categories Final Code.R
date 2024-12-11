# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(caret)
library(scales)
library(stringr)  
library(shiny)
library(leaflet)
library(plotly)
library(tidyr)


#DRUG GROUPS CODE

# Load dataset
project <- read_excel("/Users/benfraser/Desktop/Final Dataset.xlsx")

# Define drug categories
drug_categories <- list(
  antibiotics = c("AMOXICILLIN", "AMOXICILLIN/POTASSIUM CLAV", "AZITHROMYCIN", "BACITRACIN", 
                  "CEFDINIR", "CEFPODOXIME PROXETIL", "CEFTRIAXONE SODIUM", "CEFUROXIME AXETIL", 
                  "CEPHALEXIN", "CIPROFLOXACIN HCL", "CLINDAMYCIN HCL", "CLINDAMYCIN PHOSPHATE"),
  
  analgesics = c("ACETAMINOPHEN WITH CODEINE", "BUPRENORPHINE HCL/NALOXONE HCL", 
                 "BUTALB/ACETAMINOPHEN/CAFFEINE", "FENTANYL", "HYDROCODONE/ACETAMINOPHEN"),
  
  cardiovascular = c("AMIODARONE HCL", "AMLODIPINE BESYLATE", "AMLODIPINE BESYLATE/BENAZEPRIL",
                     "AMLODIPINE/VALSARTAN/HCTHIAZID", "APIXABAN", "ATENOLOL"),
  
  endocrine_metabolic = c("ACARBOSE", "ANASTROZOLE", "CANAGLIFLOZIN", "DEXAMETHASONE", 
                          "EXEMESTANE", "EXENATIDE", "GLIMEPIRIDE"),
  
  psychiatric_neurological = c("ALPRAZOLAM", "ARIPIPRAZOLE", "BUPROPION HCL", "BUSPIRONE HCL",
                               "CARBAMAZEPINE", "CARBIDOPA/LEVODOPA"),
  
  anti_inflammatory_immunosuppressants = c("ADALIMUMAB", "AZATHIOPRINE", "BECLOMETHASONE DIPROPIONATE",
                                           "BETAMETHASONE DIPROPIONATE"),
  
  gastrointestinal_renal = c("CALCIUM ACETATE", "DICYCLOMINE HCL", "FAMOTIDINE", "LACTULOSE",
                             "LANSOPRAZOLE", "LINACLOTIDE"),
  
  other_therapeutic_agents = c("0.9 % SODIUM CHLORIDE", "ACETAZOLAMIDE", "ACYCLOVIR",
                               "ALENDRONATE SODIUM", "ALCOHOL ANTISEPTIC PADS")
)

# First, create dummy variables for drug categories
dataset_clean <- project %>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>%  # Replace NAs with 0
  mutate(across(where(is.character), as.factor))  # Convert character columns to factors

# Add drug category columns
for (category in names(drug_categories)) {
  dataset_clean[[category]] <- as.integer(dataset_clean$generic_name %in% drug_categories[[category]])
}

# Calculate IQR for total_drug_cost
Q1 <- quantile(dataset_clean$total_drug_cost, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset_clean$total_drug_cost, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filter data within IQR bounds
dataset_filtered <- dataset_clean %>%
  filter(total_drug_cost >= lower_bound & total_drug_cost <= upper_bound)

# Calculate average costs by drug category with confidence intervals
average_costs <- dataset_filtered %>%
  select(total_drug_cost, all_of(names(drug_categories))) %>%
  pivot_longer(cols = -total_drug_cost, 
               names_to = "drug_category", 
               values_to = "is_category") %>%
  filter(is_category == 1) %>%
  group_by(drug_category) %>%
  summarise(
    mean_cost = mean(total_drug_cost, na.rm = TRUE),
    se = sd(total_drug_cost, na.rm = TRUE) / sqrt(n()),
    n = n(),
    ci_lower = mean_cost - 1.96 * se,
    ci_upper = mean_cost + 1.96 * se
  ) %>%
  mutate(
    # Using base R functions instead of stringr
    drug_category = gsub("_", " ", drug_category),
    drug_category = tools::toTitleCase(tolower(drug_category))
  )

# Create enhanced bar plot
ggplot(average_costs, aes(x = reorder(drug_category, mean_cost), y = mean_cost)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, 
                color = "darkred") +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Average Drug Cost by Category",
    subtitle = "After removing outliers (using 1.5 × IQR method)",
    x = NULL,
    y = "Average Cost",
    caption = "Error bars represent 95% confidence intervals"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray50"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Prepare filtered data for modeling
model_data <- dataset_filtered %>%
  select(
    total_drug_cost,
    all_of(names(drug_categories)),
    bene_count, total_claim_count, total_30_day_fill_count,
    total_day_supply, pills_per_day, claim_per_bene,
    avg_days_per_claim
  ) %>%
  na.omit()

# Split the filtered data
set.seed(123)
train_indices <- createDataPartition(model_data$total_drug_cost, p = 0.8, list = FALSE)
train_data <- model_data[train_indices, ]
test_data <- model_data[-train_indices, ]

# Fit model on filtered data
predictors <- setdiff(names(model_data), "total_drug_cost")
formula <- as.formula(paste("total_drug_cost ~", paste(predictors, collapse = " + ")))
filtered_model <- lm(formula, data = train_data)

# Calculate performance metrics
predictions <- predict(filtered_model, newdata = test_data)
metrics <- list(
  RMSE = sqrt(mean((predictions - test_data$total_drug_cost)^2)),
  MAE = mean(abs(predictions - test_data$total_drug_cost)),
  R_squared = summary(filtered_model)$r.squared
)

# Print model summary and metrics
print("Model Performance Metrics:")
print(metrics)

#LEAFLET CODE

# Load required libraries
library(leaflet)
library(dplyr)
library(stringr)

# Create data frame with state data
state_data <- data.frame(
  state = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "IA", "ID", "IL", 
            "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", 
            "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", 
            "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"),
  avg_drug_cost = c(1504.16, 5094.461, 5737.579, 6370.168, 8385.636, 4234.785, 7381.336, 2677.041,
                    8478.23, 17238.6, 9774.717, 5590.245, 1500.138, 4699.506, 6491.3, 3187.577,
                    3818.616, 12289.62, 5218.75, 4394.433, 5229.035, 17359.13, 4712.763, 6231.884,
                    6012.982, 2546.731, 7113.11, 1036.24, 3825.525, 5140.572, 6225.136, 4245.208,
                    4093.959, 6816.221, 6298.148, 4536.021, 17656.55, 4368.63, 5663.13, 4234.956,
                    7702.232, 4379.678, 8715.895, 3134.231, 3116.597, 4687.758, 4408.598, 7744.218,
                    5073.858, 756.752),
  avg_claim_count = c(143.6667, 139.5225, 124.5571, 100.69, 121.2629, 89.73333, 112.0364, 95.7,
                      142.4385, 138.9207, 101.5, 155.5116, 102.2222, 137.4379, 129.2581, 128.9512,
                      139.1616, 178.2336, 119.1456, 86.78689, 79.5, 139.4382, 113.7246, 146.0458,
                      138.4844, 96.58824, 139.5206, 129.2, 159.2308, 84.10526, 107.8496, 99.18519,
                      105.8, 133.1703, 142.1278, 181.5179, 115.8824, 131.2259, 115.697, 127.41,
                      203.8947, 171.5625, 124.2687, 93.48148, 122.8814, 102.6, 103.9067, 128.0968,
                      137.1364, 85)
)

# Add state coordinates
state_coords <- data.frame(
  state = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "IA", "ID", "IL", 
            "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", 
            "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", 
            "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"),
  lat = c(61.3850, 32.7990, 34.9697, 34.0489, 36.7783, 39.5501, 41.6032, 38.9108, 27.6648, 32.1656, 
          21.0943, 41.8780, 44.0682, 40.6331, 39.8494, 38.5266, 37.6681, 31.1695, 42.4072, 39.0458, 
          44.6939, 44.3148, 46.7296, 38.5737, 32.7546, 46.8797, 35.7596, 47.5515, 41.4925, 43.1939, 
          40.0583, 34.5199, 38.8026, 42.9538, 40.4173, 35.0078, 44.0593, 40.9046, 41.5801, 33.8361, 
          44.2998, 35.7478, 31.9686, 39.3210, 37.7693, 44.5588, 47.7511, 44.7844, 38.5976, 42.7475),
  lng = c(-152.2683, -86.8073, -92.3731, -111.0937, -119.4179, -105.7821, -73.0877, -75.5277, 
          -81.5158, -82.9001, -157.4983, -93.0977, -114.7420, -89.3985, -86.2583, -96.7265, 
          -84.6701, -91.8318, -71.3824, -76.6413, -69.3819, -85.6024, -94.6859, -92.6044, 
          -89.6787, -110.3626, -79.0193, -100.4472, -99.9018, -71.5724, -74.4057, -105.8701, 
          -116.4194, -75.5278, -82.9071, -97.0929, -120.5137, -77.8280, -71.4774, -80.9450, 
          -100.3363, -86.7072, -99.9018, -111.8910, -78.1700, -72.5778, -120.7401, -89.8879, 
          -80.4549, -107.2085)
)

# Merge coordinates with state data
state_data <- merge(state_data, state_coords, by = "state")

# Create the map with separate circles
map1 <- leaflet(state_data) %>%
  addTiles() %>%
  setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
  # Add circles for drug costs
  addCircleMarkers(
    lng = ~lng,
    lat = ~lat,
    radius = 10,
    color = "red",
    fillColor = ~colorNumeric("YlOrRd", domain = avg_drug_cost)(avg_drug_cost),
    fillOpacity = 0.7,
    weight = 1,
    group = "Drug Costs",
    popup = ~paste(
      "<strong>", state, "</strong><br>",
      "Average Drug Cost: $", format(round(avg_drug_cost, 2), big.mark=",")
    )
  ) %>%
  # Add circles for claim counts
  addCircleMarkers(
    lng = ~lng + 0.5, # Slightly offset the longitude to avoid overlap
    lat = ~lat,
    radius = 10,
    color = "blue",
    fillColor = ~colorNumeric("Blues", domain = avg_claim_count)(avg_claim_count),
    fillOpacity = 0.7,
    weight = 1,
    group = "Claim Counts",
    popup = ~paste(
      "<strong>", state, "</strong><br>",
      "Average Claim Count: ", round(avg_claim_count, 1)
    )
  ) %>%
  # Add legends
  addLegend(
    "bottomright",
    pal = colorNumeric("YlOrRd", domain = state_data$avg_drug_cost),
    values = ~avg_drug_cost,
    title = "Average Drug Cost ($)",
    opacity = 0.7
  ) %>%
  addLegend(
    "bottomleft",
    pal = colorNumeric("Blues", domain = state_data$avg_claim_count),
    values = ~avg_claim_count,
    title = "Average Claim Count",
    opacity = 0.7
  ) %>%
  # Add layer control
  addLayersControl(
    overlayGroups = c("Drug Costs", "Claim Counts"),
    options = layersControlOptions(collapsed = FALSE)
  )

map1

#SHINY CODE

# Load Data
file_path <- "C:/Users/DellLPTP/Documents/410 Final Dataset.xlsx"
if (!file.exists(file_path)) stop("Data file not found.")
project <- read_excel(file_path)

# Data Cleaning
numeric_columns <- c("bene_count", "total_claim_count", "total_day_supply", 
                     "pills_per_day", "claim_per_bene", "total_drug_cost")

dataset_clean <- project %>%
  select(all_of(numeric_columns)) %>%
  drop_na()

# Outlier Removal
remove_outliers_iqr <- function(df, cols) {
  df %>% filter(
    if_all(all_of(cols), ~ between(., 
                                   quantile(., 0.25, na.rm = TRUE) - 1.5 * IQR(.),
                                   quantile(., 0.75, na.rm = TRUE) + 1.5 * IQR(.)
    ))
  )
}

dataset_clean <- remove_outliers_iqr(dataset_clean, numeric_columns)

# Model Training
set.seed(123)
train_indices <- createDataPartition(dataset_clean$total_drug_cost, p = 0.8, list = FALSE)
train_data <- dataset_clean[train_indices, ]
test_data <- dataset_clean[-train_indices, ]

# Linear Model
formula <- as.formula(paste("total_drug_cost ~", paste(numeric_columns[-6], collapse = " + ")))
log_odds_model <- lm(formula, data = train_data)

# Shiny App
ui <- fluidPage(
  titlePanel("Healthcare Analytics Dashboard"),
  tabsetPanel(
    # Prediction Tab
    tabPanel("Cost Prediction",
             sidebarLayout(
               sidebarPanel(
                 numericInput("bene_count", "Beneficiary Count:", 1000),
                 numericInput("claim_count", "Total Claim Count:", 100),
                 numericInput("day_supply", "Total Day Supply:", 30),
                 numericInput("pills_per_day", "Pills per Day:", 2),
                 numericInput("claim_per_bene", "Claim per Beneficiary:", 5),
                 actionButton("predict", "Predict Cost")
               ),
               mainPanel(
                 h3("Prediction Results"),
                 verbatimTextOutput("predictionOutput"),
                 plotlyOutput("predictionBar")
               )
             )
    ),
    
    # EDA Tab
    tabPanel("Exploratory Data Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("x_var", "X-Axis Variable:", 
                             choices = numeric_columns[-6], 
                             selected = "bene_count"),
                 checkboxInput("show_trend", "Show Trend Line", TRUE)
               ),
               mainPanel(
                 plotlyOutput("edaPlot", height = "500px"),
                 verbatimTextOutput("edaSummary")
               )
             )
    ),
    
    # State Analysis Tab
    tabPanel("State Analysis",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("metrics", "Select Metrics to Display:",
                                    choices = c("Drug Costs" = "drug_costs",
                                                "Claim Counts" = "claim_counts"),
                                    selected = c("drug_costs", "claim_counts")),
                 hr(),
                 sliderInput("cost_range", "Filter by Drug Cost:",
                             min = 0, max = 20000,
                             value = c(0, 20000)),
                 style = "background-color: #f5f5f5;"
               ),
               mainPanel(
                 leafletOutput("stateMap", height = "600px"),
                 div(style = "margin-top: 20px;", textOutput("stateAnalysisSummary"))
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$predict, {
    new_data <- data.frame(
      bene_count = input$bene_count,
      total_claim_count = input$claim_count,
      total_day_supply = input$day_supply,
      pills_per_day = input$pills_per_day,
      claim_per_bene = input$claim_per_bene
    )
    
    predicted_cost <- predict(log_odds_model, new_data)
    
    output$predictionOutput <- renderText({
      paste("Predicted Total Drug Cost: $", format(round(predicted_cost, 2), big.mark=","))
    })
    
    output$predictionBar <- renderPlotly({
      avg_cost <- 7131.24
      
      plot_ly(
        x = c("Average Drug Cost", "Predicted Cost"),
        y = c(avg_cost, predicted_cost),
        type = "bar",
        marker = list(color = c("steelblue", "orange"))
      ) %>% layout(
        title = "Cost Comparison",
        yaxis = list(title = "Total Cost ($)")
      )
    })
  })
  
  output$edaPlot <- renderPlotly({
    p <- ggplot(dataset_clean, aes_string(x = input$x_var, y = "total_drug_cost")) +
      geom_point(alpha = 0.6) +
      theme_minimal() +
      labs(title = paste("Drug Cost vs", input$x_var))
    
    if (input$show_trend) {
      p <- p + geom_smooth(method = "lm", se = TRUE)
    }
    ggplotly(p)
  })
  
  output$edaSummary <- renderPrint({
    summary_stats <- summary(dataset_clean[[input$x_var]])
    correlation <- cor(dataset_clean[[input$x_var]], dataset_clean$total_drug_cost, use = "complete.obs")
    cat("Correlation Coefficient:", round(correlation, 3), "\n\n")
    cat("Summary Statistics for", input$x_var, ":\n")
    print(summary_stats)
  })
  
  # State Map
  output$stateMap <- renderLeaflet({
    leaflet(state_data) %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addLayersControl(
        overlayGroups = c("Drug Costs", "Claim Counts"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observe({
    proxy <- leafletProxy("stateMap", data = state_data)
    proxy %>% clearMarkers()
    
    if ("drug_costs" %in% input$metrics) {
      filtered_data <- state_data %>%
        filter(avg_drug_cost >= input$cost_range[1],
               avg_drug_cost <= input$cost_range[2])
      
      proxy %>% addCircleMarkers(
        data = filtered_data,
        lng = ~lng,
        lat = ~lat,
        radius = 10,
        color = "red",
        fillColor = ~colorNumeric("YlOrRd", domain = avg_drug_cost)(avg_drug_cost),
        fillOpacity = 0.7,
        group = "Drug Costs",
        popup = ~paste(
          "<strong>", state, "</strong><br>",
          "Average Drug Cost: $", format(round(avg_drug_cost, 2), big.mark=",")
        )
      )
    }
    
    if ("claim_counts" %in% input$metrics) {
      proxy %>% addCircleMarkers(
        lng = ~lng + 0.5,
        lat = ~lat,
        radius = 10,
        color = "blue",
        fillColor = ~colorNumeric("Blues", domain = avg_claim_count)(avg_claim_count),
        fillOpacity = 0.7,
        group = "Claim Counts",
        popup = ~paste(
          "<strong>", state, "</strong><br>",
          "Average Claim Count: ", round(avg_claim_count, 1)
        )
      )
    }
  })
}

shinyApp(ui = ui, server = server)
