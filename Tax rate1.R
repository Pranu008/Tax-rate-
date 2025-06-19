# ui.R (User Interface)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr) # For pivot_longer
library(RColorBrewer) # For color palettes

# --- Data Definition ---

# Data for corporate tax rates (simplified and approximate for illustration)
# Source: Based on information from PwC, ClearTax, IndiaFilings, InvestIndia, and PRS India search results.
# Note: Effective rates include applicable surcharge (assuming highest slab for general categories for illustrative purposes) and 4% Health & Education Cess.
# Actual rates can be more nuanced with various income slabs for surcharges and specific exemptions.
tax_data <- data.frame(
  Financial_Year = c(
    rep("2015-16", 3), rep("2016-17", 3), rep("2017-18", 3), rep("2018-19", 3),
    rep("2019-20", 5), rep("2020-21", 5), rep("2021-22", 5), rep("2022-23", 5),
    rep("2023-24", 5), rep("2024-25", 5)
  ),
  Assessment_Year = c(
    rep("2016-17", 3), rep("2017-18", 3), rep("2018-19", 3), rep("2019-20", 3),
    rep("2020-21", 5), rep("2021-22", 5), rep("2022-23", 5), rep("2023-24", 5),
    rep("2024-25", 5), rep("2025-26", 5)
  ),
  Type = c(
    "Domestic (General)", "Domestic (Small Turnover)", "Foreign",
    "Domestic (General)", "Domestic (Small Turnover)", "Foreign",
    "Domestic (General)", "Domestic (Small Turnover)", "Foreign",
    "Domestic (General)", "Domestic (Small Turnover)", "Foreign",
    "Domestic (Old Regime >400Cr)", "Domestic (Old Regime <=400Cr)", "Domestic (Sec 115BAA)", "Domestic (Sec 115BAB)", "Foreign",
    "Domestic (Old Regime >400Cr)", "Domestic (Old Regime <=400Cr)", "Domestic (Sec 115BAA)", "Domestic (Sec 115BAB)", "Foreign",
    "Domestic (Old Regime >400Cr)", "Domestic (Old Regime <=400Cr)", "Domestic (Sec 115BAA)", "Domestic (Sec 115BAB)", "Foreign",
    "Domestic (Old Regime >400Cr)", "Domestic (Old Regime <=400Cr)", "Domestic (Sec 115BAA)", "Domestic (Sec 115BAB)", "Foreign",
    "Domestic (Old Regime >400Cr)", "Domestic (Old Regime <=400Cr)", "Domestic (Sec 115BAA)", "Domestic (Sec 115BAB)", "Foreign",
    "Domestic (Old Regime >400Cr)", "Domestic (Old Regime <=400Cr)", "Domestic (Sec 115BAA)", "Domestic (Sec 115BAB)", "Foreign"
  ),
  Basic_Rate = c(
    30, 29, 40,
    30, 25, 40, # FY 2016-17, small turnover threshold changed
    30, 25, 40, # FY 2017-18, small turnover threshold changed
    30, 25, 40,
    30, 25, 22, 15, 40, # FY 2019-20, new regimes introduced
    30, 25, 22, 15, 40,
    30, 25, 22, 15, 40,
    30, 25, 22, 15, 40,
    30, 25, 22, 15, 40,
    30, 25, 22, 15, 35 # FY 2024-25, assuming foreign rate reduced to 35% as per budget mentions
  ),
  Effective_Rate = c(
    34.94, 32.76, 43.68, # Assuming highest surcharge and 4% cess for general categories
    34.94, 29.12, 43.68,
    34.94, 29.12, 43.68,
    34.94, 29.12, 43.68,
    34.94, 29.12, 25.17, 17.16, 43.68, # Post-2019 cuts
    34.94, 29.12, 25.17, 17.16, 43.68,
    34.94, 29.12, 25.17, 17.16, 43.68,
    34.94, 29.12, 25.17, 17.16, 43.68,
    34.94, 29.12, 25.17, 17.16, 43.68,
    34.94, 29.12, 25.17, 17.16, 38.08 # Foreign rate 35% + 5% surcharge + 4% cess = ~38.08%
  )
)
tax_data$Financial_Year_Numeric <- as.numeric(substr(tax_data$Financial_Year, 1, 4))


# Data for impact on corporate business (Illustrative based on search results)
# These percentages represent growth or change, not absolute values.
corporate_impact_data <- data.frame(
  Financial_Year = c(
    "2014-15", "2015-16", "2016-17", "2017-18", "2018-19",
    "2019-20", "2020-21", "2021-22", "2022-23", "2023-24", "2024-25"
  ),
  Financial_Year_Numeric = c(
    2014, 2015, 2016, 2017, 2018,
    2019, 2020, 2021, 2022, 2023, 2024
  ),
  Corporate_Tax_Collection_Growth_Percent = c(
    NA, 8, 16, 16, 8, # Approx growth, based on general trends before 2019 cuts
    -16, -18, 55, 18, 20, 10 # Post-2019 figures based on search (e.g., 55% jump FY21-22, 20% in FY24)
  ),
  Corporate_Profitability_Growth_Percent = c(
    NA, 5, 7, 9, 12, # Pre-2019, moderate growth
    20, 15, 30, 25, 15.3, 10 # Post-2019 jumps mentioned (e.g., 244% over several years, 15.3% in FY24)
  )
)

# Reshape corporate_impact_data for easier plotting with plotly
corporate_impact_data_long <- corporate_impact_data %>%
  pivot_longer(
    cols = c(Corporate_Tax_Collection_Growth_Percent, Corporate_Profitability_Growth_Percent),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = case_when(
      Metric == "Corporate_Tax_Collection_Growth_Percent" ~ "Corporate Tax Collection Growth (%)",
      Metric == "Corporate_Profitability_Growth_Percent" ~ "Corporate Profitability Growth (%)"
    )
  )

# Data for broader economic impact (Illustrative based on search results)
# GDP Growth Source: Macrotrends, ClearTax, TradingEconomics
# FDI Inflows Source: Macrotrends, PIB, Times of India
# Unemployment Rate Source: YCharts, Forbes India, Macrotrends
economy_data <- data.frame(
  Financial_Year = c(
    "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20",
    "2020-21", "2021-22", "2022-23", "2023-24", "2024-25"
  ),
  Financial_Year_Numeric = c(
    2014, 2015, 2016, 2017, 2018, 2019,
    2020, 2021, 2022, 2023, 2024
  ),
  GDP_Growth_Percent = c(
    7.41, 8.00, 8.26, 6.80, 6.45, 3.87, # Pre-COVID trends
    -5.78, 9.69, 6.99, 8.15, 8.2 # COVID impact and recovery, 2024-25 is projection
  ),
  FDI_Inflows_Billion_USD = c(
    34.58, 44.01, 44.46, 39.97, 42.12, # Pre-2019 cuts
    50.61, 64.36, 44.73, 49.94, 28.08, 0.353 # Post-2019, showing fluctuations and recent sharp decline in net FDI in FY25
  ),
  Unemployment_Rate_Percent = c(
    7.67, 7.63, 7.60, 7.62, 7.65, 6.51, # Pre-COVID, 2019 drop
    7.86, 6.38, 4.82, 4.17, 4.20 # COVID peak, subsequent decline/stabilization
  )
)

# Reshape economy_data for easier plotting with plotly
economy_data_long <- economy_data %>%
  pivot_longer(
    cols = c(GDP_Growth_Percent, FDI_Inflows_Billion_USD, Unemployment_Rate_Percent),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = case_when(
      Metric == "GDP_Growth_Percent" ~ "GDP Growth (%)",
      Metric == "FDI_Inflows_Billion_USD" ~ "FDI Inflows (Billion USD)",
      Metric == "Unemployment_Rate_Percent" ~ "Unemployment Rate (%)"
    )
  )

# Define UI for application using shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "India Corporate Tax & Economic Impact Dashboard", titleWidth = 350),
  dashboardSidebar(
    width = 350, # Wider sidebar for better display of controls
    # Custom CSS for sidebar to ensure Inter font and a cleaner look
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600&display=swap');
        body { font-family: 'Inter', sans-serif; }
        .main-sidebar, .sidebar-menu li a {
          font-family: 'Inter', sans-serif;
        }
        .main-header .logo {
          font-family: 'Inter', sans-serif;
          font-weight: 600;
          font-size: 20px; /* Adjust as needed for title fit */
        }
        .skin-blue .main-header .navbar .sidebar-toggle:hover {
            background-color: #3182ce;
        }
        .skin-blue .main-header .logo {
            background-color: #4299e1;
        }
        .skin-blue .main-header .logo:hover {
            background-color: #3182ce;
        }
        .skin-blue .main-header .navbar {
            background-color: #4299e1;
        }
        .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
            background-color: #2d3748; /* Darker sidebar background */
            color: #e2e8f0;
        }
        .skin-blue .sidebar a {
            color: #e2e8f0;
        }
        .skin-blue .sidebar a:hover {
            background-color: #4a5568; /* Hover color for sidebar items */
        }
        .irs-bar, .irs-bar-edge, .irs-from, .irs-to, .irs-single {
          background: #4299e1 !important;
          border-color: #4299e1 !important;
        }
        .irs-max, .irs-min, .irs-grid-text {
          color: #e2e8f0 !important; /* Lighter text for slider labels */
        }
        .control-label {
          color: #e2e8f0 !important; /* Lighter text for control labels */
          font-weight: 600;
        }
        .checkbox-inline, .checkbox {
          color: #e2e8f0 !important;
        }
        .radio input[type=\"radio\"]:checked + label::before {
          background-color: #4299e1 !important;
          border-color: #4299e1 !important;
        }
        .radio input[type=\"radio\"]:checked + label::after {
          background-color: #fff !important;
        }
        .box {
          border-radius: 12px;
          box-shadow: 0 4px 20px rgba(0, 0, 0, 0.1);
        }
        .plotly .modebar {
          background-color: transparent !important;
        }
        .disclaimer {
            font-size: 0.85em;
            color: #666;
            margin-top: 10px;
            padding: 10px;
            background-color: #fff3cd;
            border-left: 5px solid #ffc107;
            border-radius: 5px;
        }
      "))
    ),
    sidebarMenu(
      id = "tabs", # Add an ID to the sidebarMenu for conditional panels
      menuItem("Tax Rates", tabName = "tax_rates", icon = icon("chart-line")),
      menuItem("Corporate Impact", tabName = "corporate_impact", icon = icon("building")),
      menuItem("Economic Impact", tabName = "economy_impact", icon = icon("globe-asia")),
      
      # Filters for Tax Rates tab
      conditionalPanel(
        condition = "input.tabs == 'tax_rates'",
        checkboxGroupInput(
          inputId = "taxType",
          label = "Select Type(s) of Corporate Tax:",
          choices = unique(tax_data$Type),
          selected = unique(tax_data$Type) # Select all by default
        ),
        radioButtons(
          inputId = "graphTypeRates", # Specific ID for this tab's graph type
          label = "Select Graph Type:",
          choices = c("Line Graph", "Bar Graph"),
          selected = "Line Graph"
        ),
        sliderInput(
          inputId = "yearRangeRates",
          label = "Select Financial Year Range:",
          min = min(tax_data$Financial_Year_Numeric),
          max = max(tax_data$Financial_Year_Numeric),
          value = c(min(tax_data$Financial_Year_Numeric), max(tax_data$Financial_Year_Numeric)),
          step = 1,
          sep = ""
        )
      ),
      
      # Filters for Corporate Impact tab
      conditionalPanel(
        condition = "input.tabs == 'corporate_impact'",
        checkboxGroupInput(
          inputId = "corporateImpactMetric", # Specific ID for this tab's metric
          label = "Select Metric(s):",
          choices = unique(corporate_impact_data_long$Metric),
          selected = unique(corporate_impact_data_long$Metric)
        ),
        radioButtons(
          inputId = "graphTypeCorporate", # Specific ID for this tab's graph type
          label = "Select Graph Type:",
          choices = c("Line Graph", "Bar Graph"),
          selected = "Line Graph"
        ),
        sliderInput(
          inputId = "yearRangeCorporate",
          label = "Select Financial Year Range:",
          min = min(corporate_impact_data$Financial_Year_Numeric, na.rm = TRUE),
          max = max(corporate_impact_data$Financial_Year_Numeric, na.rm = TRUE),
          value = c(min(corporate_impact_data$Financial_Year_Numeric, na.rm = TRUE), max(corporate_impact_data$Financial_Year_Numeric, na.rm = TRUE)),
          step = 1,
          sep = ""
        )
      ),
      
      # Filters for Economic Impact tab
      conditionalPanel(
        condition = "input.tabs == 'economy_impact'",
        checkboxGroupInput(
          inputId = "economyImpactMetric", # Specific ID for this tab's metric
          label = "Select Metric(s):",
          choices = unique(economy_data_long$Metric),
          selected = unique(economy_data_long$Metric)
        ),
        radioButtons(
          inputId = "graphTypeEconomy", # Specific ID for this tab's graph type
          label = "Select Graph Type:",
          choices = c("Line Graph", "Bar Graph"),
          selected = "Line Graph"
        ),
        sliderInput(
          inputId = "yearRangeEconomy",
          label = "Select Financial Year Range:",
          min = min(economy_data$Financial_Year_Numeric, na.rm = TRUE),
          max = max(economy_data$Financial_Year_Numeric, na.rm = TRUE),
          value = c(min(economy_data$Financial_Year_Numeric, na.rm = TRUE), max(economy_data$Financial_Year_Numeric, na.rm = TRUE)),
          step = 1,
          sep = ""
        )
      )
    )
  ),
  dashboardBody(
    # Body custom CSS for background
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f0f4f8; /* Lighter background for the main content area */
        }
      "))
    ),
    tabItems(
      # Tax Rates Tab Content
      tabItem(tabName = "tax_rates",
              fluidRow(
                box(
                  title = "Indian Corporate Tax Rates Over Time",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("taxPlot", height = "550px"),
                  div(class = "disclaimer",
                      "Note: Effective rates include applicable surcharge (assuming highest slab for general categories) and 4% Health & Education Cess. Actual rates can vary based on specific income slabs and exemptions. Data is approximate and for illustrative purposes based on publicly available information."
                  )
                )
              )
      ),
      
      # Corporate Impact Tab Content
      tabItem(tabName = "corporate_impact",
              fluidRow(
                box(
                  title = "Impact on Corporate Business (Illustrative Annual Growth)",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("corporateImpactPlot", height = "550px"),
                  div(class = "disclaimer",
                      "Note: This data represents illustrative annual growth percentages for Corporate Tax Collection and Corporate Profitability. These are based on general trends and reported impacts from various sources and should be considered as indicative, not precise, official statistics."
                  )
                )
              )
      ),
      
      # Economic Impact Tab Content
      tabItem(tabName = "economy_impact",
              fluidRow(
                box(
                  title = "Impact on Indian Economy (Illustrative Macroeconomic Trends)",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("economyImpactPlot", height = "550px"),
                  div(class = "disclaimer",
                      "Note: This data on GDP Growth, FDI Inflows, and Unemployment Rate is illustrative and compiled from publicly available sources (e.g., Macrotrends, YCharts, Forbes, PIB). Economic impacts are complex and influenced by numerous factors beyond corporate tax policy."
                  )
                )
              )
      )
    )
  )
)

# server.R (Server Logic)
server <- function(input, output, session) {
  
  # Reactive expression for Tax Rates data based on user inputs
  filtered_tax_data <- reactive({
    req(input$taxType) # Ensure taxType input is not NULL
    
    tax_data %>%
      filter(Type %in% input$taxType) %>%
      filter(Financial_Year_Numeric >= input$yearRangeRates[1] & Financial_Year_Numeric <= input$yearRangeRates[2])
  })
  
  # Render the interactive plot for Tax Rates
  output$taxPlot <- renderPlotly({
    plot_data <- filtered_tax_data()
    
    p <- ggplot(plot_data, aes(x = Financial_Year_Numeric, y = Effective_Rate, color = Type,
                               text = paste(
                                 "Financial Year: ", Financial_Year, "<br>",
                                 "Assessment Year: ", Assessment_Year, "<br>",
                                 "Type: ", Type, "<br>",
                                 "Effective Rate: ", Effective_Rate, "%"
                               ))) +
      labs(x = "Financial Year", y = "Effective Tax Rate (%)", title = "Evolution of Corporate Tax Rates") +
      scale_x_continuous(breaks = unique(tax_data$Financial_Year_Numeric), labels = unique(tax_data$Financial_Year)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = -45, hjust = 0, family = 'Inter'),
            axis.text.y = element_text(family = 'Inter'),
            axis.title = element_text(family = 'Inter', face = "bold"),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold", family = 'Inter'),
            legend.position = "top",
            legend.title = element_blank(),
            text = element_text(family = 'Inter'))
    
    if (input$graphTypeRates == "Line Graph") {
      p <- p + geom_line(lwd = 1) + geom_point(size = 3)
    } else { # Bar Graph
      p <- p + geom_col(aes(fill = Type), position = "dodge") +
        scale_fill_manual(values = brewer.pal(length(unique(plot_data$Type)), "Set2")) # Use a color palette for bars
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(
          title = "Financial Year",
          tickmode = "array",
          tickvals = unique(tax_data$Financial_Year_Numeric),
          ticktext = unique(tax_data$Financial_Year),
          tickangle = -45
        ),
        yaxis = list(title = "Effective Tax Rate (%)"),
        # legend = list(orientation = 'v', x = 0, y = 1.1, font = list(size = 10)),
        hovermode = 'x unified'
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE,
             modeBarButtonsToRemove = list('sendDataToCloud', 'hoverClosestCartesian', 'hoverCompareCartesian'))
  })
  
  # Reactive expression for Corporate Impact data based on user inputs
  filtered_corporate_impact_data <- reactive({
    req(input$corporateImpactMetric) # Ensure corporateImpactMetric input is not NULL
    
    corporate_impact_data_long %>%
      filter(Metric %in% input$corporateImpactMetric) %>%
      filter(Financial_Year_Numeric >= input$yearRangeCorporate[1] & Financial_Year_Numeric <= input$yearRangeCorporate[2])
  })
  
  # Render the interactive plot for Corporate Impact
  output$corporateImpactPlot <- renderPlotly({
    plot_data <- filtered_corporate_impact_data()
    
    p <- ggplot(plot_data, aes(x = Financial_Year_Numeric, y = Value, color = Metric,
                               text = paste(
                                 "Financial Year: ", Financial_Year, "<br>",
                                 "Metric: ", Metric, "<br>",
                                 "Value: ", Value, "%"
                               ))) +
      labs(x = "Financial Year", y = "Annual Growth Rate (%)", title = "Impact Metrics on Corporate Business") +
      scale_x_continuous(breaks = unique(corporate_impact_data$Financial_Year_Numeric), labels = unique(corporate_impact_data$Financial_Year)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = -45, hjust = 0, family = 'Inter'),
            axis.text.y = element_text(family = 'Inter'),
            axis.title = element_text(family = 'Inter', face = "bold"),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold", family = 'Inter'),
            legend.position = "top",
            legend.title = element_blank(),
            text = element_text(family = 'Inter'))
    
    if (input$graphTypeCorporate == "Line Graph") {
      p <- p + geom_line(lwd = 1) + geom_point(size = 3)
    } else { # Bar Graph
      p <- p + geom_col(aes(fill = Metric), position = "dodge") +
        scale_fill_manual(values = c("Corporate Tax Collection Growth (%)" = "#00b2e2", "Corporate Profitability Growth (%)" = "#8cd790")) # Custom colors
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(
          title = "Financial Year",
          tickmode = "array",
          tickvals = unique(corporate_impact_data$Financial_Year_Numeric),
          ticktext = unique(corporate_impact_data$Financial_Year),
          tickangle = -45
        ),
        yaxis = list(title = "Annual Growth Rate (%)"),
        legend = list(orientation = 'h', x = 0, y = 1.1, font = list(size = 10)),
        hovermode = 'x unified'
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE,
             modeBarButtonsToRemove = list('sendDataToCloud', 'hoverClosestCartesian', 'hoverCompareCartesian'))
  })
  
  # Reactive expression for Economic Impact data based on user inputs
  filtered_economy_data <- reactive({
    req(input$economyImpactMetric) # Ensure economyImpactMetric input is not NULL
    
    economy_data_long %>%
      filter(Metric %in% input$economyImpactMetric) %>%
      filter(Financial_Year_Numeric >= input$yearRangeEconomy[1] & Financial_Year_Numeric <= input$yearRangeEconomy[2])
  })
  
  # Render the interactive plot for Economic Impact
  output$economyImpactPlot <- renderPlotly({
    plot_data <- filtered_economy_data()
    
    # Determine y-axis label based on selected metrics
    y_axis_label <- "Value"
    if ("GDP Growth (%)" %in% input$economyImpactMetric && !"FDI Inflows (Billion USD)" %in% input$economyImpactMetric && !"Unemployment Rate (%)" %in% input$economyImpactMetric) {
      y_axis_label <- "GDP Growth (%)"
    } else if (!"GDP Growth (%)" %in% input$economyImpactMetric && "FDI Inflows (Billion USD)" %in% input$economyImpactMetric && !"Unemployment Rate (%)" %in% input$economyImpactMetric) {
      y_axis_label <- "FDI Inflows (Billion USD)"
    } else if (!"GDP Growth (%)" %in% input$economyImpactMetric && !"FDI Inflows (Billion USD)" %in% input$economyImpactMetric && "Unemployment Rate (%)" %in% input$economyImpactMetric) {
      y_axis_label <- "Unemployment Rate (%)"
    } else { # Mixed or all selected
      y_axis_label <- "Value (varies by metric)"
    }
    
    
    p <- ggplot(plot_data, aes(x = Financial_Year_Numeric, y = Value, color = Metric,
                               text = paste(
                                 "Financial Year: ", Financial_Year, "<br>",
                                 "Metric: ", Metric, "<br>",
                                 "Value: ", Value,
                                 ifelse(Metric == "GDP Growth (%)", "%",
                                        ifelse(Metric == "FDI Inflows (Billion USD)", " Billion USD",
                                               ifelse(Metric == "Unemployment Rate (%)", "%", "")))
                               ))) +
      labs(x = "Financial Year", y = y_axis_label, title = "Macroeconomic Trends in India") +
      scale_x_continuous(breaks = unique(economy_data$Financial_Year_Numeric), labels = unique(economy_data$Financial_Year)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = -45, hjust = 0, family = 'Inter'),
            axis.text.y = element_text(family = 'Inter'),
            axis.title = element_text(family = 'Inter', face = "bold"),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold", family = 'Inter'),
            legend.position = "top",
            legend.title = element_blank(),
            text = element_text(family = 'Inter'))
    
    if (input$graphTypeEconomy == "Line Graph") {
      p <- p + geom_line(lwd = 1) + geom_point(size = 3)
    } else { # Bar Graph
      p <- p + geom_col(aes(fill = Metric), position = "dodge") +
        scale_fill_manual(values = brewer.pal(length(unique(plot_data$Metric)), "Dark2")) # Use a different color palette
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(
          title = "Financial Year",
          tickmode = "array",
          tickvals = unique(economy_data$Financial_Year_Numeric),
          ticktext = unique(economy_data$Financial_Year),
          tickangle = -45
        ),
        yaxis = list(title = y_axis_label), # Dynamic y-axis title
        legend = list(orientation = 'h', x = 0, y = 1.1, font = list(size = 10)),
        hovermode = 'x unified'
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE,
             modeBarButtonsToRemove = list('sendDataToCloud', 'hoverClosestCartesian', 'hoverCompareCartesian'))
  })
}

# Run the application
shinyApp(ui = ui, server = server)