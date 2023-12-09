library(dplyr)
library(fuzzyjoin)
library(sf)
library(rnaturalearth)
library(leaflet)
library(shiny)
library(plotly)
library(shinydashboard) 
library(DT)
library(shinythemes)
library(RColorBrewer)
################################ load data and preprocessing #################

# 加载世界地图数据
world <- ne_countries(scale = "medium", returnclass = "sf")
world$admin <- trimws(world$admin)

# 将地理坐标系转换为平面坐标系 EPSG:3857（用于面积计算）
world_area <- st_transform(world, crs = 3857)

# 计算地面积（单位：平方米），并转换为平方公里
world_area <- world_area %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6)  # 转换为平方公里

# 提取大洲数据，为tab2做准备
world_continent <- world %>%
  select(admin, continent,-geometry)

# load production data
product_data <- read.csv('production.csv', header = TRUE)

# 将 Item 列转换为小写
product_data$Item <- tolower(product_data$Item)

# 定义 fruit 类型列表
fruit_types <- c(
  "apples", "avocados", "bananas", "cherries", "cocoa beans",
  "oil palm fruit", "other beans, green", "other fruits"
)

# clean data to match different fruit type
fruit_data <- product_data %>%
  filter(grepl("fruit|apple|banana|cherry|cocoa", Item)) %>%
  mutate(Item = case_when(
    grepl("apples", Item, ignore.case = TRUE) ~ "apples",
    grepl("avocados", Item, ignore.case = TRUE) ~ "avocados",
    grepl("bananas", Item, ignore.case = TRUE) ~ "bananas",
    grepl("cherries", Item, ignore.case = TRUE) ~ "cherries",
    grepl("cocoa beans", Item, ignore.case = TRUE) ~ "cocoa beans",
    grepl("oil palm fruit", Item, ignore.case = TRUE) ~ "oil palm fruit",
    grepl("other beans, green", Item, ignore.case = TRUE) ~ "other beans, green",
    grepl("other fruits", Item, ignore.case = TRUE) ~ "other fruits, n.e.c",
    TRUE ~ NA_character_  # 没有匹配的值设为 NA
  )) %>%
  filter(!is.na(Item))  # 过滤掉 NA 值的行

# 将 Year 保持为数值类型
fruit_data$Year <- as.numeric(as.character(fruit_data$Year))


# 重编码国家名称
name_corrections <- c(
  "Bahamas" = "The Bahamas",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Brunei Darussalam" = "Brunei",
  "Cabo Verde" = "Cape Verde",
  "China, Hong Kong SAR" = "Hong Kong S.A.R.",
  "China, Macao SAR" = "Macao S.A.R.",
  "China, Taiwan Province of" = "Taiwan",
  "Côte d'Ivoire" = "Ivory Coast",
  "Congo" = "Republic of Congo",
  "Czechia" = "Czech Republic",
  "Democratic People's Republic of Korea" = "North Korea",
  "Eswatini" = "Swaziland",
  "Guinea-Bissau" = "Guinea Bissau",
  "Iran (Islamic Republic of)" = "Iran",
  "Lao People's Democratic Republic" = "Laos",
  "Micronesia (Federated States of)" = "Federated States of Micronesia",
  "Netherlands (Kingdom of the)" = "Netherlands",
  "North Macedonia" = "Macedonia",
  "Republic of Korea" = "South Korea",
  "Republic of Moldova" = "Moldova",
  "Russian Federation" = "Russia",
  "Serbia" = "Republic of Serbia",
  "Sudan (former)" = "Sudan",
  "Syrian Arab Republic" = "Syria",
  "Timor-Leste" = "East Timor",
  "Türkiye" = "Turkey",
  "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
  "Venezuela (Bolivarian Republic of)" = "Venezuela",
  "Viet Nam" = "Vietnam"
)
fruit_data$Area <- recode(fruit_data$Area, !!!name_corrections)




###################### 根据不同图表对数据的需求pivot data #####################
fruit_data_map <- fruit_data %>%
  group_by(Area, Item) %>%
  summarise(production = round(sum(Value, na.rm = TRUE), 2))

fruit_data_line <- fruit_data %>%
  group_by(Year, Area, Item) %>%
  summarise(production = round(sum(Value, na.rm = TRUE), 2))


fruit_data_bar <- fruit_data %>%
  group_by(Area) %>%
  summarise(production = round(sum(Value, na.rm = TRUE), 2))


fruit_data_pie <- fruit_data %>%
  group_by(Area, Item, Year) %>%
  summarise(production = round(sum(Value, na.rm = TRUE) / 1e7, 2))%>%
  filter(production > 0)   # 去除 production 为 0 的国家
##################
# USER INTERFACE #
##################

###################### Map Tab #####################
global_map_tab <- tabPanel(
  "Global fruit Production",
  # 第一行，放 valueBox 占满整行
  fluidRow(
    column(width = 12,
           valueBoxOutput("summary", width = 12)  # 宽度设置为 12，占满一行
    )
  ),
  
  # 第二行，地图和折线图并排显示
  fluidRow(
    column(width = 6,  # 左侧地图占6列（50%）
           leafletOutput("map", height = "700px", width = "100%")  # 设置地图高度和宽度
    ),
    column(width = 6,  # 右侧折线图占6列（50%）
           # 在折线图上方添加描述文字
           h4("Click on the map to compare fruit production densities across countries."),
           
           # 添加 Clear 按钮
           actionButton("clear_selection", "Clear Country Selection", style = "margin-bottom: 20px;"),
           
           # 折线图输出
           plotlyOutput("line_chart", height = "600px", width = "100%")  # 设置折线图高度和宽度
    )
  ),
  
  ############## 悬浮的panel ################
  absolutePanel(
    top = 160, left = 300 , draggable = TRUE,
    selectInput(
      inputId = "fruit_type", 
      label = "Select fruit Type", 
      choices = c("all", fruit_types), 
      selected = "all"
    ),
    style = "background-color: rgba(255, 255, 255, 0.8); padding: 10px; border-radius: 8px;"
  )
)

###################### Continent Tab ###################
continent_tab <- tabPanel(
  "Continent Production",  
  fluidRow(
    column(12,
           h4("Click on a continent to view production details by country"),
           plotlyOutput("continent_bar_chart")
    )
  ),
  
  fluidRow(
    column(12,
           DTOutput("country_table") 
    )
  )
)

###################### fruit Type Tab ###################
type_tab <- tabPanel(
  title = "Percentage of fruit Type",  
  fluidPage(
    # 使用 sidebarLayout 结构
    sidebarLayout(
      # sidebarPanel 放置所有选择器
      sidebarPanel(
        h4("Filter Options"),
        
        # 国家下拉选择框
        selectInput(
          'country',
          label = 'Country',
          choices = c('All', sort(unique(fruit_data_pie$Area))),
          selected = 'All'
        ),
        
        
        ################## 改变slider 颜色 ################
        tags$head(
          tags$style(HTML("
      .js-irs-0 .irs-bar {
        background-color: #FF5733;  /* 滑块的轨道颜色 */
        border-color: #FF5733;
      }
      .js-irs-0 .irs-single, .js-irs-0 .irs-to, .js-irs-0 .irs-from {
        background-color: #FF5733;  /* 滑块选择器的颜色 */
        border-color: #FF5733;
      }
      .js-irs-0 .irs-slider {
        background-color: #FF5733;  /* 滑块手柄的颜色 */
        border-color: #FF5733;
      }
    "))
        ),
        
        # 年份选择器
        sliderInput(
          'year_range',
          label = 'Year Range',
          min = min(fruit_data_pie$Year),
          max = max(fruit_data_pie$Year),
          value = c(1961, 1966), 
          step = 1,
          sep = "",
          animate = TRUE 
        )
      ),
      
      # mainPanel 放置图表
      mainPanel(
        # 饼图展示各类水果的生产比例
        plotlyOutput('fruit_pie_chart', height = "600px", width = "100%")
      )
    )
  )
)

############## Guide Tab ####################
# Summary subtab

guide_summary = tabPanel(
  "Summary",
  h2(HTML("<b>Summary</b>"), style = "text-align:center"),
  br(),
  br(),
  span(
    h4("The User Interface focuses on a comprehensive study of fruit production across the world 
    during the years 1961 to 2012. 
          Our visualization concentrates on 
          gaining insights into the fruit production different of different product 
          types and countries over this period.",
       
       # Change the margin and line height of the text
       style = "font-weight:normal; border: 1px solid #ddd; padding: 10px; margin: 0 50px; line-height: 2;" 
    ),
    style = "margin-left:50px; margin-right:50px;"  # Adjust left and right margins
  )
)

# Data subtab
guide_data = tabPanel(
  "Data",
  br(),
  h2(
    HTML("<b> Data </b>"),
    style = "text-align:center"
  ),
  br(),
  span(
    h4(
      "The production data is the fruit production across the world from 1961 to 2013",
      style = "border: 1px solid #ddd; padding: 10px; margin: 0 50px; line-height: 1.5; font-weight: normal;"
    ),
    br(),
    br(),
    a("Click here to download the production data.", href = "https://www.fao.org/faostat/en/#data/QCL")
  )
)


ui <- navbarPage(
  theme = shinytheme("united"),
  "Global fruit Production Dashboard",
  global_map_tab,
  continent_tab,
  type_tab,
  navbarMenu("Guide", 
             guide_summary,
             guide_data)
)

################
# SHINY SERVER #
################

#This function is used to create map according to different data
create_map <- function(world_fruit_data) {
  
  # 自定义分段, 单位为吨, 设置从 0 到 100 billion tons (1e11)
  bins <- c(0, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11)  # 100k, 1M, 10M, 100M, 1B, 10B, 100B
  
  # 扩展颜色映射比例，为每个分段设置颜色
  pal <- colorBin(
    palette = c("#ffffcc", "#ffeda0", "#feb24c", "#fd8d3c", "#f03b20", "#bd0026", "#800026"),
    domain = world_fruit_data$production,
    bins = bins,
    na.color = "gray"  # "No data" 区域显示为灰色
  )
  
  # 使用 leaflet 绘制 Choropleth map
  map <- leaflet(world_fruit_data) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%  # 使用 CartoDB Positron 瓦片 (白色背景)
    setView(lng = 0, lat = 20, zoom = 2)%>%
    addPolygons(
      fillColor = ~pal(production), # density,production/square meter, 问一下tut,我查的图用的是原数据
      weight = 1,
      color = "white",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions( # 鼠标移动到某个polygen,会highlight出来
        weight = 3,
        color = "black",
        bringToFront = TRUE
      ),
      # 为每个国家设置 layerId 为国家的名称，便于点击事件捕获
      layerId = ~admin,  # 将 admin 列作为 layerId，为了click output id
      # toolips
      label = ~paste(admin, ": ", 
                     ifelse(production >= 1e9, 
                            paste(round(production / 1e9, 2), " billion tons"),  # 如果生产量超过 1 billion，显示为 billion tons
                            ifelse(production >= 1e6, 
                                   paste(round(production / 1e6, 2), " million tons"),  # 如果生产量超过 1 million，显示为 million tons
                                   paste(production, " tons")))),  # 其余情况显示为 tons
      
      
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      pal = pal,
      values = ~production,
      opacity = 0.7,
      title = "fruit Production",
      position = "bottomleft",
      labFormat = function(type, cuts, p) {
        # 根据定义的 cuts 动态生成标签，labels 必须和 cuts 对应
        labels <- c("0 t", "100,000 t", "1 million t", "10 million t", "100 million t", "1 billion t", "10 billion t", "100 billion t")
        return(labels)
      },
      na.label = "No data"  # 设置 NA 的标签为 "No data"
    )
  
  
  return(map)
}



server <- function(input, output, session) {
  ################## map tab ########################
  ################ map ##############################  
  # 根据用户选择的 fruit_type 过滤数据
  
  # 创建 reactive 表达式，基于选择的水果类型过滤地图数据
  map_filted_data <- reactive({
    if (input$fruit_type == 'all') {
      # 当选择 "all" 时，按国家汇总所有水果类型的总量
      aggregated_data <- fruit_data_map %>%
        group_by(Area) %>%
        summarise(production = sum(production, na.rm = TRUE))
    } else {
      # 否则，仅过滤选择的水果类型并汇总
      aggregated_data <- fruit_data_map %>%
        filter(Item == input$fruit_type) %>%
        group_by(Area) %>%
        summarise(production = sum(production, na.rm = TRUE))
    }
    
    # 将过滤后的数据与世界地图数据进行连接
    world_fruit_data <- world %>%
      left_join(aggregated_data, by = c("admin" = "Area"))
    return(world_fruit_data)
  })
  
  # 生成vlauebox
  output$summary <- renderValueBox({
    total_fruit_million <- sum(map_filted_data()$production, na.rm = TRUE) / 1000000
    valueBox(
      paste0(prettyNum(round(total_fruit_million, 2), big.mark = ","), " million tons"),
      "Total fruit production"
    )
  })
  
  # 生成地图
  output$map <- renderLeaflet({
    create_map(map_filted_data()) 
  })
  
  #################### line chart ######################
  
 
  # 使用 reactiveValues 来存储点击的国家（多个）和选择的水果类型
  selected <- reactiveValues(countries = list(), fruit_type = 'all')
  
  # 监听地图点击事件
  observeEvent(input$map_shape_click, {
    clicked_country <- input$map_shape_click$id  # 获取点击的国家 ID
    
    # 如果国家不在列表中，则添加
    if (!(clicked_country %in% selected$countries)) {
      selected$countries <- c(selected$countries, clicked_country)
    }
  })
  
  
  # 监听 selectInput 选择栏变化
  observeEvent(input$fruit_type, {
    selected$fruit_type <- input$fruit_type  # 更新 reactiveValues 中的水果类型
  })
  
  
  # 监听 abosulte pannel 变化
  observeEvent(input$clear_selection, {
    selected$countries <- list()  # 清空国家列表
  })
  
  
  # 创建 reactive 表达式，处理折线图数据
  line_chart_data <- reactive({
    # 根据 选择的 fruit_type 过滤数据
    filtered_fruit_data <- if (selected$fruit_type == 'all') {
      fruit_data_line
    } else {
      fruit_data_line %>%
        filter(Item == selected$fruit_type)
    }
    
    # 根据点击的国家过滤数据
    if (length(selected$countries) == 0) {
      # 没有选择国家时的处理逻辑
      filtered_data <- filtered_fruit_data %>%
        group_by(Year) %>%
        summarise(production = sum(production, na.rm = TRUE))
      
      return(list(
        data = filtered_data,
        title = "fruit Production Over Time for All Countries",
        yaxis_title = "Total fruit Production (tons)"
      ))
    } else {
      
      # 有选择国家时的处理逻辑
      combined_data <- data.frame()
      
      for (country in selected$countries) {
        
        # add production density column
        
        # join with world data to get country area
        join_data <- filtered_fruit_data %>%
          filter(Area == country) %>%
          left_join(world_area, by = c("Area" = "admin"))
        
        # add production_density
        join_data <- join_data %>%
          mutate(production_density = production / area_km2)
        
        # group data by year to get the production_density according to different year
        filtered_data <- join_data %>%
          group_by(Year) %>%
          summarise(production_density = mean(production_density, na.rm = TRUE))
        
        # add country name
        filtered_data <- filtered_data %>%
          mutate(Country = country)
        
        # combine multiple selected country data
        combined_data <- rbind(combined_data, filtered_data)
      }
      
      if (nrow(combined_data) > 0) {
        return(list(
          data = combined_data,
          title = "fruit Production Density Over Time for Selected Countries",
          yaxis_title = "fruit Production Density (tons/km²)"
        ))
      } else {
        return(NULL)
      }
    }
  })
  
  
  # 根据 line_chart_data 更新折线图
  output$line_chart <- renderPlotly({
    chart_data <- line_chart_data()
    print(chart_data)
    
    if (is.null(chart_data)) {
      # No data case
      plot_ly() %>%
        layout(title = "No Data Available for the Selected Countries and fruit Type")
    } else {
      # Determine y-axis and color based on selection
      y_var <- if (length(selected$countries) == 0) {
        chart_data$data$production
      } else {
        chart_data$data$production_density
      }
      
      color_var <- if (length(selected$countries) == 0) {
        NULL
      } else {
        chart_data$data$Country
      }
      
      # Create plotly object
      plot_ly(chart_data$data, x = ~Year, y = y_var, color = color_var, type = 'scatter', mode = 'lines+markers') %>%
        layout(title = chart_data$title,
               xaxis = list(title = "Year", dtick = 5, showgrid = FALSE),
               yaxis = list(title = chart_data$yaxis_title, showgrid = FALSE),
               plot_bgcolor = 'rgba(0,0,0,0)',
               paper_bgcolor = 'rgba(0,0,0,0)')
    }
  })
  
  
  
  ################## continent tab ########################
  
  # 数据准备
  
  country_data <- fruit_data %>%
    group_by(Area) %>%
    summarise(total_production = round(sum(Value, na.rm = TRUE) / 1e7, 2)) %>%
    filter(total_production > 0) %>%  # 去除 total_production 为 0 的国家
    inner_join(world_continent, by = c("Area" = "admin")) %>%
    select(-geometry)  # 删除 geometry 列
  
  
  continent_data <- country_data %>%
    group_by(continent)%>%
    summarise(total_production = sum(total_production, na.rm = TRUE))
  
  
  # 大洲生产量柱状图
  output$continent_bar_chart <- renderPlotly({
    plot_ly(
      continent_data, 
      x = ~continent, 
      y = ~total_production, 
      type = 'bar', 
      source = "bar",  # 设置独立的 source 名称
      marker = list(
        color = 'purple', 
        line = list(color = 'black', width = 1.5),  # 边框设置
        opacity = 0.8  # 正常显示时的透明度
      ),
      hovertemplate = "Continent: %{x}<br>Total Production: %{y}<extra></extra>"  # 自定义悬停时的提示信息
    ) %>%
      layout(
        title = "Production by Continent", 
        xaxis = list(title = "Continent"), 
        yaxis = list(title = "Total Production (million tons)")
      )
  })
  
  # 初始化显示全球排名
  output$country_table <- renderDT({
    print(country_data)
    datatable(country_data %>%
                arrange(desc(total_production)),  # 按生产量全局排名
              options = list(
                pageLength = 10,  # 默认显示10条数据
                lengthMenu = c(5, 10, 25, 50)  # 用户可选择显示条目的数量
              ),
              colnames = c("Country", "Total Production (million tons)", "Continent"),  # 自定义显示的列名
              rownames = FALSE)
  })
  
  # 捕获用户点击柱状图事件
  observeEvent(event_data("plotly_click", source = "bar"), {
    click_data <- event_data("plotly_click", source = "bar")
    selected_continent <- click_data$x
    
    # 根据用户选择的大洲，过滤国家数据
    filtered_data <- country_data %>%
      filter(continent == selected_continent) %>%
      arrange(desc(total_production))
    
    # 在下方显示排名表
    output$country_table <- renderDT({
      datatable(filtered_data, 
                options = list(
                  pageLength = 10,  # 默认显示10条数据
                  lengthMenu = c(5, 10, 25, 50)  # 用户可选择显示条目的数量
                ),
                colnames = c("Country", "Total Production (million tons)", "Continent"),  # 自定义显示的列名
                rownames = FALSE)
    })
  })
  
  
  
  
  ######################### fruit type tab #########################
  
  
  # 动态更新的饼图
  output$fruit_pie_chart <- renderPlotly({
    
    # 根据输入的 country 和 year_range 进行筛选
    filtered_data <- fruit_data_pie %>%
      filter(
        (input$country == 'All' | Area == input$country),
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      )
    
    # 检查筛选后的数据是否为空
    if (nrow(filtered_data) == 0) {
      # 返回一个提示信息，告知用户没有数据
      return(plot_ly() %>%
               layout(
                 title = "No Data Available",
                 annotations = list(
                   text = "No data available for the selected filters.",
                   x = 0.5, y = 0.5, showarrow = FALSE,
                   font = list(size = 20)
                 )
               )
      )
    }
    
    
    
    # 按水果类型（即 Item）汇总生产量数据
    pie_data <- filtered_data %>%
      group_by(Item) %>%
      summarise(total_production = sum(production))
    
    
    # 使用柔和的调色板（Pastel1）
    colors <- brewer.pal(n = min(8, nrow(pie_data)), name = "Pastel1")
    
    # 绘制饼图，鼠标悬停时才显示标签
    plot_ly(
      data = pie_data,
      labels = ~Item,  # 将水果类型作为标签
      values = ~total_production,  # 使用生产量作为值
      type = 'pie',
      textinfo = 'none',  # 不显示标签
      hoverinfo = 'label+percent+value',  # 鼠标悬停时显示标签、百分比、值
      text = ~paste("fruit Type:", Item, "<br>Production:", total_production),
      marker = list(
        colors = colors,  # 使用柔和的调色板
        line = list(color = '#FFFFFF', width = 2)  # 白色边框，边框宽度为2
      ),
      pull = 0.05  # 扇区间的拉出效果，增加视觉吸引力
    ) %>%
      layout(
        title = "fruit Type Distribution",
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1, font = list(size = 16)),  # 横向排列图例
        margin = list(t = 50, l = 50, r = 50, b = 100)  # 调整图表的边距
      )
  })
  
}

# 启动应用
shinyApp(ui = ui, server = server)
