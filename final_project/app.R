## load required packages
library(babynames)
library(tidyverse)
library(scales)
library(shiny)
library(snakecase)
library(reactable)

# custom theme

theme_cdc <- function(){
  theme_bw() %+replace% theme(
    text = element_text(size = 16),
    panel.grid.major= element_line(linetype = "longdash"),
    panel.grid.minor= element_blank()
  )}

ui <- fluidPage(
  title = "Baby Name Popularity: 1880 - 2017",
  textInput("name",
            "Input Baby Name:",
            "John"),
  tableOutput("rank_table"),
  plotOutput("plot",
             width = "80%",
             height = "600px"),
  plotOutput("histogram",
             width = "80%",
             height = "600px"),
  reactableOutput("distinct_names"),
  plotOutput("babies_born",
             width = "80%",
             height = "600px"),  
  plotOutput("distinct_name_chart",
             width = "80%",
             height = "600px"),
  plotOutput("popular_names_by_year",
             width = "80%",
             height = "600px")  
)

server <- function(input, output, session) {
  max_prop <- reactive(babynames %>%
                              filter(name == input$name) %>%
                              mutate(max_prop = max(prop)) %>%
                              filter(prop == max_prop) %>%
                              select(prop) %>%
                              distinct())
  
  max_prop_year <- reactive(babynames %>%
                                   filter(name == input$name) %>%
                                   mutate(max_prop = max(prop)) %>%
                                   filter(prop == max_prop) %>%
                                   select(year) %>%
                                   distinct())
  
  text_intercept <- reactive(babynames %>%
                                    filter(name == input$name) %>%
                                    group_by(sex) %>%
                                    mutate(min_prop = max(prop), count = sum(n)) %>%
                                    ungroup() %>%
                                    filter(count == max(count)) %>%
                                    filter(prop == min_prop) %>%
                                    select(year) %>%
                                    mutate(year = case_when(year > 2010 ~ year - 10,
                                                            year < 1900 ~ year + 30,
                                                            .default = year)) %>%
                                    distinct())
  
  baby_data <- reactive(babynames %>%
                               filter(name == input$name))
  
  output$plot <- renderPlot({
    baby_data() %>%
      ggplot(., aes(x = year, y = prop, color = sex)) +
      geom_point() +
      geom_line() + 
      geom_vline(xintercept = as.double(max_prop_year()),
                 color = "blue") +
      geom_text(inherit.aes = FALSE,
                x = as.double(text_intercept()) - 15, 
                y = as.double(max_prop()),
                color = "blue", 
                label = paste("Peak Popularity",
                              "\n",
                              "Year:",
                              max_prop_year(),
                              "\n",
                              "Prop.:",
                              percent(max_prop()$prop, 
                                      accuracy = 0.001))) +
      theme_cdc() +
      scale_y_continuous(labels = percent,
                         n.breaks = 15) +
      scale_x_continuous(n.breaks = 15) +
      ggtitle(input$name) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = NULL,
           y = "Proportion", 
           color = "Sex")})

  
  output$rank_table <- renderTable({
    babynames %>%
      mutate(sex = if_else(sex == "M",
                           "(Male)",
                           "(Female)")) %>%
      group_by(sex,
               name) %>%
      summarise(total_usage = sum(n)) %>%
      arrange(desc(total_usage)) %>%
      mutate(rank = rank(-total_usage)) %>%
      mutate(rank_ends = case_when(rank %in% c(11,12,13) ~ "th",
                                   rank %% 10 == 1 ~ 'st',
                                   rank %% 10 == 2 ~ 'nd',
                                   rank %% 10 == 3 ~'rd',
                                   TRUE ~ "th")) %>%
      mutate(rank = paste0(rank, rank_ends)) %>%
      select(-rank_ends) %>%
      ungroup() %>%
      pivot_wider(names_from = "sex",
                  values_from = c("total_usage", "rank")) %>%
      filter(name == input$name) %>%
      rename_all(str_to_title) %>%
      rename_with(~str_replace_all(.,"_"," ")) %>%
      mutate_if(is.integer,
                comma)})
  
  output$histogram <- renderPlot({
   baby_data() %>%
      ggplot(.,
             aes(x = year,
                 y = n,
                 fill = sex)) +
      geom_col() +
      labs(x = "Year",
           y = "Count",
           fill = "Sex") +
      scale_x_continuous(n.breaks = 15) +
      scale_y_continuous(n.breaks = 15,
                         labels = dollar_format(scale = .0001, suffix = "K", prefix = " ")) +
      theme_cdc()
  })
  
  
  output$distinct_names <- renderReactable({
    babynames %>%
      group_by(year) %>%
      summarise(babies_born = sum(n),
                distinct_names = n_distinct(name)) %>%
      mutate_if(is.integer,
                comma) %>%
      rename_all(to_title_case) %>%
      reactable()
  })
  
  output$distinct_name_chart <- renderPlot({
    babynames %>%
      group_by(year) %>%
      summarise(distinct_names = n_distinct(name)) %>%
      ggplot(.,
             aes(x = year,
                 y = distinct_names)) +
      geom_col() +
      labs(x = "Year",
           y = "Distinct Names") +
      scale_x_continuous(n.breaks = 15) +
      scale_y_continuous(n.breaks = 15) +      
      theme_cdc() +
      ggtitle("Number of Distinct Names by Year")
  })
  
  output$babies_born <- renderPlot({
    babynames %>%
      group_by(year) %>%
      summarise(babies_born = sum(n)) %>%
      ggplot(.,
             aes(x = year,
                 y = babies_born)) +
      geom_col() +
      labs(x = "Year",
           y = "Distinct Names") +
      scale_y_continuous(n.breaks = 15,
                         labels = dollar_format(scale = .0001, suffix = "K", prefix = " ")) +
      scale_x_continuous(n.breaks = 15) +
      theme_cdc() +
      ggtitle("Number of Babies Born per Year")
  })
  
  output$popular_names_by_year <- renderPlot({
    babynames %>%
      group_by(year,
               sex) %>%
      mutate(max_prop = max(prop)) %>%
      ungroup() %>%
      filter(prop == max_prop) %>%
      ggplot(.,
             aes(x = year,
                 y = prop,
                 colour = name)) +
      geom_point() +
      labs(x = "Year",
           y = "Proportion",
           colour = "Name") +
      scale_x_continuous(n.breaks = 15) +
      scale_y_continuous(n.breaks = 15,
                         labels = percent) +         
      ggtitle("Most Popular Names by Year") +
      theme_cdc()
  })
  
}

shinyApp(ui, server)