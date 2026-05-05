

library(shiny)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

edges_raw <- read.csv("edges_final_corrected.csv")
nodes_raw <- read.csv("nodes_final.csv")

nodes_df <- nodes_raw |> select(name = label, id, gender, social_class, house_affiliation)
edges_df <- edges_raw |> rename(from = Source, to = Target, weight = Weight)

g <- graph_from_data_frame(d = edges_df, vertices = nodes_df, directed = TRUE)

g_tidy <- as_tbl_graph(g) |>
  activate(nodes) |>
  mutate(
    degree_in   = centrality_degree(mode = "in"),
    degree_out  = centrality_degree(mode = "out"),
    betweenness = centrality_betweenness(normalized = TRUE)
  )

class_colors <- c(
  "Royalty"  = "#8B1A1A",
  "Noble"    = "#B8860B",
  "Knight"   = "#4A708B",
  "Commoner" = "#8B7355"
)

ui <- fluidPage(
  titlePanel("A Knight of the Seven Kingdoms — Dialogue Network"),
  sidebarLayout(
    sidebarPanel(
      selectInput("size_by", "Size nodes by:",
                  choices  = c("In-Degree" = "degree_in",
                               "Out-Degree" = "degree_out",
                               "Betweenness" = "betweenness"),
                  selected = "betweenness"
      ),
      sliderInput("min_weight", "Min edge weight:",
                  min = 1, max = 300, value = 80, step = 10
      ),
      hr(),
      p("Node color = social class. Edge width = word count.", style = "font-size:0.85em;")
    ),
    mainPanel(
      plotOutput("net_plot", height = "580px")
    )
  )
)

server <- function(input, output, session) {
  output$net_plot <- renderPlot({
    net <- g_tidy |>
      activate(edges) |>
      filter(weight >= input$min_weight) |>
      activate(nodes) |>
      filter(centrality_degree(mode = "all") > 0) |>
      mutate(
        degree_in   = centrality_degree(mode = "in"),
        degree_out  = centrality_degree(mode = "out"),
        betweenness = centrality_betweenness(normalized = TRUE)
      )
    
    ggraph(net, layout = "fr") +
      geom_edge_link(aes(width = weight, alpha = weight),
                     color = "grey55", show.legend = FALSE) +
      scale_edge_width(range = c(0.3, 3)) +
      scale_edge_alpha(range = c(0.15, 0.65)) +
      geom_node_point(aes(size  = .data[[input$size_by]] + 0.01,
                          color = social_class), alpha = 0.9) +
      scale_size(range = c(3, 13), guide = "none") +
      scale_color_manual(values = class_colors, name = "Social Class") +
      geom_node_text(aes(label = name), size = 2.7, repel = TRUE, color = "grey20") +
      theme_graph(base_size = 12) +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui = ui, server = server)