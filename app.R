library(shiny)
library(bslib)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(visNetwork)

edges_raw <- read.csv("edges_final_corrected.csv")
nodes_raw <- read.csv("nodes_final.csv")

nodes_df <- nodes_raw |> select(name = label, gender, social_class, house_affiliation)
edges_df <- edges_raw |> rename(from = Source, to = Target, weight = Weight)

g <- graph_from_data_frame(d = edges_df, vertices = nodes_df, directed = TRUE)

g_tidy <- as_tbl_graph(g) |>
  activate(nodes) |>
  mutate(
    degree_in    = centrality_degree(mode = "in"),
    degree_out   = centrality_degree(mode = "out"),
    degree_total = centrality_degree(mode = "all"),
    betweenness  = centrality_betweenness(normalized = TRUE, weights = NA),
    constraint   = node_constraint()
  )

nodes_full <- g_tidy |> activate(nodes) |> as_tibble()

density_val <- round(edge_density(g), 3)
assort_val  <- round(assortativity_nominal(g, as.integer(as.factor(V(g)$social_class))), 3)

set.seed(42)
g_und    <- as.undirected(g, mode = "collapse", edge.attr.comb = list(weight = "sum"))
comms    <- cluster_louvain(g_und, weights = E(g_und)$weight)
mod_val  <- round(modularity(comms), 3)
n_comms  <- length(unique(membership(comms)))

class_colors <- c(
  "Royalty"  = "#8B1A1A",
  "Noble"    = "#B8860B",
  "Knight"   = "#4A708B",
  "Commoner" = "#8B7355"
)

edge_colors <- c(
  "Royalty"  = "#D32F2F",
  "Noble"    = "#E8961A",
  "Knight"   = "#1565C0",
  "Commoner" = "#795548"
)

ui <- navbarPage(
  title = "⚔  A Knight of the Seven Kingdoms: Dialogue Network",
  theme = bs_theme(bootswatch = "flatly", primary = "#8B1A1A"),
  collapsible = TRUE,

  tabPanel("Introduction",
    fluidRow(
      column(8,
        h2("Who Speaks to Whom in Westeros?",
           style = "margin-top:20px; color:#8B1A1A;"),
        p("This app maps the dialogue network of Season 1 of ",
          em("A Knight of the Seven Kingdoms"), " (HBO, 2025). ",
          "Each of the ", strong("27 nodes"), " is a named speaking character. ",
          "Each of the ", strong("95 directed edges"), " points from speaker to listener, ",
          "weighted by word count. The network is directed because in a feudal society, ",
          "who talks to whom and how much is not neutral given such an inequality among social classes."),
        hr(),
        p(span("FINDING 1  ·  STRUCTURAL HOLES",
               style = "font-size:0.72em; font-weight:bold; letter-spacing:0.08em; color:#8B1A1A;")),
        h4("Dunk connects two worlds that have no reason to talk to each other",
           style = "color:#8B1A1A; margin-top:2px;"),
        p(span("Dunk", style = "color:#4A708B; font-weight:bold;"),
          "'s betweenness is ", strong("0.76"),
          ", meaning he sits on 76% of all shortest paths in the network. ",
          "To survive the Trial by Seven, he needed champions from wherever he could find them: ",
          "a commoner armorer, fellow hedge knights, and ultimately a royal prince. ",
          "That recruitment across every class line is what his score reflects."),
        br(),
        p(span("Egg", style = "color:#8B1A1A; font-weight:bold;"),
          " ranks second at ", strong("0.13"),
          ", for a reason that matches his storyline. ",
          "He is a royal prince traveling as a commoner squire. ",
          "That dual identity also supports this claim."),
        br(),
        p(span("Baelor", style = "color:#8B1A1A; font-weight:bold;"),
          " and ",
          span("Maekar", style = "color:#8B1A1A; font-weight:bold;"),
          " have some in-degree because people approach them out of feudal obligation. ",
          "But their betweenness is near zero. ",
          "Formal rank and structural importance point in opposite directions here."),
        br(),
        p("The one exception is ",
          span("Raymun Fossoway", style = "color:#B8860B; font-weight:bold;"),
          " at ", strong("~0.06"),
          ", higher than any royal. ",
          "He is the broker within the noble tier during the trial. ",
          "Within a single class, position matters more than title."),
        br(),
        p(strong("Go to the Centrality tab and switch between In-Degree and Betweenness."),
          " Watch how Egg moves up relative to the royals, and how Baelor drops.",
          style = "color:#8B1A1A; background:#fdf3f3; padding:10px 14px; border-left:3px solid #8B1A1A; border-radius:3px;"),
        hr(),
        p(span("FINDING 2  ·  HOMOPHILY",
               style = "font-size:0.72em; font-weight:bold; letter-spacing:0.08em; color:#B8860B;")),
        h4("Feudal hierarchy presented in data however weaker than expected",
           style = "color:#B8860B; margin-top:2px;"),
        p("The class assortativity score is ", strong(assort_val), ". ",
          "Characters do tend to talk within their own class. ",
          "The number is positive but does not reach 1.0. ",
          "This can be explained by the Trial by Seven, which forced conversations between ",
          "characters who would never otherwise meet, and those interactions show up directly in the score."),
        br(),
        p("This also connects to Granovetter and Burt's research on how homophily is not always ",
          "the right choice. Having allies from multiple groups will help you more."),
        hr(),
        h4("How to use this app", style = "color:#4A708B;"),
        tags$ul(
          tags$li(strong("Network:"),
            " adjust node size by centrality measure and filter edge weight to see which ties matter most."),
          tags$li(strong("Centrality:"),
            " compare how differently each character ranks depending on which measure you use."),
          tags$li(strong("Network Measures:"),
            " density / class assortativity / community detection results."),
          tags$li(strong("Data collection process and measurements analysis are still in progress."))
        )
      ),
      column(4,
        div(style = "background:#f8f5f0; border-left:4px solid #8B1A1A;
                     padding:15px; margin-top:20px; border-radius:4px;",
          h4("At a glance", style = "margin-top:0;"),
          tags$table(class = "table table-sm", style = "font-size:0.9em;",
            tags$tbody(
              tags$tr(tags$td("Characters"),          tags$td(strong(vcount(g)))),
              tags$tr(tags$td("Dialogue edges"),      tags$td(strong(ecount(g)))),
              tags$tr(tags$td("Density"),             tags$td(strong(density_val))),
              tags$tr(tags$td("Class assortativity"), tags$td(strong(assort_val))),
              tags$tr(tags$td("Modularity"),          tags$td(strong(mod_val))),
              tags$tr(tags$td("Communities"),         tags$td(strong(n_comms)))
            )
          ),
          hr(),
          h5("Color key"),
          p(span("●", style=paste0("color:",class_colors["Royalty"],"; font-size:1.2em;")),
            " Royalty  ",
            span("●", style=paste0("color:",class_colors["Noble"],"; font-size:1.2em;")),
            " Noble"),
          p(span("●", style=paste0("color:",class_colors["Knight"],"; font-size:1.2em;")),
            " Knight  ",
            span("●", style=paste0("color:",class_colors["Commoner"],"; font-size:1.2em;")),
            " Commoner")
        )
      )
    )
  ),

  tabPanel("Network",
    sidebarLayout(
      sidebarPanel(width = 3,
        h5("Node size"),
        selectInput("net_measure", NULL,
          choices  = c("In-Degree"    = "degree_in",
                       "Out-Degree"   = "degree_out",
                       "Betweenness"  = "betweenness"),
          selected = "betweenness"
        ),
        hr(),
        h5("Minimum edge weight"),
        sliderInput("weight_min", NULL,
          min = 1, max = 300, value = 100, step = 10
        ),
        hr(),
        div(style = "font-size:0.82em; color:#444; line-height:1.9;",
          strong("Edge color = class of the speaker:"), br(),
          span("━━", style=paste0("color:",edge_colors["Royalty"],"; font-size:1.1em;")),
          " Royalty", br(),
          span("━━", style=paste0("color:",edge_colors["Noble"],"; font-size:1.1em;")),
          " Noble", br(),
          span("━━", style=paste0("color:",edge_colors["Knight"],"; font-size:1.1em;")),
          " Knight", br(),
          span("━━", style=paste0("color:",edge_colors["Commoner"],"; font-size:1.1em;")),
          " Commoner", br(), br(),
          em("Edge width = word count (tie strength)")
        )
      ),
      mainPanel(width = 9,
        visNetworkOutput("vis_net", height = "600px"),
        br(),
        p(em("Nodes arranged by feudal tier — Royalty at top, Commoner at bottom.",
             " Edge color = speaker's class; width = word count.",
             " Hover over any node to see stats. Drag to rearrange."),
          style = "color:grey; font-size:0.83em;")
      )
    )
  ),

  tabPanel("Centrality",
    sidebarLayout(
      sidebarPanel(width = 3,
        h5("Select measure"),
        radioButtons("bar_measure", NULL,
          choices  = c("In-Degree"   = "degree_in",
                       "Out-Degree"  = "degree_out",
                       "Betweenness" = "betweenness",
                       "Constraint"  = "constraint"),
          selected = "betweenness"
        ),
      ),
      mainPanel(width = 9,
        plotOutput("centrality_bar", height = "520px")
      )
    )
  ),

  tabPanel("Network Measures",
    fluidRow(
      column(4,
        div(style = "border-left:4px solid #8B1A1A; padding:12px 15px;
                     background:
                     box-shadow:0 1px 3px rgba(0,0,0,0.07);",
          h5("Density", style = "margin-top:0; color:#666;"),
          h2(density_val, style = "color:#8B1A1A; margin:4px 0;"),

        )
      ),
      column(4,
        div(style = "border-left:4px solid #B8860B; padding:12px 15px;
                     background:
                     box-shadow:0 1px 3px rgba(0,0,0,0.07);",
          h5("Class Assortativity", style = "margin-top:0; color:#666;"),
          h2(assort_val, style = "color:#B8860B; margin:4px 0;"),

        )
      ),
      column(4,
        div(style = "border-left:4px solid #4A708B; padding:12px 15px;
                     background:
                     box-shadow:0 1px 3px rgba(0,0,0,0.07);",
          h5("Modularity (Louvain)", style = "margin-top:0; color:#666;"),
          h2(mod_val, style = "color:#4A708B; margin:4px 0;"),

        )
      )
    )
  )
)

server <- function(input, output, session) {

  filtered_net <- reactive({
    g_tidy |>
      activate(edges) |>
      filter(weight >= input$weight_min) |>
      activate(nodes) |>
      filter(centrality_degree(mode = "all") > 0)
  })

  output$vis_net <- renderVisNetwork({
    net   <- filtered_net()
    n_tbl <- net |> activate(nodes) |> as_tibble()
    e_tbl <- net |> activate(edges) |> as_tibble()

    if (nrow(n_tbl) == 0) return(visNetwork(data.frame(), data.frame()))

    tier  <- c("Royalty" = 1, "Noble" = 2, "Knight" = 3, "Commoner" = 4)
    idx   <- n_tbl$name
    n2c   <- setNames(n_tbl$social_class, n_tbl$name)

    full_stats <- nodes_full |>
      select(name, degree_in, degree_out, betweenness, constraint)

    n_tbl <- n_tbl |>
      select(name, social_class, house_affiliation) |>
      left_join(full_stats, by = "name")

    nodes_vis <- n_tbl |>
      mutate(
        id    = name,
        label = name,
        level = tier[social_class],
        color = class_colors[social_class],
        value = (.data[[input$net_measure]] + 0.01) * 2,
        title = paste0("<b>", name, "</b><br>",
                       "Class: ", social_class, "<br>",
                       "In-degree: ",   degree_in,  "<br>",
                       "Out-degree: ",  degree_out, "<br>",
                       "Betweenness: ", round(betweenness, 3),
                       "<br><i>(full network values)</i>")
      )

    edges_vis <- e_tbl |>
      mutate(
        from_name = idx[from], to_name = idx[to],
        from  = from_name, to = to_name,
        title = paste0("<b>", from_name, " → ", to_name, "</b><br>Words: ", weight),
        width = case_when(weight >= 400 ~ 8, weight >= 200 ~ 5,
                          weight >= 100 ~ 2.5, TRUE ~ 1),
        color = edge_colors[n2c[from_name]]
      ) |>
      select(-from_name, -to_name)

    visNetwork(nodes_vis, edges_vis) |>
      visNodes(
        borderWidth = 2,
        font   = list(size = 12, color = "#111"),
        shadow = list(enabled = TRUE, size = 4),
        color  = list(
          highlight = list(background = "#FFD700", border = "#333"),
          hover     = list(background = "#FFF9C4", border = "#555")
        )
      ) |>
      visEdges(
        arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
        smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.25)
      ) |>
      visOptions(highlightNearest = FALSE) |>
      visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) |>
      visHierarchicalLayout(
        direction       = "UD",
        sortMethod      = "directed",
        levelSeparation = 180,
        nodeSpacing     = 130
      )
  })

  output$centrality_bar <- renderPlot({
    m <- input$bar_measure

    labels <- c(
      degree_in   = "In-Degree — Times Spoken To",
      degree_out  = "Out-Degree — Times Initiating Speech",
      betweenness = "Betweenness Centrality (normalized)",
      constraint  = "Constraint — Lower = More Bridging"
    )

    plot_df <- if (m == "constraint") {
      nodes_full |>
        filter(!is.na(constraint)) |>
        arrange(constraint) |>
        mutate(name = factor(name, levels = name))
    } else {
      nodes_full |>
        arrange(desc(.data[[m]])) |>
        mutate(name = factor(name, levels = rev(name)))
    }

    ggplot(plot_df, aes(x = .data[[m]], y = name, fill = social_class)) +
      geom_col(width = 0.72, alpha = 0.88) +
      scale_fill_manual(values = class_colors, name = "Social Class") +
      labs(title = labels[m], x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position    = "bottom",
        plot.title         = element_text(size = 13, face = "bold", color = "#333")
      )
  })

}

shinyApp(ui = ui, server = server)
