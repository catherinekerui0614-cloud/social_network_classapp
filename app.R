library(shiny)
library(bslib)
library(tidyverse)
library(igraph)
library(tidygraph)
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
assort_val  <- round(assortativity_nominal(g, as.integer(as.factor(V(g)$social_class)), directed = TRUE), 3)

set.seed(42)
g_und   <- as.undirected(g, mode = "collapse", edge.attr.comb = list(weight = "sum"))
comms   <- cluster_louvain(g_und, weights = E(g_und)$weight)
mod_val <- round(modularity(comms), 3)
n_comms <- length(unique(membership(comms)))

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
  title = "A Knight of the Seven Kingdoms: Dialogue Network",
  theme = bs_theme(bootswatch = "flatly", primary = "#8B1A1A"),
  collapsible = TRUE,

  tabPanel("Introduction",
    fluidRow(
      column(8,
        h2("Who Speaks to Whom in Westeros?",
           style = "margin-top:20px; color:#8B1A1A;"),
        p("This app maps the dialogue network of Season 1 of ",
          em("A Knight of the Seven Kingdoms"), " (HBO, 2026). ",
          "Each of the ", strong("27 nodes"), " is a named speaking character. ",
          "Each of the ", strong("95 directed edges"), " points from speaker to listener and weighted by word count. ",
          "The network is directed because speaker and listener are not interchangeable roles. ",
          "In a feudal society, that direction becomes sociologically meaningful as who speaks / ",
          "who is addressed / how much one speaks can reveal hierarchy with dependence across social classes."),
        p("The Trial by Seven is a legal combat ritual in this world: when a character is accused of a crime, either side can invoke trial by combat, requiring each side to field seven champions. The outcome then determines guilt or innocence.",
          style = "font-size:0.85em; color:#666; background:#f5f5f5; padding:8px 12px; border-radius:3px;"),
        hr(),

        p(span("FINDING 1  \u00b7  STRUCTURAL HOLES",
               style = "font-size:0.72em; font-weight:bold; letter-spacing:0.08em; color:#8B1A1A;")),
        h4("Dunk connects social worlds that rarely speak directly",
           style = "color:#8B1A1A; margin-top:2px;"),

        p(span("Dunk", style = "color:#4A708B; font-weight:bold;"),
          "'s normalized betweenness score is ", strong("0.76"),
          ", meaning he lies on a large share of the network's shortest paths after normalization. ",
          "In the series his Trial by Seven recruitment helps explain why his dialogue ties cross class lines. ",
          "He needed support from a commoner armorer with fellow hedge knights and ultimately a royal prince. ",
          "Thus his score captures the result, many shortest routes between other characters pass through his position. ",
          "A node removal check can also confirm this, so removing Dunk fragments the network into ",
          strong("6 components"), " — one main group of 20 characters, one pair, and four isolates. ",
          "This is how Burt's idea of structural holes helps interpret his score. ",
          "His importance comes less from speaking a lot than from occupying the space ",
          "between otherwise weakly connected parts of the network (Burt, 2004)."),
        br(),
        p(span("Egg", style = "color:#8B1A1A; font-weight:bold;"),
          " ranks second at ", strong("0.13"),
          ". He is Aegon Targaryen, a royal prince (tier 1) traveling as a commoner squire (tier 4). ",
          "Egg's dual identity places him at both extremes simultaneously, unlike characters who cross just one adjacent social tier. ",
          "That dual identity helps explain why he occupies a secondary brokerage position in the network."),
        br(),
        p(span("Baelor", style = "color:#8B1A1A; font-weight:bold;"),
          " and ",
          span("Maekar", style = "color:#8B1A1A; font-weight:bold;"),
          " have some in degree, a pattern consistent with their high status positions, ",
          "but their betweenness is near zero. ",
          "This means that while speech flows to them it does not flow through them. ",
          "They are dead ends where they are visible as targets and not necessary for connecting ",
          "otherwise separated parts of the network."),
        br(),
        p("Most other characters follow the same pattern of high status nodes attract incoming ties ",
          "but have near zero betweenness. The one exception is ",
          span("Raymun Fossoway", style = "color:#B8860B; font-weight:bold;"),
          " (betweenness = ",
          textOutput("raymun_btwn", inline = TRUE),
          "), a noble whose score is higher than any royal in the network. ",
          "He acts as an intermediary within the noble tier during the trial, ",
          "suggesting that even within a single class structural position matters more than title."),
        br(),
        p(strong("Go to the Centrality tab and switch between In Degree and Betweenness."),
          " Watch how Baelor receives incoming ties but his betweenness is near zero.",
          style = "color:#8B1A1A; background:#fdf3f3; padding:10px 14px;
                   border-left:3px solid #8B1A1A; border-radius:3px;"),
        hr(),

        p(span("FINDING 2  \u00b7  HOMOPHILY",
               style = "font-size:0.72em; font-weight:bold; letter-spacing:0.08em; color:#B8860B;")),
        h4("Feudal hierarchy shows up in the data, but weaker than expected",
           style = "color:#B8860B; margin-top:2px;"),

        p("The class assortativity score is ", strong(assort_val), ". ",
          "McPherson et al. (2001) describe homophily as the tendency for similar nodes ",
          "to connect more often. Here the positive score suggests that same class directed ties ",
          "occur slightly more often than they would under random class mixing."),
        br(),
        p("But ", strong(assort_val), " is far from 1.0. One plausible interpretation is that the Trial by Seven ",
          "pulls characters across class lines, weakening what might otherwise appear as stronger class separation. ",
          "These cross class ties are bridges in Granovetter's sense because they connect otherwise separate social positions. ",
          "But they are not always weak in this network as Dunk and Egg have the highest word count tie of all. ",
          "Brokerage here comes from intense cross class relationships that differs from casual acquaintances. ",
          "Dunk's low constraint score supports this interpretation because his contacts are less redundant with one another."),
        br(),
        p(strong("Go to the Centrality tab and switch to Constraint."),
          " Constraint measures how redundant a character's contacts are, which is whether they all already know each other ",
          "(high constraint + closed circle) or come from different unconnected worlds (low constraint + bridging position). ",
          "Dunk's score reflects an ego network with contacts across every social tier.",
          style = "color:#B8860B; background:#fdf9ec; padding:10px 14px;
                   border-left:3px solid #B8860B; border-radius:3px;"),
        hr(),

        h4("How to use this app", style = "color:#4A708B;"),
        tags$ul(
          tags$li(strong("Network:"),
            " adjust node size by centrality measure; filter edge weight with the slider to show only the strongest ties."),
          tags$li(strong("Centrality:"),
            " compare how character rankings shift across four measures."),
          tags$li(strong("Network Measures:"),
            " three global measures: density, class assortativity, and Louvain modularity."),
          tags$li(strong("Data:"),
            " how the network was constructed and what choices were made.")
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
          p(span("\u25cf", style = paste0("color:", class_colors["Royalty"], "; font-size:1.2em;")),
            " Royalty  ",
            span("\u25cf", style = paste0("color:", class_colors["Noble"], "; font-size:1.2em;")),
            " Noble"),
          p(span("\u25cf", style = paste0("color:", class_colors["Knight"], "; font-size:1.2em;")),
            " Knight  ",
            span("\u25cf", style = paste0("color:", class_colors["Commoner"], "; font-size:1.2em;")),
            " Commoner")
        )
      )
    )
  ),

  tabPanel("Data",
    fluidRow(
      column(8,
        h3("Data Collection", style = "margin-top:20px; color:#333;"),
        p("The network was built from publicly available episode transcript files for Season 1 of ",
          em("A Knight of the Seven Kingdoms"), " (HBO, 2026), covering all six episodes. ",
          "Only named characters with at least one line of dialogue were included as nodes."),
        hr(),

        h5("Nodes"),
        p("Each node is a named speaking character. Characters who appear but never speak and unnamed background characters are excluded. The final network has ",
          strong("27 nodes"), " across four social classes: Royalty / Noble / Knight / Commoner. ",
          "Node attributes include gender, social class, and house affiliation."),

        h5("Edges"),
        p("Each directed edge points from speaker to listener. ",
          "The edge weight is the word count of dialogue spoken from source to target. ",
          "Word count is used as a limited proxy for observable verbal investment instead of full tie strength. ",
          "It connects to Granovetter's idea that stronger ties involve greater time and energy, ",
          "but it cannot capture the full quality of a relationship or any other types of emotional closeness. ",
          "In this project a character who speaks 500 words to another is treated as making ",
          "a larger observable verbal investment than one who gives a two word reply, ",
          "though word count cannot capture every dimension of relationship strength."),

        h5("Key data decisions"),
        tags$ol(
          tags$li(strong("Scene boundaries."),
            " The function lead() assigns the next speaker as the target, ",
            "which creates false edges at scene transitions. ",
            "Nine false ties created by scene transitions were identified manually. ",
            "These cross scene target assignments were removed and the relevant word counts ",
            "were reassigned to the correct listener within the original scene when the listener was identifiable. ",
            "Because listener identity is not always explicit in transcripts, ",
            "the next speaker was used as the default listener proxy and manually corrected ",
            "where scene boundaries or group addressed speech made that proxy misleading."),
          tags$li(strong("Group conversations."),
            " When one character addresses multiple characters simultaneously, ",
            "the word count is divided equally across all targets. ",
            "Multiplying would artificially inflate a speaker's total weight every time they address a group, ",
            "making group speeches appear to create more total verbal investment than individual ones. ",
            "Dividing keeps the speaker's total verbal budget constant and distributes it across each relationship formed. ",
            "This is an approximation since when the transcript did not specify a primary listener, ",
            "equal division can avoid over-assigning the entire speech to one character."),
          tags$li(strong("Self-loops."),
            " Seventeen instances where a character spoke without an identifiable audience ",
            "(monologue turns) were removed from the edge list. ",
            "Self-loops do not represent character-to-character interaction and would inflate degree centrality."),
          tags$li(strong("Egg's social class."),
            " Egg (Aegon Targaryen) travels in disguise as a commoner squire throughout Episodes 1 to 3 in Season 1. ",
            "He is coded as Royalty based on his actual identity which is confirmed by the end of the season. ",
            "His performed class and actual class do not align neatly thus a sensitivity check coding Egg as Commoner ",
            "would be needed to test how much this choice changes class assortativity.")
        ),

        h5("Measurements"),
        p("Four node level measures are calculated on the full directed graph before any filtering:"),
        tags$ul(
          tags$li(strong("In-degree:"), " number of distinct incoming dialogue ties or how many other characters speak to this character."),
          tags$li(strong("Out-degree:"), " number of distinct outgoing dialogue ties or how many other characters this character speaks to."),
          tags$li(strong("Betweenness centrality:"), " normalized measure of how often a node lies on shortest paths between other nodes. Calculated on the unweighted directed graph so that structural position determines the score instead of word volume."),
          tags$li(strong("Constraint:"), " measures how interconnected a character's contacts are. Low constraint means contacts come from different and non-overlapping groups. Because the original network is directed, constraint here should be read as an approximation of brokerage rather than a complete measure of social support.")
        ),
        p("Three network level measures: density, class assortativity (nominal, categorical by social_class, directed = TRUE), ",
          "and Louvain modularity (run on the weighted undirected projection)."),

        h5("Interpretive caution"),
        p("Because the network is built from dialogue it measures spoken interaction rather than emotional closeness. ",
          "A high edge weight means more observable words exchanged and not necessarily a stronger personal bond."),

        hr(),
        h5("References"),
        tags$ul(style = "font-size:0.88em;",
          tags$li("Burt, R. S. (2004). Structural holes and good ideas. ", em("American Journal of Sociology"), ", 110(2), 349\u2013399."),
          tags$li("Granovetter, M. S. (1973). The strength of weak ties. ", em("American Journal of Sociology"), ", 78(6), 1360\u20131380."),
          tags$li("McPherson, M., Smith-Lovin, L., & Cook, J. M. (2001). Birds of a feather: Homophily in social networks. ", em("Annual Review of Sociology"), ", 27, 415\u2013444."),
          tags$li("Jackson, M. O. (2019). ", em("The Human Network"), ". Pantheon Books. [Chapter 2]")
        ),
        hr(),
        p("Transcript source: episode transcript files. All word counts extracted programmatically using R.",
          style = "font-size:0.83em; color:#888;")
      ),
      column(4,
        div(style = "background:#f8f5f0; border-left:4px solid #4A708B;
                     padding:15px; margin-top:20px; border-radius:4px;",
          h5("Data summary", style = "margin-top:0;"),
          tags$table(class = "table table-sm", style = "font-size:0.9em;",
            tags$tbody(
              tags$tr(tags$td("Episodes"),              tags$td(strong("6"))),
              tags$tr(tags$td("Final nodes"),            tags$td(strong(vcount(g)))),
              tags$tr(tags$td("Final edges"),            tags$td(strong(ecount(g)))),
              tags$tr(tags$td("Removed: cross scene"),  tags$td(strong("9"))),
              tags$tr(tags$td("Removed: self loops"),   tags$td(strong("17"))),
              tags$tr(tags$td("Max weight"),       tags$td(strong(paste(max(edges_df$weight), "words")))),
              tags$tr(tags$td("Median weight"),    tags$td(strong(paste(median(edges_df$weight), "words"))))
            )
          ),
          hr(),
          h5("Class breakdown"),
          tags$table(class = "table table-sm", style = "font-size:0.9em;",
            tags$tbody(
              tags$tr(
                tags$td(span("\u25cf", style = paste0("color:", class_colors["Royalty"], ";"))),
                tags$td("Royalty"),
                tags$td(strong(sum(nodes_df$social_class == "Royalty")))
              ),
              tags$tr(
                tags$td(span("\u25cf", style = paste0("color:", class_colors["Noble"], ";"))),
                tags$td("Noble"),
                tags$td(strong(sum(nodes_df$social_class == "Noble")))
              ),
              tags$tr(
                tags$td(span("\u25cf", style = paste0("color:", class_colors["Knight"], ";"))),
                tags$td("Knight"),
                tags$td(strong(sum(nodes_df$social_class == "Knight")))
              ),
              tags$tr(
                tags$td(span("\u25cf", style = paste0("color:", class_colors["Commoner"], ";"))),
                tags$td("Commoner"),
                tags$td(strong(sum(nodes_df$social_class == "Commoner")))
              )
            )
          )
        )
      )
    )
  ),

  tabPanel("Network",
    sidebarLayout(
      sidebarPanel(width = 3,
        h5("Node size"),
        selectInput("net_measure", NULL,
          choices  = c("In Degree"   = "degree_in",
                       "Out Degree"  = "degree_out",
                       "Betweenness" = "betweenness"),
          selected = "betweenness"
        ),
        hr(),
        h5("Minimum edge weight (words)"),
        sliderInput("weight_min", NULL,
          min = 1, max = max(edges_df$weight), value = 100, step = 25
        ),
        hr(),
        div(style = "font-size:0.82em; color:#444; line-height:1.9;",
          strong("Edge color = speaker's class:"), br(),
          span("\u2501\u2501", style = paste0("color:", edge_colors["Royalty"], "; font-size:1.1em;")),
          " Royalty", br(),
          span("\u2501\u2501", style = paste0("color:", edge_colors["Noble"], "; font-size:1.1em;")),
          " Noble", br(),
          span("\u2501\u2501", style = paste0("color:", edge_colors["Knight"], "; font-size:1.1em;")),
          " Knight", br(),
          span("\u2501\u2501", style = paste0("color:", edge_colors["Commoner"], "; font-size:1.1em;")),
          " Commoner", br(), br(),
          "Edge width = word count / a limited proxy for verbal investment"
        )
      ),
      mainPanel(width = 9,
        visNetworkOutput("vis_net", height = "600px"),
        br(),
        p("Nodes are arranged by feudal tier as an interpretive layout choice, not a calculated network measure.",
             " Spatial distance in the visualization should not be interpreted as network distance.",
             " Edge color = speaker's class / width = word count.",
             " Hover over a node to see stats. Drag to rearrange.",
          style = "color:grey; font-size:0.83em;")
      )
    )
  ),

  tabPanel("Centrality",
    sidebarLayout(
      sidebarPanel(width = 3,
        h5("Select measure"),
        radioButtons("bar_measure", NULL,
          choices  = c("In Degree"   = "degree_in",
                       "Out Degree"  = "degree_out",
                       "Betweenness" = "betweenness",
                       "Constraint"  = "constraint"),
          selected = "betweenness"
        )
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
                     background:#fdf3f3; border-radius:4px;
                     box-shadow:0 1px 3px rgba(0,0,0,0.07);",
          h5("Density", style = "margin-top:0; color:#666;"),
          h2(density_val, style = "color:#8B1A1A; margin:4px 0;"),
          p("Only ", strong(paste0(round(density_val * 100, 1), "%")),
            " of all possible directed conversations actually occur, meaning most character pairs never directly speak. ",
            "Television dialogue is also constrained by scene structure and plot, ",
            "so this sparse network does not by itself indicate class based separation. ",
            "The assortativity score shows whether the conversations that do exist tend to stay within the same class; ",
            "the centrality measures show which characters sit at the center of those interactions.",
            style = "font-size:0.85em; color:#555; margin-bottom:0;")
        )
      ),
      column(4,
        div(style = "border-left:4px solid #B8860B; padding:12px 15px;
                     background:#fdf9ec; border-radius:4px;
                     box-shadow:0 1px 3px rgba(0,0,0,0.07);",
          h5("Class Assortativity", style = "margin-top:0; color:#666;"),
          h2(assort_val, style = "color:#B8860B; margin:4px 0;"),
          p("A positive value suggests that same class ties occur slightly more often than random mixing would predict. ",
            "The value is well below 1.0, meaning class boundaries are present but not absolute. ",
            "One possible explanation is that the Trial by Seven creates cross class interaction that weakens class separation.",
            style = "font-size:0.85em; color:#555; margin-bottom:0;")
        )
      ),
      column(4,
        div(style = "border-left:4px solid #4A708B; padding:12px 15px;
                     background:#f0f5f8; border-radius:4px;
                     box-shadow:0 1px 3px rgba(0,0,0,0.07);",
          h5("Modularity (Louvain)", style = "margin-top:0; color:#666;"),
          h2(mod_val, style = "color:#4A708B; margin:4px 0;"),
          p("In the weighted undirected version of the network, Louvain community detection found only ",
            strong(n_comms), " communities, not four class based clusters. ",
            "Because speech direction is collapsed in this version, the result reflects conversational clustering ",
            "rather than a direct test of feudal hierarchy.",
            style = "font-size:0.85em; color:#555; margin-bottom:0;")
        )
      )
    ),
    fluidRow(
      column(12,
        br(),
        radioButtons("analysis_view", NULL,
          choices = c("Louvain Community Detection" = "louvain",
                      "Network without Dunk"        = "removal"),
          selected = "louvain", inline = TRUE
        ),
        conditionalPanel("input.analysis_view == 'louvain'",
          p("Node color = detected community. Compare to the Network tab where color = social class. ",
            "The algorithm found ", strong(n_comms), " communities based purely on dialogue patterns.",
            style = "font-size:0.85em; color:#555;")
        ),
        conditionalPanel("input.analysis_view == 'removal'",
          p("Nodes in the ", strong("main connected group"), " are shown in grey. ",
            strong(style = "color:#CC0000;", "Red nodes"), " are completely disconnected after Dunk is removed. ",
            "Hover over any node to see its name.",
            style = "font-size:0.85em; color:#555;")
        ),
        visNetworkOutput("analysis_net", height = "520px")
      )
    )
  )

)

server <- function(input, output, session) {

  output$raymun_btwn <- renderText({
    val <- nodes_full |> filter(name == "Raymun Fossoway") |> pull(betweenness)
    round(val, 3)
  })

  output$analysis_net <- renderVisNetwork({
    tier <- c("Royalty" = 1, "Noble" = 2, "Knight" = 3, "Commoner" = 4)

    if (input$analysis_view == "removal") {
      g_no_dunk <- delete_vertices(g, "Dunk")
      comp      <- components(g_no_dunk)
      main_comp <- which.max(comp$csize)
      n_tbl2 <- data.frame(
        name         = V(g_no_dunk)$name,
        social_class = V(g_no_dunk)$social_class,
        comp_id      = comp$membership,
        stringsAsFactors = FALSE
      )
      nodes2 <- n_tbl2 |>
        mutate(
          id    = name,
          label = name,
          level = tier[social_class],
          color = ifelse(comp_id == main_comp, "#AAAAAA", "#CC0000"),
          font.color = ifelse(comp_id == main_comp, "#222222", "#ffffff"),
          title = paste0("<b>", name, "</b><br>Class: ", social_class,
                         ifelse(comp_id == main_comp, "", "<br><b>Disconnected</b>"))
        )
      e2 <- as_data_frame(g_no_dunk, what = "edges")
      edges2 <- if (nrow(e2) == 0) {
        data.frame(from = character(), to = character())
      } else {
        e2 |> mutate(color = "#CCCCCC", width = 0.6)
      }
      visNetwork(nodes2, edges2) |>
        visNodes(borderWidth = 1.5, font = list(size = 12)) |>
        visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.35)),
                 smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.2)) |>
        visHierarchicalLayout(direction = "UD", levelSeparation = 160, nodeSpacing = 110) |>
        visOptions(highlightNearest = FALSE) |>
        visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = FALSE,
                       navigationButtons = TRUE)

    } else {
      comm_colors <- setNames(
        c("#4A708B", "#B8860B", "#8B1A1A", "#5B8B5B")[seq_len(n_comms)],
        paste0("C", seq_len(n_comms))
      )
      mem <- membership(comms)
      nodes_l <- nodes_full |>
        mutate(
          id         = name,
          label      = paste0(name, "\n(C", mem[name], ")"),
          community  = paste0("C", mem[name]),
          color      = comm_colors[paste0("C", mem[name])],
          level      = tier[social_class],
          title      = paste0("<b>", name, "</b><br>Community: ", mem[name],
                              "<br>Class: ", social_class)
        )
      edges_l <- edges_df |>
        mutate(color = "#CCCCCC",
               width = scales::rescale(weight, to = c(0.4, 4)))
      visNetwork(nodes_l, edges_l) |>
        visNodes(borderWidth = 1.5, font = list(size = 11)) |>
        visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.35)),
                 smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.2)) |>
        visHierarchicalLayout(direction = "UD", levelSeparation = 160, nodeSpacing = 110) |>
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) |>
        visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = FALSE,
                       navigationButtons = TRUE)
    }
  })

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

    tier <- c("Royalty" = 1, "Noble" = 2, "Knight" = 3, "Commoner" = 4)
    idx  <- n_tbl$name
    n2c  <- setNames(n_tbl$social_class, n_tbl$name)

    full_stats <- nodes_full |>
      select(name, degree_in, degree_out, betweenness, constraint)

    n_tbl <- n_tbl |>
      select(name, social_class, house_affiliation) |>
      left_join(full_stats, by = "name")

    measure_names <- c(
      degree_in  = "In Degree",
      degree_out = "Out Degree",
      betweenness = "Betweenness"
    )

    nodes_vis <- n_tbl |>
      mutate(
        id    = name,
        label = name,
        level = tier[social_class],
        color = class_colors[social_class],
        measure_value = .data[[input$net_measure]],
        value = ifelse(
          max(measure_value, na.rm = TRUE) == min(measure_value, na.rm = TRUE),
          20,
          scales::rescale(measure_value, to = c(10, 45))
        ),
        title = paste0("<b>", name, "</b><br>",
                       "Class: ", social_class, "<br>",
                       "Node size: ", measure_names[[input$net_measure]], "<br>",
                       "In-degree: ",   degree_in,  "<br>",
                       "Out-degree: ",  degree_out, "<br>",
                       "Betweenness: ", round(betweenness, 3),
                       "<br><i>(full network values)</i>")
      )

    edges_vis <- e_tbl |>
      mutate(
        from_name = idx[from], to_name = idx[to],
        from  = from_name, to = to_name,
        title = paste0("<b>", from_name, " \u2192 ", to_name, "</b><br>Words: ", weight),
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
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) |>
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
      degree_in   = "In Degree — How many other characters speak to this character",
      degree_out  = "Out Degree — How many other characters this character speaks to",
      betweenness = "Betweenness Centrality (normalized) — Brokerage across shortest paths",
      constraint  = "Constraint — Lower = less redundant contacts"
    )

    plot_df <- if (m == "constraint") {
      nodes_full |>
        filter(!is.na(constraint)) |>
        arrange(constraint) |>
        mutate(name = factor(name, levels = rev(name)))
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
