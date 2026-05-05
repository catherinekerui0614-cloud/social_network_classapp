# ══════════════════════════════════════════════════════════════════════════════
# app.R — A Knight of the Seven Kingdoms: Dialogue Network
# SOCI 0226 Social Networks | Spring 2026
#
# FILE STRUCTURE REQUIRED:
#   /your_project_folder/
#       app.R
#       data/
#           edges_final_corrected.csv
#           nodes_final.csv
# ══════════════════════════════════════════════════════════════════════════════

library(shiny)
library(bslib)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(visNetwork)

# ─────────────────────────────────────────────────────────────────────────────
# DATA SETUP — runs once at launch
# graph_from_data_frame pattern from ClassScript_2 and ClassScript_5
# ─────────────────────────────────────────────────────────────────────────────

edges_raw <- read.csv("edges_final_corrected.csv")
nodes_raw <- read.csv("nodes_final.csv")

# graph_from_data_frame: first column of vertices is used as vertex names
nodes_df <- nodes_raw |>
  select(name = label, id, gender, social_class, house_affiliation)

edges_df <- edges_raw |>
  rename(from = Source, to = Target, weight = Weight)

# Directed, weighted igraph object
g <- graph_from_data_frame(d = edges_df, vertices = nodes_df, directed = TRUE)

# ── Centrality measures ───────────────────────────────────────────────────────
# in/out degree: ClassScript_4_revised
# betweenness normalized = TRUE: ClassScript_6
# node_constraint: Week 6, Burt structural holes

g_tidy <- as_tbl_graph(g) |>
  activate(nodes) |>
  mutate(
    degree_in    = centrality_degree(mode = "in"),
    degree_out   = centrality_degree(mode = "out"),
    degree_total = centrality_degree(mode = "all"),
    betweenness  = centrality_betweenness(normalized = TRUE),
    constraint   = node_constraint()
  )

nodes_full <- g_tidy |> activate(nodes) |> as_tibble()

# ── Network-level measures ────────────────────────────────────────────────────

# Directed density: m / n(n-1)  — ClassScript_3 formula
density_val <- round(edge_density(g), 3)

# Assortativity by social class — ClassScript_7: assortativity_nominal()
assort_val <- round(
  assortativity_nominal(g, as.integer(as.factor(V(g)$social_class))), 3
)

# Community detection: Louvain on undirected — ClassScript_8: cluster_louvain()
set.seed(42)
g_und <- as.undirected(g, mode = "collapse",
                        edge.attr.comb = list(weight = "sum"))
comms   <- cluster_louvain(g_und, weights = E(g_und)$weight)
mod_val <- round(modularity(comms), 3)
n_comms <- length(unique(membership(comms)))

# Community vector keyed by vertex name (robust to ordering)
comm_vec  <- setNames(as.character(membership(comms)), V(g_und)$name)

# Add community to full graph and node table
g_tidy <- g_tidy |>
  activate(nodes) |>
  mutate(community = factor(comm_vec[name]))

nodes_full$community <- factor(comm_vec[nodes_full$name])

# ── Custom color palette ──────────────────────────────────────────────────────
class_colors <- c(
  "Royalty"  = "#8B1A1A",   # deep crimson
  "Noble"    = "#B8860B",   # dark gold
  "Knight"   = "#4A708B",   # steel blue
  "Commoner" = "#8B7355"    # earth brown
)
comm_colors <- c("#C0392B", "#2E86AB", "#27AE60", "#8E44AD", "#F39C12")

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────

ui <- navbarPage(
  title = "⚔  A Knight of the Seven Kingdoms: Dialogue Network",
  theme = bs_theme(bootswatch = "flatly", primary = "#8B1A1A", font_scale = 0.95),
  collapsible = TRUE,

  # ── TAB 1: INTRODUCTION ────────────────────────────────────────────────────
  tabPanel("Introduction",
    fluidRow(
      column(8,
        h2("Who Speaks to Whom in Westeros?", style = "margin-top:20px; color:#8B1A1A;"),
        p("This app maps the dialogue network of Season 1 of ",
          em("A Knight of the Seven Kingdoms"), " (HBO, 2025). ",
          "Each of the ", strong("27 nodes"), " is a named speaking character. ",
          "Each of the ", strong("95 directed edges"), " points from speaker to listener, ",
          "weighted by word count. The network is directed because in a feudal society, ",
          "who talks to whom — and how much — is not neutral. It reflects rank, obligation, and access."),
        hr(),
        h4("Finding 1: Dunk is the network's bottleneck, not just its center",
           style = "color:#8B1A1A;"),
        p("Dunk's betweenness score is ",
          strong("0.76"), " — meaning he sits on 76% of all shortest paths between any two characters. ",
          "Egg is second at 0.13. This gap matters: Dunk is not just well-connected, he is the ",
          "structural link between two worlds that otherwise barely speak to each other — the commoners ",
          "and the royals. Without him, those two halves of the network are largely disconnected. ",
          "Go to the Centrality tab and toggle between In-Degree and Betweenness: the character ",
          "most spoken to is not the same as the character holding the network together."),
        hr(),
        h4("Finding 2: Class boundaries exist in the network, but they're weaker than expected",
           style = "color:#B8860B;"),
        p("The assortativity score by social class is ", strong(assort_val), ". ",
          "A positive number means characters do tend to talk within their own class — lords to lords, ",
          "commoners to commoners. But 0.16 is far from 1.0. That gap reflects something real about the show: ",
          "the Trial by Seven forces cross-class alliances that would never happen under normal feudal rules. ",
          "Egg makes this possible. He moves between his royal family and the common world, carrying ",
          "information and relationships across a divide that the social structure would otherwise keep sealed."),
        hr(),
        h4("How to use this app", style = "color:#4A708B;"),
        tags$ul(
          tags$li(strong("Network:"), " choose what drives node size, switch between the full network and a single character's connections, and filter out weaker edges."),
          tags$li(strong("Centrality:"), " compare how differently each character ranks depending on which measure you use."),
          tags$li(strong("Network Measures:"), " density, class assortativity, community detection, and constraint.")
        )
      ),
      column(4,
        div(
          style = "background:#f8f5f0; border-left:4px solid #8B1A1A; padding:15px; margin-top:20px; border-radius:4px;",
          h4("At a glance", style = "margin-top:0;"),
          tags$table(class = "table table-sm", style = "font-size:0.9em;",
            tags$tbody(
              tags$tr(tags$td("Characters"),          tags$td(strong(vcount(g)))),
              tags$tr(tags$td("Dialogue edges"),      tags$td(strong(ecount(g)))),
              tags$tr(tags$td("Density"),             tags$td(strong(density_val))),
              tags$tr(tags$td("Class assortativity"), tags$td(strong(assort_val))),
              tags$tr(tags$td("Modularity"),          tags$td(strong(mod_val))),
              tags$tr(tags$td("Communities"),         tags$td(strong(n_comms))),
              tags$tr(tags$td("Directed"),            tags$td(strong("Yes"))),
              tags$tr(tags$td("Edge weight"),         tags$td(strong("Word count")))
            )
          ),
          hr(),
          h5("Color key"),
          p(span("●", style = paste0("color:", class_colors["Royalty"], "; font-size:1.2em;")),
            " Royalty  ",
            span("●", style = paste0("color:", class_colors["Noble"], "; font-size:1.2em;")),
            " Noble"),
          p(span("●", style = paste0("color:", class_colors["Knight"], "; font-size:1.2em;")),
            " Knight  ",
            span("●", style = paste0("color:", class_colors["Commoner"], "; font-size:1.2em;")),
            " Commoner")
        )
      )
    )
  ),

  # ── TAB 2: NETWORK VISUALIZATION ──────────────────────────────────────────
  tabPanel("Network",
    sidebarLayout(
      sidebarPanel(width = 3,
        h5("View Mode"),
        radioButtons("view_mode", NULL,
          choices  = c("Full Network" = "full", "Ego Network" = "ego"),
          selected = "full"
        ),
        conditionalPanel(
          condition = "input.view_mode == 'ego'",
          selectInput("ego_char", "Select character:",
            choices  = sort(nodes_full$name),
            selected = "Dunk"
          ),
          helpText(em("Shows only this character's direct connections."))
        ),
        hr(),
        h5("Node size"),
        selectInput("net_measure", "Size nodes by:",
          choices  = c("In-Degree"    = "degree_in",
                       "Out-Degree"   = "degree_out",
                       "Total Degree" = "degree_total",
                       "Betweenness"  = "betweenness"),
          selected = "betweenness"
        ),
        hr(),
        h5("Minimum edge weight"),
        sliderInput("weight_min", NULL,
          min = 1, max = 300, value = 100, step = 10
        ),
        hr(),
        div(style = "font-size:0.82em; color:#444; line-height:1.8;",
          strong("Edge color = class of the speaker:"), br(),
          span("━━", style = paste0("color:", class_colors["Royalty"],  "; font-size:1.1em;")), " Royalty", br(),
          span("━━", style = paste0("color:", class_colors["Noble"],    "; font-size:1.1em;")), " Noble", br(),
          span("━━", style = paste0("color:", class_colors["Knight"],   "; font-size:1.1em;")), " Knight", br(),
          span("━━", style = paste0("color:", class_colors["Commoner"], "; font-size:1.1em;")), " Commoner", br(),
          em("Edge width = word count")
        )
      ),
      mainPanel(width = 9,
        visNetworkOutput("vis_net", height = "620px"),
        br(),
        uiOutput("net_caption")
      )
    )
  ),

  # ── TAB 3: CENTRALITY ─────────────────────────────────────────────────────
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
        hr(),
        helpText(strong("In-Degree:"), " how often others speak to this character. High in-degree in a feudal setting usually signals rank — but not always."),
        br(),
        helpText(strong("Out-Degree:"), " how often this character initiates speech. Those lower in the hierarchy often have to work harder to be heard."),
        br(),
        helpText(strong("Betweenness:"), " how often this character sits between others on the shortest path. High betweenness = the network depends on you to pass information across groups."),
        br(),
        helpText(strong("Constraint:"), " how closed-off a character's connections are. Low constraint means your contacts don't all know each other — you're bridging separate worlds.")
      ),
      mainPanel(width = 9,
        plotOutput("centrality_bar", height = "530px"),
        br(),
        uiOutput("centrality_interp")
      )
    )
  ),

  # ── TAB 4: NETWORK MEASURES ────────────────────────────────────────────────
  tabPanel("Network Measures",
    fluidRow(
      column(4,
        div(style = "border-left:4px solid #8B1A1A; padding:12px 15px; background:#fff; margin:10px 5px; border-radius:3px; box-shadow:0 1px 3px rgba(0,0,0,0.07);",
          h5("Density", style = "margin-top:0; color:#666;"),
          h2(density_val, style = "color:#8B1A1A; margin:4px 0;"),
          p("Only 13.5% of all possible directed conversations exist in this network. Most characters never directly speak to most other characters — which is exactly what you'd expect in a rigid class society where interaction is largely limited by rank and proximity. The few characters who do cross those lines become disproportionately important.", style = "font-size:0.85em; color:#444;")
        )
      ),
      column(4,
        div(style = "border-left:4px solid #B8860B; padding:12px 15px; background:#fff; margin:10px 5px; border-radius:3px; box-shadow:0 1px 3px rgba(0,0,0,0.07);",
          h5("Assortativity (Social Class)", style = "margin-top:0; color:#666;"),
          h2(assort_val, style = "color:#B8860B; margin:4px 0;"),
          p("Characters do tend to talk within their own class — lords to lords, commoners to commoners. But 0.16 is well below 1.0, which means the pattern isn't that clean. The show deliberately breaks the feudal norm: the Trial by Seven pulls people together who would otherwise never interact, and the assortativity score captures that tension between structure and disruption.", style = "font-size:0.85em; color:#444;")
        )
      ),
      column(4,
        div(style = "border-left:4px solid #4A708B; padding:12px 15px; background:#fff; margin:10px 5px; border-radius:3px; box-shadow:0 1px 3px rgba(0,0,0,0.07);",
          h5("Modularity (Louvain)", style = "margin-top:0; color:#666;"),
          h2(mod_val, style = "color:#4A708B; margin:4px 0;"),
          p(paste0("The algorithm found ", n_comms, " communities — but they don't map neatly onto the four social classes. Low modularity (0.03) means the detected groups are loosely separated. Rather than four insular class clusters, the network is surprisingly integrated, with key characters actively connecting across what should be rigid social divides."), style = "font-size:0.85em; color:#444;")
        )
      )
    ),
    hr(),
    fluidRow(
      column(6,
        h4("Do communities follow class lines?"),
        p(em("Node color = algorithmically detected community. If the network were truly class-segregated, you'd expect each community to match a social tier. See how well that holds."),
          style = "color:grey; font-size:0.83em; margin-top:0;"),
        plotOutput("community_plot", height = "430px")
      ),
      column(6,
        h4("Who bridges disconnected worlds?"),
        p(em("Constraint measures how much your contacts already know each other. Low = you connect groups that wouldn't otherwise meet. Dashed line = network average."),
          style = "color:grey; font-size:0.83em; margin-top:0;"),
        plotOutput("constraint_bar", height = "430px")
      )
    )
  ),

  # ── TAB 5: DATA COLLECTION ─────────────────────────────────────────────────
  tabPanel("Data",
    fluidRow(
      column(7,
        h3("How the data was collected", style = "margin-top:20px; color:#8B1A1A;"),
        h5("Source"),
        p("Dialogue was drawn from publicly available transcripts for all six episodes of Season 1 of ",
          em("A Knight of the Seven Kingdoms"), " (HBO, 2025), accessed via Scraps from the Loft. ",
          "Character attributes — gender, social class, house — were verified against A Wiki of Ice and Fire and the Game of Thrones Wiki."),
        h5("Nodes"),
        p("27 named speaking characters. Excluded: 11 non-verbal transcript labels (e.g., [chuckles], [sighs]) and 14 unnamed or crowd entries (e.g., Man 1, soldier). Only characters with actual named dialogue lines were kept."),
        h5("How edges were extracted"),
        p("Each transcript was processed in R. A regex pattern matched bracketed speaker tags ([Character Name]). The next speaker in the same scene was assigned as the target — the assumption being that when someone speaks in a conversation, they are addressing the person who responds."),
        p("Scene boundaries were identified manually. Nine edges were deleted where the lead() function incorrectly paired the last speaker of one scene with the first speaker of the next — these were conversations that never actually happened."),
        h5("Edge weight"),
        p("Weight = word count of the dialogue spoken from source to target. A long speech counts more than a one-word reply, which better reflects how much conversational investment went into a relationship. For scenes where a character addressed a group, the word count was split equally among the identified targets — this keeps the total verbal investment fixed rather than inflating every individual relationship."),
        h5("Self-loops"),
        p("Turns where a character spoke alone — Dunk talking to his horse, for instance — were removed. They don't represent interaction between two people and would inflate degree counts for no meaningful reason."),
        h5("Final dataset"),
        p(strong("27 characters | 95 directed edges | weights = word count")),
        h5("References"),
        tags$ul(
          tags$li("Elson, D.K., Dames, N., & McKeown, K.R. (2010). Extracting Social Networks from Literary Fiction. ACL 2010."),
          tags$li("Granovetter, M.S. (1973). The Strength of Weak Ties. American Journal of Sociology."),
          tags$li("Burt, R.S. (2004). Structural Holes and Good Ideas. American Journal of Sociology."),
          tags$li("McPherson, M., Smith-Lovin, L., & Cook, J.M. (2001). Birds of a Feather. Annual Review of Sociology.")
        )
      ),
      column(4, offset = 1,
        div(style = "background:#f8f5f0; padding:15px; border-radius:4px; margin-top:20px;",
          h5("Node Attributes"),
          tags$table(class = "table table-sm table-bordered",
            tags$thead(tags$tr(tags$th("Column"), tags$th("Description"))),
            tags$tbody(
              tags$tr(tags$td("label"),             tags$td("Character name")),
              tags$tr(tags$td("gender"),            tags$td("M / W")),
              tags$tr(tags$td("social_class"),      tags$td("Royalty / Noble / Knight / Commoner")),
              tags$tr(tags$td("house_affiliation"), tags$td("Targaryen, Fossoway, etc."))
            )
          ),
          hr(),
          h5("Edge Attributes"),
          tags$table(class = "table table-sm table-bordered",
            tags$thead(tags$tr(tags$th("Column"), tags$th("Description"))),
            tags$tbody(
              tags$tr(tags$td("Source"), tags$td("Speaking character")),
              tags$tr(tags$td("Target"), tags$td("Character spoken to")),
              tags$tr(tags$td("Weight"), tags$td("Word count of that dialogue turn"))
            )
          )
        )
      )
    )
  )
)

# ─────────────────────────────────────────────────────────────────────────────
# SERVER
# ─────────────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Dynamic element: reactive filtered/ego network ────────────────────────
  # Rebuilds the subgraph on every control change.
  # In ego mode, mirrors ClassScript_2's make_ego_graph() logic using tidygraph filters.
  filtered_net <- reactive({
    base <- g_tidy |>
      activate(edges) |>
      filter(weight >= input$weight_min) |>
      activate(nodes) |>
      # Remove nodes with no remaining edges after weight filtering
      filter(centrality_degree(mode = "all") > 0)

    if (input$view_mode == "ego") {      # Ego network: keep focal node + all nodes it shares an edge with (order 1)
      focal <- input$ego_char
      base |>
        activate(nodes) |>
        filter(name == focal | name %in% {
          e <- base |> activate(edges) |> as_tibble()
          n <- base |> activate(nodes) |> as_tibble() |> pull(name)
          focal_idx <- which(n == focal)
          neighbors <- unique(c(e$from[e$from == focal_idx | e$to == focal_idx],
                                e$to[e$from   == focal_idx | e$to == focal_idx]))
          n[neighbors]
        }) |>
        activate(nodes) |>
        mutate(
          degree_in    = centrality_degree(mode = "in"),
          degree_out   = centrality_degree(mode = "out"),
          degree_total = centrality_degree(mode = "all"),
          betweenness  = centrality_betweenness(normalized = TRUE)
        )
    } else {
      base |>
        activate(nodes) |>
        mutate(
          degree_in    = centrality_degree(mode = "in"),
          degree_out   = centrality_degree(mode = "out"),
          degree_total = centrality_degree(mode = "all"),
          betweenness  = centrality_betweenness(normalized = TRUE)
        )
    }
  })

  # ── visNetwork: handles BOTH full network and ego mode ───────────────────
  # Full mode: visHierarchicalLayout with level = social class tier.
  #   → nodes spread into 4 rows (Royalty top, Commoner bottom), arrows visible.
  # Ego mode: focal node pinned at center, neighbors on a circle, physics off.
  # Both modes: edge width = word count, edge color = speaker's social class.
  # Pattern from class app.R: visNetwork + visNodes + visEdges + visPhysics
  output$vis_net <- renderVisNetwork({
    net   <- filtered_net()
    n_tbl <- net |> activate(nodes) |> as_tibble()
    e_tbl <- net |> activate(edges) |> as_tibble()

    if (nrow(n_tbl) == 0) return(visNetwork(data.frame(), data.frame()))

    idx_to_name   <- n_tbl$name
    name_to_class <- setNames(n_tbl$social_class, n_tbl$name)

    edge_class_colors <- c(
      "Royalty"  = "#D32F2F",
      "Noble"    = "#E8961A",
      "Knight"   = "#1565C0",
      "Commoner" = "#795548"
    )

    # ── Build edges (same for both modes) ──────────────────────────────────
    edges_vis <- e_tbl |>
      mutate(
        from_name = idx_to_name[from],
        to_name   = idx_to_name[to],
        from  = from_name,
        to    = to_name,
        title = paste0("<b>", from_name, " → ", to_name, "</b><br>Words: ", weight),
        width = case_when(
          weight >= 400 ~ 8,
          weight >= 200 ~ 5,
          weight >= 100 ~ 2.5,
          TRUE          ~ 1
        ),
        color = edge_class_colors[name_to_class[from_name]]
      ) |>
      select(-from_name, -to_name)

    if (input$view_mode == "full") {
      # ── FULL NETWORK: hierarchical layout by social class tier ────────────
      # visHierarchicalLayout + node$level assigns each node to a row.
      # Royalty=1 (top) → Commoner=4 (bottom). Direction arrows stay visible.
      tier_level <- c("Royalty" = 1, "Noble" = 2, "Knight" = 3, "Commoner" = 4)

      nodes_vis <- n_tbl |>
        mutate(
          id    = name,
          label = name,
          level = tier_level[social_class],
          color = class_colors[social_class],
          group = social_class,
          value = (.data[[input$net_measure]] + 0.01) * 2,
          title = paste0(
            "<b>", name, "</b><br>",
            "Class: ", social_class, "<br>",
            "In-degree: ",  degree_in,  "<br>",
            "Out-degree: ", degree_out, "<br>",
            "Betweenness: ", round(betweenness, 3)
          )
        )

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
          # curvedCW separates A→B from B→A visually even in hierarchical layout
          smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.25)
        ) |>
        visOptions(
          # Full network: clicking a node should NOT cascade-highlight Dunk's
          # unrelated edges. The Ego Network tab is the right place for exploration.
          highlightNearest = FALSE
        ) |>
        visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE,
                       tooltipDelay = 80) |>
        # Hierarchical layout: 4 tiers, nodes spread horizontally within each tier
        visHierarchicalLayout(
          direction       = "UD",        # Up → Down
          sortMethod      = "directed",
          levelSeparation = 180,         # vertical gap between tiers
          nodeSpacing     = 130          # horizontal gap between nodes in same tier
        )

    } else {
      # ── EGO NETWORK: focal node at center, neighbors on circle ────────────
      focal      <- input$ego_char
      n_neighbors <- nrow(n_tbl) - 1
      angles <- if (n_neighbors > 0)
        seq(0, 2 * pi, length.out = n_neighbors + 1)[-(n_neighbors + 1)]
      else numeric(0)
      radius <- 280

      nodes_vis <- n_tbl |>
        mutate(
          id    = name,
          label = name,
          color = class_colors[social_class],
          group = social_class,
          value = (.data[[input$net_measure]] + 0.01) * 3,
          title = paste0(
            "<b>", name, "</b><br>",
            "Class: ", social_class, "<br>",
            "In-degree: ",  degree_in,  "<br>",
            "Out-degree: ", degree_out, "<br>",
            "Betweenness: ", round(betweenness, 3)
          )
        )

      focal_row  <- which(nodes_vis$name == focal)
      other_rows <- which(nodes_vis$name != focal)
      nodes_vis$x <- NA_real_
      nodes_vis$y <- NA_real_
      nodes_vis$x[focal_row] <- 0
      nodes_vis$y[focal_row] <- 0
      if (length(other_rows) > 0 && length(angles) > 0) {
        nodes_vis$x[other_rows] <- round(radius * cos(angles[seq_along(other_rows)]))
        nodes_vis$y[other_rows] <- round(radius * sin(angles[seq_along(other_rows)]))
      }
      nodes_vis$fixed <- nodes_vis$name == focal

      visNetwork(nodes_vis, edges_vis) |>
        visNodes(
          borderWidth = 2,
          font   = list(size = 13, color = "#111", bold = TRUE),
          shadow = list(enabled = TRUE, size = 5),
          color  = list(
            highlight = list(background = "#FFD700", border = "#333"),
            hover     = list(background = "#FFF9C4", border = "#555")
          )
        ) |>
        visEdges(
          arrows = list(to = list(enabled = TRUE, scaleFactor = 0.55)),
          smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.2)
        ) |>
        visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1)) |>
        visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE,
                       tooltipDelay = 80) |>
        visPhysics(enabled = FALSE)
    }
  })

  # ── Caption below network ─────────────────────────────────────────────────
  output$net_caption <- renderUI({
    if (input$view_mode == "full") {
      p(em("Nodes arranged by feudal tier — Royalty at top, Commoner at bottom. A diagonal edge means someone is speaking across class lines. Edge color = speaker's class; edge width = word count. Hover over any node to see its stats."),
        style = "color:grey; font-size:0.83em;")
    } else {
      p(em("Select a character to see everyone they speak to or hear from directly. Edge color = speaker's class; edge width = word count. Hover for stats."),
        style = "color:grey; font-size:0.83em;")
    }
  })

  # ── Centrality bar chart ──────────────────────────────────────────────────
  # Pattern from ClassScript_3 (bar chart) and ClassScript_5 (fill by attribute)
  output$centrality_bar <- renderPlot({
    m <- input$bar_measure

    label_map <- c(
      degree_in   = "In-Degree — Times Spoken To",
      degree_out  = "Out-Degree — Times Initiating Speech",
      betweenness = "Betweenness Centrality (normalized) — Broker Position",
      constraint  = "Burt's Constraint — Lower = More Structural Holes Spanned"
    )

    # Constraint: ascending order shows most powerful brokers at top
    if (m == "constraint") {
      plot_df <- nodes_full |>
        filter(!is.na(constraint)) |>
        arrange(constraint) |>
        mutate(name = factor(name, levels = name))
    } else {
      plot_df <- nodes_full |>
        arrange(desc(.data[[m]])) |>
        mutate(name = factor(name, levels = rev(name)))
    }

    ggplot(plot_df, aes(x = .data[[m]], y = name, fill = social_class)) +
      geom_col(width = 0.72, alpha = 0.88) +
      scale_fill_manual(values = class_colors, name = "Social Class") +
      labs(title = label_map[m], x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position    = "bottom",
        plot.title         = element_text(size = 13, face = "bold", color = "#333")
      )
  })

  # ── Centrality interpretation text ───────────────────────────────────────
  output$centrality_interp <- renderUI({
    txt <- switch(input$bar_measure,
      "degree_in" = HTML(
        "<b>In-Degree — who gets spoken to</b><br><br>
        In a feudal hierarchy, people approach those above them. So high in-degree partly reflects
        formal rank: Baelor Targaryen sits at the top because others constantly address him.
        But Dunk's in-degree is just as high — not because of rank, but because he's the character
        the narrative converges on. He's sought out by knights, commoners, and royals alike.
        That's a different kind of centrality: not inherited, but earned through being the person
        everyone needs to deal with."
      ),
      "degree_out" = HTML(
        "<b>Out-Degree — who initiates speech</b><br><br>
        Dunk leads here by a wide margin. A hedge knight with no house and no income has to talk
        his way through every situation — negotiating lodging, challenging nobles, asking favors
        from people who have no obligation to listen. High out-degree for someone at the bottom
        of the hierarchy isn't a sign of power; it's a sign of precarity. Compare that to Baelor,
        whose out-degree is low. He doesn't need to initiate — people come to him."
      ),
      "betweenness" = HTML(
        "<b>Betweenness — who holds the network together</b><br><br>
        Dunk's score of 0.76 is the sharpest result in this data. He appears on 76% of the
        shortest paths between any two characters. Without him, the commoner characters and the
        royal characters have almost no connection to each other. Egg ranks second at 0.13,
        and his position makes sense differently: his disguise as a squire lets him move between
        his royal family and the world outside it, acting as a relay between two groups that
        otherwise don't communicate. Neither of them scores highest on in-degree. Betweenness
        and degree measure genuinely different things."
      ),
      "constraint" = HTML(
        "<b>Constraint — how closed off is your network?</b><br><br>
        If all your contacts know each other, you're not bridging anyone — you're just part of
        one closed group. That's high constraint. Dunk and Egg have the lowest constraint scores
        because their contacts come from completely different social worlds that don't overlap.
        A royal, a blacksmith, a hedge knight, a puppet maker — they don't talk to each other
        except through Dunk. Characters at the periphery — those who only spoke within one class
        — have high constraint. Their connections are redundant, and the network doesn't depend
        on them to function."
      )
    )
    div(style = "background:#f8f5f0; border-left:4px solid #8B1A1A; padding:12px 15px; color:#333; border-radius:3px;",
        txt)
  })

  # ── Community detection visualization ────────────────────────────────────
  # cluster_louvain on undirected graph — ClassScript_8 pattern
  output$community_plot <- renderPlot({
    g_comm <- as_tbl_graph(g_und) |>
      activate(nodes) |>
      mutate(
        community    = factor(comm_vec[name]),
        degree_total = centrality_degree(mode = "all"),
        sclass       = V(g)[name]$social_class      # pull social_class from directed g
      )

    ggraph(g_comm, layout = "fr") +
      geom_edge_link(aes(alpha = weight), color = "grey65", show.legend = FALSE) +
      scale_edge_alpha(range = c(0.05, 0.55)) +
      geom_node_point(aes(size = degree_total, color = community), alpha = 0.9) +
      scale_size(range = c(3, 14), guide = "none") +
      scale_color_manual(
        values = comm_colors[seq_len(n_comms)],
        name   = "Louvain Community"
      ) +
      geom_node_text(aes(label = name), size = 2.4, repel = TRUE, color = "grey20") +
      labs(
        caption = paste0(n_comms, " communities detected (modularity = ", mod_val, "). ",
                         "Neither community maps cleanly onto a single social class — ",
                         "the network is more integrated than the feudal hierarchy would predict.")
      ) +
      theme_graph(base_size = 11) +
      theme(
        legend.position = "bottom",
        plot.caption    = element_text(color = "grey45", size = 8)
      )
  })

  # ── Constraint bar (structural holes) ────────────────────────────────────
  output$constraint_bar <- renderPlot({
    avg_c <- mean(nodes_full$constraint, na.rm = TRUE)

    nodes_full |>
      filter(!is.na(constraint)) |>
      arrange(constraint) |>
      mutate(name = factor(name, levels = name)) |>
      ggplot(aes(x = constraint, y = name, fill = social_class)) +
      geom_col(width = 0.72, alpha = 0.88) +
      geom_vline(xintercept = avg_c, linetype = "dashed", color = "grey40", linewidth = 0.7) +
      scale_fill_manual(values = class_colors, name = "Social Class") +
      annotate("text", x = avg_c + 0.012, y = 2,
               label = paste0("avg = ", round(avg_c, 2)),
               color = "grey40", size = 3, hjust = 0) +
      labs(
        title    = "Who bridges disconnected groups?",
        subtitle = "Lower score = connects groups that don't otherwise talk to each other",
        x        = "Constraint score (lower = more bridging)",
        y        = NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position    = "bottom",
        plot.subtitle      = element_text(color = "grey50", size = 9)
      )
  })

}

# ─────────────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
