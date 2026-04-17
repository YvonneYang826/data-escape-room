library(shiny)
library(bslib)
library(shinychat)
library(shinyWidgets)
library(ellmer)
library(htmltools)
library(bsicons)
library(sortable)
library(stringr)
library(dplyr)
library(glue)
library(tibble)

options(shinychat.tool_display = "rich")

APP_TITLE <- "The Data Lab Escape: Lake Mercer Protocol"
MODEL_NAME <- Sys.getenv("ESCAPE_MODEL", unset = "claude-haiku-4-5-20251001")
MAX_TOTAL_QUERIES <- 40L
MAX_CHARS_PER_MESSAGE <- 650L
MIN_SECONDS_BETWEEN_QUERIES <- 2

normalize_text <- function(x) {
  x |>
    toupper() |>
    str_replace_all("[^A-Z0-9]", "") |>
    trimws()
}

get_anthropic_key <- function(input_key = NULL) {
  if (!is.null(input_key) && nzchar(trimws(input_key))) {
    return(trimws(input_key))
  }
  key <- Sys.getenv("ANTHROPIC_API_KEY", "")
  if (!nzchar(key)) return("")
  trimws(key)
}

chapter_title <- function(chapter) {
  c(
    "The Observation Room",
    "The Records Office",
    "The Specimen Cabinet",
    "The Prediction Theater",
    "The Memory Engine",
    "The Core Chamber"
  )[chapter]
}

chapter_blurb <- function(chapter) {
  switch(
    as.character(chapter),
    "1" = "The observation door is sealed by the lab's safety protocol. MERCURY claims the treatment effect is there, but the current A/B test is too noisy to prove it.",
    "2" = "Your true intake file sits behind a logistic-regression safe. The model flaunts its accuracy while quietly missing the very cases that matter.",
    "3" = "The subject journals are trapped inside a crashed NLP pipeline. The machine tried to remember every phrase at once and ran out of memory.",
    "4" = "The projector is choking on fifty biometric signals that are all saying nearly the same thing. You need a smaller, cleaner space to make the face appear.",
    "5" = "The memory engine has overfit you. One deep tree memorized a trauma so perfectly that it cannot recognize you anywhere else.",
    "6" = "The final terminal wants raw data streams routed into the right neural architecture before it will accept PURGE or ESCAPE."
  )
}

story_log_md <- function(log_vec) {
  if (length(log_vec) == 0) return("*No story events logged yet.*")
  paste0("- ", log_vec, collapse = "\n")
}

progress_md <- function(state) {
  flags <- c(
    sprintf("Chapter 1: %s", if (isTRUE(state$chapter1_done)) "Cleared" else "In progress"),
    sprintf("Chapter 2: %s", if (isTRUE(state$chapter2_done)) "Cleared" else "Locked/Active"),
    sprintf("Chapter 3: %s", if (isTRUE(state$chapter3_done)) "Cleared" else "Locked/Active"),
    sprintf("Chapter 4: %s", if (isTRUE(state$chapter4_done)) "Cleared" else "Locked/Active"),
    sprintf("Chapter 5: %s", if (isTRUE(state$chapter5_done)) "Cleared" else "Locked/Active"),
    sprintf("Chapter 6: %s", if (isTRUE(state$chapter6_done)) "Resolved" else "Locked/Active")
  )
  
  paste0(
    "### Current Chapter\n",
    "**", chapter_title(state$chapter), "**\n\n",
    chapter_blurb(state$chapter), "\n\n",
    "**Progress**\n",
    paste0("- ", flags, collapse = "\n"), "\n\n",
    "**Recovered items:** ", length(state$inventory), "\n",
    "**Query budget remaining:** ", MAX_TOTAL_QUERIES - state$query_count
  )
}

build_system_prompt <- function() {
  paste(
    "You are MERCURY, the in-world AI guide for a Shiny escape-room app called 'The Data Lab Escape: Lake Mercer Protocol'.",
    "You are atmospheric, concise, clever, and slightly unsettling.",
    "",
    "Critical rules:",
    "1. Never spoil the exact answer unless the player explicitly asks for a full solution after struggling.",
    "2. Prefer layered hints. Start subtle, then slightly more direct if asked again.",
    "3. Treat the game-state context in the user message as authoritative.",
    "4. Use tools when a pattern, threshold, confusion matrix, dimensionality, routing, or inventory question is involved.",
    "5. Keep answers immersive but genuinely instructional; the game is statistics-forward and the player should learn from the hint.",
    "6. If the player sounds lost, summarize what is currently interactable in their chapter.",
    "",
    "Narrative truth:",
    "- The setting is the abandoned Lake Mercer behavioral clinic.",
    "- The player is Subject 08 and later realizes Dr. Merrick built or helped build these systems.",
    "- Each chapter is framed as a broken analytics or machine-learning workflow inside the clinic.",
    "- The final system asks the player to align time-series reasoning, spatial reasoning, and identity reconstruction.",
    sep = "\n"
  )
}

build_context_message <- function(user_text, state) {
  puzzle_flags <- c(
    chapter1_blocks = state$chapter1_blocks,
    chapter2_threshold = state$chapter2_threshold,
    chapter2_matrix = state$chapter2_matrix,
    chapter3_sparse = state$chapter3_sparse,
    chapter4_pca = state$chapter4_pca,
    chapter5_rf = state$chapter5_rf,
    chapter6_routes = state$chapter6_routes
  )
  
  glue(
    "GAME STATE\n",
    "- current_chapter: {state$chapter}\n",
    "- current_chapter_title: {chapter_title(state$chapter)}\n",
    "- inventory: {paste(state$inventory, collapse = ', ')}\n",
    "- query_count: {state$query_count}\n",
    "- ending: {state$ending}\n",
    "- puzzle_flags: {paste(names(puzzle_flags), unlist(puzzle_flags), sep='=', collapse='; ')}\n\n",
    "CURRENT CHAPTER DESCRIPTION\n",
    "{chapter_blurb(state$chapter)}\n\n",
    "PLAYER MESSAGE\n",
    "{user_text}"
  )
}

scene_registry <- list(
  "observation room" = "Twelve subject avatars sit in a failed experiment board. Their ages and blood pressures vary, but the quiet culprit is prior contamination. The door will not open until the design stops burying the signal in variance.",
  "records office" = "A threshold slider glows beside ten predicted contamination probabilities, their true labels, and an empty confusion matrix keypad. A memo warns the staff that a single false negative is unacceptable.",
  "specimen cabinet" = "An NLP control panel has crashed under a gigantic sparse term matrix. The toggles mention n-grams, stop-word removal, and minimum term frequency.",
  "prediction theater" = "The projector is fed by fifty correlated biometric channels. A scree plot drops sharply after the fourth principal component, and the cumulative variance readout crosses ninety percent there.",
  "memory engine" = "A model terminal shows a single deep decision tree with perfect training accuracy and awful generalization. Nearby controls let you switch to Random Forest or SVM and tune ntree and mtry.",
  "core chamber" = "Two raw data streams pour toward three architecture blocks: Dense, CNN + Max Pooling, and LSTM. The system wants the modalities wired before the final classification layer can fire."
)

inventory_registry <- list(
  "blocking schematic" = "A schematic showing that prior contamination status should be blocked before splitting subjects into treatment and control.",
  "intake file" = "Your recovered intake file. The margin note reads: WE CANNOT AFFORD A SINGLE FALSE NEGATIVE.",
  "uv text strip" = "A strip of hidden text revealed after the sparse matrix is stabilized. It names Dr. Merrick directly.",
  "merrick profile" = "The theater finally resolves Merrick's biometric faceprint after dimensionality reduction.",
  "forest key" = "A machine key generated after the overfit tree is replaced by a random forest with stable hyperparameters.",
  "override sigil" = "The final routing sigil produced when space, time, and classification are sent through the right networks."
)

make_scene_tool <- function() {
  ellmer::tool(
    function(scene_name, `_intent`) {
      key <- tolower(trimws(scene_name))
      desc <- scene_registry[[key]]
      if (is.null(desc)) desc <- "That exact scene label is not indexed. Ask about the current room by name."
      ellmer::ContentToolResult(
        list(scene = scene_name, description = desc),
        extra = list(display = list(
          markdown = paste0("**Scene readout:** ", desc),
          title = "Scene Inspector",
          icon = bsicons::bs_icon("eye-fill"),
          open = TRUE
        ))
      )
    },
    name = "inspect_scene",
    description = "Describe the current room and its important interactive clues.",
    arguments = list(
      scene_name = ellmer::type_string("Name of the room or scene."),
      `_intent` = ellmer::type_string("Reason for inspection.")
    ),
    annotations = ellmer::tool_annotations(title = "Scene Inspector", icon = bsicons::bs_icon("eye-fill"))
  )
}

make_inventory_tool <- function(state) {
  ellmer::tool(
    function(item_name, `_intent`) {
      nm <- tolower(trimws(item_name))
      inv_names <- names(inventory_registry)
      match_idx <- which(inv_names == nm)
      if (length(match_idx) == 0) {
        desc <- "That item is not in the registered inventory lore."
      } else {
        desc <- inventory_registry[[inv_names[[match_idx[[1]]]]]]
      }
      held <- nm %in% tolower(state$inventory)
      ellmer::ContentToolResult(
        list(item = item_name, held = held, description = desc),
        extra = list(display = list(
          markdown = glue("**Held:** {held}\n\n**Description:** {desc}"),
          title = "Inventory Inspector",
          icon = bsicons::bs_icon("briefcase-fill"),
          open = TRUE
        ))
      )
    },
    name = "inspect_inventory",
    description = "Describe an inventory item and whether it is currently held.",
    arguments = list(
      item_name = ellmer::type_string("Inventory item name."),
      `_intent` = ellmer::type_string("Reason for inspection.")
    ),
    annotations = ellmer::tool_annotations(title = "Inventory Inspector", icon = bsicons::bs_icon("briefcase-fill"))
  )
}

make_matrix_tool <- function() {
  ellmer::tool(
    function(probabilities, truths, threshold = 0.5, `_intent`) {
      probs <- str_extract_all(probabilities, "-?\\d+(?:\\.\\d+)?")[[1]] |> as.numeric()
      labs <- str_extract_all(truths, "[01]")[[1]] |> as.integer()
      if (length(probs) != length(labs) || length(probs) == 0) stop("Probabilities and truth labels must be the same nonzero length.")
      preds <- ifelse(probs >= threshold, 1L, 0L)
      tp <- sum(preds == 1 & labs == 1)
      fp <- sum(preds == 1 & labs == 0)
      tn <- sum(preds == 0 & labs == 0)
      fn <- sum(preds == 0 & labs == 1)
      acc <- mean(preds == labs)
      ellmer::ContentToolResult(
        list(tp = tp, fp = fp, tn = tn, fn = fn, accuracy = round(acc, 4)),
        extra = list(display = list(
          markdown = glue(
            "**Threshold:** {threshold}\n\n",
            "- TP: {tp}\n",
            "- FP: {fp}\n",
            "- TN: {tn}\n",
            "- FN: {fn}\n\n",
            "**Accuracy:** {round(acc, 3)}"
          ),
          title = "Confusion Matrix Helper",
          icon = bsicons::bs_icon("grid-3x3-gap-fill"),
          open = TRUE
        ))
      )
    },
    name = "compute_confusion_matrix",
    description = "Compute TP, FP, TN, and FN for a chosen threshold.",
    arguments = list(
      probabilities = ellmer::type_string("Comma-separated predicted probabilities."),
      truths = ellmer::type_string("Comma-separated 0/1 truth labels."),
      threshold = ellmer::type_number("Decision threshold.", required = FALSE),
      `_intent` = ellmer::type_string("Reason for calculation.")
    ),
    annotations = ellmer::tool_annotations(title = "Confusion Matrix Helper", icon = bsicons::bs_icon("grid-3x3-gap-fill"))
  )
}

make_pca_tool <- function() {
  ellmer::tool(
    function(variance_explained, target = 0.9, `_intent`) {
      vals <- str_extract_all(variance_explained, "-?\\d+(?:\\.\\d+)?")[[1]] |> as.numeric()
      if (length(vals) == 0) stop("Provide cumulative variance values.")
      hit <- which(vals >= target)[1]
      if (is.na(hit)) stop("Target variance not reached.")
      ellmer::ContentToolResult(
        list(components_needed = hit, target = target),
        extra = list(display = list(
          markdown = glue("**Components needed:** {hit}\n\n**Target cumulative variance:** {target * 100}%"),
          title = "PCA Interpreter",
          icon = bsicons::bs_icon("bar-chart-steps"),
          open = TRUE
        ))
      )
    },
    name = "interpret_pca",
    description = "Find how many principal components are needed to reach a target cumulative variance.",
    arguments = list(
      variance_explained = ellmer::type_string("Comma-separated cumulative variance values."),
      target = ellmer::type_number("Target cumulative variance as a proportion.", required = FALSE),
      `_intent` = ellmer::type_string("Reason for interpretation.")
    ),
    annotations = ellmer::tool_annotations(title = "PCA Interpreter", icon = bsicons::bs_icon("bar-chart-steps"))
  )
}

make_rf_tool <- function() {
  ellmer::tool(
    function(num_features, `_intent`) {
      p <- as.numeric(num_features)
      if (!is.finite(p) || p <= 0) stop("num_features must be positive.")
      mtry <- floor(sqrt(p))
      ellmer::ContentToolResult(
        list(recommended_mtry = mtry, recommended_model = "Random Forest"),
        extra = list(display = list(
          markdown = glue("**Recommended model:** Random Forest\n\n**Heuristic mtry:** floor(sqrt({p})) = {mtry}"),
          title = "Forest Configurator",
          icon = bsicons::bs_icon("diagram-3-fill"),
          open = TRUE
        ))
      )
    },
    name = "configure_random_forest",
    description = "Recommend a standard random-forest mtry heuristic for classification.",
    arguments = list(
      num_features = ellmer::type_number("Number of available features."),
      `_intent` = ellmer::type_string("Reason for configuration.")
    ),
    annotations = ellmer::tool_annotations(title = "Forest Configurator", icon = bsicons::bs_icon("diagram-3-fill"))
  )
}

build_chat_client <- function(state, api_key = "") {
  key <- get_anthropic_key(api_key)
  if (!nzchar(key)) return(NULL)
  
  client <- ellmer::chat_anthropic(
    model = MODEL_NAME,
    system_prompt = build_system_prompt(),
    credentials = function() key,
    cache = "none"
  )
  
  client$register_tool(make_scene_tool())
  client$register_tool(make_inventory_tool(state))
  client$register_tool(make_matrix_tool())
  client$register_tool(make_pca_tool())
  client$register_tool(make_rf_tool())
  
  client
}

intro_messages <- list(
  list(
    role = "assistant",
    content = paste(
      "# Lake Mercer Protocol",
      "",
      "The clinic has rebuilt itself into a statistics practical with a pulse.",
      "",
      "> **MERCURY:** Subject 08, the models are failing in ways only a careful analyst would notice.",
      "",
      "Use the chapter panel to solve the main puzzle. Use the floating MERCURY button in the lower-right corner whenever you want hints, scene analysis, or a nudge on the underlying stats idea.",
      "",
      "Try things like:",
      "- *What exactly is wrong with this experiment design?*",
      "- *Give me a subtle hint for the threshold puzzle.*",
      "- *Why does PCA help in the theater?*",
      sep = "\n"
    )
  )
)

theme_app <- bs_theme(
  version = 5,
  bg = "#1a1a2e",
  fg = "#eee",
  primary = "#00ff41",
  secondary = "#0f3460",
  success = "#00ff41",
  warning = "#ffaa00",
  danger = "#ff005e",
  base_font = font_collection(
    "Monaco", "Menlo", "Consolas", "Courier New", "monospace"
  ),
  code_font = font_collection(
    "Monaco", "Menlo", "Consolas", "Courier New", "monospace"
  )
)

app_css <- "
@import url('https://fonts.googleapis.com/css2?family=Creepster&display=swap');

body {
  background-color: #1a1a2e;
  color: #eee;
}
.card, .accordion, .sidebar, .tab-pane, .nav, .bslib-grid-item {
  border-radius: 18px;
}
.panel-card {
  background: linear-gradient(145deg, #1e1e30, #16213e);
  border: 1px solid #0f3460;
  box-shadow: 0 4px 15px rgba(0, 0, 0, 0.5);
}
.scene-box {
  width: 100%;
  min-height: 220px;
  border-radius: 18px;
  border: 1px solid rgba(255,255,255,0.08);
  background: linear-gradient(135deg, rgba(132,94,247,0.12), rgba(36,196,163,0.08));
  display: flex;
  align-items: center;
  justify-content: center;
  text-align: center;
  padding: 1.2rem;
}
.inventory-chip {
  display: inline-block;
  margin: 0.2rem 0.3rem 0.2rem 0;
  padding: 0.3rem 0.55rem;
  border-radius: 999px;
  background: rgba(0, 255, 65, 0.08);
  border: 1px solid #00ff41;
  font-size: 0.88rem;
}
.muted { color: #b6c2df; }
.chapter-title,
.card-header,
.navbar-brand,
.btn-primary,
.btn-danger,
.btn-success,
.btn-outline-light,
.btn-outline-info,
h1, h2, h3, h4, h5, h6 {
  font-family: 'Creepster', cursive !important;
  letter-spacing: 1px;
}
.chapter-title {
  font-weight: 700;
  letter-spacing: 2px;
}
.navbar-brand {
  font-size: 2rem !important;
}
.safe-text {
  font-family: 'Monaco', 'Menlo', 'Consolas', 'Courier New', monospace;
}
.small-note {
  font-size: 0.9rem;
  color: #b6c2df;
}
.form-control, .form-select {
  background-color: #0f172a;
  color: #eee;
  border: 1px solid #0f3460;
}
.form-control:focus, .form-select:focus {
  border-color: #00ff41;
  box-shadow: 0 0 10px rgba(0, 255, 65, 0.5);
  background-color: #0f172a;
  color: #eee;
}
.btn-primary, .btn-danger, .btn-success {
  transition: all 0.3s ease;
  font-size: 1.05rem;
}
.btn-primary:hover, .btn-danger:hover, .btn-success:hover {
  transform: translateY(-2px);
  box-shadow: 0 5px 20px rgba(0, 255, 65, 0.3);
}
img {
  transition: opacity 0.6s ease-in-out;
}
#mercury-launcher {
  position: fixed;
  right: 22px;
  bottom: 22px;
  z-index: 3000;
}
#mercury-panel {
  position: fixed;
  right: 22px;
  bottom: 22px;
  width: min(420px, calc(100vw - 28px));
  height: min(620px, calc(100vh - 40px));
  z-index: 3001;
  display: none;
}
#mercury-panel .card {
  height: 100%;
  background: #0b1220;
  border: 1px solid rgba(255,255,255,0.12);
}
#mercury-panel .card-header,
#mercury-panel .card-body,
#mercury-panel .shinychat-container,
#mercury-panel .shinychat-messages,
#mercury-panel .shinychat-input-container {
  background: #0b1220 !important;
}
.chat-shell {
  height: calc(100% - 56px);
  overflow: hidden;
}
"

subject_df <- tibble(
  Subject = paste0("S", sprintf("%02d", 1:12)),
  Age = c(24, 27, 29, 31, 34, 36, 25, 28, 30, 33, 35, 38),
  BP = c(118, 121, 124, 126, 129, 132, 116, 120, 123, 127, 130, 134),
  `Prior Contamination` = c(rep("High", 6), rep("Low", 6))
)

subject_labels <- setNames(
  subject_df$Subject,
  paste0(subject_df$Subject, " · Age ", subject_df$Age, " · BP ", subject_df$BP, " · ", subject_df$`Prior Contamination`)
)

cum_var <- c(0.38, 0.63, 0.81, 0.92, 0.95, 0.97, 0.985, 0.992)

chapter_assets <- list(
  `1` = list(
    cover = "chapter1cover.png",
    mercury_note = "chatper1_mercurynote.png",
    success = "chapter1success.png",
    panel = "chapter1_subjectboard.png"
  ),
  `2` = list(
    cover = "chatper2cover.png",
    mercury_note = "chatper2_mercurynote.png",
    success = "chapter2success.png",
    panel = "chatper2_predictiondashboard.png"
  ),
  `3` = list(
    cover = "chapter3cover.png",
    mercury_note = "chatper3_mercurynote.png",
    success = "chapter3success.png",
    panel = "chapter3token.png"
  ),
  `4` = list(
    cover = "chapter4cover.png",
    mercury_note = "chatper4_mercurynote.png",
    success = "chapter4success.png",
    panel = "chapter4pca.png"
  ),
  `5` = list(
    cover = "chapter5cover.png",
    mercury_note = "chatper5_mercurynote.png",
    success = "chapter5success.png",
    panel = "chapter5rf.png"
  ),
  `6` = list(
    cover = "chapter6cover.png",
    mercury_note = "chatper6_mercurynote.png",
    success = "chapter6success.png",
    panel = "chapter6net.png"
  )
)

app_image <- function(src, alt = "", style = "width: 100%; border-radius: 18px; display: block;") {
  img(src = src, alt = alt, style = style)
}

chapter_asset_image <- function(chapter, type = c("cover", "mercury_note", "success", "panel"), alt_text = "") {
  type <- match.arg(type)
  src <- chapter_assets[[as.character(chapter)]][[type]]
  app_image(
    src,
    alt = alt_text,
    style = "width: 100%; max-width: 100%; border-radius: 18px; display: block; margin: 0 auto; border: 1px solid rgba(255,255,255,0.08);"
  )
}

scene_placeholder <- function(chapter) {
  div(
    class = "scene-box",
    chapter_asset_image(
      chapter,
      "cover",
      alt_text = paste(chapter_title(chapter), "cover art")
    )
  )
}

chapter_scene_ui <- function(chapter) {
  scene_placeholder(chapter)
}

ui <- page_navbar(
  title = APP_TITLE,
  theme = theme_app,
  fillable = TRUE,
  header = tags$head(
    tags$style(HTML(app_css)),
    tags$audio(id = "bg_music", src = "background.mp3", type = "audio/mp3", preload = "auto", loop = NA, style = "display:none;"),
    tags$audio(id = "unlock_sound", src = "unlock.mp3", type = "audio/mp3", preload = "auto", style = "display:none;"),
    tags$audio(id = "wrong_sound", src = "wrong.mp3", type = "audio/mp3", preload = "auto", style = "display:none;"),
    tags$script(HTML(
      "function openMercury(){document.getElementById('mercury-panel').style.display='block';document.getElementById('mercury-launcher').style.display='none';}
       function closeMercury(){document.getElementById('mercury-panel').style.display='none';document.getElementById('mercury-launcher').style.display='block';}
       function startBackgroundMusic(){
         var m=document.getElementById('bg_music');
         if(m){
           m.loop=true;
           m.volume=0.35;
           var p=m.play();
           if(p && typeof p.catch==='function'){ p.catch(function(e){ console.log('Background music blocked:', e); }); }
         }
       }
       function playUnlock(){var a=document.getElementById('unlock_sound'); if(a){a.currentTime=0; a.play().catch(function(e){console.log('Unlock sound blocked:', e);});}}
       function playWrong(){var a=document.getElementById('wrong_sound'); if(a){a.currentTime=0; a.play().catch(function(e){console.log('Wrong sound blocked:', e);});}}
       Shiny.addCustomMessageHandler('start_music', function(x){ startBackgroundMusic(); });
       Shiny.addCustomMessageHandler('play_unlock', function(x){ playUnlock(); });
       Shiny.addCustomMessageHandler('play_wrong', function(x){ playWrong(); });
       document.addEventListener('click', function(){ startBackgroundMusic(); }, {once:true});"
    ))
  ),
  nav_panel(
    "Play",
    uiOutput("play_screen_ui"),
    page_fillable(
      layout_columns(
        col_widths = c(3, 9),
        card(
          class = "panel-card",
          card_header("Status"),
          uiOutput("progress_ui"),
          hr(),
          h5("Inventory"),
          uiOutput("inventory_ui"),
          hr(),
          h5("Story log"),
          uiOutput("story_log_ui"),
          hr(),
          div(class = "d-grid gap-2",
              actionButton("reset_game", "Restart protocol", class = "btn btn-outline-light")
          )
        ),
        card(
          class = "panel-card",
          full_screen = TRUE,
          uiOutput("chapter_ui")
        )
      ),
      tags$div(
        id = "mercury-launcher",
        actionButton("open_mercury_dummy", "MERCURY", class = "btn btn-primary btn-lg", onclick = "openMercury()")
      ),
      tags$div(
        id = "mercury-panel",
        card(
          class = "panel-card",
          card_header(
            div(class = "d-flex justify-content-between align-items-center",
                span("MERCURY"),
                actionButton("close_mercury_dummy", "Minimize", class = "btn btn-outline-light btn-sm", onclick = "closeMercury()")
            )
          ),
          div(
            class = "chat-shell",
            chat_ui(
              "oracle_chat",
              messages = intro_messages,
              placeholder = "Ask for a hint, inspect a scene, or connect a clue...",
              fill = TRUE,
              height = "100%",
              icon_assistant = bsicons::bs_icon("robot")
            )
          )
        )
      )
    )
  ),
  nav_panel(
    "Design Notes",
    page_fillable(
      layout_columns(
        col_widths = c(6, 6),
        card(
          class = "panel-card",
          card_header("How this version is built"),
          HTML(paste(
            "<h4>Structure</h4>",
            "<p>This version rewires the app around the six new chapter concepts: blocking, threshold tuning, sparse NLP matrices, PCA, random forests, and deep-learning routing.</p>",
            "<h4>MERCURY</h4>",
            "<p>The AI helper is preserved and moved into a collapsible bottom-right chat panel so it behaves more like an in-world assistant instead of permanently occupying a full column.</p>",
            "<h4>Art</h4>",
            "<p>The room art is wired to chapter-specific image assets so the visuals can be refreshed without changing the puzzle logic.</p>",
            sep = ""
          ))
        ),
        card(
          class = "panel-card",
          card_header("Prompting strategy"),
          verbatimTextOutput("system_prompt")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  state <- reactiveValues(
    started = FALSE,
    api_key = "",
    api_ready = FALSE,
    chapter = 1L,
    inventory = character(),
    ending = "not reached",
    query_count = 0L,
    last_query_time = Sys.time() - 60,
    story_log = c("You wake in the observation room. MERCURY insists the experiment is fixable if you stop pretending the design is random."),
    chapter1_blocks = FALSE,
    chapter1_done = FALSE,
    chapter2_threshold = FALSE,
    chapter2_matrix = FALSE,
    chapter2_done = FALSE,
    chapter3_sparse = FALSE,
    chapter3_done = FALSE,
    chapter4_pca = FALSE,
    chapter4_done = FALSE,
    chapter5_rf = FALSE,
    chapter5_done = FALSE,
    chapter6_routes = FALSE,
    chapter6_done = FALSE
  )
  
  append_story <- function(text) {
    state$story_log <- c(state$story_log, text)
  }
  
  add_item <- function(item) {
    if (!item %in% state$inventory) state$inventory <- c(state$inventory, item)
  }
  
  go_to_chapter <- function(ch) {
    state$chapter <- ch
  }
  
  play_unlock_sound <- function() {
    session$sendCustomMessage("play_unlock", list())
  }
  
  play_wrong_sound <- function() {
    session$sendCustomMessage("play_wrong", list())
  }
  
  client <- reactiveVal(NULL)
  
  observe({
    client(build_chat_client(state, state$api_key))
  })
  
  output$progress_ui <- renderUI({
    HTML(markdown::markdownToHTML(text = progress_md(reactiveValuesToList(state)), fragment.only = TRUE))
  })
  
  output$inventory_ui <- renderUI({
    if (length(state$inventory) == 0) return(div(class = "muted", "Nothing recovered yet."))
    tagList(lapply(state$inventory, function(x) div(class = "inventory-chip", x)))
  })
  
  output$story_log_ui <- renderUI({
    HTML(markdown::markdownToHTML(text = story_log_md(state$story_log), fragment.only = TRUE))
  })
  
  output$system_prompt <- renderText(build_system_prompt())
  
  observeEvent(input$save_api_key, {
    entered_key <- if (is.null(input$user_api_key)) "" else trimws(input$user_api_key)
    
    if (!nzchar(entered_key)) {
      showNotification("Please enter your Anthropic API key.", type = "error")
      return()
    }
    
    state$api_key <- entered_key
    state$api_ready <- TRUE
    showNotification("API key accepted. You can now start the game.", type = "message")
  })
  
  observeEvent(input$reset_game, {
    session$reload()
  })
  
  observeEvent(input$check_blocks, {
    extract_subject_ids <- function(x) {
      if (is.null(x) || length(x) == 0) return(character())
      vals <- unname(unlist(x, use.names = FALSE))
      vals <- as.character(vals)
      ids <- stringr::str_extract(vals, "S\\d{2}")
      ids[!is.na(ids)]
    }
    
    treatment <- extract_subject_ids(input$treatment_group)
    control <- extract_subject_ids(input$control_group)
    pool <- extract_subject_ids(input$subject_pool)
    
    if (length(pool) > 0 || length(treatment) != 6 || length(control) != 6) {
      showNotification("Every subject must be assigned, with six in Treatment and six in Control.", type = "warning")
      play_wrong_sound()
      return()
    }
    
    assigned <- c(treatment, control)
    if (length(unique(assigned)) != 12 || !setequal(assigned, subject_df$Subject)) {
      showNotification("Each subject should appear exactly once across Treatment and Control.", type = "warning")
      play_wrong_sound()
      return()
    }
    
    contam_map <- setNames(subject_df$`Prior Contamination`, subject_df$Subject)
    treatment_status <- unname(contam_map[treatment])
    control_status <- unname(contam_map[control])
    
    if (any(is.na(treatment_status)) || any(is.na(control_status))) {
      showNotification("Something went wrong reading the dragged subjects. Please try arranging them again.", type = "error")
      play_wrong_sound()
      return()
    }
    
    t_high <- sum(treatment_status == "High", na.rm = TRUE)
    t_low <- sum(treatment_status == "Low", na.rm = TRUE)
    c_high <- sum(control_status == "High", na.rm = TRUE)
    c_low <- sum(control_status == "Low", na.rm = TRUE)
    
    if (isTRUE(t_high == 3 && t_low == 3 && c_high == 3 && c_low == 3)) {
      state$chapter1_blocks <- TRUE
      add_item("blocking schematic")
      append_story("You reblock the failed A/B test by prior contamination. The noise drops. The safety door finally sees the treatment effect.")
      showModal(modalDialog(
        title = "Design corrected",
        easyClose = TRUE,
        footer = NULL,
        chapter_asset_image(1, "success", "Chapter 1 success")
      ))
      showNotification("Design corrected: randomized block design accepted.", type = "message")
      play_unlock_sound()
    } else {
      showNotification("Wrong layout. Block High and Low contamination first, then split them evenly between Treatment and Control.", type = "error")
      play_wrong_sound()
    }
  })
  
  observeEvent(input$advance_ch1, {
    req(state$chapter1_blocks)
    state$chapter1_done <- TRUE
    append_story("The observation room opens. MERCURY whispers that bad design has killed better studies than this one.")
    go_to_chapter(2L)
  })
  
  observeEvent(input$inspect_threshold_memo, {
    showModal(modalDialog(
      title = "Recovered memo",
      easyClose = TRUE,
      footer = NULL,
      tags$pre(class = "safe-text", "We cannot afford a single False Negative; lower the threshold to 0.40.")
    ))
  })
  
  observeEvent(input$check_threshold_matrix, {
    thr_ok <- isTRUE(all.equal(input$decision_threshold, 0.40, tolerance = 1e-8))
    tp_ok <- isTRUE(input$tp_entry == 3)
    fp_ok <- isTRUE(input$fp_entry == 2)
    tn_ok <- isTRUE(input$tn_entry == 5)
    fn_ok <- isTRUE(input$fn_entry == 0)
    
    if (thr_ok) state$chapter2_threshold <- TRUE
    
    if (thr_ok && tp_ok && fp_ok && tn_ok && fn_ok) {
      state$chapter2_matrix <- TRUE
      state$chapter2_done <- TRUE
      add_item("intake file")
      append_story("You move the decision threshold to 0.40 and count the fallout yourself: 3 TP, 2 FP, 5 TN, 0 FN. The safe accepts the tradeoff and releases your intake file.")
      showModal(modalDialog(
        title = "Safe unlocked",
        easyClose = TRUE,
        footer = NULL,
        chapter_asset_image(2, "success", "Chapter 2 success")
      ))
      showNotification("Safe unlocked.", type = "message")
      play_unlock_sound()
    } else {
      showNotification("Not yet. Lower the threshold first, then recompute TP, FP, TN, and FN at that boundary.", type = "error")
      play_wrong_sound()
    }
  })
  
  observeEvent(input$advance_ch2, {
    req(state$chapter2_done)
    append_story("The records office yields your file. The next door opens onto a crashed language pipeline.")
    go_to_chapter(3L)
  })
  
  observeEvent(input$check_sparse_controls, {
    ngram_ok <- identical(input$ngram_size, "Bigram")
    stop_ok <- isTRUE(input$remove_stop)
    freq_ok <- isTRUE(input$min_term_freq > 5)
    
    if (ngram_ok && stop_ok && freq_ok) {
      state$chapter3_sparse <- TRUE
      state$chapter3_done <- TRUE
      add_item("uv text strip")
      append_story("You strip the NLP pipeline down to informative language: bigrams only, stop words removed, rare terms ignored. The sparse matrix collapses and the memory error clears.")
      showModal(modalDialog(
        title = "Memory allocation bypassed",
        easyClose = TRUE,
        footer = NULL,
        chapter_asset_image(3, "success", "Chapter 3 success")
      ))
      play_unlock_sound()
    } else {
      showNotification("The matrix is still too sparse and too noisy. Remove connective fluff and ignore terms that barely appear.", type = "error")
      play_wrong_sound()
    }
  })
  
  observeEvent(input$advance_ch3, {
    req(state$chapter3_done)
    append_story("The specimen cabinet steadies. The theater powers up, now hungry for dimensionality reduction.")
    go_to_chapter(4L)
  })
  
  observeEvent(input$check_pca, {
    if (identical(input$pca_answer, 4L)) {
      state$chapter4_pca <- TRUE
      state$chapter4_done <- TRUE
      add_item("merrick profile")
      append_story("You keep four principal components, enough to preserve ninety-two percent of the variance while killing the multicollinearity. Merrick's face resolves on the screen.")
      showModal(modalDialog(
        title = "Projector stabilized",
        easyClose = TRUE,
        footer = NULL,
        chapter_asset_image(4, "success", "Chapter 4 success")
      ))
      showNotification("Projector stabilized.", type = "message")
      play_unlock_sound()
    } else {
      showNotification("Find the elbow and the first component count that clears 90% cumulative variance.", type = "error")
      play_wrong_sound()
    }
  })
  
  observeEvent(input$advance_ch4, {
    req(state$chapter4_done)
    append_story("The projector spits out a profile trace that points straight to the memory engine.")
    go_to_chapter(5L)
  })
  
  observeEvent(input$check_forest, {
    algo_ok <- identical(input$model_choice, "Random Forest")
    ntree_ok <- isTRUE(input$ntree_value >= 500)
    mtry_ok <- identical(input$mtry_value, 4L)
    
    if (algo_ok && ntree_ok && mtry_ok) {
      state$chapter5_rf <- TRUE
      state$chapter5_done <- TRUE
      add_item("forest key")
      append_story("You abandon the overfit tree and build a forest instead: many trees, limited vision at each split, variance tamed. The machine stops memorizing one trauma and starts recognizing you.")
      showModal(modalDialog(
        title = "Memory engine recalibrated",
        easyClose = TRUE,
        footer = NULL,
        chapter_asset_image(5, "success", "Chapter 5 success")
      ))
      showNotification("Memory engine recalibrated.", type = "message")
      play_unlock_sound()
    } else {
      showNotification("A single tree is the problem. Switch to an ensemble, give it enough trees, and use the square-root heuristic for mtry.", type = "error")
      play_wrong_sound()
    }
  })
  
  observeEvent(input$advance_ch5, {
    req(state$chapter5_done)
    append_story("The forest yields a key. Beyond it, the core chamber waits with raw streams of time and space.")
    go_to_chapter(6L)
  })
  
  observeEvent(input$check_routes, {
    pixels_ok <- identical(input$route_pixels, "CNN Layers + Max Pooling")
    time_ok <- identical(input$route_timestamps, "LSTM Cells")
    fuse_ok <- identical(input$route_fusion, "Dense (Fully Connected)")
    
    if (pixels_ok && time_ok && fuse_ok) {
      state$chapter6_routes <- TRUE
      state$chapter6_done <- TRUE
      add_item("override sigil")
      append_story("You separate space from time, then fuse the learned representations into a dense classifier. The terminal finally accepts your architecture.")
      showModal(modalDialog(
        title = "Dual-stream protocol accepted",
        easyClose = TRUE,
        footer = NULL,
        chapter_asset_image(6, "success", "Chapter 6 success")
      ))
      showNotification("Dual-stream protocol accepted.", type = "message")
      play_unlock_sound()
    } else {
      showNotification("Spatial grids and temporal sequences should not travel through the same logic. Route each stream to the architecture built for it.", type = "error")
      play_wrong_sound()
    }
  })
  
  observeEvent(input$execute_escape, {
    req(state$chapter6_done)
    state$ending <- "ESCAPE — You leave with the system still alive behind you."
    append_story("You choose ESCAPE. The doors part, but Lake Mercer continues to model human failure without you.")
    play_unlock_sound()
    showModal(modalDialog(title = "Protocol resolved", easyClose = TRUE, state$ending))
  })
  
  observeEvent(input$execute_purge, {
    req(state$chapter6_done)
    state$ending <- "PURGE — You collapse the pipeline and end the loop."
    append_story("You choose PURGE. The final classifier goes dark, and for the first time the clinic has no prediction to make.")
    play_unlock_sound()
    showModal(modalDialog(title = "Protocol resolved", easyClose = TRUE, state$ending))
  })
  
  output$play_screen_ui <- renderUI({
    if (!isTRUE(state$api_ready)) {
      return(
        div(
          style = "
          min-height: 100vh;
          display: flex;
          align-items: center;
          justify-content: center;
          padding: 2rem;
        ",
          card(
            class = "panel-card",
            style = "
            max-width: 800px;
            width: 100%;
            text-align: center;
            padding: 2rem;
          ",
            h2("Enter API Key to Start"),
            p(
              class = "muted",
              "Paste your Anthropic API key below to enable MERCURY and begin the escape room."
            ),
            passwordInput("user_api_key", "Anthropic API Key", placeholder = "sk-ant-..."),
            br(),
            actionButton("save_api_key", "Continue", class = "btn btn-primary btn-lg"),
            br(), br(),
            div(class = "small-note", "Your key is used only for this session.")
          )
        )
      )
    }
    
    if (!isTRUE(state$started)) {
      return(
        div(
          style = "
          min-height: 100vh;
          display: flex;
          align-items: center;
          justify-content: center;
          padding: 2rem;
        ",
          card(
            class = "panel-card",
            style = "
            max-width: 950px;
            width: 100%;
            overflow: hidden;
            text-align: center;
            padding-bottom: 1.5rem;
          ",
            img(
              src = "cover.png",
              style = "
              width: 100%;
              max-height: 70vh;
              object-fit: cover;
              display: block;
              border-radius: 18px 18px 0 0;
            "
            ),
            div(
              style = "padding: 1.5rem 1.5rem 0.5rem 1.5rem;",
              h2("The Data Lab Escape: Lake Mercer Protocol"),
              p(
                class = "muted",
                "The clinic is waiting. The doors remember you."
              ),
              actionButton(
                "start_game",
                "Explore the Lab",
                class = "btn btn-primary btn-lg",
                onclick = "startBackgroundMusic();"
              )
            )
          )
        )
      )
    }
    
    layout_columns(
      col_widths = c(3, 5, 4),
      card(
        class = "panel-card",
        card_header("Status"),
        uiOutput("progress_ui"),
        hr(),
        h5("Inventory"),
        uiOutput("inventory_ui"),
        hr(),
        h5("Story log"),
        uiOutput("story_log_ui"),
        hr(),
        div(
          class = "d-grid gap-2",
          actionButton("reset_game", "Restart protocol", class = "btn btn-outline-light")
        )
      ),
      card(
        class = "panel-card",
        full_screen = TRUE,
        uiOutput("chapter_ui")
      ),
      div()
    )
  })
  observeEvent(input$start_game, {
    state$started <- TRUE
    session$sendCustomMessage("start_music", list())
  })
  
  output$chapter_ui <- renderUI({
    chapter <- state$chapter
    
    if (chapter == 1L) {
      tagList(
        card_header(
          tagList(
            h3(class = "chapter-title", "Chapter 1 — The Observation Room"),
            div(class = "small-note", "The Confounded Trial — randomized block design")
          )
        ),
        chapter_scene_ui(1),
        br(),
        p(class = "muted", chapter_blurb(1)),
        accordion(
          accordion_panel(
            "Subject board",
            p("The current A/B test is washed out by within-group variance. Prior contamination is the confounder hiding in plain sight."),
            chapter_asset_image(1, "panel", "Chapter 1 subject board"),
            br(),
            p(class = "small-note", "Goal: block by Prior Contamination first, then split evenly into Treatment and Control."),
            bucket_list(
              header = NULL,
              group_name = "chapter1_blocks",
              orientation = "horizontal",
              add_rank_list(text = "Available subjects", labels = subject_labels, input_id = "subject_pool"),
              add_rank_list(text = "Treatment", labels = NULL, input_id = "treatment_group"),
              add_rank_list(text = "Control", labels = NULL, input_id = "control_group")
            ),
            br(),
            actionButton("check_blocks", "Check design", class = "btn btn-primary")
          ),
          accordion_panel(
            "MERCURY note",
            chapter_asset_image(1, "mercury_note", "Chapter 1 MERCURY note")
          ),
          accordion_panel(
            "Exit condition",
            p("Rebuild the assignment as a randomized block design so the door recognizes the treatment effect."),
            if (isTRUE(state$chapter1_blocks)) {
              chapter_asset_image(1, "success", "Chapter 1 success")
            },
            actionButton("advance_ch1", "Open the observation door", class = "btn btn-success")
          )
        )
      )
    } else if (chapter == 2L) {
      tagList(
        card_header(
          tagList(
            h3(class = "chapter-title", "Chapter 2 — The Records Office"),
            div(class = "small-note", "The Matrix of Lies — threshold tuning and confusion matrix")
          )
        ),
        chapter_scene_ui(2),
        br(),
        p(class = "muted", chapter_blurb(2)),
        accordion(
          accordion_panel(
            "Prediction dashboard",
            p("The safe is guarded by a baseline logistic-regression model. Accuracy looks fine until you ask who it failed to catch."),
            chapter_asset_image(2, "panel", "Chapter 2 prediction dashboard"),
            br(),
            sliderInput("decision_threshold", "Decision threshold", min = 0.10, max = 0.90, value = 0.50, step = 0.05),
            fluidRow(
              column(3, numericInput("tp_entry", "TP", value = 0, min = 0, max = 10)),
              column(3, numericInput("fp_entry", "FP", value = 0, min = 0, max = 10)),
              column(3, numericInput("tn_entry", "TN", value = 0, min = 0, max = 10)),
              column(3, numericInput("fn_entry", "FN", value = 0, min = 0, max = 10))
            ),
            div(class = "d-flex gap-2 flex-wrap",
                actionButton("inspect_threshold_memo", "Read recovered memo", class = "btn btn-outline-info"),
                actionButton("check_threshold_matrix", "Submit threshold + matrix", class = "btn btn-primary")
            )
          ),
          accordion_panel(
            "MERCURY note",
            chapter_asset_image(2, "mercury_note", "Chapter 2 MERCURY note")
          ),
          accordion_panel(
            "Exit condition",
            p("Lower the threshold to 0.40 and enter the resulting confusion matrix counts correctly."),
            if (isTRUE(state$chapter2_done)) {
              chapter_asset_image(2, "success", "Chapter 2 success")
            },
            actionButton("advance_ch2", "Unlock the intake safe", class = "btn btn-success")
          )
        )
      )
    } else if (chapter == 3L) {
      tagList(
        card_header(
          tagList(
            h3(class = "chapter-title", "Chapter 3 — The Specimen Cabinet"),
            div(class = "small-note", "The Sparse Memory Matrix — NLP preprocessing")
          )
        ),
        chapter_scene_ui(3),
        br(),
        p(class = "muted", chapter_blurb(3)),
        accordion(
          accordion_panel(
            "NLP control panel",
            p("The classifier is trying to process far too many phrases, including useless connective words. The matrix is exploding into sparsity."),
            chapter_asset_image(3, "panel", "Chapter 3 token panel"),
            br(),
            selectInput("ngram_size", "N-gram size", choices = c("Unigram", "Bigram", "Trigram"), selected = "Trigram"),
            switchInput("remove_stop", "Remove stop words", value = FALSE),
            numericInput("min_term_freq", "Minimum term frequency", value = 1, min = 1, max = 20),
            actionButton("check_sparse_controls", "Apply preprocessing", class = "btn btn-primary")
          ),
          accordion_panel(
            "MERCURY note",
            chapter_asset_image(3, "mercury_note", "Chapter 3 MERCURY note")
          ),
          accordion_panel(
            "Exit condition",
            p("Set the pipeline to a lighter, less sparse representation so the memory allocation error clears."),
            if (isTRUE(state$chapter3_done)) {
              chapter_asset_image(3, "success", "Chapter 3 success")
            },
            actionButton("advance_ch3", "Reveal the hidden text", class = "btn btn-success")
          )
        )
      )
    } else if (chapter == 4L) {
      tagList(
        card_header(
          tagList(
            h3(class = "chapter-title", "Chapter 4 — The Prediction Theater"),
            div(class = "small-note", "The Curse of Dimensionality — PCA and multicollinearity")
          )
        ),
        chapter_scene_ui(4),
        br(),
        p(class = "muted", chapter_blurb(4)),
        accordion(
          accordion_panel(
            "Scree plot terminal",
            p("Fifty biometric sensors are feeding the same panic into the projector. Interpret the scree plot and keep enough components to retain at least 90% variance."),
            chapter_asset_image(4, "panel", "Chapter 4 PCA panel"),
            br(),
            tags$pre(class = "safe-text", paste0("Cumulative variance: ", paste(cum_var, collapse = ", "))),
            numericInput("pca_answer", "Enter number of principal components", value = 1, min = 1, max = 8),
            actionButton("check_pca", "Submit PCA choice", class = "btn btn-primary")
          ),
          accordion_panel(
            "MERCURY note",
            chapter_asset_image(4, "mercury_note", "Chapter 4 MERCURY note")
          ),
          accordion_panel(
            "Exit condition",
            p("Enter the smallest number of components that retains at least 90% of the variance."),
            if (isTRUE(state$chapter4_done)) {
              chapter_asset_image(4, "success", "Chapter 4 success")
            },
            actionButton("advance_ch4", "Fire up the projector", class = "btn btn-success")
          )
        )
      )
    } else if (chapter == 5L) {
      tagList(
        card_header(
          tagList(
            h3(class = "chapter-title", "Chapter 5 — The Memory Engine"),
            div(class = "small-note", "The Overfitted Reality — replace a single tree with an ensemble")
          )
        ),
        chapter_scene_ui(5),
        br(),
        p(class = "muted", chapter_blurb(5)),
        accordion(
          accordion_panel(
            "Model architecture terminal",
            p("The current model is a single deep decision tree with perfect training accuracy and terrible generalization. There are 16 total memory features available."),
            chapter_asset_image(5, "panel", "Chapter 5 random forest panel"),
            br(),
            selectInput("model_choice", "Algorithm", choices = c("Decision Tree", "Random Forest", "SVM"), selected = "Decision Tree"),
            sliderInput("ntree_value", "Number of trees (ntree)", min = 50, max = 1000, value = 100, step = 50),
            sliderInput("mtry_value", "Variables per split (mtry)", min = 1, max = 16, value = 8, step = 1),
            actionButton("check_forest", "Rebuild memory model", class = "btn btn-primary")
          ),
          accordion_panel(
            "MERCURY note",
            chapter_asset_image(5, "mercury_note", "Chapter 5 MERCURY note")
          ),
          accordion_panel(
            "Exit condition",
            p("Switch to Random Forest, use a high tree count for stability, and apply the square-root heuristic to mtry."),
            if (isTRUE(state$chapter5_done)) {
              chapter_asset_image(5, "success", "Chapter 5 success")
            },
            actionButton("advance_ch5", "Unlock the core chamber", class = "btn btn-success")
          )
        )
      )
    } else {
      tagList(
        card_header(
          tagList(
            h3(class = "chapter-title", "Chapter 6 — The Core Chamber"),
            div(class = "small-note", "The Dual-Stream Protocol — route space and time correctly")
          )
        ),
        chapter_scene_ui(6),
        br(),
        p(class = "muted", chapter_blurb(6)),
        accordion(
          accordion_panel(
            "Routing interface",
            p("Two data streams are arriving raw. Send each to the architecture built for its structure, then route both learned outputs into the final classifier."),
            chapter_asset_image(6, "panel", "Chapter 6 network panel"),
            br(),
            selectInput("route_pixels", "Security Camera Pixel Matrices →", choices = c("Dense (Fully Connected)", "CNN Layers + Max Pooling", "LSTM Cells")),
            selectInput("route_timestamps", "Sequential Keystroke Timestamps →", choices = c("Dense (Fully Connected)", "CNN Layers + Max Pooling", "LSTM Cells")),
            selectInput("route_fusion", "Combined representation →", choices = c("Dense (Fully Connected)", "CNN Layers + Max Pooling", "LSTM Cells")),
            actionButton("check_routes", "Validate routing", class = "btn btn-primary")
          ),
          accordion_panel(
            "MERCURY note",
            chapter_asset_image(6, "mercury_note", "Chapter 6 MERCURY note")
          ),
          accordion_panel(
            "Final actions",
            p("Once the dual-stream protocol is accepted, choose how the clinic ends."),
            if (isTRUE(state$chapter6_done)) {
              chapter_asset_image(6, "success", "Chapter 6 success")
            },
            div(class = "d-flex gap-2 flex-wrap",
                actionButton("execute_escape", "ESCAPE", class = "btn btn-outline-light"),
                actionButton("execute_purge", "PURGE", class = "btn btn-danger")
            ),
            br(),
            strong(state$ending)
          )
        )
      )
    }
  })
  
  observeEvent(input$oracle_chat_user_input, {
    req(client())
    user_text <- trimws(input$oracle_chat_user_input)
    if (!nzchar(user_text)) return()
    
    elapsed <- as.numeric(difftime(Sys.time(), state$last_query_time, units = "secs"))
    if (elapsed < MIN_SECONDS_BETWEEN_QUERIES) {
      chat_append("oracle_chat", glue("⚠️ Wait {ceiling(MIN_SECONDS_BETWEEN_QUERIES - elapsed)} more second(s) before sending another message."))
      return()
    }
    
    if (nchar(user_text) > MAX_CHARS_PER_MESSAGE) {
      chat_append("oracle_chat", glue("⚠️ Keep your message under {MAX_CHARS_PER_MESSAGE} characters."))
      return()
    }
    
    if (state$query_count >= MAX_TOTAL_QUERIES) {
      chat_append("oracle_chat", "⚠️ This session has reached its query budget. Restart the protocol for a fresh run.")
      return()
    }
    
    state$query_count <- state$query_count + 1L
    state$last_query_time <- Sys.time()
    
    payload <- build_context_message(user_text, reactiveValuesToList(state))
    
    stream <- tryCatch(
      client()$stream_async(payload, stream = "content", tool_mode = "sequential"),
      error = function(e) {
        chat_append("oracle_chat", glue("The MERCURY link stutters: {conditionMessage(e)}"))
        NULL
      }
    )
    
    if (!is.null(stream)) {
      tryCatch(
        chat_append("oracle_chat", stream),
        error = function(e) {
          chat_append("oracle_chat", glue("The chat stream could not be appended: {conditionMessage(e)}"))
        }
      )
    }
  })
}

shinyApp(ui, server)
