library(shiny)
library(shinyFiles)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)

# UI #######################################################################

ui <- fluidPage(
  # theme = shinytheme("simplex"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
      .footer {
        align-items: center;
        position: fixed;
        bottom: 0;
        margin-top: 20px;
        text-align: center;
        font-size: 0.9em;
        color: grey;
      }      
    "))    
  ),
  titlePanel(
    # Logo im Titel hinzufügen
    tags$div(
      tags$img(src = "logo.PNG", alt = "Logo", style = "height:30px; margin-right:10px; margin-top:-12px;"),
      "COA Vorbereitung - (Synthes Artikel)",
      # Hinzufügen des Info-Buttons
      actionButton("infoButton", label = NULL, icon = icon("info-circle"),
                   style = "color: #17a2b8; background-color: transparent; border: none; margin-left: 10px; font-size: 35px; margin-top:-8px;")
    )
  ),
  fluidRow(
    # Oben links: Einstellungen und Bedienelemente
    column(
      width = 3,
      wellPanel(
        fileInput('file', 'Lade PDF-Dateien von Werth:', multiple = TRUE, accept = c('.pdf')),

        # Dynamisch sichtbare Buttons:
        conditionalPanel(
          condition = "output.dataUploaded",
          actionButton("removeRows", "Markierte Messung entfernen",
                       class = "btn-danger", style = "width:100%; margin-top:5px;"),
          actionButton("downloadExcel", "Excel Datei herunterladen", style = "width:100%; margin-top:5px;", class = "btn-success"),
          actionButton("resetApp", "Alles zurücksetzen", class = "btn-primary", style = "width:100%; margin-top:5px;")
        )
      )
    ),

    # Dynamisch sichtbare Elemente:
    conditionalPanel(
      condition = "output.dataUploaded",

      # Oben Mitte: Metadaten-Tabelle
      column(
        width = 3,
        box(
          title = "Informationen", status = "info", solidHeader = TRUE, collapsible = FALSE,
          DTOutput("metadataTable"), width = NULL, height = "300px",
          tags$img(src = "Legende.PNG", alt = "Legende", style = "height:30px; margin-top:15px;")
        )
      ),

      # Oben Rechts: Statistik-Tabelle (min, max, Mittelwert)
      column(
        width = 6,
        box(
          title = "Zusammenfassung", status = "info", solidHeader = TRUE, collapsible = FALSE,
          DTOutput("statsTable"), width = NULL, height = "auto"
        )
      )
    )
  ),

  # Darunter: Die Tabelle mit allen Messungen / breite Tabelle
  conditionalPanel(
    condition = "output.dataUploaded",
    fluidRow(
      column(
        12,
        box(
          title = "Alle Messungen aus den PDF-Messprotokollen:", status = "info", solidHeader = TRUE, collapsible = FALSE, width = NULL,
          DTOutput("dataTable")
        )
      )
    )
  ),

  # Darunter: Die Zeitachsen-Visualisierung
  conditionalPanel(
    condition = "output.dataUploaded",
    fluidRow(
      column(
        width = 12,
        box(
          title = "Zeitpunkt der Messungen (Etikettenzeit)", status = "info", solidHeader = TRUE, collapsible = FALSE, width = NULL,
          timevisOutput("timeline")
        )
      )
    )
  ),

  # Abstand von 30px
  conditionalPanel(
    condition = "output.dataUploaded",
    tags$hr(style = "margin-top: 20px;"),

  fluidRow(
    column(
      width = 6,
      div(
        style = "text-align: center; margin-left: 300px;",
        pickerInput(
          inputId = 'plot_type',
          label = 'Wähle den Plot-Typ aus:',
          choices = c("Punktdiagramm", "Boxplot"),
          options = list(`style` = "btn-info"),
          selected = "Punktdiagramm"
        )
      )
    ),

    column(
      width = 6,
      div(
        style = "text-align: center;",
        sliderTextInput(
          inputId = "aspect_ratio",
          label = "Breite der Grafiken:",
          grid = TRUE,
          force_edges = TRUE,
          choices = seq(0, 1, by = 0.1),
          selected = "0.5"
        )
      )
    )
  ),
    fluidRow(
      column(
        width = 12,
        div(
          style = "background-color: #f4f4f9; padding: 15px; width: 100%; height: auto;",
          uiOutput("plots", style = "background-color: #f4f4f9;")  # Setzt auch den Hintergrund des uiOutput auf die gleiche Farbe
        )
      )
    )
  ),
  # Footer
  div(class = "footer",
      includeHTML("footer.html")
  )
)
