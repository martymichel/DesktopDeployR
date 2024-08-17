library(shiny)

source("functions.R")

# Server-Logik #################################################################
server <- function(input, output, session) {
  data_rv <- reactiveValues(combined_df = NULL, wide_df = NULL, Names_unique = NULL)

  # Reset-Funktion: Setzt alle gespeicherten Daten zurück
  reset_app <- function() {
    # Beende die Anzeige der Plots
    output$plots <- renderUI(NULL)
    data_rv$combined_df <- NULL
    data_rv$wide_df <- NULL
    data_rv$Names_unique <- NULL
    data_rv$dataUploaded <- FALSE # Daten wurden zurückgesetzt
    rm(list = ls(envir = .GlobalEnv))  # Lösche alle globalen Variablen
    gc()  # Führe Garbage Collection durch, um Speicher freizugeben
  }

#### Modal-Dialoge #############################################################

  # Zeige Modal zur Bestätigung des Zurücksetzens
  observeEvent(input$resetApp, {
    showModal(modalDialog(
      title = "Sind Sie sicher?",
      "Möchten Sie wirklich alle Daten zurücksetzen? Diese Aktion kann nicht rückgängig gemacht werden.",
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("confirmReset", "Ja, zurücksetzen")
      )
    ))
  })

  # Handling der Bestätigung zum Zurücksetzen
  observeEvent(input$confirmReset, {
      removeModal()  # Schliesst das Modal
      reset_app()    # Setzt die App zurück
      Sys.sleep(6) # Wartezeit, um die Modal-Dialoge zu schliessen
      showModal(modalDialog(
        title = "Zurückgesetzt",
        # HTML-Text in blau und fett: "Die App wurde erfolgreich zurückgesetzt. Sie können jetzt neue PDF-Messprotokolle hochladen."
        HTML("<p style='color:blue; font-weight:bold;'>Die App wurde erfolgreich zurückgesetzt.<br>
             Sie können jetzt neue PDF-Messprotokolle hochladen.</p>"),
      ))
    })

#### Hinweis zur Nutzung ########################################################

  # Popup-Meldung beim Start der App
  shinyalert(
    title = "Hinweis zur Nutzung:",
    text = paste(
      "1. Mit 'Browse' Messprotokoll-PDFs hochladen.",
      "---",
      "2. Informationen prüfen.",
      "---",
      "3. Fehlmessungen (Zeilen) markieren (werden blau)",
      "---",
      "4. 'Markierte Messungen entfernen' klicken",
      "---",
      "5. Min, Max, Mittelwert oben rechts prüfen",
      "---",
      "6. Excel-Messprotokol ist downloadbar.",
      "---",
      "7. Programmfenster am Ende immer schliessen.",
      "---",
      "Danke! Gruss, Michel",
      sep = "\n"
    ),

    type = "info",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE
  )

  # Beobachte den Klick auf den Info-Button und zeige den Info-Modal erneut
  observeEvent(input$infoButton, {
    shinyalert(
      title = "Anweisung:",
      text = paste(
        "1. Mit 'Browse' Messprotokoll-PDFs hochladen.",
        "---",
        "2. Informationen prüfen.",
        "---",
        "3. Fehlmessungen (Zeilen) markieren (werden blau)",
        "---",
        "4. 'Markierte Messungen entfernen' klicken",
        "---",
        "5. Min, Max, Mittelwert oben rechts prüfen",
        "---",
        "6. Excel-Messprotokol ist downloadbar.",
        "---",
        "7. Programmfenster am Ende immer schliessen.",
        "---",
        "Danke! Gruss, Michel",
        sep = "\n"
      ),
      type = "info",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE
    )
  })

#### Erstellung der Timeline-Daten für timevis ################################

# Erstellung der reaktiven Timeline-Daten
timeline_data <- reactive({
  req(data_rv$combined_df) # Sicherstellen, dass combined_df vorhanden ist

  # Erstellen eines neuen DataFrames für timevis
  timeline_data <- data_rv$combined_df %>%
    mutate(
      # Prüfen, ob der Datumszeitpunkt einen Zeitstempel enthält
      Prod_Dat_Zeit = if_else(
        str_detect(Prod_Dat_Zeit, "/"),  # Wenn das Datum einen Schrägstrich enthält
        Prod_Dat_Zeit,  # Lassen Sie das Datum unverändert
        paste0(Prod_Dat_Zeit, "/12:00")  # Fügen Sie Standardzeit 12:00 hinzu
      ),
      Prod_Dat_Zeit = str_replace(Prod_Dat_Zeit, "/", " "),  # Ersetze '/' durch ein Leerzeichen
      Prod_Dat_Zeit = dmy_hm(Prod_Dat_Zeit, tz = "Europe/Zurich")  # Parse Datum und Zeit
    ) %>%
    filter(!is.na(Prod_Dat_Zeit)) %>%  # Nur gültige Daten auswählen
    group_by(Bemerkungen) %>%  # Gruppierung nach Bemerkungen
    mutate(
      id = cur_group_id(),  # ID basierend auf Bemerkungen
      content = Bemerkungen,  # Inhalt der Timeline: Nur Bemerkungen
      start = Prod_Dat_Zeit,
      end = NA  # Kein Enddatum, da es sich um Punkte handelt
    ) %>%
    ungroup() %>%
    select(id, content, start, end) %>%
    distinct()

  return(timeline_data)
})

# Rendering der Timeline mit timevis
output$timeline <- renderTimevis({
  req(timeline_data())

  # Erstellung der Zeitachse mit Optionen
  timevis(
    data = timeline_data(),
    options = list(
      showCurrentTime = FALSE,  # Deaktivieren der Anzeige der aktuellen Zeit
      height = "200px",  # Höhe der Timeline
      width = "100%",  # Breite der Timeline
      showMajorLabels = TRUE,  # Zeige Hauptbeschriftungen
      showMinorLabels = TRUE,  # Zeige Nebenbeschriftungen
      clickToUse = TRUE  # Aktivieren Sie die interaktive Nutzung der Timeline
    )
  )
})

#### Datenimport und -verarbeitung ############################################
  observe({
    req(input$file)
    data_rv$dataUploaded <- TRUE # Daten wurden hochgeladen

    inFile <- input$file$datapath

    # Import und Verarbeitung der PDF-Daten
    combined_df <- do.call(rbind, lapply(inFile, parse_pdf)) # Alle PDFs parsen und zusammenführen

    # Umwandlung in das Breitformat
    wide_df <- pivot_wider(combined_df, names_from = Merkmal, values_from = c(Ist, Soll, Toll_pos, Toll_neg, Abw))

    # Einzigartige Merkmalsnamen extrahieren
    Names_unique <- gsub("^Ist_", "", grep("^Ist_", names(wide_df), value = TRUE))

    # Speichern der umgewandelten Daten und Merkmale in reactiveValues
    data_rv$combined_df <- combined_df
    data_rv$wide_df <- wide_df
    data_rv$Names_unique <- Names_unique
  })

#### Modale Dialogfunktion für die Pfadauswahl ################################
  show_save_modal <- function(default_filename) {
    showModal(modalDialog(
      title = "Speicherort auswählen",
      textInput("save_path", "Wähle den Speicherort und Dateinamen:", value = default_filename, width = '100%'),
      # HTML-Text einfügen in fett schwarz: "Beispiel: 'C:/Users/Username/Downloads/Dateiname.xlsx'",
      HTML("<p style='font-weight:bold; color:black;'>Beispiel: C:/Users/Username/Downloads/Dateiname.xlsx</p>"),
      br(),
      # HTML-Text einfügen in rot: "Hinweis: Der Download kann bis zu 30 Sekunden dauern, bis er abgeschlossen ist.",
      HTML("<p style='color:red;'>Hinweis: Der Download kann bis zu 30 Sekunden dauern, bis er abgeschlossen ist.<br>
           Die Erstellung des Excel-Messprotokolls läuft im Hintergrund.</p>"),
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("confirm_save", "Speichern")
      ),
      size = "l"  # Breiteres Modal für lange Pfade
    ))
  }

#### Trigger, um den modalen Dialog anzuzeigen #################################
  observeEvent(input$downloadExcel, {
    # Generiere den Standard-Dateinamen
    datecode <- format(Sys.Date(), "%Y%m%d")
    namelot <- unique(data_rv$wide_df$Lot_Nr)[1]
    artNr <- unique(data_rv$wide_df$Artikel)[1]

    # Standardspeicherpfad des Browsers ermitteln (z.B., typischerweise im Downloads-Ordner)
    default_directory <- file.path(Sys.getenv("USERPROFILE"), "Downloads") # Unter Windows, passe für andere OS an
    if (!dir.exists(default_directory)) {
      default_directory <- "~" # Fallback auf Home-Verzeichnis
    }

    # Voller Pfad für den Standard-Dateinamen
    default_filename <- file.path(default_directory, paste(datecode, namelot, artNr, "Messprotokoll.xlsx", sep = "_"))

    # Zeige den modalen Dialog zur Auswahl des Speicherorts und Dateinamens
    show_save_modal(default_filename)
  })

#### Plotting ################################################################
  create_plot <- function(merkmal_name, plot_type, aspect_ratio, wide_df) {

    # Merkmalsnamen vorbereiten
    ist_column_name <- paste0("Ist_", merkmal_name)
    soll_column_name <- paste0("Soll_", merkmal_name)
    toll_pos_column_name <- paste0("Toll_pos_", merkmal_name)
    toll_neg_column_name <- paste0("Toll_neg_", merkmal_name)

    # Extrahieren der einheitlichen Soll- und Toleranzwerte
    soll_value <- unique(wide_df[[soll_column_name]])[1]
    toll_pos_value <- unique(wide_df[[toll_pos_column_name]])[1]
    toll_neg_value <- unique(wide_df[[toll_neg_column_name]])[1]

    # Plot-Erstellung
    if (plot_type == "Punktdiagramm") {
      plot <- ggplot(data = wide_df, aes_string(x = "Bemerkungen", y = ist_column_name, color = "Kav")) +
        geom_point(size = 2.5, show.legend = TRUE) +
        geom_hline(yintercept = soll_value, linetype = "dashed", color = "green") +
        geom_hline(yintercept = soll_value + toll_pos_value, linetype = "dashed", color = "red") +
        geom_hline(yintercept = soll_value + toll_neg_value, linetype = "dashed", color = "red") +
        theme_bw() +
        coord_flip() +
        labs(title = paste0("Punktdiagramm für ", merkmal_name), y = ist_column_name) +
        theme(legend.position = "bottom",
              legend.key.size = unit(0.5, "cm"),
              panel.background = element_rect(fill = "white", color = NA),
              plot.background = element_rect(fill = "#f4f4f9", color = NA),
              plot.margin = margin(10, 10, 10, 10, "pt"),
              axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
              axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 12)
              )

      if (!is.null(aspect_ratio)) {
        plot <- plot + theme(aspect.ratio = as.numeric(aspect_ratio))
      }

    } else if (plot_type == "Boxplot") {
      plot <- ggplot(data = wide_df, aes_string(x = "Kav", y = ist_column_name, fill = "Kav")) +
        geom_boxplot() +
        geom_hline(yintercept = soll_value, linetype = "dashed", color = "green") +
        geom_hline(yintercept = soll_value + toll_pos_value, linetype = "dashed", color = "red") +
        geom_hline(yintercept = soll_value + toll_neg_value, linetype = "dashed", color = "red") +
        theme_bw() +
        coord_flip() +
        theme(legend.position = "none",
              panel.background = element_rect(fill = "white", color = NA),
              plot.background = element_rect(fill = "#f4f4f9", color = NA))

      if (!is.null(aspect_ratio)) {
        plot <- plot + theme(aspect.ratio = as.numeric(aspect_ratio))
      }
    }

    return(plot)
  }

  output$plots <- renderUI({
    tagList(lapply(data_rv$Names_unique, function(name) {
      plotOutput(paste0("plot_", name))
    }))
  })

  plot_outputs <- reactive({
    lapply(data_rv$Names_unique, function(name) {
      output[[paste0("plot_", name)]] <- renderPlot({
        req(input$plot_type, input$aspect_ratio)
          if (is.null(data_rv$wide_df)) {
            return(NULL)  # Vermeide das Rendern eines Plots, wenn keine Daten vorhanden sind
          }
        create_plot(name, input$plot_type, input$aspect_ratio, data_rv$wide_df)
      })
    })
  })

  # Beobachten, wann die Plots aktualisiert werden müssen
  observe({
    plot_outputs()
  })

# Informatione Table #################################################################

  output$metadataTable <- renderDT({
    req(data_rv$wide_df)
    full_df <- data_rv$wide_df

    # Alle Spalten in full_df als charakter Formatieren
    full_df[] <- lapply(full_df, as.character)

    Metadatatable <- t(unique(full_df[, c(3:6)])[1, ])

    datatable(Metadatatable, colnames = "", escape = FALSE,
              options = list(dom = 't', ordering = FALSE))
  })

# Grosse Tabelle mit allen Daten ###############################################

  output$dataTable <- renderDT({
    req(data_rv$wide_df)
    wide_df <- data_rv$wide_df
    wide_df <- wide_df[, c(1, 9, 11, 13:ncol(wide_df))]

    # Farbbedingungen direkt in den Zellinhalten für Ist-Werte anwenden
    merkmale <- gsub("Ist_", "", grep("Ist_", names(wide_df), value = TRUE))

    for (merkmal in merkmale) {
      ist_col <- paste0("Ist_", merkmal)
      soll_col <- paste0("Soll_", merkmal)
      toll_pos_col <- paste0("Toll_pos_", merkmal)
      toll_neg_col <- paste0("Toll_neg_", merkmal)

      wide_df[[ist_col]] <- mapply(function(ist, soll, toll_pos, toll_neg) {
        color <- ifelse(
          ist > soll + toll_pos | ist < soll + toll_neg, "red",  # Rote Farbe für ausserhalb der Toleranz
          ifelse(
            ist > soll + toll_pos * 0.66 & ist < soll + toll_pos |
              ist < soll + toll_neg * 0.66 & ist > soll + toll_neg, "orange",  # Orange Farbe für innerhalb der Toleranz
            "lightgreen"
          )
        )
        # Fette Schrift für Ist-Werte ausserhalb der Toleranz
        if (ist > soll + toll_pos | ist < soll + toll_neg) {
          ist <- paste0("<b>", ist, "</b>")
        }

        HTML(sprintf('<span style="background-color: %s;">%s</span>', color, ist))
      }, wide_df[[ist_col]], wide_df[[soll_col]], wide_df[[toll_pos_col]], wide_df[[toll_neg_col]], SIMPLIFY = FALSE)
    }

    # Anpassung der Nachmessung-Spalte: TRUE -> "JA", FALSE -> ""
    wide_df$Nachmessung <- ifelse(wide_df$Nachmessung, "JA", "")

    # Spaltentitel "Prod_Dat_Zeit" zu "Musterentnahme" ändern
    # colnames(wide_df)[2] <- "Musterentnahme"

    # Alle Spaltentitel die "Toll_pos" oder "Toll_neg" enthalten, zu "Tol.pos." bzw. "Tol.neg." ändern
    colnames(wide_df) <- gsub("Toll_pos", "Tol.pos.", colnames(wide_df))
    colnames(wide_df) <- gsub("Toll_neg", "Tol.neg.", colnames(wide_df))

    # Alle Spaltentitel die "Soll_" enthalten, zu "Nominal_" ändern
    colnames(wide_df) <- gsub("Soll_", "Nominal_", colnames(wide_df))

    # DataTable erstellen mit stats_df.
    datatable(
      wide_df,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        autoWidth = FALSE,
        ordering = FALSE,
        rowCallback = JS(
          "function(row, data, index) {",
          "  if (data[0] == 'JA') {",
          "    $(row).css('background-color', '#fffec9');",
          "  }",
          "}"
        )
      ),
      class = "display cell-border stripe"
    )
  })

#### Zweite Tabelle für statistische Zusammenfassungen #####################
  output$statsTable <- renderDT({
    req(data_rv$wide_df)
    wide_df <- data_rv$wide_df

    # Identifizieren aller Ist-Spalten und der entsprechenden Soll- und Toleranzwerte
    ist_columns <- grep("^Ist_", names(wide_df), value = TRUE)
    soll_columns <- gsub("Ist_", "Soll_", ist_columns)
    toll_pos_columns <- gsub("Ist_", "Toll_pos_", ist_columns)
    toll_neg_columns <- gsub("Ist_", "Toll_neg_", ist_columns)

    # Berechnen von Min, Max und Mean für jede Ist-Spalte
    stats_df <<- data.frame(Merkmal = c("Min", "Mittelwert", "Max"))
    for (col in ist_columns) {
      min_val <- round(min(wide_df[[col]], na.rm = TRUE), 3)
      max_val <- round(max(wide_df[[col]], na.rm = TRUE), 3)
      mean_val <- round(mean(wide_df[[col]], na.rm = TRUE), 3)

      # Identifizieren der Soll- und Toleranzwerte für das aktuelle Merkmal
      soll_val <- unique(wide_df[[gsub("Ist_", "Soll_", col)]])
      toll_pos_val <- unique(wide_df[[gsub("Ist_", "Toll_pos_", col)]])
      toll_neg_val <- unique(wide_df[[gsub("Ist_", "Toll_neg_", col)]])

      # Farbanpassung für die aggregierten Werte
      stats_df[[col]] <- mapply(function(val) {
        color <- ifelse(
          val > soll_val + toll_pos_val | val < soll_val + toll_neg_val, "red",
          ifelse(
            val > soll_val + toll_pos_val * 0.66 & val < soll_val + toll_pos_val |
              val < soll_val + toll_neg_val * 0.66 & val > soll_val + toll_neg_val, "orange",
            "lightgreen"
          )
        )
        # Fette Schrift für Ist-Werte ausserhalb der Toleranz
        if (val > soll_val + toll_pos_val | val < soll_val + toll_neg_val) {
          val <- paste0("<b>", val, "</b>")
        }
        HTML(sprintf('<span style="background-color: %s;">%s</span>', color, val))
      }, c(min_val, mean_val, max_val))
    }

    # stats_df als globale Variable speichern
    assign("stats_df", stats_df, envir = .GlobalEnv)

    # DataTable erstellen mit stats_df.
    # Die Tabelle sollt nur 500px breit sein und linksbündig.
    # Die Tabelle solle sich visuell von der Haupttabelle unterscheiden.
    datatable(stats_df, escape = FALSE,
              options = list(dom = 't',
                             ordering = FALSE,
                             scrollX = TRUE
                             ),
              rownames = FALSE,
              class = "display cell-border stripe",
              width = "500px")
  })

##### Aktion zum Entfernen von Zeilen #####################################

  # Aktion zum Entfernen von ausgewählten Zeilen
  observeEvent(input$removeRows, {
    req(data_rv$wide_df, input$dataTable_rows_selected)

    if(!is.null(input$dataTable_rows_selected)){
      wide_df_updated <- data_rv$wide_df[-input$dataTable_rows_selected, ] # Aktualisiere wide_df

      data_rv$wide_df <- wide_df_updated # Aktualisieren
    }
  })

#### Export als EXCEL #########################################################
  # Handling der Speicherung nach Bestätigung
  observeEvent(input$confirm_save, {
    req(input$save_path, data_rv$wide_df)
    removeModal()

    # Pfad zur Vorlage im "www" Verzeichnis der Shiny-App
    vorlagepath <- file.path("www", "Vorlage_PruefMessprotokoll.xlsx")

    # Erstelle das Workbook mit den Daten
    CreateMessprotokollFUNC(data_rv$wide_df, input$save_path, vorlagepath)

    # Informiere den Benutzer über den Speicherort
    showModal(modalDialog(
      title = "Download abgeschlossen",
      paste("Die Datei wurde gespeichert unter:", input$save_path),
      easyClose = TRUE
    ))
  })
#### Bestätung des Uploads ####################################################
  output$dataUploaded  <- reactive({
    data_rv$dataUploaded
  })
  outputOptions(output, "dataUploaded", suspendWhenHidden = FALSE)

#### Beenden der App ##########################################################
  session$onSessionEnded(function() {
    stopApp()
  })

}