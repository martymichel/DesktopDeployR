# Funtionen ################################################################

# Funktion um die Zeilen mit dem fehlenden Platzhalter zu korrigieren
correct_lines <- function(text_lines) {
 pattern <- "((M\\d+_(?:FormKegel))|M\\d+)(\\s+)(\\d)"
 corrected_lines <- gsub(pattern, "\\1 X \\4", text_lines)
 return(corrected_lines)
}

####

TextCleanFunction <- function(text) {
 text        <- correct_lines(text) # Anwenden der correct_lines-Funktion
 text        <- paste(text, collapse = "\n") # Zusammenführen der Zeilen zu einem Text
 text_lines  <- strsplit(text, '\n')[[1]] # Aufteilen des Texts in Zeilen
 text_clean  <- sapply(text_lines, trimws) # Anwenden der trimws-Funktion auf alle Zeilen
 text_clean  <- text_clean[text_clean != ""] # Entfernen von leeren Zeichenfolgen
 return(text_clean)
}

####

SearchTableFunction <- function(text_clean) {
 mask <- str_detect(text_clean, "M\\d+") |
  str_detect(text_clean, "M[0-9A-Za-z\\.]+#[0-9]+") |
  str_detect(text_clean, "M[0-9A-Za-z_\\.]+#[0-9A-Za-z]+") |
  str_detect(text_clean, "^\\d{2}a_[PBLY][1-4]_#RC\\d{2}")
 return(mask)
}

####
# Suchen der Zeilen, die die Metadaten enthalten
letzte_element <- function(x) x[[length(x)]] # Funktion zur Extraktion des letzten Elements eines Vektors

####
# Funktion um die Tabellenzeilen zu finden und zu extrahieren
TableParsingFunction <- function(text_clean, mask) {
 table_rows        <- data.frame(text_clean[mask]) # Entfernen von Zeilen, die nicht zu den Messergebnissen gehören
 table_rows_vector <- as.character(table_rows[, 1]) # Datenrahmen in einen Vektor von Zeichenketten umwandeln
 split_rows        <- strsplit(table_rows_vector, "\\s+") # Jede Zeile in ihre Bestandteile aufteilen

 # Entferne alle Einzel- oder Doppelbuchstaben wie "D" oder "TH" aus split_rows
 split_rows <- lapply(split_rows, function(row) {
  row[!grepl("^[A-Za-z]{1,2}$|Wkl|Dst", row)]
 })

 # Die aufgeteilten Zeilen zu einer Matrix zusammenführen
 matrixDat <- do.call(rbind, lapply(split_rows, function(row) {
  c(row, rep(NA, max(sapply(split_rows, length)) - length(row)))[1:max(sapply(split_rows, length))]
 }))

 table_rows   <- as.data.frame(matrixDat, stringsAsFactors = FALSE) # Konvertieren der Matrix in einen Datenrahmen
  table_rows <- table_rows %>%
    mutate(
      V1 = if_else(grepl("#", V1) & grepl("_", V1), V1, gsub("_", "#", V1))
    ) %>%
    separate(V1, c("Merkmal", "Kav"), sep = "#", remove = TRUE)
 colnames(table_rows) <- c("Merkmal", "Kav", "Ist", "Soll", "Toll_pos", "Toll_neg", "Abw") # Neue Spaltennamen für die Tabelle
 table_rows[, 3:7]    <- sapply(table_rows[, 3:7], as.numeric) # Die hinteren fünf Spalten table_rows[, 3:7] in numerische Werte umwandeln
 table_rows           <- table_rows[complete.cases(table_rows), ] # Entfernen von Zeilen mit fehlenden Werten
 return(table_rows)
}

####
# Funktion um die Metadaten aus dem Header zu extrahieren
# Info: tail() sollte "letzte_element()" ersetzen, da die Funktion besser ist
HeaderParsingFunction <- function(text_clean) {
 Pruefer       <- tail(str_split(text_clean[grepl("Pruefer|Bediener", text_clean)], "\\s+")[[1]], 1)
 Date          <- as.Date(letzte_element(str_split(text_clean[startsWith(text_clean, "Uhrzeit:")], "\\s+"))[[5]], "%d.%m.%Y")
 Lot_Nr        <- letzte_element(str_split(text_clean[grepl("Lot.Nr|Lotnummer", text_clean)], "\\s+"))[[2]]
 Artikel       <- letzte_element(str_split(text_clean[startsWith(text_clean, "Artikel-Nr")], "\\s{2,}"))[[2]]
 Artikelname   <- gsub("^Artikel\\s+", "", text_clean[startsWith(text_clean, "Artikel")][[1]])
 Index         <- letzte_element(str_split(text_clean[startsWith(text_clean, "Zeichnungsindex")], "\\s+"))[[2]]
 Werkzeug_Nr   <- letzte_element(str_split(text_clean[grepl("Werkzeug-Nr|Werkzeugnummer", text_clean)], "\\s+"))[[2]]
 Messmaschine  <- tail(str_split(text_clean[grepl("Messmaschine", text_clean)], "\\s+")[[1]], 1)
 Prod_Dat_Zeit <- tail(str_split(text_clean[grepl("Prod.-Dat./Zeit|Produktionsdatum", text_clean)], "\\s+")[[1]], 1)
 Bemerkungen   <- letzte_element(str_split(text_clean[grepl("Bemerkungen|Bemerkung", text_clean)], "\\s{2,}"))[[2]]

 # Prüfen, ob in Bemerkungen die Worte "Nachmessung" oder "Nachm." stehen, wenn ja, Nachmessung = TRUE
 Nachmessung  <- grepl("Nachmessung|Nachm.", Bemerkungen)

 return(data.frame(Nachmessung, Date, Lot_Nr, Artikel, Artikelname, Index, Werkzeug_Nr, Messmaschine, Prod_Dat_Zeit, Pruefer, Bemerkungen))
}

####
# Debugger
# file_path <- choose.files()
# df <- parse_pdf(file_path)

# Parser Funktion
parse_pdf <- function(file_path){
 text        <- pdftools::pdf_text(file_path) # Einlesen des PDFs
 text_clean  <- TextCleanFunction(text) # Anwenden der TextCleanFunction auf den Text
 mask        <- SearchTableFunction(text_clean) # Suchen der Tabellenzeilen anhand der Merkmalsbezeichnungen
 table_rows  <- TableParsingFunction(text_clean, mask) # Anwenden der TableParsingFunction auf den Text
 nCavs       <- length(unique(table_rows$Kav)) # Anzahl der Kavitäten
 metadata    <- HeaderParsingFunction(text_clean) # Extrahieren der Metadaten aus dem Header
 metadata    <- cbind(metadata, nCavs) # Anzahl der Kavitäten an die Metadaten anhängen

 # Beide Datenramen zusammenführen
 df <- cbind(metadata, table_rows)
 return(df)
}

####

# Erzeugt ein Messprotokoll aus den Messergebnissen (in Excel-Format)
CreateMessprotokollFUNC <- function(wide_df, speicherpath, vorlagepath) {
  # Lade die Excel-Vorlage
  wb <- loadWorkbook(vorlagepath)

  # Merkmalnamen extrahieren, die bereits im Breitformat vorliegen
  Names_unique <- gsub("^Ist_", "", grep("^Ist_", names(wide_df), value = TRUE))

  # Konvertiere die Kav-Spalte zu einem Faktor und sortiere die Level in der natürlichen Reihenfolge
  wide_df$Kav <- factor(wide_df$Kav, levels = unique(wide_df$Kav))

  # Einzeiler zum Zuweisen der Schussnummern
  wide_df$Schuss <- cumsum(c(1, diff(as.numeric(wide_df$Kav)) < 0))

  # Dataframe zusammensetzen aus den Informationen von wide_df.
  # Enhält: Prod_Dat_Zeit (nur Datum), Prod_Dat_Zeit (nur Zeit), Schuss, Kav, und alle Messergebnisse (Spalten mit "Ist_" beginnend)
  wide_df$Prod_Datum <- as.Date(sub("/.*", "", wide_df$Prod_Dat_Zeit), format = "%d.%m.%Y")
  wide_df$Prod_Zeit  <- sub(".*/", "", wide_df$Prod_Dat_Zeit)
  messwerte_spalten  <- grep("^Ist_", names(wide_df), value = TRUE)

  # In Spec und out of Spec berechnen. Wieviele Messwerte sind ausserhalb der Toleranz?
  for (i in 1:length(Names_unique)) {
    wide_df[[paste("Spec_", Names_unique[i])]] <- ifelse(wide_df[[paste0("Ist_", Names_unique[i])]] < wide_df[[paste0("Soll_", Names_unique[i])]] +
                                                           wide_df[[paste0("Toll_neg_", Names_unique[i])]] |
                                                           wide_df[[paste0("Ist_", Names_unique[i])]] > wide_df[[paste0("Soll_", Names_unique[i])]] +
                                                           wide_df[[paste0("Toll_pos_", Names_unique[i])]], 1, 0)
  }

  # Erstelle eine transponierte Matrix über die Merkmale, die Anzahl in Toleranz (=0), die Anzahl ausserhalb der Toleranz (=1) und gesamte Anzahl der Messwerte pro Merkmal.
  Spec_Matrix <- matrix(0, nrow = 3, ncol = length(Names_unique))

  for (i in 1:length(Names_unique)) {
    Spec_Matrix[1, i] <- sum(wide_df[[paste("Spec_", Names_unique[i])]] == 0)
    Spec_Matrix[2, i] <- sum(wide_df[[paste("Spec_", Names_unique[i])]] == 1)
    Spec_Matrix[3, i] <- length(wide_df[[paste0("Ist_", Names_unique[i])]])
  }

  # Entfernen von jeglichen Attributen, damit es eine reine Werte-Matrix bleibt
  Spec_Matrix <- unname(Spec_Matrix)

  # Dataframe "Einzelresultate" zusammensetzen aus den Informationen von wide_df.
  Einzelresultate    <- wide_df[, c("Prod_Datum", "Prod_Zeit", "Schuss", "Kav", messwerte_spalten)]

  # Kavität wieder zu Charakter umwandeln
  Einzelresultate$Kav <- as.character(Einzelresultate$Kav)

  # Umbenennen der Spaltentitel zu "Datum", "Zeit", "Schuss", "Kavität" und den Merkmalnamen ohne "Ist_"
  names(Einzelresultate) <- c("Datum", "Zeit", "Schuss", "Kavität", Names_unique)

  #### Worksheet Einzelresultate ausfüllen ########################################
  # Namen angeben
  sheet2 <- "Einzelresultate"

  # Schreiben Sie die Daten in das zweite Worksheet
  writeData(wb, sheet = sheet2, x = Einzelresultate, startCol = 1, startRow = 9, rowNames = FALSE, colNames = TRUE)  # Einfügen der Daten

  #### Worksheet Zusammenfassung ausfüllen ########################################

  # Wählen Sie das zweite Worksheet aus
  sheet1 <- "Zusammenfassung"

  # Schreiben Sie die Daten in das erste Worksheet
  writeData(wb, sheet = sheet1, x = unique(wide_df$Artikel)[1],     startCol = 4, startRow = 3) # Artikelnummer
  writeData(wb, sheet = sheet1, x = unique(wide_df$Artikelname)[1], startCol = 4, startRow = 4) # Artikelname
  writeData(wb, sheet = sheet1, x = unique(wide_df$Lot_Nr)[1],      startCol = 4, startRow = 5) # Lotnummer

  writeData(wb, sheet = sheet1, x = unique(wide_df$Werkzeug_Nr)[1], startCol = 10, startRow = 3) # Werkzeugnummer
  writeData(wb, sheet = sheet1, x = unique(wide_df$Index)[1],       startCol = 10, startRow = 4) # Index
  writeData(wb, sheet = sheet1, x = unique(wide_df$nCavs)[1],       startCol = 10, startRow = 5) # Anzahl Kavitäten

  writeData(wb, sheet = sheet1, x = "N/A", startCol = 15, startRow = 3) # Zeichnungsnummer nicht verfügbar

  # Dataframe "Zusammenfassung" zusammensetzen aus den Informationen von wide_df.
  # Enhält: "Names_unique", die Sollwerte (Spalten mit "Soll_" beginnend), die oberen Toleranzen (Spalten mit "Toll_pos_" beginnend), die unteren Toleranzen (Spalten mit "Toll_neg_" beginnend)
  sollwerte_spalten  <- grep("^Soll_", names(wide_df), value = TRUE)
  sollwerte_values   <- unlist(unique(wide_df[, sollwerte_spalten]))

  toll_pos_spalten   <- grep("^Toll_pos_", names(wide_df), value = TRUE)
  toll_pos_values    <- unlist(unique(wide_df[, toll_pos_spalten]))

  toll_neg_spalten   <- grep("^Toll_neg_", names(wide_df), value = TRUE)
  toll_neg_values    <- unlist(unique(wide_df[, toll_neg_spalten]))

  Zusammenfassung    <- data.frame(rbind(sollwerte_values, toll_pos_values, toll_neg_values), stringsAsFactors = FALSE)
  # Entferne die Zeilennamen und verwende die Namen in der ersten Zeile als Spaltennamen
  rownames(Zusammenfassung) <- NULL
  colnames(Zusammenfassung) <- Names_unique

  # Füge Zusammenfassung ins sheet1 ein auf Position 5, 8
  writeData(wb, sheet = sheet1, x = Zusammenfassung, startCol = 5, startRow = 8)  # Einfügen der Daten

  # Füge die InSpec/outOfSpec-Informationen ins sheet1 ein auf der ersten Position 5, 23 ( + Merkmale). Wichtig: ohne Spaltennamen und Zeilennamen der Matrix
  writeData(wb, sheet = sheet1, x = Spec_Matrix, startCol = 5, startRow = 23, rowNames = FALSE, colNames = FALSE)  # Einfügen der Daten

  # Messmittelnummer für jedes Merkmal in seine eigene Zelle schreiben
  for (i in 1:length(Names_unique)) {
    writeData(wb, sheet = sheet1, x = unique(wide_df$Messmaschine)[1], startCol = 4 + i, startRow = 26)  # Messmittelnummer
  }

  # Bemerkungen
  comments <- paste("Bemerkungen aus PDF:", paste(unique(wide_df$Bemerkungen), collapse = ", "))
  writeData(wb, sheet = sheet1, x = comments, startCol = 2, startRow = 29) # Bemerkungen

  # #### Speicherung der Datei unter einem neuen Namen #############################
  # # Neuen Dateinamen festlegen
  # datecode <- format(Sys.Date(), "%Y%m%d") # Datumcode YYYYMMDD erzeugen
  # namelot  <- unique(wide_df$Lot_Nr)[1]
  # artNr    <- unique(wide_df$Artikel)[1]
  #
  # filename <- paste(datecode, namelot, artNr, "Messprotokoll.xlsx", sep = "_")

  # Speichern der Datei unter einem neuen Namen
  wb <- saveWorkbook(wb, speicherpath, overwrite = TRUE, returnValue = TRUE)

  return(wb)

}
