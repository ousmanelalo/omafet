library(shiny)
library(DT) # Pour des tableaux interactifs
library(ggplot2) # Pour les graphiques
library(dplyr) # Pour la manipulation de données
library(shinydashboard) # Pour une interface de tableau de bord moderne
library(readr) # Pour lire les CSV de manière robuste
library(tidyr) # Pour la fonction replace_na
library(shinyalert) # Pour les boîtes de dialogue de confirmation
library(writexl) # Pour écrire des fichiers Excel (xlsx)
library(htmltools) # Assurez-vous que ce package est chargé

# Ajoutez cette ligne au tout début de votre script app.R
# Même si la locale est déjà en UTF-8, cela peut aider à s'assurer de la cohérence.
Sys.setlocale("LC_CTYPE", "fr_FR.UTF-8") # Ou "en_US.UTF-8" si c'est ce que votre système préfère


# Ces fichiers seront créés dans le même répertoire que votre app.R
VENTES_FILE <- "ventes.csv"
DEPENSES_FILE <- "depenses.csv"

# --- Fonction utilitaire pour créer un fichier CSV vide si absent ---
create_empty_csv_if_missing <- function(file_path, col_names) {
  if (!file.exists(file_path)) {
    df <- data.frame(matrix(ncol = length(col_names), nrow = 0))
    colnames(df) <- col_names
    
    # Assurer que les colonnes Date et Montant ont le bon type initial
    if ("Date" %in% col_names) {
      df$Date <- as.Date(character(0))
    }
    if ("Montant" %in% col_names) {
      df$Montant <- as.numeric(character(0))
    }
    # Écrire le CSV en utilisant write.csv de base R avec fileEncoding pour l'UTF-8
    write.csv(df, file_path, row.names = FALSE, fileEncoding = "UTF-8")
    message(paste("Fichier CSV vide créé:", file_path))
  }
}

# Crée les fichiers vides si besoin au démarrage de l'application
create_empty_csv_if_missing(VENTES_FILE, c("Date", "Description", "Montant", "Catégorie"))
create_empty_csv_if_missing(DEPENSES_FILE, c("Date", "Description", "Montant", "Catégorie"))


# --- Définition de l'Interface Utilisateur (UI) ---
ui <- dashboardPage(
  dashboardHeader(title = "Restaurant O'Mafet"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gérant", tabName = "gerant_tab", icon = icon("edit")),
      menuItem("Comptabilité", tabName = "proprietaire_tab", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      # Contenu de l'onglet "Gérant (Saisie)"
      tabItem(tabName = "gerant_tab",
              
              fluidRow(
                box(
                  title = "Saisie des recettes de vente", status = "success", solidHeader = TRUE, width = 6,
                  dateInput("sale_date", "Date de la vente:", value = Sys.Date()),
                  textInput("sale_description", "Description (ex: Vente du jour, Boissons):", placeholder = "Vente du jour"),
                  numericInput("sale_amount", "Montant de la vente (FCFA):", value = 0, min = 0),
                  selectInput("sale_category", "Catégorie:",
                              choices = c("Plats", "Boissons", "Desserts", "Service traiteur", "Autres")),
                  actionButton("add_sale_button", "Ajouter la Recette", icon = icon("plus-circle")),
                  actionButton("update_sale_button", "Confirmer la modification", icon = icon("check")),
                  br(),br(),
                  textOutput("status_message_sale")
                ),
                box(
                  title = "Saisie des Dépenses", status = "danger", solidHeader = TRUE, width = 6,
                  dateInput("expense_date", "Date de la dépense:", value = Sys.Date()),
                  textInput("expense_description", "Description (ex: Achat légumes, Loyer):", placeholder = "Achat légumes"),
                  numericInput("expense_amount", "Montant de la dépense (FCFA):", value = 0, min = 0),
                  selectInput("expense_category", "Catégorie de dépense:",
                              choices = c("Ingrédients", "Salaires", "Loyer", "Nigelec", "NDE", "Carburant",
                                          "Entretien", "Marketing", "Autres")),
                  actionButton("add_expense_button", "Ajouter la Dépense", icon = icon("minus-circle")),
                  actionButton("update_expense_button", "Confirmer la Modification", icon = icon("check")),
                  br(),br(),
                  textOutput("status_message_expense")
                )
              )
      ),
      
      # Contenu de l'onglet "Propriétaire (Comptabilité)"
      tabItem(tabName = "proprietaire_tab",
              fluidRow(
                box(
                  title = "Sélection de la période", status = "primary", solidHeader = TRUE, width = 12,
                  dateRangeInput("date_range_owner", "Sélectionner la période:",
                                 start = Sys.Date() - 30, end = Sys.Date(),
                                 language = "fr", separator = "à")
                )
              ),
              fluidRow(
                infoBoxOutput("total_sales_box", width = 4),
                infoBoxOutput("total_expenses_box", width = 4),
                infoBoxOutput("net_profit_box", width = 4)
              ),
              fluidRow(
                box(
                  title = "Tendances des Recettes et Dépenses", status = "info", solidHeader = TRUE, width = 12,
                  plotOutput("sales_expenses_trend")
                )
              ),
              fluidRow( # Nouvelle ligne pour les graphiques de répartition
                box(
                  title = "Répartition des Recettes par Catégorie", status = "info", solidHeader = TRUE, width = 6,
                  plotOutput("sales_category_pie") # Nouveau plotOutput
                ),
                box(
                  title = "Répartition des Dépenses par Catégorie", status = "info", solidHeader = TRUE, width = 6,
                  plotOutput("expense_category_pie")
                )
              ),
              fluidRow(
                box(
                  title = "Détail des Recettes", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("sales_table"),
                  br(), # Petit espace
                  actionButton("edit_selected_sale", "Modifier la ligne sélectionnée", icon = icon("pencil-alt")),
                  actionButton("delete_selected_sale", "Supprimer la ligne sélectionnée", icon = icon("trash-alt"), class = "btn-danger")
                )
              ),
              fluidRow(
                box(
                  title = "Détail des Dépenses", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("expenses_table"),
                  br(), # Petit espace
                  actionButton("edit_selected_expense", "Modifier la ligne sélectionnée", icon = icon("pencil-alt")),
                  actionButton("delete_selected_expense", "Supprimer la ligne sélectionnée", icon = icon("trash-alt"), class = "btn-danger")
                )
              )
      )
    )
  )
)

# --- Définition de la Logique du Serveur ---
server <- function(input, output, session) {
  
  # Réactifs pour stocker les données lues des CSV
  sales_data_reactive <- reactiveVal(NULL)
  expenses_data_reactive <- reactiveVal(NULL)
  
  # Observer pour charger les données au démarrage et à chaque mise à jour
  observe({
    tryCatch({
      # Forcer la lecture en UTF-8 avec read_csv (car elle supporte l'argument locale)
      sales_data_raw <- read_csv(VENTES_FILE, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
        # Nettoyer la colonne Catégorie pour s'assurer de l'encodage
        mutate(Catégorie = iconv(Catégorie, from = "UTF-8", to = "UTF-8", sub = "byte")) 
      
      sales_data_reactive(sales_data_raw %>%
                            mutate(
                              Montant = as.numeric(Montant),
                              Date = as.Date(Date)
                            ))
      
      # Forcer la lecture en UTF-8 avec read_csv
      expenses_data_raw <- read_csv(DEPENSES_FILE, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
        # Nettoyer la colonne Catégorie pour s'assurer de l'encodage
        mutate(Catégorie = iconv(Catégorie, from = "UTF-8", to = "UTF-8", sub = "byte"))
      
      expenses_data_reactive(expenses_data_raw %>%
                               mutate(
                                 Montant = as.numeric(Montant),
                                 Date = as.Date(Date)
                               ))
      
      showNotification("Données chargées depuis les fichiers CSV.", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Erreur de chargement des données depuis les CSV:", e$message,
                             "Vérifiez que les colonnes Montant et Date sont au bon format dans vos CSV. L'encodage attendu est UTF-8."),
                       type = "error", duration = NULL)
    })
  })
  
  # Réactif pour stocker l'index de la ligne actuellement modifiée (ventes)
  current_sale_edit_index <- reactiveVal(NULL)
  # Réactif pour stocker l'index de la ligne actuellement modifiée (dépenses)
  current_expense_edit_index <- reactiveVal(NULL)
  
  # --- Logique pour l'interface du Gérant ---
  
  # Message de statut pour les recettes
  output$status_message_sale <- renderText({ "" })
  
  # Gérer l'ajout d'une nouvelle recette
  observeEvent(input$add_sale_button, {
    req(input$sale_description, input$sale_amount)
    
    if (input$sale_amount < 0) {
      showNotification("Le montant de la vente ne peut pas être négatif.", type = "warning")
      return()
    }
    
    new_sale <- data.frame(
      Date = input$sale_date,
      Description = input$sale_description,
      Montant = input$sale_amount,
      Catégorie = input$sale_category,
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      current_sales <- sales_data_reactive()
      updated_sales <- bind_rows(current_sales, new_sale)
      # Utiliser write.csv pour l'écriture
      write.csv(updated_sales, VENTES_FILE, row.names = FALSE, fileEncoding = "UTF-8")
      
      # Relire pour assurer la cohérence des types et mettre à jour le réactif
      sales_data_reactive(read_csv(VENTES_FILE, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
                            # Nettoyer la colonne Catégorie à la relecture aussi
                            mutate(Catégorie = iconv(Catégorie, from = "UTF-8", to = "UTF-8", sub = "byte")) %>%
                            mutate(Montant = as.numeric(Montant), Date = as.Date(Date)))
      
      showNotification("Recette ajoutée avec succès!", type = "message")
      updateTextInput(session, "sale_description", value = "")
      updateNumericInput(session, "sale_amount", value = 0)
    }, error = function(e) {
      showNotification(paste("Erreur lors de l'ajout de la recette:", e$message), type = "error")
    })
  })
  
  # Gérer la modification d'une recette sélectionnée
  observeEvent(input$edit_selected_sale, {
    selected_row_index <- input$sales_table_rows_selected
    req(selected_row_index) # S'assurer qu'une ligne est sélectionnée
    
    # Obtient la ligne à partir des données filtrées affichées dans le tableau
    data_to_edit <- filtered_sales()[selected_row_index, ]
    
    # Trouver l'index de cette ligne dans les données non filtrées/originales (sales_data_reactive())
    # C'est une méthode de recherche par correspondance. Idéalement, utilisez un ID unique.
    all_sales <- sales_data_reactive()
    idx_in_original_data <- which(
      all_sales$Date == data_to_edit$Date &
        all_sales$Description == data_to_edit$Description &
        all_sales$Montant == data_to_edit$Montant &
        all_sales$Catégorie == data_to_edit$Catégorie
    )
    
    if (length(idx_in_original_data) == 0) {
      showNotification("La ligne sélectionnée n'a pas pu être trouvée dans les données originales. Elle a peut-être été modifiée ou supprimée. Actualisez la page.", type = "error")
      return()
    } else if (length(idx_in_original_data) > 1) {
      showNotification("Attention: Plusieurs lignes identiques trouvées. La modification pourrait ne pas cibler la bonne ligne. Un ID unique par entrée est recommandé pour éviter cela.", type = "warning", duration = 8)
      # Pour cette implémentation simple, on prendra la première correspondance.
      idx_in_original_data <- idx_in_original_data[1] 
    }
    
    current_sale_edit_index(idx_in_original_data)
    
    # Mettre à jour les champs de saisie avec les données de la ligne sélectionnée
    updateDateInput(session, "sale_date", value = data_to_edit$Date)
    updateTextInput(session, "sale_description", value = data_to_edit$Description)
    updateNumericInput(session, "sale_amount", value = data_to_edit$Montant)
    updateSelectInput(session, "sale_category", selected = data_to_edit$Catégorie)
    
    showNotification("Ligne de recette chargée pour modification. Modifiez les champs et cliquez sur 'Confirmer la Modification'.", type = "message", duration = 5)
    updateTabItems(session, "sidebarMenu", selected = "gerant_tab") # Basculer vers l'onglet Gérant
  })
  
  # Confirmer la modification d'une recette
  observeEvent(input$update_sale_button, {
    req(current_sale_edit_index()) # S'assurer qu'un index est stocké pour modification
    
    if (input$sale_amount < 0) {
      showNotification("Le montant de la vente ne peut pas être négatif.", type = "warning")
      return()
    }
    
    current_sales <- sales_data_reactive()
    idx <- current_sale_edit_index()
    
    # Mettre à jour les valeurs de la ligne à l'index spécifié
    current_sales[idx, "Date"] <- input$sale_date
    current_sales[idx, "Description"] <- input$sale_description
    current_sales[idx, "Montant"] <- input$sale_amount
    current_sales[idx, "Catégorie"] <- input$sale_category
    
    tryCatch({
      # Utiliser write.csv pour l'écriture
      write.csv(current_sales, VENTES_FILE, row.names = FALSE, fileEncoding = "UTF-8")
      sales_data_reactive(read_csv(VENTES_FILE, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
                            # Nettoyer la colonne Catégorie à la relecture aussi
                            mutate(Catégorie = iconv(Catégorie, from = "UTF-8", to = "UTF-8", sub = "byte")) %>%
                            mutate(Montant = as.numeric(Montant), Date = as.Date(Date)))
      
      showNotification("Recette modifiée avec succès!", type = "message")
      current_sale_edit_index(NULL) # Réinitialiser l'index de modification
      updateTextInput(session, "sale_description", value = "") # Nettoyer les champs
      updateNumericInput(session, "sale_amount", value = 0)
    }, error = function(e) {
      showNotification(paste("Erreur lors de la modification de la recette:", e$message), type = "error")
    })
  })
  
  # Gérer la suppression d'une recette sélectionnée
  observeEvent(input$delete_selected_sale, {
    selected_row_index <- input$sales_table_rows_selected
    req(selected_row_index) # S'assurer qu'une ligne est sélectionnée
    
    # Récupérer les données de la ligne à supprimer (pour affichage dans la confirmation)
    data_to_delete <- filtered_sales()[selected_row_index, ]
    
    shinyalert(
      title = "Confirmer la suppression",
      text = paste0("Voulez-vous vraiment supprimer la recette du ", data_to_delete$Date, " : '", data_to_delete$Description, "' (", data_to_delete$Montant, " FCFA) ?"),
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "Oui, supprimer",
      cancelButtonText = "Annuler",
      callbackR = function(value) {
        if (isTRUE(value)) { # Si l'utilisateur clique sur "Oui, supprimer"
          all_sales <- sales_data_reactive()
          
          # Trouver l'index de la ligne dans le dataframe original pour la suppression
          # C'est le même challenge que pour la modification sans ID unique
          idx_in_original_data <- which(
            all_sales$Date == data_to_delete$Date &
              all_sales$Description == data_to_delete$Description &
              all_sales$Montant == data_to_delete$Montant &
              all_sales$Catégorie == data_to_delete$Catégorie
          )
          
          if (length(idx_in_original_data) == 0) {
            showNotification("La ligne sélectionnée n'a pas pu être trouvée dans les données originales. Elle a peut-être été modifiée ou supprimée. Actualisez la page.", type = "error")
            return()
          } else if (length(idx_in_original_data) > 1) {
            showNotification("Attention: Plusieurs lignes identiques trouvées. La suppression pourrait ne pas cibler la bonne ligne. Un ID unique par entrée est recommandé pour éviter cela.", type = "warning", duration = 8)
            # Pour cette implémentation simple, on prendra la première correspondance.
            idx_in_original_data <- idx_in_original_data[1]
          }
          
          tryCatch({
            updated_sales <- all_sales[-idx_in_original_data, ] # Supprimer la ligne
            # Utiliser write.csv pour l'écriture
            write.csv(updated_sales, VENTES_FILE, row.names = FALSE, fileEncoding = "UTF-8")
            sales_data_reactive(read_csv(VENTES_FILE, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
                                  # Nettoyer la colonne Catégorie à la relecture aussi
                                  mutate(Catégorie = iconv(Catégorie, from = "UTF-8", to = "UTF-8", sub = "byte")) %>%
                                  mutate(Montant = as.numeric(Montant), Date = as.Date(Date)))
            showNotification("Recette supprimée avec succès!", type = "message")
          }, error = function(e) {
            showNotification(paste("Erreur lors de la suppression de la recette:", e$message), type = "error")
          })
        }
      }
    )
  })
  
  
  # Message de statut pour les dépenses
  output$status_message_expense <- renderText({ "" })
  
  # Gérer l'ajout d'une nouvelle dépense
  observeEvent(input$add_expense_button, {
    req(input$expense_description, input$expense_amount)
    
    if (input$expense_amount < 0) {
      showNotification("Le montant de la dépense ne peut pas être négatif.", type = "warning")
      return()
    }
    
    new_expense <- data.frame(
      Date = input$expense_date,
      Description = input$expense_description,
      Montant = input$expense_amount,
      Catégorie = input$expense_category,
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      current_expenses <- expenses_data_reactive()
      updated_expenses <- bind_rows(current_expenses, new_expense)
      # Utiliser write.csv pour l'écriture
      write.csv(updated_expenses, DEPENSES_FILE, row.names = FALSE, fileEncoding = "UTF-8")
      
      expenses_data_reactive(read_csv(DEPENSES_FILE, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
                               # Nettoyer la colonne Catégorie à la relecture aussi
                               mutate(Catégorie = iconv(Catégorie, from = "UTF-8", to = "UTF-8", sub = "byte")) %>%
                               mutate(Montant = as.numeric(Montant), Date = as.Date(Date)))
      
      showNotification("Dépense ajoutée avec succès!", type = "message")
      updateTextInput(session, "expense_description", value = "")
      updateNumericInput(session, "expense_amount", value = 0)
    }, error = function(e) {
      showNotification(paste("Erreur lors de l'ajout de la dépense:", e$message), type = "error")
    })
  })
  
  # Gérer la modification d'une dépense sélectionnée
  observeEvent(input$edit_selected_expense, {
    selected_row_index <- input$expenses_table_rows_selected
    req(selected_row_index)
    
    data_to_edit <- filtered_expenses()[selected_row_index, ]
    
    all_expenses <- expenses_data_reactive()
    idx_in_original_data <- which(
      all_expenses$Date == data_to_edit$Date &
        all_expenses$Description == data_to_edit$Description &
        all_expenses$Montant == data_to_edit$Montant &
        all_expenses$Catégorie == data_to_edit$Catégorie
    )
    
    if (length(idx_in_original_data) == 0) {
      showNotification("La ligne sélectionnée n'a pas pu être trouvée dans les données originales. Elle a peut-être été modifiée ou supprimée. Actualisez la page.", type = "error")
      return()
    } else if (length(idx_in_original_data) > 1) {
      showNotification("Attention: Plusieurs lignes identiques trouvées. La modification pourrait ne pas cibler la bonne ligne. Un ID unique par entrée est recommandé pour éviter cela.", type = "warning", duration = 8)
      idx_in_original_data <- idx_in_original_data[1]
    }
    
    current_expense_edit_index(idx_in_original_data)
    
    updateDateInput(session, "expense_date", value = data_to_edit$Date)
    updateTextInput(session, "expense_description", value = data_to_edit$Description)
    updateNumericInput(session, "expense_amount", value = data_to_edit$Montant)
    updateSelectInput(session, "expense_category", selected = data_to_edit$Catégorie)
    
    showNotification("Ligne de dépense chargée pour modification. Modifiez les champs et cliquez sur 'Confirmer la Modification'.", type = "message", duration = 5)
    updateTabItems(session, "sidebarMenu", selected = "gerant_tab")
  })
  
  # Confirmer la modification d'une dépense
  observeEvent(input$update_expense_button, {
    req(current_expense_edit_index())
    
    if (input$expense_amount < 0) {
      showNotification("Le montant de la dépense ne peut pas être négatif.", type = "warning")
      return()
    }
    
    current_expenses <- expenses_data_reactive()
    idx <- current_expense_edit_index()
    
    current_expenses[idx, "Date"] <- input$expense_date
    current_expenses[idx, "Description"] <- input$expense_description
    current_expenses[idx, "Montant"] <- input$expense_amount
    current_expenses[idx, "Catégorie"] <- input$expense_category
    
    tryCatch({
      # Utiliser write.csv pour l'écriture
      write.csv(current_expenses, DEPENSES_FILE, row.names = FALSE, fileEncoding = "UTF-8")
      expenses_data_reactive(read_csv(DEPENSES_FILE, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
                               # Nettoyer la colonne Catégorie à la relecture aussi
                               mutate(Catégorie = iconv(Catégorie, from = "UTF-8", to = "UTF-8", sub = "byte")) %>%
                               mutate(Montant = as.numeric(Montant), Date = as.Date(Date)))
      
      showNotification("Dépense modifiée avec succès!", type = "message")
      current_expense_edit_index(NULL)
      updateTextInput(session, "expense_description", value = "")
      updateNumericInput(session, "expense_amount", value = 0)
    }, error = function(e) {
      showNotification(paste("Erreur lors de la modification de la dépense:", e$message), type = "error")
    })
  })
  
  # Gérer la suppression d'une dépense sélectionnée
  observeEvent(input$delete_selected_expense, {
    selected_row_index <- input$expenses_table_rows_selected
    req(selected_row_index)
    
    data_to_delete <- filtered_expenses()[selected_row_index, ]
    
    shinyalert(
      title = "Confirmer la suppression",
      text = paste0("Voulez-vous vraiment supprimer la dépense du ", data_to_delete$Date, " : '", data_to_delete$Description, "' (", data_to_delete$Montant, " FCFA) ?"),
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "Oui, supprimer",
      cancelButtonText = "Annuler",
      callbackR = function(value) {
        if (isTRUE(value)) {
          all_expenses <- expenses_data_reactive()
          
          idx_in_original_data <- which(
            all_expenses$Date == data_to_delete$Date &
              all_expenses$Description == data_to_delete$Description &
              all_expenses$Montant == data_to_delete$Montant &
              all_expenses$Catégorie == data_to_delete$Catégorie
          )
          
          if (length(idx_in_original_data) == 0) {
            showNotification("La ligne sélectionnée n'a pas pu être trouvée dans les données originales. Elle a peut-être été modifiée ou supprimée. Actualisez la page.", type = "error")
            return()
          } else if (length(idx_in_original_data) > 1) {
            showNotification("Attention: Plusieurs lignes identiques trouvées. La suppression pourrait ne pas cibler la bonne ligne. Un ID unique par entrée est recommandé pour éviter cela.", type = "warning", duration = 8)
            idx_in_original_data <- idx_in_original_data[1]
          }
          
          tryCatch({
            updated_expenses <- all_expenses[-idx_in_original_data, ]
            # Utiliser write.csv pour l'écriture
            write.csv(updated_expenses, DEPENSES_FILE, row.names = FALSE, fileEncoding = "UTF-8")
            expenses_data_reactive(read_csv(DEPENSES_FILE, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
                                     # Nettoyer la colonne Catégorie à la relecture aussi
                                     mutate(Catégorie = iconv(Catégorie, from = "UTF-8", to = "UTF-8", sub = "byte")) %>%
                                     mutate(Montant = as.numeric(Montant), Date = as.Date(Date)))
            showNotification("Dépense supprimée avec succès!", type = "message")
          }, error = function(e) {
            showNotification(paste("Erreur lors de la suppression de la dépense:", e$message), type = "error")
          })
        }
      }
    )
  })
  
  # --- Logique pour l'interface du Propriétaire ---
  
  # Filtrer les données de vente par la période sélectionnée
  filtered_sales <- reactive({
    data <- sales_data_reactive()
    req(data)
    data %>%
      mutate(
        Montant = as.numeric(Montant),
        Date = as.Date(Date)
      ) %>%
      filter(Date >= input$date_range_owner[1] & Date <= input$date_range_owner[2])
  })
  
  # Filtrer les données de dépense par la période sélectionnée
  filtered_expenses <- reactive({
    data <- expenses_data_reactive()
    req(data)
    data %>%
      mutate(
        Montant = as.numeric(Montant),
        Date = as.Date(Date)
      ) %>%
      filter(Date >= input$date_range_owner[1] & Date <= input$date_range_owner[2])
  })
  
  # --- Calculs et Affichage des Value Boxes ---
  output$total_sales_box <- renderInfoBox({
    total_sales <- sum(filtered_sales()$Montant, na.rm = TRUE)
    infoBox(
      "Recettes Totales",
      paste0(format(total_sales, big.mark = " ", scientific = FALSE), " FCFA"),
      icon = icon("money-bill-alt"),
      color = "green",
      fill = TRUE
    )
  })
  
  output$total_expenses_box <- renderInfoBox({
    total_expenses <- sum(filtered_expenses()$Montant, na.rm = TRUE)
    infoBox(
      "Dépenses Totales",
      paste0(format(total_expenses, big.mark = " ", scientific = FALSE), " FCFA"),
      icon = icon("hand-holding-usd"),
      color = "red",
      fill = TRUE
    )
  })
  
  output$net_profit_box <- renderInfoBox({
    net_profit <- sum(filtered_sales()$Montant, na.rm = TRUE) - sum(filtered_expenses()$Montant, na.rm = TRUE)
    color <- if (net_profit >= 0) "blue" else "maroon"
    infoBox(
      "Bénéfice Net",
      paste0(format(net_profit, big.mark = " ", scientific = FALSE), " FCFA"),
      icon = icon("chart-line"),
      color = color,
      fill = TRUE
    )
  })
  
  # --- Graphique des Tendances Recettes/Dépenses ---
  output$sales_expenses_trend <- renderPlot({
    req(filtered_sales(), filtered_expenses())
    
    sales_daily <- filtered_sales() %>%
      group_by(Date) %>%
      summarise(Montant = sum(Montant, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Type = "Recettes")
    
    expenses_daily <- filtered_expenses() %>%
      group_by(Date) %>%
      summarise(Montant = sum(Montant, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Type = "Dépenses")
    
    all_data_daily <- bind_rows(sales_daily, expenses_daily)
    
    min_date <- min(input$date_range_owner[1], all_data_daily$Date, na.rm = TRUE)
    max_date <- max(input$date_range_owner[2], all_data_daily$Date, na.rm = TRUE)
    all_dates <- seq.Date(from = min_date, to = max_date, by = "day")
    
    full_data <- expand.grid(Date = all_dates, Type = c("Recettes", "Dépenses")) %>%
      left_join(all_data_daily, by = c("Date", "Type")) %>%
      replace_na(list(Montant = 0))
    
    ggplot(full_data, aes(x = Date, y = Montant, color = Type)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = "Tendances Quotidiennes des Recettes et Dépenses",
           y = "Montant (FCFA)", x = "Date") +
      theme_minimal() +
      scale_color_manual(values = c("Recettes" = "green", "Dépenses" = "red")) +
      scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # --- Graphique de Répartition des Recettes par Catégorie (NOUVEAU) ---
  output$sales_category_pie <- renderPlot({
    req(filtered_sales())
    sales_summary <- filtered_sales() %>%
      group_by(Catégorie) %>%
      summarise(Total_Montant = sum(Montant, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Pourcentage = Total_Montant / sum(Total_Montant))
    
    if (nrow(sales_summary) == 0 || sum(sales_summary$Total_Montant) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Aucune recette pour cette période.") + theme_void())
    }
    
    ggplot(sales_summary, aes(x = "", y = Total_Montant, fill = Catégorie)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(title = "Répartition des Recettes par Catégorie") +
      geom_text(aes(label = scales::percent(Pourcentage, accuracy = 0.1)),
                position = position_stack(vjust = 0.5), size = 4) +
      theme(legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5))
  })
  
  # --- Graphique de Répartition des Dépenses par Catégorie ---
  output$expense_category_pie <- renderPlot({
    req(filtered_expenses())
    expenses_summary <- filtered_expenses() %>%
      group_by(Catégorie) %>%
      summarise(Total_Montant = sum(Montant, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Pourcentage = Total_Montant / sum(Total_Montant))
    
    if (nrow(expenses_summary) == 0 || sum(expenses_summary$Total_Montant) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Aucune dépense pour cette période.") + theme_void())
    }
    
    ggplot(expenses_summary, aes(x = "", y = Total_Montant, fill = Catégorie)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(title = "Répartition des Dépenses par Catégorie") +
      geom_text(aes(label = scales::percent(Pourcentage, accuracy = 0.1)),
                position = position_stack(vjust = 0.5), size = 4) +
      theme(legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5))
  })
  
  # --- Tableaux Détaillés (DT) pour les Recettes avec boutons d'exportation intégrés ---
  output$sales_table <- renderDT({
    req(filtered_sales())
    datatable(filtered_sales(),
              extensions = 'Buttons', # Active l'extension des boutons
              options = list(
                pageLength = 10,  
                scrollX = TRUE,
                dom = 'Bfrtip', # Place les boutons ('B') en haut
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print') # Définit les boutons
              ),
              filter = 'top',
              selection = 'single', # Permet la sélection d'une seule ligne
              rownames = FALSE
    )
  })
  
  # --- Tableaux Détaillés (DT) pour les Dépenses avec boutons d'exportation intégrés ---
  output$expenses_table <- renderDT({
    req(filtered_expenses())
    datatable(filtered_expenses(),
              extensions = 'Buttons', # Active l'extension des boutons
              options = list(
                pageLength = 10,  
                scrollX = TRUE,
                dom = 'Bfrtip', # Place les boutons ('B') en haut
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print') # Définit les boutons
              ),
              filter = 'top',
              selection = 'single', # Permet la sélection d'une seule ligne
              rownames = FALSE
    )
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)