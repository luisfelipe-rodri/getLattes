library("getLattes")
library("dplyr")
library("shiny")
library("shinydashboard")
library("xml2")
library("writexl")
library("DT")
library("purrr")
library("shinydashboard")
library("waiter")
library("shinyjs")

appCSS <- "
#loading-content {
  position: fixed;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  background: #f8f9fa;
  border: 1px solid #ced4da;
  border-radius: 8px;
  box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
  padding: 20px;
  z-index: 1000;
  text-align: center;
  color: #495057;
}
#loading-content h2 {
  margin-bottom: 15px;
  font-size: 18px;
  font-weight: bold;
  color: #007bff;
}
#loading-content img {
  max-width: 100%;
  height: auto;
}
"

infos <- c("Artigos Publicados" = "artigos_publicados",
           "Graduação" = "formacao_graduacao",
           "Mestrado" = "formacao_mestrado",
           "Doutorado" = "formacao_doutorado",
           "Areas de Atuação" = "areas_atuacao",
           "Atuações Profissionais" = "atuacoes_profissionais",
           "Bancas Doutorado" = "bancas_doutorado",
           "Bancas Mestrado" = "bancas_mestrado",
           "Bancas Graduação" = "bancas_graduacao",
           "Capítulo em Livros" = "capitulos_livros",
           "Endereço Profissional" = "endereco_profissional",
           "Eventos e Congressos" = "eventos_congressos",
           "Idiomas" = "idiomas",
           "Linha de Pesquisa" = "linha_pesquisa",
           "Livros Publicados" = "livros_publicados",
           "Organização de eventos" = "organizacao_eventos",
           "Orientações Doutorado" = "orientacoes_doutorado",
           "Orientações Mestrado" = "orientacoes_mestrado",
           "Orientações Pós-Doutorado" = "orientacoes_posdoutorado",
           "Outras Produções Ténicas" = "outras_producoes_tecnicas",
           "Projeto de Pesquisa" = "participacao_projeto",
           "Produção técnica" = "producao_tecnica")

# Define UI
ui <- fluidPage(
  
  useShinyjs(),
  
  inlineCSS(appCSS),
  
  dashboardPage(
    
    dashboardHeader(title = "Get Lattes", titleWidth = 350,
                    
                    tags$li(class = "dropdown", tags$a(href = "https://roneyfraga.com/getLattes/articles/introduction_getLattes.html", class = "my_class", "Sobre", target="_blank"))
                    
                    ),
    
    dashboardSidebar(
      
      width = 350,
      
      fileInput("files", "Faça upload dos arquivos .zip",
                accept = c(".zip"), multiple = TRUE),
      
      checkboxGroupInput("tables", "Selecione as informações:",
                         infos, inline = TRUE,
                         selected = c("Dados Gerais" = "dados_gerais")),
      
      actionButton("submit", "Enviar")
      
    ),
    
    dashboardBody(
      
      fluidRow(
        
        align = "center",
        
        box(
          
          title = "Currículos Carregados", status = "primary",
          solidHeader = TRUE, length=355,
          
          hidden(div(
            id = "loading-content",
            h2("Carregando...")
          )),
          
          DTOutput("xml_tables")
          
          ),
        
        box(
          
          align = "center",
          
          h4("Faça download das informações selecionadas"),
          
          downloadButton('download',"Download")
          
          )
        
      )
    )
  )
  
)

# Define server
server <- function(input, output) {
  
  observeEvent(input$submit, {
    
    show("loading-content")
    
    # Check if files are selected
    req(input$files)
    
    # Create a unique temporary directory
    temp_dir <- tempdir()
    
    # Unzip each selected file
    unzip_dirs <- lapply(seq_along(input$files$datapath), function(i) {
      dir <- tempfile(pattern = paste0("file_", i), tmpdir = temp_dir)
      dir.create(dir)
      unzip(input$files$datapath[i], exdir = dir)
      return(dir)
    })
    
    # Read XML files and create tables
    xml_tables <- lapply(unzip_dirs, function(unzip_dir) {
      xml_files <- list.files(unzip_dir, pattern = "\\.xml$", full.names = TRUE)
      xml_data <- lapply(xml_files, xml2::read_xml)
      return(xml_data)
    })
    
    xml_select <- lapply(xml_tables, function(table) table[[seq_along(table)]])
    
    # Selected tables
    
    tables_selected <- list()
    
    dados_gerais <- data.frame(bind_rows(lapply(xml_select, getDadosGerais)))
    tables_selected[["dados_gerais"]] <- dados_gerais
    
    if ("artigos_publicados" %in% input$tables) {
      
      artigos_publicados <- data.frame(bind_rows(lapply(xml_select, getArtigosPublicados)))
      tables_selected[["artigos_publicados"]] <- artigos_publicados
      
    }
    
    if ("linha_pesquisa" %in% input$tables) {
      
      linha_pesquisa <- data.frame(bind_rows(lapply(xml_select, getLinhaPesquisa)))
      tables_selected[["linha_pesquisa"]] <- linha_pesquisa
      
    }
    
    if ("formacao_graduacao" %in% input$tables) {
      
      formacao_graduacao <- data.frame(bind_rows(lapply(xml_select, getFormacaoGraduacao)))
      tables_selected[["formacao_graduacao"]] <- formacao_graduacao |> distinct()
      
    }
    
    if ("formacao_mestrado" %in% input$tables) {
      
      xml_select |>
        purrr::map(safely(getFormacaoMestrado)) |>
        purrr::map(pluck, 'result') |>
        purrr::discard(is.null) |>
        dplyr::bind_rows() |>
        dplyr::distinct() -> formacao_mestrado
      tables_selected[["formacao_mestrado"]] <- formacao_mestrado
      
    }
    
    if ("formacao_doutorado" %in% input$tables) {
      
      xml_select |>
        purrr::map(safely(getFormacaoDoutorado)) |>
        purrr::map(pluck, 'result') |>
        purrr::discard(is.null) |>
        dplyr::bind_rows() |>
        dplyr::distinct() -> formacao_doutorado
      tables_selected[["formacao_doutorado"]] <- formacao_doutorado
      
    }
    
    if ("areas_atuacao" %in% input$tables) {
      
      areas_atuacao <- data.frame(bind_rows(lapply(xml_select, getAreasAtuacao)))
      tables_selected[["areas_atuacao"]] <- areas_atuacao
      
    }
    
    if ("atuacoes_profissionais" %in% input$tables) {
      
      atuacoes_profissionais <- data.frame(bind_rows(lapply(xml_select, getAtuacoesProfissionais)))
      tables_selected[["atuacoes_profissionais"]] <- atuacoes_profissionais
      
    }
    
    if ("bancas_doutorado" %in% input$tables) {
      
      bancas_doutorado <- data.frame(bind_rows(lapply(xml_select, getBancasDoutorado)))
      tables_selected[["bancas_doutorado"]] <- bancas_doutorado
      
    }
    
    if ("bancas_graduacao" %in% input$tables) {
      
      bancas_graduacao <- data.frame(bind_rows(lapply(xml_select, getBancasGraduacao)))
      tables_selected[["bancas_graduacao"]] <- bancas_graduacao
      
    }
    
    if ("capitulos_livros" %in% input$tables) {
      
      capitulos_livros <- data.frame(bind_rows(lapply(xml_select, getCapitulosLivros)))
      tables_selected[["capitulos_livros"]] <- capitulos_livros
      
    }
    
    if ("endereco_profissional" %in% input$tables) {
      
      endereco_profissional <- data.frame(bind_rows(lapply(xml_select, getEnderecoProfissional)))
      tables_selected[["endereco_profissional"]] <- endereco_profissional
      
    }
    
    if ("eventos_congressos" %in% input$tables) {
      
      eventos_congressos <- data.frame(bind_rows(lapply(xml_select, getEventosCongressos)))
      tables_selected[["eventos_congressos"]] <- eventos_congressos
      
    }
    
    if ("idiomas" %in% input$tables) {
      
      idiomas <- data.frame(bind_rows(lapply(xml_select, getIdiomas)))
      tables_selected[["idiomas"]] <- idiomas
      
    }
    
    if ("linha_pesquisa" %in% input$tables) {
      
      linha_pesquisa <- data.frame(bind_rows(lapply(xml_select, getLinhaPesquisa)))
      tables_selected[["linha_pesquisa"]] <- linha_pesquisa
      
    }
    
    if ("livros_publicados" %in% input$tables) {
      
      livros_publicados <- data.frame(bind_rows(lapply(xml_select, getLivrosPublicados)))
      tables_selected[["livros_publicados"]] <- livros_publicados
      
    }
    
    if ("organizacao_eventos" %in% input$tables) {
      
      organizacao_eventos <- data.frame(bind_rows(lapply(xml_select, getOrganizacaoEventos)))
      tables_selected[["organizacao_eventos"]] <- organizacao_eventos
      
    }
    
    if ("orientacoes_doutorado" %in% input$tables) {
      
      orientacoes_doutorado <- data.frame(bind_rows(lapply(xml_select, getOrientacoesDoutorado)))
      tables_selected[["orientacoes_doutorado"]] <- orientacoes_doutorado
      
    }
    
    if ("orientacoes_mestrado" %in% input$tables) {
      
      orientacoes_mestrado <- data.frame(bind_rows(lapply(xml_select, getOrientacoesMestrado)))
      tables_selected[["orientacoes_mestrado"]] <- orientacoes_mestrado
      
    }
    
    if ("orientacoes_posdoutorado" %in% input$tables) {
      
      orientacoes_posdoutorado <- data.frame(bind_rows(lapply(xml_select, getOrientacoesPosDoutorado)))
      tables_selected[["orientacoes_posdoutorado"]] <- orientacoes_posdoutorado
      
    }
    
    if ("outras_producoes_tecnicas" %in% input$tables) {
      
      outras_producoes_tecnicas <- data.frame(bind_rows(lapply(xml_select, getProducaoTecnica)))
      tables_selected[["outras_producoes_tecnicas"]] <- outras_producoes_tecnicas
      
    }
    
    if ("participacao_projeto" %in% input$tables) {
      
      participacao_projeto <- data.frame(bind_rows(lapply(xml_select, getParticipacaoProjeto)))
      tables_selected[["participacao_projeto"]] <- participacao_projeto
      
    }
    
    if ("producao_tecnica" %in% input$tables) {
      
      producao_tecnica <- data.frame(bind_rows(lapply(xml_select, getProducaoTecnica)))
      tables_selected[["producao_tecnica"]] <- producao_tecnica
      
    }
    
    hide(id = "loading-content", anim = TRUE, animType = "fade") 
    
    output$xml_tables <- DT::renderDataTable({
      datatable(
        dados_gerais[1],
        options = list(
          scrollX = TRUE,
          scrollY = "350px",
          paging = FALSE,
          searching = FALSE
        )        
      )
    })
    
    output$download <- downloadHandler(
      filename = function(){"lattes.xlsx"}, 
      content = function(fname){
        write_xlsx(tables_selected, fname)
      }
    )
    
  })
}

# Run the Shiny app
shinyApp(ui, server)

