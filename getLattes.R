library(getLattes)
library(dplyr)
library(shiny)
library(bslib)
library(xml2)
library(writexl)
library(DT)
library(purrr)
library(shinyjs)

# ------------------------------------------------------------------
# Catálogo de informações: rótulo -> id, agrupado por tema
# ------------------------------------------------------------------
grupos <- list(
  "Formação e atuação" = c(
    "Graduação"               = "formacao_graduacao",
    "Mestrado"                = "formacao_mestrado",
    "Doutorado"               = "formacao_doutorado",
    "Áreas de atuação"        = "areas_atuacao",
    "Atuações profissionais"  = "atuacoes_profissionais",
    "Endereço profissional"   = "endereco_profissional",
    "Idiomas"                 = "idiomas",
    "Linha de pesquisa"       = "linha_pesquisa"
  ),
  "Produção" = c(
    "Artigos publicados"      = "artigos_publicados",
    "Capítulos de livros"     = "capitulos_livros",
    "Livros publicados"       = "livros_publicados",
    "Produção técnica"        = "producao_tecnica",
    "Outras produções técnicas" = "outras_producoes_tecnicas",
    "Projetos de pesquisa"    = "participacao_projeto"
  ),
  "Orientações e bancas" = c(
    "Orientações de doutorado"     = "orientacoes_doutorado",
    "Orientações de mestrado"      = "orientacoes_mestrado",
    "Orientações de pós-doutorado" = "orientacoes_posdoutorado",
    "Bancas de doutorado"          = "bancas_doutorado",
    "Bancas de mestrado"           = "bancas_mestrado",
    "Bancas de graduação"          = "bancas_graduacao"
  ),
  "Eventos" = c(
    "Eventos e congressos"    = "eventos_congressos",
    "Organização de eventos"  = "organizacao_eventos"
  )
)

# id -> nome da função do pacote getLattes
funcoes <- c(
  artigos_publicados        = "getArtigosPublicados",
  formacao_graduacao        = "getFormacaoGraduacao",
  formacao_mestrado         = "getFormacaoMestrado",
  formacao_doutorado        = "getFormacaoDoutorado",
  areas_atuacao             = "getAreasAtuacao",
  atuacoes_profissionais    = "getAtuacoesProfissionais",
  bancas_doutorado          = "getBancasDoutorado",
  bancas_mestrado           = "getBancasMestrado",
  bancas_graduacao          = "getBancasGraduacao",
  capitulos_livros          = "getCapitulosLivros",
  endereco_profissional     = "getEnderecoProfissional",
  eventos_congressos        = "getEventosCongressos",
  idiomas                   = "getIdiomas",
  linha_pesquisa            = "getLinhaPesquisa",
  livros_publicados         = "getLivrosPublicados",
  organizacao_eventos       = "getOrganizacaoEventos",
  orientacoes_doutorado     = "getOrientacoesDoutorado",
  orientacoes_mestrado      = "getOrientacoesMestrado",
  orientacoes_posdoutorado  = "getOrientacoesPosDoutorado",
  outras_producoes_tecnicas = "getProducaoTecnica",
  participacao_projeto      = "getParticipacaoProjeto",
  producao_tecnica          = "getProducaoTecnica"
)

todos_ids <- unlist(grupos, use.names = FALSE)

# ------------------------------------------------------------------
# Tema e CSS
# ------------------------------------------------------------------
tema <- bs_theme(
  version      = 5,
  bootswatch   = "flatly",
  primary      = "#1f6f8b",
  secondary    = "#6c7a89",
  success      = "#2a9d8f",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter"),
  "border-radius" = "0.6rem"
)

css <- "
body { background: #f4f6f9; }
.navbar-brand { font-weight: 700; letter-spacing: .3px; }
.card { border: 0; box-shadow: 0 2px 10px rgba(20, 40, 60, .07); }
.card-header { font-weight: 600; background: #fff; border-bottom: 1px solid #eef1f4; }
.bslib-sidebar-layout > .sidebar { background: #fff; }
.grupo-titulo {
  font-size: .72rem; font-weight: 700; text-transform: uppercase;
  letter-spacing: .08em; color: #6c7a89; margin: 1rem 0 .25rem;
}
.shiny-options-group .checkbox, .shiny-options-group .form-check { margin-bottom: 0; }
.btn-enviar { font-weight: 600; }
#acc_sobre { margin-top: .25rem; }
.card-body > * + * { margin-top: .5rem; }
.vazio {
  text-align: center; color: #8795a1; padding: 3rem 1rem;
}
.vazio .icone { font-size: 2.5rem; margin-bottom: .5rem; }
.dataTables_wrapper { font-size: .9rem; }
#loading-content {
  position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%);
  background: #fff; border-radius: 12px; padding: 24px 36px; z-index: 2000;
  box-shadow: 0 8px 30px rgba(0, 0, 0, .18); text-align: center; color: #1f6f8b;
}
#loading-overlay {
  position: fixed; inset: 0; background: rgba(244, 246, 249, .7); z-index: 1999;
}
"

# ------------------------------------------------------------------
# UI
# ------------------------------------------------------------------
ui <- page_sidebar(
  title = tagList(icon("graduation-cap"), " Get Lattes"),
  theme = tema,
  fillable = FALSE,
  useShinyjs(),
  tags$head(tags$style(HTML(css))),

  sidebar = sidebar(
    width = 360,

    h6("1. Currículos", class = "fw-bold mb-2"),
    fileInput(
      "files", NULL,
      accept = ".zip", multiple = TRUE,
      buttonLabel = "Selecionar…",
      placeholder = "Arquivos .zip do Lattes"
    ),

    h6("2. Informações", class = "fw-bold mt-2 mb-1"),
    div(
      class = "d-flex gap-2 mb-1",
      actionButton("sel_todos", "Marcar todas",   class = "btn-sm btn-outline-primary"),
      actionButton("sel_nenhum", "Limpar",        class = "btn-sm btn-outline-secondary")
    ),
    helpText("Os dados gerais sempre são incluídos."),

    accordion(
      id = "acc_infos", open = FALSE, multiple = TRUE,
      !!!imap(grupos, function(opcoes, titulo) {
        accordion_panel(
          title = titulo,
          checkboxGroupInput(
            paste0("tabs_", make.names(titulo)), NULL,
            choices = opcoes
          )
        )
      })
    ),

    actionButton(
      "submit", "Processar currículos",
      icon = icon("play"), class = "btn-primary btn-enviar w-100 mt-3"
    ),

    hr(),
    div(class = "grupo-titulo", "Ajuda"),
  accordion(
    id = "acc_sobre", open = FALSE, multiple = TRUE,
    accordion_panel(
      value = "sobre",
      title = tagList(icon("circle-info"), " Sobre este aplicativo"),
      p("Este aplicativo é uma interface gráfica para o pacote ",
        tags$a(
          tags$code("getLattes"),
          href = "https://cran.r-project.org/web/packages/getLattes/refman/getLattes.html",
          target = "_blank", rel = "noopener"
        ),
        " do R. Ele permite extrair informações de currículos da Plataforma Lattes ",
        "sem escrever código: o pacote faz a leitura dos arquivos XML e este app ",
        "organiza o processo em uma página web."),
      p(class = "text-muted small mb-0",
        "Os arquivos enviados são processados apenas durante a sessão. ",
        "A extração de dados é feita pelo pacote getLattes; este aplicativo ",
        "não altera nem substitui o pacote.")
    ),
    accordion_panel(
      value = "uso",
      title = tagList(icon("circle-question"), " Como usar"),
      tags$ol(
        class = "mb-3",
        tags$li(tags$strong("Obtenha os currículos."),
                " Na Plataforma Lattes, abra o currículo desejado e baixe-o em ",
                "formato XML. O arquivo vem compactado em .zip; ",
                "não é preciso descompactá-lo."),
        tags$li(tags$strong("Envie os arquivos."),
                " Na barra lateral, em “1. Currículos”, clique em ",
                "“Selecionar…” e escolha um ou mais arquivos .zip ",
                "(cada .zip deve conter o XML de um currículo)."),
        tags$li(tags$strong("Escolha as informações."),
                " Em “2. Informações”, abra os grupos e marque o que deseja ",
                "extrair. Use “Marcar todas” para selecionar tudo ou “Limpar” ",
                "para desmarcar. Os dados gerais sempre são incluídos."),
        tags$li(tags$strong("Processe."),
                " Clique em “Processar currículos” e aguarde. ",
                "Com muitos currículos ou muitas informações, pode demorar um pouco."),
        tags$li(tags$strong("Confira os resultados."),
                " Os indicadores no topo mostram quantos currículos, tabelas e ",
                "linhas foram gerados. Na pré-visualização, use o seletor para ",
                "alternar entre as tabelas e os filtros de cada coluna para buscar."),
        tags$li(tags$strong("Exporte."),
                " Clique em “Baixar lattes.xlsx” para obter um arquivo Excel ",
                "com uma aba para cada informação selecionada.")
      ),
      p(class = "text-muted small mb-0",
        tags$strong("Dica: "),
        "se você alterar a seleção ou enviar novos arquivos, clique novamente ",
        "em “Processar currículos” para atualizar as tabelas e o download.")
    )
  )
  ),

  # Indicadores
  layout_columns(
    col_widths = c(4, 4, 4),
    gap = "1.5rem",
    class = "mb-4",
    value_box("Currículos", textOutput("n_cvs"),
              showcase = icon("users"), theme = "primary"),
    value_box("Tabelas geradas", textOutput("n_tabelas"),
              showcase = icon("table"), theme = "success"),
    value_box("Linhas no total", textOutput("n_linhas"),
              showcase = icon("list"), theme = "secondary")
  ),

  layout_columns(
    col_widths = c(8, 4),
    gap = "1.5rem",
    class = "mb-4",

    card(
      card_header(icon("file-lines"), " Pré-visualização"),
      full_screen = TRUE,
      min_height = 420,
      uiOutput("seletor_tabela"),
      DTOutput("xml_tables")
    ),

    card(
      card_header(icon("download"), " Exportação"),
      p("Baixe um arquivo Excel com uma aba para cada informação selecionada.",
        class = "text-muted"),
      uiOutput("resumo_selecao"),
      downloadButton("download", "Baixar lattes.xlsx",
                     class = "btn-success w-100 mt-2")
    )
  ),

  tags$footer(
    class = "text-center text-muted small py-3",
    "Interface para o pacote ",
    tags$a("getLattes",
           href = "https://cran.r-project.org/web/packages/getLattes/refman/getLattes.html",
           target = "_blank", rel = "noopener"),
    " (R)."
  )
)

# ------------------------------------------------------------------
# Servidor
# ------------------------------------------------------------------
server <- function(input, output, session) {

  # Estado
  tabelas <- reactiveVal(NULL)

  # Habilita o download apenas quando há dados
  disable("download")

  # Seleção em massa
  ids_grupos <- setNames(
    paste0("tabs_", make.names(names(grupos))), names(grupos)
  )

  observeEvent(input$sel_todos, {
    iwalk(grupos, function(opcoes, titulo) {
      updateCheckboxGroupInput(session, ids_grupos[[titulo]], selected = opcoes)
    })
  })

  observeEvent(input$sel_nenhum, {
    iwalk(grupos, function(opcoes, titulo) {
      updateCheckboxGroupInput(session, ids_grupos[[titulo]], selected = character(0))
    })
  })

  selecionadas <- reactive({
    unlist(lapply(ids_grupos, function(id) input[[id]]), use.names = FALSE)
  })

  output$resumo_selecao <- renderUI({
    n <- length(selecionadas())
    tags$p(
      tags$strong(n), if (n == 1) " informação adicional selecionada"
      else " informações adicionais selecionadas",
      class = "mb-0"
    )
  })

  # Processamento
  observeEvent(input$submit, {

    if (is.null(input$files)) {
      showNotification("Envie ao menos um arquivo .zip.", type = "warning")
      return()
    }

    show("loading-content"); show("loading-overlay")
    on.exit({ hide("loading-content"); hide("loading-overlay") }, add = TRUE)

    # Descompacta cada .zip em um diretório temporário próprio
    dirs <- lapply(seq_along(input$files$datapath), function(i) {
      d <- tempfile(pattern = paste0("cv_", i, "_"))
      dir.create(d)
      unzip(input$files$datapath[i], exdir = d)
      d
    })

    # Lê o primeiro XML de cada zip
    xml_select <- lapply(dirs, function(d) {
      arq <- list.files(d, pattern = "\\.xml$", full.names = TRUE)
      if (length(arq) == 0) return(NULL)
      xml2::read_xml(arq[1])
    }) |> purrr::compact()

    if (length(xml_select) == 0) {
      showNotification("Nenhum XML encontrado nos arquivos enviados.",
                       type = "error")
      return()
    }

    # Extrai uma tabela, ignorando currículos que falhem
    extrair <- function(nome_fun) {
      fun <- getExportedValue("getLattes", nome_fun)
      xml_select |>
        purrr::map(purrr::safely(fun)) |>
        purrr::map("result") |>
        purrr::discard(is.null) |>
        dplyr::bind_rows() |>
        dplyr::distinct() |>
        as.data.frame()
    }

    ids <- unique(c("dados_gerais", selecionadas()))
    funs <- c(dados_gerais = "getDadosGerais", funcoes)

    resultado <- list()
    withProgress(message = "Processando…", value = 0, {
      for (id in ids) {
        incProgress(1 / length(ids), detail = id)
        resultado[[id]] <- tryCatch(
          extrair(funs[[id]]),
          error = function(e) {
            showNotification(paste("Falha em", id, ":", conditionMessage(e)),
                             type = "error")
            NULL
          }
        )
      }
    })

    resultado <- purrr::discard(resultado, is.null)
    tabelas(resultado)
    enable("download")

    showNotification(
      paste0(length(resultado), " tabela(s) gerada(s) para ",
             length(xml_select), " currículo(s)."),
      type = "message"
    )
  })

  # Indicadores
  output$n_cvs <- renderText({
    t <- tabelas()
    if (is.null(t)) "—" else nrow(t$dados_gerais)
  })
  output$n_tabelas <- renderText({
    t <- tabelas()
    if (is.null(t)) "—" else length(t)
  })
  output$n_linhas <- renderText({
    t <- tabelas()
    if (is.null(t)) "—" else format(sum(vapply(t, nrow, integer(1))),
                                    big.mark = ".")
  })

  # Seletor da tabela exibida
  output$seletor_tabela <- renderUI({
    t <- tabelas()
    req(t)
    rotulos <- setNames(todos_ids, names(unlist(grupos)))
    nomes <- vapply(names(t), function(id) {
      r <- names(todos_ids)[match(id, todos_ids)]
      if (is.na(r)) "Dados gerais" else r
    }, character(1))
    selectInput("tabela_ver", "Tabela exibida:",
                choices = setNames(names(t), nomes), width = "100%")
  })

  # Pré-visualização
  output$xml_tables <- renderDT({
    t <- tabelas()
    if (is.null(t)) {
      return(datatable(
        data.frame(Aviso = "Envie os arquivos .zip e clique em “Processar currículos”."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    req(input$tabela_ver)
    datatable(
      t[[input$tabela_ver]],
      rownames = FALSE,
      filter = "top",
      class = "stripe hover compact",
      options = list(
        scrollX = TRUE,
        scrollY = "340px",
        pageLength = 25,
        deferRender = TRUE,
        language = list(
          search = "Buscar:", info = "_START_–_END_ de _TOTAL_",
          lengthMenu = "_MENU_ por página",
          paginate = list(previous = "Anterior", `next` = "Próxima"),
          zeroRecords = "Nenhum registro encontrado"
        )
      )
    )
  })

  # Download
  output$download <- downloadHandler(
    filename = function() "lattes.xlsx",
    content = function(file) {
      req(tabelas())
      writexl::write_xlsx(tabelas(), file)
    }
  )
}

# Elemento de carregamento (fora do UI principal para ficar acima de tudo)
ui <- tagList(
  ui,
  hidden(div(id = "loading-overlay")),
  hidden(div(
    id = "loading-content",
    h4("Carregando…"),
    div(class = "spinner-border text-primary", role = "status")
  ))
)

shinyApp(ui, server)
