# Get Lattes

[![CRAN](https://www.r-pkg.org/badges/version/getLattes)](https://cran.r-project.org/package=getLattes)
[![Shiny App](https://img.shields.io/badge/app-shinyapps.io-1f6f8b)](https://luisfeliperodrigues.shinyapps.io/getLattes/)

Aplicativo web para **extrair dados de currículos da Plataforma Lattes** e exportá-los em uma planilha Excel, sem escrever código. Ele é uma interface gráfica para o pacote [`getLattes`](https://cran.r-project.org/web/packages/getLattes/refman/getLattes.html), escrito em R.

**Acesse o aplicativo:** <https://luisfeliperodrigues.shinyapps.io/getLattes/>

---

## Sumário

- [Sobre](#sobre)
- [O aplicativo](#o-aplicativo)
  - [Como usar](#como-usar)
  - [Informações que podem ser extraídas](#informações-que-podem-ser-extraídas)
  - [Executar localmente](#executar-localmente)
  - [Publicar no shinyapps.io](#publicar-no-shinyappsio)
- [O pacote getLattes](#o-pacote-getlattes)
  - [Instalação](#instalação)
  - [Exemplo de uso](#exemplo-de-uso)
  - [Principais funções](#principais-funções)
- [Limitações](#limitações)
- [Créditos](#créditos)

---

## Sobre

A Plataforma Lattes, do CNPq, reúne os currículos de pesquisadores brasileiros e permite exportá-los em formato XML. Esse formato é completo, mas pouco prático para análise. O pacote `getLattes` lê esses arquivos e transforma o conteúdo em tabelas (*data frames*) do R, e este repositório oferece um aplicativo [Shiny](https://shiny.posit.co/) para usar o pacote pelo navegador.

Com ele é possível, por exemplo:

- reunir dados de vários pesquisadores em uma única planilha;
- levantar a produção bibliográfica, as orientações e as bancas de um grupo ou programa;
- preparar uma base para análises bibliométricas ou de formação acadêmica.

## O aplicativo

### Como usar

1. **Obtenha os currículos.** Na Plataforma Lattes, baixe o currículo desejado em formato XML. O arquivo vem compactado em `.zip` e não precisa ser descompactado.
2. **Envie os arquivos.** Na barra lateral, em *1. Currículos*, selecione um ou mais arquivos `.zip` (cada um deve conter o XML de um currículo).
3. **Escolha as informações.** Em *2. Informações*, abra os grupos e marque o que deseja extrair. Use *Marcar todas* ou *Limpar* para selecionar ou desmarcar tudo. Os **dados gerais** sempre são incluídos.
4. **Processe.** Clique em *Processar currículos* e aguarde.
5. **Confira os resultados.** Os indicadores no topo mostram quantos currículos, tabelas e linhas foram gerados. Na pré-visualização, escolha a tabela no seletor e use os filtros de cada coluna para buscar.
6. **Exporte.** Clique em *Baixar lattes.xlsx* para obter um arquivo Excel com uma aba para cada informação selecionada.

> Se você alterar a seleção ou enviar novos arquivos, clique novamente em *Processar currículos* para atualizar as tabelas e o download.

### Informações que podem ser extraídas

| Grupo | Informações |
|---|---|
| **Formação e atuação** | Graduação, mestrado, doutorado, áreas de atuação, atuações profissionais, endereço profissional, idiomas, linha de pesquisa |
| **Produção** | Artigos publicados, capítulos de livros, livros publicados, produção técnica, outras produções técnicas, projetos de pesquisa |
| **Orientações e bancas** | Orientações de doutorado, mestrado e pós-doutorado; bancas de doutorado, mestrado e graduação |
| **Eventos** | Eventos e congressos, organização de eventos |

Além disso, os **dados gerais** de cada pesquisador são sempre exportados, e o identificador de 16 dígitos do Lattes (`id`) permite cruzar as tabelas entre si.

### Executar localmente

Requisitos: R (≥ 4.1, pelo uso do operador `|>`) e os pacotes abaixo.

```r
install.packages(c(
  "shiny", "bslib", "shinyjs", "DT",
  "dplyr", "purrr", "xml2", "writexl",
  "getLattes"
))
```

Depois, na pasta do projeto:

```bash
Rscript -e 'shiny::runApp("app.R", port = 3838, launch.browser = TRUE)'
```

O app abrirá em `http://127.0.0.1:3838`. Para acessá-lo a partir de outra máquina da rede, acrescente `host = "0.0.0.0"` à chamada de `runApp()`.

No Debian/Ubuntu, a instalação de alguns pacotes pode exigir bibliotecas do sistema:

```bash
sudo apt install -y r-base r-base-dev build-essential \
  libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev \
  libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev \
  libtiff5-dev libjpeg-dev
```

### Publicar no shinyapps.io

Com o pacote `rsconnect` e a conta já autorizada (`rsconnect::setAccountInfo()`):

```r
rsconnect::deployApp(
  appDir = ".",
  appFiles = "app.R",
  appPrimaryDoc = "app.R",
  appName = "NOME-DO-APP",
  account = "SUA-CONTA",
  forceUpdate = TRUE
)
```

Use o mesmo `appName` de um app existente para atualizá-lo no mesmo endereço. Nunca publique tokens, *secrets* ou arquivos com currículos no repositório.

## O pacote getLattes

O [`getLattes`](https://cran.r-project.org/package=getLattes) importa os dados de currículos Lattes: lê os arquivos XML, converte-os em listas do R e organiza o conteúdo em tabelas. Há funções para áreas de atuação, artigos, livros, capítulos, bancas, orientações, projetos e outras seções.

- Documentação: <https://cran.r-project.org/web/packages/getLattes/refman/getLattes.html>
- Tutorial: <https://roneyfraga.com/getLattes/articles/introduction_getLattes.html>
- Código-fonte: <https://github.com/roneyfraga/getLattes>

### Instalação

Pelo CRAN:

```r
install.packages("getLattes")
```

Ou a versão de desenvolvimento, pelo GitHub:

```r
# install.packages("devtools")
devtools::install_github("roneyfraga/getLattes")
```

### Exemplo de uso

Um currículo, usando um arquivo de exemplo que acompanha o pacote:

```r
library(getLattes)
library(xml2)

zip_xml   <- system.file("extdata/4984859173592703.zip", package = "getLattes")
curriculo <- xml2::read_xml(zip_xml)

getDadosGerais(curriculo)
getArtigosPublicados(curriculo)
```

Vários currículos de uma vez, sem que um erro em um arquivo interrompa o processo:

```r
library(dplyr)
library(purrr)

zips <- c(
  system.file("extdata/4984859173592703.zip", package = "getLattes"),
  system.file("extdata/3051627641386529.zip", package = "getLattes")
)

curriculos <- purrr::map(zips, safely(read_xml)) |>
  purrr::map(pluck, "result")

artigos <- purrr::map(curriculos, safely(getArtigosPublicados)) |>
  purrr::map(pluck, "result") |>
  dplyr::bind_rows()

# número de artigos por pesquisador
artigos |>
  dplyr::group_by(id) |>
  dplyr::tally(name = "artigos")
```

O pacote também permite exportar publicações em formato RIS com `writePublicationsRis()`.

### Principais funções

O aplicativo usa as funções abaixo, todas do pacote `getLattes`:

| Função | Conteúdo |
|---|---|
| `getDadosGerais()` | Dados gerais do pesquisador |
| `getFormacaoGraduacao()`, `getFormacaoMestrado()`, `getFormacaoDoutorado()` | Formação acadêmica |
| `getAreasAtuacao()`, `getAtuacoesProfissionais()`, `getEnderecoProfissional()` | Atuação e endereço profissional |
| `getIdiomas()`, `getLinhaPesquisa()` | Idiomas e linhas de pesquisa |
| `getArtigosPublicados()`, `getCapitulosLivros()`, `getLivrosPublicados()` | Produção bibliográfica |
| `getProducaoTecnica()`, `getParticipacaoProjeto()` | Produção técnica e projetos |
| `getOrientacoesDoutorado()`, `getOrientacoesMestrado()`, `getOrientacoesPosDoutorado()` | Orientações |
| `getBancasDoutorado()`, `getBancasMestrado()`, `getBancasGraduacao()` | Bancas |
| `getEventosCongressos()`, `getOrganizacaoEventos()` | Eventos |

A lista completa e os argumentos de cada função estão na [documentação do pacote](https://cran.r-project.org/web/packages/getLattes/refman/getLattes.html).

## Limitações

- O **download dos currículos** na Plataforma Lattes exige a resolução de CAPTCHA, o que dificulta a coleta em massa. A documentação do pacote menciona ferramentas da comunidade para esse fim; este aplicativo trabalha apenas com os arquivos XML já baixados.
- O aplicativo processa os dados **durante a sessão** e não os armazena. Mesmo assim, evite enviar currículos que não possam ser tratados em um serviço externo.
- No plano gratuito do shinyapps.io, o app pode "dormir" quando fica sem uso, e o primeiro acesso demora alguns segundos. Muitos currículos de uma só vez podem esgotar a memória disponível.

## Créditos

- O pacote **getLattes** foi desenvolvido por Roney Fraga Souza, Winicius Sabino e Luis Felipe de Souza Rodrigues. Consulte o [repositório do pacote](https://github.com/roneyfraga/getLattes) para licença e forma de citação.
- Este aplicativo é uma interface para o pacote e não altera nem substitui o seu funcionamento.
- Lattes é uma plataforma do [CNPq](https://lattes.cnpq.br/).
