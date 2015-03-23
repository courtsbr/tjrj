#' Baixa a porra toda
#' 
#' @export
pesquisa_codigos <- function(codigos, path) {
  link <- 'http://www4.tjrj.jus.br/ConsultaUnificada/consulta.do'
  r <- httr::GET(link)
  h <- rvest::html(r, encoding = 'UTF-8')
  tabelas <- rvest::html_nodes(h, 'select')
  coma_txt <- rvest::html_text(rvest::html_nodes(tabelas[[9]], 'option'))[-1]
  coma_val <- rvest::html_attr(rvest::html_nodes(tabelas[[9]], 'option'), 'value')[-1]
  comp_txt <- rvest::html_text(rvest::html_nodes(tabelas[[10]], 'option'))[-1]
  comp_val <- rvest::html_attr(rvest::html_nodes(tabelas[[10]], 'option'), 'value')[-1]
  inst_txt <- rvest::html_text(rvest::html_nodes(tabelas[[8]], 'option'))[-1]
  inst_val <- rvest::html_attr(rvest::html_nodes(tabelas[[8]], 'option'), 'value')[-1]
  comp_d <- dplyr::data_frame(comp_txt = comp_txt, comp_val = comp_val)
  coma_d <- dplyr::data_frame(coma_txt = coma_txt, coma_val = coma_val)
  
  expnd <- data.frame(expand.grid(comp_val, coma_val, codigos))
  expnd <- dplyr::mutate_each(expnd, funs = dplyr::funs(as.character(.)))
  names(expnd) <- c('comp_val', 'coma_val', 'codigo')
  expnd <- dplyr::inner_join(expnd, comp_d, 'comp_val')
  expnd <- dplyr::inner_join(expnd, coma_d, 'coma_val')
  expnd$inst_txt <- inst_txt[1]
  expnd$inst_val <- inst_val[1]
  
  d <- dplyr::distinct(expnd)
  d <- dplyr::group_by(d, 
                       inst_txt, 
                       coma_txt, 
                       comp_txt, 
                       inst_val, 
                       coma_val,
                       comp_val, 
                       codigo)
  
  d <- dplyr::do(d, pesquisa_codigo_um(., p = path))
  d <- dplyr::ungroup(d)
  d$a <- sprintf(
    '%s/%s_%s_%s_%s.html', path, d$codigo, d$inst_val, d$coma_val, d$comp_val
  )
  d
}

#' @export
dados_list <- function() {
  dados <- list(
    'acao' = 'consulta',
    'tipoConsultaHidden' = '',
    'descOrigem' = '',
    'descComarca' = '',
    'descCompetencia' = '',
    'novaTela' = 'true',
    'tipousuario' = '',
    'origem' = '1',
    'tiposecinst' = '1',
    'comarca' = '201',
    'competencia' = '01',
    'anoInicio' = '1900',
    'anoFinal' = '2015',
    'numeroCpfCnpj' = ''
  )
  dados
}

#' @export
pesquisa_codigo_um <- function(d, p) {
  link <- 'http://www4.tjrj.jus.br/consultaProcessoNome/consultaCPF.do'
  a <- sprintf(
    '%s/%s_%s_%s_%s.html', p, d$codigo, d$inst_val, d$coma_val, d$comp_val
  )
  if(!file.exists(a)) {
    dados <- dados_list()
    dados$origem <- d$inst_val
    dados$comarca <- d$coma_val
    dados$competencia <- d$comp_val
    dados$numeroCpfCnpj <- d$codigo
    r <- httr::POST(link, body = dados, encode = 'form')
    cat(httr::content(r, 'text'), file = a)
  }
  if(file.info(a)$size <= 15640) file.remove(a)
  d
}

rm_accent <- function (x) gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))

#' @export
parse_file <- function(f) {
  try({
    h <- rvest::html(f, encoding = 'UTF-8')
    tab <- rvest::html_node(h, xpath = '//form[@name="consultaNomeForm"]//table[3]')
    if(is.null(tab)) {
      file.remove(f)
    } else {
      tab <- rvest::html_table(tab)
      tab <- dplyr::mutate(tab, X1 = gsub('( ?\r\n )+', '', gsub(' +', ' ', X1)))
      tab <- dplyr::filter(tab, stringr::str_detect(X1, '[A-Za-z0-9]'))
      tab <- tidyr::separate(tab, X1, c('key', 'val'), extra = 'merge', sep = ':')
      # tab <- dplyr::filter(tab, !is.na(val))
      # print(tab)
      # tab <- dplyr::mutate(tab, val = ifelse(is.na(val), key, val),
      #                      key = ifelse(stringr::str_length(key) > 20, 'processo', key))
      # tab$key <- rm_accent(tolower(tab$key))
      # d <- tidyr::spread(tab, key, val)
      return(tab)
    }
  })
  return(data.frame())
}

#' @export
parse_files <- function(path) {
  l <- list.files(path, full.names = TRUE)
  d <- dplyr::data_frame(l = l)
  d <- dplyr::group_by(d, l)
  d <- dplyr::do(d, parse_file(.$l))
  d <- dplyr::ungroup(d)
  d
}


