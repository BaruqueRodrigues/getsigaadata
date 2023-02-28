#' pega disciplinas do sigaa
#'
#' @param area da graduacao,
#' @param campus campus em que deve buscar as disciplinas
#'
#' @return
#' @export
#'
#' @examples
pega_discp <- function(area = 3,
                       campus){


  pega_info <- function(area = 3, campus =3){

    rs_driver_object <- RSelenium::rsDriver(browser = "firefox",
                                            chromever = NULL,
                                            port = netstat::free_port(),
                                            verbose = FALSE
    )
    remDr <- rs_driver_object$client


    remDr$navigate("https://sigaa.unb.br/sigaa/public/turmas/listar.jsf")

    #Seleciona o campus
    remDr$findElement(using = "xpath", value = '//*[@id="formTurma:inputNivel"]')$clickElement()
    Sys.sleep(.5)
    #seleciona a graduação
    remDr$findElement(using = "xpath", value = paste0('//*[@id="formTurma:inputNivel"]/option[',
                                                      area,
                                                      "]",
                                                      sep = "")
    )$clickElement()
    Sys.sleep(.5)
    #Seleciona a unidade
    remDr$findElement(using = "xpath", value = '//*[@id="formTurma:inputDepto"]')$clickElement()
    Sys.sleep(.5)
    #seleciona o campus
    remDr$findElement(using = "xpath", value = paste0('//*[@id="formTurma:inputDepto"]/option[',
                                                      campus,
                                                      "]",
                                                      sep = "")
    )$clickElement()
    Sys.sleep(.5)
    #manda o formulario
    remDr$findElement(using = "xpath", value = '//*[@id="formTurma"]/table/tfoot/tr/td/input[1]')$clickElement()


    horario <- remDr$findElements(using = "xpath", '//*[@id="turmasAbertas"]/table/tbody')


    horarios <- purrr::map(horario, ~.x$getElementText()) %>% unlist()

    dataset_horario <- horarios %>%
      #tranforma a lita em tibble
      dplyr::tibble(x=.) %>%
      # quebra as disciplinas em linhas
      tidyr::separate_rows(x,sep =  "CDS....") %>%
      #remove a primeira
      dplyr::slice(-1) %>%
      #crinado as colunas com disciplinas
      tidyr::separate(x, sep = "\n0.",
                      into = c("titulo", "detalhes_2",
                               "detalhes_3", "detalhes_4",
                               "detalhes_5", "detalhes_6",
                               "detalhes_7", "detalhes_8"),
                      remove = FALSE) %>%
      #empilhando as disciplinas
      tidyr::pivot_longer(
        c(contains("detalhes")),
        names_to = "disciplina",
        values_to = "descricao"
      ) %>%
      #removendo disciplinas vazias
      tidyr::drop_na(descricao) %>%
      #removendo colunas
      dplyr::select(-c(x, disciplina)) %>%

      #pegando o horário das disciplinas
      dplyr::mutate(horario = stringr::str_extract(descricao, "\\d{1,2}[a-zA-Z]+\\d{2}")) %>%
      tidyr::separate(
        descricao,
        sep = "\n\n",
        into =  c("disciplina", "vagas")

      ) %>%
      #pegando as informações de vagas e local
      dplyr::mutate(vagas = stringr::str_remove(vagas, "\\n")) %>%
      tidyr::separate(
        vagas,
        sep = " ",
        into = c("vagas disponíveis", "vagas preenchidas", "local")

      ) %>%
      #Extraindo os dias da semana
      dplyr::mutate(dias_semana = stringr::str_extract(horario, "^\\d{1,2}"),
                    horario_aula = stringr::str_extract(horario, "\\d{2}$"),
                    periodo_aula = stringr::str_extract(horario, "[a-zA-Z]")
      ) %>%
      tidyr::separate_longer_position(c(dias_semana,horario_aula), width = 1) %>%
      dplyr::mutate(dias_semana = dplyr::recode(dias_semana,
                                                "2" = "segunda", "3" = "terça",
                                                "4" = "quarta", "5" = "quinta", "6" = "sexta",
                                                "1" = "domingo", "7" = "sabado"),
                    horario_aula = dplyr::case_when(periodo_aula == "M" & horario_aula == 1 ~ "08:00",
                                                    periodo_aula == "M" & horario_aula == 2 ~ "09:00",
                                                    periodo_aula == "M" & horario_aula == 3 ~ "10:00",
                                                    periodo_aula == "M" & horario_aula == 4 ~ "11:00",
                                                    periodo_aula == "M" & horario_aula == 5 ~ "12:00",
                                                    periodo_aula == "M" & horario_aula == 1 ~ "13:00",
                                                    periodo_aula == "T" & horario_aula == 1 ~ "13:00",
                                                    periodo_aula == "T" & horario_aula == 2 ~ "14:00",
                                                    periodo_aula == "T" & horario_aula == 3 ~ "15:00",
                                                    periodo_aula == "T" & horario_aula == 4 ~ "16:00",
                                                    periodo_aula == "T" & horario_aula == 5 ~ "17:00",
                                                    periodo_aula == "T" & horario_aula == 6 ~ "18:00",
                                                    periodo_aula == "T" & horario_aula == 7 ~ "19:00",
                                                    periodo_aula == "N" & horario_aula == 1 ~ "19:00",
                                                    periodo_aula == "N" & horario_aula == 2 ~ "20:00",
                                                    periodo_aula == "N" & horario_aula == 3 ~ "21:00",
                                                    periodo_aula == "N" & horario_aula == 4 ~ "22:00"
                    ),
                    inicio_ou_fim = rep(c("inicio", "fim"), nrow(.), times = 2)
      )
    remDr$quit()
    dataset_horario
  }


  dataset <- purrr::map2_dfr(.x = area,
                              .y = campus,
                              ~pega_info(area =.x,
                                         campus = .y))

  dataset
}



