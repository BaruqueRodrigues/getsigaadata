#' constroi calendario das disciplinas
#'
#' @param dataset_horario dataset que contem as disciplinas construido pela funcao pega_info
#'
#' @return
#' @export
#'
#' @examples
constroi_calendario <- function(dataset_horario){

  calendario <- dplyr::tibble(
    "horario" = c(
      "08:00", "09:00", "10:00",
      "11:00", "12:00", "13:00",
      "13:00", "14:00", "15:00",
      "16:00", "17:00", "18:00",
      "19:00", "20:00", "21:00",
      "22:00"),

    "segunda" = c(
      "", "", "",
      "", "", "",
      "", "", "",
      "", "", "",
      "", "", "",
      ""),
    "terça" = c(
      "", "", "",
      "", "", "",
      "", "", "",
      "", "", "",
      "", "", "",
      ""),
    "quarta" = c(
      "", "", "",
      "", "", "",
      "", "", "",
      "", "", "",
      "", "", "",
      ""),
    "quinta" = c(
      "", "", "",
      "", "", "",
      "", "", "",
      "", "", "",
      "", "", "",
      ""),
    "sexta" = c(
      "", "", "",
      "", "", "",
      "", "", "",
      "", "", "",
      "", "", "",
      ""),
    "sabado" = c(
      "", "", "",
      "", "", "",
      "", "", "",
      "", "", "",
      "", "", "",
      ""),
  )

  calendario <- calendario %>%
    tidyr::pivot_longer(
      c(2:7),
      names_to = "dias_semana"
    ) %>%
    dplyr::left_join(
      dataset_horario %>%
        dplyr::select(horario_aula , dias_semana,
                      titulo, disciplina, inicio_ou_fim
        ),
      by = c("horario"= "horario_aula",
             "dias_semana" = "dias_semana")
    ) %>%
    dplyr::mutate(
      dsc_aula = str_c(titulo, disciplina, inicio_ou_fim,
                       sep = " | "),

    ) %>%
    dplyr::select(
      -c(value:inicio_ou_fim)  ) %>%
    tidyr::pivot_wider(
      names_from = dias_semana,
      values_from = dsc_aula
    ) %>%
    dplyr::mutate_all(as.character)

  calendario
}
