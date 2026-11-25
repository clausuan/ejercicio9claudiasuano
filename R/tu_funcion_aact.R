#' Comparar tipos de intervención en ensayos clínicos según patología
#'
#' Consulta la base de datos remota AACT combinando las tablas \code{ctgov.studies},
#' \code{ctgov.interventions} y \code{ctgov.conditions} mediante SQL para contar
#' el número de ensayos clínicos según el tipo de intervención (\code{intervention_type})
#' en una patología específica.
#'
#' @param con Objeto de conexión a la base de datos AACT (creado con \code{DBI::dbConnect}).
#' @param patologia Cadena de texto con el nombre (o parte del nombre) de la enfermedad
#'   en inglés tal como figura en ClinicalTrials.gov (por defecto, \code{"Breast Cancer"}).
#'
#' @return Un \code{data.frame} con las columnas \code{tipo_intervencion} y \code{n_estudios},
#'   ordenado de mayor a menor frecuencia.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- conectar_aact()
#' res <- intervenciones_por_patologia(con, patologia = "Diabetes")
#' print(res)
#' DBI::dbDisconnect(con)
#' }
intervenciones_por_patologia <- function(con, patologia = "Breast Cancer") {
  if (!DBI::dbIsValid(con)) {
    stop("La conexión a la base de datos no es válida o está cerrada.")
  }
  if (!is.character(patologia) || length(patologia) != 1 || nchar(trimws(patologia)) == 0) {
    stop("El parámetro 'patologia' debe ser una cadena de texto no vacía.")
  }

  patron <- paste0("%", patologia, "%")
  patron_sql <- DBI::dbQuoteString(con, patron)

  query <- sprintf(
    "SELECT
       i.intervention_type AS tipo_intervencion,
       COUNT(DISTINCT s.nct_id) AS n_estudios
     FROM ctgov.studies s
     INNER JOIN ctgov.interventions i
       ON s.nct_id = i.nct_id
     INNER JOIN ctgov.conditions c
       ON s.nct_id = c.nct_id
     WHERE c.name ILIKE %s
       AND i.intervention_type IS NOT NULL
     GROUP BY i.intervention_type
     ORDER BY n_estudios DESC;",
    patron_sql
  )

  resultado <- DBI::dbGetQuery(con, query)
  return(resultado)
}
