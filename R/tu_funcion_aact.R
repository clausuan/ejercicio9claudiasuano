#' Comparar tipos de intervencion en ensayos clinicos segun patologia
#'
#' Consulta la base de datos remota AACT combinando las tablas \code{ctgov.studies},
#' \code{ctgov.interventions} y \code{ctgov.conditions} mediante SQL para contar
#' el numero de ensayos clinicos segun el tipo de intervencion (\code{intervention_type})
#' en una patologia especifica.
#'
#' @param con Objeto de conexion a la base de datos AACT (creado con \code{DBI::dbConnect}).
#' @param patologia Cadena de texto con el nombre (o parte del nombre) de la enfermedad
#'   en ingles tal como figura en ClinicalTrials.gov (por defecto, \code{"Breast Cancer"}).
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
    stop("La conexion a la base de datos no es valida o esta cerrada.")
  }
  if (!is.character(patologia) || length(patologia) != 1 || nchar(trimws(patologia)) == 0) {
    stop("El parametro 'patologia' debe ser una cadena de texto no vacia.")
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
