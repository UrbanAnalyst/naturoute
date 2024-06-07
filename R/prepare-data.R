#' Prepare data from an Open Street Map (OSM) file
#'
#' @param city A character string giving the name of the city or region for
#' which natural routes are to be calculated.
#' @param osm_file A character string giving the path to an OSM file. The
#' function will work most efficiently if this in in `.pbf` format.
#' @param osm_boundary_id The OSM identifier of a bounding polygon for the
#' region of interest.
#' @param results_dir Directory in which processed OSM extracts are to be saved.
#' @return A character vector of two elements containing full file paths to data
#' on the OSM network and natural spaces.
#' @export
nr_prepare_data <- function (city, osm_file, osm_boundary_id, results_dir) {

    fbdry <- get_bounary_polygon (osm_boundary_id)
    f <- extract_data_in_bdry (city, results_dir, fbdry)
}


get_bounary_polygon <- function (osm_id) {
    f <- file.path (results_dir, paste0 (city, "-boundary.osm"))
    if (!file.exists (f)) {
        cmd <- paste ("osmium getid -r -t ", path, paste0 ("r", osm_id), "-o", f)
        system (cmd)
    }
    return (f)
}

extract_data_in_bdry <- function (city, results_dir, fbdry) {
    f <- file.path (results_dir, paste0 (city, ".osm.pbf"))
    if (!file.exists (f)) {
        cmd <- paste ("osmium extract -p", ftmp, path, "-o", f)
        system (cmd)
    }
    return (f)
}
