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
nr_prepare_data <- function (city, osm_file, osm_boundary_id, results_dir = ".") {

    results_dir <- normalizePath (results_dir)

    fbdry <- get_bounary_polygon (osm_file, osm_boundary_id, results_dir, city)
    f <- extract_data_in_bdry (city, osm_file, results_dir, fbdry)
    f_network <- extract_network_data (city, results_dir)
    f_natural <- extract_natural_data (city, results_dir)

    c (network = f_network, natural = f_natural)
}


get_bounary_polygon <- function (osm_file, osm_id, results_dir, city) {
    fout <- file.path (results_dir, paste0 (city, "-boundary.osm"))
    if (!file.exists (fout)) {
        cmd <- paste ("osmium getid -r -t ", osm_file, paste0 ("r", osm_id), "-o", fout)
        system (cmd)
    }
    return (fout)
}

extract_data_in_bdry <- function (city, osm_file, results_dir, fbdry) {
    f <- file.path (results_dir, paste0 (city, ".osm.pbf"))
    if (!file.exists (f)) {
        cmd <- paste ("osmium extract -p", fbdry, osm_file, "-o", f)
        system (cmd)
    }
    return (f)
}

extract_network_data <- function (city, results_dir) {
    fin <- file.path (results_dir, paste0 (city, ".osm.pbf"))
    if (!file.exists (fin)) {
        stop ("Input file [", fin, "] not found", call. = FALSE)
    }
    fout <- file.path (results_dir, paste0 (city, "-network.osm"))

    if (!file.exists (fout)) {
        tags <- c (
            "highway", "restriction", "access", "foot", "motorcar",
            "motor_vehicle", "vehicle", "toll", "bicycle",
            "cycleway", "cycleway:left", "cycleway:right"
        )
        tags <- paste0 (paste0 ("wr/", tags), collapse = " ")
        cmd <- paste ("osmium tags-filter", fin, tags, "-o", fout)
        system (cmd)
    }

    return (fout)
}

extract_natural_data <- function (city, results_dir) {
    fin <- file.path (results_dir, paste0 (city, ".osm.pbf"))
    if (!file.exists (fin)) {
        stop ("Input file [", fin, "] not found", call. = FALSE)
    }
    fout <- file.path (results_dir, paste0 (city, "-natural.osm"))

    if (!file.exists (fout)) {
        tags <- c (
            "w/leisure=garden,park,nature_reserve,playground",
            "w/surface=grass",
            "w/landuse=forest,meadow,recreation_ground,village_green",
            "wr/natural"
        )
        fout <- paste0 (city, "-natural.osm")
        cmd <- paste ("osmium tags-filter", fin, paste0 (tags, collapse = " "), "-o", fout)
        system (cmd)
    }

    return (fout)
}
