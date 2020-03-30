requireNamespace("glue")
requireNamespace("purrr")
requireNamespace("gh")
requireNamespace("magrittr")
requireNamespace("tibble")
requireNamespace("dplyr")

OWNER = "mlr-org"
REPO = "mlr3orga"
FILE = "labels.json"

retrieve_labels = function(file = NULL) {
  if (is.null(file)) {
    labels = gh::gh("/repos/:owner/:repo/labels", owner = OWNER, repo = REPO)
    if (is.list(labels))
      labels = purrr::map_dfr(labels, magrittr::extract, c("name", "color"))
    else
      labels = tibble::tibble(name = character(), color = character())
  } else {
    labels = purrr::map_dfr(jsonlite::read_json(file), identity)
    labels$color = sub("^#", "", labels$color)
  }
  labels
}

remove_label = function(name, ...) {
  message(glue::glue("Deleting label '{name}'"))
  gh::gh("DELETE /repos/:owner/:repo/labels/:name", owner = OWNER, repo = REPO, name = name)
}

add_label = function(name, color, ...) {
  message(glue::glue("Creating label '{name}' with color '{color}'"))
  gh::gh("POST /repos/:owner/:repo/labels", owner = OWNER, repo = REPO,
    name = name, color = sub("^#", "", color), ...)
}

patch_label = function(name, color, ...) {
  message(glue::glue("Patching label '{name}'"))
  gh::gh("PATCH /repos/:owner/:repo/labels/:current_name", owner = OWNER, repo = REPO,
    current_name = name, color = sub("^#", "", color), ...)
}


remote_labels = retrieve_labels()
local_labels = retrieve_labels(FILE)

### create missing labels
new_labels = dplyr::anti_join(local_labels, remote_labels, by = "name")
purrr::pwalk(new_labels, add_label)

### patch existing labels
patch_labels = dplyr::inner_join(remote_labels, local_labels, by = "name", suffix = c(".x", ""))
patch_labels = dplyr::select(dplyr::filter(patch_labels, color.x != color), c("name", "color"))
purrr::pwalk(patch_labels, patch_label)

### remove extra labels
extra_labels = dplyr::anti_join(remote_labels, local_labels, by = "name")
purrr::pwalk(extra_labels, remove_label)
