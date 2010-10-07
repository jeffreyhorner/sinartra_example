###########################################
# Load any libraries you need here
###########################################
library(datasets)
library(RJSONIO)





###########################################
# Top level HTML pages
###########################################

router$get("/", function(...) {
	render_brew("index",list(...))
})

router$get("/*.html", function(splat,query) {
	render_brew(splat,list(query=query))
})

router$get("/public/*", function(splat, ...) {
static_file(file.path(path, "public", splat))
})

router$get("/dataset/:dataset/", function(dataset) {
	if (any(ls('package:datasets') == dataset))
		render_brew("dataset",list(dataset=dataset))
	else
		render_brew("error",list(dataset=dataset))
})

###
# The real web services
###

# Missing/broken part of sinartra
render_json <- function(object) {

	# Some datasets are instances of a sub-class of data.frame
	# and RJSONIO doesn't know what to do with them, so we just
	# use trickery.
	if (inherits(object,'data.frame',which=TRUE) > 0){

		class(object) <- 'data.frame'

		# Even these may have ts objects as columns so lets 
		# just punt for right now.
		for (i in names(object)){
			if (inherits(object[[i]],'ts')){
				object[[i]] <- NA
			}
		}
	}

	# Unclassing here is unclassy. Would be nice to use as.data.frame
	# but it seems that the original ts object is stuffed into the result
	# somehow.
	if (inherits(object,'ts')){
		object <- unclass(object)
	}

	if (inherits(object,'table') || inherits(object,'array')){
		object <- as.data.frame(object)
	}

	json <- toJSON(object)

	sinartra:::render(json, mime_type = "application/json")
}

router$get('/dataset/:dataset.json', function(dataset){
	if (any(ls('package:datasets') == dataset))
		render_json(get(dataset))
})

