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

###
# The real web services
###

router$get('/dataset/:dataset.json', function(dataset){
	if (!any(ls('package:datasets') == dataset))
		render_brew("error",list(dataset=dataset))

	render_json(get(dataset))
})

bad_plot <- function(dataset){
    t <- tempfile('png')
    png(t)
    #par(mar=rep(0,4))
    plot(rnorm(100),main=paste('Fail for',dataset,' but Check it!'),col=rainbow(100,alpha=runif(100,0,1)),pch='.',cex=c(2,3,4,5,10,50,100))
    dev.off()
	payday <- readBin(t,'raw',file.info(t)$size)
	unlink(t)
	payday
}

router$get('/dataset/:dataset.png', function(dataset){

	if (!any(ls('package:datasets') == dataset))
		render_brew("error",list(dataset=dataset))

	t <- tempfile('png')
	cat('tempfile is',t,'\n')
	png(t)
    suppressWarnings(
		eval(
			substitute(
				example(dataset,package='datasets',ask=FALSE),
				list(dataset=dataset)
			)
		)
	)
	dev.off()
	payday <- try(readBin(t,'raw',file.info(t)$size))
	unlink(t)
	if (inherits(payday,'try-error') || length(payday) <=1 ){
		payday <- bad_plot(dataset)
	}
	list(
		payload = payday,
		"content-type" = 'image/png',
		"headers" = c(),
		"status code" = 200
	)
})

