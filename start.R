library(sinartra)
library(evaluate)
start_sinartra <- function(app_path=getwd(),port=8181) {

	if (app_path!=getwd()){
		oldwd <- setwd(app_path)
		on.exit(setwd(oldwd))
	}

	if (!file.exists('routes.R')){
		stop(paste("routes.R does not exist in",app_path))
	}

	if (!file.exists('views')){
		stop(paste("views does not exist in",app_path))
		on.exit(setwd(oldwd))
	}

	options(help.ports=port)

	router_env = new.env(hash=TRUE,parent=globalenv())
	router_env$router_mtime <- 0
	router_env$router <- Router$clone()

	render_path <- function(path, query, ...){
		oldwd <- setwd(app_path)
		on.exit(setwd(oldwd))
		mtime = as.integer(file.info('routes.R')$mtime)
		if (mtime > router_env$router_mtime){
			sys.source('routes.R',router_env)
			router_env$router_mtime <- mtime
		}
		router_env$router$route(path, query)
	}

	assignInNamespace("httpd", render_path, "tools")
	if (tools:::httpdPort == 0L) {
		help.start()
			#options("help_type" = "html")
	}
	cat('port is ',tools:::httpdPort,'\n')

	return(invisible(router_env$router))
}
