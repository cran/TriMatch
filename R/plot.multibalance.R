utils::globalVariables(c('value','covariate','variable','model','group'))

#' Multiple covariate balance assessment plot.
#' 
#' A graphic based upon [cv.bal.psa()] function in the `PSAgraphics`
#' package. This graphic plots the effect sizes for multiple covariates before and
#' after propensity score adjustment.
#' 
#' @param tpsa results of [trips()].
#' @param tmatch results of [trimatch()].
#' @param grid if TRUE, then a grid of three plots for each model will be displayed.
#' @param cols character vector of covariates (i.e. column names) from the original 
#'        data to include in the plot. By default all covariates used in the
#'        logistic regression model are used.
#' @return a \code{ggplot2} figure.
#' @export
multibalance.plot <- function(tpsa, tmatch, grid=TRUE, cols) {
	if(!missing(tmatch)) {
		tpsa <- attr(results, 'triangle.psa', exact=TRUE)
	}
	
	covs <- attr(tpsa, 'data')
	m1 <- attr(tpsa, 'model1')
	if(missing(cols)) {
		cols <- attr(m1$terms, 'term.labels')
	}
	covs <- covs[,cols]
	
	#Recode factors. First we'll covert logicals and factors with two levels to integers
	for(i in 1:ncol(covs)) {
		if(is.logical(covs[,i])) {
			covs[,i] <- as.integer(covs[,i])
		} else if(is.factor(covs[,i]) & length(levels(covs[,i])) == 2) {
			covs[,i] <- as.integer(covs[,i])
		}
	}
	if('factor' %in% sapply(covs, class)) {
		#Convert remaining factors using cv.trans.psa from PSAgraphics
		#covs <- as.data.frame(cv.trans.psa(covs))
		covs <- cv.trans.psa(covs)[[1]]
		#names(covs) <- gsub('covariates.transformed.', '', names(covs))
	}
	
	tpsa2 <- cbind(tpsa, (covs))
	
	if(!missing(tmatch)) {
		rows <- c(tmatch$Control, tmatch$Treat1, tmatch$Treat2)
		tpsa2 <- tpsa2[rows,]
	} else {
		warning('Balance estimates may include observations not in the matched dataset. It is 
				recommended that the tmatch parameter is specified instead.')
	}
	
	results <- data.frame(covariate=character(), model=integer(), unadjusted=numeric(),
						  adjusted=numeric(), stringsAsFactors=FALSE)
	for(i in 1:3) {
		m <- tpsa2[!is.na(tpsa2[,paste('model', i, sep='')]),]
		
		bal <- covariateBalance(m[,names(m) %in% names(covs)], 
						  m[,paste('model', i, sep='')], 
						  m[,paste('ps', i, sep='')],
						  m[,paste('strata', i, sep='')])
		results <- rbind(results, data.frame(
			covariate = row.names(bal$effect.sizes),
			model = rep(i, ncol(covs)),
			unadjusted = bal$effect.sizes[,'stES_unadj'],
			adjusted = bal$effect.sizes[,'stES_adj'],
			stringsAsFactors = FALSE
		))
	}
	
	row.names(results) <- 1:nrow(results)
	results <- melt(results, id.vars=c('covariate','model'))
	results$group <- paste(results$variable, results$model, sep='-')
	
	results <- results[rev(order(results$model, results$covariate)),]
	
	results$covariate <- factor(results$covariate, ordered=TRUE)
	
	p <- ggplot(results, 
		aes(x=value, y=covariate, color=variable, shape=factor(model), linetype=factor(model))) + 
		geom_point() + geom_path(alpha=.5, aes(group=group)) +
		ylab('Covariate') + xlab('Effect Size') +
		scale_color_hue('Adjustment') + scale_linetype('Model') + scale_shape('Model')
	if(grid) {
		p <- p + facet_grid(~ model)
	}
	
	return(p)
}
