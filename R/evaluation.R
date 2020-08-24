#' Evaluation
#'
#' Bankruptcy fees evaluation function
#'
#' @export
#' @param input. Required.
evaluation <- function(inputformula, inputvalues) {
	install.packages("survey")
	library(survey)
	useformula <- as.formula(inputformula)
	usevalues <- as.data.frame(inputvalues)
	design31 <- svydesign(id = ~casenumber, weights = ~pweight, data = evaluationdata)
	model31 <- svyglm(lnfeeexpord ~ lnassets + prepack + preneg + lnsales + xyearfiled + shop, design = design31)
	newdata1 <- with(data, usevalues)
	pred <- predict(model31, newdata = newdata1, se.fit = T)
	efit <- pred[1]
	lfit <- efit - 1.44 * sqrt(attr(pred, "var")) / .97
	hfit <- efit + 1.44 * sqrt(attr(pred, "var")) / .97
	list(efit = efit, hfit = hfit, lfit = lfit)
}
