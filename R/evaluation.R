#' Evaluation
#'
#' Bankruptcy fees evaluation function
#'
#' @export
#' @param input. Required.
evaluation <- function(inputformula, inputvalues) {
	library(survey)
	useformula <- as.formula(inputformula)
	usevalues <- as.data.frame(inputvalues)
	design34 <- svydesign(id = ~casenumber, weights = ~pweight, data = evaluationdata)
	model34 <- svyglm(lnfeeexpord ~ lnroles + lnassets + lndaysin + yearconfirmed + lnemployees + shop + saleall, design = design34)
	newdata1 <- with(evaluationdata, usevalues)
	pred <- predict(model34, newdata = newdata1, se.fit = T)
	efit <- pred[1]
	lfit <- efit - 1.44 * sqrt(attr(pred, "var")) / .97
	hfit <- efit + 1.44 * sqrt(attr(pred, "var")) / .97
	list(efit = efit, hfit = hfit, lfit = lfit)
}
