#' A function determine model type
#'
#' This function allows you to find what kind a model is.
#' @param model The model passed to the function
#' @keywords Model
#' @return The type of the object passed into this function. Defaults to Linear Regression and Logistic Regression
#' @export
#' @examples
#' model <- lm(formula = Sepal.Width ~ Petal.Length, data = iris)
#' DetermineModelType(model)

DetermineModelType <-function(model){
  if(stringr::str_detect(string = as.character(model$call)[1], pattern = "^lm")){
    return("Linear Regression")
  }else if(stringr::str_detect(string = as.character(model$call)[1], pattern = "^glm")){
    return("Logistic Regression")
  }else if(stringr::str_detect(string = as.character(model$call)[1], pattern = "cox")){
    return("test")
  }
}



#' A function to summary linear model regression set table
#'
#' This function allows you to generate linear regression table
#' @param model linear model passed to the function
#' @param pType Linear regression table type, defaults to "withPValue"
#' @param estimateType Linear regression table type, defaults to "betase"
#' @param pType whether put P into final table, default to withPValue, another selection is withMark
#' @param digits Digit number of the estimate
#' @param pDigits Digit number of the P value
#' @param ... other parameters to pass into tidy function from broom
#' @keywords Model
#' @return regression table with reference level added
#' @export
#' @examples
#' model <- lm(formula = Sepal.Width ~ Petal.Length, data = iris)
#' clean.lm(model)
clean.lm <- function(model, pType = "withPValue", estimateType = "betase", digits = 2, pDigits = 4, ...){
####
# parameter check part  ----------------------------------------------------------------------------------
####

  ModelIsNull <- is.na(model)|is.null(model)
  pTypeIsNull <- is.na(pType)|is.null(pType)
  estimateTypeIsNull <- is.na(estimateType)|is.null(estimateType)
  digitsIsNull <- is.na(digits)|is.null(digits)
  pDigitsIsNull <- is.na(pDigits)|is.null(pDigits)
  if(any(ModelIsNull, pTypeIsNull, estimateTypeIsNull, digitsIsNull, pDigitsIsNull)){
    stop()
    print("Error, one of the paramater is empty and is without default value. Please check the parameters")
  }
  pTypeAcceptable <- pType %in% c("withPValue", "withMark")
  if(!pTypeAcceptable){
    pType <- "withPValue"
    print("WARNING, no P Type selected, set default to withPValue")
  }
  estimateTypeAcceptable  <- estimateType  %in% c("betase", "betaci")
  if(!estimateTypeAcceptable){
    estimateType <- "betase"
    print("WARNING, no estimate Type selected, set default to betase")
  }
  estimate <- std.error <- conf.low <- conf.high <- p.value <- NULL # This line is to pass note check in R package buliding


  ## generate possible result
  regressionTableBeforeClean <- broom::tidy(x = model, conf.int = T, ...)
  regressionTableAfterClean <- regressionTableBeforeClean %>%
    dplyr::mutate(
      betase = paste0(
        sprintf(paste0("%.", digits, "f"), estimate),
        " (",
        sprintf(paste0("%.", digits, "f"), std.error),
        ")"
      ),
      betaci = paste0(
        sprintf(paste0("%.", digits, "f"), estimate),
        " (",
        sprintf(paste0("%.", digits, "f"), conf.low),
        ", ",
        sprintf(paste0("%.", digits, "f"), conf.high),
        ")"
      ),
      pValue = sprintf(paste0("%.", pDigits, "f"), p.value),
      betaseWithMark = dplyr::case_when(
        pValue < 0.05 & pValue >= 0.01 ~ paste0(betase, "*"),
        pValue < 0.01 & pValue >= 0.001 ~ paste0(betase, "**"),
        pValue < 0.001 ~ paste0(betase, "***"),
        TRUE ~ NA_character_
      ),
      betaciWithMark = dplyr::case_when(
        pValue < 0.05 & pValue >= 0.01 ~ paste0(betase, "*"),
        pValue < 0.01 & pValue >= 0.001 ~ paste0(betase, "**"),
        pValue < 0.001 ~ paste0(betase, "***"),
        TRUE ~ NA_character_
      )
    )

  ## output selection
  if(estimateType == "betase"){
    tablePart1 = regressionTableAfterClean[,"betase"]
    return(tablePart1)
  }else if(estimateType == "betaci"){
    tablePart1 = regressionTableAfterClean[,"betaci"]
    return(tablePart1)
  }

  if(pType == "withPValue" & estimateType == "betase"){
    tablePart2 = regressionTableAfterClean[,"pvalue"]
    tableFinal <- cbind(tablePart1, tablePart2)
    names(tableFinal) <- c("beta (se)", "P")
  }else if(pType == "withPValue" & estimateType == "betaci"){
    tablePart2 = regressionTableAfterClean[,"pvalue"]
    tableFinal <- cbind(tablePart1, tablePart2)
    names(tableFinal) <- c("beta (95%CI)", "P")
  }else if(pType == "withMark" & estimateType == "betase"){
    tablePart1 <- regressionTableAfterClean[,"betaseWithMark"]
    tableFinal <- tablePart1
    names(tableFinal) <- c("beta (se)")
  }else if(pType == "withMark" & estimateType == "betaci"){
    tablePart1 <- regressionTableAfterClean[,"betaciWithMark"]
    tableFinal <- tablePart1
    names(tableFinal) <- c("beta (95%CI)")
  }
  return(tableFinal)



}




#' A function to summary linear model regression set table
#'
#' This function allows you to generate linear regression table
#' @param outcome Linear model set outcome. It could be a character vector or a list
#' @param target variable of interest. For now the code only supports one target variable of interest
#' @param covariate control variables in your model. It should be a list of vectors
#' @param pType Linear regression table type, defaults to "withPValue"
#' @param estimateType Linear regression table type, defaults to "betase"
#' @param pType whether put P into final table, default to withPValue, another selection is withMark
#' @param digits Digit number of the estimate
#' @param pDigits Digit number of the P value
#' @param strata Stratification variable
#' @param ... other parameters to pass into tidy function from broom
#' @keywords Model
#' @return regression table with reference level added
#' @export
#' @examples
#' model <- lm(formula = Sepal.Width ~ Petal.Length, data = iris)
#' clean.lm(model)
regressionset.automatical.lm <- function(data, outcome, target, covariate, pType = "withPValue", estimateType = "betase", digits = 2, pDigits = 4, strata = NULL, ...){
  ####
  # temp function #----------------------------------------------------------------------------------
  ####
  regressionWhenTargetIsFactor <- function(formula){
    lengthOfFactorTarget <- length(table(data[[target]]))
    model <- lm(formula = formula, data = data.complete)
    clean.lm(model = model) %>%
      dplyr::slice(2:lengthOfFactorTarget)
  }
  regressionWhenTargetIsNumeric <- function(formula){
    lengthOfNumericTarget <- 2
    model <- lm(formula = formula, data = data.complete)
    clean.lm(model = model) %>%
      dplyr::slice(2)
  }
  ####
  # parameter check part #----------------------------------------------------------------------------------
  ####
  dataIsNull <- is.na(data)|is.null(data)
  outcomeIsNull <- is.na(outcome)|is.null(outcome)
  targetIsNull <- is.na(target)|is.null(target)
  covariateIsNull <- is.na(covariate)|is.null(covariate)
  pTypeIsNull <- is.na(pType)|is.null(pType)
  estimateTypeIsNull <- is.na(estimateType)|is.null(estimateType)
  digitsIsNull <- is.na(digits)|is.null(digits)
  pDigitsIsNull <- is.na(pDigits)|is.null(pDigits)
  if(any(outcomeIsNull, targetIsNull, covariateIsNull, pTypeIsNull, estimateTypeIsNull, digitsIsNull, pDigitsIsNull)){
    stop()
    print("Error, one of the paramater is empty or without default value. Please check the parameters")
  }
  pTypeAcceptable <- pType %in% c("withPValue", "withMark")
  if(!pTypeAcceptable){
    pType <- "withPValue"
    print("WARNING, no P Type selected, set default to withPValue")
  }
  estimateTypeAcceptable  <- estimateType  %in% c("betase", "betaci")
  if(!estimateTypeAcceptable){
    estimateType <- "betase"
    print("WARNING, no estimate Type selected, set default to betase")
  }
  estimate <- std.error <- conf.low <- conf.high <- p.value <- NULL


# data completeness check part ---------------------------------------------------------------------------------------
  dataInThisAnalysis <- data[,c(outcome, target, covariate)]
  data.complete <- dataInThisAnalysis[complete.cases(dataInThisAnalysis),]
  DataNotComplete <- nrow(dataInThisAnalysis) != nrow(data.complete)
  if(DataNotComplete){
    print("WARNING, data with variables selected is not complete, automatically select rows with variables not missing!")
  }

  formulaGenerated <- addCovariateSetBySet(outcome = outcome, target = target, covariate = covariate)



  ## generate possible result
  regressionTableBeforeClean <- broom::tidy(x = model, conf.int = T, ...)
  regressionTableAfterClean <- regressionTableBeforeClean %>%
    dplyr::mutate(
      betase = paste0(
        sprintf(paste0("%.", digits, "f"), estimate),
        " (",
        sprintf(paste0("%.", digits, "f"), std.error),
        ")"
      ),
      betaci = paste0(
        sprintf(paste0("%.", digits, "f"), estimate),
        " (",
        sprintf(paste0("%.", digits, "f"), conf.low),
        ", ",
        sprintf(paste0("%.", digits, "f"), conf.high),
        ")"
      ),
      pValue = sprintf(paste0("%.", pDigits, "f"), p.value),
      betaseWithMark = dplyr::case_when(
        pValue < 0.05 & pValue >= 0.01 ~ paste0(betase, "*"),
        pValue < 0.01 & pValue >= 0.001 ~ paste0(betase, "**"),
        pValue < 0.001 ~ paste0(betase, "***"),
        TRUE ~ NA_character_
      ),
      betaciWithMark = dplyr::case_when(
        pValue < 0.05 & pValue >= 0.01 ~ paste0(betase, "*"),
        pValue < 0.01 & pValue >= 0.001 ~ paste0(betase, "**"),
        pValue < 0.001 ~ paste0(betase, "***"),
        TRUE ~ NA_character_
      )
    )

  ## output selection
  if(estimateType == "betase"){
    tablePart1 = regressionTableAfterClean[,"betase"]
  }else if(estimateType == "betaci"){
    tablePart1 = regressionTableAfterClean[,"betaci"]
  }

  if(pType == "withPValue" & estimateType == "betase"){
    tablePart2 = regressionTableAfterClean[,"pvalue"]
    tableFinal <- cbind(tablePart1, tablePart2)
    names(tableFinal) <- c("beta (se)", "P")
  }else if(pType == "withPValue" & estimateType == "betaci"){
    tablePart2 = regressionTableAfterClean[,"pvalue"]
    tableFinal <- cbind(tablePart1, tablePart2)
    names(tableFinal) <- c("beta (95%CI)", "P")
  }else if(pType == "withMark" & estimateType == "betase"){
    tablePart1 <- regressionTableAfterClean[,"betaseWithMark"]
    tableFinal <- tablePart1
    names(tableFinal) <- c("beta (se)")
  }else if(pType == "withMark" & estimateType == "betaci"){
    tablePart1 <- regressionTableAfterClean[,"betaciWithMark"]
    tableFinal <- tablePart1
    names(tableFinal) <- c("beta (95%CI)")
  }
  return(tableFinal)
}

#' A function to Add variable set after set
#'
#' This function allows you to add variable set into your model set by set
#' @param outcome Linear model set outcome. It could be a character or a character vector
#' @param target variable of interest. For now it can only support one variable of interest. Multiple variables would lead to problems
#' @param covariate control variables in your model. It should be a list of vectors
#' @keywords Model
#' @return regression table with reference level added
#' @export
#' @examples
#' outcome <- "outcome"
#' target <- "V1"
#' covariate <- list(c("V2", "V3"), c("V5", "V6"))
#' addCovariateSetBySet(outcome = outcome, target = target, covariate = covariate)

addCovariateSetBySet <- function(outcome, target, covariate){
  covariateLengthMoreThanTwo <- length(covariate)>=2
  covariateIsList <- is.list(covariate)
  # covariateUnlistResultMoreThanThree <- length(unlist(covariate)) >= 3
  covariateIsListWithTwoOrMoreVector <- covariateLengthMoreThanTwo & covariateIsList

  outcomeIsCharacter <- is.character(outcome)
  outcomeOnlyOne <- length(outcome) == 1
  outcomeIsOnlyOneCharacter <- outcomeIsCharacter & outcomeOnlyOne
  outcomeMoreThanTwo <- length(outcome)>=2
  outcomeIsMoreThanTwoCharacter <- outcomeIsCharacter & outcomeMoreThanTwo
  if(covariateIsListWithTwoOrMoreVector & outcomeIsOnlyOneCharacter){
    covariatePart <- lapply(1:length(covariate), function(x){
      paste0(unlist(covariate[1:x]), collapse = "+")
    }) %>% unlist()
    targetPart <- target
    outcomePart <- outcome
    formulaList <- lapply(1:length(covariatePart), function(n){
      leftPart <- paste0(outcomePart, "~", targetPart)
      formulaList <- paste(leftPart, covariatePart[n], sep = "+")
      return(formulaList)
    })
  }

  if(covariateIsListWithTwoOrMoreVector & outcomeIsMoreThanTwoCharacter){
    covariatePart <- lapply(covariate, function(x){
      paste0(x, collapse = "+")
    }) %>% unlist()
    targetPart <- target
    outcomePart <- outcome %>% unlist()
    covariatePartList <- lapply(1:length(covariatePart), function(x){
      formulaList <- paste0(unlist(covariatePart[1:x]), collapse = "+")
      return(formulaList)
    })
    rightPartList <- paste(targetPart, covariatePartList, sep = "+")
    formulaList <- lapply(1:length(rightPartList), function(i){
      formulaList <- lapply(1:length(outcomePart), function(j){
        formulaList <- paste(outcomePart[i], "~", rightPartList[j])
        return(formulaList)
      })
      return(formulaList)
    })

  }
  formulaList <- lapply(formulaList, unlist)
  return(formulaList)
}


#' A function to summary linear model regression set table based on formula
#'
#' This function allows you to generate linear regression table
#' @param formulaList This should be a list of formula.
#' @param targetLogical Whether include the first independent variable as the variable of interest. Default to True
#' @param pType Linear regression table type, defaults to "withPValue"
#' @param estimateType Linear regression table type, defaults to "betase"
#' @param pType whether put P into final table, default to withPValue, another selection is withMark
#' @param digits Digit number of the estimate
#' @param pDigits Digit number of the P value
#' @param strata Stratification variable. Strata variable would not be adjusted on default. Include the strata variable in the variable if needed
#' @param ... other parameters to pass into tidy function from broom
#' @keywords Model
#' @return regression table with reference level added
#' @export
#' @examples
#' model <- lm(formula = Sepal.Width ~ Petal.Length, data = iris)
#' clean.lm(model)

regressionset.manual.lm <- function(formulaList, targetLogical = TRUE, pType = "withPValue", estimateType = "betase", digits = 2, pDigits = 4, strata = NULL, ...){

# Parameter check part ----------------------------------------------------------------------------------

  ## check if any of the parameter is empty
  formulaListIsNull <- is.na(formulaList)|is.null(formulaList)
  targetLogicalIsNull <- is.na(targetLogical)|is.na(targetLogical)
  pTypeIsNull <- is.na(pType)|is.null(pType)
  estimateTypeIsNull <- is.na(estimateType)|is.null(estimateType)
  digitsIsNull <- is.na(digits)|is.null(digits)
  pDigitsIsNull <- is.na(pDigits)|is.null(pDigits)

  ## check if all the parameter input is valid



}

# complete.cases()
# "outcome1 ~ V2+V3+V4+V5+V6" %>% as.formula() %>% labels()
# all.vars(as.formula("outcome1 ~ V2+V3+V4+V5+V6"))
# all.vars("outcome1 ~ V2+V3+V4+V5+V6")

