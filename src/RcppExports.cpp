// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// fstStore
SEXP fstStore(String fileName, SEXP table, SEXP compression);
RcppExport SEXP fst_fstStore(SEXP fileNameSEXP, SEXP tableSEXP, SEXP compressionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< String >::type fileName(fileNameSEXP);
    Rcpp::traits::input_parameter< SEXP >::type table(tableSEXP);
    Rcpp::traits::input_parameter< SEXP >::type compression(compressionSEXP);
    rcpp_result_gen = Rcpp::wrap(fstStore(fileName, table, compression));
    return rcpp_result_gen;
END_RCPP
}
// fstMeta
List fstMeta(String fileName);
RcppExport SEXP fst_fstMeta(SEXP fileNameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< String >::type fileName(fileNameSEXP);
    rcpp_result_gen = Rcpp::wrap(fstMeta(fileName));
    return rcpp_result_gen;
END_RCPP
}
// fstRead
SEXP fstRead(SEXP fileName, SEXP columnSelection, SEXP startRow, SEXP endRow);
RcppExport SEXP fst_fstRead(SEXP fileNameSEXP, SEXP columnSelectionSEXP, SEXP startRowSEXP, SEXP endRowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type fileName(fileNameSEXP);
    Rcpp::traits::input_parameter< SEXP >::type columnSelection(columnSelectionSEXP);
    Rcpp::traits::input_parameter< SEXP >::type startRow(startRowSEXP);
    Rcpp::traits::input_parameter< SEXP >::type endRow(endRowSEXP);
    rcpp_result_gen = Rcpp::wrap(fstRead(fileName, columnSelection, startRow, endRow));
    return rcpp_result_gen;
END_RCPP
}
