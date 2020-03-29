#include <Rcpp.h>

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// [[Rcpp::plugins("cpp11")]]

// [[Rcpp::export]]
Rcpp::DataFrame SIR_CF(double i_rate, double r_rate, double init_p,double dt) {
  Rcpp::IntegerVector pool = Rcpp::seq(1, 1500);
  Rcpp::NumericVector tt = Rcpp::as<Rcpp::NumericVector>(pool)*dt;
  int tt_len=tt.length();
  Rcpp::NumericVector SS(tt_len);
  Rcpp::NumericVector II(tt_len);
  Rcpp::NumericVector RR(tt_len);
  
  SS[0]=1-init_p;
  II[0]=init_p;
  RR[0]=0;
  
  for(int i=1; i<tt_len; i++) {
    SS[i]=SS[i-1]*(1-i_rate*II[i-1]*dt);
    II[i]=II[i-1]*(1+(i_rate*SS[i-1]-r_rate)*dt);
    RR[i]=RR[i-1]+dt*r_rate*II[i-1];
  }
  
  std::vector<double> TIME;
  std::vector<double> Vals;
  std::vector<std::string> Group;
  Rcpp::CharacterVector Group_S=rep(Rcpp::CharacterVector("S"),tt_len);
  Rcpp::CharacterVector Group_I=rep(Rcpp::CharacterVector("I"),tt_len);
  Rcpp::CharacterVector Group_R=rep(Rcpp::CharacterVector("R"),tt_len);
  
  
  TIME.insert(TIME.begin(),tt.begin(),tt.end());
  TIME.insert(TIME.end(),tt.begin(),tt.end());
  TIME.insert(TIME.end(),tt.begin(),tt.end());
  
  Group.insert(Group.begin(),Group_S.begin(),Group_S.end());
  Group.insert(Group.end(),Group_I.begin(),Group_I.end());
  Group.insert(Group.end(),Group_R.begin(),Group_R.end());
  
  Vals.insert(Vals.begin(),SS.begin(),SS.end());
  Vals.insert(Vals.end(),II.begin(),II.end());
  Vals.insert(Vals.end(),RR.begin(),RR.end());
  
  Rcpp::DataFrame df = Rcpp::DataFrame::create( Rcpp::Named("Time") = TIME, Rcpp::Named("Group") = Group, Rcpp::Named("Vals") = Vals);
  return df;
}
