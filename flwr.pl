
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XQUERY implementation by means of SWI-Prolog
% Jesus Almendros (April 2009)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PATH-EXP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

path_exp((NA,NB,NC),Vars):- path_var(r,(NA,NB,NC),Vars).

path_exp(X,path(DocN,XPath)):-atomic(DocN),
                        string_to_term(DocN,doc(Doc)),!,     
                        execute_term(Doc,X,XPath).   

path_exp(X,path(Var,XPath)):-atomic(Var),is_var(Var,_),!,  path_var(r,X,path(Var,XPath)).

path_exp(X,path(Y,XPath)):-!,execute_term(Y,X,XPath).  

path_exp([X],String):-atomic(String),X=String.

 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WHERE-EXP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

where_exp(_,_,_,nil):-!.

where_exp(T,N,X,tree(and,T1,nil)):-!,where_exp(T,N,X,T1).

where_exp(T,N,X,tree(and,T1,T2)):-where_exp(T,N,X,T1),!,where_exp(T,N,X,T2).

where_exp(T,N,X,tree(or,T1,_)):-where_exp(T,N,X,T1).

where_exp(T,N,X,tree(or,_,T2)):-where_exp(T,N,X,T2).

where_exp(T,N,X,tree(Exp,nil,nil)):- solve(T,N,X,Exp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOLVE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 

 

solve(r,Name,_,Expr):-Expr=varpath(Name,Var,_,for,1),			
			variable(T,_,_),T=..[value,for,Var|_],!, 
			path_exp(O,path(Var,'')),
			member(_,O).

solve(r,Name,_,Expr):-Expr=varpath(Name,Var,_,for,1),  
			variable(T,_,_),T=..[value,let,Var|_],!,  
			path_exp(_,path(Var,'')).

solve(r,Name,_,Expr):-Expr=varpath(_,Name,_,for,1),!.

solve(r,Name,X,Expr):-Expr=varpath(_,Name,XPath,for,2),
			variable(T,_,_),T=..[value,for,Name|_],!,  
			path_exp(O,path(X,XPath)),
			member(_,O).

solve(r,Name,X,Expr):-Expr=varpath(_,Name,XPath,for,2),
			variable(T,_,_),T=..[value,let,Name|_],!, 
			path_exp(_,path(X,XPath)).

 
solve(r,Name,_,Expr):-Expr=varpath(Name,_,_,for,2),!.
			
			 
 

solve(nr,_,_,Expr):-Expr=varpath(_,_,_,for,_),!.

solve(_,Name,X,Expr):- Expr=..[Op,varpath(Name,XPath1,for),varpath(Name,XPath2,for)], !,
				path_exp(O1,path(X,XPath1)),
				path_exp(O2,path(X,XPath2)),
				member(E1,O1),
				member(E2,O2),
				literal(E1,L1),literal(E2,L2),
 	                        Value=..[Op,L1,L2], 
                                Value.

solve(_,Name,X,Expr):- Expr=..[Op,varpath(Name,XPath1,let),varpath(Name,XPath2,for)], !,
				path_exp(O1,path(X,XPath1)),
				path_exp(O2,path(X,XPath2)),
				 
				member(E2,O2),
				literal(O1,L1),literal(E2,L2),
 	                        Value=..[Op,L1,L2], 
                                Value.

solve(_,Name,X,Expr):- Expr=..[Op,varpath(Name,XPath1,for),varpath(Name,XPath2,let)], !,
				path_exp(O1,path(X,XPath1)),
				path_exp(O2,path(X,XPath2)),
				member(E1,O1),
				 
				literal(E1,L1),literal(O2,L2),
 	                        Value=..[Op,L1,L2], 
                                Value.

solve(_,Name,X,Expr):- Expr=..[Op,varpath(Name,XPath1,let),varpath(Name,XPath2,let)], !,
				path_exp(O1,path(X,XPath1)),
				path_exp(O2,path(X,XPath2)),
				 
				literal(O1,L1),literal(O2,L2),
 	                        Value=..[Op,L1,L2], 
                                Value.


solve(r,Name,X,Expr):-Expr=..[Op,varpath(Name,XPath1,for),varpath(Y,XPath2,for)], !,
				path_exp(O1,path(X,XPath1)), 
				member(E1,O1),
				path_var(nr,Out2,path(Y,XPath2)),
				member(O2,Out2),								
 	            		literal(E1,L1),literal(O2,L2),
 	                        Value=..[Op,L1,L2],  
                		Value.

solve(r,Name,X,Expr):-Expr=..[Op,varpath(Name,XPath1,for),varpath(Y,XPath2,let)], !,
				path_exp(O1,path(X,XPath1)), 
				member(E1,O1),
				path_var(nr,Out2,path(Y,XPath2)),
				 								
 	            		literal(E1,L1),literal(Out2,L2),
 	                        Value=..[Op,L1,L2],  
                		Value.

solve(r,Name,X,Expr):-Expr=..[Op,varpath(Name,XPath1,let),varpath(Y,XPath2,for)], !,
				path_exp(O1,path(X,XPath1)), 
				 
				path_var(nr,Out2,path(Y,XPath2)),
				member(O2,Out2),								
 	            		literal(O1,L1),literal(O2,L2),
 	                        Value=..[Op,L1,L2],  
                		Value.

solve(r,Name,X,Expr):-Expr=..[Op,varpath(Name,XPath1,let),varpath(Y,XPath2,let)], !,
				path_exp(O1,path(X,XPath1)), 
				 
				path_var(nr,Out2,path(Y,XPath2)),
				 								
 	            		literal(O1,L1),literal(Out2,L2),
 	                        Value=..[Op,L1,L2],  
                		Value.

solve(r,Name,Y,Expr):-Expr=..[Op,varpath(X,XPath1,for),varpath(Name,XPath2,for)], !,
				path_exp(O2,path(Y,XPath2)),
				path_var(nr,Out1,path(X,XPath1)),
				member(E2,O2),  
				member(O1,Out1),
				literal(O1,L1),literal(E2,L2),
 	                        Value=..[Op,L1,L2], 
                		Value.

solve(r,Name,Y,Expr):-Expr=..[Op,varpath(X,XPath1,let),varpath(Name,XPath2,for)], !,
				path_exp(O2,path(Y,XPath2)),
				path_var(nr,Out1,path(X,XPath1)),
				member(E2,O2),  
				 
				literal(Out1,L1),literal(E2,L2),
 	                        Value=..[Op,L1,L2], 
                		Value.

solve(r,Name,Y,Expr):-Expr=..[Op,varpath(X,XPath1,for),varpath(Name,XPath2,let)], !,
				path_exp(O2,path(Y,XPath2)),
				path_var(nr,Out1,path(X,XPath1)),
				   
				member(O1,Out1),
				literal(O1,L1),literal(O2,L2),
 	                        Value=..[Op,L1,L2], 
                		Value.

solve(r,Name,Y,Expr):-Expr=..[Op,varpath(X,XPath1,let),varpath(Name,XPath2,let)], !,
				path_exp(O2,path(Y,XPath2)),
				path_var(nr,Out1,path(X,XPath1)),
				 
				literal(Out1,L1),literal(O2,L2),
 	                        Value=..[Op,L1,L2], 
                		Value.

solve(r,_,_,Expr):-Expr=..[Op,varpath(Z,XPath1,for),varpath(Y,XPath2,for)], !,
				path_var(nr,Out1,path(Z,XPath1)), 
				member(O1,Out1),
				path_var(nr,Out2,path(Y,XPath2)),
				member(O2,Out2),								
 	            		literal(O1,L1),literal(O2,L2),
 	                        Value=..[Op,L1,L2],  
                		Value.

solve(r,_,_,Expr):-Expr=..[Op,varpath(Z,XPath1,for),varpath(Y,XPath2,let)], !,
				path_var(nr,Out1,path(Z,XPath1)), 
				member(O1,Out1),
				path_var(nr,Out2,path(Y,XPath2)),
				 								
 	            		literal(O1,L1),literal(Out2,L2),
 	                        Value=..[Op,L1,L2],  
                		Value.

solve(r,_,_,Expr):-Expr=..[Op,varpath(Z,XPath1,let),varpath(Y,XPath2,for)], !,
				path_var(nr,Out1,path(Z,XPath1)), 
				 
				path_var(nr,Out2,path(Y,XPath2)),
				member(O2,Out2),								
 	            		literal(Out1,L1),literal(O2,L2),
 	                        Value=..[Op,L1,L2],  
                		Value.

solve(r,_,_,Expr):-Expr=..[Op,varpath(Z,XPath1,let),varpath(Y,XPath2,let)], !,
				path_var(nr,Out1,path(Z,XPath1)), 
				 
				path_var(nr,Out2,path(Y,XPath2)),
				 								
 	            		literal(Out1,L1),literal(Out2,L2),
 	                        Value=..[Op,L1,L2],  
                		Value.

solve(nr,Name,_,Expr):-Expr=..[_,varpath(Name,_,_),varpath(_,_,_)], !.
					
									
					
				 
solve(nr,Name,_,Expr):-Expr=..[_,varpath(_,_,_),varpath(Name,_,_)], !.	 

solve(nr,_,_,Expr):-Expr=..[_,varpath(_,_,_),varpath(_,_,_)], !.
	 
					 
solve(_,Name,X,Expr):-Expr=..[Op,varpath(Name,XPath1,for),XPath2], !,  
  			path_exp(O1,path(X,XPath1)),   
			member(E1,O1),
                    	literal(E1,L1),literal(XPath2,L2),   
 	                Value=..[Op,L1,L2],  
                    	Value.

solve(_,Name,X,Expr):-Expr=..[Op,varpath(Name,XPath1,let),XPath2], !,  
  			path_exp(O1,path(X,XPath1)),   
			 
                    	literal(O1,L1),literal(XPath2,L2),   
 	                Value=..[Op,L1,L2],  
                    	Value.

solve(_,Name,Y,Expr):-Expr=..[Op,XPath1,varpath(Name,XPath2,for)],!,
                    	path_exp(O2,path(Y,XPath2)), 
			member(E2,O2),
			literal(XPath1,L1),literal(E2,L2),
 	                Value=..[Op,L1,L2],   
                    	Value.

solve(_,Name,Y,Expr):-Expr=..[Op,XPath1,varpath(Name,XPath2,let)],!,
                    	path_exp(O2,path(Y,XPath2)), 
			 
			literal(XPath1,L1),literal(O2,L2),
 	                Value=..[Op,L1,L2],   
                    	Value.

solve(_,_,_,Expr):-Expr=..[Op,varpath(Y,XPath1,for),XPath2], !,  
  			path_var(nr,Out1,path(Y,XPath1)),   
			member(O1,Out1),
                    	literal(O1,L1),literal(XPath2,L2),   
 	                Value=..[Op,L1,L2],  
                    	Value.

solve(_,_,_,Expr):-Expr=..[Op,varpath(Y,XPath1,let),XPath2], !,  
  			path_var(nr,Out1,path(Y,XPath1)),   
			 
                    	literal(Out1,L1),literal(XPath2,L2),   
 	                Value=..[Op,L1,L2],  
                    	Value.

solve(_,_,_,Expr):-Expr=..[Op,XPath2,varpath(Y,XPath1,for)], !,  
  			path_var(nr,Out1,path(Y,XPath1)),   
			member(O1,Out1),
                    	literal(O1,L1),literal(XPath2,L2),   
 	                Value=..[Op,L1,L2],  
                    	Value.

solve(_,_,_,Expr):-Expr=..[Op,XPath2,varpath(Y,XPath1,let)], !,  
  			path_var(nr,Out1,path(Y,XPath1)),   
			 
                    	literal(Out1,L1),literal(XPath2,L2),   
 	                Value=..[Op,L1,L2],  
                    	Value.

 




 

 

