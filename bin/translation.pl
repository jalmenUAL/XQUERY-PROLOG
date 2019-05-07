%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XQUERY implementation by means of SWI-Prolog
% Jesus Almendros (February 2010)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% barckets, parenthesis no closed
% operations closed
% operadores aritmeticos con comillas

 

% RDF/OWL
% Funciones Prolog
% errores compilador



 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRANSLATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



translate([],_):-!. 

%--------------FLOWER EXPRESSION  

 

translate([value(for,Name,in,Path,where,Cond,return,empty,Bool),Pattern],N):- !,  
                                update(K), 
                                update(M),
				assert((querying(PR,N):-findall(P,querying(P,K),Result),flat(Result,PR))), 
                                translate_xquery(value(for,Name,in,Path,where,Cond,return,empty,Bool),K,Pattern,M,Name).


translate([value(for,Name,in,Path,where,Cond,return,Var,Bool),Pattern],N):- !,  
                                update(K), 
                                update(M),
				(is_doc(Path)->assert(condition2(tree(varpath(Name,Var,'',for,1),nil,nil)));
				(is_var(Path,_)->
				assert(condition2(tree(varpath(Name,Path,'',for,2),nil,nil)));
				is_xpath(Path,Var2,VPath),
				assert(condition2(tree(varpath(Name,Var2,VPath,for,2),nil,nil)))
				)),				
				assert((querying(PR,N):-findall(P,querying(P,K),Result),flat(Result,PR))),
				translate_xquery(value(for,Name,in,Path,where,Cond,return,Var,Bool),K,Pattern,M,Name).
	
translate([value(let,Name,:=,Path,where,Cond,return,empty,Bool),Pattern],N):-   !,
                                update(M), 
                                translate_xquery(value(let,Name,:=,Path,where,Cond,return,empty,Bool),N,Pattern,M,Name).

				 
translate([value(let,Name,:=,Path,where,Cond,return,Var,Bool),Pattern],N):- !,
                                update(M), 
				update(R),
				assert((querying(PR,N):-findall(P,querying(P,R),Result),flat(Result,PR))),
                                translate_xquery(value(let,Name,:=,Path,where,Cond,return,Var,Bool),R,Pattern,M,Name). 
                                 
translate([value(for,Name,in,Path,return,empty,Bool),Pattern],N):-  
				 !,
                                update(K),
                                update(M),
				
				assert((querying(PR,N):-findall(P,querying(P,K),Result),flat(Result,PR))),
                                translate_xquery(value(for,Name,in,Path,return,empty,Bool),K,Pattern,M,Name).

translate([value(for,Name,in,Path,return,Var,Bool),Pattern],N):-!,
                                update(K),
                                update(M),
				(is_doc(Path)->assert(condition2(tree(varpath(Name,Var,'',for,1),nil,nil)));
				(is_var(Path,_)->
				assert(condition2(tree(varpath(Name,Path,'',for,2),nil,nil)));
				is_xpath(Path,Var2,VPath),
				assert(condition2(tree(varpath(Name,Var2,VPath,for,2),nil,nil)))
				)),
				assert((querying(PR,N):-findall(P,querying(P,K),Result),flat(Result,PR))),
				translate_xquery(value(for,Name,in,Path,return,Var,Bool),K,Pattern,M,Name).

translate([value(let,Name,:=,Path,return,empty,Bool),Pattern],N):-  !,
                                update(M), 
                                translate_xquery(value(let,Name,:=,Path,return,empty,Bool),N,Pattern,M,Name).


translate([value(let,Name,:=,Path,return,Var,Bool),Pattern],N):- !,
                                update(M), 
				update(R),
				assert((querying(PR,N):-findall(P,querying(P,R),Result),flat(Result,PR))),
                                translate_xquery(value(let,Name,:=,Path,return,Var,Bool),R,Pattern,M,Name). 
                                 



%-----LABELED ITEM

translate([element(Label,Att,Expr)],N):-!, 
                        update(M),  
                        translate(Expr,M), 
                        assert((querying([element(Label,Att,Y)],N):-querying(Y,M))).


%----------VARIABLE


translate([value(Query,_,_)|_],_):-is_var(Query,_),
				condition2(tree(varpath(Name,Query,Path,for,2),nil,nil)),
				(\+condition(tree(varpath(Name,Query,Path,for,2),nil,nil))->
				assert(condition(tree(varpath(Name,Query,Path,for,2),nil,nil)));true),
				fail. 

translate([value(Query,_,_)|_],_):-is_var(Query,_),
				condition2(tree(varpath(Query,Name,Path,for,1),nil,nil)),
				(\+condition(tree(varpath(Query,Name,Path,for,1),nil,nil))->
				assert(condition(tree(varpath(Query,Name,Path,for,1),nil,nil)));true),
				fail. 

translate([value(Query,_,_)|_],_):-is_xpath(Query,Var,_),
				condition2(tree(varpath(Name,Var,Path,for,2),nil,nil)),
				(\+condition(tree(varpath(Name,Var,Path,for,2),nil,nil))-> 
				assert(condition(tree(varpath(Name,Var,Path,for,2),nil,nil)));true),
				fail. 

translate([value(Query,_,_)|_],_):-is_xpath(Query,Var,_),
				condition2(tree(varpath(Var,Name,Path,for,1),nil,nil)),
				(\+condition(tree(varpath(Var,Name,Path,for,1),nil,nil))->
				assert(condition(tree(varpath(Var,Name,Path,for,1),nil,nil)));true),
				fail. 

translate([value(Query,_,_)],N):-   
                        is_var(Query,SVar), 
			
                        variable(value(for,X,in,Path,where,Cond,return,_,_),_,NV),  
                        is_var(X,XVar), 
                        XVar==SVar,!,
                        string_to_term(XVar,Term),    
                        retrieve_flwr(Term,Path,Cond,PathA,DocA,CondA),  
			(\+condition(CondA)->assert(condition(CondA));true),
			translate_var(NV,DocA,PathA),     		
			assert((querying([VarName],N):-flwr(Result,N),member(VarName,Result))), 
			(NV\=N->assert((flwr(Term,N):-path_exp(Term,path(X,''))));true) .

 
translate([value(Query,_,_)],N):-  
                        is_var(Query,SVar), 
			 
                        variable(value(let,X,:=,Path,where,Cond,return,_,_),_,NV),    
                        is_var(X,XVar),
                        XVar==SVar,!,  
                        string_to_term(XVar,Term),    
                        retrieve_flwr(Term,Path,Cond,PathA,DocA,CondA),
			(\+condition(CondA)->assert(condition(CondA));true), 			 
			translate_var(NV,DocA,PathA),                         
			assert((querying(Result,N):-flwr(Result,N))),			 
			(NV\=N->assert((flwr(Term,N):-path_exp(Term,path(X,''))));true) .

 

 

translate([value(Query,_,_)],N):-   
                        is_var(Query,SVar), 
			 
                        variable(value(for,X,in,Path,return,_,_),_,NV),  
                        is_var(X,XVar),
                        XVar==SVar,!, 
                        string_to_term(XVar,Term),  
                        retrieve_flwr(Term,Path,'',PathA,DocA,_), 
			translate_var(NV,DocA,PathA),  		 
                        assert((querying([VarName],N):- flwr(Result,N),member(VarName,Result))),
			(NV\=N->assert((flwr(Term,N):-path_exp(Term,path(X,''))) );true).

 


translate([value(Query,_,_)],N):-  
                        is_var(Query,SVar), 
			 
                        variable(value(let,X,:=,Path,return,_,_),_,NV),  
                        is_var(X,XVar),
                        XVar==SVar,!,
                        string_to_term(XVar,Term),
                        retrieve_flwr(Term,Path,'',PathA,DocA,_),
			translate_var(NV,DocA,PathA),   	 
                        assert((querying(Result,N):- flwr(Result,N))), 		  
			(NV\=N->assert((flwr(Term,N):-path_exp(Term,path(X,''))));true) .

 

%-------XPATH 



translate([value(Query,_,_)],N):- 
                        is_xpath(Query,Var,Path),
			 
                        is_var(Var,_),!, 
			translate_var(N,Var,Path),  
			assert((querying(A,N):-flwr(A,N))).

  
translate([value(Query,_,_)],N):- 
                        is_xpath(Query,Var,Path),!,
			assert((querying(A,N):-path_exp(A,path(Var,Path)))).

%------STRING

translate([string(String,_,Env)],N):-is_var(Env,_),!,  
			 assert((querying([' ',String,' '],N):-path_exp(A,path(Env,'')),member(_,A))).


translate([string(String,_,Env)],N):-is_xpath(Env,VEnv,PEnv),!,  
			 assert((querying([String],N):-path_exp(A,path(VEnv,PEnv)),member(_,A))).


  


%------FLOWER   

translate([value(for,Name,in,Path,where,Cond,return,empty,Bool),Pattern|Relement],N):-  !, 
                update(K),
		update(M),
		assert((querying(PR,N):-findall(P,querying(P,M),Result),flat(Result,PR))),
                translate_xquery_list(value(for,Name,in,Path,where,Cond,return,empty,Bool),M,Pattern,Relement,K,Name).

                             

translate([value(for,Name,in,Path,where,Cond,return,Var,Bool),Pattern|Relement],N):-  !, 
                update(K),
		update(M),
		(is_doc(Path)->assert(condition2(tree(varpath(Name,Var,'',for,1),nil,nil)));
				(is_var(Path,_)->
				assert(condition2(tree(varpath(Name,Path,'',for,2),nil,nil)));
				is_xpath(Path,Var2,VPath),
				assert(condition2(tree(varpath(Name,Var2,VPath,for,2),nil,nil)))
				)),
		assert((querying(PR,N):-findall(P,querying(P,M),Result),flat(Result,PR))),                 
		translate_xquery_list(value(for,Name,in,Path,where,Cond,return,Var,Bool),M,Pattern,Relement,K,Name).
				 

translate([value(let,Name,:=,Path,where,Cond,return,empty,Bool),Pattern|Relement],N):- !,
                 update(K),
                 translate_xquery_list(value(let,Name,:=,Path,where,Cond,return,empty,Bool),N,Pattern,Relement,K,Name).


translate([value(let,Name,:=,Path,where,Cond,return,Var,Bool),Pattern|Relement],N):-!,
                update(K),
                update(R),
		assert((querying(PR,N):-findall(P,querying(P,R),Result),flat(Result,PR))),
		translate_xquery_list(value(let,Name,:=,Path,where,Cond,return,Var,Bool),R,Pattern,Relement,K,Name).
                                 
translate([value(for,Name,in,Path,return,empty,Bool),Pattern|Relement],N):- !,
                        update(K),
			update(M),			 
			assert((querying(PR,N):-findall(P,querying(P,M),Result),flat(Result,PR))),
                        translate_xquery_list(value(for,Name,in,Path,return,empty,Bool),M,Pattern,Relement,K,Name).


translate([value(for,Name,in,Path,return,Var,Bool),Pattern|Relement],N):-!, 
                        update(K),
			update(M),
			(is_doc(Path)->assert(condition2(tree(varpath(Name,Var,'',for,1),nil,nil)));
				(is_var(Path,_)->
				assert(condition2(tree(varpath(Name,Path,'',for,2),nil,nil)));
				is_xpath(Path,Var2,VPath),
				assert(condition2(tree(varpath(Name,Var2,VPath,for,2),nil,nil)))
				)), 
			assert((querying(PR,N):-findall(P,querying(P,M),Result),flat(Result,PR))),
                        translate_xquery_list(value(for,Name,in,Path,return,Var,Bool),M,Pattern,Relement,K,Name).
				 
translate([value(let,Name,:=,Path,return,empty,Bool),Pattern|Relement],N):- !, 
                        update(K),
                        translate_xquery_list(value(let,Name,:=,Path,return,empty,Bool),N,Pattern,Relement,K,Name).
            
translate([value(let,Name,:=,Path,return,Var,Bool),Pattern|Relement],N):-!,
                        update(K),
			update(R),
			assert((querying(PR,N):-findall(P,querying(P,R),Result),flat(Result,PR))),
                        translate_xquery_list(value(let,Name,:=,Path,return,Var,Bool),R,Pattern,Relement,K,Name).
                                 


 

%------LABELED ITEM


translate([element(Label,Att,Expr)|Relement],N):-!,  
                        update(M), 
                        translate(Expr,M),  
                        update(K),
                        assert((querying([element(Label,Att,Y)|Z],N):-querying(Y,M),querying(Z,K))),
                        translate(Relement,K).


			

                                

%-----------------VARIABLE

 
translate([value(Query,_,_)|Relement],N):-   
                        is_var(Query,SVar), 
			 
                        variable(value(for,X,in,Path,where,Cond,return,_,_),_,NV),  
                        is_var(X,XVar), 
                        XVar==SVar,!,
			update(K),
			update(S),
			translate(Relement,S),
			assert((querying(PR,N):-findall(P,querying(P,K),LP),
			findall(R,querying(R,S),LR), 
                        combine([LP,LR],PRL),member(PR,PRL))), 
                        string_to_term(XVar,Term),   
                        retrieve_flwr(Term,Path,Cond,PathA,DocA,CondA), 
			(\+condition(CondA)->assert(condition(CondA));true),
			translate_var(NV,DocA,PathA),     
			assert((querying([VarName],K):-flwr(Result,K),member(VarName,Result))), 
			(NV\=K->assert((flwr(Term,K):-path_exp(Term,path(X,''))));true) .


 translate([value(Query,_,_)|Relement],N):-  
                        is_var(Query,SVar), 
			 
                        variable(value(let,X,:=,Path,where,Cond,return,_,_),_,NV),    
                        is_var(X,XVar),
                        XVar==SVar,!,  
			update(K),
			update(S),
			translate(Relement,S),
			assert((querying(PR,N):-findall(P,querying(P,K),LP),
			findall(R,querying(R,S),LR), 
                        combine([LP,LR],PRL),member(PR,PRL))), 
                        string_to_term(XVar,Term),    
                        retrieve_flwr(Term,Path,Cond,PathA,DocA,CondA),
			(\+condition(CondA)->assert(condition(CondA));true), 			 
			translate_var(NV,DocA,PathA),                         
			assert((querying(Result,K):-flwr(Result,K))),			 
			(NV\=K->assert((flwr(Term,K):-path_exp(Term,path(X,''))));true) .

 

translate([value(Query,_,_)|Relement],N):-  
                        is_var(Query,SVar), 
			 
                        variable(value(for,X,in,Path,return,_,_),_,NV),  
                        is_var(X,XVar),
                        XVar==SVar,!,
			update(K),
			update(S),
			translate(Relement,S),
			assert((querying(PR,N):-findall(P,querying(P,K),LP),
			findall(R,querying(R,S),LR), 
                        combine([LP,LR],PRL),member(PR,PRL))), 
                        string_to_term(XVar,Term),  
                        retrieve_flwr(Term,Path,'',PathA,DocA,_), 
			translate_var(NV,DocA,PathA),  		 
                        assert((querying([VarName],K):- flwr(Result,K),member(VarName,Result))),
			(NV\=K->assert((flwr(Term,K):-path_exp(Term,path(X,''))) );true).


 translate([value(Query,_,_)|Relement],N):-  
                        is_var(Query,SVar), 
			 
                        variable(value(let,X,:=,Path,return,_,_),_,NV),  
                        is_var(X,XVar),
                        XVar==SVar,!,
			update(K),
			update(S),
			translate(Relement,S),
			assert((querying(PR,N):-findall(P,querying(P,K),LP),
			findall(R,querying(R,S),LR), 
                        combine([LP,LR],PRL),member(PR,PRL))), 
                        string_to_term(XVar,Term),
                        retrieve_flwr(Term,Path,'',PathA,DocA,_),
			translate_var(NV,DocA,PathA),   	 
                        assert((querying(Result,K):- flwr(Result,K))), 		  
			(NV\=K->assert((flwr(Term,K):-path_exp(Term,path(X,''))));true) .

   

                                        
%-------XPATH 


translate([value(Query,_,_)|Relement],N):- 
                        is_xpath(Query,Var,Path),
			 
                        is_var(Var,_),!, 
			update(K),
			translate_var(K,Var,Path),  
			assert((querying(A,K):-flwr(A,K))),
			update(S),                           
			assert((querying(PR,N):-findall(P,querying(P,K),LP),
			findall(R,querying(R,S),LR), 
                        combine([LP,LR],PRL),member(PR,PRL))),                            
			translate(Relement,S). 

  

			
 
  

%-------STRING




translate([string(String,_,_)|Relement],N):-    
				update(K),
				assert((querying([' ',String,' '|A],N):-
				 
				querying(A,K))), 
                                translate(Relement,K).

  
 


 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRANSLATE_XQUERY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

translate_xquery_list(For,N,Pattern,Relement,K,VarName):-!,                                
                                        asserta(variable(For,Pattern,K)),  
                                        translate([value(VarName,VarName,empty)],K),				   
					translate([Pattern|Relement],N).
					 
                                         
                                         

                                          

translate_xquery(For,N,Pattern,K,VarName):- 
                                        asserta(variable(For,Pattern,K)),  
                                        translate([value(VarName,VarName,empty)],K), 
					translate([Pattern],N).
					 

                
                     
%%%%%%%%%%%%%%%%
% TRANSLATE-VAR
%%%%%%%%%%%%%%%%%
                    
translate_var(K,_,_):-defined(K),!.

translate_var(K,Doc,Path):- 
				assert((flwr(Var,K):-path_exp(Var,path(Doc,Path)))),
				assert(defined(K)).

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RETRIEVE-FLWR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

retrieve_flwr(_,Path,'',PathA,Doc,[]):-!,
                        concat_atom([Doc|PathToken],'/',Path),  
                        concat_atom( PathToken ,'/',PathA).

retrieve_flwr(_,Path,Cond,PathA,Doc,CondA):-
                        concat_atom([Doc |PathToken],'/',Path),   
                        concat_atom( PathToken ,'/',PathA), 
			condition_parent(Cond,CondA).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONDITION PARENT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		              

condition_parent(nil,nil):-!.

condition_parent(true,true):-!.

condition_parent(tree(varpath(X,Path),nil,nil),tree(A,nil,nil)):-!,condition_hand_side(varpath(X,Path),A).

condition_parent(tree(or,R,L),tree(or,RA,LA)):-!, 
						condition_parent(R,RA),
						condition_parent(L,LA).
condition_parent(tree(and,R,L),tree(and,RA,LA)):-!, 
						condition_parent(R,RA),
						condition_parent(L,LA).

condition_parent(tree(X,R,L),tree(XA,RA,LA)):-!,condition_parent(X,XA),
						condition_parent(R,RA),
						condition_parent(L,LA).

condition_parent(Exp,Exp2):-Exp=..[Op,LHS,RHS],
				condition_hand_side(LHS,LHSA),
				condition_hand_side(RHS,RHSA),
				Exp2=..[Op,LHSA,RHSA].


condition_hand_side(String,String):-atomic(String),!.

condition_hand_side(varpath(Var,Path),varpath(VarA,PathA,Inst)):-
				parent(Var,VarA,PathRoot,Inst),
				(Path\='',PathRoot\='' -> concat_atom([PathRoot,Path],'/',PathA);
				(Path='' -> PathA=PathRoot; PathA=Path)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% retrieve_conditions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





retrieve_conditions:- variable(value(for,VarName,in,_,where,_,return,_,_),_,K),  	
			is_var(VarName,_),
			collect_conditions_aux(VarName,Conds),  
			assert((path_var(r,X,path(VarName,B)):- 
                       	querying(Y,K), 
			where_exp(r,VarName,Y,Conds),
			path_exp(X,path(Y,B)))),
 			assert((path_var(nr,X,path(VarName,B)):- 
                       	querying(Y,K), 
			where_exp(nr,VarName,Y,Conds),
			path_exp(X,path(Y,B)))),
 			fail.
			 


 


retrieve_conditions:- variable(value(let,VarName,:=,_,where,_,return,_,_),_,K),
			is_var(VarName,_), 
			collect_conditions_aux(VarName,Conds), 
			assert((path_var(r,X,path(VarName,B)):- 
                       	querying(Y,K), 
			where_exp(r,VarName,Y,Conds),
			path_exp(X,path(Y,B)))),
 			assert((path_var(nr,X,path(VarName,B)):- 
                       	querying(Y,K), 
			where_exp(nr,VarName,Y,Conds),
			path_exp(X,path(Y,B)))),
 			fail.
		 

 


retrieve_conditions:- variable(value(for,VarName,in,_,return,_,_),_,K), 
			is_var(VarName,_),
			collect_conditions_aux(VarName,Conds),  
			assert((path_var(r,X,path(VarName,B)):- 
                       	querying(Y,K), 
			where_exp(r,VarName,Y,Conds),
			path_exp(X,path(Y,B)))),
 			assert((path_var(nr,X,path(VarName,B)):- 
                       	querying(Y,K), 
			where_exp(nr,VarName,Y,Conds),
			path_exp(X,path(Y,B)))),
 			fail.
			 


 


retrieve_conditions:- variable(value(let,VarName,:=,_,return,_,_),_,K),
			is_var(VarName,_), 
			collect_conditions_aux(VarName,Conds), 
			assert((path_var(r,X,path(VarName,B)):- 
                       	querying(Y,K), 
			where_exp(r,VarName,Y,Conds),
			path_exp(X,path(Y,B)))),
 			assert((path_var(nr,X,path(VarName,B)):- 
                       	querying(Y,K), 
			where_exp(nr,VarName,Y,Conds),
			path_exp(X,path(Y,B)))),
 			fail.
		 

 


retrieve_conditions.

%%%%%%%%%%%%%%%%%%%%
% FLATTEN-AND
%%%%%%%%%%%%%%%%%%%%%
 
flatten_and([],nil):-!.

flatten_and([X|L],tree(and,X,Tree)):-flatten_and(L,Tree).  

%%%%%%%%%%%%%%%%%%%%%
% COLLECT-CONDITIONS-AUX
%%%%%%%%%%%%%%%%%%%%%%%

 
collect_conditions_aux(VarName,CondF):- 
			findall(Cond,(condition(Cond),contains_term(VarName,Cond)),Conds),
			flatten_and(Conds,CondF). 


 			 
	
 
 