
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XQUERY implementation by means of SWI-Prolog
% Jesus Almendros (April 2009)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IS-VAR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_var(String,_):-is_xpath(String,_,_),!,fail.

is_var(String,VarName):-concat_atom([_,Var],'$',String),concat_atom(['Var',Var],'',VarName).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IS-TRIPLE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_triple(String,(NA,NB,NC)):-	concat_atom([_,L],'(',String),
				concat_atom([L3,_],')',L),
				concat_atom([A,B,C],',',L3),
				is_var(A,NA),is_var(B,NB),is_var(C,NC),!.

is_triple_dolar(String,(A,B,C)):- concat_atom([_,L],'(',String),
				concat_atom([L3,_],')',L),
				concat_atom([A,B,C],',',L3),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IS-XPATH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_xpath(String,Var,Path):- concat_atom([Var,Root|Elem],'/',String),
                                concat_atom([Root|Elem],'/',Path).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IS-STRING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_string(String):-is_var(String,_),!,fail.
is_string(String):-is_xpath(String,_,_),!,fail.
is_string(String):-is_attribute(String,_),!,fail.
is_string(_).
                                                

%%%%%%%%%%%%%%%%%%%
% IS-ATTRIBUTE
%%%%%%%%%%%%%%%%%%%%%

is_attribute(Att,Name):-concat_atom(L,'@',Att),
                                L=['',Name].

%%%%%%%%%%%%%%%%%%%%%
% ATTRIBUTE VALUES
%%%%%%%%%%%%%%%%%%%%

attribute_values([],_,[]):-!.

attribute_values([Att=V|RAtt],Att,[V|RV]):-!,attribute_values(RAtt,Att,RV).

attribute_values([_|RAtt],Att,RV):-attribute_values(RAtt,Att,RV).
         

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UPDATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update(N):-identifier(M),!,N is M+1,retract(identifier(M)),assert(identifier(N)). 

update(N):-N=1,assert(identifier(1)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UPDATE-VAR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_var(N):-identifier_var(M),!,N is M+1,retract(identifier_var(M)),assert(identifier_var(N)). 

update_var(N):-N=1,assert(identifier_var(1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STRING TO TERM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

string_to_term(S,T):- 
		concat_atom([A,B],'/@',S),!,
		string_to_term(A,SA),
		G=..['@',B],
		T=..['/',SA,G].

string_to_term(S,T):-string_to_atom(S,A), 
                        atom_to_term(A,T,_).

%%%%%%%%%%%%%%%%%%
% IS_DOC
%%%%%%%%%%%%%%%%%%%

is_doc(XPath):- 
		string_to_term(XPath,doc(_)),!.

is_doc(XPath):-concat_atom([VarRoot|_],'/',XPath),
		string_to_term(VarRoot,doc(_)),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PARENT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 

parent(VarName,VarName,'',for):-  variable(value(for,VarName,in,Path,where,_,return,_,_),_,_), 
			 
			concat_atom([VarRoot|LPath],'/',Path),  
			concat_atom(LPath,'/',_),
			string_to_term(VarRoot,doc(_)),!.
 
parent(VarName,VarName,'',let):-  variable(value(let,VarName,:=,Path,where,_,return,_,_),_,_),  
			 
			concat_atom([VarRoot|LPath],'/',Path),  
			concat_atom(LPath,'/',_),
			string_to_term(VarRoot,doc(_)),!.

parent(VarName,VarName,'',for):-  variable(value(for,VarName,in,Path,return,_,_),_,_), 
			 
			concat_atom([VarRoot|LPath],'/',Path),  
			concat_atom(LPath,'/',_),
			string_to_term(VarRoot,doc(_)),!.
		 
parent(VarName,VarName,'',let):-  variable(value(let,VarName,:=,Path,return,_,_),_,_), 
			 
			concat_atom([VarRoot|LPath],'/',Path),  
			concat_atom(LPath,'/',_),
			string_to_term(VarRoot,doc(_)),!.

parent(VarName,VarName,'',for):-  variable(value(for,VarName,in,_,where,_,return,_,_),_,_),!. 
			 
			 
 
parent(VarName,V,PT,Inst):-  variable(value(let,VarName,:=,Path,where,_,return,_,_),_,_),  
			 
			concat_atom([VarRoot|LPath],'/',Path),  
			concat_atom(LPath,'/',PathRoot),
			parent(VarRoot,V,P,Inst),
			(P\='' -> concat_atom([P,PathRoot],'/',PT); PT=PathRoot),!.		

 
parent(VarName,VarName,'',for):-  variable(value(for,VarName,in,_,return,_,_),_,_),!.
			 
			 	
		 
parent(VarName,V,PT,Inst):-  variable(value(let,VarName,:=,Path,return,_,_),_,_), 
			 
			concat_atom([VarRoot|LPath],'/',Path),  
			concat_atom(LPath,'/',PathRoot),
			parent(VarRoot,V,P,Inst),
			(P\='' -> concat_atom([P,PathRoot],'/',PT); PT=PathRoot),!.		


            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMBINE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


combine([],[]):-!.
combine(ListList,[Heads|List]):-
                        heads_rests(ListList,Heads,Rests),
                        combine(Rests,List),
                        Heads\=[],!.

combine(ListList,[]):-
                        heads_rests(ListList,Heads,Rests),
                        combine(Rests,_),
                        Heads=[].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HEAD-RESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


heads_rests([],[],[]).

heads_rests([[]|RL],RNX,RRX):-heads_rests(RL,RNX,RRX).

heads_rests([[X|RX]|RL],RSX,[RX|RRX]):-is_list(X),!,
                                         heads_rests(RL,RNX,RRX),append(X,RNX,RSX).

heads_rests([[X|RX]|RL],[X|RNX],[RX|RRX]):-heads_rests(RL,RNX,RRX).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LITERAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 
literal(X,X):-literal(X),!.

literal(X,Y):-string_to_term(X,Y),nonvar(Y),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LITERAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 

literal(X):-atomic(X),atom_chars(X,[Y|_]),'0'@=< Y, Y @=< '9',!,fail.

literal(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FLAT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flat([],[]).
flat([X|XL],R):-flat(XL,RL),append(X,RL,R).

:-dynamic_pred.
