
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XQUERY implementation by means of SWI-Prolog
% Jesus Almendros (April 2009)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%
% NEWCOMP
%%%%%%%%%%%%%%%%%%%%%%

newcomp(File,Def):-load_xml(File,Token), 
		clean(Token,Clean),   
		remove_br(Clean,Term), 
		analyze(empty,empty,Term,Def),
		write('Compiled successfully'),nl,!.

newcomp(_,_):-write('Compiler Error'),nl,!.
		


%%%%%%%%%%%%%%%%%%%%%
% REMOVE_BR
%%%%%%%%%%%%%%%%%%%%%


remove_br([],[]):-!.

remove_br([element(Label,Att,SubElement)|Elements],
			[element(Label,Att,RSubElement)|RElements]):-!,
			remove_br(SubElement,RSubElement),
			remove_br(Elements,RElements).

remove_br([String|Elements],Def):-
			remove_br_string(String,RString),
			remove_br(Elements,RElements),
			append(RString,RElements,Def).

remove_br_string(String,RString):-concat_atom(List,' ',String),  
				token(List,RString).


%%%%%%%%%%%%%%%%%%%%%%%%
% TOKEN
%%%%%%%%%%%%%%%%%%%%%%%%

token([],[]):-!.

token(['{'|Expr],Compiled):-!,token_bracket(Expr,Compiled).

token([for,Var,in,Path,return|Rest],[string(for,Var,in,Path,return)|TRest]):-!,
			token(Rest,TRest).

token([let,Var,:=,Path,return|Rest],[string(let,Var,:=,Path,return)|TRest]):-!,
			token(Rest,TRest).

token([for,Var,in,Path,where|Rest],[string(for,Var,in,Path,where,Tree,return)|TRest]):-!,
			token_cond(Rest,nil,Tree,Final),
			token(Final,TRest).

token([let,Var,:=,Path,where|Rest],[string(let,Var,:=,Path,where,Tree,return)|TRest]):-!,
			token_cond(Rest,nil,Tree,Final),
			token(Final,TRest).

token(['}'|Rest],Compiled):-!,token(Rest,Compiled).

token([XPath|Rest],[string(XPath)|TRest]):-token(Rest,TRest).


%%%%%%%%%%%%%%%%%%%%%
% TOKEN-COND
%%%%%%%%%%%%%%%%%%%%%



token_cond([return|Final],Tree,Tree,Final):-!.

token_cond([and|Rest],HI,Tree,Final):-!,token_cond(Rest,tree(and,HI,_),Tree,Final).

token_cond([or|Rest],HI,Tree,Final):-!,token_cond(Rest,tree(or,HI,_),Tree,Final).

token_cond(['('|Rest],nil,Tree,Final):-!,token_cond_aux(Rest,nil,HD,End),token_cond(End,HD,Tree,Final).

token_cond(['('|Rest],tree(and,HI,_),Tree,Final):-!,token_cond_aux(Rest,nil,HD,End),token_cond(End,tree(and,HI,HD),Tree,Final).

token_cond(['('|Rest],tree(or,HI,_),Tree,Final):-!,token_cond_aux(Rest,nil,HD,End),token_cond(End,tree(or,HI,HD),Tree,Final).

token_cond([Exp|Rest],nil,Tree,Final):-!,build_term(Exp,TExp),token_cond(Rest,tree(TExp,nil,nil),Tree,Final).

token_cond([Exp|Rest],tree(and,HI,_),Tree,Final):-build_term(Exp,TExp),!,token_cond(Rest,tree(and,HI,tree(TExp,nil,nil)),Tree,Final).

token_cond([Exp|Rest],tree(or,HI,_),Tree,Final):-!,build_term(Exp,TExp),token_cond(Rest,tree(or,HI,tree(TExp,nil,nil)),Tree,Final).


token_cond_aux([')'|Rest],Tree,Tree,Rest):-!.

token_cond_aux([and|Rest],HI,Tree,End):-!,token_cond_aux(Rest,tree(and,HI,_),Tree,End).

token_cond_aux([or|Rest],HI,Tree,End):-!,token_cond_aux(Rest,tree(or,HI,_),Tree,End).

token_cond_aux(['('|Rest],nil,Tree,Final):-!,token_cond_aux(Rest,nil,HD,End),token_cond_aux(End,HD,Tree,Final).

token_cond_aux(['('|Rest],tree(and,HI,_),Tree,Final):-!,token_cond_aux(Rest,nil,HD,End),token_cond_aux(End,tree(and,HI,HD),Tree,Final).

token_cond_aux(['('|Rest],tree(or,HI,_),Tree,Final):-!,token_cond_aux(Rest,nil,HD,End),token_cond_aux(End,tree(or,HI,HD),Tree,Final).

token_cond_aux([Exp|Rest],nil,Tree,End):-!,build_term(Exp,TExp),token_cond_aux(Rest,tree(TExp,nil,nil),Tree,End).

token_cond_aux([Exp|Rest],tree(and,HI,_),Tree,End):-!,build_term(Exp,TExp),token_cond_aux(Rest,tree(and,HI,tree(TExp,nil,nil)),Tree,End).

token_cond_aux([Exp|Rest],tree(or,HI,_),Tree,End):-!,build_term(Exp,TExp),token_cond_aux(Rest,tree(or,HI,tree(TExp,nil,nil)),Tree,End).

%%%%%%%%%%%%%%%
% BUILD-TERM
%%%%%%%%%%%%%%

build_term(Exp,Term):- concat_atom([L,R],'=',Exp),!,build_term_list([L,R],[TL,TR]),Term=..[=,TL,TR].

build_term(Exp,Term):- concat_atom([L,R],'>',Exp),!,build_term_list([L,R],[TL,TR]),Term=..[>,TL,TR].

build_term(Exp,Term):- concat_atom([L,R],'<',Exp),!,build_term_list([L,R],[TL,TR]),Term=..[<,TL,TR].

build_term(Exp,Term):- concat_atom([L,R],'=<',Exp),!,build_term_list([L,R],[TL,TR]),Term=..[=<,TL,TR].

build_term(Exp,Term):- concat_atom([L,R],'>=',Exp),!,build_term_list([L,R],[TL,TR]),Term=..[>=,TL,TR].

build_term(Exp,Term):-concat_atom([F,S],'(',Exp),concat_atom([F2,_],')',S),concat_atom(Args,',',F2),
			build_term_list(Args,TArgs),string_to_atom(F,AF),Term=..[AF|TArgs].


build_term_list([],[]):-!.

build_term_list([L|RArgs],[varpath(Var,'')|TArgs]):-	
				concat_atom([Var],'/',L),
				concat_atom([_,_],'$',L),!,
				build_term_list(RArgs,TArgs).

build_term_list([L|RArgs],[varpath(Var,PathR)|TArgs]):-	
			concat_atom([Var|Path],'/',L),
			concat_atom([_,_],'$',L),!,
			concat_atom(Path,'/',PathR),
			build_term_list(RArgs,TArgs).

build_term_list([L|RArgs],[L|TArgs]):-!,build_term_list(RArgs,TArgs).


%%%%%%%%%%%%%%%%%%%%
% TOKEN-BRACKET
%%%%%%%%%%%%%%%%%%%%%


token_bracket([],[]):-!.


token_bracket([for,Var,in,Path,return|Rest],[value(for,Var,in,Path,return)|TRest]):-!,
			token(Rest,TRest).

token_bracket([let,Var,:=,Path,return|Rest],[value(let,Var,:=,Path,return)|TRest]):-!,
			token(Rest,TRest).

token_bracket([for,Var,in,Path,where|Rest],[value(for,Var,in,Path,where,Tree,return)|TRest]):-!, 
			token_cond(Rest,nil,Tree,Final),
			token(Final,TRest).

token_bracket([let,Var,:=,Path,where|Rest],[value(let,Var,:=,Path,where,Tree,return)|TRest]):-!,
			token_cond(Rest,nil,Tree,Final),
			token(Final,TRest).

token_bracket(['}'|Rest],Compiled):-!,token(Rest,Compiled).

token_bracket([XPath|Rest],[value(XPath)|TRest]):-token_bracket(Rest,TRest).				
				

%%%%%%%%%%%%%%%%%%%
% ANALYZE
%%%%%%%%%%%%%%%%%%%


 
analyze(_,_,[],[]):-!.
 
analyze(empty,empty,[element(Label,Att,Sub)|Elements],[element(Label,Att,ASub)|AElements]):-!,
			analyze(empty,empty,Sub,ASub), 
			analyze(empty,empty,Elements,AElements).
			 
			 

analyze(empty,empty,[value(for,Var,in,XPath,return),Pattern|Elements],
			[value(for,Var,in,XPath,return,empty,empty),APattern|AElements]):-!,  
			analyze(Var,XPath,[Pattern|Elements],[APattern|AElements]). 
			
		
analyze(empty,empty,[value(let,Var,:=,XPath,return),Pattern|Elements],
			[value(let,Var,:=,XPath,return,empty,empty),APattern|AElements]):-!,  
			analyze(Var,XPath,[Pattern|Elements],[APattern|AElements]).

analyze(empty,empty,[value(for,Var,in,XPath,where,Cond,return),Pattern|Elements],
			[value(for,Var,in,XPath,where,Cond,return,empty,empty),APattern|AElements]):-!,  
			analyze(Var,XPath,[Pattern|Elements],[APattern|AElements]).
			

analyze(empty,empty,[value(let,Var,:=,XPath,where,Cond,return),Pattern|Elements],
			[value(let,Var,:=,XPath,where,Cond,return,empty,empty),APattern|AElements]):-!,  
			analyze(Var,XPath,[Pattern|Elements],[APattern|AElements]).
			 	
   			
analyze(empty,empty,[value(El,empty,empty)|Elements],[value(El,empty,empty)|AElements]):-!,analyze(empty,empty,Elements,AElements).

analyze(empty,empty,[string(El,empty,empty)|Elements],[string(El,empty,empty)|AElements]):-!,analyze(empty,empty,Elements,AElements).

analyze(Var,XPath,[element(Label,Att,Sub)|Elements],[element(Label,Att,ASub)|AElements]):-!,
			analyze(Var,XPath,Sub,ASub),
			analyze(Var,XPath,Elements,AElements).

 		 

analyze(Var,XPath,[value(for,NVar,in,XPath2,return),Pattern|Elements],
		[value(for,NVar,in,XPath2,return,Var,XPath),APattern|AElements]):-!,
		 
		analyze(NVar,XPath2,[Pattern|Elements],[APattern|AElements]).

 

analyze(Var,XPath,[value(let,NVar,:=,XPath2,return),Pattern|Elements],
		[value(let,NVar,:=,XPath2,return,Var,XPath),APattern|AElements]):-!,
		 analyze(NVar,XPath2,[Pattern|Elements],[APattern|AElements]).

 

analyze(Var,XPath,[value(for,NVar,in,XPath2,where,Cond,return),Pattern|Elements],
		[value(for,NVar,in,XPath2,where,Cond,return,Var,XPath),APattern|AElements]):-!,

		 
		analyze(NVar,XPath2,[Pattern|Elements],[APattern|AElements]).

 

analyze(Var,XPath,[value(let,NVar,:=,XPath2,where,Cond,return),Pattern|Elements],
		[value(let,NVar,:=,XPath2,where,Cond,return,Var,XPath),APattern|AElements]):-!,
		 
		analyze(NVar,XPath2,[Pattern|Elements],[APattern|AElements]).

   			
analyze(Var,XPath,[value(El)|Elements],[value(El,Var,XPath)|AElements]):-
					 analyze(Var,XPath,Elements,AElements).

analyze(Var,XPath,[string(El)|Elements],[string(El,Var,XPath)|AElements]):-	 
					analyze(Var,XPath,Elements,AElements).

%%%%%%%%%%%%%%%%%%%%% 
% USED
%%%%%%%%%%%%%%%%%%%%%


used(Var,value(El)):-concat_atom([_,NVar],'$',Var),
	      concat_atom([_,NVar],'$',El),!.

used(Var,value(El)):-concat_atom([_,NVar],'$',Var),
		concat_atom([_,B],'$',El),concat_atom([NVar|_],'/',B).

%%%%%%%%
% CLEAN
%%%%%%%%


clean([],[]):-!.

clean([El|Relements],[AnEl|AnRelements]):-!,clean(El,AnEl),
			clean(Relements,AnRelements).

clean(element(Label,Att,Subelements),element(Label,Att,AnSubelements)):-!,
		clean(Subelements,AnSubelements).

clean(String,AnString2):-atomic(String),remove_r(String,AnString1),
					remove_b(AnString1,AnString2).


%%%%%%%%%%%%%%%%
% REMOVE_R
%%%%%%%%%%%%%%%%
					 

remove_r(String,String2):-concat_atom(List,'\n',String),     
			     concat_atom(List,' ',String2).
     
remove_b(String,String2):-concat_atom(List,' ',String), 
			remove_l_b(List,List2),
			concat_atom(List2,' ',String2).

%%%%%%%%%%%%%%%%%
% REMOVE_L_B
%%%%%%%%%%%%%%%%%%

remove_l_b([],[]):-!.

remove_l_b([''|R1],R2):-!,remove_l_b(R1,R2).

remove_l_b([' '|R1],R2):-!,remove_l_b(R1,R2).

remove_l_b([X|R1],[X|R2]):-remove_l_b(R1,R2).

                     

 