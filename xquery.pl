%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XQUERY implementation by means of SWI-Prolog
% Jesus Almendros (April 2009)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  XQUERY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 

xquery(Query,Out):- 
                        ex_query(Query,OutputDoc),
                        write_xml(Out,OutputDoc),
			write('Executed Successfully'),nl,!.

xquery(_,_):-write('Execution Error!'),nl,!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EX-QUERY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ex_query(Query,OutputDoc):-load_query(Query,N), 
			load_ontology,
                        querying(OutputDoc,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD-ONTOLOGY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_ontology:-ontology(Doc),load(Doc),fail.

load_ontology.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD-QUERY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_query(Query,N):-   retractall(identifier(_)),
                        retractall(identifier_var(_)),
                        retractall(querying(_,_)),
                        retractall(variable(_,_,_)),
                        retractall(flwr(_,_)),
                        retractall(path_var(_,_,_)),
			retractall(condition(_)),
			retractall(condition2(_)),
			retractall(constraint(_,_,_)),
			retractall(defined(_)),
			retractall(ontology(_)),
			retractall(last(_)),
                        update(N),
			newcomp(Query,Comp),
                        translate(Comp,N),
			retrieve_conditions.
