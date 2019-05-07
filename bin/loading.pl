%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XQUERY implementation by means of SWI-Prolog
% Jesus Almendros (April 2009)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DYNAMIC PREDICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dynamic_pred:-
      dynamic (querying)/2,variable/3,identifier/1,identifier_var/1,
                path_var/3,flwr/2,
		condition/1,constraint/3,defined/1,
		ontology/1,last/1.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   

load:-
      op(1132,fx,[for]),
      op(1131,xfy,[in]),
      op(1130,fx,[let]),
      op(1133,xfy,[:=]),
      op(1128,xfy,[where]),
      op(1127,xf,[return]).
                
     
    


:-load. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD-XML
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_xml(Doc,List):-load_structure(Doc,List,[dialect(sgml)]).
 
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WRITE-XML
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_xml(File,Term):-open(File,write,F),xml_write(F,Term,[]),close(F).


