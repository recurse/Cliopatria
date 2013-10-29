:- asserta(user:file_search_path(cliopatria, '.')).
:- asserta(user:file_search_path(test, cliopatria('test'))).
:- asserta(user:file_search_path(api, cliopatria('api'))).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(api(qaapi)).

init_ont :-
    rdf_reset_db,
    rdf_load('test/Qldarch/Qldarch.ttl', [graph('http://qldarch.net/ns/rdf/2012-06/terms#')]).
    
:- begin_tests(qldarch, [
        setup(init_ont),
        cleanup(rdf_reset_db)
    ]).

ns_owlontology(R) :-
    rdf_equal(R, owl:'Ontology').
ns_rdftype(R) :-
    rdf_equal(R, rdf:type).
ns_qldarch(R) :-
    rdf_equal(R, qldarch:'').
ns_auxterms(R) :-
    rdf_equal(R, qaat:'').
ns_qaint(R) :-
    rdf_equal(R, qaint:'').

test(loaded) :-
    ns_owlontology(Ontology),
    ns_rdftype(A),
    ns_qldarch(Qldarch),
    ns_auxterms(Auxterms),
    ns_qaint(Qaint),
    assertion(rdf(Qldarch, A, Ontology, Qldarch)),
    assertion(rdf(Qaint, A, Ontology, Qldarch)),
    assertion(rdf(Auxterms, A, Ontology, Qldarch)).

test(a_subj_is_in_graph) :-
    is_resource_in_graph(qldarch:label, qldarch:'').

test(a_pred_is_in_graph) :-
    is_resource_in_graph(rdfs:subClassOf, qldarch:'').

test(a_obj_is_in_graph) :-
    is_resource_in_graph(rdfs:'Class', qldarch:'').

test(not_in_graph, [fail]) :-
    is_resource_in_graph(qldarch:notAPredicate, qldarch:'').

test(direct_subclass) :-
    is_subclass_of(qldarch:'Entity', qldarch:'Evincible').

test(direct_superclass_query, [ set(X == [
            'http://qldarch.net/ns/rdf/2012-06/terms#Entity',
            'http://qldarch.net/ns/rdf/2012-06/terms#Evincible' ])]) :-
    is_subclass_of(qldarch:'Entity', X).

:- end_tests(qldarch).


% Used during testing, but not at the moment.

accept_cert(SSL, ProblemCert, AllCerts, FirstCert, Error) :-
    debug(cert, 'SSL Error: ~w', [Error]),
    debug(tracecert, 'SSL: ~w, PC: ~w, AC: ~w, FC: ~w, E: ~w',
        [SSL, ProblemCert, AllCerts, FirstCert, Error]),
    true.

