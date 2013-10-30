:- asserta(user:file_search_path(cliopatria, '.')).
:- asserta(user:file_search_path(api, cliopatria('api'))).
:- asserta(user:file_search_path(test, cliopatria('test/Qldarch'))).
:- asserta(user:file_search_path(data, test('data'))).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_compare)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(api(qaapi)).

init_ont :-
    rdf_reset_db,
    load_ont.
    
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

test(self_subclass) :-
    is_subclass_of(qldarch:'Entity', qldarch:'Entity').

test(direct_subclass) :-
    is_subclass_of(qldarch:'Entity', qldarch:'Evincible').

test(direct_superclass_query, [ set(X == [
            'http://qldarch.net/ns/rdf/2012-06/terms#Entity',
            'http://qldarch.net/ns/rdf/2012-06/terms#Evincible' ])]) :-
    is_subclass_of(qldarch:'Entity', X).

test(indirect_subclass) :-
    is_subclass_of(qldarch:'Transcript', qldarch:'Entity').

test(not_a_subclass, [fail]) :-
    is_subclass_of(qldarch:'Transcript', qldarch:'Relationship').

test(all_subclasses) :-
    findall(sc(X,Y), is_subclass_of(X, Y), List),
    forall(member(sc(X,Y), List), is_subclass_of(X, Y)).

:- end_tests(qldarch).

:- begin_tests(reconciliation, [
        setup(init_ont),
        cleanup(rdf_reset_db)
    ]).

test(test01_basic) :-
    rdf_equal(Test, qaint:test),
    rdf_equal(Expected, qaint:expected),
    rdf_equal(Out, qaint:out),
    load_file(data('basic.ttl'), Test),
    load_file(data('basic_out.ttl'), Expected),
    foreach(entail(S, P, O, Test), call(rdf_assert, S, P, O, Out)),
    (
        findall(rdf(S,P,O), rdf(S, P, O, Expected), ExpGraph),
        findall(rdf(S,P,O), rdf(S, P, O, Out), OutGraph),
        rdf_equal_graphs(ExpGraph, OutGraph, _) ->
            true ;
            output_graphs(Expected, Out)
    ).

test(test02_basic_with_bnode) :-
    rdf_equal(Test, qaint:test),
    rdf_equal(Expected, qaint:expected),
    rdf_equal(Out, qaint:out),
    load_file(data('basic_bnode.ttl'), Test),
    load_file(data('basic_bnode_out.ttl'), Expected),
    foreach(entail(S, P, O, Test), call(rdf_assert, S, P, O, Out)),
    (
        findall(rdf(S,P,O), rdf(S, P, O, Expected), ExpGraph),
        findall(rdf(S,P,O), rdf(S, P, O, Out), OutGraph),
        rdf_equal_graphs(ExpGraph, OutGraph, _) ->
            true ;
            output_graphs(Expected, Out)
    ).

output_graphs(Expected, Received) :-
    write('Expected:'),
    rdf_save_canonical_turtle(stream(user_output), [graph(Expected)]),
    write('Received:'),
    rdf_save_canonical_turtle(stream(user_output), [graph(Received)]),
    !, fail.

:- end_tests(reconciliation).

% Setup predicates
load_ont :-
    rdf_load(test('Qldarch.ttl'), [graph('http://qldarch.net/ns/rdf/2012-06/terms#')]).
%    rdf_load('test/Qldarch/Qldarch.ttl', [graph('http://qldarch.net/ns/rdf/2012-06/terms#')]).

% Used during testing, but not at the moment.

accept_cert(SSL, ProblemCert, AllCerts, FirstCert, Error) :-
    debug(cert, 'SSL Error: ~w', [Error]),
    debug(tracecert, 'SSL: ~w, PC: ~w, AC: ~w, FC: ~w, E: ~w',
        [SSL, ProblemCert, AllCerts, FirstCert, Error]),
    true.

