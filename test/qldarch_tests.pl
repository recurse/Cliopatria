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

test(indirect_subproperty) :-
    is_subproperty_of(qldarch:firmName, rdfs:label).

test(direct_subproperty_query, [ set(X == [
            'http://www.w3.org/2000/01/rdf-schema#label',
            'http://qldarch.net/ns/rdf/2012-06/terms#label',
            'http://qldarch.net/ns/rdf/2012-06/terms#plural',
            'http://qldarch.net/ns/rdf/2012-06/terms#firmName' ])]) :-
    is_subproperty_of(X, rdfs:label).

:- end_tests(qldarch).

e_files('basic.ttl', 'basic_out.ttl').
e_files('basic_bnode.ttl', 'basic_bnode_out.ttl').
e_files('has_transcript.ttl', 'has_transcript_out.ttl').
e_files('depicts_building.ttl', 'depicts_building_out.ttl').

:- begin_tests(entailment, [
        setup(init_ont),
        cleanup(rdf_reset_db)
    ]).

% Entailment test files
test(entailment, [
        setup(init_ont),
        cleanup(rdf_reset_db),
        forall(external_test_case(e_files, InFile, OutFile)) ]) :-
    assertion(atom(InFile)),
    assertion(atom(OutFile)),
    rdf_equal(Test, qaint:test),
    rdf_equal(Expected, qaint:expected),
    rdf_equal(Out, qaint:out),
    load_file(data(InFile), Test, _, [silent(true)]),
    load_file(data(OutFile), Expected, _, [silent(true)]),
    foreach(entail(S, P, O, Test), call(rdf_assert, S, P, O, Out)),
    foreach(rdf(S, P, O, Test), call(rdf_assert, S, P, O, Out)),
    (
        findall(rdf(S,P,O), rdf(S, P, O, Expected), ExpGraph),
        findall(rdf(S,P,O), rdf(S, P, O, Out), OutGraph),
        rdf_equal_graphs(ExpGraph, OutGraph, _) ->
            true ;
            output_graphs(Expected, Out)
    ).

:- end_tests(entailment).

init_rec :-
    init_ont,
    load_file(data('dummy_entities1.ttl'), qaint:'entities1', _, [silent(true)]),
    load_file(data('dummy_entities2.ttl'), qaint:'entities2', _, [silent(true)]),
    rdf_equal(Catalogue, qacatalog:''),
    rdf_assert(qaint:'testera', qacatalog:hasEntityGraph, qaint:'entities1', Catalogue),
    rdf_assert(qaint:'testerb', qacatalog:hasEntityGraph, qaint:'entities2', Catalogue).

r_files('rec_building.ttl', 'rec_building_out.ttl').
r_files('rec_ext_building.ttl', 'rec_ext_building_out.ttl').
r_files('rec_drawingtype.ttl', 'rec_drawingtype_out.ttl').
r_files('rec_firm.ttl', 'rec_firm_out.ttl').
r_files('rec_ext_firm.ttl', 'rec_ext_firm_out.ttl').
r_files('rec_person.ttl', 'rec_person_out.ttl').
r_files('rec_ext_person.ttl', 'rec_ext_person_out.ttl').
r_files('rec_ext_typology.ttl', 'rec_ext_typology_out.ttl').

r_fail_files('rec_ext_drawingtype.ttl', _).

:- begin_tests(reconciliation, [
        cleanup(rdf_reset_db),
        setup(init_ont)
    ]).

% Reconciliation test files
test(reconciliation, [
        setup(init_rec),
        cleanup(rdf_reset_db),
        forall(external_test_case(r_files, InFile, OutFile)) ]) :-
    assertion(atom(InFile)),
    assertion(atom(OutFile)),
    rdf_equal(Test, qaint:test),
    rdf_equal(Expected, qaint:expected),
    rdf_equal(Out, qaint:out),
    rdf_equal(ReconciledTo, qaat:reconciledTo),
    load_file(data(InFile), Test, _, [silent(true)]),
    load_file(data(OutFile), Expected, _, [silent(true)]),
    foreach(reconciled_to(PE, B, Test), call(rdf_assert, PE, ReconciledTo, B, Out)),
    foreach(rdf(S, P, O, Test), call(rdf_assert, S, P, O, Out)),
    (
        findall(rdf(S,P,O), rdf(S, P, O, Expected), ExpGraph),
        findall(rdf(S,P,O), rdf(S, P, O, Out), OutGraph),
        rdf_equal_graphs(ExpGraph, OutGraph, _) ->
            true ;
            output_graphs(Expected, Out)
    ).

test(reconciliation_fail, [
        setup(init_rec),
        cleanup(rdf_reset_db),
        forall(external_test_case(r_fail_files, InFile, OutFile)) ]) :-
    assertion(atom(InFile)),
    rdf_equal(Test, qaint:test),
    load_file(data(InFile), Test, _, [silent(true)]),
    findall(r(PE,B), reconciled_to(PE, B, Test), []).

:- end_tests(reconciliation).

external_test_case(Filepred, X, Y) :-
    (
        nb_current(Filepred, Files),
        nonvar(Files)
    ) -> (
        member(X, Files),
        call(Filepred, X, Y)
    ) ;
    call(Filepred, X, Y).


% Setup predicates
load_ont :-
    load_file(test('Qldarch.ttl'), 'http://qldarch.net/ns/rdf/2012-06/terms#', 'http://qldarch.net/ns/rdf/2012-06/terms#', [silent(true)]).

% Utility to dump erroneous graphs to user_output
output_graphs(Expected, Received) :-
    writef('Expected:\n'),
    rdf_save_turtle(stream(user_output), [graph(Expected)]),
    writef('Received:\n'),
    rdf_save_turtle(stream(user_output), [graph(Received)]),
    !, fail.

% Used during testing, but not at the moment.

accept_cert(SSL, ProblemCert, AllCerts, FirstCert, Error) :-
    debug(cert, 'SSL Error: ~w', [Error]),
    debug(tracecert, 'SSL: ~w, PC: ~w, AC: ~w, FC: ~w, E: ~w',
        [SSL, ProblemCert, AllCerts, FirstCert, Error]),
    true.

