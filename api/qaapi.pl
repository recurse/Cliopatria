:- module(qaingest,
    [
        do/1
    ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_json)).
:- use_module(library(uri)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

:- http_handler(qaapi(ingest), load_omeka, []).

/* <module> api hooks for qldarch ingest

This module provides qldarch ingest support.

*/

:- rdf_meta
    assertNormalizedStmt(o,o,o,+),
    entail(r,r,o),
    qldarch(o,o,o),
    instance_of(r,r,r).

:- rdf_register_ns('qldarch', 'http://qldarch.net/ns/rdf/2012-06/terms#').
:- rdf_register_ns('qaat', 'http://qldarch.net/ns/omeka/2012-11/auxterms#').
:- rdf_register_ns('qavocab', 'http://qldarch.net/ns/skos/2013-02/vocab#').
:- rdf_register_ns('qaint', 'http://qldarch.net/ns/rdf/2013-08/internal#').
:- rdf_register_ns('rdf', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- rdf_register_ns('rdfs', 'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_register_ns('owl', 'http://www.w3.org/2002/07/owl#').
:- rdf_register_ns('xsd', 'http://www.w3.org/2001/XMLSchema#').
:- rdf_register_ns('dcterms', 'http://purl.org/dc/terms/').
:- rdf_register_ns('dcam', 'http://purl.org/dc/dcam/').
:- rdf_register_ns('foaf', 'http://xmlns.com/foaf/0.1/').
:- rdf_register_ns('skos', 'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_ns('geo', 'http://www.w3.org/2003/01/geo/wgs84_pos#').
:- rdf_register_ns('frbr', 'http://purl.org/vocab/frbr/core#').
:- rdf_register_ns('crm', 'http://www.cidoc-crm.org/cidoc-crm/').

%%	load_omeka(+Request)
%
%	HTTP  handler  that  ingests an omeka export.
%
%	@see	http://n2.talis.com/wiki/RDF_JSON_Specification describes
%		the used graph-serialization.
%	@see	http://n2.talis.com/wiki/Bounded_Descriptions_in_RDF for
%		a description of the various descriptions
%	@bug	Currently only supports =cbd=.

load_omeka(Request) :-
	http_parameters(Request,
			[ filename(Filename,
			    [ description('The file containing the omeka export')
			    ])
			]),
    do(Filename).

do(Filename) :-
    baseURI(BaseURI),
    format(atom(LoadGraph), '~a~a', [BaseURI, 'load']),
    load_file(Filename, LoadGraph),
    format(atom(CleanGraph), '~a~a', [BaseURI, 'clean']),
    cleanGraph(LoadGraph, CleanGraph),
    readGraph(CleanGraph, Graph),
	graph_json(Graph, JSON),
	reply_json(JSON).

%%  atom_split(+In, +Sep, -Head, -Tail)
%
%   Split the atom In 2 on the first occurance of Sep
atom_split(In, Sep, Head, Tail) :-
    atom_length(Sep, Length),
    sub_atom(In, B, Length, E, Sep),
    sub_atom(In, 0, B, _, Head),
    sub_atom(In, _, E, 0, Tail).

%%  baseURI(-BaseURI)
%
%   Obtain a base URI based on the current timestamp.
baseURI(BaseURI) :-
    P2D = '~|~`0t~a~2+', % Zero padded 2 digit integer
    atomic_list_concat(
        ['http://qldarch.net/ns/graph/temp/~a', P2D, P2D, '-', P2D, P2D, P2D, '.', P2D, 'Z#'],
        TempURIFormat
    ),
    get_time(Timestamp),
    stamp_date_time(Timestamp, date(Year, Month, Day, Hour, Minute, Second, _, _, _), 'UTC'),
    atom_split(Second, '.', Sec, Sub),
    format(atom(BaseURI), TempURIFormat, [Year, Month, Day, Hour, Minute, Sec, Sub]).

load_file(Filename, Graph) :-
    rdf_db:rdf_load(Filename, [graph(Graph)]).

cleanGraph(In, Clean) :-
    foreach(clean(S, P, O, In), call(assertNormalizedStmt, S, P, O, Clean)).

%%  assertNormalizeStmt(+S, +P, +O, +G)
%
%   Normalize any URIs in ths statment [S,P,O] and assert the result
%   in graph G. G is not normalized prior to assert.
assertNormalizedStmt(S, P, O, G) :-
    debug(qanorm, 'Normalizing Subject: ~w', [S]),
    normalizeNode(S, NS),
    debug(qanorm, 'Normalizing Predicate: ~w', [P]),
    normalizeNode(P, NP),
    debug(qanorm, 'Normalizing Object: ~w', [O]),
    normalizeNode(O, NO),
    debug(qanorm, 'Asserting [~w, ~w, ~w in ~w]', [NS, NP, NO, G]),
    rdf_db:rdf_assert(NS, NP, NO, G).

normalizeNode(Node, NormNode) :-
    (Node=literal(_) ; rdf_is_bnode(Node)) ->
        Node=NormNode ;
        iri_normalized(Node, NormNode).

readGraph(GraphURI, Graph) :-
    findall(rdf(S,P,O), entail(S, P, O, GraphURI), Graph).

clean(S, P, O, G) :-
    rdf(S, P, O, G),
    (
        instance_of(S, qaat:'PseudoEntity', G) ; 
        instance_of(S, qldarch:'Evincible', G)
    ).

entail(S, P, O, G) :-
    rdf(S, P, O, G).

%   {?P a owl:SymmetricProperty. ?S ?P ?O} => {?O ?P ?S}.
%entail(S, P, O, G) :-
%    has_type(P, 'owl:SymmetricProperty', G),
%    entail(O, P, S, G).

%   {?P a owl:TransitiveProperty. ?S ?P ?X. ?X ?P ?O.} => {?S ?P ?O}.
%entail(S, P, O, G) :-
%    has_type(P, 'owl:TransitiveProperty', G),
%    reachable(S, P, O, G).

%   {?C rdfs:subClassOf ?D. ?S a ?C} => {?S a ?D}.
entail(S, RdfType, C, G) :-
    rdf_equal(RdfType, rdf:type),
    instance_of(S, C, G),
    \+ rdf(S, rdf:type, C, G).

%   {
%      ?interview a qldarch:Interview ;
%          qaat:transcriptItem ?titem .
%      ( ?lit ?dt ) log:dtlit ?titem .
%      ( ?lit "http://qldarch.net/omeka/admin/items/show/(.*)" ) string:scrape ?itemNumber .
%      ( "http://qldarch.net/omeka/items/show/" ?itemNumber ) string:concatenation ?itemUriString .
%          ?itemUri log:uri ?itemUriString.
%          ?itemUri a qldarch:Transcript .
%    } => {
%      ?interview qldarch:hasTranscript ?itemUri .
%    } .
entail(Interview, HasTranscript, ItemURI, G) :-
    entail_G(Interview, HasTranscript, ItemURI, G).

entail_G(Interview, HasTranscript, ItemURI, G) :-
    rdf_equal(HasTranscript, qldarch:hasTranscript),
    instance_of(Interview, qldarch:'Interview', G),
    rdf(Interview, qaat:transcriptItem, TranscriptItem, G),
    convert_item_to_URI(TranscriptItem, ItemURI).

convert_item_to_URI(Item, ItemURI) :-
    Item = literal(atom(V)), !,
    debug(qaconvert, 'Found atom item: ~w', Item),
    sub_atom(V, Before, _, After, 'admin/'),
    sub_atom(V, 0, Before, _, Prefix),
    sub_atom(V, _, After, 0, Suffix),
    atom_concat(Prefix, Suffix, ItemURI), !.

convert_item_to_URI(Item, ItemURI) :-
    Item = literal(type(_,V)), !,
    debug(qaconvert, 'Found typed item: ~w', Item),
    sub_atom(V, Before, _, After, 'admin/'),
    sub_atom(V, 0, Before, _, Prefix),
    sub_atom(V, _, After, 0, Suffix),
    atom_concat(Prefix, Suffix, ItemURI), !.

convert_item_to_URI(Item, _) :-
    debug(qaconvert, 'Found item: ~w', Item), fail.

instance_of(S, C, G) :-
    (nonvar(S) ; nonvar(C)), !,
    rdf(S, rdf:type, Class, G),
    is_subclass_of(Class, C).

instance_of(S, C, G) :-
    is_resource_in_graph(S, G),
    instance_of(S, C, G).


%    {
%        ?interview a qldarch:Interview ;
%            qaat:transcriptItem ?titem .
%        ( ?lit ?dt ) log:dtlit ?titem .
%        ?lit string:matches "http://qldarch.net/omeka/items/show/(.*)" .
%        ?itemUri log:uri ?lit.
%        ?itemUri a qldarch:Transcript .
%    } => {
%        ?interview qldarch:hasTranscript ?itemUri .
%    } .

is_subclass_of(Sub, Super) :-
    Sub=Super.

is_subclass_of(Sub, Super) :-
    (nonvar(Sub), nonvar(Super)) ->
    qldarch(Sub, rdfs:subClassOf, Super), !.

%   {?C rdfs:subClassOf ?D. ?D rdfs:subClassOf ?E} => {?C rdfs:subClassOf ?E}.
is_subclass_of(Sub, Super) :-
    (nonvar(Sub) ->
        qldarch(Sub, rdfs:subClassOf, Mid),
        is_subclass_of(Mid, Super)
    ) ;
    (nonvar(Super) ->
        qldarch(Mid, rdfs:subClassOf, Super),
        is_subclass_of(Sub, Mid)
    ) ;
    qldarch(Sub, rdfs:subClassOf, Mid),
    is_subclass_of(Mid, Super).

qldarch(S, P, O) :-
    rdf(S, P, O, 'http://qldarch.net/ns/rdf/2012-06/terms#').

is_resource_in_graph(R, G) :-
    empty_nb_set(Set),
    (
        (
            is_subject_in_graph(R, G),
            add_nb_set(R, Set, New),
            New == true
        ) ;
        (
            is_predicate_in_graph(R, G),
            add_nb_set(R, Set, New),
            New == true
        ) ;
        (
            is_predicate_in_graph(R, G),
            add_nb_set(R, Set, New),
            New == true
        )
    ).

is_subject_in_graph(S, G) :-
    rdf(S, _, _, G).

is_predicate_in_graph(P, G) :-
    rdf(_, P, _, G).

is_object_in_graph(O, G) :-
    rdf(_, _, O, G).
