:- module(qaingest,
    [
        is_subclass_of/2,
        is_subproperty_of/2,
        is_resource_in_graph/2,
        load_file/2,
        load_file/3,
        load_file/4,
        entail/4,
        reconciled_to/3,
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
    entail(r,r,o,r),
    qldarch(o,o,o),
    reconciled_to(r,r,r),
    create_entity(r,r,r),
    is_subclass_of(r,r),
    is_subproperty_of(r,r),
    load_file(+,r),
    load_file(+,r,r),
    load_file(+,r,r,+),
    is_resource_in_graph(r,r),
    instance_of(r,r,r).

:- rdf_register_ns('qldarch', 'http://qldarch.net/ns/rdf/2012-06/terms#').
:- rdf_register_ns('qaat', 'http://qldarch.net/ns/omeka/2012-11/auxterms#').
:- rdf_register_ns('qavocab', 'http://qldarch.net/ns/skos/2013-02/vocab#').
:- rdf_register_ns('qaint', 'http://qldarch.net/ns/rdf/2013-08/internal#').
:- rdf_register_ns('qaomeka', 'http://qldarch.net/omeka/items/show/').
:- rdf_register_ns('qacatalog', 'http://qldarch.net/ns/rdf/2013-09/catalog#').
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
    ingest(Filename, Graph),
	graph_json(Graph, JSON),
	reply_json(JSON).

ingest(Filename, Graph) :-
    baseURI(BaseURI),
    format(atom(LoadGraph), '~a~a', [BaseURI, '/graph#load']),
    load_file(Filename, LoadGraph),
    format(atom(CleanGraph), '~a~a', [BaseURI, '/graph#clean']),
    cleanGraph(LoadGraph, CleanGraph),
    readGraph(CleanGraph, Graph).

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
        ['http://qldarch.net/ns/ingest/~a', P2D, P2D, '-', P2D, P2D, P2D, '.', P2D, 'Z'],
        TempURIFormat
    ),
    get_time(Timestamp),
    stamp_date_time(Timestamp, date(Year, Month, Day, Hour, Minute, Second, _, _, _), 'UTC'),
    atom_split(Second, '.', Sec, Sub),
    format(atom(BaseURI), TempURIFormat, [Year, Month, Day, Hour, Minute, Sec, Sub]).

load_file(Filename, Graph) :-
    load_file(Filename, Graph, _).

load_file(Filename, Graph, BaseURI) :-
    load_file(Filename, Graph, BaseURI, []).

load_file(Filename, Graph, BaseURI, Options) :-
    ( atom(BaseURI) ; rdf_equal(BaseURI, qaomeka:'') ),
    append(Options, [graph(Graph), base_uri(BaseURI)], ExtOptions),
    rdf_db:rdf_load(Filename, ExtOptions).

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

% NOT SURE ABOUT THIS ONE.
% Now we have Portraits and Biographies, can we so easily make the assumption
% all interviewees are Architects.
% { ?p a foaf:Person .
%   [ qldarch:interviewee ?p ] . 
%   } => { ?p a qldarch:Architect} .
%entail(Person, RdfType, Architect, G) :-
%    rdf_equal(RdfType, rdf:type),
%    rdf_equal(Architect, qldarch:'Architect'),
%    rdf(_, qldarch:interviewee, Person, G),
%    entail(Person, rdf:type, foaf:'Person').

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
    rdf_equal(HasTranscript, qldarch:hasTranscript),
    instance_of(Interview, qldarch:'Interview', G),
    rdf(Interview, qaat:transcriptItem, TranscriptItem, G),
    convert_item_to_URI(TranscriptItem, ItemURI).

%   { ?e qaat:reconciledTo ?building .
%     ?e qaat:projectNameOf ?s .
%   } => { ?s qldarch:depictsBuilding ?building } .
entail(S, DepictsBuilding, Building, G) :-
    rdf_equal(DepictsBuilding, qldarch:depictsBuilding),
    instance_of(PE, qaat:'ProjectName', G),
    reconciledTo(PE, Building, G),
    rdf(PE, qaat:projectNameOf, S).

convert_item_to_URI(Item, ItemURI) :-
    (   Item = literal(atom(V)) ;
        Item = literal(type(_,V))
    ), !,
    debug(qaconvert, 'Found atom item: ~w', Item),
    sub_atom(V, Before, _, After, 'admin/'),
    sub_atom(V, 0, Before, _, Prefix),
    sub_atom(V, _, After, 0, Suffix),
    atom_concat(Prefix, Suffix, ItemURI), !.

convert_item_to_URI(Item, _) :-
    debug(qaconvert, 'Found unconvertable item: ~w', Item), fail.

%   Shortcircuit reconcilation if we have memoized it previously.
reconciled_to(PseudoEntity, Entity, G) :-
    atom(PseudoEntity), atom(Entity),
    rdf(PseudoEntity, qaat:reconciledTo, Entity, G), !.

reconciled_to(PseudoEntity, Building, G) :-
    instance_of(PseudoEntity, qaat:'ProjectName', G),
    reconciled_to_building(PseudoEntity, Building, G).

reconciled_to(PseudoEntity, DrawingType, G) :-
    instance_of(PseudoEntity, qaat:'DrawingType', G),
    reconciled_to_drawing_type(PseudoEntity, DrawingType, G).

reconciled_to(PseudoEntity, Firm, G) :-
    instance_of(PseudoEntity, qaat:'Firm', G),
    reconciled_to_firm(PseudoEntity, Firm, G).

%   { ?e a qaat:ProjectName .
%     ?e qaat:label ?l .
%     ?building a qldarch:Structure .
%     ?building qldarch:label ?buildinglabel .
%     ?l str:equalIgnoringCase ?buildinglabel .
%   } => { ?e qaat:reconciledTo ?building } . 
reconciled_to_building(PseudoEntity, Building, G) :-
    atom(PseudoEntity),
    rdf(PseudoEntity, qaat:label, Label, G),
    label_value(Label, Value),
    entity_graph(EntityGraph, G),
    (   rdf(Building, qldarch:label, literal(exact(Value), _), EntityGraph) ->
        true ;
        (   create_entity(qldarch:'Structure', Building, G),
            rdf_assert(Building, qldarch:label, Label, G)
        )
    ), !.

%   { ?e a qaat:DrawingType .
%     ?e qaat:label ?label .
%     ?type a skos:Concept .
%     ?type skos:inScheme qavocab:DrawingType .
%     ?type qldarch:label ?typelabel .
%     ?label str:equalIgnoringCase ?typelabel .
%   } => { ?e qaat:reconciledTo ?type } . 
reconciled_to_drawing_type(PseudoEntity, DrawingType, G) :-
    atom(PseudoEntity),
    rdf(PseudoEntity, qaat:label, Label, G),
    label_value(Label, Value),
    qldarch(DrawingType, qldarch:label, literal(exact(Value), _)),
    qldarch(DrawingType, skos:inScheme, qavocab:'DrawingTypes'),
    instance_of(DrawingType, skos:'Concept', qldarch:''), !.

%   { ?e a qaat:Firm .
%     ?e qaat:label ?l .
%     ?firm a qldarch:Firm .
%     ?firm rdfs:label ?firmName .
%     ?l str:equalIgnoringCase ?firmName .
%   } => { ?e qaat:reconciledTo ?firm } .
reconciled_to_firm(PseudoEntity, Firm, G) :-
    atom(PseudoEntity),
    rdf(PseudoEntity, qaat:label, Label, G),
    label_value(Label, Value),
    entity_graph(EntityGraph, G),
    (   is_subproperty_of(Pred, rdfs:label),
        rdf(Firm, Pred, literal(exact(Value), _), EntityGraph) ->
        true ;
        (   create_entity(qldarch:'Firm', Firm, G),
            rdf_assert(Firm, qldarch:label, Label, G)
        )
    ), !.

create_entity(Type, Entity, G) :-
    nonvar(G) ->
        rdf_bnode(Entity),
        rdf_assert(Entity, rdf:type, Type, G)
    ; throw(error(instantiation_error, _)).
        
instance_of(S, C, G) :-
    (nonvar(S) ; nonvar(C)), !,
    rdf(S, rdf:type, Class, G),
    is_subclass_of(Class, C).

instance_of(S, C, G) :-
    is_resource_in_graph(S, G),
    instance_of(S, C, G).

is_subclass_of(Sub, Super) :-
    nonvar(Sub), nonvar(Super),
    Sub=Super, !.

is_subclass_of(Sub, Super) :-
    Sub=Super.

is_subclass_of(Sub, Super) :-
    (nonvar(Sub), nonvar(Super)) ->
    qldarch(Sub, rdfs:subClassOf, Super), !.

%   {?C rdfs:subClassOf ?D. ?D rdfs:subClassOf ?E} => {?C rdfs:subClassOf ?E}.
is_subclass_of(Sub, Super) :-
    (nonvar(Sub) ->
        qldarch(Sub, rdfs:subClassOf, Mid),
        is_subclass_of(Mid, Super), !
    ) ;
    (nonvar(Super) ->
        qldarch(Mid, rdfs:subClassOf, Super),
        is_subclass_of(Sub, Mid), !
    ) ;
    qldarch(Sub, rdfs:subClassOf, Mid),
    is_subclass_of(Mid, Super).

is_subproperty_of(Sub, Super) :-
    nonvar(Sub), nonvar(Super),
    Sub=Super, !.

is_subproperty_of(Sub, Super) :-
    Sub=Super.

is_subproperty_of(Sub, Super) :-
    (nonvar(Sub), nonvar(Super)) ->
    qldarch(Sub, rdfs:subPropertyOf, Super), !.

%   {?C rdfs:subClassOf ?D. ?D rdfs:subClassOf ?E} => {?C rdfs:subClassOf ?E}.
is_subproperty_of(Sub, Super) :-
    (nonvar(Sub) ->
        qldarch(Sub, rdfs:subPropertyOf, Mid),
        is_subproperty_of(Mid, Super), !
    ) ;
    (nonvar(Super) ->
        qldarch(Mid, rdfs:subPropertyOf, Super),
        is_subproperty_of(Sub, Mid), !
    ) ;
    qldarch(Sub, rdfs:subPropertyOf, Mid),
    is_subproperty_of(Mid, Super).

qldarch(S, P, O) :-
    rdf(S, P, O, 'http://qldarch.net/ns/rdf/2012-06/terms#').

is_resource_in_graph(R, G) :-
    nonvar(R), nonvar(G), !,
    (
        is_subject_in_graph(R, G) ;
        is_predicate_in_graph(R, G) ;
        is_object_in_graph(R, G)
    ), !.


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
            is_object_in_graph(R, G),
            add_nb_set(R, Set, New),
            New == true
        )
    ).

is_subject_in_graph(S, G) :-
    rdf(S, _, _, G),
    rdf_is_resource(S).

is_predicate_in_graph(P, G) :-
    rdf(_, P, _, G),
    rdf_is_resource(P).

is_object_in_graph(O, G) :-
    rdf(_, _, O, G),
    rdf_is_resource(O).

entity_graph(EntityGraph, Default) :-
    EntityGraph = Default.

entity_graph(EntityGraph, _) :-
    rdf_equal(Catalogue, qacatalog:''),
    rdf(_, qacatalog:hasEntityGraph, EntityGraph, Catalogue).
    
label_value(literal(atom(Label)), Value) :-
    Label = Value.

label_value(literal(type(_, Label)), Value) :-
    Label = Value.

label_value(literal(lang(_, Label)), Value) :-
    Label = Value.
