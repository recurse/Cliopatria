:- module(qaingest,
    [
        is_subclass_of/2,
        is_subproperty_of/2,
        is_resource_in_graph/2,
        load_file/2,
        load_file/3,
        load_file/4,
        entail/4,
        entailment/2,
        reconciled_to/3,
        atom_split/4,
        do/1
    ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_json)).
:- use_module(library(uri)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(gensym)).
:- use_module(library(dialect/ifprolog)).

:- http_handler(qaapi(ingest), load_omeka, []).

/* <module> api hooks for qldarch ingest

This module provides qldarch ingest support.

*/

:- rdf_meta
    assertNormalizedStmt(o,o,o,+),
    entail(r,r,?,r),
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

%%  atom_split(+In, +Sep, -Head, -Tail) is det
%
%   Split the atom In 2 on the first occurance of Sep
atom_split_first(In, Sep, Head, Tail) :-
    atom_split(In, Sep, Head, Tail), !.

%%  atom_split_first(+In, +Sep, -Head, -Tail) is nondet
%
%   Split the atom In 2 on the occurances of Sep
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
    atom_split_first(Second, '.', Sec, Sub),
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

%   FIXME: TESTS REQUIRED
entailment(Graph, OutGraph) :-
    rdf_bnode(TempGraph),
    rdf_create_graph(TempGraph),
    foreach(entail(S, P, O, Graph), call(rdf_assert, S, P, O, TempGraph)),
    foreach(rdf(S, P, O, Graph), call(rdf_assert, S, P, O, TempGraph)),
    ( setof(S, is_blank_resource_in_graph(S, TempGraph), Subjects) ; Subjects = []),
    foreach(member(X, Subjects), call(ground_bnode, X, TempGraph)), !,
    ( var(OutGraph) -> rdf_bnode(OutGraph) ; rdf_unload_graph(OutGraph) ),
    rdf_create_graph(OutGraph),
    foreach((
        rdf(S, P, O, TempGraph),
        instance_of(S, qldarch:'Evincible', TempGraph),
        \+ instance_of(P, qaat:'AuxProperty', qldarch:'')),
        call(rdf_assert, S, P, O, OutGraph)), !,
    rdf_unload_graph(TempGraph).

ground_bnode(S, G) :-
    findall(C, instance_of(S, C, G), Classes),
    member(C, Classes) *-> (
        \+ ( qldarch(Sub, rdfs:subClassOf, C), member(Sub, Classes) ),
        (
            (   ifprolog:index(C, '#', _) ->
                atom_split(C, '#', Fragments),
                last(Fragments, Fragment)
            ) ; (
                atom_split(C, '/', Fragments),
                last(Fragments, Fragment)
            )
        ), !,
        atomic_list_concat(['http://qldarch.net/rdf/2012-12/resources/', Fragment, '#'], ResourceBase),
        gensym(ResourceBase, ResourceURI),
        \+ rdf_resource(ResourceURI),
        foreach(rdf(S,P,O), call(rdf_update, S, P, O, subject(ResourceURI))),
        foreach(rdf(S1,P,S), call(rdf_update, S1, P, S, object(ResourceURI)))
    ) ;
    true.

% FIXME: TESTS REQUIRED
entail(S, P, O, G) :-
    rdf(S, P, O, G) ->
    true ;
    (   owl_inverse_of(P, IP),
        rdf(O, IP, S, G)
    ).

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
    rdf_equal(HasTranscript, qldarch:hasTranscript),
    rdf(Interview, qaat:transcriptItem, TranscriptItem, G),
    instance_of(Interview, qldarch:'Interview', G),
    convert_item_to_URI(TranscriptItem, ItemURI).

entail(DigitalObject, QAPred, Entity, G) :-
    % Moved body to avoid split predicate warning.
    classification(DigitalObject, QAPred, Entity, G).

% NOTE: If we ever interview anyone who isn't an Architect we can no longer make
%   this assumption.
%   { ?p a foaf:Person .
%     [ qldarch:interviewee ?p ] . 
%   } => { ?p a qldarch:Architect} .
%   FIXME: TESTS REQUIRED
entail(Person, RdfType, Architect, G) :-
    rdf_equal(RdfType, rdf:type),
    rdf_equal(Architect, qldarch:'Architect'),
    entail(_DigitalObject, qldarch:interviewee, Person, G),
    instance_of(Person, foaf:'Person', G).

%   { ?do a qldarch:DigitalObject .
%     ?do qldarch:associatedFirm ?firm .
%     ?do qldarch:depictsBuilding ?building .
%   } => { ?building qldarch:associatedFirm ?firm } .
%entail(Building, AssociatedFirm, Firm, G) :-
%    rdf_equal(AssociatedFirm, qldarch:associatedFirm),
%    entail(DigitalObject, qldarch:depictsBuilding, Building, G),
%    entail(DigitalObject, AssociatedFirm, Firm, G).

%   { ?do a qldarch:DigitalObject .
%     ?do qldarch:location ?location .
%     ?do qldarch:depictsBuilding ?building .
%   } => { ?building qldarch:location ?location } .
%   Note: ?do qldarch:location ?location is exported directly from omeka via D2RQ
%entail(Building, QALocation, Location, G) :-
%    rdf_equal(QALocation, qldarch:location),
%    rdf(DigitalObject, QALocation, Location, G),
%    instance_of(DigitalObject, qldarch:'DigitalObject', G),
%    entail(DigitalObject, qldarch:depictsBuilding, Building, G),
%    instance_of(Building, qldarch:'Structure').

%   { ?do a qldarch:DigitalObject .
%     ?do qaat:latitude ?lat .
%     ?do qldarch:depictsBuilding ?building .
%   } => { ?building geo:lat ?lat } .
%entail(Building, GeoLat, Latitude, G) :-
%    rdf_equal(GeoLat, geo:lat),
%    rdf_equal(QAATLat, qaat:latitude),
%    entail_latlong(QAATLat, Building, Latitude, G).

%   { ?do a qldarch:DigitalObject .
%     ?do qaat:longitude ?long .
%     ?do qldarch:depictsBuilding ?building .
%   } => { ?building geo:long ?long } .
%entail(Building, GeoLong, Longitude, G) :-
%    rdf_equal(GeoLong, geo:long),
%    rdf_equal(QAATLong, qaat:longitude),
%    entail_latlong(QAATLong, Building, Longitude, G).

% TODO: Portraits of people have changed since protyping in N3, so they will need
%   to be done separately.

%   { ?firm a qldarch:Firm .
%     ?e qaat:reconciledTo ?firm .
%     ?e qaat:contemporaryTo ?d .
%     ?st a qaat:SubjectType .
%     ?st qaat:label "Firm"^^xsd:string .
%     ?st qaat:subjectType ?d .
%     ?d a qldarch:Portrait .
%     ?d qaat:preferredImage "Yes"^^xsd:string .
%   } => { ?firm qldarch:preferredImage ?d } .
%entail(Agent, PreferredImage, DigitalObject, G) :-
%    rdf_equal(PreferredImage, qldarch:preferredImage),
%    rdf(PE, qaat:contemporaryTo, DigitalObject),
%    rdf(DigitalObject, qaat:preferredImage, literal('Yes')),
%    instance_of(DigitalObject, qldarch:'Portrait', G),
%    rdf(SubjectType, qaat:label, literal('Firm'), G),
%    rdf(SubjectType, qaat:subjectType, DigitalObject, G),
%    reconciled_to(PE, Agent, G),
%    instanceof(Agent, qldarch:'Firm', G).

%  Combine the following two rules to avoid constructing the relationship object
%  We may want to do that in the future, so leave the option open, but for now
%  just entail the relationships directly.
%
%  { ?rclass rdfs:subClassOf qldarch:Relationship ;
%      qldarch:impliesRelationship ?impliedPredicate .
%    [ a ?rclass ;
%      qldarch:subject ?s ;
%      qldarch:object ?o ] .
%  } => {
%    ?s ?impliedPredicate ?o
%  } .
%  
%  { ?s ?p ?o .
%    ?p a owl:ObjectProperty ;
%       qldarch:entailsRelationship ?rclass .
%    ?rclass rdfs:subClassOf qldarch:Relationship .
%  } => {
%    [ a ?rclass ;
%      qldarch:subject ?s ;
%      qldarch:object ?o ;
%      qldarch:evidence [ a qldarch:Evidence ;
%        rdf:subject ?s ;
%        rdf:predicate ?p ;
%        rdf:object ?o ] ] .
%  } . 
%entail(S, ImpliedPred, O, G) :-
%    qldarch(Pred, qldarch:entailsRelationship, RelClass),
%    instance_of(Pred, owl:'ObjectProperty', G),
%    is_subclass_of(RelClass, qldarch:'Relationship'),
%    is_subclass_of(ImplyingRelClass, RelClass),
%    qldarch(ImplyingRelClass, qldarch:impliesRelationship, ImpliedPred),
%    rdf(S, Pred, O, G).

%   { ?e qaat:reconciledTo ?person .
%     ?do a qldarch:Interview .
%       ?e qaat:interviewerIn ?do .
%   } => { ?do qldarch:interviewer ?person } .
%   { ?e qaat:reconciledTo ?person .
%     ?do a qldarch:Interview .
%       ?e qaat:intervieweeIn ?do .
%   } => { ?do qldarch:interviewer ?person } .
%   { ?e qaat:reconciledTo ?person .
%     ?do a qldarch:DigitalObject .
%     ?e qaat:created ?do .
%   } => { ?do dcterms:creator ?person } .
%   { ?e qaat:reconciledTo ?building .
%     ?e qaat:projectNameOf ?s .
%   } => { ?s qldarch:depictsBuilding ?building } .
%   { ?e qaat:reconciledTo ?person .
%     ?do a qldarch:Transcript .
%     ?e qaat:transcriberOf ?do .
%   } => { ?do qldarch:transcriber ?person } .
%   { ?e qaat:reconciledTo ?type .
%     ?do a qldarch:DigitalObject .
%     ?e qaat:buildingTypologyOf ?do .
%     ?do qldarch:depictsBuilding ?building .
%   } => { ?building qldarch:buildingTypology ?type } .
%   { ?e a qaat:BuildingTypology .
%     ?e qaat:reconciledTo ?type .
%     ?e qaat:definiteMapIcon ?dicon .
%     ?type a qldarch:BuildingTypology .
%   } => { ?type qldarch:definiteMapIcon ?dicon } .
%   { ?e a qaat:BuildingTypology .
%     ?e qaat:reconciledTo ?type .
%     ?e qaat:indefiniteMapIcon ?dicon .
%     ?type a qldarch:BuildingTypology .
%   } => { ?type qldarch:indefiniteMapIcon ?dicon } .
%   { ?e qaat:reconciledTo ?firm .
%     ?e qaat:contemporaryTo ?s .
%   } => { ?s qldarch:associatedFirm ?firm } .
classification(DigitalObject, QAPred, Entity, G) :-
    predicate_pair(QAPred, QAATPred),
    rdf(PE, QAATPred, DigitalObject, G),
    reconciled_to(PE, Entity, G). % Note: reconciled_to does instance_of check.
    % Consider adding a domain check to the entailment.

% FIXME: TESTS REQUIRED
predicate_pair(QAPred, QAATPred) :-
    rdf_equal(QAPred, qldarch:interviewee),
    rdf_equal(QAATPred, qaat:intervieweeIn).

predicate_pair(QAPred, QAATPred) :-
    rdf_equal(QAPred, qldarch:interviewer),
    rdf_equal(QAATPred, qaat:interviewerIn).

predicate_pair(QAPred, QAATPred) :-
    rdf_equal(QAPred, qldarch:transcriber),
    rdf_equal(QAATPred, qaat:transcriberOf).

predicate_pair(QAPred, QAATPred) :-
    rdf_equal(QAPred, dcterms:creator),
    rdf_equal(QAATPred, qaat:created).

predicate_pair(QAPred, QAATPred) :-
    rdf_equal(QAPred, qldarch:depictsBuilding),
    rdf_equal(QAATPred, qaat:projectNameOf).

predicate_pair(QAPred, QAATPred) :-
    rdf_equal(QAPred, qldarch:buildingTypology),
    rdf_equal(QAATPred, qaat:buildingTypologyOf).

predicate_pair(QAPred, QAATPred) :-
    rdf_equal(QAPred, qldarch:definiteMapIcon),
    rdf_equal(QAATPred, qaat:definiteMapIcon).

predicate_pair(QAPred, QAATPred) :-
    rdf_equal(QAPred, qldarch:indefiniteMapIcon),
    rdf_equal(QAATPred, qaat:indefiniteMapIcon).

predicate_pair(QAPred, QAATPred) :-
    rdf_equal(QAPred, qldarch:associatedFirm),
    rdf_equal(QAATPred, qaat:contemporaryTo).

convert_item_to_URI(Item, ItemURI) :-
    (   Item = literal(atom(V)) ;
        Item = literal(type(_,V))
    ), !,
    debug(qaconvert, 'Found atom item: ~w', Item),
    (
        sub_atom(V, Before, _, After, 'admin/') ->
        (   sub_atom(V, 0, Before, _, Prefix),
            sub_atom(V, _, After, 0, Suffix),
            atom_concat(Prefix, Suffix, ItemURI)
        ) ;
        V = ItemURI
    ), !.

convert_item_to_URI(Item, _) :-
    debug(qaconvert, 'Found unconvertable item: ~w', Item), fail.

%   Shortcircuit reconcilation if we have memoized it previously.
%   Do we want to do this, if so, memoize each reconciled_to method.
%reconciled_to(PseudoEntity, Entity, G) :-
%    atom(PseudoEntity), atom(Entity),
%    rdf(PseudoEntity, qaat:reconciledTo, Entity, G), !.

reconciled_to(PseudoEntity, Entity, G) :-
    instance_of(PseudoEntity, qaat:'ProjectName', G),
    reconciled_to_building(PseudoEntity, Entity, G).

reconciled_to(PseudoEntity, Entity, G) :-
    instance_of(PseudoEntity, qaat:'DrawingType', G),
    reconciled_to_drawing_type(PseudoEntity, Entity, G).

reconciled_to(PseudoEntity, Entity, G) :-
    instance_of(PseudoEntity, qaat:'Firm', G),
    reconciled_to_firm(PseudoEntity, Entity, G).

reconciled_to(PseudoEntity, Entity, G) :-
    instance_of(PseudoEntity, qaat:'Person', G),
    reconciled_to_person(PseudoEntity, Entity, G).

reconciled_to(PseudoEntity, Entity, G) :-
    instance_of(PseudoEntity, qaat:'BuildingTypology', G),
    reconciled_to_typology(PseudoEntity, Entity, G).

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
    (   
        (   entity_graph(EntityGraph, G),
            (   
                rdf(Building, qldarch:label, literal(exact(Value), _), EntityGraph),
                instance_of(Building, qldarch:'Structure', EntityGraph)
            )
        ) -> true ;
        (   var(Building) ->
            (   create_entity(qldarch:'Structure', Building, G),
                rdf_assert(Building, qldarch:label, literal(type(xsd:string, Value)), G)
            ) ;
            fail
        )
    ),
    !.

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
    (   
        (   entity_graph(EntityGraph, G),
            is_subproperty_of(Pred, rdfs:label),
            rdf(Firm, Pred, literal(exact(Value), _FirmName), EntityGraph),
            instance_of(Firm, qldarch:'Firm', EntityGraph)
        ) -> true ;
        (   var(Firm) ->
            (   create_entity(qldarch:'Firm', Firm, G),
                rdf_assert(Firm, qldarch:firmName, literal(type(xsd:string, Value)), G)
            ) ;
            fail
        )
    ), !.

%   { ?e a qaat:Person .
%     ?e qaat:label ?l .
%     ?person a foaf:Person .
%     ?person foaf:firstName ?fn .
%     ?person foaf:lastName ?ln .
%     ?l str:containsIgnoringCase ?fn .
%     ?l str:containsIgnoringCase ?ln .
%   } => { ?e qaat:reconciledTo ?person } .
reconciled_to_person(PseudoEntity, Person, G) :-
    atom(PseudoEntity),
    rdf(PseudoEntity, qaat:label, Label, G),
    label_value(Label, Value),
    (
        (   entity_graph(EntityGraph, G),
            match_person_names(Value, Person, EntityGraph)
        ) -> true ;
        (   var(Person) ->
            (   create_entity(foaf:'Person', Person, G),
                % Assume the first space splits the full-name into first- and last-names
                atom_split_first(Value, ' ', FirstName, LastName),
                rdf_assert(Person, foaf:firstName, literal(type(xsd:string, FirstName)), G),
                rdf_assert(Person, foaf:lastName, literal(type(xsd:string, LastName)), G)
            ) ;
            fail
        )
    ), !.

match_person_names(FullName, Person, EntityGraph) :-
    % Try various splits against foaf:firstName
    atom_split(FullName, ' ', FNPrefix, _),
    rdf(Person, foaf:firstName, literal(prefix(FNPrefix), FirstName), EntityGraph),
    label_value(literal(FirstName), FNValue),
    % The suffix of the fullname from the first-name + space is the last-name
    atom_length(FNValue, FNLength),
    FNSLength is FNLength + 1,
    sub_atom(FullName, FNSLength, _, 0, LastName),
    %    atom_split(FullName, FNSPrefix, _, LastName),
    rdf(Person, foaf:lastName, literal(exact(LastName), _), EntityGraph),
    % Domain and Range should ensure this, but double check anyway as Firms and People
    % sometimes share names
    instance_of(Person, foaf:'Person', EntityGraph),
    !.

%   { ?e a qaat:BuildingTypology .
%     ?e qaat:label ?label .
%     ?type a qldarch:BuildingTypology .
%     ?type qldarch:label ?typelabel .
%     ?label str:equalIgnoringCase ?typelabel .
%   } => { ?e qaat:reconciledTo ?type } .
reconciled_to_typology(PseudoEntity, Typology, G) :-
    atom(PseudoEntity),
    rdf(PseudoEntity, qaat:label, Label, G),
    label_value(Label, Value),
    (   
        (   entity_graph(EntityGraph, G),
            rdf(Typology, qldarch:label, literal(exact(Value), _), EntityGraph),
            instance_of(Typology, qldarch:'BuildingTypology', EntityGraph)
        ) -> true ;
        (   var(Typology) ->
            (   create_entity(qldarch:'BuildingTypology', Typology, G),
                rdf_assert(Typology, qldarch:label, Label, G)
            ) ;
            fail
        )
    ), !.

entail_latlong(QAATPred, Building, Latitude, G) :-
    nonvar(Building), !,
    entail(DigitalObject, qldarch:depictsBuilding, Building, G),
    instance_of(Building, qldarch:'Structure', G),
    instance_of(DigitalObject, qldarch:'DigitalObject', G),
    rdf(DigitalObject, QAATPred, LatitudeOut, G),
    as_decimal(LatitudeOut, Latitude).

entail_latlong(QAATPred, Building, Latitude, G) :-
    var(Building), !,
    rdf(DigitalObject, QAATPred, LatitudeOut, G),
    as_decimal(LatitudeOut, Latitude),
    instance_of(DigitalObject, qldarch:'DigitalObject', G),
    entail(DigitalObject, qldarch:depictsBuilding, Building, G),
    instance_of(Building, qldarch:'Structure', G).

create_entity(Type, Entity, G) :-
    ( nonvar(G), var(Entity) ) ->
        rdf_bnode(Entity),
        rdf_assert(Entity, rdf:type, Type, G)
    ; throw(error(instantiation_error, _)).
        
instance_of(S, C, G) :-
    nonvar(S), var(C),
    empty_nb_set(Set), !,
    rdf(S, rdf:type, Class, G),
    is_subclass_of(Class, C),
    add_nb_set(C, Set, New),
    New == true.

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
    (nonvar(Sub) *->
        qldarch(Sub, rdfs:subPropertyOf, Mid),
        is_subproperty_of(Mid, Super)
    ) ;
    (nonvar(Super) *->
        qldarch(Mid, rdfs:subPropertyOf, Super),
        is_subproperty_of(Sub, Mid)
    ) ;
    (
        qldarch(Sub, rdfs:subPropertyOf, Mid),
        is_subproperty_of(Mid, Super)
    ).

owl_inverse_of(Property, InverseProperty) :-
    qldarch(Property, owl:inverseOf, InverseProperty) ->
    true ;
    qldarch(InverseProperty, owl:inverseOf, Property).

domain(Predicate, Domain) :-
    qldarch(Predicate, rdfs:domain, Domain).
    
range(Predicate, Range) :-
    qldarch(Predicate, rdfs:range, Range).

qldarch(S, P, O) :-
    rdf(S, P, O, 'http://qldarch.net/ns/rdf/2012-06/terms#').

ontology('http://qldarch.net/ns/rdf/2012-06/terms#').

ontology(S, P, O) :-
    ontology(G),
    rdf(S, P, O, G).

is_blank_resource_in_graph(R, G) :-
    empty_nb_set(Set),
    is_resource_in_graph(R, G),
    rdf_is_bnode(R),
    add_nb_set(R, Set, New),
    New == true.

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

as_decimal(type(_, Raw), Out) :-
    !, atom_number(Raw, _),
    Out = type(xsd:decimal, Raw).

as_decimal(lang(_,_), _) :-
    !, fail.

as_decimal(literal(In), Out) :-
    as_decimal(In, Out).

as_decimal(In, Out) :-
    atom_number(In, _),
    Out = type(xsd:decimal, In).

entity_graph(EntityGraph) :-
    rdf_equal(Catalogue, qacatalog:''),
    rdf(_, qacatalog:hasEntityGraph, EntityGraph, Catalogue).

entity_graph(EntityGraph, Default) :-
    EntityGraph = Default.

entity_graph(EntityGraph, _Default) :-
    entity_graph(EntityGraph).
    
label_value(literal(type(_, Label)), Value) :-
    Label = Value.

label_value(literal(lang(_, Label)), Value) :-
    Label = Value.

label_value(literal(Label), Value) :-
    atom(Label),
    Label = Value.

