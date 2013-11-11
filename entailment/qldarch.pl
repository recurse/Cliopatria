/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2010, University of Amsterdam,
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(qldarch_entailment,
	  [
	  ]).
:- use_module(rdfql(rdfql_runtime)).	% runtime tests
:- use_module(library('semweb/rdf_db'),
	      [ rdf_global_id/2,
		rdf_subject/1,
		rdf_equal/2,
		(rdf_meta)/1,
		op(_,_,_)
	      ]).

:- use_module(api(qaapi)).

/** <module> RDFS-Lite entailment

The function of an entailment module is  to provide an implementation of
rdf/3 that extends basic triple-lookup using the entailment rules of the
semantic web sub language of RDF.

This entailment module does the part  of   RDFS  that can be implemented
efficiently  using  backward  chaining.  Notably    it   does  *not*  do
type-reasoning on the basis of domains/ranges of properties. It does:

	* Use the property hierarchy
	* Define the serql special relations
	* Handle rdfs:subClassOf as transitive
	* Handle rdfs:subPropertyOf as transitive
	* Handle rdf:type using subProperties and rdfs:subClassOf
*/

:- rdf_meta
    rdf(o,o,o),
	rdf(o,o,o,o).

:- public
	rdf/3,
	rdf/4.

/*
 * Define rdf/4 to allow capturing of catalogue
 */

rdf(S, P, O, G) :-
    debug(qldarch, 'Entailing 1: ~W ~W ~W in ~W', [S, [quoted(true)], P, [quoted(true)], O, [quoted(true)], G, [quoted(true)]]),
    rdf_equal(Cat, qacatalog:''),
    debug(qldarch, 'Cat(~w) = ~w', [Cat, G]),
    ( G = Cat ; G =.. [:, Cat, _] ),
    debug(qldarch, 'Querying catalogue: ~w', [G]),
    catalogue(S, P, O).

rdf(S, P, O, G) :-
    debug(qldarch, 'Entailing 2: ~W ~W ~W in ~W', [S, [quoted(true)], P, [quoted(true)], O, [quoted(true)], G, [quoted(true)]]),
    (
        ontology(G) ;
        ( G =.. [:, Inner, _], ontology(Inner) )
    ),
    debug(qldarch, 'Querying ontology: ~w', [G]),
    tbox(S, P, O).

%    rdf_equal(qacatalog:'', S),
%    debug(qldarch, 'proved subject: ~w~n', [S]),
%    debug(qldarch, 'entailing current graph~n', []),
%    rdf_equal(P, qacatalog:currentContentGraph),
%    debug(qldarch, 'proved predicate: ~w~n', [P]),
%    content_graph(O),
%    debug(qldarch, 'proved object: ~w~n', [O]).

%rdf(S, P, O, G) :-
%    debug(qldarch, 'entailing graph ~w~n', [G]),
%	rdf_db:rdf(S, P, O, G).

rdf(S, P, O) :-
    ( content_graph(G) ; entity_graph(G) ),
    debug(qldarch, 'Querying type ~w ~w ~w in ~w', [S, P, O, G]),
    abox(S, P, O, G).

/*
 * Utility methods.
 */

catalogue(S, P, O) :-
    (
        rdf_equal(S, qacatalog:'') ->
        has_graphs(P, O)
    ).

has_graphs(P, O) :-
    (
        rdf_equal(P, qacatalog:'hasEntityGraph'),
        entity_graph(O)
    ) ;
    (
        rdf_equal(P, qacatalog:'hasContentGraph'),
        content_graph(O)
    ).

tbox(S, P, O) :-
    (
        rdf_equal(P, rdfs:subClassOf),
        is_subclass_of(S, O),
        nonvar(S),nonvar(O) % Ensure grounded
    ) ;
    (
        rdf_equal(P, rdfs:subPropertyOf),
        is_subproperty_of(S, O),
        nonvar(S),nonvar(O) % Ensure grounded
    ) ;
    (
        ontology(G),
        abox(S, P, O, G)
    ).

abox(S, P, O, G) :-
    (
        rdf_equal(P, rdf:type),
        qaingest:instance_of(S, O, G)
    ) ;
    (
        rdf_db:rdf(S, P, O, G)
    ).

		 /*******************************
		 *	       REGISTER		*
		 *******************************/

:- multifile
	cliopatria:entailment/2.

cliopatria:entailment(qldarch, qldarch_entailment).
