/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam,
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

:- module(cpack,
	  [ cpack_install/1,		% +NameOrURL
	    cpack_upgrade/0,
	    cpack_upgrade/1,		% +Name
	    cpack_configure/1,		% +Name
	    cpack_add_dir/2,		% +ConfigEnabled, +Directory
	    cpack_create/3,		% +Name, +Title, +Options
	    cpack_register/3,		% +Name, +Dir, +Options
	    current_cpack/1,		% ?Name
	    cpack_property/2		% ?Name, ?Property
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(http/http_open)).
:- use_module(library(uri)).
:- use_module(library(lists)).
:- use_module(library(git)).
:- use_module(library(setup)).
:- use_module(library(conf_d)).
:- use_module(library(filesex)).
:- use_module(library(settings)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(option)).

/** <module> The ClioPatria package manager

*/

:- setting(cpack:package_directory, atom, cpack,
	   'Directory where packages are downloaded').
:- setting(cpack:server, atom, 'http://cliopatria.swi-prolog.org/',
	   'Address of the fallback server').

:- rdf_register_ns(cpack, 'http://cliopatria.swi-prolog.org/schema/cpack#').
:- rdf_register_ns(foaf,  'http://xmlns.com/foaf/0.1/').

%%	cpack_install(+Install) is det.
%
%	Install package by name or URL. The URL  of a CPACK can be found
%	on  the  web-page  of  the  package.   If  a  *name*  is  given,
%	cpack_install/1 queries the configured servers  for the package.
%	For example:
%
%	  ==
%	  ?- cpack_install('EDM').
%	  % Trying CPACK server at http://cliopatria.swi-prolog.org/cpack/EDM ...
%	  % Installing package EDM:
%	  %    EDM -- View Europeana Data Model
%	  % Initialized empty Git repository in /home/jan/tmp/test/cpack/EDM/.git/
%	  %     Installing EDM.pl ...
%	  % /home/jan/tmp/test/config-enabled/010-packs.pl compiled into conf_packs 0.00 sec, 1,480 bytes
%	  % Added the following config files:
%	  %     /home/jan/tmp/test/config-enabled/010-packs.pl
%	  %     /home/jan/tmp/test/config-enabled/EDM.pl
%	  %   library(count) compiled into count 0.02 sec, 13,280 bytes
%	  %  skin(EDM) compiled into edm 0.02 sec, 52,984 bytes
%	  % /home/jan/tmp/test/config-enabled/EDM.pl compiled into conf_EDM 0.02 sec, 56,112 bytes
%	  true.
%	  ==
%
%	@see	http://cliopatria.swi-prolog.org is the central package
%		repository.
%	@param	Install is either a URL on the server that returns the
%		installation parameter (this is shown in the info box
%		of the package), or the name of a package or a list of
%		package names.

cpack_install(URL) :-
	\+ is_list(URL),
	uri_is_global(URL), !,
	cpack_package_data(URL, Terms),
	cpack_install_terms(Terms).
cpack_install(Name) :-
	cpack_load_profile,
	(   rdf_has(_, cpack:servers, List),
	    rdfs_member(Server, List)
	;   setting(cpack:server, Server)
	),
	ensure_slash(Server, ServerDir),
	pack_data_url(ServerDir, Name, URL),
	print_message(informational, cpack(probe(URL))),
	catch(cpack_package_data(URL, Terms), E, true),
	(   var(E)
	->  !, cpack_install_terms(Terms)
	;   print_message(error, E),
	    fail
	).

pack_data_url(ServerDir, Names, URL) :-
	is_list(Names), !,
	maplist(pack_param, Names, Params),
	uri_query_components(Query, Params),
	atomic_list_concat([ServerDir, cpack, /?, Query], URL).
pack_data_url(ServerDir, Name, URL) :-
	atomic_list_concat([ServerDir, cpack, /, Name], URL).

pack_param(Name, p(Name)).


ensure_slash(Server, ServerDir) :-
	(   sub_atom(Server, _, _, 0, /)
	->  ServerDir = Server
	;   atom_concat(Server, /, ServerDir)
	).

cpack_package_data(URL, Terms) :-
	setup_call_cleanup(http_open(URL, In, []),
			   read_stream_to_terms(In, Terms),
			   close(In)).

read_stream_to_terms(In, Terms) :-
	read_term(In, Term0, []),
	read_stream_to_terms(Term0, In, Terms).

read_stream_to_terms(end_of_file, _, []) :- !.
read_stream_to_terms(Term, In, [Term|T]) :-
	read_term(In, Term1, []),
	read_stream_to_terms(Term1, In, T).


%%	cpack_install_terms(+Terms) is det.
%
%	Install from the server reply.

cpack_install_terms(Terms) :-
	(   Terms = [cpack(Name, Packages)]
	->  print_message(informational, cpack(requires(Name, Packages))),
	    maplist(package_status, Packages, Status),
	    maplist(download_package, Status),
	    maplist(configure_package, Packages)
	;   Terms = [no_cpack(Name)]
	->  existence_error(cpack, Name)
	;   Terms = [error(Error)]
	->  throw(Error)
	;   domain_error(cpack_reply, Terms)
	).

%%	package_status(+CpackTerm, -Status)
%
%	@param	Status is a term cpack(Package, State), where State is
%		one of =no_change=, upgrade(Old, New) or =new=.

package_status(cpack(Package, Options),
	       cpack(Package, Options, Status)) :-
	cpack_package_dir(Package, Dir, false),
	directory_file_path(Dir, '.git', GitRepo),
	(   exists_directory(GitRepo)
	->  option(branch(Branch), Options, master),
	    atom_concat('origin/', Branch, Commit),
	    git_describe(OldVersion, [directory(Dir)]),
	    git([fetch], [ directory(Dir) ]),
	    git_describe(NewVersion, [directory(Dir),commit(Commit)]),
	    (	OldVersion == NewVersion
	    ->	Status = no_change(OldVersion)
	    ;	Status = upgrade(OldVersion, NewVersion)
	    )
	;   Status = new
	).

download_package(cpack(Package, _, no_change(OldVersion))) :- !,
	print_message(informational, cpack(no_change(Package, OldVersion))).
download_package(cpack(Package, Options, upgrade(Old, New))) :- !,
	print_message(informational, cpack(upgrade(Package, Old, New))),
	option(branch(Branch), Options, master),
	cpack_package_dir(Package, Dir, false),
	atom_concat('origin/', Branch, Commit),
	git([merge, Commit],
	    [ directory(Dir)
	    ]).
download_package(cpack(Package, Options, new)) :-
	option(pack_repository(Repository), Options),
	print_message(informational, cpack(download(Package, Repository))),
	cpack_package_dir(Package, Dir, false),
	cpack_download(Repository, Dir).

configure_package(cpack(Package, Options)) :-
	cpack_module_options(Options, ModuleOptions),
	cpack_configure(Package, ModuleOptions).

cpack_module_options([], []).
cpack_module_options([H0|T0], [H|T]) :-
	cpack_module_option(H0, H), !,
	cpack_module_options(T0, T).
cpack_module_options([_|T0], T) :-
	cpack_module_options(T0, T).

cpack_module_option(url(URL), home_url(URL)).


%%	cpack_download(+Repository, +TargetDir)
%
%	Download Repository to Dir.
%
%	@tbd	Branches, trust

cpack_download(_Package, Dir) :-
	directory_file_path(Dir, '.git', GitRepo),
	exists_directory(GitRepo), !,
	git([pull],
	    [ directory(Dir)
	    ]).				% Too simplistic
cpack_download(git(GitURL, Options), Dir) :-
	findall(O, git_clone_option(O, Options), OL),
	append([ [clone, GitURL, Dir]
	       | OL
	       ], GitOptions),
	git(GitOptions, []).

git_clone_option(['-b', Branch], Options) :-
	option(branch(Branch), Options).

%%	cpack_upgrade
%
%	Upgrade all packages to the server versions.

cpack_upgrade :-
	findall(Name, current_cpack(Name), Names),
	cpack_install(Names).

%%	cpack_upgrade(Package)
%
%	Upgrade Package.  This is the same as cpack_install(Package).

cpack_upgrade(Name) :-
	cpack_install(Name).

%%	cpack_configure(+Name) is det.
%
%	Just configure a package.

cpack_configure(Name) :-
	cpack_configure(Name, []).

cpack_configure(Name, Options) :-
	cpack_package_dir(Name, Dir, false),  !,
	exists_directory(Dir),
	(   conf_d_enabled(ConfigEnabled)
	->  cpack_add_dir(ConfigEnabled, Dir, Options)
	;   existence_error(directory, 'config-enabled')
	).
cpack_configure(Name, _) :-
	existence_error(cpack, Name).


%%	cpack_add_dir(+ConfigEnable, +PackageDir)
%
%	Install package located in directory PackageDir.
%
%	@tbd	Register version-tracking with register_git_module/3.

cpack_add_dir(ConfigEnable, Dir) :-
	cpack_add_dir(ConfigEnable, Dir, []).

cpack_add_dir(ConfigEnable, Dir, Options) :-
	directory_file_path(ConfigEnable, '010-packs.pl', PacksFile),
	directory_file_path(Dir, 'config-available', ConfigAvailable),
	file_base_name(Dir, Pack),
	add_pack_to_search_path(PacksFile, Pack, Dir, Modified, Options),
	setup_default_config(ConfigEnable, ConfigAvailable, []),
	(   Modified == true		% Update paths first!
	->  load_files(PacksFile, [if(true)])
	;   true
	),
	conf_d_reload.


add_pack_to_search_path(PackFile, Pack, Dir, Modified, Options) :-
	exists_file(PackFile), !,
	read_file_to_terms(PackFile, Terms, []),
	New = (:- cpack_register(Pack, Dir, Options)),
	(   memberchk(New, Terms)
	->  Modified = false
	;   Old = (:- cpack_register(Pack, _, _)),
	    memberchk(Old, Terms)
	->  selectchk(Old, Terms, New, Terms2),
	    open(PackFile, write, Out),
	    write_search_path_header(Out),
	    Templ = cpack_register(_, _, _),
	    forall(member((:-Templ), Terms2),
		   format(Out, ':- ~q.~n', [Templ])),
	    close(Out)
	;   open(PackFile, append, Out),
	    extend_search_path(Out, Pack, Dir, Options),
	    close(Out),
	    Modified = true
	).
add_pack_to_search_path(PackFile, Pack, Dir, true, Options) :-
	open(PackFile, write, Out),
	write_search_path_header(Out),
	extend_search_path(Out, Pack, Dir, Options),
	close(Out).

write_search_path_header(Out) :-
	format(Out, '/* Generated file~n', []),
	format(Out, '   This file defines the search-path for added packs~n', []),
	format(Out, '*/~n~n', []),
	format(Out, ':- module(conf_packs, []).~n~n', []),
	format(Out, ':- multifile user:file_search_path/2.~n', []),
	format(Out, ':- dynamic user:file_search_path/2.~n', []),
	format(Out, ':- multifile cpack:registered_cpack/2.~n~n', []).

extend_search_path(Out, Pack, Dir, Options) :-
	format(Out, ':- ~q.~n', [cpack_register(Pack, Dir, Options)]).


		 /*******************************
		 *	   REGISTRATION		*
		 *******************************/

%%	cpack_register(+PackName, +Dir, +Options)
%
%	Attach a CPACK to the search paths

cpack_register(PackName, Dir, Options) :-
	throw(error(context_error(nodirective,
				  cpack_register(PackName, Dir, Options)), _)).


user:term_expansion((:-cpack_register(PackName, Dir, Options)), Clauses) :-
	Term =.. [PackName,'.'],
	Clauses = [ user:file_search_path(PackName, Dir),
		    user:file_search_path(cpacks, Term),
		    cpack:registered_cpack(PackName, Dir, Options)
		  ].

:- multifile
	registered_cpack/3.

%%	current_cpack(-Name) is nondet.
%
%	True when Name is the name of a registered package.

current_cpack(Name) :-
	registered_cpack(Name, _, _).

%%	cpack_property(Name, Property) is nondet.
%
%	True when Property is a property of the CPACK Name.  Defined
%	properties are:
%
%	  * directory(Dir)

cpack_property(Name, Property) :-
	property_cpack(Property, Name).

property_cpack(directory(Dir), Name) :-
	registered_cpack(Name, LocalDir, _),
	absolute_file_name(LocalDir, Dir).
property_cpack(Option, Name) :-
	registered_cpack(Name, _, Options),
	member(Option, Options).

%%	prolog_version:git_module_hook(?Name, ?Directory, ?Options) is
%%	nondet.
%
%	Make packages available for the   version management implemented
%	by library(version).

:- multifile
	prolog_version:git_module_hook/3.

prolog_version:git_module_hook(Name, Directory, Options) :-
	registered_cpack(Name, LocalDir, Options),
	absolute_file_name(LocalDir, Directory).


		 /*******************************
		 *	CREATE NEW PACKAGES	*
		 *******************************/

%%	cpack_create(+Name, +Title, +Options) is det.
%
%	Create a new package.  Options include
%
%	  * type(Type)
%	  Label of a subclass of cpack:Package.  Default is =package=
%	  * title(Title)
%	  Title for the package.  Should be a short line.
%	  * foafname(FoafName)
%	  foaf:name to put into the default template
%	  * foafmbox(Email)
%	  foaf:mbox to put into the default template
%
%	Default options are  extracted  from   the  cpack:Profile  named
%	=default=
%
%	@tbd	Allow selection profile, auto-loading of profile, etc.

cpack_create(Name, Title, Options) :-
	cpack_load_schema,
	cpack_load_profile,
	option(type(Type), Options, package),
	package_class_id(Type, PkgClass),
	default_bindings(default, Name, DefaultBindings),
	merge_options(Options,
		      [ name(Name),
			title(Title),
			pkgclass(PkgClass)
		      | DefaultBindings
		      ], Vars),
	cpack_package_dir(Name, Dir, true),
	forall(cpack_dir(SubDir, Type),
	       make_cpack_dir(Dir, SubDir)),
	forall(cpack_template(In, Out),
	       install_template_file(In, Out, Vars)),
	git([init], [directory(Dir)]),
	git([add, '.'], [directory(Dir)]),
	git([commit, '-m', 'Installed template'], [directory(Dir)]),
	git([tag, epoch], [directory(Dir)]),
	git_setup_push(Dir, Vars).

package_class_id(Label, TurtleID) :-
	package_class(Label, Class),
	rdf_global_id(Prefix:Name, Class),
	atomic_list_concat([Prefix, :, Name], TurtleID).

package_class(Label, Class) :-
	rdf_has(Class, rdfs:label, literal(Label)),
	rdfs_subclass_of(Class, cpack:'Package'), !.
package_class(Label, _) :-
	domain_error(package_class, Label).

default_bindings(Profile, Name, Bindings) :-
	findall(B, default_binding(Profile, Name, B), Bindings).

default_binding(ProfileName, Name, B) :-
	rdf_has(Profile, cpack:name, literal(ProfileName)),
	(   rdf_has(Profile, cpack:defaultAuthor, Author),
	    (   rdf_has(Author, foaf:name, literal(AuthorName)),
		B = foafname(AuthorName)
	    ;   rdf_has(Author, foaf:mbox, literal(MBOX)),
		B = foafmbox(MBOX)
	    )
	;   rdf_has(Profile, cpack:fetchRepositoryTemplate, literal(GitTempl)),
	    substitute(GitTempl, '@CPACK@', Name, GitRepo),
	    B = fetchrepository(GitRepo)
	;   rdf_has(Profile, cpack:pushRepositoryTemplate, literal(GitTempl)),
	    substitute(GitTempl, '@CPACK@', Name, GitRepo),
	    B = pushrepository(GitRepo)
	).

%%	git_setup_push(+Dir, +Vars) is det.
%
%	Set an origin for the newly  created repository. This also tries
%	to  setup  a  bare  repository  at   the  remote  machine  using
%	git_create_origin/2.

git_setup_push(Dir, Vars) :-
	option(pushrepository(PushURL), Vars), !,
	option(title(Title), Vars, 'ClioPatria CPACK'),
	git([remote, add, origin, PushURL], [directory(Dir)]),
	directory_file_path(Dir, '.git/config', Config),
	setup_call_cleanup(open(Config, append, Out),
			   format(Out, '[branch "master"]\n\
			   		\tremote = origin\n\
					\tmerge = refs/heads/master\n', []),
			   close(Out)),
	catch(git_create_origin(PushURL, Title), E,
	      print_message(error, E)).
git_setup_push(_,_).

%%	git_create_origin(+PushURL, +Title) is det.
%
%	Try to create the repository origin. As the user has setup push,
%	we hope he setup SSH appropriately. Note that this only works if
%	the remote user has a real shell and not a git-shell.

git_create_origin(PushURL, Title) :-
	uri_components(PushURL, Components),
	uri_data(scheme, Components, Scheme),
	(   Scheme == ssh
	->  uri_data(authority, Components, Authority)
	;   Authority = Scheme
	),
	uri_data(path, Components, Path),
	file_directory_name(Path, Parent),
	file_base_name(Path, Repo),
	format(atom(Command),
	       'cd "~w" && mkdir "~w" && cd "~w" && \
	       git init --bare && echo "~w" > description && \
	       touch git-daemon-export-ok',
	       [Parent, Repo, Repo, Title]),
	process_create(path(ssh), [ Authority, Command ], []).


%%	make_cpack_dir(+BaseDir, +CPACKDir) is det.
%
%	Setup th directory structure for a new package.

make_cpack_dir(Dir, SubDir) :-
	directory_file_path(Dir, SubDir, New),
	(   exists_directory(New)
	->  true
	;   make_directory_path(New),
	    print_message(informational, cpack(create_directory(New)))
	).

install_template_file(In, Out, Vars) :-
	option(name(Name), Vars),
	absolute_file_name(In, InFile, [access(read)]),
	substitute(Out, '@NAME@', Name, OutFile),
	cpack_package_dir(Name, Dir, true),
	directory_file_path(Dir, OutFile, OutPath),
	copy_file_with_vars(InFile, OutPath, Vars),
	print_message(informational, cpack(installed_template(OutFile))).

substitute(In, From, To, Out) :-
	sub_atom(In, B, _, A, From), !,
	sub_atom(In, 0, B, _, Start),
	sub_atom(In, _, A, 0, End),
	atomic_list_concat([Start, To, End], Out).
substitute(In, _, _, In).

cpack_dir('rdf', _).
cpack_dir('rdf/cpack', _).
cpack_dir('config-available', _).
cpack_dir('entailment', _).
cpack_dir('applications', _).
cpack_dir('api', _).
cpack_dir('components', _).
cpack_dir('skin', _).
cpack_dir('lib', _).
cpack_dir('web', _).
cpack_dir('web/js', _).
cpack_dir('web/css', _).
cpack_dir('web/html', _).

cpack_template(library('cpack/config-available.pl.in'),
	       'config-available/@NAME@.pl').
cpack_template(library('cpack/DEFAULTS.in'),
	       'config-available/DEFAULTS').
cpack_template(library('cpack/pack.ttl.in'),
	       'rdf/cpack/@NAME@.ttl').


		 /*******************************
		 *	      PROFILE		*
		 *******************************/

%%	cpack_load_profile is det.
%
%	Try to load the profile from user_profile('.cpack.ttl').
%
%	@tbd Prompt for a default profile (notably fill in the servers).

cpack_load_profile :-
	absolute_file_name(user_profile('.cpack.ttl'), Path,
			   [ access(read),
			     file_errors(fail)
			   ]), !,
	rdf_load(Path).
cpack_load_profile.


%%	cpack_load_schema
%
%	Ensure the CPACK schema data is loaded.

cpack_load_schema :-
	rdf_attach_library(rdf(cpack)),
	rdf_load_library(cpack).



		 /*******************************
		 *	       UTIL		*
		 *******************************/

%%	cpack_package_dir(+PackageName, -Dir, +Create)
%
%	Installation directory for Package

cpack_package_dir(Name, Dir, Create) :-
	setting(cpack:package_directory, PackageDir),
	directory_file_path(PackageDir, Name, Dir),
	(   (   Create == false
	    ;	exists_directory(Dir)
	    )
	->  true
	;   make_directory(Dir)
	).

:- multifile
	prolog:message//1,
	prolog:error_message//1.

prolog:message(cpack(Message)) -->
	message(Message).

message(create_directory(New)) -->
	[ 'Created directory ~w'-[New] ].
message(installed_template(File)) -->
	[ 'Installed template ~w'-[File] ].
message(requires(Name, Packages)) -->
	(   { is_list(Name) }
	->  [ 'Packages ~w require the following packages:'-[Name] ]
	;   [ 'Package ~w requires the following packages:'-[Name] ]
	),
	sub_packages(Packages),
	[ nl, 'Querying package status ...'-[] ].
message(no_change(Name, Version)) -->
	[ '   ~w: ~t~30|no change (~w)'-[Name, Version] ].
message(upgrade(Name, Old, New)) -->
	[ '   ~w: ~t~30|upgrading (~w..~w) ...'-[Name, Old, New] ].
message(download(Name, git(Url, _))) -->
	[ '   ~w: ~t~30|downloading from ~w ...'-[Name, Url] ].
message(probe(URL)) -->
	[ 'Trying CPACK server at ~w ...'-[URL] ].

sub_packages([]) --> [].
sub_packages([H|T]) --> sub_package(H), sub_packages(T).

sub_package(cpack(Name, Options)) -->
	{ option(title(Title), Options) }, !,
	[ nl, '   ~w: ~t~30|~w'-[Name, Title] ].
sub_package(cpack(Name, _)) -->
	[ nl, '   ~w: ~t~30|~w'-[Name] ].

prolog:error_message(cpack_error(Error)) -->
	cpack_error(Error).

cpack_error(not_satisfied(Pack, Reasons)) -->
	[ 'Package not satisfied: ~p'-[Pack] ],
	not_satisfied_list(Reasons).

not_satisfied_list([]) --> [].
not_satisfied_list([H|T]) --> not_satisfied(H), not_satisfied_list(T).

not_satisfied(no_token(Token)) -->
	[ nl, '   Explicit requirement not found: ~w'-[Token] ].
not_satisfied(file(File, Problems)) -->
	[ nl, '   File ~p'-[File] ],
	file_problems(Problems).

file_problems([]) --> [].
file_problems([H|T]) --> file_problem(H), file_problems(T).

file_problem(predicate_not_found(PI)) -->
	[ nl, '        Predicate not resolved: ~w'-[PI] ].


		 /*******************************
		 *	  COMPATIBILITY		*
		 *******************************/

:- if(\+current_predicate(selectchk/4)).

select(X, XList, Y, YList) :-
	select_(XList, X, Y, YList).

select_([], _, _, []).
select_([X|XList], X, Y, [Y|YList]) :-
	select_(XList, X, Y, YList).
select_([X0|XList], X, Y, [X0|YList]) :-
	select_(XList, X, Y, YList).

%%	selectchk(X, XList, Y, YList) is semidet.
%
%	Semi-deterministic version of select/4.

selectchk(X, XList, Y, YList) :-
	select(X, XList, Y, YList), !.

:- endif.
