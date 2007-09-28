%% Copyright (c) 2007, Kevin A. Smith<kevin@hypotheticalabs.com>
%% 
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without 
%% modification, are permitted provided that the following 
%% conditions are met:
%% 
%% * Redistributions of source code must retain the above copyright notice, 
%% this list of conditions and the following disclaimer.
%% 
%% * Redistributions in binary form must reproduce the above copyright 
%% notice, this list of conditions and the following disclaimer in the 
%% documentation and/or other materials provided with the distribution.
%% 
%% * Neither the name of the hypotheticalabs.com nor the names of its 
%% contributors may be used to endorse or promote products derived from 
%% this software without specific prior written permission.
%% 
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR 
%% TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF 
%% THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%% DAMAGE.

-module(atom_parser).

-export([parse_feed/1]).

-include("atomizer.hrl").

parse_feed(RawFeed) ->
	CB = fun(Event, State) ->
				 handle_event(Event, State)
		 end,
	erlsom:sax(RawFeed, [], CB).

handle_event(startDocument, _State) ->
	io:format("startDocument~n"),
	[{cmd, start}, {md, #feed{}}, {entries, []}];

handle_event({endElement, _NS, "feed", _}, [{cmd, _Command}, {md, Feed}, {entries, Entries}]) ->
	io:format("end feed element~n"),
	Feed#feed{entries=Entries};

handle_event({startElement, _NS, "title", _, _Attrs}, [{cmd, start}, {md, Feed}, {entries, Entries}]) ->
	build_state(titletext, Feed, Entries);

handle_event({characters, Text}, [{cmd, titletext}, {md, Feed}, {entries, Entries}]) ->
	build_state(permalink, Feed#feed{title=Text}, Entries);

handle_event({startElement, _NS, "id", _, _Attrs}, [{cmd, permalink}, {md, Feed}, {entries, Entries}]) ->
	build_state(permalinktext, Feed, Entries);

handle_event({characters, Text}, [{cmd, permalinktext}, {md, Feed}, {entries, Entries}]) ->
	io:format("permalinktext~n"),
	build_state(entry, Feed#feed{url=Text}, Entries);

handle_event({startElement, _NS, "link", _, Attrs}, [{cmd, permalink}, {md, Feed}, {entries, Entries}]) ->
	io:format("permalink~n"),
	build_state(entry, Feed#feed{url=extract_link_url(Attrs)}, Entries);

handle_event({startElement, _NS, "name", _, _Attrs}, [{cmd, entry}, {md, Feed}, {entries, Entries}]) ->
	build_state(nametext, Feed, Entries);

handle_event({characters, Text}, [{cmd, nametext}, {md, Feed}, {entries, Entries}]) ->
	io:format("nametext~n"),
	build_state(entry, Feed#feed{author=Text}, Entries);

handle_event({startElement, _NS, "entry", _, _Attrs}, [{cmd, entry}, {md, Feed}, {entries, Entries}]) ->
	build_state(entrytitle, Feed, [#feedentry{content=""}|Entries]);

handle_event({endElement, _NS, "entry", _}, [{cmd, _Command}, {md, Feed}, {entries, Entries}]) ->
	build_state(entry, Feed, Entries);

handle_event({startElement, _NS, "title", _, _Attrs}, [{cmd, entrytitle}, {md, Feed}, {entries, Entries}]) ->
	build_state(entrytitletext, Feed, Entries);

handle_event({characters, Text}, [{cmd, entrytitletext}, {md, Feed}, {entries, Entries}]) ->
	io:format("getting entrytitletext~n"),
	[Entry|T] = Entries,
	UpdatedEntry = Entry#feedentry{title=Text, author=Feed#feed.author},
	build_state(entrylink, Feed, [UpdatedEntry|T]);

handle_event({startElement, _NS, "link", _, Attrs}, [{cmd, entrylink}, {md, Feed}, {entries, Entries}]) ->
	io:format("getting entrylink~n"),
 	[Entry|T] = Entries,
 	UpdatedEntry = Entry#feedentry{permalink=extract_link_url(Attrs)},
	build_state(entrycontent, Feed, [UpdatedEntry|T]);

handle_event({startElement, _NS, "content", _, _Attrs}, [{cmd, entrycontent}, {md, Feed}, {entries, Entries}]) ->
	io:format("getting entrycontent~n"),
	build_state(entrycontenttext, Feed, Entries);

handle_event({characters, Text}, [{cmd, entrycontenttext}, {md, Feed}, {entries, Entries}]) ->
 	[Entry|T] = Entries,
	UpdatedEntry = Entry#feedentry{content=lists:append(Entry#feedentry.content, Text)},
	UpdatedEntries = [UpdatedEntry|T],
	build_state(entrycontenttext, Feed, UpdatedEntries);

handle_event(_Event, State) ->
	State.

extract_link_url(Attrs) ->
	[Href|_T] = [Url || {attribute, "href", "href", [], Url} <- Attrs],
	Href.

build_state(Command, Feed, Entries) ->
	lists:flatten([build_cmd(Command), build_state(Feed, Entries)]).

build_state(Feed, Entries) ->
   [{md, Feed}, {entries, Entries}].

build_cmd(Command) ->
	{cmd, Command}.

