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
	erlsom:sax(RawFeed, [], fun handle_event/2).

handle_event(startDocument, _State) ->
	[{cmd, start}, {md, #feed{}}, {entries, []}];

handle_event({endElement, _NS, "feed", _}, [{cmd, _Command}, {md, Feed}, {entries, Entries}]) ->
	Feed#feed{entries=Entries};

handle_event({startElement, _NS, "title", _, _Attrs}, [{cmd, start}, {md, Feed}, {entries, Entries}]) ->
	build_state(titletext, Feed, Entries);

handle_event({characters, Text}, [{cmd, titletext}, {md, Feed}, {entries, Entries}]) ->
	build_state(start, Feed#feed{title=Text}, Entries);

handle_event({startElement, _NS, "id", _, _Attrs}, [{cmd, start}, {md, Feed}, {entries, Entries}]) ->
	build_state(permalinktext, Feed, Entries);

handle_event({characters, Text}, [{cmd, permalinktext}, {md, Feed}, {entries, Entries}]) ->
	build_state(start, Feed#feed{url=Text}, Entries);

handle_event({startElement, _NS, "link", _, Attrs}, [{cmd, start}, {md, Feed}, {entries, Entries}]) ->
	build_state(start, Feed#feed{url=extract_link_url(Attrs)}, Entries);

handle_event({startElement, _NS, "name", _, _Attrs}, [{cmd, entry}, {md, Feed}, {entries, Entries}]) ->
	build_state(nametext, Feed, Entries);

handle_event({characters, Text}, [{cmd, nametext}, {md, Feed}, {entries, Entries}]) ->
 	[Entry|T] = Entries,
 	UpdatedEntry = Entry#feedentry{author=Text},
	build_state(entry, Feed, [UpdatedEntry|T]);

handle_event({startElement, _NS, "entry", _, _Attrs}, [{cmd, _Command}, {md, Feed}, {entries, Entries}]) ->
	build_state(entry, Feed, [#feedentry{content=""}|Entries]);

handle_event({endElement, _NS, "entry", _}, [{cmd, _Command}, {md, Feed}, {entries, Entries}]) ->
	build_state(start, Feed, Entries);

handle_event({startElement, _NS, "title", _, _Attrs}, [{cmd, entry}, {md, Feed}, {entries, Entries}]) ->
	build_state(entrytitletext, Feed, Entries);

handle_event({characters, Text}, [{cmd, entrytitletext}, {md, Feed}, {entries, Entries}]) ->
	[Entry|T] = Entries,
	UpdatedEntry = Entry#feedentry{title=Text},
	build_state(entry, Feed, [UpdatedEntry|T]);

handle_event({startElement, _NS, "link", _, Attrs}, [{cmd, entry}, {md, Feed}, {entries, Entries}]) ->
 	[Entry|T] = Entries,
 	UpdatedEntry = Entry#feedentry{permalink=extract_link_url(Attrs)},
	build_state(entry, Feed, [UpdatedEntry|T]);

handle_event({startElement, _NS, "content", _, _Attrs}, [{cmd, entry}, {md, Feed}, {entries, Entries}]) ->
	build_state(entry, Feed, Entries);

handle_event({characters, Text}, [{cmd, entrycontenttext}, {md, Feed}, {entries, Entries}]) ->
 	[Entry|T] = Entries,
	UpdatedEntry = Entry#feedentry{content=lists:append(Entry#feedentry.content, Text)},
	UpdatedEntries = [UpdatedEntry|T],
	build_state(entry, Feed, UpdatedEntries);

handle_event(_Event, State) ->
	State.

extract_link_url(Attrs) ->
	hd([Url || {attribute, "href", _, _, Url} <- Attrs]).

build_state(Command, Feed, Entries) ->
	[{cmd, Command}, {md, Feed}, {entries, Entries}].
