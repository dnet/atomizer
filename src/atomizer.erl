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

-module(atomizer).

-export([parse_url/1, parse_file/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include("atomizer.hrl").

parse_url(Url) ->
	case fetcher:fetch(Url) of
		{error, Reason} ->
			throw(Reason);
		{ok, ContentType, _Status, _Headers, Body} ->
			parse(examine_content_type(ContentType), Body)
	end.

parse_file(FilePath) ->
	{ok, Raw} = file:read_file(FilePath),
	Feed = binary_to_list(Raw),
	parse(examine_content(Feed), Feed).
					
examine_content(Feed) ->
	case regexp:match(Feed, "<feed") of
		{match, _, _} ->
			atom;
		nomatch ->
			case regexp:match(Feed, "<channel") of
				{match, _, _} ->
					rss;
				nomatch ->
					unknown
			end
	end.
	
examine_content_type(ContentType) ->
	case ContentType of
		"text/xml" ->
			rss;
		"application/rss+xml" ->
			rss;
		"application/atom+xml" ->
			atom;
		"application/xml" ->
			atom;
		true ->
			unknown
	end.

parse(unknown, _Feed) ->
	uknown;

parse(rss, Feed) ->
	void;

parse(atom, Feed) ->
	atom_parser:parse_feed(Feed).
	
