-module (client).

-export([rpc/2, list/1, register/3, follow/3, tweet/3, 
	 tweets/2, following/2, followers/2, twittermost/1]).

list(Service) ->
    rpc(Service, {self(), list}).

register(Service, Login, Name) ->
    rpc(Service, {self(), register, {Login, Name}}).

follow(Service, Login, Follow) ->
    rpc(Service, {self(), follow, {Login, Follow}}).

tweet(Service, Login, Tweet) ->
    rpc(Service, {self(), tweet, {Login, Tweet}}).

tweets(Service, Login) ->
    rpc(Service, {self(), tweets, Login}).

following(Service, Login) ->
    rpc(Service, {self(), following, Login}).

followers(Service, Login) ->
    rpc(Service, {self(), followers, Login}).

twittermost(Service) ->
    {ok, Users} = list(Service),
    Xs = lists:map(fun ({Login, _}=User) ->
		      {ok, Tweets} = tweets(Service, Login),
		      {length(Tweets), User}
		   end, Users),
    lists:sort(fun ({N, _}, {M, _}) -> N > M end, Xs).

rpc(Service, Msg) ->
    Service ! Msg,
    receive
	Reply -> Reply
	after
	    1000 ->
		{timeout, Service, Msg}
	end.
