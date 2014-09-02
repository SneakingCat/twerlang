%% The twerlang_server is a dummy implementation of a Twitter like
%% application. The purpose is educational and the usage of low level
%% constructs is intentional.
-module (twerlang_server).

-export [start/0].

%% User record.
-record(user, { id = undefined :: tuple(),
	        following = [] :: list(),
	        followers = [] :: list(),
	        tweets    = [] :: list() }).

%% State record.
-record(state, { users = [] :: list() }).

%% Start the server.
start() ->
    io:format("=> STARTING <=~n"),
    spawn (fun () ->
		   register(?MODULE, self()),
		   message_receiver(#state {})
	   end).

message_receiver(#state {users=Users}=State) ->
    receive
	%% List all registered users as a lists of tuples in the
	%% format {ok, [{atom(), string()}]}.
	{From, list} when is_pid(From) ->
	    From ! {ok, lists:map(fun user_tuple/1, Users)},
	    message_receiver(State);

	%% Apply one of the actions (followers, following, tweets) on
	%% the given login name. Replies can be one of the following:
	%% {ok, [{atom(), string()}]}
	%% {ok, [string()]}
	%% {error, no_such_user}
	%% {error, illegal_register_format}
	{From, Action, Login} when is_pid(From),
				   is_atom(Login),
				   (Action == following orelse 
				    Action == followers orelse
				    Action == tweets) ->
	    case find_user(Login, Users) of
		{ok, User} ->
		    List = case Action of
			       following -> User#user.following;
			       followers -> User#user.followers;
			       tweets    -> lists:reverse(User#user.tweets)
			   end,						
		    From ! {ok, List},
		    message_receiver(State);
		_          ->
		    From ! {error, no_such_user},
		    message_receiver(State)
	    end;

	{From, Action, _Anything} when is_pid(From),
				       (Action == following orelse
					Action == followers orelse
					Action == tweets) ->
	    From ! {error, illegal_register_format},
	    message_receiver(State);

	%% Follow another user. Replies can be one of:
	%% {ok, following}
	%% {error, no_such_user}
	%% {error, illegal_register_format}
	{From, follow, {Login, Follow}} when is_pid(From),
					     is_atom(Login),
					     is_atom(Follow) ->
		case {find_user(Login, Users), find_user(Follow, Users)} of
		    {{ok, LoginUser}, {ok, FollowUser}} ->
			NewState = State#state {users=link_users(LoginUser,
								 FollowUser,
								 Users)},
			From ! {ok, following},
			message_receiver(NewState);
		    _                                   ->
			From ! {error, no_such_user},
			message_receiver(State)
		end;

	%% Tweet for the given user. Replies can be one of:
	%% {ok, [string()]}
	%% {error, no_such_user}
	%% {error, illegal_register_format}
	{From, tweet, {Login, Tweet}} when is_pid(From),
					   is_atom(Login),
					   is_list(Tweet) ->
	    case find_user(Login, Users) of
		{ok, User} ->
		    TweetedUser  = tweet_user(Tweet, User),
		    TweetedUsers = tweet_followers(Tweet, 
						   User#user.followers,
						   Users),
		    From ! {ok, lists:reverse(TweetedUser#user.tweets)},
		    message_receiver(State#state 
				     {users=[TweetedUser|removeUsers([User], 
								     TweetedUsers)]});
		_           ->
		    
		    From ! {error, no_such_user},
		    message_receiver(State)
	    end;

	%% Register a new user in the format {atom(),
	%% string()}. Replies can be one of:
	%% {ok, registered}
	%% {error, already_registered}
        %% {error, illegal_register_format}
	{From, register, {Login, Name}} when is_pid(From),
					     is_atom(Login),
					     is_list(Name) ->
	    case find_user(Login, Users) of
		not_found ->
		    NewUser = #user {id={Login, Name}},
		    From ! {ok, registered},		    
		    message_receiver(State#state {users = [NewUser|Users]});
		_ ->
		    From ! {error, already_registered},
		    message_receiver(State)
	    end;
		
	{From, register, _Anything} when is_pid(From) ->
	    From ! {error, illegal_register_format},
	    message_receiver(State);

	%% Killing the server.
	just_die ->
	    io:format("=> STOPPING <=~n");

	%% Last chance catch of incoming messages.
	Anything ->
	    io:format("=> UNKNOWN: ~w <=~n", [Anything]),
	    message_receiver(State)			 
    end.

%% Extract a user tuple from a user record.
user_tuple(#user {id=Id}) -> Id.

%% Find a user in the database.
find_user(Login, Users) ->    
    case lists:filter(fun (#user { id={X, _}}) -> Login == X end, Users) of
	[User] -> {ok, User};	    
	_      -> not_found
    end.

%% Link the users in an follow/followers relation.
link_users(#user {id=Id1, following=Following}=User1, 
	   #user {id=Id2, followers=Followers}=User2, Users) ->
    AlreadyFollowing = lists:any(fun (X) -> X == Id2 end, Following),
    if
	AlreadyFollowing -> Users;
	true             ->
	    NewUser1 = User1#user {following=[Id2|Following]},
	    NewUser2 = User2#user {followers=[Id1|Followers]},
	    [NewUser1 | [NewUser2 | removeUsers([User1, User2], Users)]]
    end.

%% Filter away the given users from the user database.
removeUser(User, Users) ->
    lists:filter(fun (X) -> X /= User end, Users).

%% Filter away the given users from the user database.
removeUsers(Xs, Db) -> lists:foldl(fun removeUser/2, Db, Xs).

%% Add a tweet to the user.
tweet_user(Tweet, #user {tweets=Tweets}=User) -> 
    User#user {tweets=[Tweet|Tweets]}.

tweet_followers(Tweet, Logins, Users) ->
    LoginUsers = lists:map(fun ({Login, _}) ->
				   element(2, find_user(Login, Users))
			   end, Logins),
    TweetedUsers = lists:map(fun (User) ->
				     tweet_user(Tweet, User)
			     end, LoginUsers),
    TweetedUsers ++ removeUsers(LoginUsers, Users).
