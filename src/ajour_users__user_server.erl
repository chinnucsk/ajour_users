-module(ajour_users__user_server).

-behaviour(gen_server).

-compile(export_all).


validate_password(UserId, Password, Users) ->
    erlsha2:sha256(Password) =:= dict:fetch(UserId, Users).


start_link() ->
    gen_server:start_link({local, ajour_users}, ajour_users, [], []).


% Init -- perhaps from a database.
init(_Arg) ->
    {ok, dict:new()}.


handle_call({create_user, UserId, Password}, _From, Users) ->
    case dict:is_key(UserId, Users) of
        true ->
            {reply, {error, <<"User already exists">>}, Users};
        false ->
            Users1 = dict:store(UserId, Password, Users),
            {reply, ok, Users1}
    end;
handle_call({update_user, UserId, Password, NewPassword}, _From, Users) ->
    case dict:is_key(UserId, Users) of
        false ->
            {reply, {error, <<"User does not exist">>}, Users};
        true ->
            case validate_password(UserId, Password, Users) of
                false ->
                    {reply, {error, <<"Invalid Password">>}, Users};
                true ->
                    Users1 = dict:store(UserId, NewPassword, Users),
                    {reply, ok, Users1}
            end
    end;
handle_call({delete_user, UserId, Password}, _From, Users) ->
    case dict:is_key(UserId, Users) of
        false ->
            {reply, {error, <<"User does not exist">>}, Users};
        true ->
            case validate_password(UserId, Password, Users) of
                false ->
                    {reply, {error, <<"Invalid Password">>}, Users};
                true ->
                    Users1 = dict:erase(UserId, Users),
                    {reply, ok, Users1}
            end
    end.
    

handle_cast({validate_user, UserId, Password}, Users) ->
    case dict:is_key(UserId, Users) of
        false ->
            {reply, {error, <<"User does not exist">>}, Users};
        true ->
            {reply, validate_password(UserId, Password, Users), Users}
    end.


