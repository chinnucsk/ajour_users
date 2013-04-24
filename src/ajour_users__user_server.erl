-module(ajour_users__user_server).

-behaviour(gen_server).

-compile(export_all).

-record(ajour_user, {user_id,
                     password_sha}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%           helpers:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
validate_password(UserId, Password, Users) ->
    erlsha2:sha256(Password) =:= dict:fetch(UserId, Users).

write_user_record_to_database(UserId, Password) ->
    %% write the user to the database
    User_r = #ajour_user{user_id = UserId, password_sha = erlsha2:sha256(Password)},
    Fun = fun() ->
                  mnesia:write(User_r)
          end,
    mnesia:transaction(Fun).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%           The External Interface:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link() -> {ok,pid()} | ignore |
                      {error, {already_started, pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ajour_users}, ajour_users, [], []).

create_user(UserId, Password) ->
    gen_server:call(ajour_users, {create_user, UserId, Password}).

update_user(UserId, OldPassword, NewPassword) ->
    gen_server:call(ajour_users, {update_user, UserId, OldPassword, NewPassword}).

delete_user(UserId, Password) ->
    gen_server:call(ajour_users, {delete_user, UserId, Password}).

validate_user(UserId, Password) ->
    gen_server:cast(ajour_users, {validate_user, UserId, Password}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%           The GenServer Interface:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Arg) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(users_db,
                        [{attributes, record_info(fields, ajour_user)}]),
    {ok, dict:new()}.


handle_call({create_user, UserId, Password}, _From, Users) ->
    case dict:is_key(UserId, Users) of
        true ->
            {reply, {error, <<"User already exists">>}, Users};
        false ->
            write_user_record_to_databaseb(UserId, Password),
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
                    write_user_record_to_database(UserId, NewPassword),
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
