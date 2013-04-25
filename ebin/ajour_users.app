{application,ajour_users,
             [{description,"Ajour Users storage"},
              {vsn,"1"},
              {modules,[ajour_users__user_server]},
              {registered,[ajour_users]},
              {applications,[kernel,stdlib,cowboy,mnesia]},
              {mod,{ajour_users_app,[]}},
              {env,[]}]}.
