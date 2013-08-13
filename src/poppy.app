{application, poppy,
    [
        {description, "blocking TCP server"},
        {vsn, "1.0.0"},
        {id, "poppy"},
        {modules,      [poppy, poppy_sup, poppy_ser, poppy_fsm]},
        {registered,   [poppy_sup, poppy_ser]},
        {applications, [kernel, stdlib, sasl]},
        %%
        %% mod: Specify the module name to start the application, plus args
        %%
        {mod, {poppy, []}},
        {env, []}
    ]
}.