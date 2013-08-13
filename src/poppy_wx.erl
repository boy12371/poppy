%% @author Richard
%% @doc @todo a micro web UI of the blocking tcp server


-module(poppy_wx).
-author("kuangyel2000@gmail.com").



%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, init/0]).

-include_lib("wx/include/wx.hrl").

-define(ABOUT, ?wxID_ABOUT).
-define(EXIT,   ?wxID_EXIT).
-define(APPEND,        131).
-define(UNDO,          132).
-define(OPEN,          133).
-define(SAVE,          134).
-define(NEW,           135).



%% Top-level function: create the wx-server, the graphical objects,
%% show the application, process and clean up on termination.
start() ->
    spawn(?MODULE, init, []).

init() ->
    Wx = wx:new(),
    Frame = wxFrame:new(    Wx,
                            ?wxID_ANY,
                            "Poppy System",
                            [   {pos, {400, 300}},
                                {size, {800, 600}},
                                {style, ?wxDEFAULT_FRAME_STYLE}
                            ]
                       ),
    Text = wxTextCtrl:new(    Frame,
                              ?wxID_ANY,
                              [{value, "mnesia database"}, {style, ?wxTE_MULTILINE}]
                         ),
    setup(Frame, Text),
    wxFrame:show(Frame),
    loop(Frame, Text),
    wx:destroy().



%% ====================================================================
%% Internal functions
%% Top-level frame: create a menu bar, two menus, two menu items
%% and a status bar. Connect the frame to handle events.
%% ====================================================================
setup(Frame, Text) ->
    MenuBar = wxMenuBar:new(),
    File    = wxMenu:new(),
    Edit    = wxMenu:new(),
    Help    = wxMenu:new(),
    wxFrame:createStatusBar(Frame),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxMenuBar:append(MenuBar, File, "&File"),
    wxMenuBar:append(MenuBar, Edit, "&Edit"),
    wxMenuBar:append(MenuBar, Help, "&Help"),
    wxMenu:append(Help, ?ABOUT,         "About Poppy"),
    wxMenu:append(File, ?NEW,           "New\tCtrl-N"),
    wxMenu:append(File, ?OPEN,   "Open saved\tCtrl-O"),
    wxMenu:append(File, ?SAVE,         "Save\tCtrl-S"),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?EXIT,         "Quit\tCtrl-Q"),
    wxMenu:append(Edit, ?APPEND, "Add en&try\tCtrl-T"),
    wxMenu:append(Edit, ?UNDO,  "Undo latest\tCtrl-U"),
    wxFrame:setStatusText(Frame, "Poppy Database"),
    wxTextCtrl:setEditable(Text, false),
    wxFrame:connect(Frame, command_menu_selected),
    wxFrame:connect(Frame, close_window).



loop(Frame, Text) ->
    receive
        #wx{id = ?APPEND, event = #wxCommand{type = command_menu_selected}} ->
            Prompt = "Please enter text here.",
            MD = wxTextEntryDialog:new(Frame, Prompt, [{caption, "New database entry"}]),
            case wxTextEntryDialog:showModal(MD) of
                ?wxID_OK ->
                    Str = wxTextEntryDialog:getValue(MD),
                    wxTextCtrl:appendText(Text, [10]++Str);
                _ ->
                    ok
            end,
            wxDialog:destroy(MD),
            loop(Frame, Text);
        #wx{id = ?UNDO, event = #wxCommand{type = command_menu_selected}} ->
            {StartPos,EndPos} = lastLineRange(Text),
            wxTextCtrl:remove(Text, StartPos - 2, EndPos + 1),
            loop(Frame, Text);
        #wx{id = ?OPEN, event = #wxCommand{type = command_menu_selected}} ->
            wxTextCtrl:loadFile(Text, "POPPY"),
            loop(Frame, Text);
        #wx{id = ?SAVE, event = #wxCommand{type = command_menu_selected}} ->
            wxTextCtrl:saveFile(Text, [{file, "POPPY"}]),
            loop(Frame, Text);
        #wx{id = ?NEW, event = #wxCommand{type = command_menu_selected}} ->
            {_,EndPos} = lastLineRange(Text),
            StartPos = wxTextCtrl:xYToPosition(Text, 0, 0),
            wxTextCtrl:replace(Text, StartPos, EndPos, "poppy_wx"),
            loop(Frame, Text);
        #wx{id = ?ABOUT, event = #wxCommand{}} ->
            Str = "poppy_wx is a tool of mnesia database written in wxerlang.",
            MD = wxMessageDialog:new(   Frame,
                                        Str,
                                        [   {   style,
                                                ?wxOK bor ?wxICON_INFORMATION
                                            },
                                            {   caption,
                                                "About poppy_wx"
                                            }
                                        ]
                                    ),
            wxDialog:showModal(MD),
            wxDialog:destroy(MD),
            loop(Frame, Text);
        #wx{id = ?EXIT, event = #wxCommand{type = command_menu_selected}} ->
            io:format("Quit this program.~n"),
            wxWindow:close(Frame, [])
    end.


lastLineRange(Text) ->
    ListOfLines = string:tokens(Text, [[10]]),  %% Tokenize buffer on newlines
    LastLine    = lists:last(ListOfLines),           %% find the last line
    StarPos     = wxTextCtrl:getlastposition() - length(LastLine),
    EndPos      = wxTextCtrl:getlastposition(),
    {StarPos, EndPos}.

