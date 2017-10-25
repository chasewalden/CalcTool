:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_parameters)).

nowrite(T).
nonl.

:- [calculus].
:- [matrix].


startServer(PORT) :-
http_server(http_dispatch, [port(PORT)]).

:- http_handler(root(.), homepage, []).
:- http_handler(root(derive), derivepage, []).
:- http_handler(root(integrate), integratepage, []).
:- http_handler(root(matrix), matrixpage, []).


navitem("Derivative", "plus" ,"/derive").
navitem("Integral", "signal", "/integrate").
navitem("Matrices", "th", "/matrix").

homepage(_Request) :- pagetemplate("Home",
    html([
        p("Please click on a tool link")
    ])
).

derivepage(Request) :-
    get_function(Request, Func),
    term_string(T, Func),
    derive(T, x, D),
    term_string(D, Deriv),
    pagetemplate("Derive",
        html([
            div(class("form-inline"), [
                form([action='derive', method='POST'],[
                    p(class("form-group"),[
                        label([class="control-label", for=function], "f(x) = "),
                        input([class="form-control", name=function, type=textarea, value=Func]),
                        input([class="form-control", name=submit, type=submit, value="Derive"])
                    ])
                ]),
                p(class("form-group"),[
                    label([class="control-label", for=derivative], "f'(x) = "),
                    input([class="form-control", name=derivative, type=textarea, value=Deriv])
                ])
            ])
        ])
    ).


integratepage(Request) :-
    get_function(Request, Func),
    term_string(T, Func),
    integrate(T, x, I),
    term_string(I, Integ),
    pagetemplate("Integrate",
        html([
            div(class("form-inline"), [
            form([action='integrate', method='POST'],[
                p(class("form-group"),[
                    label([class="control-label", for=function], "f(x) = "),
                    input([class="form-control", name=function, type=textarea, value=Func]),
                    input([class="form-control", name=submit, type=submit, value="Integrate"])
                ])
            ]),
            p(class("form-group"),[
                label([class="control-label", for=derivative], "F(x) = "),
                input([class="form-control", name=derivative, type=textarea, value=Integ])
            ])
        ])
    ])
).

matrixfunction("invert", "A^(-1) = ").
matrixfunction("rref", "RREF(A) = ").
matrixfunction("multiply", "A ○ B = ").

matrixpage(Request) :-
    get_matrix_args(Request, Func, MA, MB),
    term_string(A, MA), term_string(B, MB),
    do_matrix(Func, A, B, O),
    term_string(O, MO),
    matrixfunction(Func, ResultLabel),
    pagetemplate("Integrate",
        html([
            div(class("form-horizontal"),[
            form([action='matrix', method='POST'],[
                p(class("form-group"),[
                    label([class="control-label", for=matrixA], "A = "),
                    input([class="form-control", name=matrixA, type=textarea, value=MA])
                ]),
                p(class("form-group"),[
                    label([class="control-label", for=matrixB], "B = "),
                    input([class="form-control", name=matrixB, type=textarea, value=MB])
                ]),
                p(class("form-group"),[
                    button([class="form-control", name=submit, type=submit, value="multiply"], "Multiply"),
                    button([class="form-control", name=submit, type=submit, value="invert"], "Invert A"),
                    button([class="form-control", name=submit, type=submit, value="rref"], "RREF A")
                ])
            ]),
            p(class("form-group"),[
                label([class="control-label", for=derivative], ResultLabel),
                input([class="form-control", name=derivative, type=textarea, value=MO])
            ])
        ])
    ])
).

get_function(Request, "") :- member(method(get), Request), !.
get_function(Request, Func) :- member(method(post), Request),
        http_parameters(Request, [
            function(Func, [default(""), string])
        ]).

get_matrix_args(Request, "multiply", "[[1,2],[3,4]]", "[[1,2],[3,4]]") :- member(method(get), Request), !.
get_matrix_args(Request, S, MA, MB) :- member(method(post), Request),
    http_parameters(Request, [
        submit(S, [string]),
        matrixA(MA, [default("[[1,2],[3,4]]"), string]),
        matrixB(MB, [default("[[1,2],[3,4]]"), string])
    ]).

do_matrix("multiply", A, B, O) :- dotProductMatrix(A,B,O).
do_matrix("invert", A, _, O) :- invertMatrix(A,O).
do_matrix("rref", A, _, O) :- reducedEchelon(A,O).


css(URL) --> html(link([
    type('text/css'),
    rel('stylesheet'),
    href(URL)
])).

pagetemplate(Name, Content) :- string_concat("Calc Tool - ", Name, Title), reply_html_page(
    main_style,
    [
        title(Title),
        \css("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css")
    ],
    [Content]
).


nav_bar --> {
    findall(N, navitem(N, _, _), ButtonNames),
    maplist(htmlnavitem, ButtonNames, Buttons)
},
html(
    nav([id(navbar), class(["navbar", "navbar-default"])],
        div(class("container-fluid"),
            ul(class(["nav", "navbar-nav"]), Buttons)
        )
    )
).

htmlnavitem(Name, li(a([href=Ref], [span(class(["glyphicon", Icon]), ""), " ", Name]))) :- navitem(Name, IconName, Ref), string_concat("glyphicon-", IconName, Icon).

:- multifile user:body//2.

% Body will be included
user:body(main_style, Body) -->
    html(body(class(container),
        [
            div(id(header), h1('Calculus Tool')),
            \nav_bar,
            div([id(content), class(jumbotron)] , Body)
        ]
)).
