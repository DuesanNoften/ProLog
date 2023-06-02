%=======================  Restaurantes  =========================%
%restaurante("Nombre del restaurante").
restaurante("Bella Italia").
restaurante("Italianisimo").
restaurante("McBurguesa").
restaurante("KFG").
restaurante("Casita del Bocata").
restaurante("Upway").
restaurante("Burritobell").
restaurante("Tacos de mi Tia Panchita").
restaurante("Dolche far Niente").
restaurante("Tres Pinos").

%=======================  Disposiciones  =========================%
%disposiciones("Restaurante", "Disposicion").
disposiciones("Bella Italia",  "Solo se permiten burbujas y durante la espera se debe utilizar mascarilla").
disposiciones("Italianisimo", "Utilizar mascarilla").
disposiciones("McBurguesa", "Solo se permiten burbujas").
disposiciones("KFG",  "Pago solamente en efectivo").
disposiciones("Casita del Bocata", "Si desea un asciento debera reservarlo").
disposiciones("Upway", "Pago solamente en tarjeta").
disposiciones("Burritobell",  "Si desea salsas debe pedirlas en el mostrador").
disposiciones("Tacos de mi Tia Panchita", "Utilizar mascarilla").
disposiciones("Dolche far Niente", "No se permiten mascotas").
disposiciones("Tres Pinos", "No se permite tarjeta, solo simpemovil").

%=======================  Menus  =========================%
%menu("Nombre", "Tipo de menu", "[Comida |[Tipos especificos]]").
menu("Bella Italia", "italiano", ["Pizza", ["jamon y queso", "suprema", "hawaiana"], "calzone", "espagueti"]).
menu("Italianisimo","italiano" ,["Pizza", ["pepperoni"], "calzone", "espagueti"]).
menu("McBurguesa","Comida Rapida" ,["hamburguesas", "tacos ", "papas"]).
menu("KFG", "Comida Rapida" ,["pollo", "nuggets ", "alitas"]).
menu("Casita del Bocata", "Sandwich", ["pavo", "pollo", "pato"]).
menu("Upway", "Sandwich", ["cerdo", "res", "queso"]).
menu("Burritobell",  "Mexicana", ["burrito", "tacos", "enchiladas"]).
menu("Tacos de mi Tia Panchita", "Mexicana",["burrito", "tacos", "enchiladas"]).
menu("Dolche far Niente", "Postres", ["gelato", "tiramisu", "cannoli"]).
menu("Tres Pinos", "Postres", ["cono sencillo", "cono doble", "cono arreglado"]).

%=======================  Pizzas =========================%
%pizza("Nombre del restaurante", "[Tipos especificos]").
pizza("Bella Italia", ["jamon y queso", "suprema", "hawaiana"]).
pizza("Italianisimo", ["pepperoni"]).
%pizza("Restaurante 1", "Restaurante 2").
pizza("Italianisimo","Bella Italia").

%=======================  Comida Rapida =========================%
%comidarapida("Nombre del restaurante", "[Tipos especificos]").
comidarapida("McBurguesa",["hamburguesas", "tacos", "papas"]).
comidarapida("KFG",["pollo", "nuggets", "alitas"]).
%comidarapida("Restaurante 1", "Restaurante 2").
comidarapida("McBurguesa","KFG").

%======================= Sandwich  =========================%
%sandwich("Nombre del restaurante", "[Tipos especificos]").
sandwich("Casita del Bocata",["pavo", "pollo", "pato"]).
sandwich("Upway",["cerdo", "res", "queso"]).
%sandwich("Restaurante 1", "Restaurante 2").
sandwich("Casita del Bocata", "Upway").

%======================= Mexicana =========================%
%mexicana("Nombre del restaurante", "[Tipos especificos]").
mexicana("Burritobell",["burrito", "tacos", "enchiladas"]).
mexicana("Tacos de mi Tia Panchita",["burrito", "tacos", "enchiladas"]).
%mexicana("Restaurante 1", "Restaurante 2").
mexicana("Burritobell","Tacos de mi Tia Panchita").

%======================= Postres =========================%
%postres("Nombre del restaurante", "[Tipos especificos]").
postres("Dolche far Niente",["gelato", "tiramisu", "cannoli"]).
postres("Tres Pinos",["cono sencillo", "cono doble", "cono arreglado"]).
%postres("Restaurante 1", "Restaurante 2").
postres("Dolche far Niente", "Tres Pinos").

%=======================  Direcciones =========================%
% direccion("Nombre del restaurante", "Direccion").
direccion("Bella Italia", "300m Sur de la entrada principal de la Universidad Nacional" ).
direccion("Italianisimo", "50m Sur de la entrada Banco de Costa Rica" ).
direccion("McBurguesa", "100m Norte de la entrada principal del TEC" ).
direccion("KFG", "Dentro del Oxigeno").
direccion("Casita del Bocata", "50m Oeste de las Ruinas").
direccion("Upway", "Frente al Mercado Central").
direccion("Burritobell", "400m Este del Banco Central").
direccion("Tacos de mi Tia Panchita", "Continuo al Paseo de las Flores").
direccion("Dolche far Niente", "Costado norte de la basilica").
direccion("Tres Pinos", "Dentro de Multiplaza Escazu").

%=======================  Lugares  =========================%
% lugar("Nombre del restaurante", "Lugar donde se ubica").
lugar("Bella Italia", "Heredia").
lugar("Italianisimo", "Alajuela").
lugar("McBurguesa", "Cartago").
lugar("KFG", "Alajuela").
lugar("Casita del Bocata", "Cartago").
lugar("Upway", "San Jose").
lugar("Burritobell", "San Jose").
lugar("Tacos de mi Tia Panchita", "Heredia").
lugar("Dolche far Niente", "Cartago").
lugar("Tres Pinos", "San Jose").

%=======================  Capacidad  =========================%
% capacidad("Nombre del restaurante", Capacidad maxima).
capacidad("Bella Italia", 10).
capacidad("Italianisimo", 5).
capacidad("McBurguesa", 20).
capacidad("KFG", 30).
capacidad("Casita del Bocata",25).
capacidad("Upway", 40).
capacidad("Burritobell", 25).
capacidad("Tacos de mi Tia Panchita", 10).
capacidad("Dolche far Niente",30).
capacidad("Tres Pinos", 30).

%=======================  BNF  ===============================%
%=======================  ALIMENTOS  =========================%
%alimento(Oracion, Oracion preliminar, Palabra(s) clave).
alimento(S0,S,Claves):-
    pronombre(Num,S0,S1),
    sintagma_verbal(Num,Estado,S1,S2),
    sintagma_nominal(_Gen2,Num,Estado,S2,S, Claves).
alimento(S0,S, Claves):-
    sintagma_verbal(Num,Estado,S0,S1),
    sintagma_nominal(_Gen2,Num,Estado,S1,S, Claves).
alimento(S0,S, Claves):-
    sintagma_nominal(_Gen2,_,_,S0,S, Claves).
alimento(_S0,_S,_Claves):-
    write("Lo sentimos, no se conoce ningun restaurante con ese tipo de comida"),
    nl, nl,
    restaurantec_loop().

%======================  UBICACIONES  =======================%
% ubicacion(Oracion, Oracion preliminar, Palabra(s) clave).
ubicacion(S0,S,S1):-
    preposicion(S0,S1),
    lugares(_,S).
ubicacion(S0,S,S0):-
    lugares(_,S).

%==================  CANTIDAD DE PERSONAS  ==================%
% personas(Oracion, Oracion preliminar, Palabra(s) clave).
personas(S0,S,S1):-
    preposicion(S0,[S1|_]),
    cantidad(_,S2),
    person(S2,S).
personas(S0,S,S1):-
    preposicion(S0,S1),
    cantidad(_,S).
personas(S0,S,S0):-
    cantidad(_,S1),
    person(S1,S).
personas(S0,S,S0):-
    cantidad(_,S).

%=======================  SINTAGMAS =========================%
% sintagma_nominal(Genero, Numero, Estado, Oracion preliminar,
% Oracion,Palabra clave).
sintagma_nominal(Gen,Num,Estado,S0,S, S1):-
    determinante(Gen,Num,S0,S1),
    nombre(Gen,Num,Estado,S1,S2),
    adjetivo(Gen,Num,S2,S).
sintagma_nominal(Gen,Num,Estado,S0,S, S1):-
    nombre(Gen,Num,Estado,S0,S1),
    adjetivo(Gen,Num,S1,S).
sintagma_nominal(Gen,Num,Estado,S0,S, S1):-
    determinante(Gen,Num,S0,S1),
    nombre(Gen,Num,Estado,S1,S).
sintagma_nominal(Gen,Num,Estado,S0,S, S0):-
    nombre(Gen,Num,Estado,S0,S).
% sintagma_verbal(Genero, Numero, Estado, Oracion preliminar,
% Oracion,Palabra clave).
sintagma_verbal(Num,_Estado,S0,S):-verbo(Num,S0,S).
sintagma_verbal(Num,Estado,S0,S):-
    verbo(Num,S0,S1),
    infinitivo(Estado,S1,S).

%=======================  NOMINAL =========================%
%determinante(Genero, Numero, Determinante, Oracion).
determinante(femenino, singular, [una|S],S).
determinante(femenino, plural, [unas|S],S).
determinante(masculino, singular, [un|S],S).
determinante(masculino, plural, [unos|S],S).
determinante(masculino, singular, [el|S],S).
determinante(femenino, singular, [la|S],S).
determinante(masculino, plural, [los|S],S).
determinante(femenino, plural, [las|S],S).

%pronombre(Numero, Pronombre, Oracion).
pronombre(singular,[yo|S],S).
pronombre(singular,[josue|S],S).
pronombre(singular,[isa|S],S).
pronombre(singular,[jordy|S],S).
pronombre(singular,[usted|S],S).
pronombre(singular,[el|S],S).
pronombre(singular,[ella|S],S).
pronombre(singular,[ellos|S],S).
pronombre(singular,[ellas|S],S).
pronombre(singular,[nosotros|S],S).
pronombre(singular,[nosotras|S],S).
pronombre(singular,[tu|S],S).
%adjetivo(Genero, Numero, adjetivo, Oracion).
adjetivo(femenino,singular,[rapida|S],S).
%nombre(Genero, Nomero, Estado,  Nombre, Oracion).
nombre(masculino, singular, solido, [italiano|S],S).
nombre(masculino, _, solido, [tacos|S],S).
nombre(masculino, singular, solido, [calzone|S],S).
nombre(masculino, singular, solido, [espagueti|S],S).
nombre(femenino, singular, solido, [pizza|S],S).
nombre(femenino, _, solido, [papas|S],S).
nombre(femenino, _, solido, [hamburguesas|S],S).
nombre(femenino, singular, solido, [comida|S],S).
nombre(femenino, singular, liquido, [bebida|S],S).
nombre(masculino, singular, solido, [sandwich|S],S).
nombre(masculino, singular, solido, [pollo|S],S).
nombre(masculino,_, solido, [nuggets|S],S).
nombre(femenino, _, solido, [alitas|S],S).
nombre(femenina, singular, solido, [pechuga|S],S).
nombre(masculino, singular, solido, [pavo|S],S).
nombre(masculino, singular, solido, [pato|S],S).
nombre(femenino,_, solido, [enchiladas|S],S).
nombre(masculino, singular, liquido, [cerdo|S],S).
nombre(masculino, singular, solido, [res|S],S).
nombre(masculino, singular, solido, [queso|S],S).
nombre(femenina, singular, solido, [mexicana|S],S).
nombre(masculino, singular, solido, [pozole|S],S).
nombre(masculino, singular, solido, [burrito|S],S).
nombre(masculino, _, solido, [postres|S],S).
nombre(masculino, singular, solido, [gelato|S],S).
nombre(masculino, singular, solido, [tiramisu|S],S).
nombre(masculino, singular, liquido, [cannoli|S],S).
nombre(masculino, singular, solido, [cono_arreglado|S],S).
nombre(masculino, singular, solido, [cono_sencillo|S],S).
nombre(masculino, singular, liquido, [cono_doble|S],S).
%lugares(_, Oracion). Admite cualquier lugar
lugares([_|S],S).
%person(personas, Oracion).
person([personas|S],S).
%cantidad(_, Oracion). Admite cualquier cantidad
cantidad([_|S],S).

%=======================  VERBAL =========================%
% verbo(Numero, Verbo, Oracion).
verbo(singular,[quiero|S],S).
verbo(singular, [quiere|S],S).
verbo(singular, [quieres|S],S).
verbo(plural, [queremos|S],S).
verbo(plural, [quieren|S],S).
verbo(singular,[deseo|S],S).
verbo(singular, [desea|S],S).
verbo(singular, [deseas|S],S).
verbo(plural, [deseamos|S],S).
verbo(plural, [desean|S],S).
verbo(singular, [ordeno|S],S).
verbo(singular, [pido|S],S).
verbo(singular, [pides|S],S).
verbo(singular, [pide|S],S).
verbo(plural, [pedimos|S],S).
verbo(plural, [piden|S],S).
verbo(singular, [ordenas|S],S).
verbo(singular, [ordeno|S],S).
verbo(singular, [ordena|S],S).
verbo(plural, [ordenamos|S],S).
verbo(plural, [ordenan|S],S).
verbo(singular, [pruebo|S],S).
verbo(singular, [prueba|S],S).
verbo(singular, [pruebas|S],S).
verbo(plural, [probamos|S],S).
verbo(plural, [prueban|S],S).
verbo(singular, [recomiendo|S],S).
verbo(singular, [recomiendas|S],S).
verbo(singular, [recomienda|S],S).
verbo(plural, [recomendamos|S],S).
verbo(plural, [recomiendan|S],S).
verbo(singular, [como|S],S).
verbo(singular, [come|S],S).
verbo(singular, [comes|S],S).
verbo(plural, [comemos|S],S).
verbo(plural, [comen|S],S).
verbo(singular, [tomo|S],S).
verbo(singular, [toma|S],S).
verbo(singular, [tomas|S],S).
verbo(plural, [tomamos|S],S).
verbo(plural, [toman|S],S).
verbo(singular, [viajo|S],S).
verbo(singular, [viaja|S],S).
verbo(singular, [viajas|S],S).
verbo(plural, [viajamos|S],S).
verbo(plural, [viajan|S],S).
verbo(singular, [busco|S],S).
verbo(singular, [busca|S],S).
verbo(singular, [buscas|S],S).
verbo(plural, [buscamos|S],S).
verbo(plural, [buscan|S],S).
%infinitivo(Estado,  Infinitivo, Oracion).
infinitivo(solido, [comer|S],S).
infinitivo(liquido, [tomar|S],S).
infinitivo(solido, [pedir|S],S).
infinitivo(liquido, [pedir|S],S).
infinitivo(solido, [ordenar|S], S).
infinitivo(liquido, [ordenar|S],S).
infinitivo(solido, [probar|S], S).
infinitivo(liquido, [probar|S],S).
infinitivo(solido, [recomendar|S], S).
infinitivo(liquido, [recomenadar|S],S).
infinitivo(solido, [desear|S], S).
infinitivo(liquido, [desear|S],S).
infinitivo(solido, [querer|S], S).
infinitivo(liquido, [querer|S],S).
infinitivo(solido, [viajar|S], S).
infinitivo(liquido, [viajar|S],S).
infinitivo(solido, [buscar|S], S).
infinitivo(liquido, [buscarr|S],S).
%preposicion(Preposicion Oracion).
preposicion([en|S],S).
preposicion([para|S],S).

%=====================  PARSEAR INPUT =======================%
%Caso base
parseInput([],[]).

%Se hace una lista de las palabras ingresadas por el usuario como átomos
parseInput([C|InputList], [A|Result]):-
    atom_string(A,C),
    parseInput(InputList,Result).
%Entradas: Input es la entrada de texto del usuario
%Salidas: R sera la entrada en formato analizable
getInput(Input,R):-
    split_string(Input," ",".",R1),
    parseInput(R1,R).

%======================  DELIMITANTES ========================%
% Revisa si un elemento pertenece a una lista
% Sintaxis: miembro(elemento, lista).
% Entradas: elemento, lista.
% Salidas: Booleano indicando si el elemento pertenece a la lista o no
miembro(X, [X|_]).
miembro(X, [_|R]):-miembro(X,R).

%Validar tipo de menu italiano
validaralimento(Y, X):-
    Y == [italiano],
    write("¿Que tipo de comida Italiana quiere comer?"), nl,
    read(T),
    getInput(T,Tparsed),
    alimento(Tparsed,[],AlimentoClave),
    validaralimento(AlimentoClave,X).
%Validar el resto del menu
validaralimento([Y|_], X):-
    %Ver si el tipo de comida que escribe coincide con el menu de algun
    %restaurante, P sera el lugar clave como string
    atom_string(Y, P),
    menu(X,_,B), miembro(P, B), nl.
validaralimento(Y, X):-
    Y == [pizza],
    write("¿Algun tipo de pizza especial?"),nl,
    read(L),
    pizza(X,B), miembro(L, B), nl.
validaralimento(Y, X):-
    Y == [sandwich],
    write("¿Algun tipo de pizza especial?"),nl,
    read(L),
    sandwich(X,B), miembro(L, B), nl.
validaralimento(Y, X):-
    Y == [mexicana],
    write("¿Algun tipo de comida mexicana en especifico?"),nl,
    read(L),mexicana(X,B), miembro(L, B), nl.
validaralimento(Y, X):-
    Y == [postres],
    write("¿Algun tipo de postre en especial?"),nl,
    read(L),
    postres(X,B), miembro(L, B), nl.
validaralimento(Y, X):-
    miembro(rapida, Y),
    write("¿Que tipo de comida rapida?"),nl,
    read(L),
    %Ver si el tipo de comida que escribe coincide con la lista de comida rapida
    comidarapida(X,B), miembro(L, B), nl.
validaralimento(_K,_Y):-
    write("Lo sentimos, no se conoce algun restaurante con ese tipo especifico
    de alimentacion"), nl, nl,
    restaurantec_loop().
% Valida si el lugar indicado por el usuario coincide con donde se
% Sintaxis: validarlugar(restaurante, lugar). Se utiliza como:
% validarlugar(rest, lugar), dando los dos argumentos para que retorne
% un booleano.
% Entrada: restaurante, lugar.
% Salida: Booleano indicando si el restaurante y lugar coinciden
% Restricciones: Se deben dar los dos argumentos para que funcione.
validarlugar(K, Y):-
    lugar(K, Y), !.
validarlugar(_K,_Y):-
    write("Lo sentimos, no se conoce ningun restaurante con sus preferencias en
    ese lugar"), nl, nl,
    restaurantec_loop().
% Valida si la cantidad de personas es menor o igual a la
% disponible en el restaurante.
% Sintaxis: validarcapacidad(rest, capacidad).
% Entrada: restaurante, capacidad -> dada por el usuario
% Salida: Booleano indicando si la capacidad solicitada se satisface o
% no.
% Restricciones: Se deben dar los dos argumentos para que funcione

validarcapacidad(K, Y):-
    capacidad(K, T), T >= Y.

validarcapacidad(_K,_Y):-
    write("Lo sentimos, no se conoce ningun restaurante con sus preferencias con
    esa capacidad"), nl, nl,
    restaurantec_loop().

%====================== Checks  ============================%
check_alimento(K):-
%Este fragmento busca si lo que se desea comer se haya disponible en
%algun restaurante
    write("¿Qué desea comer?"), nl,
    read(InputAlimento),
    getInput(InputAlimento,InputAlimentoParseado),
    alimento(InputAlimentoParseado,[],AlimentoClave),
    validaralimento(AlimentoClave, K).
check_ubicacion(K):-
    %Este fragmento revisa que los posibles restaurantes
    %se encuentren en el area indicada
    write("¿Donde se te antoja comer?"), nl,
    read(InputLugar),
    getInput(InputLugar,InputLugarParseado),
    ubicacion(InputLugarParseado,[],[LugarClave|_]),
    atom_string(LugarClave, P),
    validarlugar(K, P).
check_cantidad(K):-
    %Este fragmento compara la cantidad de personas indicadas
    %por el usuario con la capacidad del restaurante
    write("¿Para cuantas personas seria la reservacion?"), nl,
    read(InputPersonas),
    getInput(InputPersonas,InputPersonasParseado),
    personas(InputPersonasParseado,[],[PersonasClave|_]),
    atom_number(PersonasClave, V),
    validarcapacidad(K, V).


%======================  Interfaz   ========================%
% Funcion principal que hace las preguntas al usuario
restaurantec:-
    write("Hola, se comunica con restaurantec, el lugar donde nosotros le recomendamos en base a sus preferencias"),nl,
    restaurantec_loop().

restaurantec_loop():-
    check_alimento(K),
    check_ubicacion(K),
    check_cantidad(K),
    %K es el nombre del restaurante y S su direccion
    direccion(K,S),
    atom_concat("Nuestra sugerencia es: Restaurante ", K, O1),
    atom_concat(O1, " que se ubica ", O2),
    atom_concat(O2, S, O3),

    %O3 es la frase completa de la recomendacion
    write(O3), nl,

    %D son las disposiciones de los restaurantes
    disposiciones(K, D),
    write(D),nl, nl,

    write("Tiene alguna otra consulta?"),nl,
    read(Ans), (Ans="si"->restaurantec_loop();!).
    %Se llama recursivamente por si el usuario quiere volver a consultar.


restaurantec_loop():-
    write("Muchas gracias por usar nuestros servicios").












