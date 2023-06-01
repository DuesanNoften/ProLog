menu(tagliatela, italiano).
menu(kfg, alitas_de_pollo).
menu(manchis, hamburgesas).
menu(casita_frita, empanadas).
menu(upway, sandiwch).
menu(fajitabell, mexicana).
menu(cuina_de_laporta, espanola).
menu(lujos_de_mar_de_plata, argentina).
menu(dolce_far_niente, postres).
menu(tres_pinos, helado).

comida(italiano, [spaguetti, pizza, calzone]).
comida(alitas_de_pollo, [bbq, buffalo, rach]).
comida(hamburgesas, [sencilla, doble_torta, pollo]).
comida(empanadas,[pollo, carne, fijol, arreglada]).
comida(sandiwch, [jamon, pollo, res]).
comida(mexicana, [tacos, burritos, quesadillas]).
comida(espanola,[paella, gambas, calots]).
comida(argentina,[asado, choripan, milanesa]).
comida(postres, [gelato, tiramisu, cannoli]).
comida(helado, [cono_sencillo, cono_doble, canasta]).

direccion(tagliatela, san_pedro).
direccion(kfg, perez_zeledon).
direccion(manchis, oriental).
direccion(casita_frita, occidental).
direccion(upway, carmen).
direccion(fajitabell, san_nicolas).
direccion(cuina_de_laporta, guadalupe).
direccion(lujos_de_mar_de_plata, tres_rios).
direccion(dolce_far_niente, turrialba).
direccion(tres_pinos, el_tejar).

capacidad(tagliatela, 10).
capacidad(kfg, 20).
capacidad(manchis, 15).
capacidad(casita_frita, 15).
capacidad(upway, 20).
capacidad(fajitabell, 10).
capacidad(cuina_de_laporta, 10).
capacidad(lujos_de_mar_de_plata, 15).
capacidad(dolce_far_niente, 20).
capacidad(tres_pinos, 20).

disposiciones(tagliatela, traje_formal).
disposiciones(kfg, pago_en_efectivo).
disposiciones(manchis, pago_solamente_en_tarjeta).
disposiciones(casita_frita, pago_en_efectivo).
disposiciones(upway, llevar_mascarilla).
disposiciones(fajitabell, pago_en_tarjeta).
disposiciones(cuina_de_laporta, traje_formal).
disposiciones(lujos_de_mar_de_plata, utilizar_mascarilla).
disposiciones(dolce_far_niente, pago_en_efectivo).
disposiciones(tres_pinos, solamente_para_llevar).

verbos().

verbos([pedir|S],S).
verbos([pido|S],S).
verbos([pides|S],S).
verbos([pide|S],S).
verbos([pedimos|S],S).
verbos([piden|S],S).

verbos([ordenar|S],S).
verbos([ordenas|S],S).
verbos([ordeno|S],S).
verbos([ordena|S],S).
verbos([ordenamos|S],S).
verbos([ordenan|S],S).

verbos([probar|S],S).
verbos([pruebo|S],S).
verbos([pruebas|S],S).
verbos([prueba|S],S).
verbos([probamos|S],S).
verbos([prueban|S],S).

verbos([recomendar|S],S).
verbos([recomiendo|S],S).
verbos([recomiendas|S],S).
verbos([recomienda|S],S).
verbos([recomendamos|S],S).
verbos([recomiendan|S],S).

verbos([degustar|S],S).
verbos([degusto|S],S).
verbos([degustas|S],S).
verbos([degusta|S],S).
verbos([degustamos|S],S).
verbos([degustan|S],S).

verbos([comprar|S],S).
verbos([compro|S],S).
verbos([compras|S],S).
verbos([compra|S],S).
verbos([compramos|S],S).
verbos([compran|S],S).

verbos([explorar|S],S).
verbos([exploro|S],S).
verbos([exploras|S],S).
verbos([explora|S],S).
verbos([exploramos|S],S).
verbos([exploran|S],S).

verbos([consultar|S],S).
verbos([consulto|S],S).
verbos([consultas|S],S).
verbos([consulta|S],S).
verbos([consultamos|S],S).
verbos([consultan|S],S).

verbos([desear|S],S).
verbos([deseo|S],S).
verbos([deseas|S],S).
verbos([desea|S],S).
verbos([deseamos|S],S).
verbos([desean|S],S).

verbos([viajar|S],S).
verbos([viajo|S],S).
verbos([viajas|S],S).
verbos([viaja|S],S).
verbos([viajamos|S],S).
verbos([viajan|S],S).

verbos([encargar|S],S).
verbos([encargo|S],S).
verbos([encargas|S],S).
verbos([encarga|S],S).
verbos([encargamos|S],S).
verbos([encargan|S],S).

verbos([tomar|S],S).
verbos([tomo|S],S).
verbos([tomas|S],S).
verbos([toma|S],S).
verbos([tomamos|S],S).
verbos([toman|S],S).

verbos([disfrutar|S],S).
verbos([disfruto|S],S).
verbos([disfrutas|S],S).
verbos([disfruta|S],S).
verbos([disfrutamos|S],S).
verbos([disfrutan|S],S).

verbos([saborear|S],S).
verbos([saboreo|S],S).
verbos([saboreas|S],S).
verbos([saborea|S],S).
verbos([saboreamos|S],S).
verbos([saborean|S],S).

verbos([experimentar|S],S).
verbos([experimento|S],S).
verbos([experimentas|S],S).
verbos([experimenta|S],S).
verbos([experimentamos|S],S).
verbos([experimentan|S],S).

verbos([indicar|S],S).
verbos([indico|S],S).
verbos([indicas|S],S).
verbos([indica|S],S).
verbos([indicamos|S],S).
verbos([indican|S],S).

verbos([buscar|S],S).
verbos([busco|S],S).
verbos([buscas|S],S).
verbos([busca|S],S).
verbos([buscamos|S],S).
verbos([buscan|S],S).

verbos([localizar|S],S).
verbos([localizo|S],S).
verbos([localizas|S],S).
verbos([localiza|S],S).
verbos([localizamos|S],S).
verbos([localizan|S],S).

verbos([averiguar|S],S).
verbos([averiguo|S],S).
verbos([averiguas|S],S).
verbos([averiguamos|S],S).
verbos([averiguan|S],S).
verbos([averigua|S],S).

verbos([informar|S],S).
verbos([informo|S],S).
verbos([informas|S],S).
verbos([informa|S],S).
verbos([informamos|S],S).
verbos([informan|S],S).

sujetos([yo|S],S).
sujetos([tu|S],S).
sujetos([el|S],S).
sujetos([ella|S],S).
sujetos([ellos|S],S).
sujetos([ellas|S],S).
sujetos([nosotros|S],S).
sujetos([nosotras|S],S).
sujetos([usted|S],S).
sujetos([josue|S],S).
sujetos([isa|S],S).
sujetos([jordy|S],S).

sustantivos().

determinante([el|S],S).
determinante([la|S],S).

oracion(S0,S):- sintagma_nominal(S0,S1),
    sintagma_verbal(S1,S).

sintagma_nominal(S0,S):- determinante(S0,S1),
    sujetos(S1,S).

sintagma_verbal(S0,S):-verbos(S0,S).

sintagma_verbal(S0,S):-verbos(S0,S1),
    sujetos(S1,S).


