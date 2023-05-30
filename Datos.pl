menu(tagliatela, [italiano, [spaguetti, pizza, calzone]]).
menu(kfg, [alitas_de_pollo, [bbq, buffalo, rach]]).
menu(manchis, [hamburgesas, [sencilla, doble_torta, pollo]]).
menu(casita_frita, [empanadas,[pollo, carne, fijol, arreglada]]).
menu(upway,[sandiwch, [jamon, pollo, res]]).
menu(fajitabell, [mexicano, [tacos, burritos, quesadillas]]).
menu(cuina_de_laporta, [espa�ola,[paella, gambas, calots]]).
menu(lujos_de_mar_de_plata, [argentina,[asado, choripan, milanesa]]).
menu(dolce_far_niente, [postres, [gelato, tiramisu, cannoli]]).
menu(tres_pinos, [helado, [cono_sencillo, cono_doble, canasta]]).

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

verbos(pedir).
verbos(pido).
verbos(pides).
verbos(pide).
verbos(pedimos).
verbos(piden).

verbos(ordenar).
verbos(ordenas).
verbos(ordeno).
verbos(ordena).
verbos(ordenamos).
verbos(ordenan).

verbos(probar).
verbos(pruebo).
verbos(pruebas).
verbos(prueba).
verbos(probamos).
verbos(prueban).

verbos(recomendar).
verbos(recomiendo).
verbos(recomiendas).
verbos(recomienda).
verbos(recomendamos).
verbos(recomiendan).

verbos(degustar).
verbos(degusto).
verbos(degustas).
verbos(degusta).
verbos(degustamos).
verbos(degustan).

verbos(comprar).
verbos(compro).
verbos(compras).
verbos(compra).
verbos(compramos).
verbos(compran).

verbos(explorar).
verbos(exploro).
verbos(exploras).
verbos(explora).
verbos(exploramos).
verbos(exploran).

verbos(consultar).
verbos(consulto).
verbos(consultas).
verbos(consulta).
verbos(consultamos).
verbos(consultan).

verbos(desear).
verbos(deseo).
verbos(deseas).
verbos(desea).
verbos(deseamos).
verbos(desean).

verbos(viajar).
verbos(viajo).
verbos(viajas).
verbos(viaja).
verbos(viajamos).
verbos(viajan).

verbos(encargar).
verbos(encargo).
verbos(encargas).
verbos(encarga).
verbos(encargamos).
verbos(encargan).

verbos(tomar).
verbos(tomo).
verbos(tomas).
verbos(toma).
verbos(tomamos).
verbos(toman).

verbos(disfrutar).
verbos(disfruto).
verbos(disfrutas).
verbos(disfruta).
verbos(disfrutamos).
verbos(disfrutan).

verbos(saborear).
verbos(saboreo).
verbos(saboreas).
verbos(saborea).
verbos(saboreamos).
verbos(saborean).

verbos(experimentar).
verbos(experimento).
verbos(experimentas).
verbos(experimenta).
verbos(experimentamos).
verbos(experimentan).

verbos(indicar).
verbos(indico).
verbos(indicas).
verbos(indica).
verbos(indicamos).
verbos(indican).

verbos(buscar).
verbos(busco).
verbos(buscas).
verbos(busca).
verbos(buscamos).
verbos(buscan).

verbos(localizar).
verbos(localizo).
verbos(localizas).
verbos(localiza).
verbos(localizamos).
verbos(localizan).

verbos(averiguar).
verbos(averiguo).
verbos(averiguas).
verbos(averiguamos).
verbos(averiguan).
verbos(averigua).

verbos(informar).
verbos(informo).
verbos(informas).
verbos(informa).
verbos(informamos).
verbos(informan).

sujetos(yo).
sujetos(tu).
sujetos(el).
sujetos(ella).
sujetos(ellos).
sujetos(ellas).
sujetos(nosotros).
sujetos(nosotras).
sujetos(usted).

sustantivos().

oracion(S0,S):- sintagma_nominal(S0,S1),
    sintagma_verbal(S1,S).

sintagma_nominal(S0,S):- determinante(S0,S1),
    nombre(S1,S).

sintagma_verbal(S0,S):-verbo(S0,S).

sintagma_verbal(S0,S):-verbo(S0,S1),
    nombre(S1,S).


