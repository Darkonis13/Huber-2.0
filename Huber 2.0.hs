import Text.Show.Functions
import Data.List

--PUNTO 1
data Chofer = Chofer {nombreDeChofer:: String,kilometraje::Int, viajesHechos::[Viaje] , condicion::(Viaje->Bool)}

data Cliente = Cliente {nombreDeCliente::String, viveEn::String}

data Viaje = Viaje {fecha::(Int,Int,Int), clienteQueLoToma::Cliente, costo::Int}


--PUNTO 2
cualquierViaje :: Viaje->Bool
cualquierViaje _ = True

condicionDe200 :: Viaje->Bool
condicionDe200 = (>200).costo

masDeNLetras :: Int->Viaje->Bool
masDeNLetras cantidadDeLetras = (>cantidadDeLetras).length.nombreDeCliente.clienteQueLoToma

clienteNoViveEn :: String->Viaje->Bool
clienteNoViveEn lugar = (/=lugar).viveEn.clienteQueLoToma

--PUNTO 3
lucas = Cliente {nombreDeCliente = "Lucas", viveEn = "Victoria"}

daniel = Chofer {nombreDeChofer="Daniel", kilometraje= 23500,viajesHechos=[viajeConLucas], condicion = clienteNoViveEn "Olivos"}

viajeConLucas = Viaje {fecha = (20,04,2017), clienteQueLoToma = lucas ,costo = 150}

alejandra = Chofer {nombreDeChofer= "Alejandra", kilometraje = 180000, viajesHechos=[], condicion = cualquierViaje}

--PUNTO 4
choferPuedeTomarViaje :: Chofer->Viaje->Bool
choferPuedeTomarViaje unChofer = condicion unChofer

--PUNTO 5
liquidacionDeUnChofer :: Chofer -> Int
liquidacionDeUnChofer = sum . map costo . viajesHechos

--PUNTO 6
realizarViaje :: Viaje->[Chofer]->Chofer
realizarViaje unViaje = efectuarViaje unViaje . choferConMenosViajes . filtrarChoferes unViaje

filtrarChoferes :: Viaje->[Chofer]->[Chofer]
filtrarChoferes unViaje = filter (condicionDeViaje unViaje)

condicionDeViaje :: Viaje->Chofer->Bool
condicionDeViaje unViaje unChofer = condicion unChofer $ unViaje

choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes choferes = foldl1 compararViajes choferes


compararViajes ::  Chofer -> Chofer -> Chofer
compararViajes chofer1 chofer2
 |length(viajesHechos chofer1)>length(viajesHechos chofer2) = chofer2
 |otherwise = chofer1

efectuarViaje :: Viaje->Chofer->Chofer
efectuarViaje unViaje unChofer = unChofer {viajesHechos = (viajesHechos unChofer) ++ [unViaje]}

--PUNTO 7
--a
nitoInfy = Chofer {nombreDeChofer="Nito Infy", kilometraje = 70000, viajesHechos = listaInfinitaDeViajes, condicion = clienteAlMenos3Letras}
viajeConLucasInfinito = Viaje {fecha = (11,03,2017), costo = 50, clienteQueLoToma = lucas }

clienteAlMenos3Letras :: Viaje->Bool
clienteAlMenos3Letras = (>3) . length. nombreDeCliente. clienteQueLoToma

repetirViaje viaje = viaje : repetirViaje viaje

listaInfinitaDeViajes = repetirViaje viajeConLucasInfinito

--b: No se puede, puesto que necesito la lista de viajes y esta no se terminará de generar nunca.
--c: Si se puede, ya que no hace falta la lista de viajes, sino la condición.

--PUNTO 8 (1 punto) Inferir el tipo de la función gōngnéng

gongNeng :: (Ord b) => b->(b->Bool)->(a->b)->[a]->b
gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3
