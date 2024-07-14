type Complejo = (Float, Float)

absoluto :: Float -> Float 
absoluto n = sqrt (n ^ 2) 

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Ejercicio 1

re :: Complejo -> Float
re c = fst c

im :: Complejo -> Float
im c = snd c

suma :: Complejo -> Complejo -> Complejo
suma c1 c2 = ((re c1 + re c2), (im c1 + im c2)) 

producto :: Complejo -> Complejo -> Complejo
producto c1 c2 = (parteReal, parteImaginaria)
                where parteReal = (re c1 * re c2 + (-1) * im c1 * im c2)
                      parteImaginaria = (re c1 * im c2 + im c1 * re c2) 


conjugado :: Complejo -> Complejo
conjugado c1 = (re c1, (-1) * im c1)

inverso :: Complejo -> Complejo
inverso c1 = (re (conjugado c1) / (modulo c1 ^ 2), im (conjugado c1) / (modulo c1 ^ 2))  

-- CONSULTAR
cociente :: Complejo -> Complejo -> Complejo
cociente c1 c2 | c2 /= (0, 0) = producto c1 (inverso c2)

--CONSULTAR
potencia :: Complejo -> Integer -> Complejo
potencia c1 n   | absoluto coseno < 0.001 && absoluto coseno > 0.00 = (0, r ^ n * seno)
                | absoluto seno < 0.001 && absoluto seno > 0.00 = (r ^ n * coseno, 0)
                | otherwise = (r ^ n * coseno, r ^ n * seno)
                where r = modulo c1
                      coseno = cos (fromInteger (n) * argumento c1)
                      seno = sin (fromInteger (n) * argumento c1)


raicesCuadratica :: Float -> Float -> Float -> (Complejo, Complejo)
raicesCuadratica a b c  | d < 0 = (((-b) / (2 * a), sqrt (-d) / (2 * a)), ((-b) / (2 * a), (-1) * sqrt (-d) / (2 * a)))
                        | otherwise = ((((-b) + sqrt d) / (2 * a), 0), (((-b) - sqrt d) / (2 * a), 0))
                        where d = (b ^ 2  + (-4) * a * c)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Ejercicio 2

modulo :: Complejo -> Float
modulo c1 = sqrt ((re c1) ^ 2 + (im c1) ^ 2)

distancia:: Complejo -> Complejo -> Float
distancia c1 c2 = modulo (re c1 - re c2, im c1 - im c2)

argumento :: Complejo -> Float
argumento c1 = atan (im c1 / re c1)

pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas r o | r >= 0 && o < 2 * (pi) && o >= 0 = (r * cos o, r * sin o)

raizCuadrada :: Complejo -> (Complejo,Complejo)
raizCuadrada c1 | re c1 < 0 && absoluto (im c1) == 0 = ((0, sqrt(-(re c1))), (0, - (sqrt(-(re c1)))))
                | re c1 > 0 && absoluto (im c1) == 0 = ((sqrt(re c1), 0), (-(sqrt(re c1)), 0))
                | otherwise = (c, (producto c (-1, 0)))
                where c = pasarACartesianas (sqrt (modulo c1)) ((argumento c1) / 2)

raicesCuadraticaCompleja :: Complejo -> Complejo -> Complejo -> (Complejo,Complejo)
raicesCuadraticaCompleja c1 c2 c3 | c1 /= (0, 0) = (cociente (suma menosB (fst raicesCuadradas)) dosA, cociente (suma menosB (snd raicesCuadradas)) dosA)  
                                  where menosB = producto (-1, 0) c2 
                                        bCuadrado = potencia c2 2
                                        menos4ac = producto (-1,0) (producto (4,0) (producto c1 c3)) 
                                        dosA = producto (2, 0) c1
                                        raicesCuadradas = raizCuadrada (suma bCuadrado menos4ac) 
                                        



