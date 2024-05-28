-- Función para determinar si un número es par o impar
esPar :: Int -> Bool
esPar n = n `mod` 2 == 0

-- Función principal que toma un número de 8 dígitos y devuelve si el número formado por el quinto, sexto y séptimo dígitos es par o impar
numeroQuintoSextoSeptimo :: Int -> String
numeroQuintoSextoSeptimo num =
    let numStr = show num
        numReducido = read $ take 4 $ drop 5 $ dropWhile (== '0') numStr :: Int
    in if esPar numReducido
       then "num" ++ show numReducido ++ " even"
       else "num" ++ show numReducido ++ " odd"

-- Función para calcular el aliquot sum de un número
aliquotSum :: Int -> Int
aliquotSum n =
  let divisores = filter (\x -> n `mod` x == 0) [1..(n `div` 2)]
  in sum divisores

-- Función para determinar si un número es perfecto, abundante o poor
programaEstudiante :: Int -> String
programaEstudiante n
  | aliquot == n = "Engineering"
  | aliquot > n = "Administrative"
  | otherwise = "Humanities"
  where aliquot = aliquotSum n

-- Función para obtener el aliquot sum de los dígitos en la tercera y cuarta posición de un número
clasificarAliquot :: Int -> Int
clasificarAliquot numero =
  let tercera = (numero `div` 10000) `mod` 10
      cuarta = (numero `div` 1000) `mod` 10
  in aliquotSum (tercera * 10 + cuarta)

-- Función principal para el ID del estudiante
estudianteID :: String -> String
estudianteID xs =
  let idNumero = read xs :: Int
      reducido = numeroQuintoSextoSeptimo idNumero
  in "20" ++ take 2 xs ++ "-" ++ [xs !! 2] ++ " " ++ programaEstudiante (clasificarAliquot idNumero) ++ " " ++ reducido

main :: IO()
main = do
    lista <- getLine
    putStrLn $ estudianteID lista



