
main = do
        putStrLn "-----Bienvenido al menú principal-----"
        putStrLn "1- Serie Fibonacci"
        putStrLn "2- Presenta 10 números"
        putStrLn "3- Factorial"
        putStrLn "4- Desaparece números"
        putStrLn "5- Palíndromo "
        putStrLn "6- Calculadora"
        putStrLn "7- Salir"
        putStrLn "¿Qué opción quieres?"
        n <- getLine
        let nInt = read n :: Int
        casos nInt

incomenu = do 
        putStrLn "La Opción que ingresaste es incorrecta, por favor vuelve a teclear [1-7]"
        i <- getLine
        let iInt = read i :: Int
        casos iInt
--CASOS DEL MENÚ PRINCIPAL
casos n = do
        case n of
            1 -> seriefibo
            2 -> desplegar
            3 -> factorial
            4 -> desaparecer
            5 -> palindromo
            6 -> calculadora
            7 -> print("Saliendo")
            _ -> incomenu

--------------------------------------------SERIE FIBONACCI------------------------------------------------
ser 0 = 0
ser 1 = 1
ser n = ser (n-1) + ser (n-2)
seriefibo = do 
        putStrLn "Dame la posición que quieres imprimir "
        t <- getLine
        let n1 = read t :: Int
        putStrLn ("El valor en la posición " ++ t ++ " es: ")
        print(ser n1)
        vol
vol = do
        putStrLn "¿Deseas realizar otra búsqueda y/n?"
        g <- getLine
        eva g
eva opc = do
        case opc of 
                "y" -> seriefibo
                "n" -> main
-----------------------------------------------10 NÚMEROS--------------------------------------------------
desplegar = do
        putStrLn "Bienvenido a la opción de 10 números"
        putStrLn "Comenzando a desplegar los números"
        recu 1 
recu n = do 
        print n
        if (n < 10)
                then recu(n+1)
        else do
                putStrLn ("Se llego al limite")
                main
-----------------------------------------------FACTORIAL---------------------------------------------------
fact 0 = 1
fact n = n * fact (n - 1)
factorial = do 
        putStrLn "Bienvenido a la opción de Factorial "
        putStrLn "Dame un número"
        f <- getLine
        let n = read f :: Int
        putStrLn ("El factorial de " ++ f ++ " es: ")
        print (fact n)
        volver 
volver = do
        putStrLn "¿Deseas hacer otra operación y/n?"
        o <- getLine
        opci o 
opci n = do
        case n of
            "y" -> factorial
            "n" -> main
-----------------------------------------------DESAPARECER-------------------------------------------------
desaparecer = do
        putStrLn "Bienvenido a la opción de Desaparecer números"
        putStrLn "Comenzando a desaparecer los números"
        condicional 11
condicional n = do
        let nombres = [0,1,2,3,4,5,6,7,8,9,10]
        if n >= 1
        then do
            --let bInt = read b :: Int
            print (take n nombres)
            condicional(n-1)
        else do
            putStrLn ("Todos los números han desaparecido")
            main

------------------------------------------------PALÍNDROMO-------------------------------------------------
palindromo = do
        putStrLn "Bienvenido a la opción del Palíndromo"
        putStrLn "Dame una cadena: "
        g <- getLine
        let x = reverse g
        let y = x == g
        putStrLn ("La cadena es: ")
        print (y)
        menupalin

menupalin = do
        putStrLn "¿Deseas realizar otra comprobación y/n?"
        res <- getLine
        peticion res

peticion n = do
        case n of
            "y" -> palindromo
            "n" -> main
------------------------------------------------CALCULADORA-------------------------------------------------
-- PREGUNTA PARA UNA NUEVA OPERCIÓN
maindos = do
        putStrLn "¿Deseas realizar otra operación y/n?"
        res <- getLine
        --let op = read res :: Int
        menu res

--PETICIÓN PARA UNA NUEVA OPERACIÓN DE LA CALCULADORA
menu n = do
        case n of
            "y" -> calculadora
            "n" -> main
incorrecta = do 
        putStrLn "La Opción que ingresaste es incorrecta, por favor vuelve a teclear [1-5]"
        h <- getLine
        let hInt = read h :: Int
        casossuma hInt

calculadora = do
        putStrLn "Bienvenido a la calculadora, elige una de las opciones"
        putStrLn "1- Suma"
        putStrLn "2- Resta"
        putStrLn "3- Multiplicación"
        putStrLn "4- División"
        putStrLn "5- Salir al menú principal"
        n <- getLine
        let op = read n::Int
        casossuma op

casossuma n = do
        case n of
            1 -> sumados 
            2 -> restados
            3 -> multidos
            4 -> dividos
            5 -> main
            _ -> incorrecta

suma x y = x + y
sumados = do
        putStrLn "Dame el primer numero"
        x <- getLine
        putStrLn "Dame el segundo numero"
        y <- getLine
        let n1 = read x :: Int
        let n2 = read y :: Int
        putStrLn  ("La suma es:" ++ show (suma n1 n2))
        maindos

resta x y = x - y
restados = do
        putStrLn "Dame el primer numero"
        x <- getLine
        putStrLn "Dame el segundo numero"
        y <- getLine
        let n1 = read x :: Int
        let n2 = read y :: Int
        putStrLn  ("La resta es:" ++ show (resta n1 n2))
        maindos

multi x y = x * y
multidos = do
        putStrLn "Dame el primer numero"
        x <- getLine
        putStrLn "Dame el segundo numero"
        y <- getLine
        let n1 = read x :: Int
        let n2 = read y :: Int
        putStrLn  ("La multiplicación es:" ++ show (multi n1 n2))
        maindos

divi x y = div x y
dividos = do
        putStrLn "Dame el primer numero"
        x <- getLine
        putStrLn "Dame el segundo numero"
        y <- getLine
        let n1 = read x :: Int
        let n2 = read y :: Int
        putStrLn ("La división es:" ++ show (divi n1 n2)  )
        maindos
