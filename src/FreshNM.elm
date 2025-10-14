module FreshNM exposing (..)


type alias FreshN a =
    Int -> ( a, Int )


lift : (a -> b) -> FreshN a -> FreshN b
lift f g1 n =
    let
        ( res1, n1 ) =
            g1 n
    in
    ( f res1, n1 )


lift2 : (a -> b -> c) -> FreshN a -> FreshN b -> FreshN c
lift2 f fnA fnB n =
    let
        ( res1, n1 ) =
            fnA n

        ( res2, n2 ) =
            fnB n1
    in
    ( f res1 res2, n2 )


lift3 : (a -> b -> c -> d) -> FreshN a -> FreshN b -> FreshN c -> FreshN d
lift3 f fnA fnB fnC n =
    let
        ( res1, n1 ) =
            fnA n

        ( res2, n2 ) =
            fnB n1

        ( res3, n3 ) =
            fnC n2
    in
    ( f res1 res2 res3, n3 )
