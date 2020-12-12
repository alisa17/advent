module Tests10 exposing (..)

import Day10
    exposing
        ( calcPart1
        , calcPart2
        , makeAdapterChain
        , parseInput
        , partitionAdapters
        , sortAdapters
        , testInput1
        , testInput2
        , testInput3
        )
import Expect
import Set
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Day10 format input" <|
            \_ ->
                Expect.equal formattedInput1 (parseInput testInput1)
        , test "Day10 sort testInput1" <|
            \_ ->
                Expect.equal [ 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19 ] (sortAdapters formattedInput1)
        , test "Day10 partition testInput1" <|
            \_ ->
                Expect.equal ( 7, 0, 5 ) (partitionAdapters ( 0, 0, 0 ) (makeAdapterChain formattedInput1))
        , test "Day10 partition testInput2" <|
            \_ ->
                Expect.equal ( 22, 0, 10 ) (partitionAdapters ( 0, 0, 0 ) (makeAdapterChain <| parseInput <| testInput2))
        , test "Day10 partition testInput3" <|
            \_ ->
                Expect.equal ( 65, 0, 25 ) (partitionAdapters ( 0, 0, 0 ) (makeAdapterChain <| parseInput <| testInput3))
        , test "Day10 calcPart1" <|
            \_ ->
                Expect.equal 1625 (calcPart1 testInput3)
        , test "Day10 calcPart2" <|
            \_ ->
                Expect.equal
                    0
                    (calcPart2 testInput3)
        ]


formattedInput1 =
    [ 16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4 ]
