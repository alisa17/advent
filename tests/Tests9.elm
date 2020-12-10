module Tests9 exposing (..)

import Day9
    exposing
        ( calcPart2
        , findAllPossiblePairs
        , findContiguousSet
        , findOffendingNumber
        , findPreviousNumbers
        , parseInput
        , sumUntilFindNumber
        , testInput1
        , testInput2
        )
import Expect
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Day9 format input" <|
            \_ ->
                Expect.equal formattedInput1 (parseInput testInput1)
        , test "Day9 find previous numbers" <|
            \_ ->
                Expect.equal [ 3, 4, 5, 6, 7 ] (findPreviousNumbers 5 7 [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ])
        , test "Day9 find possible pairs" <|
            \_ ->
                Expect.equal [ ( 1, 2 ), ( 1, 3 ), ( 1, 4 ), ( 2, 3 ), ( 2, 4 ), ( 3, 4 ) ] (findAllPossiblePairs [] [ 1, 2, 3, 4 ])
        , test "Day9 find offending number testInput1" <|
            \_ ->
                Expect.equal 127 (findOffendingNumber 5 formattedInput1)
        , test "Day9 find offending number testInput2" <|
            \_ ->
                Expect.equal 20874512 (findOffendingNumber 25 (parseInput testInput2))
        , test "Day9 sumUntilFindNumber testInput1" <|
            \_ ->
                Expect.equal [ 15, 25, 47, 40 ] (sumUntilFindNumber 127 [ 15, 25, 47, 40, 62, 55 ])
        , test "Day9 findContiguousSet testInput1" <|
            \_ ->
                Expect.equal [ 15, 25, 47, 40 ] (findContiguousSet 127 formattedInput1)
        , test "Day9 calcPart2 testInput2" <|
            \_ ->
                Expect.equal 3012420 (calcPart2 testInput2)
        ]


formattedInput1 =
    [ 35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576 ]
