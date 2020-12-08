module Tests7 exposing (..)

import Day7
    exposing
        ( parseRulesString
        , part1Calc
        , part2Calc
        , testInput1
        , testInput2
        , testInput3
        )
import Expect
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Day7 format input" <|
            \_ ->
                Expect.equal formattedInput1 (parseRulesString testInput1)
        , test "Day7 part1 input1" <|
            \_ ->
                Expect.equal 4 (part1Calc testInput1)
        , test "Day7 part1 input3" <|
            \_ ->
                Expect.equal 229 (part1Calc testInput3)
        , test "Day7 part2 input1" <|
            \_ ->
                Expect.equal 32 (part2Calc testInput1)
        , test "Day7 part2 input2" <|
            \_ ->
                Expect.equal 126 (part2Calc testInput2)
        , test "Day7 part2 input3" <|
            \_ ->
                Expect.equal 6683 (part2Calc testInput3)
        ]


formattedInput1 =
    [ ( "light red", [ ( 1, "bright white" ), ( 2, "muted yellow" ) ] )
    , ( "dark orange", [ ( 3, "bright white" ), ( 4, "muted yellow" ) ] )
    , ( "bright white", [ ( 1, "shiny gold" ) ] )
    , ( "muted yellow", [ ( 2, "shiny gold" ), ( 9, "faded blue" ) ] )
    , ( "shiny gold", [ ( 1, "dark olive" ), ( 2, "vibrant plum" ) ] )
    , ( "dark olive", [ ( 3, "faded blue" ), ( 4, "dotted black" ) ] )
    , ( "vibrant plum", [ ( 5, "faded blue" ), ( 6, "dotted black" ) ] )
    , ( "faded blue", [ ( 0, "o other" ) ] )
    , ( "dotted black", [ ( 0, "o other" ) ] )
    ]
