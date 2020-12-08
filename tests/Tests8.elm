module Tests8 exposing (..)

import Day8
    exposing
        ( Step(..)
        , calcPart1
        , calcPart2
        , findPossibleMutations
        , parseInstructions
        , prepareInput
        , testInput1
        , testInput2
        , walkThrough
        , walkThroughWithChange
        )
import Expect
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Day8 format input" <|
            \_ ->
                Expect.equal formattedInput1 (parseInstructions testInput1)
        , test "Day8 prepare input" <|
            \_ ->
                Expect.equal preparedInput1 (prepareInput (parseInstructions testInput1))
        , test "Day8 part1 input1 calc correctly" <|
            \_ ->
                Expect.equal 5 (calcPart1 testInput1)
        , test "Day8 part1 input2 calc correctly" <|
            \_ ->
                Expect.equal 2058 (calcPart1 testInput2)
        , test "Day8 part2 mutates correctly" <|
            \_ ->
                Expect.equal mutations1 (findPossibleMutations preparedInput1)
        , test "Day8 part2 input1 calc correctly" <|
            \_ ->
                Expect.equal 8 (calcPart2 testInput1)
        , test "Day8 part2 input2 calc correctly" <|
            \_ ->
                Expect.equal 1000 (calcPart2 testInput2)
        ]


formattedInput1 =
    [ ( Nop, 0 )
    , ( Acc, 1 )
    , ( Jmp, 4 )
    , ( Acc, 3 )
    , ( Jmp, -3 )
    , ( Acc, -99 )
    , ( Acc, 1 )
    , ( Jmp, -4 )
    , ( Acc, 6 )
    ]


preparedInput1 =
    [ ( Nop, 0, -1 )
    , ( Acc, 1, -1 )
    , ( Jmp, 4, -1 )
    , ( Acc, 3, -1 )
    , ( Jmp, -3, -1 )
    , ( Acc, -99, -1 )
    , ( Acc, 1, -1 )
    , ( Jmp, -4, -1 )
    , ( Acc, 6, -1 )
    ]


mutations1 =
    [ [ ( Jmp, 0, -1 )
      , ( Acc, 1, -1 )
      , ( Jmp, 4, -1 )
      , ( Acc, 3, -1 )
      , ( Jmp, -3, -1 )
      , ( Acc, -99, -1 )
      , ( Acc, 1, -1 )
      , ( Jmp, -4, -1 )
      , ( Acc, 6, -1 )
      ]
    , [ ( Nop, 0, -1 )
      , ( Acc, 1, -1 )
      , ( Nop, 4, -1 )
      , ( Acc, 3, -1 )
      , ( Jmp, -3, -1 )
      , ( Acc, -99, -1 )
      , ( Acc, 1, -1 )
      , ( Jmp, -4, -1 )
      , ( Acc, 6, -1 )
      ]
    , [ ( Nop, 0, -1 )
      , ( Acc, 1, -1 )
      , ( Jmp, 4, -1 )
      , ( Acc, 3, -1 )
      , ( Nop, -3, -1 )
      , ( Acc, -99, -1 )
      , ( Acc, 1, -1 )
      , ( Jmp, -4, -1 )
      , ( Acc, 6, -1 )
      ]
    , [ ( Nop, 0, -1 )
      , ( Acc, 1, -1 )
      , ( Jmp, 4, -1 )
      , ( Acc, 3, -1 )
      , ( Jmp, -3, -1 )
      , ( Acc, -99, -1 )
      , ( Acc, 1, -1 )
      , ( Nop, -4, -1 )
      , ( Acc, 6, -1 )
      ]
    ]
