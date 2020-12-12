module Day10 exposing
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

import Set exposing (Set)


parseInput : String -> List Int
parseInput =
    String.split "\n" >> List.map (String.toInt >> Maybe.withDefault -999999)


testInput1 =
    """16
10
15
5
1
11
7
19
6
12
4"""


calcPart1 inp =
    parseInput inp
        |> makeAdapterChain
        |> partitionAdapters ( 0, 0, 0 )
        |> (\( a, b, c ) -> a * c)


calcPart2 inp =
    let
        ls =
            inp
                |> parseInput
                |> List.sort

        nodeNumberOfAncestors =
            List.map (\x -> ( x, findPrev x ls, 0 )) ls
    in
    nodeNumberOfAncestors
        |> List.indexedMap
            (\i ( a, b, c ) ->
                if i == 0 then
                    ( a, b, 1 )

                else
                    ( a, b, c )
            )
        |> calcIndexes 1
        |> List.reverse
        |> List.map (\( a, b, c ) -> c)
        |> List.head
        |> Maybe.withDefault 0


calcIndexes : Int -> List ( Int, List Int, Int ) -> List ( Int, List Int, Int )
calcIndexes i ls =
    if i == List.length ls then
        ls

    else
        let
            elToCalc =
                List.indexedMap (\ind x -> ( ind, x )) ls
                    |> List.filter (\( ind, x ) -> ind == i)
                    |> List.map Tuple.second
                    |> List.head
                    |> Maybe.withDefault ( 0, [], 0 )

            elAncestors =
                (\( a, b, c ) -> b) elToCalc

            allAncestors =
                List.filter (\( a, b, c ) -> List.member a elAncestors) ls

            ancestorsIndex =
                List.map (\( a, b, c ) -> c) allAncestors
                    |> List.foldl (+) 0

            elAncestorsTotal =
                if i == 0 then
                    1

                else
                    elAncestors
                        |> (\ancestors ->
                                List.filter (\( a, b, c ) -> List.member a ancestors) ls
                           )
                        |> List.map (\( a, b, c ) -> c)
                        |> List.foldl (+) 0

            newEl =
                (\( a, b, c ) -> ( a, b, elAncestorsTotal )) elToCalc

            newLs =
                List.map
                    (\el ->
                        if el == elToCalc then
                            newEl

                        else
                            el
                    )
                    ls
        in
        calcIndexes (i + 1) newLs


findPrev : Int -> List Int -> List Int
findPrev n ls =
    ls |> List.filter (\x -> ((n - x) >= 1) && ((n - x) <= 3))


sortAdapters =
    List.sort


findDeviceRating : List Int -> Int
findDeviceRating =
    List.maximum >> Maybe.withDefault -999999 >> (+) 3


makeAdapterChain : List Int -> List Int
makeAdapterChain ls =
    0 :: findDeviceRating ls :: ls |> sortAdapters


partitionAdapters : ( Int, Int, Int ) -> List Int -> ( Int, Int, Int )
partitionAdapters ( a, b, c ) ls =
    case ls of
        [] ->
            ( a, b, c )

        [ h ] ->
            ( a, b, c )

        [ h, r ] ->
            if (r - h) == 1 then
                ( a + 1, b, c )

            else if (r - h) == 2 then
                ( a, b + 1, c )

            else
                ( a, b, c + 1 )

        h :: r :: rest ->
            if (r - h) == 1 then
                partitionAdapters ( a + 1, b, c ) (r :: rest)

            else if (r - h) == 2 then
                partitionAdapters ( a, b + 1, c ) (r :: rest)

            else
                partitionAdapters ( a, b, c + 1 ) (r :: rest)


testInput2 =
    """0
28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"""


testInput3 =
    """0
67
118
90
41
105
24
137
129
124
15
59
91
94
60
108
63
112
48
62
125
68
126
131
4
1
44
77
115
75
89
7
3
82
28
97
130
104
54
40
80
76
19
136
31
98
110
133
84
2
51
18
70
12
120
47
66
27
39
109
61
34
121
38
96
30
83
69
13
81
37
119
55
20
87
95
29
88
111
45
46
14
11
8
74
101
73
56
132
23"""
