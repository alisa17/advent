module Day11 exposing
    ( Seat(..)
    , calcPart1
    , calcPart2
    , findAdjacents
    , findFirstSeats
    , parseInput
    , sitPeople
    , sitPeople2
    , sitPeople3
    , sitPeoplePart2
    , testInput1
    , testInput2
    )

import Set exposing (Set)


calcPart1 inp =
    parseInput inp
        |> sitPeople2 []
        |> List.map (List.filter ((==) Taken))
        |> List.map List.length
        |> List.foldl (+) 0


calcPart2 inp =
    parseInput inp
        |> sitPeople3 []
        |> List.map (List.filter ((==) Taken))
        |> List.map List.length
        |> List.foldl (+) 0


parseInput : String -> List (List Seat)
parseInput ls =
    String.split "\n" ls
        |> List.map
            (\x ->
                String.split "" x
                    |> List.map
                        (\y ->
                            if y == "L" then
                                Empty

                            else if y == "#" then
                                Taken

                            else
                                Floor
                        )
            )


testInput1 =
    """L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"""


testInput2 =
    """"""


type Seat
    = Taken
    | Floor
    | Empty


sitPeople : List (List Seat) -> List (List Seat)
sitPeople ls =
    ls
        |> List.indexedMap
            (\i x ->
                List.indexedMap
                    (\ind y ->
                        case y of
                            Floor ->
                                y

                            Taken ->
                                let
                                    allAdjacents =
                                        findAdjacents ( i, ind ) ls

                                    are4AdjOccupied =
                                        allAdjacents
                                            |> List.filter ((==) Taken)
                                            |> (\z -> List.length z >= 4)
                                in
                                if are4AdjOccupied then
                                    Empty

                                else
                                    y

                            Empty ->
                                let
                                    allAdjacents =
                                        findAdjacents ( i, ind ) ls

                                    areAllAdjVacant =
                                        allAdjacents
                                            |> List.all (\a -> (a == Empty) || (a == Floor))
                                in
                                if areAllAdjVacant then
                                    Taken

                                else
                                    y
                    )
                    x
            )


sitPeoplePart2 : List (List Seat) -> List (List Seat)
sitPeoplePart2 ls =
    ls
        |> List.indexedMap
            (\i x ->
                List.indexedMap
                    (\ind y ->
                        case y of
                            Floor ->
                                y

                            Taken ->
                                let
                                    allAdjacents =
                                        findFirstSeats ( i, ind ) ls

                                    are5AdjOccupied =
                                        allAdjacents
                                            |> List.filter ((==) Taken)
                                            |> (\z -> List.length z >= 5)
                                in
                                if are5AdjOccupied then
                                    Empty

                                else
                                    y

                            Empty ->
                                let
                                    allAdjacents =
                                        findFirstSeats ( i, ind ) ls

                                    areAllAdjVacant =
                                        allAdjacents
                                            |> List.all (\a -> (a == Empty) || (a == Floor))
                                in
                                if areAllAdjVacant then
                                    Taken

                                else
                                    y
                    )
                    x
            )


sitPeople2 : List (List Seat) -> List (List Seat) -> List (List Seat)
sitPeople2 lsPrev ls =
    if ls == lsPrev then
        ls

    else
        sitPeople2 ls (sitPeople ls)


sitPeople3 : List (List Seat) -> List (List Seat) -> List (List Seat)
sitPeople3 lsPrev ls =
    if ls == lsPrev then
        ls

    else
        sitPeople3 ls (sitPeoplePart2 ls)


findAdjacents : ( Int, Int ) -> List (List a) -> List a
findAdjacents ( row, column ) ls =
    let
        sameRow =
            getItemWithIndex row ls

        adjacentSameRow =
            case sameRow of
                Nothing ->
                    []

                Just same ->
                    [ getItemWithIndex (column - 1) same
                    , getItemWithIndex (column + 1) same
                    ]

        prevRow =
            getItemWithIndex (row - 1) ls

        adjacentPrevRow =
            case prevRow of
                Nothing ->
                    []

                Just prev ->
                    [ getItemWithIndex (column - 1) prev
                    , getItemWithIndex column prev
                    , getItemWithIndex (column + 1) prev
                    ]

        nextRow =
            getItemWithIndex (row + 1) ls

        adjacentNextRow =
            case nextRow of
                Nothing ->
                    []

                Just next ->
                    [ getItemWithIndex (column - 1) next
                    , getItemWithIndex column next
                    , getItemWithIndex (column + 1) next
                    ]
    in
    adjacentPrevRow
        ++ adjacentSameRow
        ++ adjacentNextRow
        |> List.filterMap identity


findFirstSeats : ( Int, Int ) -> List (List Seat) -> List Seat
findFirstSeats ( row, column ) ls =
    let
        upBlock =
            List.take row ls

        upUp =
            upBlock
                |> List.map (getItemWithIndex column)
                |> List.reverse
                |> List.filterMap identity
                |> List.filter ((/=) Floor)
                |> List.head

        upLeft =
            findUpLeft (column - 1) (List.reverse upBlock) []
                |> List.filterMap identity
                |> List.filter ((/=) Floor)
                |> List.head

        upRight =
            findUpRight (column + 1) (List.reverse upBlock) []
                |> List.filterMap identity
                |> List.filter ((/=) Floor)
                |> List.head

        downBlock =
            List.drop (row + 1) ls

        downDown =
            downBlock
                |> List.map (getItemWithIndex column)
                |> List.filterMap identity
                |> List.filter ((/=) Floor)
                |> List.head

        downLeft =
            findUpLeft (column - 1) downBlock []
                |> List.filterMap identity
                |> List.filter ((/=) Floor)
                |> List.head

        downRight =
            findUpRight (column + 1) downBlock []
                |> List.filterMap identity
                |> List.filter ((/=) Floor)
                |> List.head

        ownRow =
            getItemWithIndex row ls |> Maybe.withDefault []

        left =
            List.take column ownRow
                |> List.reverse
                |> List.filter ((/=) Floor)
                |> List.head

        right =
            List.drop (column + 1) ownRow
                |> List.filter ((/=) Floor)
                |> List.head
    in
    [ upLeft, upUp, upRight, downDown, downLeft, downRight, right, left ]
        |> List.filterMap identity


findUpLeft : Int -> List (List a) -> List (Maybe a) -> List (Maybe a)
findUpLeft i ls lsMaking =
    case ls of
        [] ->
            List.reverse lsMaking

        h :: rest ->
            let
                upLeftItem =
                    getItemWithIndex i h
            in
            findUpLeft (i - 1) rest (upLeftItem :: lsMaking)


findUpRight : Int -> List (List a) -> List (Maybe a) -> List (Maybe a)
findUpRight i ls lsMaking =
    case ls of
        [] ->
            List.reverse lsMaking

        h :: rest ->
            let
                upLeftItem =
                    getItemWithIndex i h
            in
            findUpRight (i + 1) rest (upLeftItem :: lsMaking)


getItemWithIndex : Int -> List a -> Maybe a
getItemWithIndex i ls =
    List.indexedMap (\ind x -> ( ind, x )) ls
        |> List.filter (\x -> Tuple.first x == i)
        |> List.map Tuple.second
        |> List.head
