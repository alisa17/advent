module Day12 exposing (..)

-- parseInput : String -> List (List Seat)


parseInput ls =
    String.split "\n" ls


findWay : List String -> DirectionFacing -> ( Int, Int ) -> ( Int, Int )
findWay ls dir ( x, y ) =
    case ls of
        [] ->
            ( x, y )

        h :: rest ->
            let
                directionToMoveIn =
                    String.left 1 h

                unitsToMoveIn =
                    String.dropLeft 1 h
                        |> String.toInt
                        |> Maybe.withDefault 999999
            in
            if directionToMoveIn == "N" then
                findWay rest dir ( x, y + unitsToMoveIn )

            else if directionToMoveIn == "S" then
                findWay rest dir ( x, y - unitsToMoveIn )

            else if directionToMoveIn == "W" then
                findWay rest dir ( x - unitsToMoveIn, y )

            else if directionToMoveIn == "E" then
                findWay rest dir ( x + unitsToMoveIn, y )

            else if directionToMoveIn == "F" then
                case dir of
                    N ->
                        findWay rest dir ( x, y + unitsToMoveIn )

                    E ->
                        findWay rest dir ( x + unitsToMoveIn, y )

                    S ->
                        findWay rest dir ( x, y - unitsToMoveIn )

                    W ->
                        findWay rest dir ( x - unitsToMoveIn, y )

            else
                findWay
                    rest
                    (transformDir dir directionToMoveIn unitsToMoveIn)
                    ( x, y )


findWayWithWayPoint :
    List String
    -> ( ( Int, Int ), ( Int, Int ) )
    -> ( ( Int, Int ), ( Int, Int ) )
findWayWithWayPoint ls ( ( x, y ), ( xW, yW ) ) =
    case ls of
        [] ->
            ( ( x, y ), ( xW, yW ) )

        h :: rest ->
            let
                directionToMoveIn =
                    String.left 1 h

                unitsToMoveIn =
                    String.dropLeft 1 h
                        |> String.toInt
                        |> Maybe.withDefault 999999

                stepX =
                    xW - x

                stepY =
                    yW - y
            in
            if directionToMoveIn == "N" then
                findWayWithWayPoint rest ( ( x, y ), ( xW, yW + unitsToMoveIn ) )

            else if directionToMoveIn == "S" then
                findWayWithWayPoint rest ( ( x, y ), ( xW, yW - unitsToMoveIn ) )

            else if directionToMoveIn == "W" then
                findWayWithWayPoint rest ( ( x, y ), ( xW - unitsToMoveIn, yW ) )

            else if directionToMoveIn == "E" then
                findWayWithWayPoint rest ( ( x, y ), ( xW + unitsToMoveIn, yW ) )

            else if directionToMoveIn == "F" then
                findWayWithWayPoint
                    rest
                    ( ( x + (stepX * unitsToMoveIn), y + (stepY * unitsToMoveIn) )
                    , ( x + (stepX * unitsToMoveIn) + stepX, y + (stepY * unitsToMoveIn) + stepY )
                    )

            else
                let
                    newWpCoord =
                        if (directionToMoveIn == "R" && unitsToMoveIn == 90) || (directionToMoveIn == "L" && unitsToMoveIn == 270) then
                            ( x + stepY, y - stepX )

                        else if (directionToMoveIn == "R" && unitsToMoveIn == 180) || (directionToMoveIn == "L" && unitsToMoveIn == 180) then
                            ( x - stepX, y - stepY )

                        else
                            ( x - stepY, y + stepX )
                in
                findWayWithWayPoint
                    rest
                    ( ( x, y ), newWpCoord )


transformDir : DirectionFacing -> String -> Int -> DirectionFacing
transformDir d t u =
    (if t == "R" then
        getDirectionAngle d + u

     else
        getDirectionAngle d - u
    )
        |> modBy 360
        |> getDirectionBasedOnAngle


type DirectionFacing
    = N
    | S
    | W
    | E


directionAngles : List ( DirectionFacing, Int )
directionAngles =
    [ ( N, 0 )
    , ( E, 90 )
    , ( S, 180 )
    , ( W, 270 )
    ]


getDirectionAngle : DirectionFacing -> Int
getDirectionAngle dir =
    directionAngles
        |> List.filter (Tuple.first >> (==) dir)
        |> List.map Tuple.second
        |> List.head
        |> Maybe.withDefault -9999


getDirectionBasedOnAngle : Int -> DirectionFacing
getDirectionBasedOnAngle a =
    directionAngles
        |> List.filter (Tuple.second >> (==) a)
        |> List.map Tuple.first
        |> List.head
        |> Maybe.withDefault N
