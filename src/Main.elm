module Main exposing (main)

import Browser
import Html exposing (Html, br, button, div, form, input, span, text)
import Html.Attributes as A exposing (class, style)
import Html.Events exposing (onClick, onInput, onSubmit)
import Random
import Set exposing (Set)



-- CARDS AND DECK


type Kind
    = KNum Int
    | KPlus Int
    | KTimes2
    | KFlip3
    | KFreeze
    | KSecond


bonusKinds : List Kind
bonusKinds =
    [ KPlus 2, KPlus 4, KPlus 6, KPlus 8, KPlus 10, KTimes2 ]


actionKinds : List Kind
actionKinds =
    [ KFlip3, KFreeze, KSecond ]


allKinds : List Kind
allKinds =
    List.map KNum (List.range 0 12) ++ bonusKinds ++ actionKinds


{-| Copies of each card in the 94-card Flip 7 deck.
-}
deckCount : Kind -> Int
deckCount kind =
    case kind of
        KNum 0 ->
            1

        KNum n ->
            n

        KPlus _ ->
            1

        KTimes2 ->
            1

        _ ->
            3


kindLabel : Kind -> String
kindLabel kind =
    case kind of
        KNum n ->
            String.fromInt n

        KPlus n ->
            "+" ++ String.fromInt n

        KTimes2 ->
            "×2"

        KFlip3 ->
            "Flip Three"

        KFreeze ->
            "Freeze"

        KSecond ->
            "Second Chance"


kindChip : Kind -> String
kindChip kind =
    case kind of
        KFlip3 ->
            "Flip 3"

        KSecond ->
            "2nd Chance"

        _ ->
            kindLabel kind


isAction : Kind -> Bool
isAction kind =
    List.member kind actionKinds


cardNumber : Kind -> Maybe Int
cardNumber kind =
    case kind of
        KNum n ->
            Just n

        _ ->
            Nothing


cardPlus : Kind -> Maybe Int
cardPlus kind =
    case kind of
        KPlus n ->
            Just n

        _ ->
            Nothing



-- MODEL


type alias Player =
    { id : Int
    , name : String
    , emoji : String
    , total : Int
    , hand : List Kind
    , busted : Bool
    , bustCard : Maybe Kind
    }


type Screen
    = Setup
    | Playing


type alias Model =
    { screen : Screen
    , players : List Player
    , nameInput : String
    , seed : Random.Seed
    , nextId : Int
    , round : Int
    , openId : Maybe Int
    , nerds : Bool
    , flipFrom : Maybe Int
    , bustPicking : Maybe Int
    , discards : List Kind
    , confirmNew : Bool
    , winnerShown : Bool
    , finished : Bool
    }


winningScore : Int
winningScore =
    200


maxPlayers : Int
maxPlayers =
    12


emojiPool : List String
emojiPool =
    [ "🦊", "🐙", "🦖", "🐸", "🐼", "🦄", "🐧", "🦉", "🐢", "🐳", "🦁", "🐯", "🐨", "🐵", "🦋", "🐝", "🦀", "🐰", "🐷", "🦔" ]


init : Int -> ( Model, Cmd Msg )
init flags =
    ( { screen = Setup
      , players = []
      , nameInput = ""
      , seed = Random.initialSeed flags
      , nextId = 1
      , round = 1
      , openId = Nothing
      , nerds = False
      , flipFrom = Nothing
      , bustPicking = Nothing
      , discards = []
      , confirmNew = False
      , winnerShown = False
      , finished = False
      }
    , Cmd.none
    )


assignEmoji : Random.Seed -> List Player -> ( String, Random.Seed )
assignEmoji seed players =
    let
        used =
            List.map .emoji players

        free =
            List.filter (\e -> not (List.member e used)) emojiPool

        pool =
            if List.isEmpty free then
                emojiPool

            else
                free

        ( index, nextSeed ) =
            Random.step (Random.int 0 (List.length pool - 1)) seed

        pick =
            pool |> List.drop index |> List.head |> Maybe.withDefault "🎲"
    in
    ( pick, nextSeed )



-- SCORING


handScore : Player -> Int
handScore p =
    if p.busted then
        0

    else
        let
            nums =
                List.filterMap cardNumber p.hand

            base =
                List.sum nums

            doubled =
                if List.member KTimes2 p.hand then
                    base * 2

                else
                    base

            plus =
                List.sum (List.filterMap cardPlus p.hand)
        in
        doubled + plus + flip7Bonus p


flip7Bonus : Player -> Int
flip7Bonus p =
    if not p.busted && Set.size (Set.fromList (List.filterMap cardNumber p.hand)) >= 7 then
        15

    else
        0


tapCard : Kind -> Player -> Player
tapCard kind p =
    if p.busted then
        p

    else if isAction kind then
        { p | hand = p.hand ++ [ kind ] }

    else if List.member kind p.hand then
        { p | hand = removeFirst kind p.hand }

    else
        { p | hand = p.hand ++ [ kind ] }


removeFirst : Kind -> List Kind -> List Kind
removeFirst kind cards =
    case cards of
        [] ->
            []

        c :: rest ->
            if c == kind then
                rest

            else
                c :: removeFirst kind rest


removeAt : Int -> List a -> List a
removeAt index items =
    List.take index items ++ List.drop (index + 1) items


undoLast : Player -> Player
undoLast p =
    if p.busted then
        { p | busted = False, bustCard = Nothing }

    else
        { p | hand = List.take (List.length p.hand - 1) p.hand }


bustWith : Maybe Kind -> Player -> Player
bustWith card p =
    { p | busted = True, bustCard = card }


seenCards : Player -> List Kind
seenCards p =
    case p.bustCard of
        Just k ->
            p.hand ++ [ k ]

        Nothing ->
            p.hand


updatePlayer : Int -> (Player -> Player) -> List Player -> List Player
updatePlayer id change players =
    List.map
        (\p ->
            if p.id == id then
                change p

            else
                p
        )
        players


winnerOf : List Player -> Maybe Player
winnerOf players =
    players
        |> List.sortBy (\p -> negate p.total)
        |> List.head


resetPlayers : List Player -> List Player
resetPlayers players =
    List.map (\p -> { p | total = 0, hand = [], busted = False, bustCard = Nothing }) players



-- CARD COUNTING


deckTotal : Int
deckTotal =
    List.sum (List.map deckCount allKinds)


countOf : Kind -> List Kind -> Int
countOf kind cards =
    List.length (List.filter (\c -> c == kind) cards)


{-| Cards in front of players this round, busted hands included.
-}
seenThisRound : Model -> List Kind
seenThisRound model =
    List.concatMap seenCards model.players


{-| True once the draw pile has run out and the earlier rounds' discards
have been shuffled back in. Cards in front of players stay out of that shuffle.
-}
pileReshuffled : Model -> Bool
pileReshuffled model =
    List.length model.discards + List.length (seenThisRound model) >= deckTotal


{-| True only if a card was actually drawn after the pile ran dry, which is the
moment the earlier discards really get shuffled back in. Used at the end of a round.
-}
reshuffleHappened : Model -> Bool
reshuffleHappened model =
    List.length model.discards + List.length (seenThisRound model) > deckTotal


{-| Copies of a card still in the draw pile.

Before a reshuffle the pile is the deck minus earlier rounds' discards minus this
round's cards. After one, every card that is not in front of a player is in the pile.

-}
leftCount : Model -> Kind -> Int
leftCount model kind =
    let
        inPlay =
            countOf kind (seenThisRound model)
    in
    if pileReshuffled model then
        max 0 (deckCount kind - inPlay)

    else
        max 0 (deckCount kind - countOf kind model.discards - inPlay)


deckLeft : Model -> List ( Kind, Int )
deckLeft model =
    List.map (\k -> ( k, leftCount model k )) allKinds


unseenTotal : Model -> Int
unseenTotal model =
    deckLeft model |> List.map Tuple.second |> List.sum


cardValue : Int -> Kind -> Float
cardValue numSum kind =
    case kind of
        KNum n ->
            toFloat n

        KPlus n ->
            toFloat n

        KTimes2 ->
            toFloat numSum

        _ ->
            0


isHeldNumber : Set Int -> Kind -> Bool
isHeldNumber held kind =
    case kind of
        KNum n ->
            Set.member n held

        _ ->
            False


type alias NextStats =
    { bust : Float
    , gain : Float
    , net : Float
    , bustCards : Int
    , unseen : Int
    }


nextStats : Model -> Player -> Maybe NextStats
nextStats model p =
    let
        deck =
            deckLeft model

        unseen =
            List.sum (List.map Tuple.second deck)

        nums =
            List.filterMap cardNumber p.hand

        held =
            Set.fromList nums

        numSum =
            List.sum nums

        bustCards =
            deck
                |> List.filter (\( k, _ ) -> isHeldNumber held k)
                |> List.map Tuple.second
                |> List.sum

        safeValue =
            deck
                |> List.filter (\( k, _ ) -> not (isHeldNumber held k))
                |> List.map (\( k, c ) -> toFloat c * cardValue numSum k)
                |> List.sum

        safeCards =
            unseen - bustCards

        atRisk =
            toFloat (handScore p)
    in
    if unseen <= 0 then
        Nothing

    else
        Just
            { bust = toFloat bustCards / toFloat unseen
            , gain =
                if safeCards > 0 then
                    safeValue / toFloat safeCards

                else
                    0
            , net = safeValue / toFloat unseen - toFloat bustCards / toFloat unseen * atRisk
            , bustCards = bustCards
            , unseen = unseen
            }


{-| What drawing one card does to a hand: Nothing means it busts them,
otherwise the new held numbers, new number total and points gained.
-}
drawEffect : Set Int -> Int -> Kind -> Maybe ( Set Int, Int, Float )
drawEffect held numSum kind =
    case kind of
        KNum n ->
            if Set.member n held then
                Nothing

            else
                Just ( Set.insert n held, numSum + n, toFloat n )

        KPlus n ->
            Just ( held, numSum, toFloat n )

        KTimes2 ->
            Just ( held, numSum, toFloat numSum )

        _ ->
            Just ( held, numSum, 0 )


decrementAt : Int -> List ( Kind, Int ) -> List ( Kind, Int )
decrementAt target deck =
    List.indexedMap
        (\i ( k, c ) ->
            if i == target then
                ( k, c - 1 )

            else
                ( k, c )
        )
        deck


{-| Returns ( chance of surviving all draws, expected points gained summed over
surviving paths weighted by probability ).
-}
flipGo : Int -> List ( Kind, Int ) -> Set Int -> Int -> ( Float, Float )
flipGo draws deck held numSum =
    let
        total =
            List.sum (List.map Tuple.second deck)

        step ( idx, ( kind, count ) ) ( surviveAcc, gainAcc ) =
            if count <= 0 then
                ( surviveAcc, gainAcc )

            else
                case drawEffect held numSum kind of
                    Nothing ->
                        ( surviveAcc, gainAcc )

                    Just ( held2, num2, gain ) ->
                        let
                            prob =
                                toFloat count / toFloat total

                            ( s, g ) =
                                flipGo (draws - 1) (decrementAt idx deck) held2 num2
                        in
                        ( surviveAcc + prob * s, gainAcc + prob * (s * gain + g) )
    in
    if draws <= 0 || total <= 0 then
        ( 1, 0 )

    else
        List.foldl step ( 0, 0 ) (List.indexedMap Tuple.pair deck)


{-| ( bust chance over three draws, average points if they survive )
-}
flipThreeStats : Model -> Player -> ( Float, Float )
flipThreeStats model p =
    let
        nums =
            List.filterMap cardNumber p.hand

        ( survive, gainSum ) =
            flipGo 3 (deckLeft model) (Set.fromList nums) (List.sum nums)
    in
    if survive <= 0 then
        ( 1, 0 )

    else
        ( 1 - survive, gainSum / survive )



-- UPDATE


type Msg
    = NameInput String
    | AddPlayer
    | RemovePlayer Int
    | StartGame
    | ToggleOpen Int
    | Tap Int Kind
    | Undo Int
    | StartBust Int
    | PickBust Int Kind
    | BustNoCard Int
    | CancelBust
    | RemoveAt Int Int
    | ToggleNerds
    | OpenFlip Int
    | PickTarget Int
    | CloseFlip
    | NextRound
    | AskNewGame
    | CancelNewGame
    | ConfirmNewGame
    | DismissWinner
    | ShowWinner


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( applyMsg msg model, Cmd.none )


applyMsg : Msg -> Model -> Model
applyMsg msg model =
    case msg of
        NameInput value ->
            { model | nameInput = value }

        AddPlayer ->
            let
                name =
                    String.trim model.nameInput
            in
            if name == "" || List.length model.players >= maxPlayers then
                model

            else
                let
                    ( emoji, nextSeed ) =
                        assignEmoji model.seed model.players

                    player =
                        { id = model.nextId
                        , name = name
                        , emoji = emoji
                        , total = 0
                        , hand = []
                        , busted = False
                        , bustCard = Nothing
                        }
                in
                { model
                    | players = model.players ++ [ player ]
                    , nameInput = ""
                    , seed = nextSeed
                    , nextId = model.nextId + 1
                }

        RemovePlayer id ->
            { model | players = List.filter (\p -> p.id /= id) model.players }

        StartGame ->
            if List.length model.players < 2 then
                model

            else
                { model | screen = Playing, round = 1, openId = Nothing }

        ToggleOpen id ->
            { model
                | bustPicking = Nothing
                , openId =
                    if model.openId == Just id then
                        Nothing

                    else
                        Just id
            }

        Tap id kind ->
            { model | players = updatePlayer id (tapCard kind) model.players }

        Undo id ->
            { model | players = updatePlayer id undoLast model.players }

        StartBust id ->
            { model | bustPicking = Just id }

        PickBust id kind ->
            { model | players = updatePlayer id (bustWith (Just kind)) model.players, bustPicking = Nothing }

        BustNoCard id ->
            { model | players = updatePlayer id (bustWith Nothing) model.players, bustPicking = Nothing }

        CancelBust ->
            { model | bustPicking = Nothing }

        RemoveAt id index ->
            { model | players = updatePlayer id (\p -> { p | hand = removeAt index p.hand }) model.players }

        ToggleNerds ->
            { model | nerds = not model.nerds }

        OpenFlip id ->
            { model | flipFrom = Just id }

        PickTarget targetId ->
            case model.flipFrom of
                Just sourceId ->
                    { model
                        | players = updatePlayer sourceId (tapCard KFlip3) model.players
                        , openId = Just targetId
                        , flipFrom = Nothing
                    }

                Nothing ->
                    model

        CloseFlip ->
            { model | flipFrom = Nothing }

        NextRound ->
            if model.finished then
                model

            else
                let
                    -- this round's cards go to the discard pile; if the pile was reshuffled
                    -- mid-round, the old discards are already back in the draw pile
                    nextDiscards =
                        if reshuffleHappened model then
                            seenThisRound model

                        else
                            model.discards ++ seenThisRound model

                    players =
                        List.map
                            (\p -> { p | total = p.total + handScore p, hand = [], busted = False, bustCard = Nothing })
                            model.players

                    over =
                        List.any (\p -> p.total >= winningScore) players
                in
                { model
                    | players = players
                    , round =
                        if over then
                            model.round

                        else
                            model.round + 1
                    , discards = nextDiscards
                    , openId = Nothing
                    , bustPicking = Nothing
                    , finished = over
                    , winnerShown = over
                }

        AskNewGame ->
            { model | confirmNew = True }

        CancelNewGame ->
            { model | confirmNew = False }

        ConfirmNewGame ->
            { model
                | players = resetPlayers model.players
                , discards = []
                , round = 1
                , openId = Nothing
                , flipFrom = Nothing
                , bustPicking = Nothing
                , confirmNew = False
                , winnerShown = False
                , finished = False
            }

        DismissWinner ->
            { model | winnerShown = False }

        ShowWinner ->
            { model | winnerShown = True }



-- FORMAT


fmt1 : Float -> String
fmt1 x =
    let
        tenths =
            round (x * 10)
    in
    String.fromInt (tenths // 10) ++ "." ++ String.fromInt (modBy 10 tenths)


fmtSigned : Float -> String
fmtSigned x =
    if x < 0 && round (abs x * 10) /= 0 then
        "-" ++ fmt1 (abs x)

    else
        "+" ++ fmt1 (abs x)


fmtPct : Float -> String
fmtPct x =
    String.fromInt (round (x * 100)) ++ "%"



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class
            (if model.nerds then
                "app nerds"

             else
                "app"
            )
        ]
        (case model.screen of
            Setup ->
                [ viewSetup model ]

            Playing ->
                viewPlaying model
        )


viewSetup : Model -> Html Msg
viewSetup model =
    let
        count =
            List.length model.players
    in
    div [ class "setup" ]
        [ div [ class "logo" ] [ text "Flip 7" ]
        , div [ class "sub" ] [ text "Who is playing tonight?" ]
        , form [ class "addRow", onSubmit AddPlayer ]
            [ input
                [ A.type_ "text"
                , A.placeholder "Player name"
                , A.value model.nameInput
                , A.maxlength 16
                , A.attribute "autocomplete" "off"
                , A.attribute "aria-label" "Player name"
                , onInput NameInput
                ]
                []
            , button [ A.type_ "submit", class "addBtn" ] [ text "Add" ]
            ]
        , div [ class "roster" ] (List.map viewRosterRow model.players)
        , div [ class "sub small" ]
            [ text
                (if count < 2 then
                    "Add at least two players to start."

                 else
                    String.fromInt count ++ " players ready."
                )
            ]
        , button
            [ class "primary wide"
            , onClick StartGame
            , A.disabled (count < 2)
            ]
            [ text "Start game" ]
        ]


viewRosterRow : Player -> Html Msg
viewRosterRow p =
    div [ class "rosterRow" ]
        [ span [ class "avatar" ] [ text p.emoji ]
        , span [ class "name grow" ] [ text p.name ]
        , button
            [ class "ghost small"
            , A.attribute "aria-label" ("Remove " ++ p.name)
            , onClick (RemovePlayer p.id)
            ]
            [ text "Remove" ]
        ]


viewPlaying : Model -> List (Html Msg)
viewPlaying model =
    [ viewHeader model
    , div [ class "list" ] (List.map (viewPlayer model) model.players)
    , viewFooter model
    , viewFlipSheet model
    , viewConfirm model
    , viewWinner model
    ]


viewHeader : Model -> Html Msg
viewHeader model =
    let
        subtitle =
            if model.nerds then
                "Round " ++ String.fromInt model.round ++ " · " ++ String.fromInt (unseenTotal model) ++ " cards left in the pile"

            else
                "Round " ++ String.fromInt model.round ++ " · first to " ++ String.fromInt winningScore ++ " wins"
    in
    div [ class "hdr" ]
        [ div []
            [ div [ class "logo" ] [ text "Flip 7" ]
            , div [ class "sub" ] [ text subtitle ]
            ]
        , div [ class "nerdSwitch" ]
            [ span
                [ class
                    (if model.nerds then
                        "nerdLabel on"

                     else
                        "nerdLabel"
                    )
                ]
                [ text "Stats for", br [] [], text "nerds" ]
            , button
                [ class
                    (if model.nerds then
                        "switch on"

                     else
                        "switch"
                    )
                , A.attribute "role" "switch"
                , A.attribute "aria-checked"
                    (if model.nerds then
                        "true"

                     else
                        "false"
                    )
                , A.attribute "aria-label" "Stats for nerds"
                , onClick ToggleNerds
                ]
                [ span [ class "knob" ] [] ]
            ]
        ]


viewFooter : Model -> Html Msg
viewFooter model =
    div [ class "foot" ]
        [ button [ class "ghost", onClick AskNewGame ] [ text "New game" ]
        , if model.finished then
            button [ class "primary grow", onClick ShowWinner ] [ text "Game over · see winner" ]

          else
            button [ class "primary grow", onClick NextRound ] [ text "Next round →" ]
        ]


viewPlayer : Model -> Player -> Html Msg
viewPlayer model p =
    let
        isOpen =
            model.openId == Just p.id

        pct =
            String.fromFloat (min 100 (toFloat p.total / 2)) ++ "%"
    in
    div
        [ class
            (if isOpen then
                "card open"

             else
                "card"
            )
        ]
        [ button
            [ class "cardHead"
            , onClick (ToggleOpen p.id)
            , A.attribute "aria-expanded"
                (if isOpen then
                    "true"

                 else
                    "false"
                )
            ]
            [ span [ class "avatar" ] [ text p.emoji ]
            , span [ class "who" ]
                [ span [ class "name" ] [ text p.name ]
                , viewPill isOpen p
                ]
            , span [ class "score" ]
                [ span [ class "big" ] [ text (String.fromInt p.total) ]
                , span [ class "of" ] [ text (" / " ++ String.fromInt winningScore) ]
                ]
            ]
        , div [ class "bar" ] [ div [ class "fill", style "width" pct ] [] ]
        , if model.nerds && not isOpen then
            viewMiniStats model p

          else
            text ""
        , if isOpen then
            viewControls model p

          else
            text ""
        ]


viewPill : Bool -> Player -> Html Msg
viewPill isOpen p =
    if p.busted then
        span [ class "pill bust" ] [ text "Busted · +0" ]

    else
        span
            [ class
                (if isOpen then
                    "pill live"

                 else
                    "pill"
                )
            ]
            [ text ("+" ++ String.fromInt (handScore p) ++ " this round") ]


viewMiniStats : Model -> Player -> Html Msg
viewMiniStats model p =
    if p.busted then
        text ""

    else
        case nextStats model p of
            Nothing ->
                text ""

            Just s ->
                div [ class "mini" ]
                    [ span [] [ text "Next card" ]
                    , span [ class "miniChip risk" ] [ text (fmtPct s.bust ++ " bust") ]
                    , span [ class "miniChip gain" ] [ text (fmtSigned s.gain ++ " avg gain") ]
                    ]


viewControls : Model -> Player -> Html Msg
viewControls model p =
    if model.bustPicking == Just p.id then
        viewBustPicker model p

    else
        viewControlsMain model p


viewControlsMain : Model -> Player -> Html Msg
viewControlsMain model p =
    div [ class "controls" ]
        [ if model.nerds then
            viewNextPanel model p

          else
            text ""
        , viewHand p
        , div [ class "grid7" ]
            (List.map (numButton model p) (List.range 0 12) ++ [ undoButton p ])
        , if model.nerds then
            div [ class "hint" ] [ text (hintText model p) ]

          else
            text ""
        , div [ class "grid6" ] (List.map (bonusButton model p) bonusKinds)
        , div [ class "grid3" ] (List.map (actionButton model p) actionKinds)
        , button
            [ class "bustBtn"
            , onClick (StartBust p.id)
            , A.disabled p.busted
            ]
            [ text "Busted" ]
        ]


viewHand : Player -> Html Msg
viewHand p =
    let
        chips =
            if List.isEmpty p.hand then
                [ span [ class "handEmpty" ] [ text "Tap cards as they are drawn. Tap again to take one back." ] ]

            else
                List.indexedMap (chipFor p) p.hand

        extra =
            (if flip7Bonus p > 0 then
                [ span [ class "chip good" ] [ text "Flip 7! +15" ] ]

             else
                []
            )
                ++ (if p.busted then
                        [ span [ class "chip dup" ] [ text (bustLabel p) ] ]

                    else
                        []
                   )
    in
    div [ class "hand" ]
        ([ span [ class "handLabel" ] [ text "Hand" ] ]
            ++ chips
            ++ extra
            ++ [ span [ class "handTotal" ] [ text ("= " ++ String.fromInt (handScore p)) ] ]
        )


chipFor : Player -> Int -> Kind -> Html Msg
chipFor p index kind =
    let
        cls =
            case kind of
                KNum _ ->
                    "chip"

                KPlus _ ->
                    "chip mod"

                KTimes2 ->
                    "chip mod"

                _ ->
                    "chip act"
    in
    button
        [ class cls
        , A.attribute "aria-label" ("Remove " ++ kindChip kind ++ " from " ++ p.name ++ "'s hand")
        , onClick (RemoveAt p.id index)
        ]
        [ text (kindChip kind) ]


bustLabel : Player -> String
bustLabel p =
    case p.bustCard of
        Just k ->
            "Busted on " ++ kindLabel k

        Nothing ->
            "Busted"


viewBustPicker : Model -> Player -> Html Msg
viewBustPicker model p =
    let
        hasNumbers =
            not (List.isEmpty (List.filterMap cardNumber p.hand))

        pickHint =
            if hasNumbers then
                "Amber cards are in " ++ p.name ++ "'s hand. Busting means drawing a repeat of one of them."

            else
                "Pick the number that " ++ p.name ++ " drew."
    in
    div [ class "controls" ]
        [ div [ class "panel" ]
            [ div [ class "pickTitle" ] [ text ("Which card busted " ++ p.name ++ "?") ]
            , div [ class "sheetSub" ] [ text (p.name ++ " scores 0 this round and the card is counted as seen.") ]
            ]
        , div [ class "grid7" ] (List.map (pickButton model p) (List.range 0 12))
        , div [ class "hint" ] [ text pickHint ]
        , div [ class "pickActions" ]
            [ button [ class "ghost grow", onClick CancelBust ] [ text "Cancel" ]
            , button [ class "ghost grow", onClick (BustNoCard p.id) ] [ text "Not sure" ]
            ]
        ]


pickButton : Model -> Player -> Int -> Html Msg
pickButton model p n =
    let
        kind =
            KNum n

        held =
            List.member kind p.hand
    in
    button
        [ class
            (if held then
                "key held"

             else
                "key"
            )
        , onClick (PickBust p.id kind)
        ]
        (span [ class "kv" ] [ text (String.fromInt n) ] :: leftLabel model kind)


leftLabel : Model -> Kind -> List (Html Msg)
leftLabel model kind =
    if model.nerds then
        [ span [ class "kl" ] [ text (String.fromInt (leftCount model kind) ++ " left") ] ]

    else
        []


numButton : Model -> Player -> Int -> Html Msg
numButton model p n =
    let
        kind =
            KNum n

        held =
            List.member kind p.hand
    in
    button
        [ class
            (if held then
                "key held"

             else
                "key"
            )
        , onClick (Tap p.id kind)
        , A.disabled p.busted
        ]
        (span [ class "kv" ] [ text (String.fromInt n) ] :: leftLabel model kind)


undoButton : Player -> Html Msg
undoButton p =
    button
        [ class "key undo"
        , A.attribute "aria-label" "Undo last card"
        , onClick (Undo p.id)
        , A.disabled (List.isEmpty p.hand && not p.busted)
        ]
        [ text "↶" ]


bonusButton : Model -> Player -> Kind -> Html Msg
bonusButton model p kind =
    let
        held =
            List.member kind p.hand

        gone =
            model.nerds && not held && leftCount model kind == 0

        cls =
            if held then
                "key bonus held"

            else if gone then
                "key bonus gone"

            else
                "key bonus"
    in
    button
        [ class cls
        , onClick (Tap p.id kind)
        , A.disabled p.busted
        ]
        (span [ class "kv" ] [ text (kindLabel kind) ] :: leftLabel model kind)


actionButton : Model -> Player -> Kind -> Html Msg
actionButton model p kind =
    let
        used =
            List.length (List.filter (\c -> c == kind) p.hand)

        clickMsg =
            if kind == KFlip3 && model.nerds then
                OpenFlip p.id

            else
                Tap p.id kind

        sub =
            if model.nerds then
                String.fromInt (leftCount model kind) ++ " left in deck"

            else if used > 0 then
                "used ×" ++ String.fromInt used

            else
                "\u{00A0}"

        cls =
            case kind of
                KFlip3 ->
                    "act flip"

                KFreeze ->
                    "act freeze"

                _ ->
                    "act second"
    in
    button
        [ class cls
        , onClick clickMsg
        , A.disabled p.busted
        ]
        [ span [ class "al" ] [ text (kindLabel kind) ]
        , span [ class "as" ] [ text sub ]
        ]


hintText : Model -> Player -> String
hintText model p =
    let
        heldNums =
            List.filterMap cardNumber p.hand

        lefts =
            List.map (\n -> leftCount model (KNum n)) heldNums
    in
    if List.isEmpty heldNums then
        "No numbers in " ++ p.name ++ "'s hand yet, so nothing can bust them."

    else
        "Amber cards are in "
            ++ p.name
            ++ "'s hand. Their remaining copies ("
            ++ String.join " + " (List.map String.fromInt lefts)
            ++ ") are the "
            ++ String.fromInt (List.sum lefts)
            ++ " cards that bust them."


viewNextPanel : Model -> Player -> Html Msg
viewNextPanel model p =
    if p.busted then
        div [ class "panel" ] [ div [ class "verdict" ] [ text (p.name ++ " is busted this round.") ] ]

    else
        case nextStats model p of
            Nothing ->
                div [ class "panel" ] [ div [ class "verdict" ] [ text "No unseen cards left to count." ] ]

            Just s ->
                let
                    atRisk =
                        handScore p

                    verdict =
                        if atRisk == 0 then
                            "Nothing to lose yet, so keep flipping."

                        else if s.net > 1 then
                            "Worth it: you stand to gain more than you risk."

                        else if s.net < -1 then
                            "Risky: you stand to lose more than you gain."

                        else
                            "Basically a coin flip: what you stand to gain matches what you stand to lose."
                in
                div [ class "panel" ]
                    [ div [ class "panelHead" ]
                        [ span [ class "panelTitle" ] [ text (p.name ++ "'s next card") ]
                        , span [ class "panelMeta" ] [ text (String.fromInt s.unseen ++ " unseen") ]
                        ]
                    , div [ class "split" ]
                        [ div [ class "safePart", style "flex" (String.fromFloat (1 - s.bust)) ] []
                        , div [ class "bustPart", style "flex" (String.fromFloat s.bust) ] []
                        ]
                    , div [ class "metrics" ]
                        [ div []
                            [ div [ class "metric risk" ] [ text (fmtPct s.bust) ]
                            , div [ class "metricLabel" ] [ text "bust chance", br [] [], text (String.fromInt s.bustCards ++ " of " ++ String.fromInt s.unseen ++ " cards") ]
                            ]
                        , div []
                            [ div [ class "metric gain" ] [ text (fmtSigned s.gain) ]
                            , div [ class "metricLabel" ] [ text "avg gain if", br [] [], text "you stay safe" ]
                            ]
                        , div []
                            [ div
                                [ class
                                    (if s.net >= 0 then
                                        "metric"

                                     else
                                        "metric risk"
                                    )
                                ]
                                [ text (fmtSigned s.net) ]
                            , div [ class "metricLabel" ] [ text "net per flip", br [] [], text ("risking " ++ String.fromInt atRisk ++ " pts") ]
                            ]
                        ]
                    , div [ class "verdict" ] [ text verdict ]
                    ]



-- FLIP THREE SHEET


viewFlipSheet : Model -> Html Msg
viewFlipSheet model =
    case model.flipFrom of
        Nothing ->
            text ""

        Just sourceId ->
            let
                -- count the Flip Three card itself as seen before working out odds
                sim =
                    updatePlayer sourceId (\p -> { p | hand = p.hand ++ [ KFlip3 ] }) model.players

                simModel =
                    { model | players = sim }
            in
            div []
                [ div [ class "scrim", onClick CloseFlip ] []
                , div [ class "sheet" ]
                    [ div [ class "grabber" ] []
                    , div [ class "sheetTitle" ] [ text "Flip Three" ]
                    , div [ class "sheetSub" ] [ text "Who is it being played on? Odds cover all three draws." ]
                    , div [ class "targets" ] (List.map (viewTarget simModel) sim)
                    , div [ class "sheetSub" ] [ text "Gain is the average points from three safe cards. Pick a target to log the Flip Three and start entering their draws." ]
                    , button [ class "ghost wide", onClick CloseFlip ] [ text "Cancel" ]
                    ]
                ]


viewTarget : Model -> Player -> Html Msg
viewTarget simModel p =
    let
        handText =
            if p.busted then
                "Busted this round"

            else if List.isEmpty p.hand then
                "No cards yet"

            else
                String.join " · " (List.map kindChip p.hand)
    in
    if p.busted then
        div [ class "target out" ]
            [ span [ class "avatar small" ] [ text p.emoji ]
            , span [ class "who" ]
                [ span [ class "name" ] [ text p.name ]
                , span [ class "handText" ] [ text handText ]
                ]
            , span [ class "outText" ] [ text "Can't be targeted" ]
            ]

    else
        let
            ( bust, gain ) =
                flipThreeStats simModel p
        in
        button [ class "target", onClick (PickTarget p.id) ]
            [ div [ class "targetTop" ]
                [ span [ class "avatar small" ] [ text p.emoji ]
                , span [ class "who" ]
                    [ span [ class "name" ] [ text p.name ]
                    , span [ class "handText" ] [ text handText ]
                    ]
                , span [ class "stat" ]
                    [ span [ class "metric risk" ] [ text (fmtPct bust) ]
                    , span [ class "metricLabel" ] [ text "bust" ]
                    ]
                , span [ class "stat" ]
                    [ span [ class "metric gain" ] [ text (fmtSigned gain) ]
                    , span [ class "metricLabel" ] [ text "if safe" ]
                    ]
                ]
            , div [ class "split thin" ]
                [ div [ class "safePart", style "flex" (String.fromFloat (1 - bust)) ] []
                , div [ class "bustPart", style "flex" (String.fromFloat bust) ] []
                ]
            ]



-- NEW GAME CONFIRM


viewConfirm : Model -> Html Msg
viewConfirm model =
    if model.confirmNew then
        div []
            [ div [ class "scrim", onClick CancelNewGame ] []
            , div [ class "dialog" ]
                [ div [ class "sheetTitle" ] [ text "Start a new game?" ]
                , div [ class "sheetSub" ] [ text "Everyone stays, all scores go back to zero." ]
                , div [ class "dialogButtons" ]
                    [ button [ class "ghost grow", onClick CancelNewGame ] [ text "Cancel" ]
                    , button [ class "primary grow", onClick ConfirmNewGame ] [ text "New game" ]
                    ]
                ]
            ]

    else
        text ""



-- WINNER


viewWinner : Model -> Html Msg
viewWinner model =
    if not model.winnerShown then
        text ""

    else
        case winnerOf model.players of
            Nothing ->
                text ""

            Just w ->
                let
                    ranked =
                        List.sortBy (\p -> negate p.total) model.players

                    past =
                        List.length (List.filter (\p -> p.total >= winningScore) model.players)

                    margin =
                        case ranked of
                            _ :: second :: _ ->
                                w.total - second.total

                            _ ->
                                0

                    message =
                        if past > 1 && margin == 0 then
                            String.fromInt past ++ " players reached " ++ String.fromInt winningScore ++ " and the top score is tied. Seating order breaks it, so " ++ w.name ++ " wins."

                        else if past > 1 then
                            String.fromInt past ++ " players went past " ++ String.fromInt winningScore ++ ", so the highest total takes it. " ++ w.name ++ " wins by " ++ String.fromInt margin ++ "."

                        else
                            w.name ++ " got there first."
                in
                div [ class "winner" ]
                    [ div [ class "winnerInner" ]
                        [ div [ class "confetti c1" ] []
                        , div [ class "confetti c2" ] []
                        , div [ class "confetti c3" ] []
                        , div [ class "confetti c4" ] []
                        , div [ class "confetti c5" ] []
                        , div [ class "confetti c6" ] []
                        , div [ class "crown" ] [ text w.emoji ]
                        , div [ class "eyebrow" ] [ text ("Round " ++ String.fromInt model.round ++ " · game over") ]
                        , div [ class "winnerName" ] [ text (w.name ++ " wins") ]
                        , div [ class "winnerMsg" ] [ text message ]
                        , div [ class "standings" ] (List.indexedMap (standingRow w) ranked)
                        , div [ class "grow" ] []
                        , button [ class "primary wide", onClick ConfirmNewGame ] [ text "New game · same players" ]
                        , button [ class "ghost wide", onClick DismissWinner ] [ text "Back to scoreboard" ]
                        ]
                    ]


standingRow : Player -> Int -> Player -> Html Msg
standingRow winner index p =
    let
        isWinner =
            p.id == winner.id

        note =
            if isWinner then
                "Past " ++ String.fromInt winningScore ++ " · winner"

            else if p.total >= winningScore then
                "Past " ++ String.fromInt winningScore ++ " · " ++ String.fromInt (winner.total - p.total) ++ " short of the win"

            else
                String.fromInt (winningScore - p.total) ++ " short of " ++ String.fromInt winningScore
    in
    div
        [ class
            (if isWinner then
                "standing top"

             else
                "standing"
            )
        ]
        [ span [ class "rank" ] [ text (String.fromInt (index + 1)) ]
        , span [ class "avatar small" ] [ text p.emoji ]
        , span [ class "who" ]
            [ span [ class "name" ] [ text p.name ]
            , span [ class "handText" ] [ text note ]
            ]
        , span [ class "big" ] [ text (String.fromInt p.total) ]
        ]



-- MAIN


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
