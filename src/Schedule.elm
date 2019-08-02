module Schedule exposing (main)

import Browser
import DateFormat
import Html as H exposing (Html, text)
import Html.Attributes as A exposing (class)
import Html.Events as E
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Set exposing (Set)
import Styles
import Time exposing (Posix)


type Msg
    = TalksFetched (Result Http.Error (List Talk))
    | FilterByTag String
    | FilterByDay Int
    | RemoveAllFilters


type alias Talk =
    { id : Int
    , title : String
    , body : String
    , tags : List String
    , level : String
    , starts_at : Maybe Posix
    , ends_at : Maybe Posix
    , created_at : String
    , updated_at : String
    , room : String
    , presenter : Presenter
    , url : String
    }


type alias Presenter =
    { name : String
    , bio : Maybe String
    }


tagDecoder : Decoder (List String)
tagDecoder =
    Decode.string
        |> Decode.andThen
            (\chunk ->
                String.split "," chunk
                    |> List.map String.trim
                    |> List.filter (\s -> s /= "")
                    |> Decode.succeed
            )


talkDecoder : Decoder Talk
talkDecoder =
    Decode.succeed Talk
        |> Decode.required "id" Decode.int
        |> Decode.required "title" Decode.string
        |> Decode.required "body" Decode.string
        |> Decode.required "tags" tagDecoder
        |> Decode.required "level" Decode.string
        |> Decode.required "starts_at" (Decode.maybe Iso8601.decoder)
        |> Decode.required "ends_at" (Decode.maybe Iso8601.decoder)
        |> Decode.required "created_at" Decode.string
        |> Decode.required "updated_at" Decode.string
        |> Decode.required "room" Decode.string
        |> Decode.required "presenter" presenterDecoder
        |> Decode.required "url" Decode.string


presenterDecoder : Decoder Presenter
presenterDecoder =
    Decode.map2
        Presenter
        (Decode.field "name" Decode.string)
        (Decode.maybe <| Decode.field "bio" Decode.string)


apiEndpoint : String
apiEndpoint =
    -- "https://codeandsupply.co/event_sessions/abstractions.json"
    "/abstractions.json"


type Filter
    = ByTag String


type alias Model =
    { talks : List Talk
    , loading : Bool
    , filters : List Filter
    , day : Int
    , timeZone : Time.Zone
    }


timeToString : Time.Zone -> Posix -> String
timeToString timeZone time =
    DateFormat.format
        [ DateFormat.hourNumber
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        , DateFormat.amPmLowercase
        ]
        timeZone
        time


fetchTalks : Cmd Msg
fetchTalks =
    Http.get
        { url = apiEndpoint
        , expect = Http.expectJson TalksFetched (Decode.list talkDecoder)
        }


breadcrumbs : List (Html m) -> List (Html m)
breadcrumbs elements =
    elements
        |> List.intersperse (H.span [] [ text " " ])


displayTags : List String -> Html Msg
displayTags tags =
    H.div [ class "flex" ]
        (List.map
            (\tag ->
                H.button
                    [ E.onClick <| FilterByTag tag
                    , class "block mr-1 text-teal-500 border border-teal-500 py-1 px-3 rounded text-xs capitalize"
                    ]
                    [ text <| " + " ++ tag ]
            )
            tags
            |> breadcrumbs
        )


displayBox : List (Html Msg) -> Html Msg
displayBox elements =
    H.div [ class "border border-gray-400 mb-6 p-4" ]
        elements


displayTalk : Time.Zone -> Talk -> Html Msg
displayTalk timeZone talk =
    let
        startTime =
            talk.starts_at
                |> Maybe.andThen (\posix -> Just <| timeToString timeZone posix)
                |> Maybe.withDefault ""

        endTime =
            talk.ends_at
                |> Maybe.andThen (\posix -> Just <| timeToString timeZone posix)
                |> Maybe.withDefault ""
    in
    displayBox
        [ H.nav []
            [ H.h2 [ class "" ] [ text talk.title ]
            , H.p [ class "text-sm" ] [ text talk.presenter.name ]
            , H.div []
                [ H.em [] [ text talk.level ]
                , H.br [] []
                ]
            ]
        , H.div [ class "text-xs" ]
            [ H.time [ A.class "" ] [ text startTime ]
            , text " - "
            , H.time [ A.class "" ] [ text endTime ]
            ]
        , H.div [ class "pt-3" ]
            [ H.div [] [ displayTags talk.tags ]
            ]
        ]


tagsFromFilters : List Filter -> List String
tagsFromFilters filters =
    List.filterMap
        (\filter ->
            case filter of
                ByTag tag ->
                    Just tag
        )
        filters


filterTalks : Model -> List Talk -> List Talk
filterTalks model talks =
    let
        filterTags =
            tagsFromFilters model.filters

        listIntersect : List String -> List String -> List String
        listIntersect a b =
            Set.toList <|
                Set.intersect
                    (Set.fromList a)
                    (Set.fromList b)
    in
    List.filter
        (\talk ->
            let
                talkDay =
                    talk.starts_at
                        |> Maybe.andThen
                            (\posix -> Just <| Time.toDay model.timeZone posix)
                        |> Maybe.withDefault 21
            in
            model.day == talkDay
        )
        talks
        |> List.filter
            (\talk ->
                List.length (listIntersect talk.tags filterTags)
                    == List.length filterTags
            )


view : Model -> Html Msg
view model =
    let
        filteredTalks =
            filterTalks model model.talks
    in
    H.div []
        [ H.h1 [ class "text-2xl mb-4" ] [ text "Abstractions 2019 Schedule" ]
        , if model.loading then
            H.div [] [ text "Fetching Talks..." ]

          else
            H.div []
                [ H.div [ class "flex mb-5" ]
                    [ H.button
                        [ E.onClick <| FilterByDay 21
                        , class <| Styles.filterDayBtn (model.day == 21)
                        ]
                        [ text "Day 1" ]
                    , H.button
                        [ E.onClick <| FilterByDay 22
                        , class <| Styles.filterDayBtn (model.day == 22)
                        ]
                        [ text "Day 2" ]
                    , H.button
                        [ E.onClick <| FilterByDay 23
                        , class <| Styles.filterDayBtn (model.day == 23)
                        ]
                        [ text "Day 3" ]
                    ]
                , if List.length model.filters > 0 then
                    H.div [ class "flex mb-5" ]
                        [ H.div [ class "flex" ]
                            (tagsFromFilters model.filters
                                |> List.sort
                                |> List.map
                                    (\tag ->
                                        H.span [ class "mr-1 text-teal-500 border border-teal-500 py-1 px-3 rounded text-xs capitalize" ] [ text tag ]
                                    )
                                |> breadcrumbs
                            )
                        , H.button
                            [ E.onClick RemoveAllFilters
                            , class " mr-1 text-white border bg-teal-500 border-teal-500 py-1 px-3 rounded text-xs capitalize"
                            ]
                            [ text "Clear Filters" ]
                        ]

                  else
                    text ""
                , H.div [] (List.map (displayTalk model.timeZone) filteredTalks)
                ]
        ]


sanitizeFilters : List Filter -> List Filter
sanitizeFilters filters =
    List.filterMap
        (\filter ->
            case filter of
                ByTag tag ->
                    Just tag
        )
        filters
        |> Set.fromList
        |> Set.toList
        |> List.map ByTag


update : Msg -> Model -> ( Model, Cmd m )
update msg model =
    case msg of
        FilterByDay day ->
            ( { model | day = day }, Cmd.none )

        RemoveAllFilters ->
            ( { model | filters = [] }, Cmd.none )

        FilterByTag tag ->
            ( { model | filters = sanitizeFilters <| List.append model.filters [ ByTag tag ] }
            , Cmd.none
            )

        TalksFetched result ->
            Result.toMaybe result
                |> Maybe.andThen
                    (\talks ->
                        Just ( { model | talks = talks, loading = False }, Cmd.none )
                    )
                |> Maybe.withDefault ( model, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { talks = [], loading = True, filters = [], day = 21, timeZone = Time.utc }
    , fetchTalks
    )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
