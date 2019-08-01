module Schedule exposing (main)

import Browser
import Html as H exposing (Html, text)
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Set exposing (Set)


type Msg
    = TalksFetched (Result Http.Error (List Talk))
    | FilterByTag String
    | RemoveAllFilters


type alias Talk =
    { id : Int
    , title : String
    , body : String
    , tags : List String
    , level : String
    , starts_at : String
    , ends_at : String
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
        |> Decode.required "starts_at" Decode.string
        |> Decode.required "ends_at" Decode.string
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
    "https://codeandsupply.co/event_sessions/abstractions.json"


type Filter
    = ByTag String


type alias Model =
    { talks : List Talk
    , loading : Bool
    , filters : List Filter
    }


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
    H.div []
        (List.map
            (\tag ->
                H.button
                    [ E.onClick <| FilterByTag tag
                    , A.class "is-capitalized"
                    , A.class "button is-small"
                    ]
                    [ text <| " + " ++ tag ]
            )
            tags
            |> breadcrumbs
        )


displayTalk : Talk -> Html Msg
displayTalk talk =
    H.div []
        [ H.h2 [ A.class "title" ] [ text talk.title ]
        , H.h3 [ A.class "subtitle" ] [ text talk.presenter.name ]
        , H.p [] [ text talk.level ]
        , H.div [] [ displayTags talk.tags ]
        , H.p [ A.class "content" ] [ text talk.body ]
        , H.hr [] []
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


filterTalks : List Filter -> List Talk -> List Talk
filterTalks filters talks =
    let
        filterTags =
            tagsFromFilters filters

        listIntersect : List String -> List String -> List String
        listIntersect a b =
            Set.toList <|
                Set.intersect
                    (Set.fromList a)
                    (Set.fromList b)
    in
    List.filter
        (\talk ->
            List.length (listIntersect talk.tags filterTags)
                == List.length filterTags
        )
        talks


view : Model -> Html Msg
view model =
    let
        filteredTalks =
            filterTalks model.filters model.talks
    in
    H.div []
        [ H.h1 [ A.class "title is-1" ] [ text "Abstractions 2019 Schedule" ]
        , if model.loading then
            H.div [] [ text "Fetching Talks..." ]

          else
            H.div []
                [ if List.length model.filters > 0 then
                    H.div []
                        [ H.div []
                            (tagsFromFilters model.filters
                                |> List.sort
                                |> List.map
                                    (\tag ->
                                        H.span
                                            [ A.class "tag is-info is-capitalized" ]
                                            [ text tag, H.button [ A.class "delete is-small" ] [] ]
                                    )
                                |> breadcrumbs
                            )
                        , H.button
                            [ E.onClick RemoveAllFilters
                            , A.class "button is-small is-warning"
                            ]
                            [ text "Clear Filters" ]
                        ]

                  else
                    text ""
                , H.div [] (List.map displayTalk filteredTalks)
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
    ( { talks = [], loading = True, filters = [] }, fetchTalks )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
