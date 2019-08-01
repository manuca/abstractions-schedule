module Schedule exposing (main)

import Browser
import Html as H exposing (Html, text)
import Html.Events as E
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type Msg
    = TalksFetched (Result Http.Error (List Talk))
    | FilterByTag String


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


displayTags : List String -> Html Msg
displayTags tags =
    H.div []
        (List.map (\tag -> H.button [ E.onClick <| FilterByTag tag ] [ text tag ]) tags
            |> List.intersperse (H.span [] [ text " / " ])
        )


displayTalk : Talk -> Html Msg
displayTalk talk =
    H.div []
        [ H.h2 [] [ text talk.title ]
        , H.h3 [] [ text <| "By: " ++ talk.presenter.name ]
        , H.div [] [ displayTags talk.tags ]
        , H.p [] [ text talk.level ]
        , H.p [] [ text talk.body ]
        , H.p [] [ text talk.url ]
        ]


filterTalks : List Filter -> List Talk -> List Talk
filterTalks filters talks =
    case List.head filters of
        Just (ByTag tag) ->
            List.filter (\talk -> List.member tag talk.tags) talks

        Nothing ->
            talks


view : Model -> Html Msg
view model =
    let
        filteredTalks =
            filterTalks model.filters model.talks
    in
    H.div []
        [ H.h1 [] [ text "Abstractions 2019 Schedule" ]
        , if model.loading then
            H.div [] [ text "Fetching Talks..." ]

          else
            H.div [] (List.map displayTalk filteredTalks)
        ]


update : Msg -> Model -> ( Model, Cmd m )
update msg model =
    case msg of
        FilterByTag tag ->
            ( { model | filters = [ ByTag tag ] }
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
