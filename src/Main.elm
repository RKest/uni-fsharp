module Main exposing (..)

import Browser
import Debug exposing (log)
import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute)
import Html.Events as Events
import Http
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Tag =
    { id : Int
    , name : String
    }


tagDecoder : D.Decoder Tag
tagDecoder =
    D.succeed Tag
        |> required "id" D.int
        |> required "name" D.string


type alias NotePreview =
    { id : Int
    , title : String
    , tags : List Tag
    }


notePreviewDecoder : D.Decoder NotePreview
notePreviewDecoder =
    D.succeed NotePreview
        |> required "id" D.int
        |> required "title" D.string
        |> required "tags" (D.list tagDecoder)


type alias HomeModel =
    { notes : List NotePreview
    , tags : List Tag
    }


homeModelDecoder : D.Decoder HomeModel
homeModelDecoder =
    D.succeed HomeModel
        |> required "notes" (D.list notePreviewDecoder)
        |> required "tags" (D.list tagDecoder)


type alias NoteModel =
    { id : Int
    , title : String
    , html : String
    }


type Model
    = Note NoteModel
    | Home HomeModel


init : () -> ( Model, Cmd Msg )
init () =
    ( Home { notes = [], tags = [] }
    , Http.get
        { url = "/api/home"
        , expect = Http.expectJson HomeResult homeModelDecoder
        }
    )


type Msg
    = Sync String
    | SyncResult (Result Http.Error ())
    | HomeResult (Result Http.Error HomeModel)
    | InitHtml String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Sync s ->
            ( model
            , Http.post
                { url = "/sync/" ++ String.fromInt model.id
                , body = Http.jsonBody (E.object [ ( "html", E.string s ), ( "id", E.int model.id ) ])
                , expect = Http.expectWhatever SyncResult
                }
            )

        SyncResult (Ok _) ->
            ( model, Cmd.none )

        SyncResult (Err e) ->
            let
                _ =
                    log "" e
            in
            ( model, Cmd.none )

        InitHtml _ ->
            Debug.todo "branch 'InitHtml _' not implemented"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        Note note ->
            noteEditview note

        Home home ->
            homeView home


noteEditview : NoteModel -> Html Msg
noteEditview note =
    div []
        [ div [] [ text note.title ]
        , div
            [ attribute "contentEditable" "true"
            , onContentEditableInput Sync
            ]
            [ text note.html ]
        ]


homeView : HomeModel -> Html Msg
homeView model =
    div []
        [ div [] [ text "Home" ]
        , div [] (List.map notePreviewView model.notes)
        , div [] (List.map tagView model.tags)
        ]


notePreviewView : NotePreview -> Html Msg
notePreviewView note =
    div []
        [ div [] [ text note.title ]
        , div [] (List.map (\t -> text t.name) note.tags)
        ]


tagView : Tag -> Html Msg
tagView tag =
    div [] [ text tag.name ]


onContentEditableInput : (String -> msg) -> Html.Attribute msg
onContentEditableInput tagger =
    Events.stopPropagationOn "input" (D.map (\x -> ( x, True )) (D.map tagger (D.at [ "target", "innerHTML" ] D.string)))
