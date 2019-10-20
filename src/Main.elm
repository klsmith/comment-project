module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Color.OneDark as OneDark
import Element exposing (Attribute, Color, Element, FocusStyle, alignLeft, alignRight, centerX, centerY, column, el, fill, focusStyle, layout, rgb, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init : Model
init =
    { activity = Reading
    , editComment = Nothing
    , comments =
        [ { author = "Kooper Smith", message = "Hello World!" }
        , { author = "Patrick Chapman", message = "Hey Guys!" }
        , { author = "Wesley Courtney", message = "3.14159" }
        ]
    }



-- MODEL


type alias Model =
    { activity : Activity
    , editComment : Maybe Comment
    , comments : List Comment
    }


type Activity
    = Reading
    | Editing


type alias Comment =
    { author : String
    , message : String
    }



-- UPDATE


type Msg
    = AddComment
    | CancelEdit
    | EditAuthor String
    | EditMessage String


update : Msg -> Model -> Model
update msg model =
    case model.editComment of
        Just editComment ->
            case msg of
                AddComment ->
                    { model
                        | editComment = Nothing
                        , comments = model.comments ++ [ editComment ]
                    }

                CancelEdit ->
                    { model | editComment = Nothing }

                EditAuthor newAuthor ->
                    { model
                        | editComment =
                            Just
                                { editComment
                                    | author = newAuthor
                                }
                    }

                EditMessage newMessage ->
                    { model
                        | editComment =
                            Just
                                { editComment
                                    | message = newMessage
                                }
                    }

        Nothing ->
            case msg of
                AddComment ->
                    { model
                        | editComment = Just { author = "", message = "" }
                    }

                CancelEdit ->
                    model

                EditAuthor newAuthor ->
                    model

                EditMessage newMessage ->
                    model



-- VIEW


view : Model -> Html Msg
view model =
    layout []
        (el
            [ Background.color OneDark.black
            , Font.color OneDark.white
            , Element.width fill
            , Element.height fill
            , Element.padding 16
            ]
            (viewCommentSection [] model)
        )


viewCommentSection : List (Attribute Msg) -> Model -> Element Msg
viewCommentSection attributes model =
    column
        attributes
        ([ h1 [ centerX ] "Comment Section:"
         ]
            ++ viewCommentList model.comments
            ++ [ viewAddComment [] model ]
        )


h1 : List (Attribute Msg) -> String -> Element Msg
h1 attributes string =
    Element.el
        ([ Element.padding 8
         , Font.size 32
         ]
            ++ attributes
        )
        (Element.text string)


viewCommentList : List Comment -> List (Element Msg)
viewCommentList comments =
    List.map viewComment comments


viewComment : Comment -> Element Msg
viewComment comment =
    column
        [ Element.padding 8
        , Element.width fill
        , Border.width 1
        , Border.color OneDark.white
        , Background.color OneDark.commentGrey
        , Element.spacing 8
        , Font.alignLeft
        ]
        [ el [ Font.color OneDark.black ] (text comment.author)
        , Element.paragraph [ Font.color OneDark.white ] [ text comment.message ]
        ]


viewAddComment : List (Attribute Msg) -> Model -> Element Msg
viewAddComment attributes model =
    el [ Element.paddingXY 0 16 ]
        (case model.editComment of
            Just comment ->
                editCommentSection attributes comment

            Nothing ->
                addCommentButton attributes
        )


editCommentSection : List (Attribute Msg) -> Comment -> Element Msg
editCommentSection attributes comment =
    column
        [ Element.width fill
        , Border.width 1
        , Background.color OneDark.gutterGrey
        ]
        [ editCommentInput EditAuthor comment.author "Author"
        , editCommentInput EditMessage comment.message "Message"
        , row
            [ Element.width fill ]
            [ addCommentButton
                [ Font.color OneDark.green, Element.width fill ]
            , cancelCommentButton
                [ Font.color OneDark.darkRed, Element.width fill ]
            ]
        ]


editCommentInput : (String -> Msg) -> String -> String -> Element Msg
editCommentInput onChange text prompt =
    Input.text
        [ Element.padding 8
        , Background.color OneDark.black
        , onEnter AddComment
        ]
        { onChange = onChange
        , text = text
        , placeholder = Just (Input.placeholder [] (Element.text prompt))
        , label = Input.labelHidden prompt
        }


addCommentButton : List (Attribute Msg) -> Element Msg
addCommentButton attributes =
    button attributes "Add Comment" (Just AddComment)


cancelCommentButton : List (Attribute Msg) -> Element Msg
cancelCommentButton attributes =
    button attributes "Cancel" (Just CancelEdit)


button : List (Attribute Msg) -> String -> Maybe Msg -> Element Msg
button attributes text onPress =
    Input.button
        ([ Border.color OneDark.white
         , Border.width 1
         , Element.padding 8
         , Font.color OneDark.darkYellow
         , Font.center
         , Background.color OneDark.gutterGrey
         ]
            ++ attributes
        )
        { label = Element.text text
        , onPress = onPress
        }



-- External Code


{-| -}
onEnter : Msg -> Element.Attribute Msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )
